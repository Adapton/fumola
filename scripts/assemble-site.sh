#!/usr/bin/env bash
# Assemble the Pages site: content-addressed runtime versions, a manifest, and
# the stable copies older clients still ask for.
#
# Why this is a script and not steps in wasm-pages.yml: this is the mechanism
# by which the runtime reaches every browser that loads it, including Hazel's
# livelits, and a workflow can only be tested by deploying. Here it can be run
# against a scratch directory (see test/assemble-site.test.sh) before it ever
# publishes anything.
#
# See Adapton/fumola#69. In short: Pages stamps `Cache-Control: max-age=600` on
# everything and offers no way to change it, so freshness cannot come from
# headers. It comes from the URL instead -- a new build is a new directory, so
# a browser holding the old one simply stops asking for it.
#
# Usage:
#   assemble-site.sh --bindings DIR --pages DIR --out DIR
#                    [--previous DIR] [--commit SHA] [--keep N]
#
#   --bindings  wasm-bindgen output: fumola_wasm.js and fumola_wasm_bg.wasm
#   --pages     the checked-in pages/ tree, copied over the site as-is
#   --out       the directory to assemble (created; must not already exist)
#   --previous  a checkout of the current gh-pages, for carrying versions
#               forward. Omitted on a first publish.
#   --commit    source commit, recorded in the manifest for provenance
#   --keep      how many versions to retain, newest first (default 10)
set -euo pipefail

BINDINGS="" PAGES="" OUT="" PREVIOUS="" COMMIT="" KEEP=10

while [ $# -gt 0 ]; do
  case "$1" in
    --bindings) BINDINGS="$2"; shift 2 ;;
    --pages)    PAGES="$2";    shift 2 ;;
    --out)      OUT="$2";      shift 2 ;;
    --previous) PREVIOUS="$2"; shift 2 ;;
    --commit)   COMMIT="$2";   shift 2 ;;
    --keep)     KEEP="$2";     shift 2 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

for required in BINDINGS PAGES OUT; do
  if [ -z "${!required}" ]; then
    echo "--${required,,} is required" >&2
    exit 2
  fi
done

GLUE="$BINDINGS/fumola_wasm.js"
WASM="$BINDINGS/fumola_wasm_bg.wasm"
for f in "$GLUE" "$WASM"; do
  if [ ! -f "$f" ]; then
    echo "missing $f -- did wasm-bindgen run?" >&2
    exit 1
  fi
done

# The pair is hashed together, never separately. wasm-bindgen generates the
# glue and the binary as a matched set, and Hazel's loader passes both URLs
# explicitly (prebundle.js), where a mismatch fails to load rather than
# misbehaving. One hash over both means a version can only ever be handed out
# whole.
#
# 16 hex characters of SHA-256. Long enough that a collision is not a thing
# that happens; short enough to read in a URL and quote in a bug report.
HASH="$(cat "$WASM" "$GLUE" | sha256sum | cut -c1-16)"
BUILT="$(date -u +%Y-%m-%dT%H:%M:%SZ)"

if [ -e "$OUT" ]; then
  echo "$OUT already exists; refusing to assemble into it" >&2
  exit 1
fi
mkdir -p "$OUT/v"

# Carry the previous versions forward. Without this, force_orphan: true in the
# publish step would delete every older version on the next deploy, and a
# pinned URL would 404 -- pinning that lasts until the next push to main is
# worse than no pinning, because the URL looks permanent.
if [ -n "$PREVIOUS" ] && [ -d "$PREVIOUS/v" ]; then
  cp -r "$PREVIOUS/v/." "$OUT/v/"
  echo "carried forward $(find "$OUT/v" -mindepth 1 -maxdepth 1 -type d | wc -l) previous version(s)"
fi

# This build's version. Overwritten rather than skipped when the hash is
# already present: a rebuild of identical content is identical, so this is a
# no-op, and being unconditional keeps a half-copied directory from a failed
# earlier run from surviving.
mkdir -p "$OUT/v/$HASH"
cp "$GLUE" "$WASM" "$OUT/v/$HASH/"

# The ordered history, newest first, with this build promoted to the front.
# Order cannot be recovered from the directory names -- they are hashes -- so
# it has to be carried in a file.
PREV_VERSIONS="[]"
if [ -n "$PREVIOUS" ] && [ -f "$PREVIOUS/versions.json" ]; then
  PREV_VERSIONS="$(cat "$PREVIOUS/versions.json")"
fi

VERSIONS="$(
  HASH="$HASH" COMMIT="$COMMIT" BUILT="$BUILT" KEEP="$KEEP" \
  python3 -c '
import json, os, sys
prev = json.loads(sys.stdin.read() or "[]")
hash_, commit, built, keep = (
    os.environ["HASH"], os.environ["COMMIT"], os.environ["BUILT"], int(os.environ["KEEP"]))
# A rehash of identical content keeps its place in history rather than
# appearing twice; its metadata is refreshed to this build.
prev = [v for v in prev if v.get("hash") != hash_]
entry = {"hash": hash_, "commit": commit, "built": built}
print(json.dumps(([entry] + prev)[:keep], indent=2))
' <<< "$PREV_VERSIONS"
)"
printf '%s\n' "$VERSIONS" > "$OUT/versions.json"

# Prune whatever fell off the end of the window.
KEPT="$(printf '%s' "$VERSIONS" | python3 -c 'import json,sys; print("\n".join(v["hash"] for v in json.load(sys.stdin)))')"
for dir in "$OUT"/v/*/; do
  [ -d "$dir" ] || continue
  name="$(basename "$dir")"
  if ! grep -qxF "$name" <<< "$KEPT"; then
    echo "pruning $name"
    rm -rf "$dir"
  fi
done

# What a client fetches to find the current version. The only mutable URL in
# the loading path, and a few hundred bytes, so revalidating it is a
# conditional request answered by a 304 -- as against revalidating 5.6 MB.
cat > "$OUT/runtime.json" <<JSON
{
  "hash": "$HASH",
  "commit": "$COMMIT",
  "built": "$BUILT",
  "js": "/v/$HASH/fumola_wasm.js",
  "wasm": "/v/$HASH/fumola_wasm_bg.wasm"
}
JSON

# The stable pair, kept as real copies rather than a redirect or a re-export
# shim. A shim would be small enough to arrive fresh while the 5.6 MB binary
# beside it stayed cached, and fresh glue with a stale binary does not load at
# all. Real copies leave existing clients exactly as they are today: stale
# together, which at least works.
cp "$GLUE" "$WASM" "$OUT/"

# The pages last, so they can never be shadowed by a generated file.
cp -r "$PAGES/." "$OUT/"

echo
echo "assembled $OUT"
echo "  version $HASH  (commit ${COMMIT:-unknown})"
echo "  retaining $(python3 -c 'import json,sys; print(len(json.load(sys.stdin)))' < "$OUT/versions.json") version(s), keep=$KEEP"
