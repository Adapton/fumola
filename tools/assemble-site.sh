#!/usr/bin/env bash
# Assemble the Pages site: content-addressed runtime versions, a manifest, and
# the stable copies older clients still ask for.
#
# Why this is a script and not steps in wasm-pages.yml: this is the mechanism
# by which the runtime reaches every browser that loads it, including Hazel's
# livelits, and a workflow can only be tested by deploying. Here it can be run
# against a scratch directory (see tools/assemble-site.test.sh) before it ever
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

# A runtime pair in the pages tree would win, and it is never the one wanted.
#
# The pages are copied last on purpose, so a hand-written file can never be
# shadowed by a generated one. That is right for index.html and wrong for the
# runtime: a `fumola_wasm*` sitting in the pages tree lands on top of the
# freshly built pair and the site serves whatever that stale file holds. The
# hashed copy under /v/ is still correct, so the manifest looks right and a
# browser following it loads fine -- only the stable root path, the one older
# clients and Hazel's loader fall back to, goes quietly stale. That is a bad
# way to find out, and it is how Adapton/fumola#87 started.
#
# These files are build output and .gitignore covers them, so a checkout never
# has them and CI is unaffected. A local tree that does is asking to publish
# something it did not build, so say so instead of resolving it.
for stray in fumola_wasm.js fumola_wasm_bg.wasm; do
  if [ -e "$PAGES/$stray" ]; then
    echo "$PAGES/$stray would shadow the runtime built in $BINDINGS." >&2
    echo "It is build output, not page content -- remove it and run again." >&2
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

# The vendored libraries get the same treatment for the same reason: they are
# served from stable URLs under max-age, so a page can pick up a new module and
# an old three.js. They are versioned as a set rather than per file, because
# OrbitControls.js imports './three.module.min.js' -- a relative path, which a
# versioned directory keeps correct without touching vendor code, and which
# would break the moment the two were allowed to come from different versions.
#
# Names are hashed along with contents, so adding or renaming a file is a new
# version even when the bytes already present are unchanged.
VENDOR_HASH=""
if [ -d "$PAGES/vendor" ]; then
  VENDOR_HASH="$(
    cd "$PAGES/vendor" && find . -type f | LC_ALL=C sort | \
      while IFS= read -r f; do printf '%s\0' "$f"; cat "$f"; done | \
      sha256sum | cut -c1-16
  )"
fi

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

# The same for vendor, but only the hashed directories: the unhashed files
# beside them come from pages/ on every publish and are not history.
if [ -n "$PREVIOUS" ] && [ -d "$PREVIOUS/vendor" ]; then
  mkdir -p "$OUT/vendor"
  find "$PREVIOUS/vendor" -mindepth 1 -maxdepth 1 -type d -exec cp -r {} "$OUT/vendor/" \;
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
  VENDOR_HASH="$VENDOR_HASH" \
  python3 -c '
import json, os, sys
prev = json.loads(sys.stdin.read() or "[]")
hash_, commit, built, keep = (
    os.environ["HASH"], os.environ["COMMIT"], os.environ["BUILT"], int(os.environ["KEEP"]))
# A rehash of identical content keeps its place in history rather than
# appearing twice; its metadata is refreshed to this build.
prev = [v for v in prev if v.get("hash") != hash_]
entry = {"hash": hash_, "commit": commit, "built": built,
         "vendor": os.environ.get("VENDOR_HASH", "")}
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
VENDOR_FIELD=""
if [ -n "$VENDOR_HASH" ]; then
  VENDOR_FIELD=",
  \"vendor\": \"/vendor/$VENDOR_HASH\""
fi
cat > "$OUT/runtime.json" <<JSON
{
  "hash": "$HASH",
  "commit": "$COMMIT",
  "built": "$BUILT",
  "js": "/v/$HASH/fumola_wasm.js",
  "wasm": "/v/$HASH/fumola_wasm_bg.wasm"$VENDOR_FIELD
}
JSON

# The stable pair, kept as real copies rather than a redirect or a re-export
# shim. A shim would be small enough to arrive fresh while the 5.6 MB binary
# beside it stayed cached, and fresh glue with a stale binary does not load at
# all. Real copies leave existing clients exactly as they are today: stale
# together, which at least works.
cp "$GLUE" "$WASM" "$OUT/"

# The pages last, so they can never be shadowed by a generated file. This also
# lays down the unhashed vendor/ files, which stay for the same reason the
# unhashed runtime pair does: pages already published reference them.
cp -r "$PAGES/." "$OUT/"

# The hashed vendor set, copied after the pages so it cannot be clobbered by
# them. Both files land in one directory, which is what keeps OrbitControls'
# relative import of three.module.min.js resolving within its own version.
if [ -n "$VENDOR_HASH" ]; then
  mkdir -p "$OUT/vendor/$VENDOR_HASH"
  cp -r "$PAGES/vendor/." "$OUT/vendor/$VENDOR_HASH/"
  # A carried-forward vendor version stays while any retained entry still
  # names it. Vendor changes far less often than the runtime, so one directory
  # usually serves the whole window rather than one per entry.
  KEPT_VENDOR="$(printf '%s' "$VERSIONS" | python3 -c '
import json, sys
print("\n".join(v.get("vendor", "") for v in json.load(sys.stdin) if v.get("vendor")))')"
  # Only the directories this script mints are candidates for pruning, which
  # is why the name is matched against the shape of a hash rather than merely
  # against the kept list. A vendored library that lives in a directory of its
  # own -- vendor/katex/, with its stylesheet and its font files -- is not a
  # version of anything and has no entry in the manifest, so "not in the kept
  # list" swept it away on every publish. It was invisible because the loose
  # files at vendor/'s root, which is how three.js is vendored, are not
  # directories and were never at risk.
  for dir in "$OUT"/vendor/*/; do
    [ -d "$dir" ] || continue
    name="$(basename "$dir")"
    [[ "$name" =~ ^[0-9a-f]{16}$ ]] || continue
    if ! grep -qxF "$name" <<< "$KEPT_VENDOR"; then
      echo "pruning vendor $name"
      rm -rf "$dir"
    fi
  done
fi

# What the guard above protects, checked rather than assumed. The stable pair
# is the fallback path, so a mistake here is invisible until something old asks
# for it; and the ordering that makes it possible is three copies apart from
# the assertion, which is exactly the distance a later edit does not notice.
for pair in "fumola_wasm.js:$GLUE" "fumola_wasm_bg.wasm:$WASM"; do
  name="${pair%%:*}"
  built="${pair#*:}"
  for path in "$OUT/$name" "$OUT/v/$HASH/$name"; do
    if ! cmp -s "$built" "$path"; then
      echo "$path does not match the runtime built in $BINDINGS." >&2
      echo "Something in the assembly is shadowing it." >&2
      exit 1
    fi
  done
done

echo
echo "assembled $OUT"
echo "  version $HASH  (commit ${COMMIT:-unknown})"
echo "  retaining $(python3 -c 'import json,sys; print(len(json.load(sys.stdin)))' < "$OUT/versions.json") version(s), keep=$KEEP"
echo "  vendor  ${VENDOR_HASH:-none}"
