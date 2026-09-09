#!/usr/bin/env bash
# Tests for assemble-site.sh.
#
# This script decides what every browser gets, including Hazel's livelits, and
# a GitHub workflow can only be tested by publishing. So the logic lives in a
# script and the script is tested here, against scratch directories, with
# fake bindings standing in for a five-megabyte build.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ASSEMBLE="$HERE/assemble-site.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

passed=0
failed=0

check() { # check <description> <expected> <actual>
  if [ "$2" = "$3" ]; then
    passed=$((passed + 1))
    printf '  ok   %s\n' "$1"
  else
    failed=$((failed + 1))
    printf '  FAIL %s\n       expected: %s\n       actual:   %s\n' "$1" "$2" "$3"
  fi
}

check_file() { # check_file <description> <path>
  if [ -f "$2" ]; then
    passed=$((passed + 1)); printf '  ok   %s\n' "$1"
  else
    failed=$((failed + 1)); printf '  FAIL %s (missing %s)\n' "$1" "$2"
  fi
}

check_absent() { # check_absent <description> <path>
  if [ ! -e "$2" ]; then
    passed=$((passed + 1)); printf '  ok   %s\n' "$1"
  else
    failed=$((failed + 1)); printf '  FAIL %s (%s still exists)\n' "$1" "$2"
  fi
}

# Fake bindings whose content we control, so hashes are predictable.
bindings() { # bindings <dir> <marker>
  mkdir -p "$1"
  printf 'export default function init() { /* %s */ }\n' "$2" > "$1/fumola_wasm.js"
  printf 'fake wasm %s\n' "$2" > "$1/fumola_wasm_bg.wasm"
}

pages() { # pages <dir> [vendor marker]
  mkdir -p "$1/web-play" "$1/vendor" "$1/vendor/katex/fonts"
  echo '<html>index</html>' > "$1/index.html"
  echo '<html>web-play</html>' > "$1/web-play/index.html"
  printf 'three %s\n' "${2:-v1}" > "$1/vendor/three.module.min.js"
  printf "import './three.module.min.js'; // %s\n" "${2:-v1}" > "$1/vendor/OrbitControls.js"
  # A library vendored as a directory rather than as loose files, which is how
  # KaTeX arrives: a stylesheet naming its font files by a relative path, so
  # both have to land under one unhashed directory for a static page to link
  # them. Every test's pages carry it, because the pruning that used to delete
  # it ran on every publish and no test that omitted it could notice.
  printf '@font-face{src:url(fonts/KaTeX.woff2)} /* %s */\n' "${2:-v1}" \
    > "$1/vendor/katex/katex.min.css"
  printf 'woff2 %s\n' "${2:-v1}" > "$1/vendor/katex/fonts/KaTeX.woff2"
}

vendor_of() { python3 -c 'import json,sys; m=json.load(sys.stdin); print(m.get("vendor",""))' < "$1/runtime.json"; }

hash_of() { python3 -c 'import json,sys; print(json.load(sys.stdin)["hash"])' < "$1/runtime.json"; }
version_count() { python3 -c 'import json,sys; print(len(json.load(sys.stdin)))' < "$1/versions.json"; }

pages "$WORK/pages"

echo "a first publish, with no previous site"
bindings "$WORK/b1" one
"$ASSEMBLE" --bindings "$WORK/b1" --pages "$WORK/pages" --out "$WORK/s1" --commit aaaa111 >/dev/null
H1="$(hash_of "$WORK/s1")"
check_file "the manifest exists"                  "$WORK/s1/runtime.json"
check_file "the version list exists"              "$WORK/s1/versions.json"
check_file "the hashed glue"                      "$WORK/s1/v/$H1/fumola_wasm.js"
check_file "the hashed wasm"                      "$WORK/s1/v/$H1/fumola_wasm_bg.wasm"
check_file "the stable glue is a real file"       "$WORK/s1/fumola_wasm.js"
check_file "the stable wasm is a real file"       "$WORK/s1/fumola_wasm_bg.wasm"
check_file "the pages are copied"                 "$WORK/s1/index.html"
check_file "nested pages are copied"              "$WORK/s1/web-play/index.html"
check "one version is recorded"            "1" "$(version_count "$WORK/s1")"
check "the manifest names the commit"      "aaaa111" \
  "$(python3 -c 'import json,sys; print(json.load(sys.stdin)["commit"])' < "$WORK/s1/runtime.json")"
check "the manifest js path is absolute"   "/v/$H1/fumola_wasm.js" \
  "$(python3 -c 'import json,sys; print(json.load(sys.stdin)["js"])' < "$WORK/s1/runtime.json")"

V1="$(vendor_of "$WORK/s1")"
check "the manifest names a vendor version" "yes" "$([ -n "$V1" ] && echo yes || echo no)"
check_file "the hashed three.js"      "$WORK/s1${V1}/three.module.min.js"
check_file "the hashed OrbitControls" "$WORK/s1${V1}/OrbitControls.js"
check_file "the unhashed vendor files stay for already-published pages" \
  "$WORK/s1/vendor/three.module.min.js"
# The whole reason for hashing the directory rather than the files: this
# relative import has to resolve inside one version.
check "OrbitControls' sibling import resolves within its own version" "yes" \
  "$(grep -q "\./three.module.min.js" "$WORK/s1${V1}/OrbitControls.js" \
     && [ -f "$WORK/s1${V1}/three.module.min.js" ] && echo yes || echo no)"

echo
echo "the stable copies match the hashed ones -- a client on either path gets the same bytes"
check "stable glue == hashed glue" "same" \
  "$(cmp -s "$WORK/s1/fumola_wasm.js" "$WORK/s1/v/$H1/fumola_wasm.js" && echo same || echo differ)"
check "stable wasm == hashed wasm" "same" \
  "$(cmp -s "$WORK/s1/fumola_wasm_bg.wasm" "$WORK/s1/v/$H1/fumola_wasm_bg.wasm" && echo same || echo differ)"

echo
echo "a second publish carries the first forward -- the point of the whole exercise"
bindings "$WORK/b2" two
"$ASSEMBLE" --bindings "$WORK/b2" --pages "$WORK/pages" --out "$WORK/s2" \
  --previous "$WORK/s1" --commit bbbb222 >/dev/null
H2="$(hash_of "$WORK/s2")"
check "the new build has a new hash"      "different" \
  "$([ "$H1" != "$H2" ] && echo different || echo same)"
check_file "the OLD version survives"     "$WORK/s2/v/$H1/fumola_wasm_bg.wasm"
check_file "the new version is present"   "$WORK/s2/v/$H2/fumola_wasm_bg.wasm"
check "two versions are recorded"  "2" "$(version_count "$WORK/s2")"
check "newest is first"            "$H2" \
  "$(python3 -c 'import json,sys; print(json.load(sys.stdin)[0]["hash"])' < "$WORK/s2/versions.json")"
check "a pinned old URL still resolves to its ORIGINAL bytes" "same" \
  "$(cmp -s "$WORK/s2/v/$H1/fumola_wasm_bg.wasm" "$WORK/b1/fumola_wasm_bg.wasm" && echo same || echo differ)"
check "the stable copy tracks the NEW build" "same" \
  "$(cmp -s "$WORK/s2/fumola_wasm_bg.wasm" "$WORK/b2/fumola_wasm_bg.wasm" && echo same || echo differ)"

echo
echo "identical content keeps its URL, and does not appear twice in history"
"$ASSEMBLE" --bindings "$WORK/b2" --pages "$WORK/pages" --out "$WORK/s3" \
  --previous "$WORK/s2" --commit cccc333 >/dev/null
check "the hash is unchanged"      "$H2" "$(hash_of "$WORK/s3")"
check "history did not grow"       "2"   "$(version_count "$WORK/s3")"
check "the entry moved to newest"  "$H2" \
  "$(python3 -c 'import json,sys; print(json.load(sys.stdin)[0]["hash"])' < "$WORK/s3/versions.json")"
check "its commit was refreshed"   "cccc333" \
  "$(python3 -c 'import json,sys; print(json.load(sys.stdin)[0]["commit"])' < "$WORK/s3/versions.json")"

echo
echo "retention is bounded, and pruning deletes the directory as well as the entry"
prev="$WORK/s3"
for i in 4 5 6; do
  bindings "$WORK/b$i" "build$i"
  "$ASSEMBLE" --bindings "$WORK/b$i" --pages "$WORK/pages" --out "$WORK/s$i" \
    --previous "$prev" --commit "dddd$i" --keep 3 >/dev/null
  prev="$WORK/s$i"
done
check "history is capped at keep"  "3" "$(version_count "$WORK/s6")"
check "directories match history"  "3" \
  "$(find "$WORK/s6/v" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
check_absent "the oldest version's directory is gone" "$WORK/s6/v/$H1"

echo
echo "vendor is versioned independently of the runtime"
# Same vendor, new runtime: the vendor version must not churn.
check "an unchanged vendor keeps its hash across a runtime-only publish" "$V1" \
  "$(vendor_of "$WORK/s2")"
# Changed vendor: a new version, and the old one still reachable.
pages "$WORK/pages2" v2
bindings "$WORK/b7" seven
"$ASSEMBLE" --bindings "$WORK/b7" --pages "$WORK/pages2" --out "$WORK/s7" \
  --previous "$WORK/s2" --commit eeee777 >/dev/null
V2="$(vendor_of "$WORK/s7")"
check "a changed vendor gets a new hash" "different" \
  "$([ "$V1" != "$V2" ] && echo different || echo same)"
check_file "the new vendor version"       "$WORK/s7${V2}/three.module.min.js"
check_file "the old vendor version survives for pinned pages" \
  "$WORK/s7${V1}/three.module.min.js"
check "the old vendor version still has its ORIGINAL bytes" "same" \
  "$(cmp -s "$WORK/s7${V1}/three.module.min.js" "$WORK/pages/vendor/three.module.min.js" \
     && echo same || echo differ)"

# A library vendored in a directory of its own is not a version and must
# outlive the pruning of versions, which once deleted it on every publish and
# left /alignment/'s stylesheet a 404 in production.
check_file "an unhashed vendor directory survives a first publish" \
  "$WORK/s1/vendor/katex/katex.min.css"
check_file "and the files it names by a relative path survive with it" \
  "$WORK/s1/vendor/katex/fonts/KaTeX.woff2"
check_file "and it survives a publish that prunes an old vendor version" \
  "$WORK/s7/vendor/katex/katex.min.css"
check "and it is the NEW bytes, not a stale copy left by the prune" "same" \
  "$(cmp -s "$WORK/s7/vendor/katex/katex.min.css" \
            "$WORK/pages2/vendor/katex/katex.min.css" && echo same || echo differ)"

echo
echo "refusals"
set +e
out="$("$ASSEMBLE" --bindings "$WORK/b1" --pages "$WORK/pages" --out "$WORK/s1" 2>&1)"; rc=$?
set -e
check "refuses to assemble into an existing directory" "1" "$rc"
set +e
"$ASSEMBLE" --bindings "$WORK/nonexistent" --pages "$WORK/pages" --out "$WORK/s9" >/dev/null 2>&1; rc=$?
set -e
check "refuses when the bindings are missing" "1" "$rc"

# A stale runtime in the pages tree used to be copied over the fresh one, and
# only the stable root path went wrong -- the hashed copy stayed correct, so
# nothing looked broken. Adapton/fumola#87.
mkdir -p "$WORK/dirtypages"
cp -r "$WORK/pages/." "$WORK/dirtypages/"
printf 'stale glue from an older build\n' > "$WORK/dirtypages/fumola_wasm.js"
set +e
out="$("$ASSEMBLE" --bindings "$WORK/b1" --pages "$WORK/dirtypages" --out "$WORK/s10" 2>&1)"; rc=$?
set -e
check "refuses when the pages carry a stale runtime" "1" "$rc"
check "and says which file it objected to" "yes" \
  "$(grep -q "fumola_wasm.js would shadow" <<< "$out" && echo yes || echo no)"
check_absent "and assembles nothing" "$WORK/s10/runtime.json"

rm -f "$WORK/dirtypages/fumola_wasm.js"
printf 'stale wasm from an older build\n' > "$WORK/dirtypages/fumola_wasm_bg.wasm"
set +e
"$ASSEMBLE" --bindings "$WORK/b1" --pages "$WORK/dirtypages" --out "$WORK/s11" >/dev/null 2>&1; rc=$?
set -e
check "refuses for the binary as well as the glue" "1" "$rc"

echo
echo "$passed passed, $failed failed"
[ "$failed" -eq 0 ]
