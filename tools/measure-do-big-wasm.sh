#!/bin/sh
#
# `do big` against `do small`, in the wasm build the playground ships, run from
# node. The native benches are `cargo bench -p fumola --bench do_big` and
# `--bench do_big_mergesort`; this is the same measurement on the other target,
# which is the one issue #122 is actually about.
#
# It goes through fumola_eval_top, which runs with no step budget, so a `do big`
# region does engage. The chunked entry points would not do: setting a budget is
# exactly what makes a region decline to run, and a measurement through them
# would time the small-step machine twice and call the result a null.
#
# Lives in tools/ rather than scripts/ on purpose. fumola-scripts.sh runs every
# script in scripts/ in CI and expects each to exit 0; this one takes minutes and
# needs a wasm toolchain.
#
# The wasm-bindgen CLI has to match the wasm-bindgen crate exactly or the
# generated bindings will not load. If the installed CLI does not match, a
# matching one is installed into a temporary --root -- not over whatever is in
# ~/.cargo/bin, which may be wanted for something else.

set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
cd "$root"

version=$(cargo metadata --format-version 1 \
    | jq -r '.packages[] | select(.name == "wasm-bindgen") | .version' | head -1)
[ -n "$version" ] || { echo "could not determine the wasm-bindgen version" >&2; exit 1; }

if [ "$(wasm-bindgen --version 2>/dev/null | awk '{print $2}')" = "$version" ]; then
    bindgen=wasm-bindgen
else
    tmp_root="${TMPDIR:-/tmp}/fumola-wasm-bindgen-$version"
    if [ ! -x "$tmp_root/bin/wasm-bindgen" ]; then
        echo "installing wasm-bindgen-cli $version into $tmp_root (not over ~/.cargo/bin)"
        cargo install wasm-bindgen-cli --version "$version" --root "$tmp_root"
    fi
    bindgen="$tmp_root/bin/wasm-bindgen"
fi

# --no-default-features drops the repl feature, whose rustyline dependency
# pulls in fs2, which does not build for wasm32.
cargo build --release --target wasm32-unknown-unknown -p fumola_wasm --no-default-features

out=$(mktemp -d)
"$bindgen" --target nodejs --out-dir "$out" \
    target/wasm32-unknown-unknown/release/fumola_wasm.wasm
cp tools/measure-do-big-wasm.js "$out/"
cd "$out" && node measure-do-big-wasm.js
