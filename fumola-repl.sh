#!/usr/bin/env bash
set -euo pipefail

# The same import list as fumola-test.sh, so the two agree and the startup log
# is readable. `-L` follows the symlinks under fumola/, and `sort` makes the
# order deterministic rather than whatever readdir returned.
#
# The order is cosmetic: `--import` only registers a file, and a module body
# runs when some `import` first names it. Registering the prelude first is in
# fact impossible -- evaluating it needs system/adapton and system/hashMap
# registered already.
cargo run --release repl --import $(find -L fumola -name "*.fumola" | sort) "$@"
