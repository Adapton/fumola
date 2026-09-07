#!/usr/bin/env bash
set -euo pipefail

# The same import list as fumola-test.sh, so the two agree and the startup log
# is readable. `sort` makes the order deterministic rather than whatever
# readdir returned; `-L` is kept in case a link ever reappears under fumola/,
# though there are none now that an import can name a sibling directory.
#
# The order is cosmetic: `--import` only registers a file, and a module body
# runs when some `import` first names it. Registering the prelude first is in
# fact impossible -- evaluating it needs system/adapton and the hashMap it
# reaches through `../collections/hashMap` registered already.
cargo run --release repl --import $(find -L fumola -name "*.fumola" | sort) "$@"
