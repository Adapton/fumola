# `fumola`

Fumola is an experimental programming language. Its surface is conventional and
borrowed from Motoko — functions, records, variants, modules — and its execution
model is **Adapton**: a program's run leaves behind a demanded computation graph
of what it named, read and demanded, and that graph is what a later run reuses.

**[fumola.org](https://fumola.org)** — what the
primitives are, where they come from, the papers, the project's history, and a
Fumola console you can run in the browser.

This file is the practical half: how to build it, run it, test it, and change it.

---

## Quick start

Fumola needs a **nightly** toolchain: `fumola_syntax` uses
`#![feature(negative_impls)]`, which stable rejects.

```bash
rustup toolchain install nightly && rustup default nightly
cargo build
```

The first build takes a few minutes — `lalrpop` parser generation dominates — so
keep the cargo cache warm rather than running `cargo clean`.

Then evaluate something:

```bash
cargo run -- eval "1 + 2"
# 3

cargo run -- eval 'force(`t := thunk { 6 * 7 })'
# 42
```

Or start a REPL with the whole Fumola library already imported:

```bash
./fumola-repl.sh
```

## The CLI

```
fumola [OPTIONS] <COMMAND>

Commands:  check  echo  format  eval  repl  test
Options:   -i, --import <FILE>...   Import one or more module files
           --log-trace / --log-debug / --log-warn
```

`--import` is how the library gets in scope. The helper scripts all pass every
`.fumola` file under `fumola/`:

```bash
cargo run -- eval 'import G "fumola/examples/gcd"; G.gcd(12, 18)' \
  --import $(find -L fumola -name "*.fumola" | sort)
# 6
```

> **Note.** `get`, `peek` and `pointer` are *not* CLI builtins. They are prelude
> helpers that the `fumola_wasm` crate defines for its JavaScript hosts, over
> `@` and `prim "adaptonPointer"`. In the CLI, `:=`, `thunk` and `force` work
> directly; the browser console on the site above is the easiest place to try
> the full set.

## Tests

Exactly what CI runs, in order:

```bash
cargo build
cargo test            # Rust unit tests
./fumola-test.sh      # Fumola unit tests (runs `fumola test` over the library)
./fumola-scripts.sh   # the example scripts in scripts/, each must exit 0
```

To see log output from a failing Rust test:

```bash
RUST_LOG=trace cargo test -- --nocapture
```

`cargo test` also covers the JavaScript boundary — `crates/fumola_wasm/tests/`
has `test_instances.rs`, `test_structures.rs` and `test_symbols.rs`.

## Using Fumola from JavaScript

The runtime is published as a wasm module on every push to `main`:

```js
import init, { fumola_create, fumola_eval_top }
  from "https://fumola.org/fumola_wasm.js";

await init();
const id = fumola_create();
fumola_eval_top(id, "`x := 41; get(`x) + 1");
// {"ok":true,"tag":"Int","value":"42"}
```

An instance is a persistent Adapton store: re-evaluating against the same `id`
reuses what the last run built. `fumola_eval` wraps your program as
``force(`name := thunk { ... })``, which is what makes an edit incremental;
`fumola_eval_top` runs it at the top level instead. `fumola_ensure_mode(id, m)`
picks the semantics — `"graphical"` keeps the DCG, `"simple"` does not.

To build the pair yourself:

```bash
rustup target add wasm32-unknown-unknown
cargo build --release --target wasm32-unknown-unknown -p fumola_wasm --no-default-features
wasm-bindgen --target web --out-dir site \
  target/wasm32-unknown-unknown/release/fumola_wasm.wasm
```

`--no-default-features` drops the `repl` feature, whose `rustyline` dependency
does not build for wasm32. The `wasm-bindgen` CLI must match the `wasm-bindgen`
crate version exactly, or the bindings will not load.

## With Hazel

[Hazel](https://hazel.org) embeds Fumola through **livelits** — graphical
literals in the program text that expand to the expression they stand for. The
integration is experimental and lives on a branch:

- [`hazelgrove/hazel@fumola-livelit-mvp`](https://github.com/hazelgrove/hazel/tree/fumola-livelit-mvp) — the source
- **[a live build](https://hazel.org/build/fumola-livelit-mvp/)** — nothing to install; open the Fumola chapters from the documentation cards
- [`docs/livelits.md`](https://github.com/hazelgrove/hazel/blob/fumola-livelit-mvp/docs/livelits.md) — the four livelits and what crosses the boundary

Hazel loads the published wasm by default. To point it at your own build,
from a Hazel checkout:

```bash
FUMOLA_REPO=/path/to/fumola ./scripts/build-fumola-wasm.sh
```

## Repository layout

| | |
| --- | --- |
| `crates/fumola` | the CLI and the library entry point |
| `crates/fumola_syntax` | AST and lexer |
| `crates/fumola_parser` | the `lalrpop` grammar |
| `crates/fumola_semantics` | the VM, and `adapton/` — `simple.rs` and `graphical/`, behind one `AdaptonState` trait |
| `crates/fumola_wasm` | the JavaScript boundary: instance store, value translation |
| `crates/fumola_proc_macro` | compile-time Fumola expressions in Rust |
| `fumola/` | the Fumola library: `collections/`, `examples/`, `system/adapton` |
| `scripts/` | runnable examples, each exercised by CI |
| `replayground-www/` | the DCG viewer: `python3 build.py` then `python3 -m http.server` |
| `vscode/` | syntax, grammar and the Fumola Dark theme |
| `pages/index.html` | the source of the site above |

## Contributing

Contributions are welcome. A few things worth knowing before a PR:

- **Nightly**, as above. There is no `rust-toolchain` file; CI installs nightly explicitly.
- **`Cargo.lock` is gitignored**, so dependency versions resolve fresh. If a build breaks for you and not for CI, that is the first place to look.
- **CI runs the four commands** under [Tests](#tests) on every PR — `cargo build`, `cargo test`, `./fumola-test.sh`, `./fumola-scripts.sh`. Running them locally first is the whole checklist.
- **A new example script** in `scripts/` becomes part of CI automatically, since `fumola-scripts.sh` globs the directory. It must exit 0.

See the [contributor guidelines](.github/CONTRIBUTING.md) for the rest.
