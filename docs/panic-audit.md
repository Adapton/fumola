# Every panic evaluation could reach, and what it is now

A host that embeds the Fumola VM had two ways for a program to fail. One it
could handle: `step` and `run` return `Result<_, Interruption>`, and an
interruption says what stopped and where. The other it could not: somewhere
below, a `todo!()`, an `unreachable!()` or an `.unwrap()` ended the process.
In the CLI that is a stack trace where a result should be. In the wasm build
it is a trap, and the instance behind it is gone -- every cell, every cached
thunk, the whole Adapton graph -- for a program that merely used a form the VM
does not implement.

This is the audit of that second path, and the record of what each site became.

## What the audit covered

Everything an evaluating program can reach: the stepper and its continuations
(`vm_step`, `vm_stack_cont`, `vm_core`), the operations and prims (`vm_ops`,
`vm_prim`, `vm_match`), definitions and imports (`vm_def`, `vm_types`),
values and quotation (`value`, `quoted`), the Adapton engine (`adapton/`),
the pretty printer (`format`, reached from `debugShow` and from
`Value::portable_hash`), the syntax the evaluator walks (`fumola_syntax::ast`),
and the two host surfaces that call all of it (`crates/fumola/src/bin`,
`crates/fumola_wasm/src`).

Not covered: `#[cfg(test)]` code, `crates/fumola/src/lib/check.rs` (assertion
helpers, whose job is to fail loudly), and `module_path.rs` (all of its
assertions are its own tests).

## The count

| | before | after |
|---|---|---|
| `panic!` | 5 | 0 |
| `unreachable!` | 10 | 0 |
| `todo!` | 115 | 0 |
| `unimplemented!` | 2 | 0 |
| `.unwrap()` / `.expect()` | 115 | 71 |

Zero is now enforced: `tools/check-no-panics.sh` runs in `rust.yml` before the
Rust build, over the same roots and with the same three exclusions, and refuses
a pull request that reintroduces one. It strips comments before matching, so a
macro *named* in prose -- as several are in this very file -- does not read as
a macro *called* in code, and it self-tests its own matcher so that it cannot
quietly stop catching things.

Counts are over `crates/*/src`, excluding `check.rs`, `prelude.rs` and
`module_path.rs` (test-only, see above) and excluding commented-out lines. The
71 remaining unwraps are accounted for below: each is either not reachable from
evaluation, or reachable only past a check a few lines above it.

## Confirmed reachable, with the program that reached it

Each of these was run against the CLI before the change. Each ended the
process; each now returns an interruption. They are the cases in
`crates/fumola/tests/test_no_panics.rs`.

| program | was | is |
|---|---|---|
| `` `(if true 1 else 2) `` | `quoted.rs:252` `todo!()` | `NotYetImplemented`, "an if expression inside a quotation" |
| `let cases = `{case 0 0} # `{case 1 1}; switch 0 ~cases` | `ast.rs:252` `panic!()` | `NotYetImplemented`, "an unquote still standing where a switch's cases were expected" |
| `actor { public func f() { 0 } }` | `vm_step.rs:406` `todo!()` | `NotYetImplemented`, "an actor declaration with no name" |
| `prim "reifyCore" ()` | `vm_prim.rs:368` `todo!()` | `NotYetImplemented`, "the reifyCore prim" |
| `prim "adaptonPoke" (1, 2, 3)` | `vm_prim.rs:360` `todo!()` | `NotYetImplemented`, "adaptonPoke with a third argument" |
| `prim "fastRandIterNew" (?0, ?10)` | `format.rs:372` `todo!()` | prints as `@opaque (pointer (0,#agent))` |
| `let m = module { #[test] public func f() { assert true } }; 0` | `vm_def.rs:453` `.unwrap()` on an empty import stack | evaluates; the test is filed under `<program>` |

`quoted.rs` was the largest single surface: 54 of its 61 live `todo!()`s were
one arm each of the `Pat`, `Dec` and `Exp` matches in `quoted_close`, so nearly
every syntactic form stopped the VM when it appeared inside a quotation --
`if`, `switch`, `while`, assignment, projection, `and`, `or`, comparison,
indexing, `label`, `return`, `assert`, `import`, variants, objects. Every one
of those functions already returned `Result<_, Interruption>`; the arms were
written before there was an interruption to return.

## Reachable by construction, not demonstrated with a program

Live code, in a signature that already carried `Result`, guarding an invariant
that a program could break but that no short program was found to break. All
converted, because converting them costs a `match` arm:

- `vm_step.rs`: a named `actor` replacing a non-actor definition; a let-var
  return naming an identifier its own environment does not bind; a frame
  continuation kept for debugging outliving its step.
- `vm_def.rs`: re-inserting a definition that had none to replace; entering,
  re-entering or leaving a definition context the table does not hold;
  finishing an import whose path is not on top of the import stack; a module
  definition producing a value that is not a module.
- `vm_core.rs`: calling into an actor already running; responding from the
  agent; upgrading an actor whose field is neither a `var` nor a function;
  creating an actor over one that exists.
- `vm_types.rs`: allocating a named pointer over one already in the store;
  stepping an array iterator whose position belongs to another store.
- `adapton/graphical/mod.rs`: an edge id with no edge; forcing a thunk with no
  cached result; ending a force that no force began.
- `convert/ser.rs`: serde's `serialize_value` before `serialize_key`.
- `vm_stack_cont.rs`: the three steps of an Adapton put-force-thunk, which the
  frame continuations name and the stepper does not implement.

## Where an interruption was not the answer

Four places have no error path to take, because their signature has no room
for one. Each was made total instead -- it answers, and evaluation continues.

**The pretty printer** (`format.rs`). `ToDoc::doc` returns a document, and it
is reached from `Value::portable_hash`, which is called while hashing a symbol
into a level. A `Hash` implementation cannot return an `Interruption`. So a
form the printer does not cover now renders as `@unrendered("...")`, naming the
form, in the same spirit as the `let-module-TODO` placeholders already there.
Several arms turned out to have a real rendering to give instead -- characters,
opaque pointers, actors, actor methods, `or`-patterns, temp vars, `reifyCore`,
`reflectCore` -- and those print properly now rather than as a marker.

Nothing that printed before printed differently *as a result of this change*,
so no hash moved and no level assignment shifted. That mattered:
`portable_hash` sets the level of every symbol, which sets level-tree shape and
the mergeSort numbers, so a printer change is a measurement change.

It did not stay true of the printer for long, and why is worth recording.
Filling in the `(Some(_), None)` arm gave it `vector(&bases.vec, "and")`,
copied from the arm beside it -- and that separator was already wrong.
`strict_concat` puts its separator against the item on its *left*, so
`{ x and y }` printed as `{xand y}`. Which parses: as one identifier applied to
another. The `todo!()` had been standing in front of a formatting bug, and
filling it in gave that bug a second home.

[#121](https://github.com/Adapton/fumola/pull/121) fixed both sites, and the
`Call` arm, which had lost its separator the same way (`f x` printing as `fx`).
Two things to carry forward from it:

- **Making a printer total can propagate a bug the `todo!()` was hiding.** Look
  at what the arm beside the one you are filling in actually does, rather than
  copying it.
- **#121 does change what prints**, so the hashing question came back. Its
  answer: `Symbol::Call` has its own arm and never reaches `Exp::Call`, so the
  only route from a symbol into changed output is `Symbol::QuotedAst` over
  syntax containing a call or a multi-base object. Nothing in the repo does --
  `./fumola-scripts.sh` regenerated every measurement file byte-identically --
  but a program that quotes a call now hashes differently than it did.

**Source positions** (`ast.rs`, `Source::expand`). Widening one source against
another stopped on every pair that was not two spans, and quoted syntax carries
`Source::Evaluation` while the prelude carries `Source::CoreInit` -- both get
spliced beside code that came from a file. Now the side that names a place in a
file wins, and two non-spans widen to `Unknown`.

**Ordering** (`adapton/mod.rs`, `vm_types.rs`). `PartialOrd::partial_cmp`
already has a way to say "these are not ordered": `None`. Two symbol kinds with
no rule between them now answer that. `ord_id_pos_option` compares two
anonymous things as `Equal`.

**Structural equality** (`value.rs`). `PartialEq` for a dynamic value has no
structure to compare -- the contents are a Rust object this crate knows nothing
about -- so two of them are equal when they are the same one. Comparing them
used to stop the VM.

## The `Active` accessors, and the one check that covers them

`Core::cont`, `env`, `store`, `counts` and `adapton` each hand back a `&mut`
into the scheduled agent's state, and unwrap the actor out of `actors.map` on
the way -- 27 unwraps across the `Active` and `ActiveBorrow` implementations. A
borrow has no room to carry an `Interruption` out, and making the whole
`Active` trait fallible would touch every line of the stepper.

So the question those unwraps cannot ask is asked once, before any of them
runs: `Core::check_schedule_choice`, called at the top of `Core::step`. Every
path that steps a core comes through `step`, so a core whose `schedule_choice`
names an actor the map does not hold, or one with no activation, now stops with
`ActorIdNotFound` or an `Impossible` rather than inside an accessor.

## Not reachable from evaluation

Listed so the next person does not have to work them out again.

- **`vm_ops.rs`, 14 unwraps.** `BigUint::to_bigint` is always `Some`, and
  num-bigint 0.4's `to_f64` returns `Some(inf)` on overflow rather than `None`
  -- checked against the pinned version, and against `10^400 + 1.5`, which
  evaluates to `inf`. The remaining one parses the constant `"256"`.
- **`vm_match.rs`, 4 unwraps.** Indices from `0..ps.vec.len()`, past a check
  that `ps.vec.len() == vs.len()`.
- **Counter exhaustion**: `next_pointer` and `next_ctx_id` are `usize`, and
  each increment stores a value first. Exhausting one needs more allocations
  than a machine can hold.
- **`package.rs`**, both crates: `include_str!` of a JSON file compiled in. A
  malformed one is a build-time bug, not a runtime path.
- **`lexer.rs`**, both crates: `iter.next()` after `iter.peek()` returned
  `Some`; `block_start` set wherever `nest_depth` left zero.
- **`vm_core.rs:477`**, `Core::empty` running an empty program.
- **`fumola_wasm/src/lib.rs`**: a key taken from the map being read; a
  coercion the `if` guard just performed.
- **`crates/fumola/src/bin/fumola.rs:270`**: inside `if false`.
- **`Exp::object_body`** was the last `panic!` in non-test code, and is gone.
  Its three callers were all in `parser.lalrpop`, applied to the result of
  `Exp::obj_field_fields`, `obj_id_fields` or `obj_base_bases` -- each of which
  wrapped a body in `Exp::Object` for the grammar to unwrap again a moment
  later. The wrap and the unwrap cancelled: the three now return
  `ExpObjectBody`, the grammar takes it as it is, and there is no arm left to
  panic in. Parse time, not evaluation, so no program reached it either way.

## Reading an interruption

`Interruption` and `fumola::Error` now implement `Display`, so a host has one
sentence to show:

```
not yet implemented: an if expression inside a quotation (at crates/fumola_semantics/src/lib/quoted.rs:252)
```

rather than the `{:?}` dump that had the message buried in it. The CLI's
`report_error` prints the sentence and logs the dump at debug level; the wasm
build puts the sentence in the `error` field, beside the `trace` and `frames`
it already carried.

Two interruptions carry a `CoreSource`, which is the file and line in the VM
to quote when reporting a VM bug:

- `NotYetImplemented(CoreSource, Option<String>)` -- the program asked for
  something the VM does not implement. Already existed; `nyi!` builds it.
- `Impossible(CoreSource, Option<String>)` -- the VM believed something about
  its own state and the thing was not so. Was a unit variant with no payload;
  now carries the same pair, built by `impossible!`, which is the macro that
  replaced `unreachable!`.
