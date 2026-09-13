# `do big { .. }`

A block whose evaluation strategy is named.

```motoko
do big {
  var i = 0; var s = 0;
  while (i < 20) { s := s + i; i := i + 1 };
  s
}
```

Two modes exist today. `do small { e }` is what `do { e }` already does: the
abstract machine in `vm_step` and `vm_stack_cont`, one step at a time, every
intermediate state materialized as a `Frame` on the heap. `do big { e }` asks
for the same meaning with those intermediate states on the Rust call stack.

This is the first step on [#122](https://github.com/Adapton/fumola/issues/122),
which asks whether the explicit continuation is where evaluation time actually
goes. It does not answer that question. It makes it measurable.

## The contract

`do big { e }` and `do small { e }` agree on:

- the value, or the failure, in the same words;
- the number of steps, and the number of redexes;
- the graph, including the event history the graphical strategy records.

`crates/fumola/tests/test_do_big.rs` asserts all three, for every program in it.
The counts are not decoration: the forms the recursive evaluator handles itself
never touch the graph, so graph equality says nothing about them, and the counts
are the only witness that the same transitions happened in the same order.

## How it does not go wrong

There is no snapshot, no restore, and no retry.

When the recursive evaluator meets a form it does not implement, it hands that
node to the machine **in place**. The machine steps the same `Core`, against a
watermark taken when the region was entered, and the evaluator takes back the
value the machine leaves in `cont`. Nothing was abandoned, so there is nothing
to undo.

That is what makes the graph claim structural rather than audited case by case.
Every adapton bracket — `force_begin`/`force_end`, the `goto` and `within`
navigations, repair, scratch — keeps exactly one implementation, the machine's.
A region never takes any of them over.

## What runs recursively

Binders (`let`, `var`, `func`), blocks, `do`, literals, variables, parentheses,
the unary and binary operators, the comparisons, `not`/`and`/`or`, `assert`,
tuples, `if`, `while`, assignment to a plain variable, and **calls**.

Everything else is handed to the machine: `force`, `@`, `do goto`, `do within`,
`do ?`, `!`, `return`, `switch`, `for`, objects, arrays, indexing, `.field`,
annotations, and the module, actor, import and object declarations.

### Calls

The callee is not examined here. `call_cont` is the machine's own dispatch --
a closure, a primitive, a dynamic value, an actor method, or two symbols
composing into a third -- and what it leaves behind says what to do next. A
closure pushes a real `Call3` frame and points `cont` at the body, which is the
case worth recursing into; everything else the machine finishes.

Pushing the *real* frame is what makes `return` work. `return_` scans the actual
stack for a `Call3`, so it finds this call's own, pops it, and writes the value
-- and the recursive evaluator notices that the stack came back to exactly this
call's level and takes the answer. Nothing about non-local exit is reimplemented.

Only ordinary closures are entered, so `call_function`'s context convention is
the only one in play. `call_function_def` saves the *callee's* context in the
frame where `call_function` saves the caller's; they disagree in the repo today,
and delegating actor methods keeps that unobservable rather than resolving it
here.

### Two exclusions that are not laziness

- **Indexing.** The `Idx2` frame arm looks *underneath itself* for an `Assign1`
  frame, to decide whether `a[i]` is a read or an assignment target. A recursive
  evaluator has no frame there to find, so `a[i] := v` would read the element and
  assign to that. `Assign` is in the set only for a plain variable target.
- **`force`.** Entering a thunk body is the machine's own `enter_thunk_body`,
  bracketed by `force_begin`/`force_end` across a frame, and a cache miss may
  hand off to `repair_loop`, which drives the graph's realignment through the
  stack. Delegating all of it is what makes the graph claim structural. It is
  also, as measured below, the thing that now matters most.

## What it does not do yet

**It still does not speed up `mergeSort`, and now we know why.** It is not
calls -- those are in the set. It is `force`. mergeSort is an adapton example:
its whole computation happens inside a forced thunk, and a region stops at the
edge of a force. The measurement below isolates that with a controlled pair.

**It never engages in web-play.** A region evaluated on the Rust stack is
atomic — there is no standing continuation to pause at. So when a step limit, a
redex limit or a breakpoint is set, the region is not entered at all: it runs
small-step instead, by the same `exp_conts` call `do { .. }` makes, which is why
the value and the counts are identical rather than merely intended to be. The
playground's chunked runner sets a step budget on every call, so `do big` is
inert there.

That is the right trade. An atomic region inside a chunked host is a frozen tab,
which is [#68](https://github.com/Adapton/fumola/issues/68)'s symptom
reintroduced by the feature meant to help. It also buys a debugging story: run a
misbehaving `do big` under `--step-limit` and it becomes `do { .. }` exactly,
inspectable one step at a time.

## What it is worth, measured

`cargo bench -p fumola --bench do_big`, release, one machine:

| workload | steps | small (ms) | big (ms) | ratio | noise floor |
| --- | ---: | ---: | ---: | ---: | ---: |
| loop, 0 reads | 720k | 53.8 | 21.2 | **0.39** | 1.02 |
| mixed (5k iters) | 435k | 44.0 | 18.8 | 0.43 | 0.96 |
| loop, 1 call | 1.12M | 90.1 | 40.2 | 0.45 | 1.00 |
| loop, 1 read | 880k | 72.8 | 33.4 | 0.46 | 0.94 |
| loop, 4 calls | 2.32M | 193.0 | 92.8 | 0.48 | 1.01 |
| **fib 22** | 1.49M | 136.8 | 65.8 | **0.48** | 1.03 |
| switching loop (20k) | 1.21M | 105.3 | 55.3 | 0.53 | 0.97 |
| binders (800 lets) | 5.6k | 1.71 | 1.05 | 0.62 | 0.84 |
| loop, 4 reads | 1.36M | 112.7 | 71.9 | 0.64 | 1.05 |
| loop, 16 reads | 3.28M | 311.5 | 220.0 | 0.71 | 0.94 |
| nested (depth 200) | 1.2k | 0.11 | 0.08 | 0.72 | 1.01 |
| nested calls (5k) | 75k | 7.65 | 6.44 | 0.84 | 0.91 |
| **fib 22, in a force** | 1.49M | 139.2 | 130.6 | *0.94* | 0.99 |

And `mergeSort` itself, loading the real module tree
(`cargo bench -p fumola --bench do_big_mergesort`):

| workload | steps | small (ms) | big (ms) | ratio | noise floor |
| --- | ---: | ---: | ---: | ---: | ---: |
| scene, size 8 | 329k | 87.0 | 79.3 | 0.91 | 1.01 |
| scene, size 16 | 732k | 187.2 | 183.9 | 0.98 | 1.07 |
| scene, size 23 | 1.24M | 337.6 | 342.3 | 1.01 | 1.02 |
| scene, size 44 | 2.46M | 708.3 | 692.0 | 0.98 | 1.04 |

Ratio below 1.00 means the region was faster. The noise floor is the same
program in the same mode timed again, so it absorbs warm-up; a ratio no further
from 1.00 than the floor beside it is not a result.

One pair carries the argument. **`fib 22` at 0.48 and `fib 22, in a force` at
0.94** are the same computation, to within eight steps — the second is the first
wrapped in `force (thunk { .. })`. Reachable, a region halves it; behind a
force, a region does nothing at all.

That is the whole explanation for the mergeSort table. Its computation happens
inside a force, so a region never reaches it. Nothing about mergeSort is
unusual; it is an adapton example doing what adapton examples do.

The rest is consistent with that reading. A loop the evaluator runs entirely
takes two fifths of the time at an identical step count — the continuation cost,
and most of the work. Adding delegated array reads walks the ratio back toward
1.00 (0.46 → 0.64 → 0.71 at one, four and sixteen reads per iteration), because
delegation returns that share of the work to the machine.

Calls, which the previous cut delegated, moved from 0.61 to 0.45 at one per
iteration and from 0.76 to 0.48 at four. That is the measured worth of this
change.

The read of this for the next step: **`force` is where the remaining time is**,
and it is the hardest thing in the file to bring in, because it is the one
bracket the delegation design was built to avoid touching.

### The same, in wasm

Everything above is native, release, x86_64. #122 is about the playground,
which is wasm, and #68's figures are wasm -- so the measurement is repeated
there. `./tools/measure-do-big-wasm.sh` builds the runtime the playground
ships, generates node bindings beside it, and runs the same programs through
`fumola_eval_top`, which has no step budget and so lets a region engage. Step
counts are read back through `fumola_steps_taken` -- added for exactly this --
and asserted equal before any time is printed.

| workload | steps | small (ms) | big (ms) | ratio | noise floor |
| --- | ---: | ---: | ---: | ---: | ---: |
| loop, 0 reads | 800k | 110.1 | 45.3 | **0.41** | 1.00 |
| fib 22 | 1.49M | 245.7 | 131.6 | **0.54** | 1.05 |
| fib 22, in a force | 1.49M | 191.7 | 239.7 | *1.25* | *1.39* |
| scene, size 8 | 329k | 218.0 | 213.4 | 0.98 | 0.97 |
| scene, size 16 | 732k | 516.0 | 512.7 | 0.99 | 1.03 |
| scene, size 23 | 1.24M | 912.4 | 903.9 | 0.99 | 1.03 |
| scene, size 44 | 2.46M | 2925.2 | 2708.2 | 0.93 | 0.93 |

The reachable rows transfer: 0.41 against 0.39 native, 0.54 against 0.48. The
mergeSort rows are flat on both targets, and every step count is
byte-identical to the native run -- 329125, 732492, 1240073, 2462583 -- which
is a cross-target check of the evaluator in its own right. The absolute times
agree with #68 (2.9 s at size 44, against its 2.5 s).

The `fib 22, in a force` row is **not a result** here. Its floor is 1.39: the
same program in the same mode came back 39% slower the second time, which is
#68's memory behaviour showing up as timing noise in a fresh wasm heap. It is
kept in the table because a floor that wide is exactly what the floor is for.

## The two loud failures

`do frobnicate { 1 }` is `UnknownEvalMode`. The mode is an ordinary identifier,
not a reserved word — `let big = 1` and `` `big `` still mean what they meant —
so an unrecognized mode is caught when the region is entered rather than by the
parser.

An interruption the host would ordinarily resume — an actor `Send`, a
`Response`, a module read — becomes `BigStepEscape` and does not resume.
Resuming means running the standing continuation, and the part of it that lived
in Rust frames is gone. Resuming anyway would give a wrong answer and no error.

## Depth

The machine's stack is on the heap and grows until memory does not. This one is
the native stack, and running out of it is not catchable: it aborts natively and
traps in wasm. So depth is spent rather than risked — past the budget a node is
handed to the machine, the same demotion any unsupported form gets.

The budget is **bytes, not syntax nodes**. Counting nodes was the first attempt
and does not work: a debug build's frames are several times a release build's,
so one constant leaves headroom in one and overflows the other. A cap of 256
nested forms overflowed a 2 MiB test thread in debug while being far too
cautious in release. `stack_spent` reads the distance from a local taken at the
region boundary, which is the quantity that actually matters.

This stopped being a formality when calls joined the set: depth now tracks the
user's recursion rather than the syntax. `recursion_past_the_depth_budget`
checks that crossing it changes nothing but the time.
