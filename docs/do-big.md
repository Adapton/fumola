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
tuples, `if`, `while`, assignment to a plain variable, **calls**, and **`force`**
-- including the repair a force can set off.

Everything else is handed to the machine: `@`, the puts, `do goto`, `do within`,
`do ?`, `!`, `return`, `switch`, `for`, objects, arrays, indexing, `.field`,
annotations, `prim "adaptonScratch"`, and the module, actor, import and object
declarations.

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

### Force

This is the one place the recursive evaluator mirrors machine control flow
rather than calling it, and the reason is worth stating. `Call` reuses the
machine's reduction because `call_function` stops with the body in `cont`. The
force path does not: `enter_thunk_body` tail-calls `exp_step` on the thunk body,
so by the time it returns the body's first step is already taken, and the
machine is in a state the recursive evaluator cannot resume from.

So `vm_big.rs` carries its own `force_value`, `force_pointer`, `forced_body` and
`repair` -- some fifty lines, each naming the machine function it mirrors
(`force_pointer`, `enter_thunk_body`, `repair_loop`, and the four frame arms).
What is **not** duplicated is anything that touches the graph. `force_begin`,
`force_end`, `repair_step` and `repair_resume` are the adapton state's own
methods, called in the order the machine calls them, with the same arguments;
the frames pushed are the frames the machine pushes, field for field. The graph
keeps its own frame stack (`FrameKind`), so "which force is running" was never
VM state to begin with. That is what the graph claim now rests on -- narrower
than "the machine does all of it", and checked rather than assumed: the harness
compares the graph's event history between the two modes for every program.

The same folding sets the step accounting. A force that enters a body is *one*
machine step that already includes the body's first descend, which the body's
own arm charges -- so the force contributes only its redex, (0, 1). A force that
ends in a value, a cache hit or an aligned node, is a whole step, (1, 1).

Two things a force does not do, in either mode: a `return` or `!` that leaves a
forced thunk pops its frame without `force_end` -- the machine's `return_` scans
past it -- and the recursive version propagates `Escaped` and closes nothing,
matching that exactly rather than fixing it. And a failure inside the body
leaves the stack standing, as it always did.

### One exclusion that is not laziness

**Indexing.** The `Idx2` frame arm looks *underneath itself* for an `Assign1`
frame, to decide whether `a[i]` is a read or an assignment target. A recursive
evaluator has no frame there to find, so `a[i] := v` would read the element and
assign to that. `Assign` is in the set only for a plain variable target.

## What it does not do yet

**It speeds up `mergeSort` only a little, and the measurement says exactly
why.** Calls and forces are both in the set now, and a region does reach
mergeSort's work -- but nearly every function body in the list and tree code it
is built from opens with a `switch` or a `.field` projection, and both are
delegated. A delegated node takes its whole subtree back to the machine,
recursive calls and all. So most of mergeSort still runs small-step in both
modes, and the ratio sits at 0.91-0.95. The controlled pairs below show each
swallower in turn: `force` before this change, `switch` and record projection
now.

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

`cargo bench -p fumola --bench do_big`, release, one machine, with calls and
forces in the recursive set:

| workload | steps | small (ms) | big (ms) | ratio | noise floor |
| --- | ---: | ---: | ---: | ---: | ---: |
| loop, 0 reads | 720k | 53.8 | 21.5 | **0.40** | 1.01 |
| loop, 1 call | 1.12M | 92.9 | 39.6 | 0.43 | 0.98 |
| **fib 22, in a force** | 1.49M | 139.8 | 66.4 | **0.47** | 1.00 |
| fib 22 | 1.49M | 138.9 | 66.4 | 0.48 | 1.00 |
| mixed (5k iters) | 435k | 41.6 | 19.9 | 0.48 | 1.00 |
| loop, 4 calls | 2.32M | 194.0 | 93.5 | 0.48 | 1.01 |
| loop, 1 read | 880k | 69.4 | 33.7 | 0.49 | 0.99 |
| switching loop (20k) | 1.21M | 104.2 | 55.1 | 0.53 | 1.00 |
| loop, 4 reads | 1.36M | 118.2 | 69.3 | 0.59 | 1.00 |
| binders (800 lets) | 5.6k | 1.64 | 1.08 | 0.66 | 0.92 |
| loop, 16 reads | 3.28M | 293.7 | 218.8 | 0.75 | 1.01 |
| nested (depth 200) | 1.2k | 0.12 | 0.09 | 0.76 | 0.97 |
| nested calls (5k) | 75k | 7.15 | 6.58 | 0.92 | 0.99 |
| **fib 22, via record** | 1.89M | 204.6 | 190.9 | *0.93* | 1.01 |
| **fib 22, via switch** | 1.83M | 174.8 | 164.9 | *0.94* | 1.02 |

And `mergeSort` itself, loading the real module tree
(`cargo bench -p fumola --bench do_big_mergesort`):

| workload | steps | small (ms) | big (ms) | ratio | noise floor |
| --- | ---: | ---: | ---: | ---: | ---: |
| scene, size 8 | 329k | 100.2 | 90.8 | 0.91 | 0.96 |
| scene, size 16 | 732k | 206.9 | 190.8 | 0.92 | 1.03 |
| scene, size 23 | 1.24M | 393.6 | 373.1 | 0.95 | 1.01 |
| scene, size 44 | 2.46M | 820.9 | 743.8 | 0.91 | 0.97 |

Ratio below 1.00 means the region was faster. The noise floor is the same
program in the same mode timed again, so it absorbs warm-up; a ratio no further
from 1.00 than the floor beside it is not a result.

Three controlled pairs carry the argument, and they are the same computation
each time -- `fib 22`, 1.49M to 1.9M steps -- differing only in one form around
or at the top of it.

**`fib 22` at 0.48 and `fib 22, in a force` at 0.47.** Before forces were in
the set the second read 0.94: the same work, unreachable behind a `force`. Now
the two agree, which is the direct measurement that a region reaches through a
force and does so at no cost.

**`fib 22, via switch` at 0.94 and `via record` at 0.93.** The recursion is
identical; the body opens with a `switch`, or builds a record and projects a
field. Both forms are delegated, and a delegated node takes its whole subtree
back to the machine -- so these read like `in a force` used to. They are the
next swallowers, and they are what mergeSort is made of.

**`mergeSort` at 0.91-0.95**, down from flat. Forces are reached; most of the
work beneath them opens with a `switch` or a projection and is handed back.
Every step count is unchanged from before forces were in the set (2462583 at
size 44), which is the accounting rule holding across a much larger surface.

The rest is consistent with that reading. A loop the evaluator runs entirely
takes two fifths of the time at an identical step count. Adding delegated array
reads walks the ratio back toward 1.00 (0.49 → 0.59 → 0.75 at one, four and
sixteen reads per iteration).

The read of this for the next step: **`switch` and `Dot`/object literals are
where the remaining time is.** `switch` needs `vm_match` and a case
continuation; `Dot` is a long arm with no stack introspection, and object
literals allocate. None is a graph bracket, so none carries the risk `force`
did.

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
| loop, 0 reads | 800k | 90.1 | 39.7 | **0.44** | 0.91 |
| fib 22 | 1.49M | 187.6 | 100.8 | **0.54** | 1.05 |
| fib 22, in a force | 1.49M | 192.7 | 94.1 | **0.49** | 1.00 |
| scene, size 8 | 329k | 234.1 | 231.0 | *0.99* | *0.95* |
| scene, size 16 | 732k | 648.6 | 550.3 | *0.85* | *0.86* |
| scene, size 23 | 1.24M | 1199.7 | 1030.3 | *0.86* | *0.88* |
| scene, size 44 | 2.46M | 2025.4 | 1991.9 | *0.98* | *1.00* |

The reachable rows transfer: 0.44 against 0.40 native, 0.54 against 0.48. And
`fib 22, in a force` at 0.49 -- it read 1.25 inside a 1.39 floor before forces
were in the set -- now agrees with `fib 22`, so a region reaches through a
force in wasm as it does natively.

The mergeSort rows are **not a result in wasm**, in either direction: every
ratio sits inside its own floor. The floors are wide here -- the same program
in the same mode came back 12-14% faster the second time at sizes 16 and 23 --
which is #68's allocator behaviour in a fresh heap, and it is larger than the
~8% effect native measured with tight floors. Consistent with native, not a
confirmation of it. Every step count is byte-identical to the native run.

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
