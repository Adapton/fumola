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
tuples, `if`, `while`, and assignment to a plain variable.

Everything else is handed to the machine: calls, `force`, `@`, `do goto`,
`do within`, `do ?`, `!`, `return`, `switch`, `for`, objects, arrays, indexing,
annotations, and the module, actor, import and object declarations.

Two of those exclusions are not laziness:

- **Indexing.** The `Idx2` frame arm looks *underneath itself* for an `Assign1`
  frame, to decide whether `a[i]` is a read or an assignment target. A recursive
  evaluator has no frame there to find, so `a[i] := v` would read the element and
  assign to that. `Assign` is in the set only for a plain variable target.
- **Calls.** Including them means choosing between `call_function`'s context
  convention and `call_function_def`'s, which disagree in the code today and are
  unobservable while both go through the machine. It also makes Rust stack depth
  track user recursion rather than syntax depth.

## What it does not do yet

**It does not speed up `mergeSort`.** Every function body is entered through a
delegated call, so `do big { M.runAll() }` is one delegation and almost no
recursive work.

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
| binders (800 lets) | 5.6k | 1.55 | 1.04 | 0.67 | 0.91 |
| nested (depth 200) | 1.2k | 0.12 | 0.07 | 0.62 | 0.94 |
| mixed (5k iters) | 435k | 41.4 | 20.2 | 0.49 | 0.96 |
| loop, 0 reads | 720k | 53.1 | 21.2 | **0.40** | 0.99 |
| loop, 1 read | 880k | 69.4 | 32.9 | 0.47 | 0.97 |
| loop, 4 reads | 1.36M | 110.9 | 72.5 | 0.65 | 1.01 |
| loop, 16 reads | 3.28M | 291.6 | 212.9 | 0.73 | 1.02 |
| loop, 1 call | 1.12M | 88.0 | 53.8 | 0.61 | 1.02 |
| loop, 4 calls | 2.32M | 192.3 | 146.3 | 0.76 | 1.02 |
| nested calls (5k) | 75k | 7.12 | 6.65 | *0.93* | 0.96 |

Ratio below 1.00 means the region was faster. The noise floor is the same
program in the same mode timed again, so it absorbs warm-up; a ratio no further
from 1.00 than the floor beside it is not a result.

Two rows carry the argument.

**`loop, 0 reads` at 0.40**: a loop whose every node is in the recursive set
runs in two fifths of the time, at an identical step count. That is the
continuation cost, and it is not a rounding error — it is most of the work.

**`nested calls (5k)` at 0.93, floor 0.96**: an expression that is one
delegation and nothing else shows no effect. Handing a node back costs nothing
measurable, which is what makes the middle rows readable.

The middle rows are the instrument: one loop, held fixed, with a rising share of
it handed back. The ratio climbs monotonically from 0.40 to 0.76 as the
delegated share grows, and the control says where it is heading. So the speedup
is not a property of the benchmark — it tracks exactly how much of the program
the recursive evaluator actually ran, which is the answer #122 asked for.

The read of this for the next step: array indexing and calls are where the
remaining time is, and both are excluded for stated reasons rather than for want
of effort.

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
bounded by the native stack, about a megabyte under wasm, and a stack overflow
is not catchable: it aborts natively and traps in wasm. So depth is spent rather
than risked — past `MAX_DEPTH` nested forms the node is handed to the machine,
the same demotion any unsupported form gets.

Unreachable as the set stands, since calls are delegated and `while` iterates
rather than recurses. It is there for the sets that come later, where it stops
being a formality.
