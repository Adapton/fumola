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
recursive work. `cargo bench -p fumola --bench do_big` reports the spread,
with a noise floor beside each figure.

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
