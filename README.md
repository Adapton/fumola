# `fumola`

Fumola is an experimental programming language.  We take inspiration from
[The Extended Adapton Recipe](https://github.com/matthewhammer/adapton-recipe),
including **Demanded Computation Graphs** extended with **symbolic space and
time**.

Fumola integrates these ideas with an otherwise conventional language, with
conventional data types.  For those features, we take inspiration from Motoko,
a language design we adopt for most "core language features" that operate
orthogonally to incremental computing primitives (e.g., primitive values,
records, variants, functions, and modules).

> **[adapton.github.io/fumola](https://adapton.github.io/fumola/)** — the same
> ideas at length, with a Fumola console you can run in the browser.

---

## The primitives

There are not many. Two ways to make a node in the graph, two ways to depend on
one, one way to look without depending, and a way to say where the nodes you
make should live.

| | |
| --- | --- |
| <code>thunk { e }</code> | Suspend a computation. A thunk is an ordinary Fumola value. |
| <code>&#96;n := e</code> | **put** — write into the cell named <code>&#96;n</code>, and evaluate to a pointer to it. |
| <code>pointer(&#96;n)</code> | The pointer that the symbol <code>&#96;n</code> names. |
| <code>@p</code> &nbsp; <code>get(&#96;n)</code> | **get** — read a cell, *recording a dependency*. <code>get(&#96;n)</code> is <code>@pointer(&#96;n)</code>. |
| <code>force(p)</code> | **force** — demand a thunk, recording a dependency. Cache hit, or cache miss. |
| <code>peek(&#96;n)</code> | Read *without* recording. Answers an option. |
| <code>do within space &#96;s { e }</code> | Names the region the cells allocated inside <code>e</code> belong to: <code>&#96;x</code> there is <code>&#96;s(&#96;x)</code>. |

A name is a **symbol**, and symbols are first-order data — an identifier
`` `hello ``, a number `7`, or an application `` `adapton(`settings) `` — so a
name can be computed. Adapton's **space** and **time** are each mostly just a
symbol; `Adapton.here()` and `Adapton.now()` give the relative ones.

Two semantics implement all of this. **Graphical** is the default, and keeps the
DCG proper: nodes, edges, each thunk's trace, and an event history that a
program can read back with `Adapton.peekEvents()`. **Simple** keeps a store and
no more — cheap and predictable, with no graph to introspect or repair.

## Example: Demand-driven `mergeSort` on a `List` of 44 input elements

<img width="1357" height="1105" alt="lazyMergeSort on 44 elements, as a Demanded Computation Graph (DCG)." src="https://github.com/user-attachments/assets/c34cd768-0b55-4110-8cb8-33ef4c455a75" />

**Interactive view.** For interactive control over the image above, see the
[interactive version here](http://matthewhammer.org/replayground/mergeSort44.html).
This **replayground** demo features interactive accommodations for exploring the
data stored in the picture, not included in the _"high level"_ image above.

**Description of the image.** The image above depicts the dynamic behavior of
`lazyMergeSort` on 44 unsorted input elements, as a **Demanded Computation Graph
(DCG)**, from a _"high level"_ vantage point, literally.

- Input list of 44 elements are the green boxes in far left column.
- Balanced "level tree" of 44 leaves is central, also green boxes, and
  constructed to the right of this input list.
- The thunks that "do" things in the DCG are blue orbs, with different kinds of
  actions (`put`, `force`, `get`) drawn as graph edges coming out of each thunk
  orb doing that action.
- To the right of this balanced tree, the sub-graph of `merge` thunks constructs
  the final sorted list, on demand.  It extends from left to right,
  horizontally.
- The tree root's stream of `merge` nodes (longest, central row of horizontal
  orbs) has the 44 elements in sorted order because we've demanded the full
  output.

**`fumola` code for the image:**

```motoko
let seed = 10;
let size = 44;
let inputArray = R.generateRandomInput(seed, size);

let inputList_ = `ListFromRandomArray := thunk {
    do within space `inputList {
        List.fromIter(inputArray.vals())
    }
};
let inputList = force(inputList_);

let inputTree_ = `LevelTreeFromList := thunk {
    do within space `inputTree {
        Seq.fromList(inputList)
    };
};
let inputTree = force(inputTree_);

let lazySorted_ = force(`lazyMergeSort := thunk {
    do within space `lazyMergeSort {
        Seq.lazyMergeSort_(inputTree)
    };
});

let sorted = force(`forceSort := thunk {
    do within space `forceSort {
        LazyList.takeN_(lazySorted_, size);
    };
});
```

## Fumola in Hazel

[Hazel](https://hazel.org) is a live functional programming environment: every
edit state has a meaning, so a program has a result even while it is incomplete.
An experimental integration embeds Fumola in Hazel through **livelits** —
user-defined graphical literals in the program text, which expand to the
expression they stand for.

It lives on the
[`fumola-livelit-mvp`](https://github.com/hazelgrove/hazel/tree/fumola-livelit-mvp)
branch, which has a [live build](https://hazel.org/build/fumola-livelit-mvp/)
you can open.

```
# declare a runtime, and the Adapton semantics it runs #
let rt : Int = ^fumola_new(7, Graphical) in

# run a program as a named thunk, and use its result as a Hazel value #
let answer : Int =
  ^fumola_put_force(7, "`gcd", "Gcd.gcd(12, 18)") in

# read the graph the run left behind, as a table of records #
let events : [EventRow] =
  ^fumola_eval(7, "Adapton.peekEvents()") in

^^probe_table(events);
```

`^fumola_put_force` wraps its program as ``force(`n := thunk { … })``, so it
runs on a forceful stack — **archivist** mode, where the tracked operations
live. That wrapper is also what gives an edit its incremental meaning: editing
re-assigns the *same* name and re-forces it, so the thunk's history is reused.
`^fumola_eval` runs at the top level with no thunk around it — **editor** mode,
where `peek`, `reset` and the graph introspection belong.

Fumola's values cross into Hazel as Hazel values: numbers, floats, booleans,
tuples, records as labeled tuples, variants, arrays as lists, options, symbols
as their text. A pointer crosses as a reference carrying what it points at.
Values whose meaning depends on the runtime — a thunk, a function — cross
opaque, carrying the source Fumola prints for them.

The runtime that Hazel loads is this repo's `fumola_wasm` crate, compiled to
WebAssembly and published to
[adapton.github.io/fumola](https://adapton.github.io/fumola/) by
[`.github/workflows/wasm-pages.yml`](.github/workflows/wasm-pages.yml).

## Publications

Two papers in particular are what Fumola implements.

- **[Adapton: composable, demand-driven incremental computation](https://doi.org/10.1145/2594291.2594324)** —
  Matthew A. Hammer, Khoo Yit Phang, Michael Hicks, Jeffrey S. Foster.
  PLDI 2014, pp. 156–166.
  *Adapton itself: demand drives the computation, and the graph of what that
  demand touched is what a later run repairs.*

- **[Incremental computation with names](https://doi.org/10.1145/2814270.2814305)** —
  Matthew A. Hammer, Jana Dunfield, Kyle Headley, Nicholas Labich,
  Jeffrey S. Foster, Michael Hicks, David Van Horn. OOPSLA 2015, pp. 748–766.
  *Names, made explicit and first-class — what this paper calls a* name *is
  what Fumola spells with a backtick.*

Further back, Adapton descends from self-adjusting computation, whose root is
[Adaptive functional programming](https://doi.org/10.1145/503272.503296)
(Acar, Blelloch, Harper; POPL 2002).

On the Hazel side:
[Hazelnut](https://doi.org/10.1145/3009837.3009900) (POPL 2017),
[Live functional programming with typed holes](https://doi.org/10.1145/3290327)
(POPL 2019), and
[Filling typed holes with live GUIs](https://doi.org/10.1145/3453483.3454059)
(PLDI 2021), the livelits paper.

## Drafts

A line of work on **typing** these primitives rather than only running them: if
names decide what gets reused, then using one name for two different things is a
bug a type system ought to catch.

- **[Fungi: Typed incremental computation with names](https://arxiv.org/abs/1808.07826)** —
  Hammer, Dunfield, Headley, Narasimhamurthy, Economou. arXiv:1808.07826.
- **[Refinement types for precisely named cache locations](https://arxiv.org/abs/1610.00097)** —
  Hammer, Dunfield, Economou, Narasimhamurthy. arXiv:1610.00097.

They stayed drafts — neither the theory nor the implementation was finished at
the time — but the ideas did not stop being right, and reviving them is current
work. In 2025, conversations with
[Bor-Yuh Evan Chang](https://www.colorado.edu/cs/bor-yuh-evan-chang) at CU
Boulder gave the direction fresh support and a new framework to carry it; his
Fall 2025 graduate seminar there, *Program Analysis: Theory and Practice*
(CSCI 4830/7135), is where that framing is being worked out.

Type-and-effect checking a Fumola program is itself a computation over a
structure that is edited a little at a time — which is exactly the shape of
problem Fumola exists to make incremental. So the checker is not only something
to build *for* Fumola; it is something to build *in* it.

## On-going work

- **Fumola semantics** performs **realignment** on DCGs via **signaling** and
  **repair** algorithms (Adapton Recipe semantics).
- More example algorithms and data structures.

## Future work

- **Replayground** permits live editing of the program being displayed, like in
  [Hazel](https://hazel.org).
- Use Fumola to author the UI aspects of the **Replayground** experience, now
  created by a static file exported from Fumola demo programs into static
  HTML/JS.

## Contributing

Contributions are welcome! Please check out the
[contributor guidelines](.github/CONTRIBUTING.md) for more information.
