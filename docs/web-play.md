# The Fumola web playground: requirements, design, and measurements

**Artifact:** <https://fumola.org/web-play> · **Source:** `pages/web-play/index.html`,
`pages/vendor/` · **Status:** running; 32 of 32 library tests execute green in the browser,
alongside seven examples and four example pairs.

This document records what was built, why each part exists, and what was measured. It is
organised by requirement rather than chronologically. §10 places the playground in the
project's history, which begins in 2022 and reaches Adapton in 2025. §6 lists defects in the
Fumola implementation that were found by building the tool, and is the section most likely to
matter outside this page.

Three companion documents cover ground this one only summarises: how the pair view's encoding
was arrived at and what broke on the way, how to present the demo, and what an edit costs a
demanded computation graph. They are listed in §13.

---

## 1. Scope

The playground is a single static HTML file plus a vendored copy of three.js, served from
GitHub Pages, running the Fumola runtime compiled to WebAssembly. There is no build step, no
framework, and no runtime network dependency.

It replaces a small "try it" box on the Fumola homepage. It is intended to overlap with, and
eventually replace, `replayground-www/`, an existing DCG viewer that renders a scene captured
to a JSON file in advance.

Its subject has widened since. It began as a way to run Fumola in a browser and show one run;
it is now also a way to show the difference between **two** runs, which is what a tool about
incremental computation has to be able to do. That is what `#[examplePair]` (§4.4) and the pair
view (§9) are for, and it is the direction the remaining work points in: incrementally built
scenes (§11.5) and scene deltas (§11.10).

---

## 2. Requirements, and what each produced

Each row states the observation that motivated the requirement and the feature built for it.

| # | Observation | Feature |
| --- | --- | --- |
| R1 | The homepage box showed unhighlighted text | Highlighting from `fumola_tokens`, the language's own lexer |
| R2 | Evaluation required pressing a button | Re-evaluation on input, debounced |
| R3 | The library was not visible from the editor | Module tree with source view |
| R4 | Library tests were not runnable without knowing their paths | `tests/` section; clicking runs the invocation |
| R5 | Reading a test required leaving the page | Clicking a test also opens its module at that test's marker |
| R6 | Modifying a test required copying it by hand | `body` button: inserts the test's code, imports re-addressed, module names qualified |
| R7 | `prim "print"` output was unreachable in a browser | `print` panel; output drained across the wasm boundary |
| R8 | Graph activity was not observable | `events` panel |
| R9 | Runs were not independent of each other | Clean store each run by default; a toggle retains state |
| R10 | Errors reported no location | Error line number, and the line banded in the editor |
| R11 | Library source had no line numbers | Gutter, sticky under horizontal scroll |
| R12 | Examples could not be shared | URL anchors and a copy button |
| R13 | Editing a test body fails one assertion at a time | `// assert` button: comments out all assertion statements |
| R14 | A program had no way to state how it should be drawn | `#[example]`, returning the components directly |
| R15 | Drawing a scene polluted the graph being drawn | `A.scratch`, a new primitive |
| R16 | Scenes were not rendered | 3D viewport; outline panel |

---

## 3. Design decisions

**3.1 Highlighting uses the language's lexer, not an editor component.**
Monaco or CodeMirror would each require a second Fumola grammar, maintained separately.
`fumola_tokens` was exported across the wasm boundary instead, and the page colours tokens
using the class names of the VSCode theme. Consequence: highlighting cannot drift from the
language. Cost: no completion, folding or multi-cursor.

**3.2 The editor is a transparent `<textarea>` over a highlighted `<pre>`.**
The standard technique for 3.1. It requires the two layers to agree on every metric; two
defects arose from that and are recorded in §7.

**3.3 Source rewriting operates on the token stream.**
Three features rewrite Fumola source: qualifying names in a pasted test body (R6), locating
assertion statements (R13), and identifying attribute spans for highlighting. Each scans
`fumola_tokens` output rather than applying regular expressions, so braces and keywords inside
strings and comments cannot affect the result.

**3.4 Scene components are supplied by the program, not extracted from it.** See §4.

**3.5 three.js is vendored (692 KB, `pages/vendor/`) and imported lazily.**
`replayground-www` imports three.js and immutable.js from `esm.sh` and `unpkg` at runtime. The
playground has no other runtime dependency, so the library is checked in and loaded by dynamic
`import()` the first time a scene is drawn. Opening the page does not fetch it.

**3.6 Anchors identify a function by definition site, not position.**
`#test=collections/hashMap:Test.testAll` names the module a function is defined in and its
qualified name within that module. A line number would be invalidated by any edit to the file;
an index by any reordering. This anchor is invalidated only by moving the function to another
module. It is removed as soon as the editor content differs from what was loaded.

---

## 4. `#[example]`, `#[examplePair]`, and what a program hands over

### 4.1 Motivation

A scene in `replayground-www` has three components: `objects` (positions in space), `outline`
(a force tree), and `history` (events, nodes, edges). The history is recorded by the runtime
and requires no decisions. The other two encode claims about what a computation means — which
force tree is worth reading, and where a structure's parts belong spatially. Neither is
derivable from the history: nothing in an event log distinguishes an input list from a merge
network, or states that they should be drawn a level apart.

The design consequence is that scene components are *offered by the program* rather than
extracted from it.

### 4.2 Interface

`fumola/system/webPlay.fumola`:

```fumola
public type SceneComponents = {
    objects : [ A.Scene.SceneObject ];
    outline : [ A.Outline.Outline ];
};
public type ExampleOutput = SceneComponents;
```

`objects` is `A.Scene.Scene`'s own array of `SceneObject`. `outline` is a forest: zero or more
trees, in the order the provider chose. A session has one force tree per force begun from
outside any other, so a single tree is the special case rather than the rule, and an example
wishing to show two need not choose between them. Both components use one convention for
absence: an empty array denotes a component the author did not supply.

Constructors `scene`, `sceneOfObjects`, `sceneOfOutline`, `sceneOfOneOutline` and
`sceneOfForces` are provided; the last offers every top-level force in the order forced, and
requires no decisions from the author.

`withHistory` adds the runtime's history.

The `#scene` tag these values once carried has been dropped. A record carrying `objects` and
`outline` is recognisable as a scene by having those fields, and requiring a wrapper only meant
that code already holding the right shape had to take it apart and put it back together —
`A.Scene.Scene` is such a record already.

### 4.4 `#[examplePair]`

An example may instead answer with two related runs and the difference between them:

```fumola
public type ExamplePairOutput = {
    left : SceneComponents;
    right : SceneComponents;
    sizes : DiffSizes;      // the four counts
    keys : DiffKeys;        // leftOnly, rightOnly, notEqual, as [Text]
};
```

Two decisions in that shape were forced by measurement rather than taste.

**The counts are stated, not derived.** `A.Diff.NodeVals` is a `Map`, whose representation is
hidden, so it crosses the boundary as an opaque *rendering* — a string, not a structure. The
first version counted it with `Array.isArray` and every panel confidently reported "0 of 0 nodes
differ" while the CLI reported 60 equal, 16 not equal, 4 only left. Parsing that rendering in
JavaScript would have been the wrong repair.

**The histories do not cross.** They are what the diff is computed *from*, not what anyone looks
at, and they were 69% of the payload. `keys` carries what a host needs to say *how* two runs
differ: three lists of names, and the assumption that anything unnamed was named by both runs and
agreed. A name is the same `Text` an object carries as its `label_`, because both come from
`debug_show` of the node's pointer — so a host matches an object against these lists directly,
with no second key scheme to keep in step.

The outlines do not cross either, until their panel is opened. See §8 for what each of those cost.

### 4.5 Contract

- An `#[example]` takes `()` and must succeed when applied to `()`. This is not enforced by the
  playground; it is a lint for CI and the CLI, and is currently unimplemented.
- The playground's trigger is the **value** the editor evaluates to, not the presence of the
  attribute. Any expression carrying `objects` and `outline` fills the panels. `#[example]` is
  how a library function opts into producing such a value and into being listed.
- `#[example]` required no parser or lexer change: attributes are already generic in the AST,
  and `handle_test` matches only the identifier `"test"`, so `#[example]` parsed and was
  ignored before any Rust was written.

### 4.4 Examples supplied

| Example | objects | outline trees | derived from |
| --- | --- | --- | --- |
| `List.exampleList` | 7 | 1 | `testFromIter`, `testSceneObjects` |
| `LazyList.exampleLazyList` | 0 | 1 | `testTakeN_` |
| `levelTree.Scene.exampleLevelTree` | 37 | 1 | `testGeom2d` |
| `mergeSort.exampleMergeSort` | 279 | 0 | `generateSceneFullDemand` |

`exampleMergeSort` supplies no outline and `exampleLazyList` no objects, so both halves of the
optional case are exercised by real examples.

`exampleLazyList` offers no objects for a reason worth recording: the only layout function for
a lazy stream is `sceneObjectsMergeSortStream`, whose third argument displaces each cell by an
offset meant to separate the parts of a merge. A list that merges nothing has no meaningful
offset, and passing zero places every cell at the origin. Offering no objects is correct until
a lazy list has a layout of its own.

---

## 5. `A.scratch`

### 5.1 Problem

Computing a scene's layout is itself a Fumola computation, and the DCG records it. Measured on
`mergeSort.generateSceneFullDemand(10, 10, null)`:

| History taken | events | nodes |
| --- | --- | --- |
| after the subject computation, before layout | 504 | 184 |
| after layout | 2043 | 657 |

Four fifths of the recorded history is the act of drawing rather than the subject.

### 5.2 Semantics

`A.scratch(thunk { … })` evaluates its argument against a branch of the adapton state, returns
the value, and restores the state. It stands to a computation as `peek` stands to a cell read:
performed from outside, and not recorded.

Implementation: one `FrameCont::Scratch(Box<adapton::state::State>)` variant, pushed by
`prim "adaptonScratch"` in the same manner as `force` pushes `ForceThunk`, with a completion
arm that restores. The VM already restores `env`, `ctx`, `cont_prim_type` and `source` on every
frame pop; the scratch frame adds the adapton state.

Two properties required deliberate handling.

**Naming counters advance; descriptive state is restored.** `meta_time` and `next_edge_id` take
the maximum of the saved and reached values, while node tables, edge tables, stack and cursor
are restored. A value computed on a branch can carry node identifiers out of it; were the
counters rolled back, the session would reissue those identifiers to different nodes and an
escaped value would read another node's content without error. Measured with rollback, two such
identifiers were byte-identical; with counters advanced they differ. An escaped identifier now
denotes nothing, which is detectable.

**Failure restores.** The VM does not unwind its stack on an `Interruption` — the stack is left
intact so error reporting can read it — so a frame's completion arm does not run. Measured
before this was handled: a scratch that wrote `` `kept := 999 `` and then failed an assertion
left the session reading `?999` where it had held `41`. `Core::run` now restores from the
outermost scratch frame on any interruption that ends the computation: all except `Done`,
`Send`, `Response`, `Breakpoint` and `Limit`, the last two being pauses a caller resumes from.

Not restored, deliberately: bindings, imports, printed output, written files, and mutable `var`
allocations. Each is named by a counter whose values a returned value may carry out; rolling
them back would leave escaped names aliasing rather than dangling. The guarantee concerns the
adapton store.

### 5.3 Effect

`levelTree.Scene.exampleLevelTree`, which performs its layout inside a scratch:

| Layout performed | objects | events | nodes |
| --- | --- | --- | --- |
| inside `A.scratch` | 37 | 44 | 21 |
| in the session | 37 | 702 | 212 |

Identical output; history reduced by a factor of sixteen.

---

## 6. Defects found in the Fumola implementation

Both were found because the playground exercises code paths the CLI and test runner do not.

### 6.1 Module bodies captured the host's top-level environment

**Symptom.** Four library tests failed in the browser; `hashMap.Test.testAll` with a
`TypeMismatch`.

**Mechanism.** Every module-level `func` captured `active.env()` as its closure environment at
first import (`vm_def.rs:576`), and `var_step` consults that environment before the lexical
definition chain, unconditionally (`vm_step.rs:41`). A host's top-level bindings therefore
became additional scope inside every module elaborated afterwards, outranking that module's own
definitions. With the prelude's one-argument `get(s)` bound, `assert (get(m, 1) == ?1)` at
`hashMap.fumola:31` — denoting hashMap's two-argument `get` — resolved to the prelude's.

**Isolating evidence.** `fumola/system/hashMap.fumola` is a symbolic link to
`../collections/hashMap.fumola`, so one file is registered under two paths, and those paths
fall on opposite sides of the prelude binding: `system/` is elaborated during the prelude's own
import, before the names exist; `collections/` afterwards. Identical bytes; `system/hashMap`
returned `()` and `collections/hashMap` raised `TypeMismatch`.

**Also relevant.** `fumola test` reported this function passing. `Core::call_function_def`
invokes a function with an empty environment (`vm_core.rs:504`), bypassing the captured closure
environment, so the test runner cannot observe this class of defect.

**Fix.** Module bodies are elaborated with no ambient environment (PR #58). The regression tests
use `fumola_eval_top`, since a `.fumola` test cannot observe the defect.

**Tested and rejected as the cause.** Import order. `--import` registers files into a map and
module bodies are elaborated lazily on first import; prelude first, last, and reverse-sorted
produced byte-identical failures. The decisive order is the binding, which is not reachable
from the command line.

### 6.2 Symbol hashing depended on target pointer width

**Symptom.** Three library tests asserting on exact graph statistics produced smaller figures in
the browser than in the CLI, from byte-identical input.

**Mechanism.** `prim "symbolLevel"` hashed `Symbol`, a `#[derive(Hash)]` enum. A derived `Hash`
writes an enum's discriminant as `isize`: eight bytes on x86_64, four on wasm32. Every symbol
therefore hashed differently on the two targets. `Symbol::Nat(BigUint)` carried a second
dependency, num-bigint selecting its digit width by `target_pointer_width`.

This is normally unobservable: a hash that only selects buckets may differ between targets. Here
it becomes a level-tree *level*, and a level determines tree shape.

**Isolating evidence.** `prim "symbolLevel"` over the same ten symbols:

```
x86_64  [262144, 64, 262144, 262272, 266240, 4224, 8321, 262144, 786433, 4162]
wasm32  [4097, 4097, 8259, 130, 532481, 130, 8192, 28739, 524354, 262210]
```

Resulting tree: `{height 6, width 10, widthLeft 8, widthRight 2}` natively;
`{height 6, width 10, widthLeft 4, widthRight 6}` under wasm32. A controlled experiment isolated
the variable: `fumola_wasm` is an ordinary Rust crate, so its exact operation sequence was run
natively (correct figures), then recompiled for `wasm32-unknown-unknown` and driven from node
(incorrect figures), with the compilation target as the only difference.

**Fix.** The level hash is taken over the symbol's source rendering, a string, which hashes
identically on all targets (issue #59, PR #60). Three assertions were updated and the new values
hold on both targets. `tests/test_symbol_levels.rs` pins the levels and names the library tests
derived from them.

---

## 7. Two rendering defects, recorded because both had the same cause

The browser's user-agent stylesheet sets `code { font-family: monospace }`, which overrides what
a `pre` inherits. Both stacked-layer techniques in the page were affected.

1. **Editor.** The highlighted layer rendered in a different typeface from the textarea over it:
   3 px of drift over 77 characters. The larger cause in the same component was soft wrapping —
   the highlighted layer is one `<span>` per token, and a break inside a word is decided per
   inline box, so a long line wrapped in a different place in each layer. Measured: the glyph at
   character 60 sat one line low and 567 px left. Neither layer wraps now.
2. **Library gutter.** The same missing declaration produced vertical drift: line boxes of
   27.1 px against the gutter's 25.59 px, which over 438 lines is 26 lines. The line numbers
   stopped early.

---

## 8. Measurements

| Quantity | Value |
| --- | --- |
| Scene as structured values, size 44 | 6,376,359 B, 4786 ms |
| Scene as JSON text (`IntoJSON.fromScene`), size 44 | 1,582,906 B, 2087 ms |
| Scene generation, sizes 4 / 10 / 16 / 24 / 44 | 119 / 279 / 484 / 1159 / 2527 ms |
| `A.scratch` overhead | not measurable; two heaviest tests unchanged at ~2.1 s |
| three.js, vendored | 692 KB, fetched on first scene only |
| Tree pair: stepping / boundary handoff / rendering | 56 ms / 196 ms / ~950 ms |
| Tree pair reply, as first written | 1.173 MB, 232 ms |
| … without the two histories | 0.355 MB, 178 ms |
| … also without the outlines, fetched on demand | 0.037 MB, 45 ms |
| Fader move, mergeSort pair, before and after the repaint fix | ~1000 ms → 1.26 ms |
| One `treeFromListRun` | 56,866 steps |
| Whole tree pair | 138,271 steps |

The three-way split in the first row is the finding that shaped the rest. A
step-limited eval was built to report progress on a run that felt slow (§12),
and the stepping turned out to be 56 ms of it. The 196 ms is one
`value_to_json` over a megabyte and cannot be chunked; the ~950 ms is
rendering. Progress reporting is therefore honest about a fifth of the
boundary cost and blind to the rest, which is why the payload was cut instead.

Two further measurements bear on incremental structure rather than on the
page, and are set out in `docs/repair-cost/repair-cost.pdf`: an edit to a
level tree changes a near-constant number of nodes as the input grows 62×
(13.7 at n=16, 17.5 at n=1000), while the same edit to mergeSort's merge
network changes 50% or 17% of the graph depending only on **which** cell is
removed.

The browser-generated scene JSON is byte-identical to the file `replayground-www/scene.json` was
produced from, so the existing renderer accepts browser-generated data unmodified.

---

## 9. Feature inventory

**Library panel.** Module tree; `examples/`, `examplePairs/` and `tests/` sections. Source view
with a line number gutter, attribute highlighting, and clickable `#[test]` / `#[example]` /
`#[examplePair]` markers each with a `body` button. Clicking any entry opens its **body** rather
than a call to it: a call is a name and brackets with nothing to edit, while a body shows the
arguments and the shape of the work. Independent font size controls; width snaps; draggable dividers.

**Editor.** Lexer-based highlighting; evaluation on input; error line number and banding;
`// assert`; `share`.

**Panels.** `objects` (3D), `outline`, `events`, `print`. Each indicates non-emptiness on its tab.
Empty `objects` and `outline` panels display the type a program would evaluate to. A pair is
described by a summary line rather than printed, since its value is large and nobody reads it.

**Pair view.** Two runs in one canvas: matched objects are drawn once and slide from where the
left run puts them to where the right run does, with their pointer lines and action geometry
following. A crossfader moves between the runs; glide indicators show each object's path, white
or the object's own colour where the runs agree and magenta where they do not; a node only one
run has peaks in magenta at the halfway mark. `trail` sets how strongly the indicators show. How
the encoding was arrived at, and the defects found on the way, are recorded separately in
`docs/diff-view-log.md`.

**3D viewport.** A box or sphere per labelled object at its stated position; a label plane and a
`textFields` plane per object, as canvas textures; a dot per `pointerFields` name; a line per
pointer, from the originating dot to the target's top. Orbit controls. Dimensions and offsets
follow `replayground-www` (`BoxDim = 0.33`), so a scene laid out for it reads at the same scale.

The viewport also names itself and can move by itself. The example's path and the camera's
position are drawn on the picture, because a screenshot travels without the URL that produced it.
`above` / `beside` / `below` walk the camera around the scene at a fixed height, with `spin` and
`out` for rate and distance; while a walk is on, the wheel changes the distance and a drag moves
what is being walked around, and only a click stops it. `ping-pong` sweeps the crossfader on its
own, eased with a pause at each end so both arrangements can be seen.

**Outline panel.** A port of `renderOutline` from `replayground-www/index.template.html`,
operating on values as they cross the wasm boundary rather than on `IntoJSON.fromOutline` output.
Same nesting, arrows (`───»`, `───>`, `══▷»`, `═══▷`) and result rows. Meta-times follow
`IntoText.metaTimesIntoText`.

---

## 10. History

### 10.1 Before the implementation

The ideas the language rests on predate it. Adapton was introduced at PLDI 2014; explicit
symbols, then called names, at OOPSLA 2015. Fungi, a type-and-effect system for incremental
programs, exists as a draft. Fumola is an attempt to put those into a language with a
conventional surface rather than a library interface.

### 10.2 Implementation phases

Dates are of the commit or merge that completed each step. Commit counts are for the period and
indicate only where the effort went.

| Period | Work |
| --- | --- |
| 2022-07 – 2022-10 | Started as `matthewhammer/motoko.rs`: a Motoko interpreter in Rust — AST, parser, and a stepping VM with an explicit continuation and stack. ~240 commits. No Adapton. |
| 2022-11 – 2023 | Sporadic. Module system, with `set_module` enforcing DAG structure (#152), then largely dormant: ~30 commits over 14 months. |
| 2024-01 | Renamed; crates renamed under the `Adapton` organisation (#2). |
| 2024-02 | Attribute syntax added — the `#[…]` form that `#[test]`, and later `#[example]`, use. |
| 2025-05 – 2025-07 | **Adapton enters the language.** Quote/unquote (#3) and the first Adapton recipe (#4): `adapton_state` integrated with the VM, navigation blocks parsed and stepped, `put`, `force`, `here()`, `now()`, and the simple (non-graph) semantics. ~95 commits. |
| 2026-01 – 2026-02 | Symbolic peek/poke effects. The mergeSort example (#5). Reorganisation into multiple crates (#8). |
| 2026-03 | **The graph itself.** `prim "adaptonReset"` takes a strategy (#24); graphical Adapton builds the DCG (#25); the DCG becomes Fumola values and several output formats, including the Unicode text form still in use (#26); a Fumola test runner (#28). ~58 commits, the largest month since 2022. |
| 2026-04 | `levelTree` and its `Edit` sub-module (#29). Scene files and a scene player: the first visualisation of a DCG (#31). |
| 2026-08 | **`replayground-www`.** DCG event logs as 3D animations (#34, #35), a static HTML build (#38), scene generation refactored (#39, #40), and DCG comparison by diffing node values (#43, #44). |
| 2026-09-04 – 09-05 | **The browser.** `fumola_wasm`: the runtime compiled to WebAssembly, with symbol translation and a value boundary for tuples, records, variants and pointers (#45, #46, #47). The homepage at fumola.org. In parallel, a Fumola livelit in Hazel backed by external incremental runtime state, on the `fumola-livelit-mvp` branch. |
| 2026-09-06 | **The playground**, in one day. §10.3. |

### 10.3 The playground

All on 2026-09-06, in approximately this order. Requirement numbers refer to §2.

| Step | |
| --- | --- |
| Web REPL at `/web-repl`, highlighted by the language's own lexer (#56) | R1, R2, R3 |
| Events panel; ready-made tests in the outline; a clean core per run | R4, R8, R9 |
| Graphical semantics throughout; clickable `#[test]`; a test opens its module at the marker | R5 |
| `body` button; print panel | R6, R7 |
| Module bodies no longer capture the host environment (#58) | §6.1 |
| Symbol hashing made target-independent (#59, #60) | §6.2 |
| Renamed to `/web-play`, with `/web-repl` retained as a redirect | |
| `A.scratch` | §5 |
| `#[example]`; examples section in the library | §4 |
| Error line reporting; line number gutter; share links; `// assert` | R10–R13 |
| 3D viewport; outline panel | R16 |

### 10.4 The pair view

All on 2026-09-07. Requirement numbers refer to §2 where one applies.

| Step | |
| --- | --- |
| Repair cost measured: what an edit costs a level tree, and a merge network | `docs/repair-cost/` |
| `#[examplePair]`: two runs, their node diff, and the counts | §4 |
| Both runs in one canvas, crossfaded | |
| Hoisted imports re-addressed after `..` imports landed | §6 |
| A step limit becomes a pause that can be resumed; `counts` cross the boundary | §12 |
| The crossfade becomes a glide: matched objects move rather than dissolve | |
| Glide indicators, with a trail and an intensity dial | |
| `W.DiffKeys`; the pair sheds its histories, then its outlines | §8 |
| mergeSort as a pair, and tree-into-list as the rung between | `docs/demo-notes.md` |
| Walking the camera, and sweeping the fader | §9 |

Three defects in that list are the same defect: shapes, then pointer lines,
then action geometry were each drawn from both runs at once, because the test
for whether to draw a thing asked whether its *ends* were in both runs rather
than whether *it* was. `docs/diff-view-log.md` records those and eight others,
with how each was found.

Two of those steps are corrections to the language rather than additions to the page, and both
were prompted by the page failing to run library code that the CLI ran. That is the pattern
worth recording: a tool that executes the library from a second host, in a second compilation
target, through a different entry point, exercises paths that neither the CLI nor `fumola test`
reaches.

## 11. Limitations

Each is tracked as an issue.

1. **No time cursor** (#63). `#action` scene objects are consequently not rendered.
2. **The outline panel does not cross-link** to the viewport or the events table (#64).
3. **The `()` contract for `#[example]` is unenforced** (#65). It belongs in `fumola test` or
   CI, not in the page.
4. **`A.Scene.Scene.outline` is a single outline** (#66, outline forests). The components are
   now a forest, so an example no longer has to choose which tree to portray, but the two
   shapes still differ. Changing `A.Scene.Scene` reaches `IntoJSON.fromScene` and the JSON
   `replayground-www` consumes, so it was not done at the same time.
5. **No scene is built incrementally** (#67). `generateSceneFullDemand` begins with
   `A.reset()`, so repeated runs cost the same. This is the principal obstacle to
   demonstrating incremental computation in a tool built to demonstrate it, and the only item
   here with research content rather than engineering content.
6. **Evaluation is synchronous** (#68, no worker); a size-44 scene blocks the tab for about
   2.5 s. Partly addressed: a run now returns between chunks and reports its step count (§12),
   so the page is no longer blind to its own progress. It does not fix the tab freezing, and
   cannot — the largest single cost is one `value_to_json` that a trampoline cannot divide. A
   worker remains the answer, and is also the only means observed of reclaiming memory:
   `fumola_reset` and `fumola_drop` both leave heap usage unchanged.
7. ~~**Published assets have stable URLs** (#69, stale deploys)~~ — **resolved** by
   content-addressed runtime versions. Observed once from the outside before the fix: a
   post-deploy fetch returned an object byte-identical to the pre-deploy one while the page was
   already current, and a minute later the same URL served the right bytes.
8. **One non-ASCII character hides every attribute below it** (#80, byte vs UTF-16 spans).
   `fumola_tokens` reports byte offsets and the page reads them as UTF-16 code units. Latent
   only because every `.fumola` file is currently pure ASCII. The fix wants making once at the
   boundary rather than at each of the six places the page uses spans.
9. **A `!` failure inside an edit closure reports the wrong thing** (#77, misleading
   UnboundIdentifer), which is why the list head is absent from every repair-cost sample.
10. **Scenes are re-derived and re-sent in full** (#83, scene deltas). Consecutive scenes are
    mostly the same scene, which is the situation Adapton exists for, applied to the boundary
    rather than to the computation. Measured, a naive delta is worth less than it looks: 75%
    of graph nodes are equal between two runs where only 13% of object *bytes* are, because
    layout is a global function of the input.
11. **Tutorial mode is not built** (#85, tour the examples). The demo has an argument that
    depends on order, and reproducing it means remembering three links, which camera suits
    each, and which numbers to quote. A design is proposed in `docs/tutorial-design.md` and
    is deliberately not settled: whether an example carries its tutorial or a tutorial names
    its example, and how much view state is worth recording, are open.

## 12. A step limit that is a pause

The VM has had a step limit since long before the page: `Interruption::Limit`
was recoverable, excluded from `ends_the_computation`, and `run` already left
the stack standing for it. Nothing resumed from one, the wasm exposed none of
it, and the CLI's `--step-limit` was parsed and thrown away beside a `// to do`.

It is wired up now. `fumola_eval_top_limited` runs at most *n* steps and
answers a value, an error, or `{ok, paused, steps}`; `fumola_resume` continues;
`fumola_abandon` drops a run nobody is waiting for. Every reply carries
`counts` — `step`, `redex`, `send` — as the difference across the run rather
than the instance's lifetime totals, and on the failure path too, since a
program that failed still did the work it did before failing.

Two details are worth recording because both were got wrong first.

**The limit is a mark, not an allowance.** `Limit::Step` fires when the
*cumulative* count reaches the limit, so a caller asking for a bare 1000 after
5000 steps stops immediately and never progresses. `Core::step_budget` is that
arithmetic, and one test is exactly that regression.

**Progress is the run's, not the instance's.** Importing the library costs tens
of thousands of steps, so a lifetime total opens high and its rise says nothing
about the program asked for. Shipped as a total first, it read 137,680 on the
first chunk of a 138,271-step run, which looked like the limit had failed.

The guarantee that nearly went: `fumola_eval_top` evaluates against a copy and
commits only on success, so an edit that does not work costs nothing. A
resumable run cannot do that — a copy would be dropped between calls and take
the paused computation with it — so it runs against the instance and keeps the
copy in a thread-local, restoring it on failure and on abandon.

What it does *not* buy is set out in §8: stepping is a fifth of the boundary
cost, and the rest is one indivisible call.

---

## 13. Companion documents

Three documents, three jobs. This one is what the playground is and why.

| | |
| --- | --- |
| `docs/diff-view-log.md` | What broke while building the pair view, and how the encoding was decided. Eleven defects with their mechanisms, and the design reasoning behind the glide, the trail, and the colours. |
| `docs/demo-notes.md` | How to show it. Three examples in order, what each demonstrates, what to say, with every number measured. |
| `docs/repair-cost/` | What an edit costs a demanded computation graph. A TeX report with its data; the source of the stability measurements quoted in §8. |
| `docs/tutorial-design.md` | A proposal for the page presenting the demo itself. Not built, and not settled. |

---

## 14. Relationship to `replayground-www`

`replayground-www` renders a pre-captured scene and has a time cursor with playback, a 3D
viewport, and a cross-linked outline with clickable chips. The playground has a live runtime, a
3D viewport, and an outline without cross-linking.

Its viewer is not re-entrant: `data` is a module-level constant, and meshes and indices are built
once at load; `loadScene()` re-renders only the events and outline panels. Porting the viewport
therefore required restructuring rather than copying, which is why the playground's scene
mounting and disposal is written as a unit that owns and releases its resources.

Remaining work, in dependency order: a metaTime cursor over the events table; a scene object
retained in the page; outline cross-linking; `#action` rendering. `replayground-www` can be
retired at that point.

The playground has since acquired one thing `replayground-www` has no equivalent for: a view of
two runs at once, with the objects they share gliding between their two layouts. That is not a
port of anything, and it is the part most specific to what Fumola is about — a static log can be
replayed, but only a live runtime can be asked the same question twice with one thing changed.
