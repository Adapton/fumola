# The Fumola web playground: requirements, design, and measurements

**Artifact:** <https://fumola.org/web-play> · **Source:** `pages/web-play/index.html`,
`pages/vendor/` · **Status:** running; 17 of 17 library tests and 4 of 4 examples execute
green in the browser.

This document records what was built, why each part exists, and what was measured. It is
organised by requirement rather than chronologically. §10 places the playground in the
project's history, which begins in 2022 and reaches Adapton in 2025. §6 lists two defects in
the Fumola implementation that were found by building the tool, and is the section most likely
to matter outside this page.

---

## 1. Scope

The playground is a single static HTML file plus a vendored copy of three.js, served from
GitHub Pages, running the Fumola runtime compiled to WebAssembly. There is no build step, no
framework, and no runtime network dependency.

It replaces a small "try it" box on the Fumola homepage. It is intended to overlap with, and
eventually replace, `replayground-www/`, an existing DCG viewer that renders a scene captured
to a JSON file in advance.

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
| R14 | A program had no way to state how it should be drawn | `#[example]` and the `#scene` return shape |
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

## 4. `#[example]` and the `#scene` shape

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
public type ExampleOutput = { #scene : SceneComponents };
```

`objects` is `A.Scene.Scene`'s own array of `SceneObject`. `outline` is a forest: zero or more
trees, in the order the provider chose. A session has one force tree per force begun from
outside any other, so a single tree is the special case rather than the rule, and an example
wishing to show two need not choose between them. Both components use one convention for
absence: an empty array denotes a component the author did not supply.

Constructors `scene`, `sceneOfObjects`, `sceneOfOutline`, `sceneOfOneOutline` and
`sceneOfForces` are provided; the last offers every top-level force in the order forced, and
requires no decisions from the author.

`withHistory : ExampleOutput -> ?A.Scene.Scene` adds the runtime's history. Its result is
optional, and non-empty only for a forest of exactly one tree, because `A.Scene.Scene` carries
a single outline (§11.4).

### 4.3 Contract

- An `#[example]` takes `()` and must succeed when applied to `()`. This is not enforced by the
  playground; it is a lint for CI and the CLI, and is currently unimplemented.
- The playground's trigger is the **value** the editor evaluates to, not the presence of the
  attribute. Any expression evaluating to `#scene` fills the panels. `#[example]` is how a
  library function opts into producing such a value and into being listed.
- `#[example]` required no parser or lexer change: attributes are already generic in the AST,
  and `handle_test` matches only the identifier `"test"`, so `#[example]` parsed and was
  ignored before any Rust was written.

### 4.4 Examples supplied

| Example | objects | outline trees | derived from |
| --- | --- | --- | --- |
| `List.exampleList` | 7 | 1 | `testFromIter`, `testSceneObjects` |
| `LazyList.exampleLazyList` | 4 | 1 | `testTakeN_`, `sceneObjectsMergeSortStream` |
| `levelTree.Scene.exampleLevelTree` | 37 | 1 | `testGeom2d` |
| `mergeSort.exampleMergeSort` | 279 | 0 | `generateSceneFullDemand` |

`exampleMergeSort` supplies no outline, and is retained as a test of the optional case.

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

The browser-generated scene JSON is byte-identical to the file `replayground-www/scene.json` was
produced from, so the existing renderer accepts browser-generated data unmodified.

---

## 9. Feature inventory

**Library panel.** Module tree; `examples/` section; `tests/` section. Source view with a line
number gutter, attribute highlighting, and clickable `#[test]` / `#[example]` markers each with a
`body` button. Independent font size controls; width snaps; draggable dividers.

**Editor.** Lexer-based highlighting; evaluation on input; error line number and banding;
`// assert`; `share`.

**Panels.** `objects` (3D), `outline`, `events`, `print`. Each indicates non-emptiness on its tab.
Empty `objects` and `outline` panels display the `#scene` type.

**3D viewport.** A box or sphere per labelled object at its stated position; a label plane and a
`textFields` plane per object, as canvas textures; a dot per `pointerFields` name; a line per
pointer, from the originating dot to the target's top. Orbit controls. Dimensions and offsets
follow `replayground-www` (`BoxDim = 0.33`), so a scene laid out for it reads at the same scale.

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
| `#[example]` and `#scene`; examples section in the library | §4 |
| Error line reporting; line number gutter; share links; `// assert` | R10–R13 |
| 3D viewport; outline panel | R16 |

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
4. **`A.Scene.Scene.outline` is a single outline** (#66). The `#scene` component is now a
   forest, so an example no longer has to choose which tree to portray, but the two shapes
   still differ: `withHistory` produces a `Scene` only for a forest of exactly one tree.
   Changing `A.Scene.Scene` reaches `IntoJSON.fromScene` and the JSON `replayground-www`
   consumes, so it was not done at the same time.
5. **No scene is built incrementally** (#67). `generateSceneFullDemand` begins with
   `A.reset()`, so repeated runs cost the same. This is the principal obstacle to
   demonstrating incremental computation in a tool built to demonstrate it, and the only item
   here with research content rather than engineering content.
6. **Evaluation is synchronous** (#68); a size-44 scene blocks the tab for about 2.5 s. A
   worker is needed before raising the input size, and is also the only means observed of
   reclaiming memory: `fumola_reset` and `fumola_drop` both leave heap usage unchanged.
7. **Published assets have stable URLs** (#69), so a deploy leaves browsers a version behind
   until a hard reload. A content hash would remove the problem.

## 12. Relationship to `replayground-www`

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
