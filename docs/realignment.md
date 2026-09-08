# Realignment

What the graphical cache does after an edit -- signaling, then repair -- the
words Fumola uses for it, and what is still to do. Written as PR #106
("Signaling and repair: the graphical cache realigns after an edit") landed,
2026-09-08. The module documentation of
`crates/fumola_semantics/src/lib/adapton/graphical/mod.rs` is the technical
companion: it carries the reading guide to the papers and the recipe, rule by
rule, and the list of places where this code and the recipe differ.

## The words

Fumola's terms differ from the Adapton papers' -- Adapton (PLDI 2014) and
Nominal Adapton (OOPSLA 2015) -- in terms, not in algorithms. The departure
is deliberate, and it is systematic: code, docs, issues, talk.

| the papers                 | Fumola        |
|----------------------------|---------------|
| dirtying                   | signaling     |
| dirty (an edge's state)    | signaled      |
| clean (an edge's state)    | aligned       |
| cleaning                   | repair        |
| change propagation         | realignment   |

A `put` that changes a cell with readers *signals*: the edges that observed
the old contents, and the force edges upstream of them, are marked signaled.
A `force` of a thunk whose trace holds a signaled edge *repairs* it: the edges
are checked in order, and each realigns or the thunk is re-evaluated. The
whole, from an edit to the next consistent demand, is *realignment*, made of
signal and repair steps that usually come in matching pairs. "Aligned" was
already the recipe's word for a store that agrees with the one a thunk last
ran under (`Store1 <= Store2`, an aligned extension); an aligned edge is the
same idea at the smallest grain. Issue #22 ("Repairing the graphical cache:
signaling and realignment algorithms") used the first of these words from the
start; the rest were settled on 2026-09-08.

When explaining to someone who knows the papers, give the table once and then
use Fumola's words.

## What landed

Before #106 the graphical cache had the graph and not the algorithm: edges
that record what they observed (`Action::Get(v)`, `Force(body, v)`), a
readers index, a trace per thunk, `FrameKind::Clean` reserved on the stack and
never pushed, `Event::RemoveEdge` defined and never emitted, and a
`// TODO -- clean.` where the cache hit was. Every `put` minted a new version
of the node with an empty cache, so a program re-run in a live graph reused
nothing -- which is also why `Outline.outline` defeated its own memo.

Three pieces closed the gap.

**Signaling**, in `put_pointer`. When the name is already taken at this time
and the contents differ, the new version is recorded and `signal` walks the
readers: an edge is marked when its recorded action is misaligned with the
new contents -- a `Get` that recorded something else, or any `Force` -- and
never when it is a `Put`, an allocation rather than an observation. Past the
first hop only force edges are followed, because the source of a signaled
edge is a thunk whose *result* may now differ, and only its forcers observed
that result. The walk stops at an edge already signaled. This is the recipe's
`dirtyNode` with the `misaligned(Space, v)` and `forces` patterns, where
Algorithm 1 of the PLDI paper marks every incoming edge and Nominal Adapton's
`dirty-paths-in` marks every path.

**Repair**, in `force_begin` and two new operations, `repair_step` and
`repair_resume`. A cached result whose trace holds a signaled edge is not
returned; the thunk is repaired first. The trace is walked in order. A `Get`
edge is compared on the spot. A `Force` edge has its target forced first --
and that may run Fumola code, so the graph cannot do it alone: `force_begin`
answers `ForceBeginResult::Repair`, and the VM loops on `repair_step`, which
answers `Aligned(v)`, `Force(pointer)` or `Reevaluate(body)`; a `RepairForced`
frame carries a forced value back to `repair_resume` for the comparison of
Algorithm 1's line 12. A force made on behalf of an edge records no new edge:
the edge under check is the one `force_end` completes. When every edge
realigns the cached result stands. When one cannot, the old trace's edges
leave the live graph (`RemoveEdge`, at last) and a new version of the thunk
is evaluated as an ordinary cache miss -- the recipe's `nextMetaMoment` and
the papers' `del-edges-out`, both.

**A matched put.** A `put` of what a cell already holds -- same value, or
same thunk body in the same space -- keeps the node, signals nothing, and
records only the allocation edge: Nominal Adapton's `Eval-refClean` and
`Eval-thunkClean`. `Settings::put_matches_equal_values` turns it off
(`` `adapton(`settings)(`putMatchesEqualValues) := false ``); even then an
equal put signals no edge, since signaling compares. The match only spares
the version.

The state is on edges, as #22 said: `Align::{Aligned, Signaled}`. Both
traversals are bracketed in the history -- `SignalingBegin`/`SignalingEnd`
and `RepairBegin`/`RepairEnd(outcome)`, with `EdgeSignaled` and `EdgeAligned`
per edge between -- the way `ForceBegin`/`ForceEnd` brackets a force. Six
counters joined the reserved symbols: `putMatched`, `signalings`,
`edgesSignaled`, `repairs`, `edgesAligned`, `reevaluations`.

### Behaviour that changed

- A thunk that writes the cell it has just read is repaired on its next
  force. `force_simple_cache_hit` expected the stale value from exactly that
  program -- the absence of repair. Under the simple strategy it still gets
  the stale value: the simple strategy is a memo table with no graph, not the
  recipe's reference semantics, whose `redoForce` would re-run. Both answers
  are now pinned as tests.
- Histories of programs that edit after a demand carry signaling events.
  `mergeSort.dcgDiffRemove` removes a list cell after the sort has read it,
  and its JSON export was the first to meet a `#signalingBegin`.
- `peekCell(p).incomingEdges` lists every reader. It used to find only the
  `Put` edge, because `Get` and `Force` edges were indexed under the access
  moment's pseudo-id; the readers index is keyed by (space, time) now, which
  is the recipe's `incomingEdges`.
- `Edge` records carry `align`; `PeekInfo.Event` has six new tags, and its
  `#forceBegin` / `#forceEnd` types were corrected to what the runtime emits.

## Why now: the numbers were the specification

The report in `docs/repair-cost/` opens with "Fumola has no repair algorithm.
That is deliberate while the specification for expected behaviour is still
being written." The specification is the numbers: `A.Diff.nodeValsDiff`
between two from-scratch runs says which nodes a repair could reuse
(`equal`), which it must recompute (`notEqual`), which it must create
(`onlyRight`) and which become garbage (`onlyLeft`). Every measurement in
`docs/mergesort-report-notes.md` is a statement about what realignment ought
to do.

The last measurement before building was the one only this reading asks for.
`notEqual` conflates two things: a thunk whose *body* differs -- the program
named a different closure the same thing, a re-allocation -- and one whose
body is the same and only its *result* differs, which is repair proper. Seed
10, n = 1000, removal of the root (cell 586):

| naming    | notEqual | new body | same body | cells | onlyRight | onlyLeft |
|-----------|---------:|---------:|----------:|------:|----------:|---------:|
| inductive | 2872     | 2835     | 28        | 9     | 732       | 1009     |
| cartesian | 3524     | 3496     | 19        | 9     | 277       | 554      |
| pathwise  | 2721     | 2684     | 28        | 9     | 3522      | 3799     |

Ninety-nine percent of `notEqual` is a new closure, and a new closure is
evaluated fresh, like an `onlyRight` cell. So the evaluations realignment must
perform are new body + same body + onlyRight: inductive 3595, cartesian 3792,
pathwise 6234, of 18105 nodes each. Cartesian does not cap the tail; the
"unmatched" advantage it showed (831 against 1741) was cells leaving
`onlyRight` and reappearing as `notEqual` under a colliding name. Its real
advantage at the root is garbage, 554 against 1009. The nine cells are the
signal sources -- one list cell and the tree path.

Two lessons, and they are why repair became the critical path. The diff
cannot separate *work redone* from *graph changed*; a running repair counts
both, and the excess of repair checks over re-evaluations is exactly what the
diff cannot see. And a diff costs two full runs per sample, which is what
made the average-case sweeps so weak (six samples per size, a bimodal
distribution); with repair, an edit costs its churn, so every position at
n <= 4096 can be measured exactly.

## How to see it

The smallest program:

    let x = `x := 1;
    let t = `t := thunk { (@ x) + 1 };
    force t;        // 2: a cache miss, and a trace with one get edge
    x := 5;         // signals t's get edge, then the force edge into t
    force t         // 6: repair finds the get misaligned and re-evaluates t

Read out of the history, with `S[`..`]` for a signaling traversal, `s` for
an edge signaled, `R[`..`]` for a repair, `a` for an edge realigned, `x` for
an edge removed, `!]` for a repair that re-evaluated and `=]` for one that
did not, that program is `S[ss]R[x!]`. The test
`the_history_brackets_signaling_and_repair` reads exactly that.

- Counters: `` @(`adapton(`counts)(`reevaluations)) `` and the five others.
- Events: `A.peekEvents()`; `IntoJSON.fromEvent` renders the new tags; the
  playground's events tab shows them as it shows every event.
- Tests: `crates/fumola/tests/test_adapton_realignment.rs` (thirteen
  programs: the spreadsheet case, the cutoff where a value comes back
  unchanged, the matched put on and off, a diamond, a chain, a thunk given a
  new body, old-trace removal, the bracket sequence, the simple strategy's
  divergence) and four `#[test]`s in `fumola/system/adapton.fumola`.

## The recipe

The code is the reference: it can be checked against from-scratch diffs and
looked at in the playground, and the recipe (`adapton-recipe.ott`) is written
to agree with it. Lining the two up found these, each a note for the recipe:

1. Edges target `qq` = (Space, Moment) there; here `target: NodeId` whose
   meta-time is the access moment, not a version. Same meaning; the type name
   here is misleading.
2. `CT-dirtyIntoClean` asks `clean(qq)` of a `Get` edge's target, and
   `clean(pp)` has no rule for a non-thunk `pp`. The rule it needs is
   "nothing to do".
3. `actionMatches` / `actionDoesNotMatch` (`~~`, `~/~`) have no rules. The
   reading implemented: `misaligned(Space, v)` matches a `Get` whose recorded
   value differs from `v` and any force; `forces` matches any force.
4. Typos: `IE-emptyForce` and `IE-cleanForce` conclude `v` where the premises
   bind `v3`; `CT-binL`'s premise starts from `Graph2`; `CN-misaligned`
   inserts `Thunk(Space, ...)` where the node was `Thunk(Space0, ...)`.
5. `Graph2(ppp)` with the current meta-moment `m` looks a node up at the
   wrong version; "latest version" is meant.
6. `E-getThunk` yields `(sp Space0, thunk e0)`; `get` of a thunk pointer here
   yields the thunk alone.
7. The graphical `put`, `get` and `undelay` rules are prose placeholders;
   `undelay` "uses dirtying" there and does not signal here.
8. The rule names still say dirty and clean.

## What is left

**In the engine.**

- A cycle check in `force_begin`: a thunk that forces itself, through any
  path, loops today. The `-` naming bug of 2026-09-08 presented as an
  out-of-memory for this reason; it should present as an error.
- Signaling on `undelay`, when a delayed put lands on a cell with readers.
- Nominal Adapton's double-use check, `consistent-action(G, alloc e, q)`: a
  name allocated with two different bodies in one run is an error there and
  a silent last-writer-wins here. `Put` edges are never signaled or checked;
  that is fine exactly as long as double use is not detected.
- `Outline.outline` memoizes through a named thunk and reads the graph with
  peeks, which record nothing. Now that an equal re-put matches, that thunk
  will hit, and a hit is stale whenever the graph changed underneath it. It
  should compute inside `A.scratch`, or stop naming its thunk.
- The simple strategy is a memo table without repair. Say so where it is
  offered, or retire it once the Hazel side no longer needs `simple` as its
  provisional default.

**In the tools.**

- A realignment-mode pair for the playground -- issue #67, "no scene is
  built incrementally, so the tool cannot yet show incremental repair". One
  graph, one edit, and the trails are the signaling and repair walk rather
  than the diff of two runs. The events exist for it now; what is missing is
  a scene built from a run edited in place, and an `examplePair` re-thought
  from a pair of runs into a run and an edit. Editing the parameters while
  keeping the view (PR #89) is already the UX of it.
- The replayground and the playground draw nothing for the six new events;
  both compare event types against the old names and pass over the rest.
- `fumola test` counts failures but exits 0, so the `./fumola-test.sh` step
  of CI cannot go red. One line.

**In the measurement.**

- The oracle. Run a program; edit in place -- `List.Edit.remove` is a put to
  the predecessor's `next`, the signal source; re-demand the output; compare
  by name against a from-scratch run of the edited program made inside
  `A.scratch(thunk { A.reset(); ... })`, whose graph is discarded on exit.
  Assert that the values agree on every name, and that `reevaluations`
  equals new body + same body + onlyRight from the diff; the excess of
  `repairs` over that is the cost the diff never saw. The nine size-16
  `#[examplePair]`s have exact numbers and are the place to start.
- Split `notEqual` into new body and same body in `A.Diff` and
  `webPlay.DiffSizes`, so the pairs report it.
- Exhaustive sweeps: every position at each size up to 4096, each edit
  costing its churn. Enough to separate log n from log^2 n within the
  memory ceiling, which six samples per size could not.
- Two axes, kept apart: work redone (`reevaluations`, counted even inside a
  scratch) and graph changed (the node diff). This is the distinction between
  reuse by names and content-addressed storage, for `docs/cache-naming.md`
  (PR #95).
- Correct `docs/mergesort-report-notes.md`: "cartesian caps the tail" is
  garbage, not work; and the doc comment on `examplePairCartesianRootRemoval`
  says a `notEqual` cell "can be dirtied and recomputed where it stands",
  which is wrong in mechanism for 3496 of its 3524 cells and wrong in its
  words.
- `eagerMergeSortRebuild`: compute the intermediates inside `A.scratch` and
  persist only the canonical output; the intermediates are O(n) different
  after any edit and nothing can be reused from them.

**In the recipe.** The eight notes above, and the terminology.

**In the docs.** `docs/cache-naming.md` (PR #95) gets the re-allocation
versus repair distinction; `docs/fumola-for-haskell-folks.md` (PR #96) gets
the table of words.
