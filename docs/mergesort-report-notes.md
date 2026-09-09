# mergeSort report notes

Working notes for the mergeSort report. What the report is turning out to be
about is naming: the merge network has to name its cells, the names decide what
a later run can reuse, and the algorithm is otherwise untouched. So the
comparison is unusually clean -- same computation, isomorphic graph, only the
names move -- and it is the part of mergeSort worth reporting.

This file tracks the strategies under consideration, what data we have on each,
and how to look at each one. It is a notebook, not a conclusion.

## The strategies

| | name of a merge cell | derived from | status |
|---|---|---|---|
| `inductive` | `mergeSort(merge-3)(6)` | the reduction node's symbol, plus the winning element's | implemented, the default |
| `cartesian` | `mergeSort(1(13))` | the pair of cells compared | implemented, PR #92 |
| `pathwise` | `mergeSort(merge-`0(R))(4)` | the traversal path to the node | implemented, PR #92 |

**`inductive`** opens a namespace for the reduction node the merge belongs to
and names each continuation for the element that won the comparison. Both
halves move when the input is edited: the tree reshapes, and the winner at a
given position changes.

**`cartesian`** names each continuation for the pair of cells compared, and
opens no namespace. A merge network never compares the same two symbols twice,
so the pair is a name no other cell can claim -- which is what lets the
namespace go, and the namespace was what carried the instability.

**`pathwise`** keeps inductive's shape but replaces the tree node's symbol with
an abstract path to it, `path ::= 0 | path(L) | path(R)`, built independently of
anything in the input. It opens a space for the merge stream keyed by that
path, and the cell within it is still named for the element that won -- so it
is `inductive` with one substitution, an address where the node's own symbol
used to be. `0` for the root rather than `R`, so the root is not confused with
a right turn. `reducePathwise` is written out rather than added as a parameter
to `reduce_`, because a parameter would sit in the environment of every thunk
the other two strategies build.

The three span a real distinction, which is probably the report's spine: what a
name is derived from. Inductive and cartesian both name by *what* is being
merged, and so move when the data moves. Pathwise names by *where*, and is
independent of the data entirely.

On the name: "coinductive" was the first candidate and reads as misleading.
Coinduction is about greatest fixed points and observation of unbounded
structures -- there is a coinductive object here, the LazyList, but the path is
not an observation of it. It is a finite address in a finite inductive tree,
fixed before any output is observed. `pathwise` for now; `positional` and
`contextual` are the other candidates.

## What we know

All figures are seed 10, one removal, whole-history node diff. There is no
repair algorithm yet, so these are counts over two independent runs: what a
repair could reuse if it existed.

### The naming region is exactly the merge network

`examplePairInductiveVsCartesian`, size 16, unedited input under both namings:

    equal 80   notEqual 34   onlyLeft 49   onlyRight 49

The 49/49 symmetry is the signature of an isomorphic graph with different
names: every merge cell exists under one naming and not the other, one for one.
The 114 that agree are the list, the tree and the reduction spine, none of
which cartesian reaches. At size 100 the same check gives tree `371/24/4/0`
and list `99/2/0/0` under *both* strategies -- identical to the node.

All three verticals are symmetric, and the arithmetic closes:

| | equal | notEqual | onlyLeft | onlyRight |
|---|---|---|---|---|
| inductive vs cartesian | 80 | 34 | 49 | 49 |
| inductive vs pathwise | 80 | 4 | 79 | 79 |
| cartesian vs pathwise | 80 | 4 | 79 | 79 |

49 merge cells plus 30 reduction-spine cells is 79. Inductive and cartesian
share the spine, so those 30 appear as `notEqual` -- same name, other contents.
Pathwise keys the spine by path too, so they move to one-side-only and
`notEqual` falls from 34 to 4. The 80 that stay equal are the list and the
tree, which no naming reaches.

### Which element you remove matters more than which naming you use

Size 16, the highest node in the input (`symbolLevel` 1581569, cell 3) and the
lowest (4, cell 10):

| pair | equal | notEqual | onlyLeft | onlyRight | unmatched |
|---|---|---|---|---|---|
| root removal, inductive | 87 | 56 | 20 | 5 | 25 |
| root removal, cartesian | 83 | 61 | 19 | 4 | 23 |
| root removal, pathwise | 61 | 25 | 77 | 62 | 139 |
| leaf removal, inductive | 134 | 20 | 9 | 0 | 9 |
| leaf removal, cartesian | 134 | 20 | 9 | 0 | 9 |
| leaf removal, pathwise | 134 | 20 | 9 | 0 | 9 |

The three leaf rows are identical to the number. When the tree does not move,
no naming can matter.

The pathwise root row is the surprise, and it went the opposite way to the
prediction written down before it was measured. An address is stable only
while the shape it addresses is stable; removing the highest node reshapes the
tree from there down, so nearly every path changes and nearly every cell is
renamed -- 139 unmatched against inductive's 25.

So the three span the trade rather than ranking on it: cartesian caps
inductive's worst case, pathwise multiplies it by five and a half, and all
three agree exactly when the shape holds. Pathwise is a poor way to name a
graph you mean to repair and a good instrument for the report, because it
holds the data fixed and varies only the shape's influence.

### Cartesian caps the tail and raises the floor

*Superseded in part -- see "two corrections" below. These figures are
`unmatched`, which is mostly garbage; under `redone` cartesian and inductive
do the same amount of work.*

All 99 removal positions at size 100:

| | inductive | cartesian |
|---|---|---|
| mean unmatched | 27.0 | 32.3 |
| median | 18 | 25 |
| max | 193 | **155** |

Top decile by inductive churn -- the near-root removals -- totals 973 against
787, and cartesian wins 9 of the top 12: 53 becomes 15, 125 becomes 67, 139
becomes 93, 78 becomes 50. The bottom 12 go the other way in 12 of 12, one of
them 12 becoming 88. Cartesian removes inductive's worst case and pays with
churn of its own: an element that wins many comparisons accumulates a cell per
partner it meets, and benign edits shuffle those partners.

### The kind of disagreement changes, not only the amount

Consistently, cartesian moves cells out of `onlyLeft`/`onlyRight` and into
`notEqual`. That is the distinction repair cares about and the one a
"how many nodes differ" figure hides. A `notEqual` cell is known to both runs
and can be dirtied and recomputed where it stands; an `onlyLeft` cell has no
counterpart, so it and everything built on it begin again. By name-match share
(equal + notEqual) at size 16: root removal 85.7% to 89.2% for cartesian, leaf
removal identical.

### At the root, at scale: the measurement the rest was missing

*Superseded in part -- see "two corrections" below. The highest-level cell is
not the worst position: an exhaustive sweep finds one 43% dearer at n=128.*

Every earlier figure here removed a cell chosen without reference to the tree
it sits in. That is the flaw: what an edit costs depends on how much of the
tree the removed node carried, so a removal has to be chosen *as* the root or
*as* a leaf, and at size 1000 the root is not cell 3 -- it is cell 586, whose
`symbolLevel` of 2121729 outranks everything else in range.

Removing the root, and removing a level-0 leaf, at size 1000, seed 10:

| naming | removal | equal | notEqual | onlyLeft | onlyRight | unmatched | share |
|---|---|---|---|---|---|---|---|
| inductive | root 586 | 14501 | 2872 | 1009 | 732 | 1741 | 9.1% |
| cartesian | root 586 | 14304 | 3524 | 554 | 277 | **831** | 4.5% |
| pathwise | root 586 | 11862 | 2721 | 3799 | 3522 | 7321 | 33.4% |
| inductive | leaf 42 | 18336 | 27 | 19 | 0 | 19 | 0.10% |
| cartesian | leaf 42 | 18330 | 30 | 22 | 3 | 25 | 0.14% |
| pathwise | leaf 42 | 18336 | 27 | 19 | 0 | 19 | 0.10% |

Three things, and the first answers the question this file listed as open.

**Cartesian's advantage survives scale and grows.** 831 against 1741 is 0.48,
where the best case at size 100 was 0.80. The open question was whether the
tail gap widened or closed with n; it widens.

**Pathwise's penalty is stable in ratio.** 4.2 times inductive here, 5.6 times
at size 16 -- so it is not a small-input artifact either, and an address really
does survive only as long as the shape it addresses.

**And the leaf row is flat: 19, 25, 19.** Across strategies whose root figures
span a factor of nine, a removal that barely moves the tree costs the same
under all three. Naming matters where the tree moves and nowhere else.

The `notEqual` column says the same thing it said at size 16: cartesian holds
*more* of it (3524 against 2872) and less unmatched, so it moves work from
rebuild into repair rather than removing it.

### Eager merging costs more repair than lazy merging

The same removals, against `eagerMergeSort`, which merges as it goes and keeps
a level tree throughout. It has one naming -- the reduction node's own symbol
-- since it never consults the strategy cell.

| sort | size | removal | unmatched | union | share |
|---|---|---|---|---|---|
| lazy, inductive | 250 | root 3 | 177 | 3848 | 4.6% |
| eager | 250 | root 3 | **2318** | 6201 | 37.4% |
| lazy, inductive | 1000 | root 586 | 1741 | 19114 | 9.1% |
| eager | 1000 | root 586 | **5176** | 26715 | 19.4% |
| lazy, inductive | 1000 | leaf 42 | 19 | 18382 | 0.10% |
| eager | 1000 | leaf 42 | 52 | 24161 | 0.22% |

Eager is worse everywhere: thirteen times the unmatched nodes at size 250,
three times at size 1000, and between two and three times even for a leaf
removal. Its graph is also larger -- 26715 nodes against 19114 -- because
`split` and `append` allocate.

That is the larger effect than any naming difference measured, which reframes
the whole exercise: how much structure an operation rebuilds matters more than
what the rebuilt nodes are called. The lazy network re-forces stream cells; the
eager sort rebuilds tree structure, and every node it rebuilds is a node whose
name pairs with tree symbols that move when the tree moves.

Two cautions about reading the sizes against each other. The root at 250 and
the root at 1000 are different draws and not comparable: cell 3 roots a 2 : 248
split, which is the degeneracy of issue #72, while cell 586 roots a 585 : 415
one. So the share falling from 37.4% to 19.4% is a change of shape, not a
scaling law. And a single removal per cell is a sample of one -- deterministic,
so repeatable, but a sample of one draw all the same.

### What the earlier scaling table actually measured

The table that used to stand here removed cell 3 at every size and found churn
roughly constant, around 300 nodes from size 500 to 5000, concluding that the
naming question shrinks with n. Cell 3 has a high level and so sits high in
every tree, but at size 1000 it is not the root, and the numbers above show a
root removal costing 1741 rather than 310. So that table measured a
high-but-not-highest node, and the conclusion drawn from it -- that either
naming already reuses 99.4% of the graph -- holds for that case and not for the
worst one.

### Sweeping every position, and two corrections it forces

*Added 2026-09-09, with `fumola/examples/mergeSort/sweep.fumola`.* Diffing two
runs used to cost 3 s and a gigabyte per sample, so every table above sampled
positions -- one, or six. `A.Native` (see `docs/native-diff.md`) brought a
sample down to the two runs and almost nothing else, and an **exhaustive**
sweep became affordable: every position, no sampling to argue about. Sizes 64,
128 and 256, all four algorithms -- 1,780 samples, about 10 minutes.

The measure is `redone` = `newBody` + `sameBody` + `onlyRight`, for the reason
in the section below: `notEqual` counts a re-allocation as if it were a repair,
and 99% of `notEqual` is re-allocation.

| n | algorithm | own | mean | median | p90 | max | at | mean/own |
|---|---|---|---|---|---|---|---|---|
| 64 | inductive | 844 | 34.3 | 22 | 65 | 219 | 39 | 4.06% |
| 64 | cartesian | 844 | 34.5 | 26 | 50 | 191 | 39 | 4.08% |
| 64 | pathwise | 844 | 50.0 | 24 | 95 | 483 | 3 | 5.93% |
| 64 | eager | 1067 | 47.7 | 26 | 95 | 301 | 3 | 4.47% |
| 128 | inductive | 1895 | 45.5 | 26 | 81 | 461 | 86 | 2.40% |
| 128 | cartesian | 1895 | 47.7 | 31 | 81 | 390 | 86 | 2.52% |
| 128 | pathwise | 1895 | 71.2 | 28 | 119 | 1163 | 3 | 3.76% |
| 128 | eager | 2352 | 62.1 | 33 | 130 | 880 | 3 | 2.64% |
| 256 | inductive | 3958 | 51.6 | 26 | 87 | 1080 | 175 | 1.30% |
| 256 | cartesian | 3958 | 56.0 | 35 | 90 | 1019 | 175 | 1.42% |
| 256 | pathwise | 3958 | 79.8 | 29 | 117 | 2457 | 3 | 2.02% |
| 256 | eager | 5227 | 81.4 | 37 | 148 | 1644 | 3 | 1.56% |

`at` is the position the maximum was found at, which is the first correction
below. The median barely moves with n at all -- 22, 26, 26 for inductive --
so the mean is being carried by the tail, not by the typical edit.

**The mean grows, sublinearly, and the lazy sorts grow differently from the
eager one.** Per doubling:

| | 64 -> 128 | 128 -> 256 |
|---|---|---|
| inductive | 1.33x | 1.13x |
| cartesian | 1.38x | 1.18x |
| pathwise | 1.42x | 1.12x |
| eager | 1.30x | 1.31x |
| *log n predicts* | *1.17x* | *1.14x* |
| *log squared predicts* | *1.36x* | *1.31x* |
| *n^0.4 predicts* | *1.32x* | *1.32x* |

The three lazy sorts' ratios **shrink** -- 1.33 then 1.13 -- and a power law's
ratios do not shrink, so whatever the lazy average is, it is log-like rather
than n to some power; the second step lands almost exactly on log n. Eager's
ratios do not shrink (1.30, 1.31), which is what a power law looks like, and a
least-squares fit gives n^0.39 with the three points nearly on the line. So the
eager sort's average cost grows in n and the lazy sorts' in log n, which is a
difference in kind and not in constant -- the first such difference these
tables have shown.

Three points still cannot *pin* the lazy law: 1.33 then 1.13 sits between log n
and log squared, and a log-squared fit is within 11% at every size. But the
shape is settled, and so is the earlier reading, which was wrong. The
six-sample means (27.2, 42.2, 21.3, 26.3 for inductive at 64/128/256/512) were
flat and non-monotonic, and I reported them as evidence that the average case
might be O(1). The exhaustive means are monotonic and rising. Six uniform
samples could not see a 1.17x-per-doubling trend, and the non-monotonicity was
the sampling, not the algorithm.

The share of the graph, though, falls: 4.06%, 2.40%, 1.30% for inductive,
roughly halving per doubling, because the graph grows about 2.2x and the mean
about 1.2x.

**Correction: the highest-level cell is not the worst position, and the error
grows.** Every table above takes the root -- the cell of highest `symbolLevel`
in range -- as the worst case. For the lazy sorts it is not, and the gap widens
with n:

| n | root | its cost | worst position | its cost | ratio |
|---|---|---|---|---|---|
| 64 | 3 | 169 | 39 | 219 | 1.30x |
| 128 | 3 | 322 | 86 | 461 | 1.43x |
| 256 | 3 | 582 | 175 | 1080 | 1.86x |

Cell 3 has the highest level in range at every size up to 512, and it roots a
2 : n-2 split -- issue #72's degeneracy, one step further on. Removing the root
of a degenerate split disturbs *less* than removing a high node with a real
subtree on either side, which is what cells 39, 86 and 175 are. So the "at the
root, at scale" table below prices a high removal and calls it the worst one,
and by n=256 the true worst is nearly twice as dear.

For pathwise and the eager sort the root *is* the worst position at all three
sizes, which fits: their cost is dominated by how much of the tree's shape
moves, and removing the top reshapes from the top down.

The lesson for the report is that a worst case has to be **swept for**, not
picked by a property of the symbol. That is what `all` is for.

**Correction: cartesian's advantage is garbage, not work.** With `redone` as
the measure, cartesian and inductive are the same algorithm as far as work
goes:

| n | inductive | cartesian |
|---|---|---|
| 64 | 20.0% of own | 20.5% |
| 128 | 17.0% | 17.6% |
| 256 | 14.7% | 15.2% |
| 512 | 18.8% | 18.3% |
| 1024 | 19.3% | 20.7% |

At the root, at n=1024, cartesian redoes *more*: 3870 against 3621. What it
does less of is **garbage** -- 615 against 1033 -- which is the `onlyLeft`
column, cells the second run never names.

It does cap the true tail, consistently, at every size swept: 191 against 219
at n=64, 390 against 461 at 128, 1019 against 1080 at 256 -- 10 to 15% lower
at the worst position, while its mean and median are slightly higher. That is
"caps the tail and raises the floor" exactly, and it is invisible at cell 3,
where cartesian looks slightly worse.

So "cartesian caps the tail and grows with n" was right about the shape and
wrong about the cause: the earlier figure was `unmatched` = `onlyLeft` +
`onlyRight`, which is mostly garbage, and garbage is what cartesian saves.
Work redone it leaves alone.

**The worst case is a constant fraction of the graph.** `redone` at the root
holds between 14.7% and 20.7% of `own` from n=64 to n=1024, for both lazy
namings, while the graph grows 22x. Not logarithmic: Θ(n log n) work redone at
a high removal, and a Θ(log² n) mean over all removals. Those are the two
theorems the data supports, and they are very far apart -- at n=1024 the
mean is a few tens of nodes and the root is 3,621.

### A balanced input tree does not help, and the degeneracy is load-bearing

*Added 2026-09-09, with `pureLevelTree.Build` and `sweep.runNodeValsOver`.*

The premise was that level trees are too badly shaped for mergeSort to have a
hope: with the library's positional symbols the root splits 2 : n-2 at every
size up to 512, and the highest-level cell owns the root at every size. So we
prepared better-shaped trees **outside** the graph and introduced them as the
input tree, to find out what a good shape is worth before building one
incrementally. `levelTree.embed` is the door: it places a pure tree under the
names `fromList` would have used, so scaffolding put up by hand is still
canonically named.

Three shapes, all embedded, so every graph is composed the same way -- an
embedded tree and a merge network, and no list-into-tree phase in any of them:

- **level tree** -- what `fromList` decides, taken by building it inside
  `A.scratch` and erasing it, so only the shape survives.
- **contracted** -- walk the cells, and for each flip a coin on (seed, round,
  symbol) to keep it or combine it with the next; repeat until one node
  remains. Depth is the round count, so no symbol's coin can own the root.
- **independent gaps** -- the same idea with a rule that reads no walk:
  contract a gap when its own coin is 1 and its left neighbour's is 0. A
  quarter per round rather than a third, and no chain.
- **midpoint** -- perfect balance, as a control.

The shapes are as advertised. Depth at n=128: contracted 12, independent 12,
level tree 19. The contracted graph is genuinely *smaller* -- 1302 nodes
against 1510 -- which is the depth win, since a merge network's size is the
sum of its subtree sizes.

**And churn is several times worse.** Every position, n=128:

| mean redone | level tree | contracted | independent gaps |
|---|---|---|---|
| inductive | **37.3** | 203.7 | 115.4 |
| cartesian | 39.4 | 165.3 | 113.1 |
| pathwise | 63.0 | 227.0 | 176.7 |

| median | level tree | contracted | independent gaps |
|---|---|---|---|
| inductive | **19** | 96 | 24 |
| cartesian | 25 | **52** | 30 |
| pathwise | 21 | 56 | 25 |

| max | level tree | contracted | independent gaps |
|---|---|---|---|
| inductive | 435 | 811 | 663 |
| cartesian | **364** | 793 | 665 |
| pathwise | 1141 | 1117 | 1024 |

The control is far worse still: midpoint balance means 587.7 with a median of
681, because every midpoint moves when one element is removed and almost
nothing is left to reuse.

**The level tree wins under every naming**, and that reverses the premise. Its
degeneracy is *load-bearing* for repair: it concentrates cost onto a few
positions and leaves most edits nearly free -- a median of 19 in a 1510-node
graph -- where balance spreads a root path of Theta(n) onto every position.
Balance is right for sorting and wrong for repairing, and the two are not the
same question. Issue #72 is a trade-off, not only a bug.

**What was ruled out on the way.** Three candidate explanations, each measured
rather than argued:

- *The `level` field.* A contracted gap's level is the round it contracted in,
  and rounds shift under an edit where `symbolLevel` never does. Relabelling
  every contracted gap with `symbolLevel` instead -- which breaks the heap
  property, so it is a measurement and not a representation -- changes the
  total from 15776 to 15722, **0.3%**. Not the levels.
- *The embedded tree.* `A.Native.diffBySpace` splits the diff by the name each
  pointer hangs under. Per position the input tree accounts for 1.9 changed
  cells under `fromList` and 7.2 under contraction, against a merge-network
  difference of 166. **3%** of the gap. Not the tree.
- *Namespace churn.* Inductive names a merge cell inside a space opened for
  the reduction node, so a changed shape renames whole namespaces; cartesian
  opens none. Cartesian halves contraction's **median** (96 to 52) and leaves
  its mean and max alone (203.7 to 165.3, 811 to 793). So namespace churn is
  real, and it explains the typical position and not the distribution.

What remains, by elimination, is the shape -- and elimination is the honest
word: there is no measurement here that *explains* why the level tree wins,
only three that say what does not. The subtree-stability figures make the
puzzle sharper rather than softer: under one removal the contracted tree keeps
111 to 118 of its 126 subtrees and the level tree 114 to 116, so the *number*
of disturbed subtrees is much the same and the difference must be in their
sizes, which is not measured. That is the next thing to measure.

Two limits on all of the above: one size, one seed; and these are from-scratch
diffs, which bound what a repair would do rather than observing one. Repair
exists now (#106), and `Scene.realignMergeFromTree` runs a real one, so the
same matrix could be taken from actual signal and repair counts instead.

## How to play with each

A local build, since none of this is deployed:

    cd <worktree>
    cargo build --release --target wasm32-unknown-unknown -p fumola_wasm --no-default-features
    wasm-bindgen --target web --out-dir bindings \
      target/wasm32-unknown-unknown/release/fumola_wasm.wasm
    rm -f bindings/*.d.ts
    bash tools/assemble-site.sh --bindings bindings --pages pages --out /tmp/site
    python3 -m http.server 8140 --bind 127.0.0.1 --directory /tmp/site

Then `http://127.0.0.1:8140/web-play/#body=collections/levelTree:Scene.<name>`
for any of `examplePairInductiveVsCartesian`,
`examplePairInductiveRootRemoval`, `examplePairCartesianRootRemoval`,
`examplePairInductiveLeafRemoval`, `examplePairCartesianLeafRemoval`. Opening
the body rather than the call means the seed, size and removal are `let`s you
can edit in place, and the view survives the re-run.

To sweep, rather than to look:

    cargo build --release
    tools/sweep-run.sh /tmp/worst.csv root 64 128 256 512 1024 -- \
        lazy-inductive lazy-cartesian lazy-pathwise eager-split
    tools/sweep-run.sh /tmp/avg.csv all 64 128 -- lazy-inductive eager-split

`root` takes the highest-level cell, `all` every position, `random:N` a
uniform sample. One CSV row per sample, with `own` beside the counts so a
figure can be read as a share of its own algorithm's graph; the columns are
documented in `fumola/examples/mergeSort/sweep.fumola`, which is also where
the run's shape lives, so a table can be reproduced rather than re-derived.

To sweep over a prepared input tree instead of one built from a list, call
`Scene`-side `sweepOver(seed, size, positions, algorithm, shape)` with a shape
of `#levelTree`, `#contracted`, `#independentGaps` or `#midpoint`. And to ask
*where* an edit landed rather than how much of it there was,
`A.Native.diffBySpace` gives one row per name the pointers hang under -- the
input tree, the merge network, a stage thunk.

One caution about `redone` on a prepared tree: it excludes
`notEqualNonThunk`, on the reasoning that a cell is written by the thunk that
puts it and that thunk's evaluation is already counted. An embedded tree's
cells are non-thunk records whose puts belong to no thunk, so `redone` scores
them as zero. Read `nonThunk` beside it, as the split above does.

In a program, the strategy is a cell the merge network peeks:

    A.reset();
    let _ = `lazyMergeSort(`strategy) := #cartesian;

It has to be written after `A.reset()`, which is why `mergeFromTreeRunWith`
takes the strategy and writes it itself rather than leaving it to the caller.
An absent cell means inductive.

## Measurement notes

Traps already hit, recorded so they are not hit again.

**One removal position is not a measurement.** The first pass through sizes 3
to 5000 used cell 3 every time and concluded cartesian changed nothing. The
position dominates the result; sample it, and prefer random positions to a
stride so no structure of the sampling grid can be read as structure in the
answer.

**"How many nodes differ" is the wrong metric** if the question is repair.
It counts a cell with no counterpart and a cell whose contents changed as the
same outcome, and they are not: one is a rebuild and the other a repair.
Report the four numbers.

**`-` is arithmetic on numeric symbols.** Naming a pair
`(xc.symbol)-(yc.symbol)` silently subtracts, the network collapses onto a few
integer names, the lazy list becomes cyclic, and the process dies for memory
with nothing to say about naming. Use application: `(xc.symbol)(yc.symbol)`.
A literal head like `` `merge-x `` is safe because the head is not a number,
which is why the existing inductive code never hit this.

**A Fumola program cannot stream its output, and the reason is not a pipe.**
A sweep at n=1000 was written to print each sample as it landed, so a long run
could be interrupted without losing everything. Nothing reached the file, and
the diagnosis recorded here -- "the output sat in a pipe buffer" -- was wrong.
`Prim.print` puts its line in the agent's own output, and the host drains that
when evaluation *returns*: a process that is killed prints nothing, however it
is piped, and `stdbuf` cannot help.

Nor is that a defect to fix in `print`. The semantics has to run under Wasm,
where there is no stdout and the host is the only thing that can say what
output means. Streaming belongs in a `Value::Dynamic` that a host binds into
the Core -- an object with methods a program calls, printing or dumping as
that host is able -- which is worth doing and is not done. Until then the
process boundary is the flush, which is why `tools/sweep-run.sh` runs one
process per (size, algorithm).

**n=1000 sweeps are expensive and grow.** About 14 s per run, so 100 sampled
positions is 200 runs, and resident memory reached 10.2 GB before being
stopped -- more than two retained node maps should need, which is worth
understanding on its own.

## Open questions

- ~~**Does the tail shrink with n?**~~ Answered above: it widens. Cartesian is
  0.48 of inductive at the root at size 1000, against 0.80 at size 100.
- **Why is eager so much worse, in detail?** The allocation counts of `split`
  and `append` would say how much of the gap is extra nodes and how much is
  nodes that failed to match. The union sizes suggest both.
- **A level-dependent hybrid.** The namespace taint is O(N) only near the
  root, and partner churn dominates near the leaves, so cartesian above some
  treap level and inductive below should give the capped tail and the low
  median together instead of trading them. `reduce_` has the node symbol and
  can get its level from `Prim.symbolLevel`.
- **The reduction spine is unaddressed.** `mergeSort(recLeft(3))` and
  `recRight(3)` are named for the tree node and go `onlyLeft` under both
  namings. Some of what remains is not merge naming at all. Naming a
  subtree's reduction by the range of elements it covers is the same move as
  cartesian, one level up.
- **Where `pathwise` should fall.** It is independent of the data, so a
  removal that does not change the tree's shape at a point should leave that
  point's names untouched -- which sounds like inductive's best case without
  inductive's worst. It also cannot distinguish two different subtrees that
  sit at the same address, which is where it should lose.
- **Building `pathwise` without disturbing the other two.** Threading a path
  parameter through `reduce_` would put it in the environment of the spine's
  thunks, which is the contamination that peeking-and-branching avoids. A
  separate recursion used only by this strategy keeps all three closure sets
  disjoint; combining them later is a second problem.
