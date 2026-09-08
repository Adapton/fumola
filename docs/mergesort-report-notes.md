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

**Verify that incremental output is actually incremental** before relying on
it. A sweep at n=1000 was written to print each sample as it landed, so a long
run could be interrupted without losing everything. Nothing reached the file:
the output sat in a pipe buffer for 27 minutes and was lost when the run was
stopped at 10.2 GB. Test the streaming on a small case first.

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
