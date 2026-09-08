# Two names for the same computation

A tutorial note, built from five scenes in `fumola/collections/levelTree.fumola`.

The merge network of a mergesort has to name its cells. The name decides what
a later run can reuse, and nothing else about the program changes when you
change it -- which makes it an unusually clean thing to look at, because the
picture of the graph stays the same and only the colours move.

## The two strategies

**Inductive** names each merge continuation for the element that won the
comparison, inside a namespace opened for the node of the reduction tree the
merge belongs to:

    do within space (`merge-symbol) { mergeInductive(xs, ys) }
    next = xc.symbol := thunk { mergeInductive(force(xc.next), ?yc) }

so a cell is `mergeSort(merge-3)(6)`. Both halves of that move when the input
is edited: the tree reshapes, and the winner at a given position changes.

**Cartesian** names it for the pair of cells compared, and opens no namespace
at all:

    let compared = (xc.symbol)(yc.symbol);
    next = compared := thunk { mergeCartesian(force(xc.next), ?yc) }

so a cell is `mergeSort(1(13))`. A merge network never compares the same two
symbols twice, so the pair is a name no other cell can claim -- which is what
lets the namespace go. The pair is ordered `xs` before `ys`, which is already
canonical: `xs` is the left subtree and `ys` the right, so the `xs` side is
always earlier in the input list, and removing some other element reshapes the
tree without reordering what remains.

Which one runs is decided by peeking a cell, `lazyMergeSort(`strategy)`, so
reading the choice records no dependency and nothing binds it -- a program that
never writes the cell sorts exactly as it always did.

## Read the vertical scene first

`examplePairInductiveVsCartesian` runs the same unedited input under both
namings. The computation is identical and the graph is isomorphic, so the
objects sit one on top of the other in space and the fader slides names from
one strategy to the other without anything moving.

    equal 80   notEqual 34   onlyLeft 49   onlyRight 49

The 49 and 49 are the whole story. Every merge cell exists under one naming
and not under the other, one for one -- that exact symmetry is what an
isomorphic graph with different names looks like. The 114 cells that agree are
the list, the tree, and the reduction spine, none of which the strategy
reaches. That is the boundary, drawn in colour.

## Then the four edit scenes

The other four are two removals seen under each naming. Which element you
remove matters far more than which naming you use, so it is held fixed across
each pair. A treap puts a symbol at a height drawn from its own hash: for seed
10 and size 16, cell 3 is the highest node in the input (`symbolLevel`
1581569) and cell 10 the lowest (4).

| pair | equal | notEqual | onlyLeft | onlyRight |
|---|---|---|---|---|
| root removal, inductive | 87 | 56 | 20 | 5 |
| root removal, cartesian | 83 | 61 | 19 | 4 |
| leaf removal, inductive | 134 | 20 | 9 | 0 |
| leaf removal, cartesian | 134 | 20 | 9 | 0 |

**The leaf rows are identical.** Not close -- the same four numbers. When the
tree does not move, the naming cannot matter, and that is the control that
makes the root rows mean something.

**The root rows differ in the kind of disagreement, not only the amount.**
Cartesian has fewer cells with no counterpart (20/5 becomes 19/4) and more
cells that have one and hold something else (`notEqual` 56 becomes 61).

That distinction is the one repair cares about, and it is easy to miss if you
only count how many nodes differ. A cell in `notEqual` is known to both runs:
it has an identity on each side, so it can be dirtied and recomputed where it
stands, and its dependents can be invalidated rather than rebuilt. A cell in
`onlyLeft` has nothing to correspond to. There is nothing to repair -- it and
everything built on it begin again.

So the honest summary of what cartesian buys is not "fewer nodes change". It is
"more of what changes is repairable".

## What this does not show

Three things, measured elsewhere and worth stating so the scenes are not read
for more than they say.

**At size 16 the effect is small.** Sweeping every removal position at size
100, cartesian wins nine of the twelve highest-churn positions and sometimes
by a lot -- 53 unmatched becomes 15, 125 becomes 67, 139 becomes 93. At size
16 the network is too small for a namespace to be worth much.

**It loses at most positions.** Over all 99 positions at size 100 the mean
unmatched goes 27.0 to 32.3 and the median 18 to 25, while the maximum goes
193 to 155. Cartesian caps the tail and raises the floor: it removes
inductive's worst case and pays for it with churn of its own, because an
element that wins many comparisons accumulates a cell per partner it meets and
benign edits shuffle those partners.

**The whole question shrinks with size.** Churn from one removal is roughly
constant -- around 300 nodes at sizes 500, 1000, 2000 and 5000 alike -- while
the graph grows to 103,800. At size 5000 either naming already reuses 99.4% of
the graph. The merge network's reputation for instability is a small-input
effect, and a claim about it measured at size 16 does not survive scale.

## The next thing to try

Make it level-dependent. The namespace taint is O(N) only for nodes near the
root, and partner churn dominates near the leaves, so use cartesian names
above some treap level and inductive names below. `reduce_` has the node symbol
already and can get its level from `Prim.symbolLevel`. That should give the
capped tail and the low median together, rather than trading one for the other.

Also unaddressed by either strategy: the reduction spine. `mergeSort(recLeft(3))`
and `recRight(3)` are named for the tree node and go `onlyLeft` under both
namings. Some of what is left is not merge naming at all.
