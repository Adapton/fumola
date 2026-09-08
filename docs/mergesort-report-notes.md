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

### At scale the whole question shrinks

Removal at cell 3, both strategies, whole-history diff:

| n | inductive unmatched | cartesian unmatched | union | churn as % of union |
|---|---|---|---|---|
| 100 | 113 | 109 | 1,496 | 7.6% |
| 500 | 276 | 282 | 8,910 | 3.1% |
| 1000 | 310 | 304 | 18,402 | 1.7% |
| 2000 | 276 | 274 | 39,465 | 0.70% |
| 5000 | 269 | 275 | 103,824 | 0.26% |

Churn from one removal is roughly constant -- nodes touched (unmatched plus
notEqual) are 591, 663, 594, 572 at n = 500, 1000, 2000, 5000 -- while the
graph grows to 103,800. So the fraction decays like 1/n and either naming
already reuses 99.4% at n=5000. The merge network's reputation for instability
is a small-input effect, and a claim about it measured at size 16 does not
survive scale. Whether the *tail* also shrinks with n is not yet measured; see
below.

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

- **Does the tail shrink with n?** The constant-churn result above is the
  average at one position. The interesting claim is about the maximum over
  positions, which is only measured at n=100 (193 against 155). A random
  sample of positions at n=1000 would say whether the gap widens or closes.
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
