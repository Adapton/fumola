# What is a cache key a function of?

Notes on naming for reuse, written for someone who thinks about
content-addressed storage. The short version: there are two answers to the
title question, they are dual rather than rival, and Fumola is in a position
to hold both at once. The long version is below, with what is measured
separated from what is conjectured, because most of this is conjectured.

## The two answers

**Content-derived.** The key is a function of the value. Two values that are
equal have one key, so sharing is automatic and requires no discipline from
the programmer. This is hash-consing, Merkle DAGs, git objects, and it is what
Pugh and Teitelbaum's function caching does for incremental computation.

**Identity-derived.** The key is chosen independently of the value. A node
keeps its key when its contents change, so the store can hold "this node, now
different", dirty it, and repair what depended on it. This is Adapton, and
Fumola's names.

The duality is sharp, and it is easiest to see as a pair of things each cannot
say.

Content keys cannot express *same node, new value*. Under them a changed node
is simply a different node, so there is no node to dirty and nothing to
propagate; incrementality has to come from re-running and hitting the cache.

Identity keys cannot express *same value, therefore same node*. Two
structurally identical results reached by different routes get different keys
and share nothing, so reuse becomes a naming problem and the programmer
acquires an obligation.

Anyone who has used git already lives with this and does not experience it as
a dilemma: objects are content-addressed and immutable, refs are names and
mutable, and the interesting operations are the ones that relate them. The
question this note is about is what the same arrangement looks like when the
thing being named is not a commit but *every intermediate value of a running
computation*.

## What content addressing buys, in the 1989 form

Pugh and Teitelbaum, "Incremental Computation via Function Caching", POPL 1989,
represent a sequence as a balanced binary tree whose shape is a function of the
contents. Their Definition 5.1 sets `level(x)` to the largest `i` such that
`hash(x)` is a multiple of `2^i`, and their Definition 5.2 -- the *chunky
decomposition scheme* -- splits a sequence immediately before its
maximal-level element, ties going to the rightmost, ignoring the first
element.

The consequence is the property they need: the decomposition of a sequence
depends only on the levels of its elements, so two similar sequences decompose
into mostly the same subsequences (their Theorems 5.2 through 5.6). Insert an
element and only `O(log n)` subsequences change. Nothing is named, nothing is
dirtied, and no discipline is imposed -- the cache does the work because equal
subsequences are literally the same key.

Their append (Figure 5.3) is a join with a middle break, and it is worth
reading for how little it needs: three candidate breaks -- the most preferable
in `S`, the one between `S` and `T`, the most preferable in `T` -- chosen by
comparing levels, in `O_prob(log|S| + log|T|)` by Theorem 5.6.

Two things they pay for it.

They need constant-time equality and hashing on values, which they get by
assuming hash-consing or lazy structure sharing.

And they cannot handle duplicates. Section 5.2 is candid: levels must be
independent random variables, and *"if a sequence simply contained n instances
of an element x, all the elements would have the same level, and the
decomposition rule would simply remove the rightmost element of a sequence"* --
degenerating to a list. They close it with *"We are pursuing research on a
representation for sequences with many duplicate elements, but it is currently
an open problem."*

## What names buy, and one thing they buy outright

Fumola gives every element a symbol and derives the level from the symbol
rather than from the element (`Prim.symbolLevel`, four geometric samples packed
out of the symbol's hash). Names in incremental computation are not new --
see Hammer et al., "Incremental computation with names", OOPSLA 2015 -- but
using the names *carried by the data* to determine a canonical balanced shape
is the specific move here, and it has a consequence worth stating plainly:

**It dissolves their open problem.** Two equal elements have distinct symbols,
therefore independent levels, and the balance argument survives untouched. A
sequence of `n` copies of one element decomposes exactly as well as a sequence
of `n` distinct ones. This is not a trade; it is strictly better, and it falls
out of naming rather than being engineered.

The honest caveat is that determinism removes the averaging. The paper's
theorems quantify over the coin flips; here the level is a fixed function of
the symbol, so a set of symbols whose levels happen to be badly distributed
stays that way permanently. Small positional symbols are the case to watch,
being few and shared across every structure that uses them (Adapton/fumola#72).
We have traded a *duplicates* problem for a *frozen realisation* problem, which
is the better problem to have, because entropy in the symbols fixes it and
nothing fixes duplicates.

## An instrument that only exists on the naming side

Because a named node keeps its identity across a change, two runs of a program
can be compared node by node, and the comparison has four outcomes rather than
two:

| | meaning |
|---|---|
| `equal` | same name, same value |
| `notEqual` | same name, different value |
| `onlyLeft` | in the first run and not the second |
| `onlyRight` | in the second and not the first |

`notEqual` is the category content addressing cannot have. Under content keys
a node whose value changed is a different node, so every change lands in
`onlyLeft`/`onlyRight` and the four dimensions collapse to two.

That distinction is not cosmetic, it is the whole question repair asks. A node
in `notEqual` exists on both sides, so it can be dirtied and recomputed where
it stands and its dependents can be invalidated rather than rebuilt. A node in
`onlyLeft` has no counterpart: there is nothing to repair, and it and
everything above it start again.

So the naming side comes with its own microscope, and content addressing does
not. This matters for the research programme more than for the runtime: it is
how you find out whether a naming scheme is any good.

## Naming schemes are an axis, not a menu

We built three ways to name the cells of one computation -- the merge network
of a mergesort -- holding the computation itself fixed. The graphs are
isomorphic; only the names differ.

| scheme | a cell's name | derived from |
|---|---|---|
| `pathwise` | `mergeSort(merge-`0(R))(4)` | the traversal path: position, no content |
| `inductive` | `mergeSort(merge-3)(6)` | the tree node's symbol, and the winner |
| `cartesian` | `mergeSort(1(13))` | the pair of cells compared: nearly all content |

Measured, at one input under all three (size 16, seed 10), every pair is
perfectly symmetric -- 49 cells exist under one naming and not the other, one
for one, with 49 merge cells and 30 spine cells accounting for the 79 in the
pathwise comparisons. That symmetry is what an isomorphic graph with different
names looks like.

Measured, under one removal at the highest node in the input:

| scheme | equal | notEqual | onlyLeft | onlyRight | unmatched |
|---|---|---|---|---|---|
| inductive | 87 | 56 | 20 | 5 | 25 |
| cartesian | 83 | 61 | 19 | 4 | 23 |
| pathwise | 61 | 25 | 77 | 62 | 139 |

and under one removal at the lowest node, all three are identical to the
number (134, 20, 9, 0). Over all 99 removal positions at size 100, cartesian's
mean unmatched is worse than inductive's (32.3 against 27.0) and its maximum is
better (155 against 193): it caps the tail and raises the floor.

The reading that makes sense of this is that **all three are attempts to
approximate content addressing using identity keys.** `cartesian` -- whose key
is a function of the two operands -- is the closest approximation, which is why
it is the one that bounds the worst case: the same comparison, reached by any
route, gets the same key, which is exactly the content-addressed property
restricted to one operation. `pathwise` is the furthest from content, and it
multiplies the worst case by five and a half, because an address survives only
as long as the shape it addresses.

Put the other way: the naming-strategy question is one you only face if you
insist on identity-naming a *derived* structure. Name it by content and there
is no strategy to choose and reuse is maximal by construction.

## Two modes, and a rule for choosing

Fumola can express both styles, and the difference is worth naming.

**Structural naming.** The name is a function of the content. Writing the same
name twice writes the same value, so the write is idempotent and clobbering is
not a hazard to be avoided but an impossibility -- collision *is* sharing. This
is why the 1989 paper contains no naming discipline anywhere.

**Pointer naming.** The name is chosen. Building new structure requires
deliberately fresh names, or an unrelated computation's cells are overwritten;
reuse and repair require deliberately *equal* names. The obligation is real and
it has preconditions worth documenting at each use.

Since structural naming forfeits repair -- a structurally named node cannot be
dirtied, because its name moves when its contents do -- the choice is per node,
not per program, and there is a rule:

- **What you intend to edit gets a pointer name.** Change propagation needs a
  fixed identity to dirty.
- **What is merely derived gets a structural name.** You never dirty it; you
  look it up and find it already built.

## The synthesis worth trying

Carry both keys on every node.

A name, for propagation, and a content hash, for sharing. The content hash of a
node is a function of its children's hashes, so it is `O(1)` per node rather
than `O(size)` -- Merkle, and our level tree is one field away from being a
Merkle tree already, its content being a symbol, a level and two child
pointers. Then keep two indexes: name to node, and hash to node.

The name index does what Adapton does. The hash index does something Adapton
cannot: it discovers, after the fact, that a freshly named node is
content-identical to one that already exists, and aliases it. Names would still
make repair possible; the hash index would recover the sharing that differing
names throw away.

The concrete thing it would buy: `append(fromList xs, fromList ys)` sharing
with `fromList (xs ++ ys)`. Under names alone these are structurally equal and
cell-disjoint, so nothing downstream of one can reuse anything computed about
the other -- and that loss is exactly what a fresh append symbol costs you.
It is objects and refs, for the interior of a computation rather than for its
history.

## Costs, and what is unproven

**Garbage.** Nothing is overwritten, so nothing is reclaimed by being
replaced. Fumola already does not reclaim memory without a worker
(Adapton/fumola#68); structural naming makes that a property of the design
rather than an omission.

**A blinded instrument.** `notEqual` cannot occur for structurally named
nodes, so the four-dimensional diff collapses to two dimensions exactly where
the synthesis applies. A win for the runtime is a loss for the microscope, and
since the microscope is how we evaluate naming, that is a real methodological
cost rather than a footnote.

**Two indexes, two costs.** Whether the hash index pays for itself is an
empirical question nobody here has asked yet, and the answer surely depends on
how much structurally-identical-but-differently-named material a real program
produces. We have no measurement of that at all.

**And the measurements above are small.** Everything in the table is size 16
to size 100, one seed, one edit. Separately we found that churn from a single
removal is roughly constant in absolute terms -- about 300 nodes at sizes 500
through 5000 -- while the graph grows to 103,800, so at size 5000 either naming
already reuses 99.4% of the graph. The naming question is a small-input
question in the regime we have measured, and we do not know whether the *tail*
behaves that way, because the tail is only measured at size 100.

## References

- William Pugh and Tim Teitelbaum. Incremental Computation via Function
  Caching. POPL 1989, 315-328. Definition 5.1 (levels), Definition 5.2 (chunky
  decomposition), Figure 5.3 (append), Theorem 5.6 (its bound), Section 5.2
  (duplicate elements, stated as open).
- Matthew A. Hammer et al. Incremental computation with names. OOPSLA 2015.
- Adapton/fumola#72, on the frozen realisation of a deterministic level draw.
- Adapton/fumola#68, on memory never being reclaimed without a worker.
- `docs/mergesort-report-notes.md`, for the measurements and how to reproduce
  them, and `fumola/collections/levelTree.fumola` for the representation.
