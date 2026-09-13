# Fumola for Haskell folks

If you write Haskell, most of Fumola will look familiar and one thing will
look wrong. This note is about the thing that looks wrong, because it is the
point.

## The familiar part

Fumola is a small strict functional language with algebraic data, pattern
matching, and thunks you force explicitly. A sequence library in it reads much
as it would in Haskell. It has a level-tree (treap-shaped) sequence
representation whose shape is a function of the data, so equal subsequences
have equal shapes -- the trick from Pugh and Teitelbaum's POPL 1989 function
caching paper, and the same reason you would hash-cons.

## The part that looks wrong

Every interesting type is written through a carrier:

```
type LevelTree_<X> = Pointer<LevelTree<X>>
type Binary<X> = { symbol : Symbol; level : Nat;
                   left : LevelTree_<X>; right : LevelTree_<X> }
```

You have seen this shape. It is `Fix f`, or `Cofree f a` when the carrier also
holds an annotation, or the extension fields of "Trees that Grow". So far, no
surprise: pick a carrier, thread it through the recursion.

The surprise is what the carrier is.

```
let p = `myCell := thunk { expensive() }
force p
```

`` `myCell `` is a *name*, chosen by the program, and `:=` binds it in a store.
This is not `newIORef`. `newIORef` hands you a fresh, opaque identity and you
have no way to ask for a particular one. Here you say which cell you mean, by
name, and two different parts of the program that say the same name get the
same cell.

If your reaction is "that is a global mutable hashtable keyed by strings, and
it will collide", you have understood it exactly, including the hazard. Read
on, because the hazard is the price of something.

## Why keys-are-values is not enough

The Haskell instinct is that a memo table is keyed by its argument. `memoize f`
builds a map from input to output; `Data.MemoTrie` does it structurally;
hash-consing does it for whole values. Keys are content, always.

Content keys give you *sharing*. Two equal values are one entry, no matter how
each was produced, and you never have to think about it. This is why laziness
plus sharing gets you so far, and why Pugh and Teitelbaum needed no naming
rules anywhere in their paper: with content keys, a collision *is* the cache
hit you wanted.

Content keys cannot give you *change propagation*, and the reason is a
one-liner: a content key cannot express "the same entry, now holding a
different value". Change the value and you have changed the key, so there is
no entry to invalidate, nothing to mark stale, and nothing to recompute
selectively. Incrementality has to come from re-running the whole computation
and hoping the cache catches most of it.

That is a real strategy -- it is the strategy of the 1989 paper -- but it is
not the strategy of `rustc`'s red-green, or Salsa, or Jane Street's
`Incremental`, or Adapton. Those all need a node that persists across a change
so it can be dirtied. To have that, the key has to be independent of the
value. Which means the key is a name, and names are the programmer's problem.

So:

| | content key | name |
|---|---|---|
| sharing | automatic | your problem |
| "same node, new value" | inexpressible | the whole point |
| collision | is sharing | is a bug |
| discipline required | none | real, with preconditions |

They are dual, not rival. If you use git you already live with both and do not
experience it as a dilemma: objects are content-addressed and immutable, refs
are names and mutable, and the interesting operations relate them. Fumola's
question is what that arrangement looks like when the thing being named is
every intermediate value of a running computation rather than a commit.

## What the discipline actually costs

Here is a concrete example, because the abstract version sounds harmless.

Mergesort's merge network has to name its cells. We built three schemes,
identical in computation -- the graphs are isomorphic, only the names differ --
and measured how much of the graph survives deleting one element of the input.

- **by position in the recursion**: names like `merge-0(R)(L)`, derived from
  where you are, nothing from the data.
- **by the tree node being reduced**: derived partly from the data.
- **by the pair of cells compared**: `1(13)`, derived almost entirely from the
  data.

Deleting the highest-level element of a 16-element input, counting nodes with
no counterpart in the other run: 25 for the middle scheme, 23 for the
pair-derived one, and **139** for the position-derived one. Delete a lowest-level
element instead and all three are identical to the number.

The reading that makes sense of that: all three are attempts to *approximate
content addressing using names*. The pair-derived scheme is the closest
approximation -- the same comparison, reached by any route, gets the same key,
which is the content-addressed property restricted to one operation -- and it
is the one that bounds the worst case. The position-derived scheme is furthest
from content and multiplies the worst case, because an address survives only
as long as the shape it addresses.

Put bluntly: **the naming-strategy question only exists because we insisted on
naming a derived structure.** Name it by content and there is no strategy to
choose.

## One place names win outright

Pugh and Teitelbaum's Section 5.2 is candid about a limitation. Their tree's
balance needs element levels to be independent random variables, and the level
is a hash of the element -- so duplicates all get the same level and the
decomposition degenerates to a list. They close the section with *"We are
pursuing research on a representation for sequences with many duplicate
elements, but it is currently an open problem."*

In Fumola the level comes from the element's *symbol*, not the element. Two
equal elements have distinct symbols, therefore independent levels, and the
balance argument is untouched. A sequence of a million copies of one value
decomposes exactly as well as a million distinct ones.

If you have ever hash-consed a sequence and watched it degenerate on repeated
data, that is the same wall, and this is what is on the other side of it. The
honest caveat is that determinism removes the averaging: the level is now a
fixed function of the symbol, so a badly distributed set of symbols stays badly
distributed forever rather than being fixed by the next coin flip. We traded a
duplicates problem for a frozen-realisation problem, which is the better
problem, because entropy in the symbols fixes it and nothing fixes duplicates.

## A diagnostic you cannot build with content keys

Because a named node keeps its identity across a change, you can compare two
runs of a program node by node, and get four outcomes rather than two:

| | |
|---|---|
| `equal` | same name, same value |
| `notEqual` | same name, **different** value |
| `onlyLeft` | in the first run only |
| `onlyRight` | in the second run only |

`notEqual` is the category content addressing cannot have -- under content keys
a node whose value changed is a different node, so everything lands in
only-one-side and the four collapse to two.

And that distinction is exactly the question repair asks. A node in `notEqual`
exists on both sides: dirty it, recompute it in place, invalidate its
dependents. A node in `onlyLeft` has no counterpart, so there is nothing to
repair -- it and everything above it start again. If you are measuring whether
an incremental system is any good, this is the measurement, and it is only
available on the naming side.

## What we think is worth trying

Carry both keys.

A name for propagation, and a content hash for sharing, where a node's hash is
a function of its children's hashes -- Merkle, so O(1) per node, not O(size).
Then two indexes: name to node, and hash to node. The name index does what
Adapton does. The hash index discovers, after the fact, that a freshly named
node is content-identical to one that already exists, and aliases it.

The concrete thing it buys: `append(fromList xs, fromList ys)` sharing with
`fromList (xs ++ ys)`. Under names alone those are structurally equal and
cell-disjoint, so nothing computed about one can be reused for the other -- and
that loss is precisely what a fresh name for the append operation costs you.

Our level tree is one field away from being a Merkle tree already; its content
is a symbol, a level, and two child pointers.

## If you want to try the idea in Haskell

You can simulate the naming half without a new language: a `Name` type, a
store keyed by `Name`, structures whose recursion goes through a `Ptr Name`,
and `unsafePerformIO` or a monad to hold the store. What you will not get is
help. Nothing checks that you kept names apart when building new structure, or
that you reused them when you meant to repair; nothing tells you that a name
you wrote clobbered a cell another computation was reading. The failure is not
a type error or an exception -- it is a silently wrong answer, or a dangling
pointer at the next read.

Fumola's contribution, such as it is, is making the names and the store part
of the semantics so those questions can be asked at all, and so that a
naming scheme can be *measured* rather than argued about. Whether the
two-key synthesis above is a good idea is unproven, and every measurement
quoted here is small -- sizes 16 to 5000, one seed, one edit at a time.

## Pointers

- Pugh and Teitelbaum, *Incremental Computation via Function Caching*, POPL
  1989. Definition 5.1 (levels), 5.2 (the decomposition), Figure 5.3 (append),
  Theorem 5.6 (its bound), Section 5.2 (duplicates, open).
- Hammer et al., *Incremental computation with names*, OOPSLA 2015 -- names in
  incremental computation, which is where Fumola's come from.
- `docs/cache-naming.md` in this repo, for the same argument without the
  Haskell framing.
- `docs/mergesort-report-notes.md`, for the measurements and how to reproduce
  them.
- `fumola/collections/levelTree.fumola`, for the representation itself.
