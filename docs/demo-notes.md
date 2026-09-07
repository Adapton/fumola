# Showing the pair view: notes for a demo

Three examples in order, each answering the question the previous one raises.
Numbers are measured, so they can be said out loud without hedging.

Open each with the library, or by link. The fader is bottom right; `trail`
sets how strongly the glide indicators show; the cluster at the top right
walks the camera around and can sweep the fader by itself.

---

## 1. A list becomes a tree — the stable case

`collections/levelTree:Scene.examplePairTreeAfterRemoval`

**61 objects. 20 of 80 nodes differ (25%): 60 equal, 16 not equal, 4 only in the
left run, 0 only in the right.**

Start here because it is small enough to follow object by object, and because
it is the case that behaves.

Drag the fader slowly. Almost everything glides: **54 of 61 objects differ only
in position**. Four go — the removed cell's own nodes — and they brighten to
magenta as you pass the halfway mark, which is where a thing that is leaving is
worth looking at.

The thing to say: the tree is a function of its contents, so an edit moves
things rather than remaking them. And it holds at scale — over 900 samples the
number of changed nodes stays near constant (13.7 at n=16, 17.5 at n=1000)
while the graph grows 62×, so the changed *fraction* falls to 0.35%.

Worth pausing on the trail colours: white-ish paths in the object's own colour
mean the two runs agree about that node; pink means they named it and
disagreed. Magenta is anything the two runs do not share.

---

## 2. A tree becomes a sorted list — where it stops behaving

`collections/levelTree:Scene.examplePairMergeFromTree`

**334 objects. 81 of 162 nodes differ (50%).**

The tree is at the bottom, the merge network above it. Same edit as before, one
cell removed. The fraction has doubled.

This is the rung that carries the argument, and it is worth using the camera
here: press `beside` and let it walk, because the network is a three-dimensional
thing and a still picture flattens the part that matters. `ping-pong` frees both
hands.

The thing to say: this is not a different algorithm being unstable, it is the
same edit costing twice as much once the merge network is involved.

---

## 3. The whole of mergeSort — and the reason

`examples/mergeSort/mergeSort:examplePairMergeSortRootCell` — **82 of 163 (50.3%)**

`examples/mergeSort/mergeSort:examplePairMergeSortTypicalCell` — **27 of 163 (16.6%)**

Show them one after the other. Same program, same seed, same size. **The only
difference is which cell was removed.**

Then the point of the whole demo: the expensive one is not expensive because of
where it sits in the list or what it holds. Cell 3 is expensive because
`symbolLevel(3)` happens to be the largest of the first few hundred, so its
symbol roots the tree — and a merge thunk is named for the input-tree node whose
stream it is built from, so that symbol's reach is however many streams descend
from its node. Unbounded, and largest at the root.

**Which edit is the expensive one is decided by a hash.**

If there is time, the scaling makes it sharper still, because an ordinary edit
is *already* stable and only the root edit is not:

| size | cell 3 | cell 13 |
|---|---|---|
| 16 | 82 changed (50.3%) | 27 changed (16.6%) |
| 32 | 144 (38.3%) | 29 (7.7%) |
| 48 | 168 (29.4%) | 33 (5.8%) |

Cell 13's cost barely moves — 27, 29, 33 — while the graph grows from 163 nodes
to 572. Cell 3's grows with the input.

---

## What it is all for

The fix is a naming, not an algorithm: name each merge thunk for the **pair of
input cells whose comparison produced it**, rather than for the tree node its
stream came from. Then the root symbol names O(log n) nodes like every other
symbol, and the merge network should get the stability the input tree already
has.

These three examples are the *before*. They exist to be run again against the
*after*.

---

## Practical notes

- **Order matters.** Small and stable, then bigger and not, then the same edit
  twice at different costs. Each one raises the question the next answers.
- **Drag slowly the first time.** The glide is the explanation; at speed it
  looks like a transition effect.
- **The outline panel is fetched on demand**, so opening it costs a moment.
  Objects are the point; outlines are rarely worth showing live.
- **Numbers are on screen.** The summary line under the picture gives the four
  counts, so there is no need to remember them.
- **A screenshot names itself.** The example's path and the camera position are
  drawn on the picture, so a still can be shared and returned to.
