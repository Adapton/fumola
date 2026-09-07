# Building the pair view: what broke, and how each was found

A log of the defects turned up while building `#[examplePair]` and its
crossfader, September 2026. Kept because most of them are not specific to this
feature: several are about the boundary between Fumola and a host, one is
about what a colour means, and the majority were invisible to reading and
visible only to use.

Ordered by how they were found rather than by severity.

---

## 1. A `Map` crosses the boundary as a rendering, so the panel read zero

**Symptom.** Every panel of a pair reported *"0 of 0 nodes differ"* while the
CLI reported 60 equal, 16 not equal, 4 only-left, for the same program.

**Mechanism.** `A.Diff.NodeVals` is a `Map`, whose representation is hidden.
Across the wasm boundary it arrives as `tag: "Opaque"` with a *string* — the
map rendered — not a structure. The page was calling `Array.isArray` on it and
getting `0` for every category.

**Fix.** The pair states its own counts. `W.diffSizes` computes them in Fumola,
where the map is a map, and `ExamplePairOutput` carries them as numbers.

**Worth keeping.** The wrong fix was available and tempting: parse the
rendering in JavaScript. That would have worked and would have coupled the page
to the debug format of a hidden type.

**Found by** running the example in the page and comparing against the CLI. No
amount of reading the page would have shown it, because the code was correct
about a shape the value does not have.

---

## 2. A loading message that waited for a frame a hidden tab never sends

**Symptom.** The overlay saying a run was under way never went away. The output
panel stayed on `"starting…"` and the run never began.

**Mechanism.** `fumola_eval_top` is one synchronous call and holds the main
thread, so a message shown in the same task never paints. The overlay therefore
went up, `await`ed a frame, and only then started the run — and
`requestAnimationFrame` does not fire in a hidden or throttled tab. The await
never settled.

**Fix.** Two parts. The wait races a timer, so it settles wherever frames are
not running. And the overlay is gone entirely: a plain line of text in the
output panel has no visibility flag, so there is nothing to leave set. Whatever
the run does next overwrites it, including every failure path.

**Worth keeping.** The first fix was the timer alone, and it was not enough as a
design: an overlay needs a flag, a flag can be stuck, and the way to make that
impossible is to have no flag. Removing the mechanism beat repairing it.

---

## 3. A hoisted import was copied rather than re-addressed

**Symptom.** Opening an example's body failed with
`ModuleFileNotFound(ModulePath { local_path: "../system/adapton" })`.

**Mechanism.** The body feature lifts a module's own import lines into a
top-level program. `importsOf` decided a path was already absolute by testing
for a `/` — correct while a bare name meant a symlinked neighbour and anything
with a slash started at the library root. After #74 a module says where its
neighbours really are, so *every* relative path has a slash, and all of them
were copied unchanged into a program whose directory is not the module's.

**Fix.** `resolveImport` rebuilds each path against the module's own directory
and folds the `..` segments away.

**Worth keeping.** #74's blast radius was checked by diffing the wasm export
list and confirming every caller still resolved. That check was sound and could
not see this: the API did not change. What changed was the shape of the data
flowing through it — a `/` stopped meaning "absolute".

And the obvious test would have missed it too. The default view kept working,
because `exampleMergeSort`'s body mentions no alias but `M` and unused imports
are filtered out before they can fail. It took clicking the one body that
reached for its module's neighbours. *"I opened the page and it was fine"* was
true and meant nothing.

---

## 4. A body could not call a private helper

**Symptom.** `UnboundIdentifer("treeFromListRun")` when opening the pair
example's body.

**Mechanism.** `qualify` puts the module in front of the names that came from
it, and by design leaves private helpers alone — its own comment says so, since
they cannot be reached from outside anyway.

**Fix.** Made the helper public.

**Worth keeping.** This is a general trap for `#[example]`, not a fact about one
example: any example whose body calls a private helper cannot be opened. If
bodies are to be the default way to read an example (#82), the linter that
checks an example takes `()` should check this too.

---

## 5. A trampolined run reported its last chunk as the whole run

**Symptom.** The summary read *"5,673 Fumola steps"* for a run that took
138,271.

**Mechanism.** Chunked evaluation reports `counts` per chunk. The page displayed
the final reply's counts, and a final chunk is a small and arbitrary fraction of
the run.

**Fix.** Terminal replies carry `steps` for the whole run, measured from the
snapshot taken when it began. A test fails if a final reply reports its chunk
instead.

**Worth keeping.** An earlier version of the same mistake pointed the other way:
`steps` was the instance's *lifetime* total, which opens at tens of thousands
because importing the library costs that much, and the first chunk of a
138,271-step run read as 137,680. Both numbers were real; neither was progress.

---

## 6. Red was attached to the milder event

**Symptom.** A red path appeared on a node that had not been deleted.

**Mechanism.** Working as specified, which was the problem. Red marked
`notEqual` — a node both runs have and disagree about — while the node the edit
actually removed simply faded away. Of 16 `notEqual` nodes only three have scene
objects at all, so the loudest colour in the picture was on
`inputTree(2-binary)`, whose offence was that a child pointer changed.

**Fix.** Red means *leaving*: a vanishing node heats to red as the fader moves
away from it. Changed paths are pink. One colour cannot mean both "this is
going" and "this is different".

**Worth keeping.** The bug was not in the data or the code; both were right. It
was in what the encoding claimed. Worth stating because it is the kind of defect
tests cannot hold.

---

## 7. The right run's pointer lines were never hidden

**Symptom.** Twice as many edges as the scene has, worst at the far right where
they coincide.

**Mechanism.** Matched *objects* were hidden on the right side, since the left
side's copies glide and would otherwise appear twice. Lines were not objects in
that sense and were never hidden, so the left run's redrawn lines and the right
run's own lines were both present.

---

## 8. A pointer line outlived the object it pointed at

**Symptom.** The removed cell's box faded out while the pointer into it stayed
at full strength, pointing at nothing.

**Mechanism.** A line belongs to two objects and survives only while both do.
Fading was per object, and lines had no owner to fade with.

---

## 9. An edge survives if the *edge* survives, not if its ends do

**Symptom.** At the far right the picture was not a binary tree: some nodes had
three children, others had lost one.

**Mechanism.** The left run's wiring was drawn at the right run's positions.
Removing a cell does not only move things, it rewires them — for the tree pair,
25 edges are in both runs, 5 only in the left, 3 only in the right. The test
asked whether an edge's two ends were present in both runs, and

    inputTree(2-binary):right -> inputTree(2-element)

has both ends in both runs and exists only in the left. So it was treated as
shared and kept at the far right, while the three edges only the right run has
were hidden as duplicates.

**Fix.** An edge is identified by where it runs from and to, and classified by
whether that identity appears in both runs.

**Worth keeping.** This is the deepest of the three edge bugs and the one whose
lesson generalises: an object-level diff is not an edge-level diff, and a
structure's shape lives in the edges. The same caution applies to any future
scene delta (#83) — 75% of nodes being equal did not make 75% of the wiring
equal.

---

## 10. One non-ASCII character hides every attribute below it

Diagnosed by a parallel session, reproduced here, filed as #80.

**Symptom.** A section sign in a doc comment made the whole `examplePairs`
section vanish from the library, with no error, while examples earlier in the
same file were still listed.

**Mechanism.** `fumola_tokens` reports **byte** offsets; the page reads them
with `substr`, which counts **UTF-16 code units**. Every character above U+007F
makes the two disagree, and everything after it is read off by the accumulated
delta. The attribute scanner tests adjacency, so it cannot match.

**Status.** Open. The fix wants making once at the boundary rather than at each
of the six places the page uses spans, or a half-fixed file would look fixed
while the caret and gutter stayed wrong.

---

## 11. An adaptive chunk size that ran away

**Symptom.** The step count appeared once, for work already finished, instead of
rising.

**Mechanism.** Chunk size aimed at a target time and scaled by how long the last
chunk took, with no bound on the ratio. A fast first chunk asked for one large
enough to contain the rest of the program.

**Fix.** Growth capped at 2× per adjustment, target lowered.

---

---

# How the encoding got decided

The defects above are one half of the log. The other half is the visual design,
which was not designed and then built: every step of it came from looking at
the thing and finding the previous step wrong. Recorded because the reasoning
is not recoverable from the final state.

## Crossfade, then glide

The first version dissolved between two pictures: both runs in one canvas, one
fading out as the other faded in. It worked and it was hard to read.

What it missed is measurable. For the tree pair, **54 of 61 objects differ only
in position** — the two runs are not two pictures, they are one picture whose
parts move. So a matched object is now drawn once and slid from where the left
run puts it to where the right run does, and the fader reads as a journey from
the old layout to the new one. The verdict on that change was immediate and not
mine: *"way, way easier to understand."*

The crossfade survives as the fallback for when nothing matches, which is the
case it was always right for.

## The fader's curve

Not linear, and the reason is legibility rather than taste. At either end one
run is solid and the other gone; in the middle both sit at 0.7. Two structures
at full strength read as one confused structure, and two at half strength read
as neither.

## Three attempts at "which way am I going"

**A white glow** on the side being entered, decaying after the fader stopped.
Dropped. Once the trails and the gliding existed it was a third signal for
something already said twice — and it was half broken in a way that revealed
the preference: matched objects are drawn from the left run only, so lighting
the right run lit things that are hidden. Moving left glowed; moving right did
nothing. The bug was preferred to the feature.

**Brightness scaled by fader position.** Dropped on the grounds that the same
gesture should not mean different things in different places: the cue is about
direction, and direction is the same claim wherever it is made.

**A trail**, which is what stayed. Each glide indicator is cut into ten
segments; a segment lights when the fader passes through it and cools over the
next second. The decay is in *time*, not in distance from the fader — a window
around the fader's position is a highlight, symmetric and blind to which way
you are going, where heat applied on passage and shed on a clock follows the
drag and lingers behind it.

## What a colour is allowed to mean

Three revisions, each narrowing it.

At first red marked `notEqual`. That put the loudest colour in the picture on
the mildest event: of 16 `notEqual` nodes only three have scene objects, so red
landed on a node whose offence was a changed child pointer, while the node the
edit actually *removed* faded away quietly.

So red became "leaving" and changed-paths became pink. Better, and still one
code too many: a node only one run has and a node the runs disagree about are
both the edit showing itself. They now share one magenta, and the eye has one
fact to learn instead of three.

Meanwhile the indicators for nodes both runs *agree* about stopped being white
and took the colour of their object — green for a box, blue for an orb. White
is what a moving object glows, so white indicators competed with the things
they were about; in the object's own colour they read as its wake.

The remaining emphasis peaks at the **halfway** mark, which is where a node
only one run has is worth looking at: the moment the edit is happening rather
than before or after. It is held opaque through that peak, so the brightest
moment is not also the most transparent one.

## Thickness, and a dial

Indicators began as `THREE.Line`, which cannot be thick: `linewidth` is ignored
in WebGL whatever the material asks for. They are cylinders now, the shape an
action arrow already uses.

They were then tuned faint enough not to hide the structure they are about —
which turned out to be faint enough not to notice. Rather than guess again, the
strength is a dial: how much is too much depends on the scene and the screen,
and neither of us could pick a number that was right for both the 61-object
tree pair and the 493-object mergeSort pair. Moving the dial lights the whole
trail for a second, because a dial whose effect you cannot see while setting it
is not a dial.

## What the picture says about itself

A screenshot arrives without the URL that produced it, and "which example is
this" is the first question anyone asks of one. So the example's path is drawn
on the picture — and only when the code is a loaded example, since a
hand-written program has no path to name.

The camera's position is written there too: how far round, how far up, how far
back. That was asked for so that a view could be described and returned to
across a conversation, which is what a screenshot cannot do on its own.

## Where the cost actually was

Worth recording because the intuition was wrong twice.

The step-limited eval was built to report progress on a run that felt slow.
Measured, the stepping is **56 ms** of a wait dominated by a **196 ms**
one-shot handoff and ~950 ms of rendering. The trampoline is honest about the
56 ms and structurally blind to the rest.

Then the fader itself: about **a second per move** on the mergeSort pair,
against 1.26 ms now. Both causes were mine, and neither was the geometry.
`material.needsUpdate = true` asks three.js to recompile a shader, and opacity
is a uniform that needs no such thing — with some nine hundred materials that
was nine hundred recompiles a move. `setFromPoints` allocates an attribute per
call, once per edge per move.

The 61-object tree pair hid both completely. It took a 493-object scene to make
them visible, which is the same lesson as the rest of this log.

---

## What the pattern is

Nine of the eleven were found by using the thing, not by reading it. Three
concern the boundary between Fumola and a host, and in each the code was correct
about a shape the value did not have. Three concern the difference between an
object and an edge. One concerns what a colour means, which no test can hold.

The two verification habits that paid for themselves: comparing the page's
answer against the CLI's for the same program, and checking a claim about
appearance at the level of the material or the data rather than from a
screenshot — the browser pane's screenshots lag, and twice sent me chasing
defects that were not there.
