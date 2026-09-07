# A tutorial mode for web-play: design

The pair view now has an argument to make — a list becomes a tree stably, a
tree becomes a sorted list unstably, and the difference is a naming — and
making it takes three examples in a particular order, the right camera on each,
and some prose. `docs/demo-notes.md` is that, written for a person to follow.
This is a design for the page following it instead.

Not built. This is a proposal to argue with.

---

## What it is not: a tour

The obvious shape is a slideshow: an ordered list of stops, next and back.

That is the wrong shape, and it is worth saying why before proposing the other
one. An example is not a slide. `examplePairTreeAfterRemoval` is the second
thing to look at when the subject is mergeSort's stability, and the *first*
thing when the subject is what a level tree is. The same example sits in
different places in different arguments, and a linear tour has to pick one.

It also puts the ordering somewhere that cannot maintain it. A list of stops
lives in the page or in a config file; the examples live in the library. Adding
an example then means editing something else, and the two drift.

## What it is: a graph, declared by each example

Each example says what it is worth reading before and after — the two ends of
one relation — and the paths through the library are whatever those links make.
A reader arriving at any example can go forwards or back, and the same example
can be the beginning of one path and the middle of another.

This also puts the knowledge where it can be kept. Which example to read before
`examplePairMergeSortRootCell` is a fact about that example, and the person who
knows it is the person writing it. Nothing else has to be edited, and a
deleted example takes its own links with it.

It follows the property `#[example]` already has: nothing is extracted from a
program against its will. The page draws what a program hands it, and it should
learn what to read next the same way.

---

## The shape

Sketched in Fumola, in `webPlay.fumola` beside `SceneComponents`:

```fumola
/// What an example says about how to read it.
public type Tutorial = {
    /// The example this is about: the same path a deep link uses, e.g.
    /// "collections/levelTree:Scene.examplePairTreeAfterRemoval".
    subject : Text;

    /// Worth reading before, and after. Same paths. A suggestion and not a
    /// sequence: a reader may arrive from anywhere.
    before : [Text];
    after : [Text];

    /// The explanation, in parts. Each part may ask for its own view.
    prose : [Passage];
};

public type Passage = {
    text : Text;
    /// How to be looking while this is read. Null keeps whatever the last
    /// passage asked for, so a passage that adds a sentence about the same
    /// picture does not have to restate the view.
    see : ?Look;
};

public type Look = {
    /// Where to stand, and whether to walk. Null leaves the camera alone.
    from : ?{ #above; #beside; #below };
    /// Degrees a second. 0 stands still at that height.
    walk : ?Nat;
    /// Distance, as a multiple of the distance the fit chose.
    out : ?Float;
    /// An object to centre on, by the label it carries -- the same `Text`
    /// that `DiffKeys` names and that `sceneObjects` writes.
    at : ?Text;
    /// For a pair: where to put the fader, or to sweep it.
    fader : ?{ #left; #right; #middle; #sweep : Nat };
};
```

Declared by a function carrying a new attribute, discovered the way
`#[example]` is — by the page reading module source:

```fumola
#[tutorial]
public func tourMergeSortRootCell() : W.Tutorial {
  { subject = "examples/mergeSort/mergeSort:examplePairMergeSortRootCell";
    before = [ "collections/levelTree:Scene.examplePairMergeFromTree" ];
    after  = [ "examples/mergeSort/mergeSort:examplePairMergeSortTypicalCell" ];
    prose  = [ ... ] }
};
```

`subject` is a field rather than an attribute argument (`#[tutorial(name)]`),
because attribute arguments would need parser support and a field needs none.
It costs a line of duplication and buys nothing to maintain.

## Why prose is in parts

Because a passage is about a view, and one example needs more than one.

The mergeSort pair is the case. The first thing to say is what the input tree
is and where it came from, which wants the camera low and still, and links back
to the tree example for anyone who has not seen it. The second is what the
merge network does, which wants the walk on, because the network is a
three-dimensional thing that a still picture flattens. The third is why
removing one cell rebuilds half of it, which wants the fader sweeping and
nothing else moving.

One block of prose and one view cannot do that. Three passages, each with its
own `Look`, can — and a passage with no `Look` simply carries on with the last
one, so adding a sentence costs nothing.

## Passages link

A passage's text should be able to name another example, and the page should
make it a link — the same deep link the library uses. That is what lets the
mergeSort chapter say "the input tree is built as in *this* example" and have
it mean something to a reader who arrives there first.

Worth keeping this to naming examples rather than inventing a markup. A
passage is a paragraph of prose with example paths in it; anything more is a
documentation system, which is not what this is.

---

## What the page does

1. Reads `#[tutorial]` declarations the way it reads `#[example]`.
2. Shows, beside an example that has one, its prose and its before/after links.
3. Applies each passage's `Look` as the reader moves through it.
4. Lets the reader step passage by passage, and follow a link to another
   example, arriving at that example's own first passage.

All the machinery a `Look` needs already exists and is driven by the same
values: `above`/`beside`/`below` with `spin` and `out`, the fader with
`ping-pong` and `pace`, and the camera readout, which already prints exactly
the numbers a `Look` would want to record.

`at` is the one new thing. Centring on a named object means finding it by label
and panning the walk's centre to it, which the drag-to-pan work already does
mechanically — what is missing is the lookup from label to position, and the
scene has that.

## Open questions

- **Is `subject` the right direction?** An example could carry its own tutorial
  instead of a tutorial naming its example. That reads better but makes
  `ExampleOutput` bigger for every example, most of which will never have prose.
- **Should `before`/`after` be checked?** A path that names nothing is a broken
  link, and only CI or the CLI can see it, since the page shows what it finds
  and cannot know what is missing.
- **How much view state is worth recording?** `Look` as sketched cannot express
  an arbitrary camera. It could take the full position, but then a tutorial
  written against one scene breaks when the scene's layout changes, and the
  layout is the program's to change.
- **Does a passage advance on its own?** A timer makes it a video; a click
  makes it a book. A book is probably right, with the walking and sweeping
  supplying the motion.
- **Where does a reader start?** Either an example with nothing before it, or
  an explicit entry point. The first is inferable and the second is honest.

## Smaller pieces worth having regardless

- Deep links to a passage, not only to an example, so a specific step can be
  sent to someone.
- The camera in the URL, so a screenshot's viewpoint can be recovered exactly.
  The position is already drawn on the picture.
- A lint that an `#[example]` takes `()` and succeeds on it — already wanted
  (#82), and a tutorial that names a broken example is worse than no tutorial.

## Related

- `docs/demo-notes.md` — the manual version, with the measured numbers
- `docs/diff-view-log.md` — how the pair view's encoding was arrived at
- #85 — the issue this document answers
- #82 — body-by-default and copy-link, both of which a tutorial would use
