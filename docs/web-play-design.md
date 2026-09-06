# Building the Fumola web playground

*A design account of [fumola.org/web-play](https://fumola.org/web-play/), written after the
session that built it.*

---

## Where it started

The Fumola homepage had a "try it" box: a text area, a Run button, one line of output. It
worked, and it made the language look worse than it is. Two things were missing, and Matthew
named both at once:

> The lack of syntax highlighting in our existing "try it" box is the main limitation today.
> Second, that it doesn't re-evaluate as we type.

The obvious answer to the first is Monaco, and we did not take it. Fumola already has a lexer,
and a VSCode theme that assigns a colour to every token kind it produces. Loading a second,
larger lexer that would have to be taught the same language again is work in the wrong
direction. So `fumola_tokens` was exposed across the wasm boundary and the page colours its
text with the language's own tokenizer — which means the highlighting cannot drift from the
language, because it *is* the language.

That decision set the pattern for everything after it. Wherever the page needed to understand
Fumola, it asked Fumola.

---

## The library became the point

The first version was an editor with a list of modules beside it. It became something else
when the question changed from *what can I type?* to *what is already here?*

The library is now a tree, and beneath it a second tree of every `#[test]` in the library,
mirroring the module structure so where a test comes from is visible rather than something to
look up. Clicking one runs it. Clicking one also opens its module scrolled to that test's
`#[test]` marker, so it can be read as well as run.

Then the reverse: `#[test]` in the source is itself a link, and beside it a `body` button that
puts the test's own code in the editor. Matthew's rationale for that pairing:

> the user can start to replace the invocation with the actual test code, and customize it.

The `body` button is where the page stopped being a text box. A body lifted out of its module
is broken on arrival — inside the module a test calls its neighbours by their bare names, and
outside there are no such names. So the paste is repaired on the way out: the module's
`public` bindings get it back in front of them, and the module's imports are re-addressed from
the top level.

That repair is a rewrite over the token stream, not a table of special cases, which is why it
applies to tests nobody has written yet. It is also where a subtle bug lived. Given

```fumola
let sceneObjects = sceneObjects1 # sceneObjects2;   // near the end of the body
```

the scan that decides which names are the body's own was position-blind, so a binding on the
last line reached backwards and made an earlier *call* to the same name look local. Fixed by
recording where each name is bound and only shadowing uses after that point. All seventeen
bodies now run green from the paste.

---

## What the page found out about the language

This is the part worth recording, because it was not the plan. Building a good viewer kept
forcing questions the CLI had never asked, and two of them were bugs in Fumola itself.

**The prelude was shadowing the library.** Four library tests failed in the browser. Matthew
called the cause from the error alone — the prelude's one-argument `get` was being reached
where a module's own two-argument `get` was meant. The mechanism turned out to be two lines:
every module-level `func` captured the host's top-level environment as its closure at
first-import, and `var_step` consults that environment *before* the lexical definitions,
unconditionally.

So a prelude, which is supposed to be the layer everything else shadows, was the layer that
shadowed everything else. His reaction is the clearest statement of the principle:

> The whole notion of being a "prelude" is that it's BEFORE EVERYTHING, ALWAYS.

It was before everything. That was the bug. The cleanest proof is a symlink:
`fumola/system/hashMap.fumola` points at `../collections/hashMap.fumola`, so one file is
registered under two paths that fall on opposite sides of the prelude binding — identical
bytes, opposite outcomes. Module bodies are now elaborated with no ambient environment, which
is the discipline the test runner had silently been assuming all along. That is also why
`fumola test` reported 28 passing on a function that crashed when a program called it: it
calls with an empty environment and cannot see this class of bug at all.

**Symbol hashing was not portable.** The other three failures produced *smaller* graphs in the
browser than in the CLI, from byte-identical input. `prim "symbolLevel"` hashed a
`#[derive(Hash)] enum`, and a derived Hash writes the discriminant as an `isize` — eight bytes
on x86_64, four on wasm32. Different levels, a differently-shaped tree, different counts
everywhere downstream.

Ordinarily that would be invisible; a hash that only picks buckets may differ freely between
targets. Here it becomes a *level*, and a level decides a level tree's shape, so it is
semantics. The level hash is now taken over the symbol's source rendering, and the two targets
agree exactly.

---

## Measure the thing, don't reason about it

Several times a confident explanation was wrong and one measurement settled it. Enough times
that it is the main methodological lesson.

**The caret drifted from its text.** The editor is a transparent textarea over a highlighted
`<pre>`, and they had come apart. The first guess — font mismatch — was real but worth only
3px over a 77-character line. The actual cause was soft wrapping: the highlighted layer is one
`<span>` per token, and a break *inside a word* is decided per inline box, so a long line
wrapped in a different place in each layer. Measured: the glyph at character 60 sat one line
low and 567px left of where the textarea put it, and every line below inherited that error.
Neither layer wraps now.

**Then the same bug in the other pane.** The library's line numbers stopped at 438 while the
code ran on. The browser's own stylesheet sets `code { font-family: monospace }`, which beats
what `pre` hands down — so the source rendered in a different typeface from the gutter beside
it. Same size, same computed line-height, real line boxes of 27.1px against 25.59px, which
over 438 lines is exactly the 26 lines that went missing. The source pane had been rendering
in the wrong font all along; nothing looked wrong until something had to line up with it.

**Import order was not the lever.** When the prelude was clobbering the library, the natural
theory was load order. It was tested — prelude first, prelude last, prelude reverse-sorted —
and produced byte-identical failures, because `--import` only registers files into a map and
bodies run lazily on first import. The decisive order was the *binding*, which is not
reachable from the command line at all. The conclusion "order is not causal" was itself too
broad, from turning the wrong knob.

**Structure is not always the cheap representation.** Handing a scene to JavaScript as
structured values seemed obviously right. Measured: 6.4 MB and 4786 ms structurally, against
1.58 MB and 2087 ms as the JSON text the program builds itself.

---

## What a program says about itself

The last stretch was the most interesting, and it came from Matthew's observation that the
existing DCG viewer — the *replayground* — can only show a scene captured in advance. The name
carries the limitation: *replay* plus *playground*, where the "re" means pre-captured.

The insight that made the live version tractable is that a scene divides unevenly:

> The role of an `#[example]` is to provide the outline that's relevant, and the objects that
> are relevant. The rest of the history is mechanical to get. But one idea of Fumola is that a
> program, not some automation, is deciding how to portray itself spatially.

Events, nodes and edges are what the runtime recorded, and it can hand them over unasked.
Which force tree is worth looking at, and where its parts belong in space, are claims about
what the computation *means* — and nothing but the program knows them.

Then a measurement complicated it. Portrayal is itself computation, and the graph records it:

| | events | nodes |
| --- | --- | --- |
| history snapshotted before layout | 504 | 184 |
| history peeked after layout | 2043 | 657 |

Four times the subject's own history is the act of drawing it. A picture mostly of itself.

The fix chosen was the strong one: a new primitive, `A.scratch(thunk { ... })`, that runs a
computation on a branch of the adapton state and keeps only what it returned. It is `peek`'s
idea one level further in — a peek reads a cell without recording that it read it; a scratch
runs a whole computation without the graph keeping any of it. Both are how a layer outside a
computation looks at it without becoming part of what it sees.

With layout inside a scratch:

| | objects | events | nodes |
| --- | --- | --- | --- |
| layout in a scratch | 37 | **44** | **21** |
| layout in the session | 37 | 702 | 212 |

The same picture, from a history sixteen times smaller.

Two details in that primitive are worth the space. The counters that hand out node ids and
edge ids are carried **forward** across the restore rather than rolled back — otherwise the
session re-issues the branch's ids to different nodes, and a value that escaped the branch
reads back somebody else's content with no error at all. Measured with rollback, two such ids
were byte-identical. And nothing unwinds the VM stack on an interruption, so a branch that
failed half way through kept its writes: a scratch that wrote `` `kept := 999 `` and then
failed left the session reading `?999` where it had held 41. Both are now handled, and both
were found by looking rather than by reasoning.

---

## Feature tour

The live page is at **[fumola.org/web-play](https://fumola.org/web-play/)**. Every link below
opens it on that example — which is itself one of the features.

**The library, left.** A tree of the module library, then a horizontal rule and a second tree
of every `#[test]` in it, on a tinted ground. Line numbers in a gutter that sticks while long
lines scroll under it. Font size controls (`A−` `A0` `A+`) that affect only this side, and
snaps for `full` / `half` / `hide`. Drag either divider to size it by hand.

**Three panels, one at a time.** *library*, *events* and *print* — and the last two light up
exactly when they have something in them, so a program that printed says so from behind
another view. `1 + 2` lights neither; `print("hi")` lights print; a `force` lights events.

**The editor, right.** Highlighted by Fumola's own lexer, re-evaluated as you type, with no
button to press. An error names its line and bands it red — and for a failure inside a library
module, it points at the nearest line in *your* program that led there.

- [`hashMap.Test.testAll`](https://fumola.org/web-play/#test=collections/hashMap:Test.testAll)
  — a test invoked from outside its module
- [`List.testFromIter`, as its body](https://fumola.org/web-play/#body=collections/List:testFromIter)
  — the test's own code, imports re-addressed and module names qualified, ready to edit
- [`levelTree.Scene.testGeom2d`](https://fumola.org/web-play/#body=collections/levelTree:Scene.testGeom2d)
  — change `generateRandomInput(10, 10)` to `(10, 6)` and watch it fail at line 12

**`// assert`.** When an assertion fails on your own text, a button offers to comment every
assertion out, so they can come back one at a time. It comments whole statements, not lines,
because `testGeom2d`'s assertion spans five of them. On the example above, pressing it reveals
`{height = 4; width = 6; …}` — the value the new assertion wants.

**`share`.** Any preset names itself in the address bar, and a button copies the link. The
anchor is the module a test is defined in and its name inside it — no line number, which every
edit would invalidate; no index, which reordering would. It survives the module being edited
around the test, and breaks only when the test moves to another module. It stands only while
it is true: the first keystroke that changes the text takes it away, because past that point
the link would be a claim about something that is not there.

**State.** "Clean store each run" by default, because it is the simpler thing to understand;
the toggle keeps the graph and the bindings between runs when you want incrementality
instead. Graphical semantics throughout — presenting Fumola on its own, the conservative
default only got in the way.

*(Screenshots are not embedded here: no headless browser was available in the environment this
was written in. The links above open the live page, which is better than a picture of it.)*

---

## Where it goes next

The replayground has a 3D viewport, a time cursor with playback, and a cross-linked outline
with clickable chips; the playground has none of those, and has a live runtime instead. The
staged path is: a live outline panel first (the library already renders replayground's exact
glyph grammar as text, today), then a scene object and a metaTime cursor, then the outline
panel cross-linked, then the viewport — at which point the replayground can be retired.

The one thing neither can do yet is build a scene *incrementally*: `generateSceneFullDemand`
opens with `A.reset()`, so two runs cost the same. That is the interesting problem, and it is
now the one in the way.
