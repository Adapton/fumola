# Fumola, three ways: source, tiles, and Hazel

This is the Fumola side of a boundary whose other side is in the Hazel repo.
Hazel's "Fumola (Tiles) / The syntax" slide is the live version, and the
better place to *read* the forms, because both of its columns evaluate. This
file is the lookup table, written after losing an hour to a divergence that
slide documents in a sentence between two examples.

It lives here rather than there because the left-hand column -- what you write
in a `.fumola` file -- is this repo's, and because someone writing a library
module is the person most likely to be caught out by the other two.

Three different things get called "the difference between Fumola and Hazel",
and conflating them is the trap:

1. **Tile spelling.** The same Fumola form, spelled differently in tiles than
   in a `.fumola` file, because Hazel's tokenizer cannot accept the original.
2. **Crossing.** What a Fumola *value* becomes when it arrives in Hazel.
3. **Gaps.** Fumola forms with no tile at all.

A `.fumola` file in the library is category 1's left column. A program typed
into a Hazel slide is its right column. They are the same language and they do
not look the same.

## 1. Tile spelling — same form, different text

| in a `.fumola` file | in Hazel tiles | why it differs |
|---|---|---|
| `#tag`, `#tag(e)` | `$tag`, `$tag(e)` | `#` is Hazel's comment delimiter and cannot begin a token |
| `xs # ys` (append) | `xs ++ ys` | the same hash |
| `case (p) b` | `case p => b` | Fumola writes no arrow; a prefix form needs a token between the pattern and the body, so the tile borrows Hazel's |
| `if c { t } else { e }` | `if c then t else e` | Fumola has no `then`; the tile borrows it and the printer emits Fumola's own form |
| `f()` | `f()`, but **one token** | an empty bracket pair would be an application whose argument slot is a hole |

Everything else is spelled the same: blocks `{ d; d; e }` with no `in`, `let
p = e`, `import M = "path"`, application `f(e)`, projection `e.0` and `e.x`,
indexing `xs[i]`, arrays `[a, b]`, tuples, records `{x = 1; y = 2}`, the
operators, `assert` / `ignore` / `return`, `switch`, `thunk`, `force`, `prim`,
`e!`, `:=`, `@`.

**The operator ladder is Fumola's, not Hazel's.** `1 | 2 + 3` does not
associate the way the same characters would in Hazel. If precedence matters,
parenthesise rather than reason from Hazel habits.

## 2. Crossing — what a value becomes

| Fumola value | arrives in Hazel as |
|---|---|
| `#tag` | `Tag` — **only the first letter is recased** |
| `[a, b]` | `[a, b]`, a list; element type from the annotation |
| `(a, b)` | a tuple, parts keeping their own types |
| `{x = 1; y = true}` | a labeled tuple `(x=Int, y=Bool)`, **fields in name order** |
| `` `name` `` | `Symbol` where one is expected, otherwise its text |
| `null` / `?(x)` | `None` / `Some(x)` |
| `()` | `()` |
| `Nat` and `Int` | both `Int` — Hazel has one integer type where Fumola has two |
| `Float` | `Float`, printed as text and read back |
| a thunk | opaque, carrying the source Fumola prints for it; `force` turns it into something Hazel has a value for |
| **a function** | **cannot cross** — no written Fumola form |
| **a hole** | **cannot cross**, and it is refused when the program runs rather than while it is typed |

Because only the first letter recases, **a tag that already begins upper case
does not survive being sent and read back.**

Values cross during *evaluation*, not elaboration, which is why a `hazel …
end` escape can carry what a Hazel variable is bound to: by then the variable
has been reduced to its value.

## 3. Gaps — Fumola forms with no tile

This is the list worth reading before planning work in a slide.

| form | state |
|---|---|
| **`func`** | **no tile.** A program typed in Hazel cannot define a function of its own — including a recursive one |
| `var` | no tile |
| `?e`, building an option | no tile; `e!`, unwrapping one, has one |
| a float literal | in the AST, no spelling yet |
| a char literal | in the AST, no spelling yet |
| a tuple *pattern* | no tile; name, `_`, literal, tag, nested tag and parens all have one |

### What the `func` gap means in practice

A view, a layout, a fold — anything recursive — cannot be written in a Hazel
slide's Fumola. The two ways round it:

- **Put the function in the library** and import it. `import`, string literals
  and `.` projection all have tiles now, so
  `import T = "fumola/examples/treeCare"; T.root(hazel m.tree end)` is
  sayable today. Adding a `.fumola` file needs no Rust change — the wasm
  build script collects them — at roughly 110 s a rebuild.
- **Add the tile**, at roughly 22 s a Hazel rebuild (`Form.re`), but across `Form.re`, the
  Fumola AST, term construction and the printer, plus deciding how a
  tile-authored `func` scopes for recursion.

The first is what `fumola/examples/treeCare.fumola` does.

An older note in Hazel's `docs/fumola-tiles-design.md` says the library is out
of reach because "`import`, string literals, and `.` projection are not tile
forms". That was true when written and is not now; `FumolaImport` and
`FumolaProj` are both in Hazel's form table.

## The Adapton core, which has no Hazel column at all

`:=` puts a value in a named cell and hands back a pointer, `@` reads one,
`thunk` suspends a computation, `force` runs it, `peek` answers an option,
`prim` reaches a runtime primitive by name. None is Hazel syntax and none is a
library function — they are the language, and they are why the tiles exist.
