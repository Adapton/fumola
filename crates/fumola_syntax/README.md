# `fumola_syntax`

The syntax of Fumola: the abstract syntax tree, the values, and the
shared types the parser and the evaluator both speak.

Fumola is an experimental programming language whose execution model is
**Adapton**: a program's run leaves behind a graph of what it named, read and
demanded, and that graph is what the next run repairs instead of redoing.

- **[fumola.org](https://fumola.org)** — the primitives, the two semantics, the
  papers, and a Fumola console that runs in the browser.
- **[adapton.org](https://adapton.org)** — Adapton itself, the recipe this
  implements.
- **[github.com/Adapton/fumola](https://github.com/Adapton/fumola)** — the
  source, the Fumola library, and the issues.

This crate is one piece of that workspace; see the repository README for how to
build, run and test the whole of it.

## License

Apache-2.0.
