# `fumola`

## FUnctional MOtoko meta-LAnguage.

Motoko extensions and tools, in Rust.

- Lexing.
- Parsing.
- Interpreter (partially implemented):
  - Interactive, break-point-style debugging.
  - Hot reload of code changes while preserving data.

## Extensions (WIP)

- Quoting and unquoting, with quoted ASTs as a value.
- Incremental caching and memoization (a la Adapton).
- local file system access (optional).
- local web system access (optional).
- REPL that interacts with IC, or a local replica.

## Out of scope

 - No static type system. 
 - [See Motoko compiler project for type system, and definitive semantics](https://github.com/dfinity/motoko).

## Related

[Online interpreter](https://mo-vm.netlify.app/)

## Contributing

Contributions are welcome! Please check out the [contributor guidelines](https://github.com/dfinity/motoko.rs/blob/main/.github/CONTRIBUTING.md) for more information.
