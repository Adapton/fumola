# `fumola`

Fumola is an experimental programming language.  We take inspiration from [The Extended Adapton Recipe](https://github.com/matthewhammer/adapton-recipe), including **Demanded Computation Graphs** extended with **symbolic space and time**.

Fumola integrates these ideas with an otherwise conventional language, with conventional data types.  For those features, we take inspiration from Motoko, a language design we adopt for most "core language features" that operate orthogonality to incremental computing primitives (e.g., primitive values, records, variants, functions, and modules).

## Example: Demand-driven `mergeSort` on a `List` of 44 input elements

<img width="1357" height="1105" alt="lazyMergeSort on 44 elements, as a Demanded Computation Graph (DCG)." src="https://github.com/user-attachments/assets/c34cd768-0b55-4110-8cb8-33ef4c455a75" />

***Interactive view***: For interactive control over the image above, see the [interactive version here](http://matthewhammer.org/replayground/mergeSort44.html).  This ***replayground*** demo features interactive accommodations for exploring the data stored in the picture, not included in the _"high level"_ image above.

***Description of the image***:
The image above depicts the dynamic behavior of `lazyMergeSort` on 44 unsorted input elements, as a ***Demanded Computation Graph (DCG)***, from a _"high level"_ vantage point, literally.

- Input list of 44 elements are the green boxes in far left column.
- Balanced "level tree" of 44 leaves is central, also green boxes, and constructed to the right of this input list.
- The thunks that "do" things in the DCG are blue orbs, with different kinds of actions (`put`, `force`, `get`) drawn as graph edges coming out of each thunk orb doing that action.
- To the right of this balanced tree, the sub-graph of `merge` thunks constructs the final sorted list, on demand.  It extends from left to right, horizontally.
- The tree root's stream of `merge` nodes (longest, central row of horizontal orbs) has the 44 elements in sorted order because we've demanded the full output.

## On-going work

- ***Fumola semantics*** performs ***realignment*** on DCGs via ***signaling*** and ***repair*** algorithms (Adapton Recipe semantics).
- More example algorithms and data structures.

## Future work

- ***Replayground*** permits live editing of the program being displayed, like in [Hazel](hazel.org).
- Use Fumola to author the UI aspects of the ***Replayground*** experience, now created by a static file exported from Fumola demo programs into static HTML/JS.

## Contributing

Contributions are welcome! Please check out the [contributor guidelines](https://github.com/dfinity/motoko.rs/blob/main/.github/CONTRIBUTING.md) for more information.
