# Diffing two graphs without leaving Rust

What `A.Native` is, what it cost to not have it, and how to measure that again.

## The bottleneck

Measuring realignment means diffing two runs' node maps: for each pointer both runs named, did
they agree? `A.Diff` has done this since #43 and the answers are right. The cost is not in the
comparing, it is in getting there. A node's value is a thunk body -- a closed expression with
its environment -- and `A.peekHistory()` converts every one of them into a Fumola value before
the Fumola code can put it in a `hashMap` and compare it. At n = 1000 that is about 19,000
nodes converted and boxed per run, twice per sample, before a single comparison happens.

The graph is already in Rust, in `im_rc` structures that clone by reference. `A.Native` reads
it where it lies: `Value::AdaptonHistory` and `Value::AdaptonNodeVals` are value forms Fumola
can hold and not look inside, and `A.Native.diff` answers with numbers.

## What it costs, measured

`mergeSort` at seed 10, removing the highest-level cell -- the same sample every table in
`mergesort-report-notes.md` uses -- in three variants: the two runs and no diff at all, the two
runs and the Fumola-side diff, the two runs and the native diff. Release build, one process at
a time, `/usr/bin/time` for wall clock and peak RSS. The n = 1000 row is the median of three;
the others are single measurements.

| n | | two runs, no diff | + `A.Diff` | + `A.Native` |
|---|---|---|---|---|
| 250 | wall | 1.03 s | 1.54 s | 1.01 s |
| | peak | 110 MB | 304 MB | 125 MB |
| 500 | wall | 6.62 s | 8.04 s | 6.63 s |
| | peak | 226 MB | 693 MB | 266 MB |
| 1000 | wall | 28.78 s | 31.85 s | 28.93 s |
| | peak | 478 MB | 1472 MB | 575 MB |

Subtracting the baseline gives what the diffing itself costs:

| at n = 1000 | wall | peak over baseline |
|---|---|---|
| `A.Diff` | 3.07 s | 994 MB |
| `A.Native` | 0.15 s | 97 MB |

**20x faster, and a tenth of the memory.** Two things worth saying plainly about that:

- On a **whole sample** the two runs dominate the clock -- 28.8 s of the 31.9 -- so a sample is
  only 1.10x faster. What it is, is **2.56x smaller in peak memory**, and memory is what killed
  the sweeps: the n = 1000 average-case runs died at 10.2 GB, and the diff was holding a
  gigabyte of converted nodes per sample on top of two live graphs.
- Below n = 500 the native diff's time is **not measurable** against the run: 1.01 s against a
  1.03 s baseline at n = 250 is noise, not a negative cost. The repetitions at n = 1000 vary by
  0.2 s and 10 MB, which is the noise floor for these figures.

## The agreement, which is the point

Faster wrong answers are worth nothing, so both implementations are checked against each other,
not just against expectations:

- `A.Native.testAgreesWithTheFumolaSideDiff` -- three nodes, an edit and a repair.
- `levelTree.Scene.testNativeDiffAgreesOnTheMergeNetwork` -- the size-16 merge network the
  comparative examples are made of, where the two runs disagree in all four categories at once,
  with an assertion that each category is non-empty so the agreement is not cheap.
- The benchmark programs print their four counts, and the `fumola` and `native` variants print
  the same ones at every size: `(3108, 563, 158, 19)` at 250, `(6946, 1439, 506, 231)` at 500,
  `(14500, 2872, 1009, 732)` at 1000.

## What stays on the Fumola side

`A.Diff.nodeValsDiff` answers with the four **maps**, and `webPlay.pair` colours a comparative
example from them: *which* cells differ is what the playground draws. `A.Native.diff` answers
with counts and builds nothing. Drawing wants the maps; measurement wants the numbers. Both
stay, and neither is the other's replacement.

`A.Native` also splits `notEqual` three ways, which the Fumola side cannot do without walking
the pairs itself: `notEqualNewBody` (a thunk named again for a different closure -- a
re-allocation, which repair must evaluate as it would a cell with no counterpart),
`notEqualSameBody` (same closure, other result -- repair proper), and `notEqualNonThunk`. At
n = 1000, 99% of `notEqual` is `notEqualNewBody`, which is the finding that corrected "cartesian
caps the tail" (see `mergesort-report-notes.md`).

## Measuring it again

    python3 tools/native-diff-bench.py <baseline|fumola|native> <size> > /tmp/p.fu
    /usr/bin/time -f "%e %M" ./target/release/fumola eval "$(cat /tmp/p.fu)" \
      -i $(find -L fumola -name "*.fumola" | sort)

Release, or the numbers mean nothing. One process at a time: the sweeps that exhausted memory
did it by running several at once.
