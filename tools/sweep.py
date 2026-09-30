#!/usr/bin/env python3
"""Write the Fumola program for one sweep.

    tools/sweep.py <size> <algorithm> <positions> [seed]

`algorithm` is a name `sweep.algorithmName` answers with -- lazy-inductive,
lazy-cartesian, lazy-pathwise, eager-split. `positions` is one of:

    root        the gap that roots the input tree: the worst case
    all         every position: the average case, exhaustively
    random:N    N positions drawn uniformly, with replacement

The program prints one CSV row per sample and nothing else, so a runner can
append its output to a file. `tools/sweep-run.sh` does that, one process at a
time -- see the module's own documentation for why the process boundary is
where the output gets flushed.
"""
import sys

TAGS = {
    "lazy-inductive": "#lazyInductive",
    "lazy-cartesian": "#lazyCartesian",
    "lazy-pathwise": "#lazyPathwise",
    "eager-split": "#eagerSplit",
}

def positions_expr(spec, size):
    if spec == "root":
        return f"[S.rootPosition({size})]"
    if spec == "all":
        return f"S.allPositions({size})"
    if spec.startswith("random:"):
        count = int(spec.split(":", 1)[1])
        # A seed of its own, so the positions do not follow the input's own randomness.
        return f"S.randomPositions(7, {size}, {count})"
    raise SystemExit(f"unknown positions spec: {spec}")

def main():
    if len(sys.argv) not in (4, 5):
        raise SystemExit(__doc__)
    size, algorithm, spec = sys.argv[1], sys.argv[2], sys.argv[3]
    seed = sys.argv[4] if len(sys.argv) == 5 else "10"
    if algorithm not in TAGS:
        raise SystemExit(f"unknown algorithm: {algorithm}; one of {', '.join(TAGS)}")
    print(f'''import S "fumola/examples/mergeSort/sweep";
do {{
  S.sweep({seed}, {size}, {positions_expr(spec, size)}, {TAGS[algorithm]});
  ""
}}''')

if __name__ == "__main__":
    main()
