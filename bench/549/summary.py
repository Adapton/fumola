"""MEASURING ONLY (^ticket(549)): one table from the alternating mergeSort runs of main and the branch.

Each run of do_big_mergesort prints, per size, the step count and the best of three times under `do small` and
`do big`. Here, per size: the step counts must agree (the same program, so the same transitions); the branch's
time over main's, from each side's best round; and the noise floor, main's spread over its own rounds, which a
difference has to clear before it means anything.
"""

import re
import sys
from pathlib import Path

ROW = re.compile(r"^(scene, size \d+)\s+(\d+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s*$")


def runs(out, side):
    found = {}
    for path in sorted(Path(out).glob(f"{side}_*.txt")):
        for line in path.read_text().splitlines():
            m = ROW.match(line)
            if m:
                name, steps, small, big = m.group(1), int(m.group(2)), float(m.group(3)), float(m.group(4))
                found.setdefault(name, []).append((steps, small, big))
    return found


def spread(xs):
    return max(xs) / min(xs) - 1


def main(out):
    m, b = runs(out, "main"), runs(out, "branch")
    print("### mergeSort, main (cb2bcf5) against deep-answers (08e7e25)\n")
    print(f"Rounds: main {max(len(v) for v in m.values())}, branch {max(len(v) for v in b.values())}. "
          "Times are each side's best round, in ms; the noise floor is main's spread over its own rounds.\n")
    print("| workload | steps (same?) | small: main | small: branch | branch / main | noise | "
          "big: main | big: branch | branch / main | noise |")
    print("|---|---|---|---|---|---|---|---|---|---|")
    ok = True
    for name in m:
        ms, bs = m[name], b.get(name, [])
        steps = {s for s, _, _ in ms} | {s for s, _, _ in bs}
        same = len(steps) == 1
        ok &= same
        cols = [name, f"{min(steps)} ({'yes' if same else 'NO: ' + ', '.join(map(str, sorted(steps)))})"]
        for i in (1, 2):
            mt, bt = [r[i] for r in ms], [r[i] for r in bs]
            cols += [f"{min(mt):.1f}", f"{min(bt):.1f}", f"{min(bt) / min(mt):.3f}", f"{spread(mt):.1%}"]
        print("| " + " | ".join(cols) + " |")
    print()
    print("Step counts agree on every size." if ok else "**Step counts DISAGREE: the timings mean nothing.**")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "out"))
