#!/usr/bin/env bash
#
# Run the mergeSort sweeps, one process at a time, appending CSV.
#
#     tools/sweep-run.sh <out.csv> <positions> <size>... -- <algorithm>...
#
# for example
#
#     tools/sweep-run.sh /tmp/worst.csv root 64 128 256 512 1024 -- \
#         lazy-inductive lazy-cartesian lazy-pathwise eager-split
#     tools/sweep-run.sh /tmp/avg.csv all 64 128 -- lazy-inductive eager-split
#
# One process per (size, algorithm), for two reasons. `Prim.print` puts its
# line in the agent's own output and the host drains it when evaluation
# returns -- a process that is killed prints nothing -- so the process
# boundary is where rows reach the file. And a sweep that dies for memory
# loses one cell of the table rather than the table.
#
# Release, or the numbers are about the debug build. One at a time, because
# running several is what exhausted memory the first time.
set -euo pipefail

BIN=${FUMOLA:-./target/release/fumola}
[ -x "$BIN" ] || { echo "no $BIN -- cargo build --release" >&2; exit 1; }

OUT="$1"; POSITIONS="$2"; shift 2
SIZES=()
while [ "$#" -gt 0 ] && [ "$1" != "--" ]; do SIZES+=("$1"); shift; done
[ "${1:-}" = "--" ] || { echo "expected -- between sizes and algorithms" >&2; exit 1; }
shift
ALGORITHMS=("$@")
[ "${#SIZES[@]}" -gt 0 ] && [ "${#ALGORITHMS[@]}" -gt 0 ] \
  || { echo "need at least one size and one algorithm" >&2; exit 1; }

IMPORTS=$(find -L fumola -name "*.fumola" | sort)
PROGRAM=$(mktemp); ERRORS=$(mktemp); trap 'rm -f "$PROGRAM" "$ERRORS"' EXIT

python3 tools/sweep.py 16 lazy-inductive root > /dev/null  # fail early on a bad tool
printf 'size,algorithm,position,own,equal,notEqual,newBody,sameBody,nonThunk,onlyLeft,onlyRight,redone,garbage\n' > "$OUT"

for size in "${SIZES[@]}"; do
  for algorithm in "${ALGORITHMS[@]}"; do
    python3 tools/sweep.py "$size" "$algorithm" "$POSITIONS" > "$PROGRAM"
    started=$(date +%s)
    # Only the rows reach the file: the program's value is the empty text, and
    # the runtime logs each printed line in its own debug form as well.
    # Stderr is kept rather than dropped, so that a failure can say why -- a
    # sweep of mine reported three FAILEDs and no reason, because I had moved
    # the binary out from under a run that was already going.
    if "$BIN" eval "$(cat "$PROGRAM")" -i $IMPORTS 2>"$ERRORS" \
         | sed 's/\x1b\[[0-9;]*m//g' | grep -E '^[0-9]+,' >> "$OUT"; then
      echo "  $size $algorithm $POSITIONS -- $(( $(date +%s) - started ))s" >&2
    else
      echo "  $size $algorithm $POSITIONS -- FAILED after $(( $(date +%s) - started ))s" >&2
      grep -vE "INFO|^$" "$ERRORS" | tail -3 | sed 's/^/      /' >&2 || true
    fi
  done
done
echo "wrote $OUT ($(( $(wc -l < "$OUT") - 1 )) samples)" >&2
