#!/usr/bin/env bash
# Refuse a panic macro in code an evaluating program can reach.
#
# Every `panic!`, `unreachable!`, `todo!` and `unimplemented!` below the
# evaluator was turned into an `Interruption`, so that a host embedding the VM
# has one error path to handle rather than an error path and a process that
# stops. docs/panic-audit.md is the record of which was which and why.
#
# That is an invariant, and nothing was keeping it: a new `todo!()` would read
# as an ordinary note to self and land unremarked. This says no.
#
# If you are here because the check failed: the arm you are writing wants
# `nyi!(line!(), "...")` when the program asked for something the VM does not
# implement, or `impossible!(line!(), "...")` when the VM believed something
# about its own state and was wrong. Both take a message meant for a person.
# Where the signature has no room for an `Interruption` -- a `Hash` impl, a
# `PartialOrd` -- make the function total instead; the audit has four worked
# examples.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE/.."

MACROS='panic!|unreachable!|todo!|unimplemented!'

# Whose job is to fail loudly: `check.rs` holds the assertion helpers the test
# suite is written in, `prelude.rs`'s are inside `#[cfg(test)]`, and
# `module_path.rs` is assertions all the way down.
EXCLUDED='crates/fumola/src/lib/check.rs|crates/fumola/src/lib/prelude.rs|crates/fumola_semantics/src/lib/module_path.rs'

ROOTS="crates/fumola/src crates/fumola_semantics/src crates/fumola_syntax/src crates/fumola_wasm/src"

# Comments are stripped before matching, so that a macro *named* in prose does
# not read as a macro *called* in code. The audit's own doc comments say
# `unimplemented!` and `todo!()` out loud, and a commented-out arm is not an
# arm. `///` and `//!` both start with `//`, so one rule covers all of them.
offenders() {
  grep -rnE "$MACROS" $ROOTS \
    | grep -vE "^($EXCLUDED):" \
    | sed 's://.*::' \
    | grep -E "$MACROS" \
    || true
}

found="$(offenders)"

if [ -n "$found" ]; then
  echo "A panic macro is reachable from evaluation:"
  echo
  echo "$found" | sed 's/^/  /'
  echo
  echo "Use nyi!() or impossible!(), or make the function total."
  echo "See docs/panic-audit.md."
  exit 1
fi

# The check has to be able to fail, or it is decoration. These run the same
# matcher over scratch text: one line of each macro, one of each way a mention
# is not a call.
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
passed=0
failed=0

check() { # check <description> <expect: catch|ignore> <line of rust>
  printf '%s\n' "$3" > "$work/probe.rs"
  local hit
  hit="$(printf '%s' "$3" | sed 's://.*::' | grep -cE "$MACROS" || true)"
  local got="ignore"
  [ "$hit" -gt 0 ] && got="catch"
  if [ "$got" = "$2" ]; then
    passed=$((passed + 1)); printf '  ok   %s\n' "$1"
  else
    failed=$((failed + 1)); printf '  FAIL %s (expected to %s, did %s)\n' "$1" "$2" "$got"
  fi
}

echo "no panic macro reachable from evaluation; the matcher still works:"
check "catches panic!"                catch  '    _ => panic!(),'
check "catches unreachable!"          catch  '    _ => unreachable!("cannot happen"),'
check "catches todo!"                 catch  '    Exp::Hole => todo!(),'
check "catches unimplemented!"        catch  '    Blob(_) => unimplemented!(),'
check "catches one after real code"   catch  '    let x = f(); todo!()'
check "ignores a line comment"        ignore '    // _ => unimplemented!(),'
check "ignores a trailing comment"    ignore '    Cont::Decs(d) => step(d), //_ => unimplemented!(),'
check "ignores a doc comment"         ignore '/// Returned as an error rather than raised with `unimplemented!`.'
check "ignores prose in a module doc" ignore '//! A `todo!()` here would be a regression.'
check "ignores nyi!"                  ignore '    Exp::Hole => nyi!(line!(), "a hole"),'
check "ignores impossible!"           ignore '    None => impossible!(line!(), "empty"),'

echo
echo "$passed passed, $failed failed"
[ "$failed" -eq 0 ]
