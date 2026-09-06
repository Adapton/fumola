//! `prim "adaptonScratch"`: run a thunk on a branch of the graph, keep what it
//! computed, discard what it did.
//!
//! The happy path is covered by `#[test]`s in `fumola/system/adapton.fumola`,
//! which is where it belongs. What cannot be written there is the failing
//! case: a Fumola test cannot catch an interruption, and the interruption is
//! the interesting part -- nothing unwinds the VM stack when one happens, so a
//! scratch frame's completion arm never runs and the restore has to be made to
//! happen some other way.

use fumola::check::assert_vm_eval as assert_;
use fumola::state::State;

/// Evaluate each program in turn against one runtime, as a REPL would, and
/// return what the last one produced. The failing ones are expected to fail;
/// what matters is the state they leave behind.
fn session(programs: &[&str]) -> String {
    let mut state = State::empty();
    let mut last = String::new();
    for p in programs {
        state.semantic_state.clear_cont();
        last = match state.eval(p) {
            Ok(v) => format!("{}", fumola_semantics::format::format_one_line(&*v)),
            Err(e) => format!("{:?}", e),
        };
    }
    last
}

#[test]
fn the_value_comes_back_and_the_writes_do_not() {
    assert_(
        r#"`kept := 41;
           let v = prim "adaptonScratch" (thunk { `kept := 999; `only := 1; 7 });
           (v, prim "adaptonPeek" (prim "adaptonPointer" `kept),
               prim "adaptonPeek" (prim "adaptonPointer" `only))"#,
        "(7, ?41, null)",
    )
}

/// The one the design turns on.
///
/// Before the unwind existed, this session read `?999`: the branch failed half
/// way through, its completion arm never ran, and its write stayed.
#[test]
fn a_failed_scratch_leaves_the_session_as_it_was() {
    let after = session(&[
        "`kept := 41",
        r#"prim "adaptonScratch" (thunk { `kept := 999; assert false; 7 })"#,
        r#"prim "adaptonPeek" (prim "adaptonPointer" `kept)"#,
    ]);
    assert_eq!(after, "?41", "a failed branch left its write behind");
}

/// An inner failure unwinds past every enclosing scratch, not just the nearest
/// one: the outermost save predates them all.
#[test]
fn a_failure_unwinds_past_nested_scratches() {
    let after = session(&[
        "`kept := 41",
        r#"prim "adaptonScratch" (thunk {
             `kept := 1;
             prim "adaptonScratch" (thunk { `kept := 2; assert false; 0 })
           })"#,
        r#"prim "adaptonPeek" (prim "adaptonPointer" `kept)"#,
    ]);
    assert_eq!(after, "?41");
}

#[test]
fn the_session_still_works_after_a_failed_scratch() {
    let after = session(&[
        "`kept := 41",
        r#"prim "adaptonScratch" (thunk { `kept := 999; assert false; 7 })"#,
        "`kept := 42",
        r#"prim "adaptonPeek" (prim "adaptonPointer" `kept)"#,
    ]);
    assert_eq!(after, "?42");
}

/// The names a branch minted must not be handed out again.
///
/// The counters that issue node ids and edge ids are kept moving forward
/// across a restore, on purpose. Rolled back instead, the session re-issues
/// the branch's ids to different nodes, and a value that escaped the branch
/// reads back somebody else's content with no error at all -- a wrong answer
/// rather than a failure. Measured with the rollback in place, these two ids
/// were identical.
#[test]
fn ids_minted_in_a_scratch_are_not_reissued() {
    assert_(
        r#"let now = func () {
             (switch (@(`adapton(`state))) { case (#Graphical(g)) g }).meta_time
           };
           let before = now();
           let _ = prim "adaptonScratch" (thunk { `a := 1; `b := 1; `c := 1; 0 });
           let after = now();
           after == before"#,
        "false",
    )
}
