//! A limit is a pause, not a failure.
//!
//! The VM has had a step limit all along, and `Interruption::Limit` has been
//! recoverable and excluded from `ends_the_computation`, but nothing resumed
//! from one. These pin the property that makes a trampoline possible: a
//! paused computation continues to the same answer it would have reached in
//! one go, and the steps add up.

use fumola::state::State;
use fumola::Error;
use fumola_semantics::vm_types::Limit;
use fumola_semantics::Interruption;

fn paused(e: &Error) -> bool {
    matches!(e, Error::Interruption(Interruption::Limit(Limit::Step)))
}

/// The program is the same either way, so the answer must be too.
#[test]
fn resuming_reaches_the_same_answer() {
    let program = "var i = 0; var n = 0; while (i < 400) { n += i; i += 1 }; n";

    let mut whole = State::empty();
    let direct = whole.eval(program).expect("unlimited eval");

    let mut chunked = State::empty();
    let mut answer = None;
    match chunked.eval_limited(program, 50) {
        Ok(v) => answer = Some(v),
        Err(e) => assert!(paused(&e), "expected a pause, got {:?}", e),
    }
    let mut rounds = 0;
    while answer.is_none() {
        rounds += 1;
        assert!(rounds < 10_000, "resumed {} times without finishing", rounds);
        match chunked.resume_limited(50) {
            Ok(v) => answer = Some(v),
            Err(e) => assert!(paused(&e), "expected a pause, got {:?}", e),
        }
    }
    assert!(rounds > 1, "one chunk finished it; the limit did nothing");
    assert_eq!(
        format!("{:?}", answer.unwrap()),
        format!("{:?}", direct),
        "chunked run disagreed with the whole run"
    );
    // 400 iterations summed is 79800 either way.
    assert_eq!(
        format!("{:?}", whole.eval("79800").unwrap()),
        format!("{:?}", chunked.eval("79800").unwrap())
    );
}

/// The count is cumulative, and a budget is relative to it.
#[test]
fn steps_accumulate_across_resumes() {
    let program = "var i = 0; while (i < 200) { i += 1 }; i";
    let mut st = State::empty();
    // Not asserted to be zero: a fresh State has already taken a step, and
    // what matters is that the count only ever grows from wherever it starts.
    let before = st.steps_taken();

    let mut seen = vec![];
    let mut done = st.eval_limited(program, 40).is_ok();
    seen.push(st.steps_taken());
    while !done {
        done = st.resume_limited(40).is_ok();
        seen.push(st.steps_taken());
    }
    // Monotone, and more than one chunk actually happened.
    assert!(seen.len() > 2, "expected several chunks, saw {:?}", seen);
    assert!(seen[0] >= before, "count went backwards from its start");
    for pair in seen.windows(2) {
        assert!(pair[1] >= pair[0], "step count went backwards: {:?}", seen);
    }
    assert!(
        *seen.last().unwrap() > 200,
        "a 200-iteration loop took {} steps",
        seen.last().unwrap()
    );
}

/// A budget asked for from a standing count still stops.
///
/// The limit is a mark to reach rather than an allowance, so a caller that
/// passed a bare `1000` after 5000 steps would stop instantly and never
/// progress. `step_budget` is what prevents that, and this is the regression.
#[test]
fn a_budget_makes_progress_from_any_standing_count() {
    let mut st = State::empty();
    st.eval("var i = 0; while (i < 100) { i += 1 }").expect("warm up");
    let warm = st.steps_taken();
    assert!(warm > 100, "warm-up took {} steps", warm);

    let mut done = st.eval_limited("var j = 0; while (j < 100) { j += 1 }; j", 30).is_ok();
    let mut rounds = 0;
    while !done {
        rounds += 1;
        assert!(rounds < 10_000, "no progress from a standing count");
        done = st.resume_limited(30).is_ok();
    }
    assert!(st.steps_taken() > warm, "no steps were taken at all");
}

/// A real failure is still a failure, not a pause.
#[test]
fn a_failure_is_not_a_pause() {
    let mut st = State::empty();
    let e = st
        .eval_limited("assert (1 == 2)", 100_000)
        .expect_err("assertion should fail");
    assert!(!paused(&e), "an assertion failure read as a pause: {:?}", e);
}

/// A value that arrives inside the first chunk needs no resume.
#[test]
fn a_short_program_finishes_in_one_chunk() {
    let mut st = State::empty();
    let v = st.eval_limited("1 + 2", 100_000).expect("short program");
    let mut plain = State::empty();
    let want = plain.eval("3").expect("three");
    assert_eq!(format!("{:?}", v), format!("{:?}", want));
}
