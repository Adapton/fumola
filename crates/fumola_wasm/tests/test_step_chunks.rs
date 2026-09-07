//! Chunked evaluation across the JavaScript boundary.
//!
//! A host that wants to show progress has to be handed control back, which
//! means the answer arrives in pieces. These pin the two properties a host
//! depends on: the pieces reach the same answer as one call, and the step
//! count it is shown rises rather than restarting.

use fumola_wasm::*;

fn parse(s: &str) -> serde_json::Value {
    serde_json::from_str(&s).expect("reply should be json")
}

/// The same program, in chunks and in one go.
#[test]
fn a_trampoline_reaches_the_same_answer() {
    let program = "var i = 0; var n = 0; while (i < 300) { n += i; i += 1 }; n";

    let whole = parse(&fumola_eval_top(fumola_create(), program));
    assert_eq!(whole["ok"], true, "unlimited eval failed: {}", whole);

    let id = fumola_create();
    let mut reply = parse(&fumola_eval_top_limited(id, program, 60));
    let mut rounds = 0;
    let mut steps = vec![];
    while reply["paused"] == true {
        steps.push(reply["steps"].as_u64().expect("paused reply carries steps"));
        rounds += 1;
        assert!(rounds < 10_000, "never finished");
        reply = parse(&fumola_resume(id, 60));
    }
    assert!(rounds > 1, "one chunk finished it; the limit did nothing");
    assert_eq!(reply["ok"], true, "chunked run failed: {}", reply);
    assert_eq!(reply["value"], whole["value"], "chunked answer differs");
    assert_eq!(reply["tag"], whole["tag"]);

    // The number a host would show must rise, or it is not progress.
    assert!(steps.len() > 1);
    for pair in steps.windows(2) {
        assert!(pair[1] > pair[0], "steps did not rise: {:?}", steps);
    }
}

/// Every chunk carries counts, pause or not.
#[test]
fn each_chunk_reports_what_it_cost() {
    let id = fumola_create();
    let first = parse(&fumola_eval_top_limited(
        id, "var i = 0; while (i < 300) { i += 1 }; i", 60,
    ));
    assert_eq!(first["paused"], true, "expected a pause: {}", first);
    assert!(
        first["counts"]["step"].as_u64().unwrap() > 0,
        "a chunk that ran reported no steps: {}", first
    );
}

/// A short program answers without pausing.
#[test]
fn a_short_program_never_pauses() {
    let id = fumola_create();
    let reply = parse(&fumola_eval_top_limited(id, "1 + 2", 1_000_000));
    assert_eq!(reply["ok"], true, "{}", reply);
    assert!(reply["paused"].is_null(), "short program paused: {}", reply);
    assert_eq!(reply["value"], "3");
}

/// A failure is an error, not a pause.
#[test]
fn a_failure_is_reported_as_a_failure() {
    let id = fumola_create();
    let reply = parse(&fumola_eval_top_limited(id, "assert (1 == 2)", 1_000_000));
    assert_eq!(reply["ok"], false, "assertion failure reported ok: {}", reply);
    assert!(reply["paused"].is_null());
    assert!(reply["counts"]["step"].as_u64().is_some(), "no counts on failure");
}

/// Abandoning a pause leaves the instance usable.
///
/// The case that matters: a host stops resuming because the user edited the
/// program. Without a way to drop the continuation, the next unlimited eval
/// would meet a non-idle agent.
#[test]
fn an_abandoned_pause_leaves_the_instance_usable() {
    let id = fumola_create();
    let paused = parse(&fumola_eval_top_limited(
        id, "var i = 0; while (i < 500) { i += 1 }; i", 40,
    ));
    assert_eq!(paused["paused"], true, "expected a pause: {}", paused);

    let dropped = parse(&fumola_abandon(id));
    assert_eq!(dropped["ok"], true, "{}", dropped);

    let after = parse(&fumola_eval_top(id, "1 + 1"));
    assert_eq!(after["ok"], true, "instance unusable after abandon: {}", after);
    assert_eq!(after["value"], "2");
}

/// Zero is not a budget that stalls.
///
/// `steps.max(1)` is why: a host passing 0 would otherwise ask the VM to stop
/// where it already stands, and resume forever without advancing.
#[test]
fn a_zero_budget_still_advances() {
    let id = fumola_create();
    let mut reply = parse(&fumola_eval_top_limited(id, "1 + 2", 0));
    let mut rounds = 0;
    while reply["paused"] == true {
        rounds += 1;
        assert!(rounds < 1_000, "a zero budget made no progress");
        reply = parse(&fumola_resume(id, 0));
    }
    assert_eq!(reply["ok"], true, "{}", reply);
    assert_eq!(reply["value"], "3");
}

/// A chunked run that fails costs nothing, as an unlimited one does not.
///
/// `fumola_eval_top` evaluates against a copy and commits on success. A
/// resumable run cannot, so it snapshots instead. This is that guarantee:
/// a binding made by a program that then failed must not survive it.
#[test]
fn a_failed_chunked_run_leaves_no_trace() {
    let id = fumola_create();
    parse(&fumola_eval_top(id, "let kept = 41"));

    // Writes, then fails, and takes several chunks to get there.
    let reply = parse(&fumola_eval_top_limited(
        id,
        "let kept = 999; var i = 0; while (i < 300) { i += 1 }; assert (1 == 2)",
        50,
    ));
    let mut reply = reply;
    let mut rounds = 0;
    while reply["paused"] == true {
        rounds += 1;
        assert!(rounds < 10_000);
        reply = parse(&fumola_resume(id, 50));
    }
    assert_eq!(reply["ok"], false, "expected the assertion to fail: {}", reply);
    assert!(rounds > 1, "it finished in one chunk; the snapshot was not exercised");

    let after = parse(&fumola_eval_top(id, "kept"));
    assert_eq!(after["ok"], true, "instance unusable after a failed run: {}", after);
    assert_eq!(
        after["value"], "41",
        "a failed chunked run kept its binding: {}", after
    );
}

/// Abandoning mid-run also costs nothing.
#[test]
fn abandoning_restores_what_was_there() {
    let id = fumola_create();
    parse(&fumola_eval_top(id, "let kept = 41"));
    let paused = parse(&fumola_eval_top_limited(
        id, "let kept = 999; var i = 0; while (i < 500) { i += 1 }; i", 40,
    ));
    assert_eq!(paused["paused"], true, "{}", paused);
    parse(&fumola_abandon(id));
    let after = parse(&fumola_eval_top(id, "kept"));
    assert_eq!(after["value"], "41", "abandon kept the run's write: {}", after);
}

/// A resume does not overwrite the snapshot with a half-run instance.
#[test]
fn a_resume_keeps_the_original_snapshot() {
    let id = fumola_create();
    parse(&fumola_eval_top(id, "let kept = 41"));
    let mut reply = parse(&fumola_eval_top_limited(
        id, "let kept = 999; var i = 0; while (i < 400) { i += 1 }; assert false", 40,
    ));
    let mut rounds = 0;
    while reply["paused"] == true {
        rounds += 1;
        assert!(rounds < 10_000);
        reply = parse(&fumola_resume(id, 40));
    }
    assert!(rounds > 3, "too few chunks to test this: {}", rounds);
    assert_eq!(reply["ok"], false);
    let after = parse(&fumola_eval_top(id, "kept"));
    assert_eq!(
        after["value"], "41",
        "after {} resumes the snapshot was no longer the original: {}", rounds, after
    );
}

/// Progress is this run's steps, not the instance's lifetime total.
///
/// An instance that has imported the library starts tens of thousands of
/// steps in. Reporting that total would open the count high and its rise
/// would say nothing about the program asked for.
#[test]
fn progress_counts_this_run_not_the_instance() {
    let id = fumola_create();
    // Spend some steps first, so a lifetime total and a per-run count differ.
    parse(&fumola_eval_top(id, "var w = 0; while (w < 400) { w += 1 }"));

    let first = parse(&fumola_eval_top_limited(
        id, "var i = 0; while (i < 400) { i += 1 }; i", 60,
    ));
    assert_eq!(first["paused"], true, "expected a pause: {}", first);
    let ran = first["steps"].as_u64().expect("steps");
    let total = first["stepsTotal"].as_u64().expect("stepsTotal");
    assert!(
        ran <= 200,
        "first chunk of 60 reported {} steps for this run", ran
    );
    assert!(
        total > ran,
        "lifetime total {} not greater than this run's {}", total, ran
    );
}
