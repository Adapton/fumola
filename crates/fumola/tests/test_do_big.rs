//! `do big { e }` must mean what `do small { e }` means.
//!
//! Every test here runs one program twice, differing by a single token, and
//! compares three things: the answer, the number of steps, and the number of
//! redexes.
//!
//! The counts are not decoration. For the forms the recursive evaluator handles
//! itself -- binders, blocks, operators, the loops, assignment -- none of them
//! touches the graph, so comparing graphs would say nothing at all about them.
//! The counts are the only witness that the recursive path made the same
//! transitions in the same order. A missing implicit dereference, a
//! subexpression evaluated in the wrong order, or a short-circuit taken one
//! branch too early are each silent in the answer and loud in the counts.
//!
//! For the forms it hands back to the machine, the situation is reversed: the
//! counts are trivially equal because the machine is doing the work, and what
//! is worth checking is that the graph came out the same. That is
//! `test_adapton_graphical::test_events_do_big`, and the adapton programs at
//! the end of this file.

use fumola_semantics::value::Value_;
use fumola_semantics::vm_types::{ActiveBorrow, Core, Counts, Interruption, Limits};

/// The answer a program gave, whichever kind of answer it was.
#[derive(Debug, PartialEq)]
enum Answer {
    Value(String),
    Failed(String),
}

fn run(src: &str) -> (Answer, Counts) {
    let prog = match fumola::check::parse(src) {
        Ok(p) => p,
        Err(e) => panic!("{} did not parse: {:?}", src, e),
    };
    let mut core = Core::new(prog);
    let answer = match core.run(&Limits::none()) {
        Ok(v) => Answer::Value(format!("{:?}", v)),
        // Compared by rendering: the point is that a region fails the way the
        // machine fails, in the same words, and `Interruption` is not `Eq`.
        Err(i) => Answer::Failed(format!("{}", i)),
    };
    (answer, core.counts().clone())
}

/// Assert that a body means the same thing under both modes.
///
/// `do small { .. }` rather than a bare `do { .. }` so that the two programs
/// differ by exactly one token, and the `Do` frame is in both.
#[track_caller]
fn agree(body: &str) -> (Answer, Counts) {
    let (small_answer, small) = run(&format!("do small {{ {} }}", body));
    let (big_answer, big) = run(&format!("do big {{ {} }}", body));

    assert_eq!(
        small_answer, big_answer,
        "\n  the two modes disagree about what `{}` means",
        body
    );
    assert_eq!(
        small.step, big.step,
        "\n  `{}`: small-step took {} steps, big-step charged {}",
        body, small.step, big.step
    );
    assert_eq!(
        small.redex, big.redex,
        "\n  `{}`: small-step retired {} redexes, big-step charged {}",
        body, small.redex, big.redex
    );
    (big_answer, big)
}

/// The value a body evaluates to, for the tests that also want to pin it.
fn value_of(v: &str) -> String {
    let prog = fumola::check::parse(v).expect("the expected value did not parse");
    let mut core = Core::new(prog);
    let v: Value_ = core.run(&Limits::none()).expect("the expected value failed");
    format!("{:?}", v)
}

#[track_caller]
fn agrees_on(body: &str, expected: &str) {
    let (answer, _) = agree(body);
    assert_eq!(answer, Answer::Value(value_of(expected)), "body: {}", body);
}

// ---------------------------------------------------------------------------
// The recursive set, with nothing delegated at all.
// ---------------------------------------------------------------------------

/// The one program in this file where the recursive evaluator does all of the
/// work: every node in it is in the set, so the step count is the count the
/// machine would have paid and did not.
#[test]
fn a_while_loop_over_binders() {
    agrees_on(
        "var i = 0; var s = 0; while (i < 20) { s := s + i; i := i + 1 }; s",
        "190",
    )
}

#[test]
fn literals_and_arithmetic() {
    agrees_on("1 + 2 * 3", "7");
    agrees_on("(1 + 2) * (3 + 4)", "21");
    agrees_on("- (1 + 2)", "-3");
}

#[test]
fn nested_parentheses() {
    agrees_on("((((1))))", "1")
}

#[test]
fn comparisons() {
    agrees_on("1 < 2", "true");
    agrees_on("2 <= 2", "true");
    agrees_on("1 == 2", "false");
}

/// Short-circuiting, on both sides of both operators.
///
/// `Or1` is the one entry in the machine's redex table that depends on the
/// value rather than only on the frame, so an `or` that short-circuits and one
/// that does not charge differently. Both are here.
#[test]
fn short_circuits() {
    agrees_on("true and false", "false");
    agrees_on("false and true", "false");
    agrees_on("true or false", "true");
    agrees_on("false or true", "true");
    agrees_on("not true", "false");
}

#[test]
fn conditionals() {
    agrees_on("if (1 < 2) 10 else 20", "10");
    agrees_on("if (2 < 1) 10 else 20", "20");
    // No else branch: the machine answers with unit.
    agree("if (2 < 1) 10");
}

#[test]
fn tuples() {
    agrees_on("(1, 2, 3)", "(1, 2, 3)");
    agrees_on("()", "()");
    agrees_on("(1 + 1, 2 + 2)", "(2, 4)");
}

#[test]
fn blocks_and_binders() {
    agrees_on("let x = 1; let y = 2; x + y", "3");
    agrees_on("let x = 1; do { let y = 2; x + y }", "3");
    // A block whose last declaration binds a plain variable answers with it.
    agrees_on("let x = 41; let y = x + 1; y", "42");
    // A block ending in `var` answers with unit.
    agree("var x = 1");
    // An empty block.
    agree("{ }");
}

#[test]
fn assertions() {
    agrees_on("assert (1 < 2)", "()");
    // And a failing one fails the same way in both modes.
    agree("assert (2 < 1)");
}

/// The implicit dereference, at the place where it is declined.
///
/// A `let`'s right-hand side keeps the pointer, because `FrameCont::Let` is one
/// of the four frames the machine checks for before dereferencing; a `var`'s
/// right-hand side is not, so it takes the value. So `y` follows `x` here, and
/// the pair is `(2, 2)` rather than `(1, 2)` -- verified against the CLI on
/// HEAD, and the reason this is worth pinning: dereferencing one position too
/// eagerly turns the alias into a copy, and nothing says so until the later
/// assignment.
#[test]
fn the_implicit_dereference() {
    agrees_on("var x = 1; let y = x; x := 2; (y, x)", "(2, 2)");
    // The same shape through a `var`, which does take the value.
    agrees_on("var x = 1; var y = x; x := 2; (y, x)", "(1, 2)")
}

#[test]
fn assignment_to_a_var() {
    agrees_on("var x = 1; x := x + 1; x", "2")
}

/// `cont_prim_type` -- the annotation that decides whether `+%` wraps -- is
/// carried by the frame, and restored when it pops. The recursive evaluator has
/// no frame, so it carries it by hand; this is the program that notices.
///
/// Both modes fail here, identically. That is the assertion.
#[test]
fn the_ambient_primitive_type() {
    agree("(200 : Nat8) +% 100");
    agrees_on("((200 : Nat8) +% 100 : Nat8)", "((200 : Nat8) +% 100 : Nat8)");
}

// ---------------------------------------------------------------------------
// Delegation: the forms the machine keeps.
// ---------------------------------------------------------------------------

/// A call is delegated, so this is really a test that handing a node back and
/// taking its value produces no drift at all.
#[test]
fn a_call_is_delegated() {
    agrees_on("func f(x : Nat) : Nat { x + 1 }; f(1)", "2");
    agrees_on("func add(x : Nat, y : Nat) : Nat { x + y }; add(20, 22)", "42");
}

#[test]
fn a_switch_is_delegated() {
    agrees_on("switch (?1) { case (?x) x; case null 0 }", "1")
}

#[test]
fn an_array_is_delegated() {
    agrees_on("let a = [1, 2, 3]; a[1]", "2");
    agrees_on("var a = [var 1, 2, 3]; a[0] := 9; a[0]", "9")
}

#[test]
fn an_object_is_delegated() {
    agrees_on("let o = { x = 1; y = 2 }; o.x + o.y", "3")
}

#[test]
fn a_for_loop_is_delegated() {
    agrees_on(
        "var s = 0; for (x in [1, 2, 3].vals()) { s := s + x }; s",
        "6",
    )
}

/// A region inside a region. The inner one is entered without asking about
/// limits again, because the outer one already settled that question.
#[test]
fn regions_nest() {
    agrees_on("1 + (do big { 2 + 3 })", "6");
    agrees_on("1 + (do small { 2 + 3 })", "6");
    agrees_on("let x = (do big { 1 + 1 }); x * 2", "4")
}

// ---------------------------------------------------------------------------
// Leaving a region other than by finishing it.
// ---------------------------------------------------------------------------

/// `return` from inside a region whose target is outside it.
///
/// The machine's `return_` scans the real stack and writes it directly. When
/// the target is below the watermark, the Rust frames standing above are the
/// continuation that the return discards by definition, so unwinding them is
/// the whole of the answer.
#[test]
fn a_return_leaves_the_region() {
    agrees_on("func f() : Nat { do big { return 3 }; 4 }; f()", "3")
}

/// The same, for `!`.
#[test]
fn a_bang_leaves_the_region() {
    agrees_on("do ? { do big { (null : ?Nat)! }; 1 }", "null");
    agrees_on("do ? { do big { (?41 : ?Nat)! + 1 } }", "?42")
}

// ---------------------------------------------------------------------------
// The two loud failures.
// ---------------------------------------------------------------------------

#[test]
fn an_unknown_mode_is_named() {
    match fumola::eval::eval("do frobnicate { 1 }") {
        Err(fumola::Error::Interruption(Interruption::UnknownEvalMode(id, _))) => {
            assert_eq!(id.as_str(), "frobnicate")
        }
        other => panic!("expected an unknown-mode error, got {:?}", other),
    }
}

/// A region is not a place to pause, so an interruption a host would have
/// resumed must not be handed back as if it could be.
///
/// The danger is precise: `Core::step` answers a `Send` by running the standing
/// continuation, and the part of that continuation which lived in Rust frames
/// is gone by the time anyone sees it. Resuming would give a wrong answer and
/// no error at all, so it does not resume.
#[test]
fn an_actor_send_inside_a_region_does_not_resume() {
    let src = r#"
        actor A { public func get() : async Nat { 1 } };
        do big { A.get() }
    "#;
    match fumola::eval::eval(src) {
        Err(fumola::Error::Interruption(Interruption::BigStepEscape(_))) => {}
        other => panic!("expected the send to be refused, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// The adapton corpus, unchanged inside a region.
// ---------------------------------------------------------------------------

/// Every adapton operation in these programs is delegated, so what is being
/// checked is that the delegation boundary does not disturb the brackets --
/// `force_begin`/`force_end`, the navigations, and repair.
#[test]
fn the_adapton_corpus_is_unchanged() {
    for strategy in ["#simple", "#graphical"] {
        let reset = format!("prim \"adaptonReset\" ({});", strategy);
        for program in [
            "force (thunk {1 + 2})",
            "let t = thunk { 1 + 2 }; (force t, force t)",
            "force (`t := thunk { @(`x := 3) })",
            "let x = `x := 1; let t = `t := thunk { @ x }; force t",
        ] {
            agree(&format!("{} {}", reset, program));
        }
    }
}

/// Signalling, realignment and a cache hit in one program -- the place a
/// re-entrant evaluator would most plausibly diverge. It cannot here, because
/// repair is reached only from the machine's own force path, which a region
/// never takes over.
#[test]
fn realignment_is_unchanged() {
    agree(
        "prim \"adaptonReset\" (#graphical); \
         let x = `x := 1; \
         let t = `t := thunk { @ x }; \
         let a = force t; \
         x := 2; \
         (a, force t)",
    );
}

/// A failed scratch inside a region leaves the session as it was.
///
/// `prim "adaptonScratch"` is delegated, so it pushes a real `Scratch` frame,
/// above the watermark. Nothing unwinds the stack on an interruption -- it is
/// left standing for inspection -- and `Core::unwind_scratch` is what puts the
/// graph back. This checks that a region does not get in its way.
#[test]
fn a_failed_scratch_inside_a_region_leaves_the_session_as_it_was() {
    agree(
        "prim \"adaptonReset\" (#graphical); \
         let c = `c := 41; \
         do big { prim \"adaptonScratch\" (thunk { c := 999; assert false }) }; \
         @ c",
    );
}
