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

use fumola_semantics::adapton::AdaptonState;
use fumola_semantics::adapton::graphical::History;
use fumola_semantics::value::Value_;
use fumola_semantics::vm_types::{Active, ActiveBorrow, Core, Counts, Interruption, Limits};

/// The answer a program gave, whichever kind of answer it was.
#[derive(Debug, PartialEq)]
enum Answer {
    Value(String),
    Failed(String),
}

/// Everything one run leaves behind that the two modes have to agree on.
struct Outcome {
    answer: Answer,
    counts: Counts,
    /// The graph's event history. Under the graphical strategy this is every
    /// `force_begin`, `force_end`, edge and repair event in order, with its
    /// meta-time; under the simple strategy it is empty. For the forms that run
    /// recursively and touch the graph -- force, and repair under it -- this is
    /// the oracle the counts cannot be.
    history: Option<History>,
}

fn run(src: &str) -> Outcome {
    let prog = match fumola::check::parse(src) {
        Ok(p) => p,
        Err(e) => panic!("{} did not parse: {:?}", src, e),
    };
    let mut core = Core::new(prog);
    let answer = match core.run(&Limits::none()) {
        Ok(v) => Answer::Value(format!("{:?}", v)),
        // Compared by rendering: the point is that a region fails the way the
        // machine fails, in the same words, and `Interruption` is not `Eq`.
        Err(i) => Answer::Failed(without_locator(&format!("{}", i))),
    };
    let counts = core.counts().clone();
    let history = core.adapton().history().ok();
    Outcome {
        answer,
        counts,
        history,
    }
}

/// A type mismatch says where in the VM the check was made -- `(at file:line)`.
/// When a region makes that check it is made in `vm_big.rs`, and the message
/// says so; making it name `vm_stack_cont.rs` instead would be a lie about
/// where the code ran. So the locator is dropped before comparing, and what is
/// asserted is that the two modes fail with the same *kind* of failure.
fn without_locator(message: &str) -> String {
    match message.rfind(" (at ") {
        Some(i) if message.ends_with(')') => message[..i].to_string(),
        _ => message.to_string(),
    }
}

/// Assert that a body means the same thing under both modes.
///
/// `do small { .. }` rather than a bare `do { .. }` so that the two programs
/// differ by exactly one token, and the `Do` frame is in both. The mode is
/// padded to the same width -- `do big   {` -- so that every span in the body
/// lands at the same offset in both programs. The graph's history embeds thunk
/// bodies as syntax, positions included, and a two-character shift would make
/// two identical graphs compare unequal.
#[track_caller]
fn agree(body: &str) -> (Answer, Counts) {
    let small = run(&format!("do small {{ {} }}", body));
    let big = run(&format!("do big   {{ {} }}", body));

    assert_eq!(
        small.answer, big.answer,
        "\n  the two modes disagree about what `{}` means",
        body
    );
    assert_eq!(
        small.counts.step, big.counts.step,
        "\n  `{}`: small-step took {} steps, big-step charged {}",
        body, small.counts.step, big.counts.step
    );
    assert_eq!(
        small.counts.redex, big.counts.redex,
        "\n  `{}`: small-step retired {} redexes, big-step charged {}",
        body, small.counts.redex, big.counts.redex
    );
    assert_eq!(
        small.history, big.history,
        "\n  `{}`: the two modes built different graphs",
        body
    );
    (big.answer, big.counts)
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

/// `force` now runs recursively; `@`, the puts, the navigations and scratch
/// are still delegated. So these programs mix the two, and what is being
/// checked -- value, counts, and the graph's event history -- is that the
/// brackets `force_begin`/`force_end` open and close in the machine's order
/// whichever side of the line they fall on.
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
/// re-entrant evaluator would most plausibly diverge, and now it is reached:
/// `force t` the second time finds a signaled edge, and `repair` in vm_big
/// drives `repair_step`/`repair_resume` in the machine's order. The event
/// history is what says the two agree.
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

// ---------------------------------------------------------------------------
// Calls, which the recursive evaluator now enters rather than handing back.
// ---------------------------------------------------------------------------

/// Recursion, where the Rust stack now tracks the user's.
#[test]
fn recursion() {
    agrees_on(
        "func fact(n : Nat) : Nat { if (n == 0) 1 else n * fact(n - 1) }; fact(10)",
        "3628800",
    );
    agrees_on(
        "func fib(n : Nat) : Nat { if (n < 2) n else fib(n - 1) + fib(n - 2) }; fib(15)",
        "610",
    )
}

/// Mutual recursion, so the two functions' environments interleave.
///
/// Through a parameter, because a forward reference does not work in Fumola at
/// all: `Dec::Func` inserts the closure when the declaration runs, so a body
/// naming a function declared later captured an environment without it. That is
/// the case below, and both modes say so in the same words -- which is a real
/// assertion, not a concession, since error identity is part of what a region
/// has to preserve.
#[test]
fn mutual_recursion_through_a_parameter() {
    agrees_on(
        "func evenWith(od : Nat -> Bool, n : Nat) : Bool { if (n == 0) true else od(n - 1) }; \
         func odd(n : Nat) : Bool { if (n == 0) false else evenWith(odd, n - 1) }; \
         (odd(7), odd(8))",
        "(true, false)",
    )
}

/// And the forward reference fails alike under both modes.
#[test]
fn a_forward_reference_fails_alike() {
    agree(
        "func even(n : Nat) : Bool { if (n == 0) true else odd(n - 1) }; \
         func odd(n : Nat) : Bool { if (n == 0) false else even(n - 1) }; \
         even(10)",
    );
}

/// A `return` from inside a recursive call unwinds to that call's own frame,
/// not to the region's.
///
/// The machine's `return_` scans the real stack for a `Call3`, and this
/// evaluator pushes a real one through `call_cont` -- so the target it finds is
/// the right one, and what is left is to notice that the stack came back to the
/// call's level and take the value.
#[test]
fn a_return_inside_a_call() {
    agrees_on("func f(n : Nat) : Nat { return n + 1; 99 }; f(1)", "2");
    agrees_on(
        "func f(n : Nat) : Nat { if (n > 3) { return 0 }; n }; (f(1), f(10))",
        "(1, 0)",
    );
    // A return out of a loop inside a call.
    agrees_on(
        "func first(n : Nat) : Nat { \
           var i = 0; while (i < 100) { if (i == n) { return i }; i := i + 1 }; 999 \
         }; first(7)",
        "7",
    );
    // Nested calls, each with its own return.
    agrees_on(
        "func inner(n : Nat) : Nat { return n * 2 }; \
         func outer(n : Nat) : Nat { return inner(n) + 1 }; outer(5)",
        "11",
    )
}

/// A closure carries its environment, and a call must not see the caller's.
#[test]
fn closures_keep_their_environment() {
    agrees_on(
        "let x = 1; func f() : Nat { x }; do { let x = 2; f() }",
        "1",
    );
    agrees_on(
        "func adder(n : Nat) : (Nat -> Nat) { func (m : Nat) : Nat { n + m } }; \
         let add3 = adder(3); add3(4)",
        "7",
    )
}

/// A primitive is dispatched by the same `call_cont` and finished by the
/// machine, because it may evaluate Fumola code of its own.
#[test]
fn a_primitive_call_is_finished_by_the_machine() {
    agrees_on("prim \"natToText\" 42", "\"42\"");
    agree("let Debug = { print = prim \"print\" }; Debug.print 1");
}

/// A call whose callee is an actor method still refuses, rather than resuming
/// into a continuation that is no longer there.
#[test]
fn an_actor_call_inside_a_region_still_refuses() {
    let src = r#"
        actor A { public func get() : async Nat { 1 } };
        do big { let x = 1; A.get() }
    "#;
    match fumola::eval::eval(src) {
        Err(fumola::Error::Interruption(Interruption::BigStepEscape(_))) => {}
        other => panic!("expected the send to be refused, got {:?}", other),
    }
}

/// Recursion deeper than the evaluator will go on the Rust stack.
///
/// Past the depth budget a node is handed to the machine, which is the same
/// demotion any unsupported form gets -- so the answer, the steps and the
/// redexes are unchanged, and only the time differs. This is the test that says
/// the demotion is invisible.
#[test]
fn recursion_past_the_depth_budget() {
    agrees_on(
        "func count(n : Nat) : Nat { if (n == 0) 0 else 1 + count(n - 1) }; count(400)",
        "400",
    )
}

/// Calls inside a `force`, so a call and the graph meet.
#[test]
fn a_call_inside_a_force() {
    agree(
        "prim \"adaptonReset\" (#graphical); \
         func double(n : Nat) : Nat { n * 2 }; \
         let x = `x := 3; \
         let t = `t := thunk { double(@ x) }; \
         let a = force t; x := 5; (a, force t)",
    );
}


// ---------------------------------------------------------------------------
// Force, which the recursive evaluator now enters.
// ---------------------------------------------------------------------------

fn graphical(body: &str) -> String {
    format!("prim \"adaptonReset\" (#graphical); {}", body)
}

/// A cache miss and then a cache hit, under both strategies.
#[test]
fn a_force_misses_then_hits() {
    for strategy in ["#simple", "#graphical"] {
        agree(&format!(
            "prim \"adaptonReset\" ({}); let t = `t := thunk {{ 1 + 2 }}; (force t, force t)",
            strategy
        ));
    }
}

/// A bare thunk, with no pointer into the graph.
#[test]
fn a_bare_thunk() {
    agrees_on("force (thunk { 40 + 2 })", "42");
    agrees_on("let t = thunk { 40 + 2 }; (force t, force t)", "(42, 42)")
}

/// Forces nested inside forces, so the `force_begin`/`force_end` brackets have
/// to nest in the graph exactly as they do on the machine's stack.
#[test]
fn nested_forces() {
    agree(&graphical(
        "let a = `a := thunk { 1 }; \
         let b = `b := thunk { (force a) + 10 }; \
         let c = `c := thunk { (force b) + 100 }; \
         (force c, force b, force a)",
    ));
}

/// A thunk body that calls, so recursion into a call happens inside recursion
/// into a force.
#[test]
fn a_call_inside_a_forced_thunk() {
    agree(&graphical(
        "func fib(n : Nat) : Nat { if (n < 2) n else fib(n - 1) + fib(n - 2) }; \
         let t = `t := thunk { fib(12) }; force t",
    ));
}

/// Repair, in its three shapes: a reader whose input changed, a reader whose
/// input changed back to the same value, and a chain two forces deep.
#[test]
fn repair_in_three_shapes() {
    // Signaled, misaligned: the reader is reevaluated.
    agree(&graphical(
        "let x = `x := 1; let t = `t := thunk { (@ x) + 1 }; \
         let a = force t; x := 5; (a, force t)",
    ));
    // Signaled, aligned after the check: the input was written with the value
    // it already had.
    agree(&graphical(
        "let x = `x := 1; let t = `t := thunk { (@ x) + 1 }; \
         let a = force t; x := 1; (a, force t)",
    ));
    // A chain: repairing `u` means forcing `t`, which is itself signaled.
    agree(&graphical(
        "let x = `x := 1; \
         let t = `t := thunk { (@ x) * 2 }; \
         let u = `u := thunk { (force t) + 1 }; \
         let a = force u; x := 10; (a, force u, force t)",
    ));
}

/// A `return` from inside a forced thunk inside a call.
///
/// `return_` scans the real stack for a `Call3`, and on its way past the
/// `ForceAdaptonPointer` frame it does not call `force_end` -- the force is
/// simply abandoned. That is what the machine does, and a region has to do the
/// same thing, which it does by propagating `Escaped` and closing nothing.
#[test]
fn a_return_inside_a_forced_thunk() {
    agrees_on(
        "func f() : Nat { force (thunk { return 5 }); 9 }; f()",
        "5",
    );
    agree(&graphical(
        "func f() : Nat { let t = `t := thunk { return 5 }; force t; 9 }; f()",
    ));
}

/// A failure inside a forced thunk fails alike, and leaves the stack standing.
#[test]
fn a_failure_inside_a_forced_thunk_fails_alike() {
    agree(&graphical("let t = `t := thunk { assert false; 1 }; force t"));
    // Not `1 / 0`: on HEAD that panics inside num-bigint rather than raising
    // `DivideByZero`, under either mode. A machine bug, filed separately.
    agree(&graphical("let t = `t := thunk { nosuch + 1 }; force t"));
}

/// Forcing something that is not a thunk fails alike.
#[test]
fn forcing_a_non_thunk_fails_alike() {
    agree("force 3");
}

/// A thunk that forces itself recursively, past the depth budget, so some of
/// the forces run on the machine and some on the Rust stack -- and the graph
/// must not be able to tell.
#[test]
fn deep_thunk_recursion_past_the_depth_budget() {
    agree(&graphical(
        "func down(n : Nat) : Nat { if (n == 0) 0 else 1 + (force (thunk { down(n - 1) })) }; \
         down(300)",
    ));
}


// ---------------------------------------------------------------------------
// Failures raised by the recursive evaluator's own arms.
// ---------------------------------------------------------------------------

/// Each of these fails inside an arm this file mirrors, not inside a shared
/// helper -- so the failure's kind is what is being pinned. The locator
/// (`at file:line`) differs by design and is not compared.
#[test]
fn type_mismatches_in_mirrored_arms_fail_alike() {
    agree("not 3");
    agree("3 and true");
    agree("true and 3");
    agree("3 or true");
    agree("false or 3");
    agree("if 3 1 else 2");
    agree("while 3 { }");
    agree("var i = 0; while (i < 1) { i := i + 1; 5 }");
    agree("assert 3");
    agree("let (a, b) = 3; a");
}
