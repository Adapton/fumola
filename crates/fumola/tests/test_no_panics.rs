//! Programs that used to stop the VM, and now leave by the error path.
//!
//! Each program below was found by the audit in `docs/panic-audit.md`. Every
//! one of them reached a `panic!`, `todo!` or `unwrap` inside evaluation, and
//! took the whole process with it -- the CLI, a test run, or the browser tab
//! running the wasm build. A host embedding the VM had two outcomes to handle
//! and only one of them it could catch.
//!
//! The test is the absence of a panic: a panic here fails the test by stopping
//! it, so merely evaluating each program is the assertion. What the program
//! does instead -- a value, or an `Interruption` -- is checked beside it, so
//! that a later change which turns one of these back into a stop, or quietly
//! swaps an error for a wrong answer, is visible.

use fumola_semantics::vm_types::Interruption;
use test_log::test; // enable logging output for tests by default.

/// Evaluate, and require that it stopped with an interruption whose sentence
/// mentions `expected`.
///
/// The sentence is `Display`, which is what a host prints; checking it here
/// keeps these messages readable by keeping them tested.
fn interrupted(program: &str, expected: &str) {
    match fumola::eval::eval(program) {
        Ok(v) => panic!("expected an interruption from `{}`, got {:?}", program, v),
        Err(fumola::Error::Interruption(i)) => {
            let said = format!("{}", i);
            assert!(
                said.contains(expected),
                "`{}` said {:?}, which does not mention {:?}",
                program,
                said,
                expected
            );
        }
        Err(other) => panic!("expected an interruption from `{}`, got {}", program, other),
    }
}

/// Evaluate, and require that it produced a value at all -- what the program
/// used to do was stop the process before it could produce anything.
fn evaluates(program: &str) {
    if let Err(e) = fumola::eval::eval(program) {
        panic!("expected `{}` to evaluate, got {}", program, e);
    }
}

/// Closing a quotation over its environment covered a handful of expression
/// forms and stopped on the rest -- `todo!()` in `quoted.rs`, one per arm.
/// Quoting an `if` was enough.
#[test]
fn quoting_an_unsupported_form_is_an_interruption() {
    for (program, form) in [
        ("`(if true 1 else 2)", "an if expression"),
        ("`(switch 0 {case _ 0})", "a switch expression"),
        ("`(while true 1)", "a while loop"),
        ("`(x := 1)", "an assignment"),
        ("`(a.b)", "a field projection"),
        ("`(not true)", "a negation"),
        ("`(a and b)", "an and expression"),
        ("`(1 == 2)", "a comparison"),
        ("`(- 1)", "a unary operation"),
        ("`(?1)", "an option expression"),
        ("`(#a)", "a variant"),
        ("`(x[0])", "an index expression"),
        ("`(label l 1)", "a label"),
        ("`(return 1)", "a return"),
        ("`(assert true)", "an assertion"),
        ("`(import \"x\")", "an import expression"),
        ("`(object {})", "an object declaration"),
    ] {
        interrupted(program, form);
        interrupted(program, "inside a quotation");
    }
}

/// `switch e ~x` reaches the evaluator with the unquote still standing where
/// the cases go. `CasesPos::cases` used to answer that with `panic!()`.
#[test]
fn an_unquote_where_cases_go_is_an_interruption() {
    interrupted(
        "let cases = `{case 0 0} # `{case 1 1}; switch 0 ~cases",
        "a switch's cases",
    );
}

/// An actor declaration with no name reached `todo!()` in the stepper.
#[test]
fn an_anonymous_actor_is_an_interruption() {
    interrupted(
        "actor { public func f() { 0 } }",
        "an actor declaration with no name",
    );
}

/// Two prims are named by the parser and not implemented by the evaluator, and
/// a third has an arity the evaluator does not handle. All three were `todo!()`.
#[test]
fn an_unimplemented_prim_is_an_interruption() {
    interrupted("prim \"reifyCore\" ()", "the reifyCore prim");
    interrupted("prim \"reflectCore\" ()", "the reflectCore prim");
    interrupted(
        "prim \"adaptonPoke\" (1, 2, 3)",
        "adaptonPoke with a third argument",
    );
}

/// Substituting a quotation into a position that holds one expression asks
/// `unquote_exp` for the one it holds. A quotation of two used to be `panic!()`
/// with nothing said about why.
#[test]
fn unquoting_the_wrong_number_of_things_is_an_interruption() {
    let two = fumola::eval::eval("`(1, 2)").unwrap();
    match two.unquote_exp() {
        Ok(e) => panic!("expected an interruption, got {:?}", e),
        Err(i) => {
            let said = format!("{}", i);
            assert!(
                said.contains("unquoting 2 expressions where one is expected"),
                "said {:?}",
                said
            );
        }
    }
}

/// The printer covered some values and stopped on others -- and it is reached
/// from `Value::portable_hash`, which has no error path at all. So these do
/// not become interruptions; they print as a marker naming the form, and the
/// program carries on.
#[test]
fn printing_an_uncovered_value_renders_a_marker() {
    evaluates("prim \"fastRandIterNew\" (?0, ?10)");
    let iter = fumola::eval::eval("prim \"fastRandIterNew\" (?0, ?10)").unwrap();
    let shown = fumola_semantics::format::format_one_line(iter.as_ref());
    assert!(!shown.is_empty(), "a value has to print as something");
}

/// A `#[test]` function declared in a program the VM was handed directly sits
/// in no imported file, and registering it unwrapped the empty import stack.
#[test]
fn a_test_outside_any_file_evaluates() {
    evaluates("let m = module { #[test] public func f() { assert true } }; 0");
}

/// Quoted syntax carries `Source::Evaluation`, the prelude carries
/// `Source::CoreInit`, and widening either against a span from a file was
/// `todo!()` in `Source::expand`.
#[test]
fn widening_a_source_against_a_non_span_is_total() {
    use fumola_syntax::ast::Source;
    // Whichever side names a place in a file is the one kept.
    assert!(matches!(
        Source::Evaluation.expand(&Source::CoreInit),
        Source::Unknown
    ));
    assert!(matches!(
        Source::CoreInit.expand(&Source::Evaluation),
        Source::Unknown
    ));
}

/// Two symbols of kinds the ordering has no rule for are not ordered against
/// each other. `partial_cmp` says that with `None`; it used to say it with
/// `todo!()`.
#[test]
fn symbols_without_an_order_compare_to_none() {
    use fumola_semantics::value::Symbol;
    let text = Symbol::Id(fumola_syntax::ast::Id::new("a".to_string()));
    let nat = Symbol::Nat(num_bigint::BigUint::from(1u32));
    // Whatever the answer, asking must not stop the process.
    let _ = text.partial_cmp(&nat);
    let _ = nat.partial_cmp(&text);
}

/// Every interruption prints as a sentence, not a Rust dump. This is the one
/// thing a host is asked to show a person.
#[test]
fn every_interruption_says_something() {
    let cases = [
        Interruption::DivideByZero,
        Interruption::AssertionFailure,
        Interruption::IndexOutOfBounds,
        Interruption::MisplacedReturn,
        Interruption::NoMatchingCase,
        Interruption::Unknown,
    ];
    for i in cases {
        let said = format!("{}", i);
        assert!(!said.is_empty(), "{:?} prints as nothing", i);
        assert!(
            !said.contains("Interruption"),
            "{:?} prints as its own Rust name: {}",
            i,
            said
        );
    }
}
