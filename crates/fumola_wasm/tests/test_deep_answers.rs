//! Deep answers: a value nested a hundred thousand deep comes out as an
//! answer, and the instance keeps working after it.
//!
//! Turning an answer into JSON used to recurse once per nested value, so an
//! option list of about 700 (a cons list, `?(head, tail)`) or nested arrays
//! of about 1,500 ran out of stack. In wasm that is a trap, and a trap leaves
//! the instance unusable. A function holding such a list crossed as its
//! printed form, and the printer overflowed sooner, at about 400.
//!
//! The answers here are checked as text: serde_json's own parser stops at a
//! depth of 128, far short of these.

use fumola_wasm::*;

const DEPTH: usize = 100_000;

const DEFS: &str = "func build(n, acc) { if (n == 0) { acc } else { build(n - 1, ?(n, acc)) } }; \
                    func nest(n, acc) { if (n == 0) { acc } else { nest(n - 1, [acc]) } }; 0";

/// The answer from its "ok" on: a run's `counts` come first.
fn answer(r: &str) -> &str {
    &r[r.find("\"ok\":").unwrap_or(0)..]
}

fn instance() -> FumolaInstanceId {
    let id = fumola_create();
    let r = fumola_eval_top(id, DEFS);
    assert!(
        answer(&r).starts_with("\"ok\":true"),
        "definitions failed: {}",
        r
    );
    id
}

/// The instance still answers after the deep one.
fn still_works(id: FumolaInstanceId) {
    let r = fumola_eval_top(id, "1 + 1");
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"Int\",\"value\":\"2\"}"),
        "the instance stopped answering: {}",
        &r[..r.len().min(300)]
    );
}

#[test]
fn an_option_list_a_hundred_thousand_deep_is_an_answer() {
    let id = instance();
    let r = fumola_eval_top(id, &format!("build({}, null)", DEPTH));
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"Option\",\"value\":{\"tag\":\"Tuple\",\"value\":[{\"tag\":\"Int\""),
        "{}",
        &r[..r.len().min(300)]
    );
    assert_eq!(r.matches("\"tag\":\"Option\"").count(), DEPTH);
    assert!(r.contains("{\"tag\":\"Null\",\"value\":null}"));
    still_works(id);
}

#[test]
fn arrays_a_hundred_thousand_deep_are_an_answer() {
    let id = instance();
    let r = fumola_eval_top(id, &format!("nest({}, [])", DEPTH));
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"List\",\"value\":[{\"tag\":\"List\""),
        "{}",
        &r[..r.len().min(300)]
    );
    // the empty array at the bottom, and one around it at every level
    assert_eq!(r.matches("\"tag\":\"List\"").count(), DEPTH + 1);
    still_works(id);
}

#[test]
fn a_function_holding_a_deep_list_answers_without_printing_it() {
    let id = instance();
    let r = fumola_eval_top(
        id,
        &format!("let held = build({}, null); func f(k) {{ held }}", DEPTH),
    );
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"Opaque\""),
        "{}",
        &r[..r.len().min(300)]
    );
    assert!(
        r.contains("too deep to print"),
        "{}",
        &r[..r.len().min(300)]
    );
    still_works(id);
}

#[test]
fn a_shallow_function_is_still_printed() {
    let id = instance();
    let r = fumola_eval_top(id, "let held = build(3, null); func f(k) { held }");
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"Opaque\",\"value\":\"@func"),
        "{}",
        r
    );
    assert!(!r.contains("too deep to print"), "{}", r);
}

#[test]
fn a_deep_list_held_in_the_instance_is_fine() {
    let id = instance();
    let r = fumola_eval_top(id, &format!("let held = build({}, null); 0", DEPTH));
    assert!(
        answer(&r).starts_with("\"ok\":true"),
        "{}",
        &r[..r.len().min(300)]
    );
    still_works(id);
}
