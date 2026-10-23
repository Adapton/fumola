//! The traps found writing Messed Up Dress Up's room rules in Fumola (Matthew's
//! Hazel reports, ^report(28), "Traps, found translating"), each fixed and
//! held here: text `!=`, literal patterns of every kind, whole numbers kept
//! one way (0 and up a Nat, below 0 an Int) with every operation and
//! comparison defined between them, Floats and text in order, and escapes in
//! text literals. And the one that was not a trap: a variant's tuple payload
//! takes single parentheses.

use fumola_wasm::*;

/// The answer from its "ok" on: a run's `counts` come first.
fn answer(r: &str) -> &str {
    &r[r.find("\"ok\":").unwrap_or(0)..]
}

fn run(text: &str) -> String {
    let id = fumola_create();
    fumola_eval_top(id, text)
}

/// Each of these programs answers `true`.
fn all_true(cases: &[&str]) {
    for c in cases {
        let r = run(c);
        assert!(
            answer(&r).starts_with("\"ok\":true,\"tag\":\"Bool\",\"value\":true"),
            "{} answered {}",
            c,
            &r[..r.len().min(300)]
        );
    }
}

#[test]
fn text_not_equal_is_not_inverted() {
    all_true(&[
        r#""a" != "b""#,
        r#"("a" != "a") == false"#,
        r#"("" != "") == false"#,
    ]);
}

#[test]
fn not_equal_is_the_opposite_of_equal_for_every_kind() {
    // (each pair, both ways: != must say the opposite of ==)
    let vals = [
        "0", "4", "(0 - 4)", "((2 - 6) * (0 - 1))", "0.5", "1.0", r#""a""#, r#""""#, "true", "false",
        "null", "()", "#A", "#B(1)", "(1, 2)", "[1, 2]", "?1",
    ];
    for a in vals {
        for b in vals {
            all_true(&[&format!("({a} != {b}) == (({a} == {b}) == false)")]);
        }
    }
}

#[test]
fn literal_patterns_of_every_kind_match() {
    all_true(&[
        r#"switch ("E") { case "E" { true }; case _ { false } }"#,
        r#"switch ("W") { case "E" { false }; case "W" { true }; case _ { false } }"#,
        "switch (true) { case false { false }; case true { true } }",
        "switch (0 - 1) { case (-1) { true }; case _ { false } }",
        "switch (0.5) { case 0.5 { true }; case _ { false } }",
        "switch (16) { case 0x10 { true }; case _ { false } }",
        "switch (1000) { case 1_000 { true }; case _ { false } }",
        "switch (null) { case null { true }; case _ { false } }",
        "switch (()) { case () { true }; case _ { false } }",
        // (and a literal of another kind does not match)
        r#"switch (1) { case "1" { false }; case _ { true } }"#,
    ]);
}

#[test]
fn whole_numbers_mix_and_are_kept_one_way() {
    all_true(&[
        "(3 - 5) + 1 == 0 - 1",
        "1 + (3 - 5) == 0 - 1",
        "(0 - 2) * (0 - 3) == 6",
        "(0 - 7) / 2 == 0 - 3",
        "(0 - 7) % 2 == 0 - 1",
        "(0 - 2) <= 1",
        "1 > (0 - 2)",
        "(0 - 2) >= (0 - 2)",
        // (an Int that comes to 4 is the Nat 4: equal, and the same map key)
        "((2 - 6) * (0 - 1)) == 4",
        "switch (HashMap.get(HashMap.put(HashMap.new(), (2 - 6) * (0 - 1), true).0, 4)) { case (?x) { x }; case _ { false } }",
        "0 - 0 == 0",
    ]);
    // (a sum of numbers is a number, never a symbolic expression left unevaluated)
    let r = run("(3 - 5) + 1");
    assert!(
        answer(&r).starts_with("\"ok\":true,\"tag\":\"Int\",\"value\":\"-1\""),
        "{}",
        r
    );
}

#[test]
fn floats_and_text_have_an_order() {
    all_true(&[
        "0.25 < 0.5",
        "0.5 <= 0.5",
        "0.75 > 0.5",
        "(0.25 >= 0.5) == false",
        "1 < 1.5",
        "(0 - 1) < 0.5",
        "0.5 + (0 - 1) == 0 - 0.5",
        r#""a" < "b""#,
        r#""ab" > "a""#,
        r#"("b" <= "a") == false"#,
    ]);
}

#[test]
fn text_literals_read_their_escapes() {
    let r = run(r#""a\"b""#);
    assert!(answer(&r).contains(r#""value":"a\"b""#), "{}", r);
    let r = run(r#""a\\b""#);
    assert!(answer(&r).contains(r#""value":"a\\b""#), "{}", r);
    let r = run(r#""a\nb""#);
    assert!(answer(&r).contains(r#""value":"a\nb""#), "{}", r);
    all_true(&[r#""\u{41}" == "A""#, r#""a\tb" != "a\\tb""#]);
}

#[test]
fn a_tuple_payload_takes_single_parentheses() {
    all_true(&[
        "#G(1, 2) == #G((1, 2))",
        "switch (#G(1, 2, #D)) { case (#G(a, b, #D)) { a + b == 3 }; case _ { false } }",
    ]);
}
