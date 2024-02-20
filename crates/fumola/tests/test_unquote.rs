use test_log::test; // enable logging output for tests by default.

fn assert_is_value(v: &str) {
    assert_(v, v)
}

fn assert_(s1: &str, s2: &str) {
    use fumola::format::format_one_line as fmt;
    let v1 = fumola::vm::eval(s1);
    let v2 = fumola::vm::eval(s2);
    assert_eq!(fmt(v1.unwrap().as_ref()), fmt(v2.unwrap().as_ref()));
}

#[test]
fn quoted_empty() {
    assert_is_value("`()");
}

#[test]
fn unquote_quoted_tuple() {
    assert_("~(`(1, 2, 3))", "(1, 2, 3)");
}

#[test]
fn unquote_quoted_tuple_composition() {
    assert_("~(`() # `(1, 2) # `(3))", "(1, 2, 3)");
}

#[test]
fn unquote_quoted_object_composition() {
    assert_("~(`{} # `{x=1; y=2} # `{z=3})", "{z=3; y=2; x=1}");
}

#[test]
fn quoted_case() {
    assert_is_value("`{case _ 0}");
}

#[test]
fn switch_on_quoted_case_composition() {
    assert_(
        "let cases = `{case 0 0} # `{case 1 1}; (switch 0 ~cases, switch 1 ~cases)",
        "(0, 1)",
    );
}
