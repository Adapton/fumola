use fumola::check::assert_vm_eval as assert_;

#[test]
fn force_thunk() {
    assert_("force (thunk {1 + 2})", "3")
}

#[test]
fn get_put() {
    assert_("@(1 := 1)", "1")
}

#[test]
fn do_goto_time_get_put() {
    assert_("do goto time 1 { @(1 := 1) }", "1")
}

#[test]
fn do_within_time_get_put() {
    assert_("do within time 1 { @(1 := 1) }", "1")
}

#[test]
fn do_goto_space_get_put() {
    assert_("do goto space 1 { @(1 := 1) }", "1")
}

#[test]
fn do_within_space_get_put() {
    assert_("do within space 1 { @(1 := 1) }", "1")
}

#[test]
fn force_put_thunk() {
    assert_("force (1 := thunk { 1 })", "1")
}

#[test]
fn do_goto_time_force_put_thunk_time() {
    assert_("do goto time 1 { force (1 := thunk { 1 }) }", "1")
}

#[test]
fn goto_space_force_put_thunk_time() {
    assert_("do goto space 1 { force (1 := thunk { 1 }) }", "1")
}

#[test]
fn get_relevant_time_now() {
    assert_("let p = 0 := 0; do goto time `t { 0 := 1 }; @ p", "0")
}

#[test]
fn get_relevant_time_later() {
    assert_(
        "let p = 0 := 0; do goto time 1 { 0 := 1 }; do goto time 2 { @ p }",
        "1",
    )
}

#[test]
fn get_relevant_time_pair() {
    assert_(
        "let p = 0 := 0; do goto time 1 { 0 := 1 }; 0 := 2; (@ p, do goto time 1 { @ p })",
        "(2, 1)",
    )
}

#[test]
fn symbol_identity() {
    assert_("1-`x == (1)-(`x)", "true");
    assert_("1+`x == (1)+(`x)", "true");
    assert_("1(`x) == 1(`x)", "true");
    assert_("-(`x) == -(`x)", "true");
    assert_("+(`x) == +(`x)", "true");
}

#[test]
fn here() {
    assert_(
        "do goto space `x { prim \"adaptonHere\" () }",
        "prim \"adaptonSpace\" `x",
    );
}

#[test]
fn now() {
    assert_(
        "do goto time `x { prim \"adaptonNow\" () }",
        "prim \"adaptonTime\" `x",
    );
}

#[test]
fn symbol_ordering() {
    assert_("1-`x == 1-`x", "true");
    assert_("1-`x == 2-`x", "false");

    assert_("1-`x <  2-`x", "true");
    assert_("2-`x <  1-`x", "false");

    assert_("1-`x <= 2-`x", "true");
    assert_("2-`x <= 1-`x", "false");

    assert_("2-`x >  1-`x", "true");
    assert_("1-`x >  2-`x", "false");

    assert_("2-`x >= 1-`x", "true");
    assert_("1-`x >= 2-`x", "false");

    // distinct quoted ASTs are not comparable, expressing time-independence (parallelism/concurrency):
    assert_("`a-`x >= `b-`x", "false");
    assert_("`b-`x >= `a-`x", "false");

    assert_("`b-`x >= `b-`x", "true");
    assert_("`a-`x >= `a-`x", "true");
}

#[test]
fn delayed_put() {
    assert_(
        r#"
  let t = (prim "adaptonNow") ();
  let cell = 0 := null;
  do goto time `t { 
    (cell, t) := ?1;
    assert ((@ cell) == null)
  };
  assert ((@ cell) == ?1)
"#,
        "()",
    )
}
