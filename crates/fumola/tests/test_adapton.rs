use fumola::check::assert_vm_eval as assert__;
//use fumola::check::assert_vm_eval_result_line as assert__;

// Run each of two caching strategies for each assert in the tests below.
fn assert_(program: &str, expected_result: &str) {
    let program1 = format!("prim \"adaptonReset\" ({}); {}", "#simple", program);
    assert__(program1.as_str(), expected_result);

    let program2 = format!("prim \"adaptonReset\" ({}); {}", "#graphical", program);
    assert__(program2.as_str(), expected_result);
}

#[test]
fn reset_graphical() {
    assert_(
        "let p = 1 := (); prim \"adaptonReset\" (#graphical); prim \"adaptonPeek\" p",
        "null",
    )
}

#[test]
fn reset_simple() {
    assert_(
        "let p = 1 := (); prim \"adaptonReset\" (#simple); prim \"adaptonPeek\" p",
        "null",
    )
}

#[test]
fn force_thunk() {
    assert_("force (thunk {1 + 2})", "3")
}

#[test]
fn force_simple_cache_hit() {
    assert_("let count = 1 := 0; let myThunk = 2 := thunk { let orig = @ count; count := 1 + (@ count); orig }; force(myThunk); (@ count, force(myThunk))", "(1, 0)")
}

#[test]
fn peek_cell_some_result() {
    assert__("prim \"adaptonReset\"(#simple); let p = 1 := thunk { }; force(p); let node = (prim \"adaptonPeekCell\" p)!; switch(node){ case(#thunk_(t)){t.result} }", "?()");
    assert__("let p = 1 := thunk { }; force(p); let node = (prim \"adaptonPeekCell\" p)!.node; switch(node){ case(#thunk_(t)){t.result} }", "?(3, ())")
}

#[test]
fn peek_cell_null_result() {
    assert__("prim \"adaptonReset\"(#simple); let p = 1 := thunk { }; let node = (prim \"adaptonPeekCell\" p)!; switch(node){ case(#thunk_(t)){t.result} }", "null");
    assert__("let p = 1 := thunk { }; let node = (prim \"adaptonPeekCell\" p)!.node; switch(node){ case(#thunk_(t)){t.result} }", "null")
}

#[test]
fn get_put() {
    assert_("@(1 := 1)", "1")
}

#[test]
fn peek_put() {
    assert_("prim \"adaptonPeek\" (1 := 1)", "?1")
}

#[test]
fn get_poke() {
    assert_(
        "@(prim \"adaptonPoke\" (prim \"adaptonPointer\" 1, 1))",
        "1",
    )
}

#[test]
fn thunk_saves_space() {
    assert_(
        "let t = do goto space `space { `t := thunk{ prim \"adaptonHere\" () } }; force(t)",
        "prim \"adaptonSpace\" `space",
    )
}

#[test]
fn do_within_time() {
    assert_(
        "do goto time 1 { prim \"adaptonNow\" () }",
        "prim \"adaptonTime\" 1",
    )
}

#[test]
fn do_within_space() {
    assert_(
        "do goto space 1 { prim \"adaptonHere\" () }",
        "prim \"adaptonSpace\" 1",
    )
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
fn two_puts_get_relevant_time_now() {
    assert_("let p = 0 := 0; do goto time `t { 0 := 1 }; @ p", "0")
}

#[test]
fn two_puts_get_relevant_time_later() {
    assert_(
        "let p = 0 := 0; do goto time 1 { 0 := 1 }; do goto time 2 { @ p }",
        "1",
    )
}

#[test]
fn two_puts_get_relevant_time_pair() {
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
fn pointer_identity() {
    assert_("`x := ()", "prim \"adaptonPointer\" `x");
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
fn get_settings() {
    assert_("@ (`adapton(`settings)(`forceBeginAlwaysMisses))", "false");
    assert_("@ (`adapton(`settings)(`forceEndForgetsResult))", "false");
}

#[test]
fn get_put_count() {
    assert_("1 := 1; @ (`adapton(`counts)(`put))", "1");
}

#[test]
fn put_settings() {
    assert_("`adapton(`settings)(`forceBeginAlwaysMisses) := true; @ (`adapton(`settings)(`forceBeginAlwaysMisses))", "true");
    assert_("`adapton(`settings)(`forceEndForgetsResult) := true; @ (`adapton(`settings)(`forceEndForgetsResult))", "true");
}

#[test]
fn get_cell_counts() {
    assert_("@(`adapton(`counts)(`thunkCells))", "0");
    assert_("@(`adapton(`counts)(`nonThunkCells))", "0");
    assert_("0 := thunk { }; @(`adapton(`counts)(`thunkCells))", "1");
    assert_("0 := (); @(`adapton(`counts)(`nonThunkCells))", "1")
}

#[test]
fn get_state() {
    assert_(
        "switch (@ (`adapton(`state))) { case (#Simple(_)) (); case (#Graphical(_)) () }",
        "()",
    )
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
        r#"let start = (prim "adaptonNow") ();
let cell = 0 := null;
while ((@ cell) == null) { 
  do goto time `t { 
    assert ((prim "adaptonNow" ()) == (prim "adaptonTime" `t));
    assert ((@ cell) == null);
    (cell, start) := ?1;
    assert ((@ cell) == null)
  };
  assert ((prim "adaptonNow" ()) == start);
};
assert ((@ cell) == ?1)
"#,
        "()",
    )
}

#[test]
fn test_events() {
    assert__(
        r#"
    force (1 := thunk { force (2 := thunk { 44 }) });
    (switch (@(`adapton(`state))) { case (#Graphical(g)) g }).events"#,
        r#"
    [
        {event = #AddNode(#Symbol(#Nat(1)), #Now, [1]); meta_time = [1]},
        {event = #AddEdge([1001]); meta_time = [1]},
        {event = #ForceBegin(#Symbol(#Nat(1)), #Now, [1]); meta_time = [2]},
        {event = #AddNode(#Symbol(#Nat(2)), #Now, [3]); meta_time = [3]},
        {event = #AddEdge([1002]); meta_time = [3]},
        {event = #ForceBegin(#Symbol(#Nat(2)), #Now, [3]); meta_time = [4]},
        {event = #AddEdge([1003]); meta_time = [5]},
        {event = #ForceEnd((#Symbol(#Nat(2)), #Now, [3]), [1003]); meta_time = [5]},
        {event = #AddEdge([1004]); meta_time = [6]},
        {event = #ForceEnd((#Symbol(#Nat(1)), #Now, [1]), [1004]); meta_time = [6]}
    ]"#,
    );
}
