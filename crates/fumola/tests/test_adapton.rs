use fumola::check::assert_vm_eval as assert_;

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