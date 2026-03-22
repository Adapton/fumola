use fumola::check::assert_vm_eval as assert_;

#[test]
fn graphical_is_default() {
    assert_(
        "let graphical = switch(@(`adapton(`state))) { case(#Graphical(g)) g }; ()",
        "()",
    )
}
