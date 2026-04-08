use fumola::check::assert_vm_eval as assert_;

#[test]
fn graphical_is_default() {
    assert_(
        "let graphical = switch(@(`adapton(`state))) { case(#Graphical(g)) g }; ()",
        "()",
    )
}

#[test]
fn test_events() {
    assert_(
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
