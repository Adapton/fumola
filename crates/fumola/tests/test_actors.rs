use fumola::ast::ToId;
use fumola::check::assert_vm_eval as assert_;
use fumola::check::assert_vm_interruption as assert_x;
use fumola::value::ActorId;
use fumola::vm_types::{Interruption, LocalPointer, NumericPointer, Pointer, ScheduleChoice};

use test_log::test; // enable logging output for tests by default.

#[test]
fn actor_a_public_func_f() {
    let p = "
    actor A { public func f () { } };
    A.f()";
    assert_(p, "()");
}

#[test]
fn actor_a_public_func_f_137() {
    let p = "
    let x = 137;
    actor A { public func f () { x } };
    A.f()";
    assert_(p, "137");
}

#[test]
fn actor_a_public_func_f_dangling() {
    let p = Pointer {
        local: LocalPointer::Numeric(NumericPointer(0)),
        owner: ScheduleChoice::Agent,
    };
    let i = Interruption::NotOwner(p);
    let p = "
    var x = 137;
    actor A { public func f () { x } };
    A.f()";
    assert_x(p, &i)
}

#[test]
fn actor_a_public_func_f_g() {
    let p = "
    let x = 137;
    actor A { public func f () { x };
              public func g () { f() } };
    A.g()";
    assert_(p, "137");
}

#[test]
fn actor_forward_def_func_g() {
    let p = "
    let x = 137;
    actor A { public func f () { g() };
              public func g () { x }; };
    A.f()";
    assert_(p, "137");
}

#[test]
fn actor_a_private_func_f_fail() {
    let i = Interruption::ActorFieldNotPublic(ActorId::Local("A".to_id()), "f".to_id());
    let p = "
    actor A { func f () { } };
    A.f()";
    assert_x(p, &i);
}

#[test]
fn actors_a_missing_func_f_fail() {
    let i = Interruption::ActorFieldNotFound(ActorId::Local("A".to_id()), "f".to_id());
    let p = "
    actor A { };
    A.f()";
    assert_x(p, &i);
}

#[test]
fn counter_inc_twice() {
    let p = "
    let y = 999;
    let x = 666;
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 1 };
    };
    assert (Counter.get() == 0);
    Counter.inc();
    assert (Counter.get() == 1);
    Counter.inc();
    assert (Counter.get() == 2);
    assert (y == 999);
    assert (x == 666);
    #ok";
    assert_(p, "#ok");
}

#[test]
fn actor_upgrade_demo_with_counter_inc() {
    let p = "
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 1 };
    };
    assert (Counter.get() == 0);
    Counter.inc();
    assert (Counter.get() == 1);
    actor Counter {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 2 };
    };
    assert (Counter.get() == 1);
    Counter.inc();
    assert (Counter.get() == 3);
    #ok";
    assert_(p, "#ok");
}

#[test]
fn actors_a_b_public_func_f_g() {
    // Actor A is forward-declared,
    // then B is defined using A's future API,
    // then A is defined, exposing API used by B.
    let p = "
    actor A { };
    actor B { public func f() { A.g() } };
    actor A { public func g() { #ok } };
    B.f()";
    assert_(p, "#ok");
}

#[test]
fn actors_a_b_public_func_f_g_fail() {
    let i = Interruption::UnboundIdentifer("A".to_id());

    // Actor A is not defined at all.
    let p = "
    actor B { public func f() { A.g() } };
    B.f()";
    assert_x(p, &i);
}

#[test]
fn actors_a_b_public_func_f_g_forward_dec() {
    // Actor A is defined after Actor B, but we permit that using an
    // open-ended top-level context (each actor can see new
    // definitions in the top level context).
    let p = "
    actor B { public func f() { A.g() } };
    actor A { public func g() { #ok } };
    B.f()";
    assert_(p, "#ok");
}

#[test]
fn actor_forward_decl() {
    let p = "
    actor A { public func f () { g() }; public func g() { #ok } };
    A.f()";
    assert_(p, "#ok");
}

#[test]
fn actor_upgrade_fail() {
    let p = "
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 1 };
    };
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc2() { x := x + 1 };
      public func inc() { inc2() };
    };
    Counter.inc();
    #ok";
    assert_(p, "#ok");
}

#[test]
fn actor_upgrade_field_ordering1() {
    let p = "
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 1 };
    };
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc2() { x := x + 1 };
      public func inc() { inc2() };
    };
    Counter.inc();
    #ok";
    assert_(p, "#ok");
}

#[test]
fn actor_upgrade_field_ordering2() {
    let p = "
    actor Counter = {
      var x = 0;
      public func get() : async Nat { x };
      public func inc() { x := x + 1 };
    };
    actor Counter = {
      public func get() : async Nat { x };
      public func inc() { inc2() };
      public func inc2() { x := x + 1 };
      var x = 0;
    };
    #ok";
    assert_(p, "#ok");
}

#[test]
fn actor_uses_global_module() {
    let p = "
    module M {
      public func f () { #ok };
    };
    actor A = {
      public func g() { M.f() };
    };
    A.g()";
    assert_(p, "#ok");
}
