use fumola::eval::eval;
use fumola::state::State;
use fumola_semantics::value::ActorId;
use fumola_semantics::vm_types::{Interruption, ModulePath};
use fumola_semantics::ToMotoko;
use fumola_syntax::ast::ToId;
use fumola_syntax::shared::Share;

use test_log::test; // enable logging output for tests by default.

#[test]
fn core_set_actor_call() {
    let mut core = State::empty();
    let id = ActorId::Alias("Counter".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "
      actor {
        var x = 0;
        public func get() : async Nat { x };
        public func inc() { x := x + 1 };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share(),),
        eval("0")
    );

    assert_eq!(
        core.call(&id, &"inc".to_id(), ().to_motoko().unwrap().share(),),
        eval("()")
    );

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share(),),
        eval("1")
    );

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "
      actor {
        var x = 0;
        public func get() : async Nat { x };
        public func inc() { x := x + 2 };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share(),),
        eval("1")
    );

    assert_eq!(
        core.call(&id, &"inc".to_id(), ().to_motoko().unwrap().share(),),
        eval("()")
    );

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share()),
        eval("3")
    );
}

#[test]
fn core_set_actor_with_ambient_module() {
    let mut core = State::empty();

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "
      module M { public func f () { #ok } };
      actor {
        public func g() { M.f() };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(&id, &"g".to_id(), ().to_motoko().unwrap().share()),
        eval("#ok")
    );
}

#[test]
fn core_set_module_and_import_it() {
    let mut core = State::empty();

    core.set_module(None, "M".to_string(), "module { public func f () { #ok } }")
        .expect("set_module");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "
      import M \"M\";
      actor {
        public func g() { M.f() };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(&id, &"g".to_id(), ().to_motoko().unwrap().share()),
        eval("#ok")
    );
}

#[test]
fn not_a_module_definition() {
    let mut core = State::empty();
    let r = core.set_module(None, "M".to_string(), "137");
    assert_eq!(r, Err(Interruption::NotAModuleDefinition.into()))
}

#[test]
fn missing_module_definition() {
    let mut core = State::empty();
    let r = core.set_module(None, "M".to_string(), "");
    assert_eq!(r, Err(Interruption::MissingModuleDefinition.into()))
}

#[test]
fn module_file_not_found() {
    let mut core = State::empty();
    let id = ActorId::Alias("A".to_id());
    let r = core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"M\"; actor { }",
    );
    let path = ModulePath {
        package_name: None,
        local_path: "M".to_string(),
    };
    assert_eq!(r, Err(Interruption::ModuleFileNotFound(path).into()))
}

#[test]
fn core_set_actor_call_set_module() {
    let mut core = State::empty();
    let id = ActorId::Alias("Counter".to_id());

    core.set_module(
        None,
        "M".to_string(),
        "module { public func inc (x) { x + 1 } }",
    )
    .expect("set_module");

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "
      import M \"M\";
      actor {
        var x = 0;
        public func get() : async Nat { x };
        public func inc() { x := M.inc(x) };
    }",
    )
    .expect("create");

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share()),
        eval("0")
    );

    assert_eq!(
        core.call(&id, &"inc".to_id(), ().to_motoko().unwrap().share()),
        eval("()")
    );

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share()),
        eval("1")
    );

    core.set_module(
        None,
        "M".to_string(),
        "module { public func inc (x) { x + 2 } }",
    )
    .expect("set_module");

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share()),
        eval("1")
    );

    assert_eq!(
        core.call(&id, &"inc".to_id(), ().to_motoko().unwrap().share()),
        eval("()")
    );

    assert_eq!(
        core.call(&id, &"get".to_id(), ().to_motoko().unwrap().share()),
        eval("3")
    );
}
