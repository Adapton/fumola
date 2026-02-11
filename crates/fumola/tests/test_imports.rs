use fumola_syntax::ast::ToId;
use im_rc::vector;

//use motoko::check::assert_vm_eval as assert_;
use fumola_semantics::value::ActorId;
use fumola_semantics::vm_types::{Interruption, ModulePath};
use fumola_semantics::ToMotoko;
use fumola_syntax::shared::Share;
use test_log::test; // enable logging output for tests by default.

#[test]
fn import_cycle() {
    let mut core = fumola::state::State::empty();

    core.set_module(None, "M".to_string(), "import M \"M\"; module { }")
        .expect("set_module");

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
    let stack = vector![path.clone(), path.clone()];
    assert_eq!(r, Err(Interruption::ImportCycle(stack).into()))
}

#[test]
fn import_dag_size2() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "M".to_string(),
        "import N \"N\"; module { public func f () { N.f() } }",
    )
    .expect("set_module M");

    core.set_module(None, "N".to_string(), "module { public func f () { #ok } }")
        .expect("set_module N");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"M\"; actor { public func f () { M.f() } }",
    )
    .expect("set_actor");

    assert_eq!(
        core.call(&id, &"f".to_id(), ().to_motoko().unwrap().share()),
        fumola::eval::eval("#ok")
    );
}

#[test]
fn import_package_module() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        Some("p".to_string()),
        "M".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import M \"mo:p/M\"; actor { public func f () { M.f() } }",
    )
    .expect("set_actor");

    assert_eq!(
        core.call(&id, &"f".to_id(), ().to_motoko().unwrap().share(),),
        fumola::eval::eval("#ok")
    );
}
