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

/// A module reaching a sibling directory by climbing out of its own.
///
/// This is what the library's symlinks stood in for: before `..` resolved,
/// `fumola/examples/mergeSort/mergeSort` could only import a module registered
/// beside it, so each dependency needed a copy in its directory.
#[test]
fn import_climbs_out_of_its_own_directory() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "lib/collections/List.fumola".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module List");

    core.set_module(
        None,
        "lib/examples/app.fumola".to_string(),
        "import L \"../collections/List\"; module { public func f () { L.f() } }",
    )
    .expect("set_module app");

    assert_eq!(
        core.eval("import A \"lib/examples/app\"; A.f()"),
        fumola::eval::eval("#ok")
    );
}

/// Two `..` in a row, from a directory two levels down -- the shape
/// `examples/mergeSort/` actually needs.
#[test]
fn import_climbs_out_of_two_directories() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "fumola/system/adapton.fumola".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module adapton");

    core.set_module(
        None,
        "fumola/examples/mergeSort/mergeSort.fumola".to_string(),
        "import A \"../../system/adapton\"; module { public func f () { A.f() } }",
    )
    .expect("set_module mergeSort");

    assert_eq!(
        core.eval("import M \"fumola/examples/mergeSort/mergeSort\"; M.f()"),
        fumola::eval::eval("#ok")
    );
}

/// A module climbing out and back into its own directory reaches itself, which
/// is a cycle -- so `..` is resolved before the map is consulted, not treated
/// as part of the name.
#[test]
fn a_path_that_climbs_back_to_itself_is_a_cycle() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "d/M.fumola".to_string(),
        "import M \"../d/M\"; module { }",
    )
    .expect("set_module");

    let path = ModulePath {
        package_name: None,
        local_path: "d/M".to_string(),
    };
    let stack = vector![path.clone(), path.clone()];
    assert_eq!(
        core.eval("import M \"d/M\"; 0"),
        Err(Interruption::ImportCycle(stack).into())
    );
}

/// `.` and a redundant `..` name the module beside the importer, as they do in
/// a filesystem.
#[test]
fn dot_and_a_redundant_dot_dot_resolve_locally() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "d/N.fumola".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module N");

    core.set_module(
        None,
        "d/M.fumola".to_string(),
        "import A \"./N\"; import B \"../d/N\"; module { public func f () { (A.f(), B.f()) } }",
    )
    .expect("set_module M");

    assert_eq!(
        core.eval("import M \"d/M\"; M.f()"),
        fumola::eval::eval("(#ok, #ok)")
    );
}

/// Registered under one normal form, so the spelling a host happens to use on
/// the command line does not decide whether an import finds the module.
#[test]
fn a_module_registered_with_a_redundant_path_is_still_found() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "./d/x/../N.fumola".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module");

    assert_eq!(
        core.eval("import N \"d/N\"; N.f()"),
        fumola::eval::eval("#ok")
    );
}

/// A `..` with nowhere left to climb is reported as the program wrote it,
/// rather than quietly naming some other module.
#[test]
fn climbing_past_the_root_reports_the_path_as_written() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "M.fumola".to_string(),
        "import X \"../outside\"; module { }",
    )
    .expect("set_module");

    assert_eq!(
        core.eval("import M \"M\"; 0"),
        Err(Interruption::ModuleFileNotFound(ModulePath {
            package_name: None,
            local_path: "../outside".to_string(),
        })
        .into())
    );
}

/// A package path resolves `..` within the package.
#[test]
fn import_climbs_within_a_package() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        Some("p".to_string()),
        "collections/List".to_string(),
        "module { public func f () { #ok } }",
    )
    .expect("set_module");

    let id = ActorId::Alias("A".to_id());

    core.set_actor(
        format!("{}:{}", file!(), line!()),
        id.clone(),
        "import L \"mo:p/examples/../collections/List\"; actor { public func f () { L.f() } }",
    )
    .expect("set_actor");

    assert_eq!(
        core.call(&id, &"f".to_id(), ().to_motoko().unwrap().share()),
        fumola::eval::eval("#ok")
    );
}

/// After a nested import finishes, the importer's *own* directory is what its
/// remaining imports resolve against.
///
/// The restore read the bottom of the import stack rather than the top, so
/// once a module's nested import came and went, its later imports resolved
/// against the outermost module's directory instead of its own. Nothing
/// noticed while a module could only import what sat beside it -- the library
/// kept a symlinked copy of each dependency in every directory that used one,
/// so the wrong directory held the right file. Three levels are the smallest
/// arrangement that tells the bottom of the stack from the top: with two, the
/// only entry left after the pop is both.
#[test]
fn a_nested_import_restores_the_importer_s_directory() {
    let mut core = fumola::state::State::empty();

    core.set_module(
        None,
        "app/outer.fumola".to_string(),
        "import M \"../lib/mid\"; module { public func f () { M.f() } }",
    )
    .expect("set_module outer");

    core.set_module(
        None,
        "lib/mid.fumola".to_string(),
        // `two` is imported only after `inner` has come and gone, so it is
        // resolved with whatever directory the restore left behind. It is a
        // sibling of `mid`, in "lib"; the outermost module lives in "app".
        "import I \"inner\"; import T \"two\";
         module { public func f () { (I.f(), T.f()) } }",
    )
    .expect("set_module mid");

    core.set_module(
        None,
        "lib/inner.fumola".to_string(),
        "module { public func f () { #inner } }",
    )
    .expect("set_module inner");

    core.set_module(
        None,
        "lib/two.fumola".to_string(),
        "module { public func f () { #two } }",
    )
    .expect("set_module two");

    assert_eq!(
        core.eval("import O \"app/outer\"; O.f()"),
        fumola::eval::eval("(#inner, #two)")
    );
}
