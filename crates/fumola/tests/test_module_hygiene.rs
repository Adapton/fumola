//! A module body must not see the host's top-level bindings.
//!
//! A host binds the prelude -- `pointer`, `get`, `peek`, `print` -- at the top
//! level, and then programs import library modules. Module bodies used to
//! capture that environment as they were elaborated, and `var_step` consults
//! the captured environment before the lexical definitions, so those four
//! names silently outranked a module's own functions of the same name. The
//! original symptom was `hashMap`'s two-argument `get(m, 1)` reaching the
//! prelude's one-argument `get`, so the whole `(m, 1)` arrived at
//! `adaptonPointer` as a tuple.
//!
//! These build the situation rather than borrowing it from the library, which
//! is what the wasm tests of the same name used to do. That worked only while
//! `fumola/collections/hashMap` happened to be elaborated after the prelude
//! was bound -- true when the prelude reached `system/hashMap`, a symlink to
//! the same file, and no longer true now that there is one hashMap and the
//! prelude reaches it. A test whose teeth depend on which copy of a file the
//! prelude walks into is a test that can be disarmed by moving a file, and
//! this one cannot.
//!
//! They are deliberately not `#[test]` functions in `.fumola`: the Fumola test
//! runner calls a function with an empty environment
//! (`Core::call_function_def`), so it cannot observe this class of bug at all
//! -- `fumola test` reported 28 passed on the very function that crashed when
//! a program called it.

use fumola::prelude;

/// A host: the prelude's names bound at the top level, as the CLI and the
/// wasm hosts both do at startup.
fn host() -> fumola::state::State {
    let mut core = fumola::state::State::empty();
    core.set_module(
        None,
        prelude::PRELUDE_PATH.to_string(),
        // Stands in for `fumola/system/prelude.fumola` without dragging in
        // adapton: what matters here is that these four names are bound at
        // the top level before a module is elaborated, not what they do.
        "module {
           public func pointer(s) { s };
           public func get(s) { 1 };
           public func peek(s) { ?1 };
           public func print(x) { () };
         }",
    )
    .expect("register the prelude");
    core.eval(&prelude::binding()).expect("bind the prelude");
    core
}

/// The original symptom, in miniature.
///
/// The module's own `get` takes two arguments and the prelude's takes one, so
/// a capture is not a subtle difference in behaviour -- the call fails.
#[test]
fn a_module_calls_its_own_get_not_the_prelude_s() {
    let mut core = host();
    core.set_module(
        None,
        "d/hashMap.fumola".to_string(),
        "module {
           public func get(m, k) { (m, k) };
           public func use_() { get(7, 1) };
         }",
    )
    .expect("register the module");

    assert_eq!(
        core.eval(r#"import H "d/hashMap"; H.use_()"#),
        fumola::eval::eval("(7, 1)"),
        "the module's own two-argument `get` should win over the prelude's"
    );
}

/// The other direction: a name bound at the top level *after* a module was
/// elaborated cannot reach into it either.
#[test]
fn a_later_top_level_binding_does_not_reach_into_a_module() {
    let mut core = host();
    core.set_module(
        None,
        "d/hashMap.fumola".to_string(),
        "module {
           public func get(m, k) { (m, k) };
           public func use_() { get(7, 1) };
         }",
    )
    .expect("register the module");

    core.eval(r#"import H "d/hashMap"; H.use_()"#)
        .expect("elaborate the module first");
    core.eval("let get = func (anything) { 999 }")
        .expect("shadow it afterwards");

    assert_eq!(
        core.eval(r#"import H "d/hashMap"; H.use_()"#),
        fumola::eval::eval("(7, 1)"),
        "a later top-level binding should not reach back into the module"
    );
}

/// One module importing another does not leak its own scope in either, so the
/// hermetic body is a property of every module, not only the first one a host
/// touches.
#[test]
fn an_imported_module_does_not_see_its_importer_s_scope() {
    let mut core = host();
    core.set_module(
        None,
        "d/inner.fumola".to_string(),
        "module { public func get(m, k) { (m, k) }; public func use_() { get(7, 1) } }",
    )
    .expect("register inner");
    core.set_module(
        None,
        "d/outer.fumola".to_string(),
        r#"import I "inner";
           module {
             public func get(s) { 999 };
             public func use_() { I.use_() };
           }"#,
    )
    .expect("register outer");

    assert_eq!(
        core.eval(r#"import O "d/outer"; O.use_()"#),
        fumola::eval::eval("(7, 1)"),
        "outer's one-argument `get` should not reach into inner"
    );
}

/// Making modules hermetic must not take the prelude away from programs,
/// which is the whole point of binding it.
#[test]
fn the_prelude_still_works_at_the_top_level() {
    let mut core = host();
    assert_eq!(core.eval("get(`cell)"), fumola::eval::eval("1"));
}
