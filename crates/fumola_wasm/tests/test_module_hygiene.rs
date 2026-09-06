//! A module body must not see the host's top-level bindings.
//!
//! The host binds the prelude -- `pointer`, `get`, `peek`, `print` -- at the
//! top level of every instance, before the library is imported. Module bodies
//! used to capture that environment as they were elaborated, and `var_step`
//! consults the captured environment before the lexical definitions, so those
//! four names silently outranked a module's own functions of the same name.
//!
//! These run against the wasm host because it is the one that binds a prelude
//! and imports the library at construction, which is the shape the bug needs.
//! They are deliberately NOT `#[test]` functions in `.fumola`: the Fumola test
//! runner calls a function with an empty environment
//! (`Core::call_function_def`), so it cannot observe this class of bug at all
//! -- `fumola test` reported 28 passed on the very function that crashed when
//! a program called it.

use fumola_wasm::*;

fn eval_top(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    serde_json::from_str(&fumola_eval_top(id, src)).expect("eval returned invalid JSON")
}

fn eval_top_ok(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    let v = eval_top(id, src);
    assert_eq!(v["ok"], serde_json::json!(true), "eval failed: {}", v);
    v
}

/// The original symptom, end to end.
///
/// `hashMap.fumola:31` reads `assert (get(m, 1) == ?1)`, meaning its own
/// two-argument `get`. With the prelude's one-argument `get(s)` captured, the
/// whole `(m, 1)` went to `adaptonPointer` as a tuple and the call died with a
/// TypeMismatch.
#[test]
fn a_module_calls_its_own_get_not_the_prelude_s() {
    let id = fumola_create();
    eval_top_ok(id, r#"import H "fumola/collections/hashMap"; H.Test.testAll()"#);
}

/// The cleanest statement of the bug, because it holds the source constant.
///
/// `fumola/system/hashMap.fumola` is a symlink to
/// `../collections/hashMap.fumola`, so one file is registered under two paths
/// -- and those two paths fall on opposite sides of the prelude binding. The
/// `system/` copy is elaborated inside the prelude's own import, before the
/// names exist; the `collections/` copy is elaborated afterwards. Identical
/// bytes, and they used to disagree: `system/` returned unit and
/// `collections/` crashed. Load position was the only variable.
#[test]
fn the_same_source_behaves_the_same_under_both_of_its_paths() {
    let id = fumola_create();
    let system = eval_top(id, r#"import H "fumola/system/hashMap"; H.Test.testAll()"#);
    let collections = eval_top(id, r#"import H "fumola/collections/hashMap"; H.Test.testAll()"#);
    assert_eq!(
        system["ok"], collections["ok"],
        "one file, two registered paths, two different answers:\n  system/      {}\n  collections/ {}",
        system, collections
    );
    assert_eq!(system["ok"], serde_json::json!(true), "both failed: {}", system);
}

/// Shadowing a prelude name at the top level does not reach into a module
/// either. The capture happened once, when the module was elaborated; this
/// pins the other direction, that a later binding cannot poison it after the
/// fact.
#[test]
fn a_later_top_level_binding_does_not_reach_into_a_module() {
    let id = fumola_create();
    eval_top_ok(id, r#"let get = func (anything) { 999 }"#);
    eval_top_ok(id, r#"import H "fumola/collections/hashMap"; H.Test.testAll()"#);
}

/// The prelude still does its job for programs, which is the whole point of
/// binding it. Making modules hermetic must not take that away.
#[test]
fn the_prelude_still_works_at_the_top_level() {
    let id = fumola_create();
    let v = eval_top_ok(id, "`cell := 41; get(`cell)");
    assert_eq!(v["value"], serde_json::json!("41"), "got {}", v);
}

/// A program that prints keeps its output no matter how deep the value it
/// returns is.
///
/// The reply used to be reparsed to attach `printed`, and serde's default
/// recursion limit is 128. A scene value nests past 200, so exactly the
/// programs whose printed output is most wanted -- the ones building a big
/// structure -- were the ones that lost it, silently.
#[test]
fn printed_output_survives_a_deeply_nested_result() {
    let id = fumola_create();
    // Deliberately size 44: at 16 the reply nests to about 122, just under
    // serde's default limit of 128, and the old code passed. This one nests
    // past 200.
    let raw = fumola_eval_top(
        id,
        r#"import M "fumola/examples/mergeSort/mergeSort";
           print("before the scene");
           M.generateSceneFullDemand(10, 44, null).sceneData"#,
    );
    // Checked on the raw reply rather than a parsed one, because
    // `serde_json::from_str` cannot read this reply either -- the same
    // recursion limit, in the harness this time. A browser's `JSON.parse`
    // has no such limit, so the reply is usable where it is actually used;
    // it is Rust's parser that needs opting out, and a test does not need to.
    assert!(
        raw.contains(r#""printed":["before the scene"]"#),
        "printed output lost from a deep reply; it begins: {}",
        &raw[..160.min(raw.len())]
    );
    assert!(raw.contains(r#""ok":true"#), "the program itself failed");
}
