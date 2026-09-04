//! Round-trip tests for the Fumola livelit's external store.
//!
//! These run natively; nothing here needs a browser or a wasm host.

use fumola_wasm::*;

fn eval_ok(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    let raw = fumola_eval(id, src);
    let v: serde_json::Value = serde_json::from_str(&raw).expect("eval returned invalid JSON");
    assert_eq!(v["ok"], serde_json::json!(true), "eval failed: {}", raw);
    v
}

fn eval_int(id: FumolaInstanceId, src: &str) -> String {
    let v = eval_ok(id, src);
    assert_eq!(v["tag"], serde_json::json!("Int"));
    v["value"].as_str().expect("int value is a string").to_string()
}

#[test]
fn round_trip_one_plus_two() {
    let id = fumola_create();
    assert_eq!(eval_int(id, "1 + 2"), "3");
}

/// The MVP's success criterion: the same livelit, edited repeatedly, against
/// one persistent runtime.
#[test]
fn repeated_edits_reuse_one_instance() {
    let id = fumola_create();
    assert_eq!(eval_int(id, "1 + 2"), "3");
    assert_eq!(eval_int(id, "10 + 20"), "30");
    assert_eq!(eval_int(id, "7 * 6"), "42");
    // Editing back to an earlier program still works.
    assert_eq!(eval_int(id, "1 + 2"), "3");
    // All of that happened in a single runtime.
    assert!(fumola_has(id));
}

#[test]
fn instances_are_independent() {
    let a = fumola_create();
    let b = fumola_create();
    assert_ne!(a, b, "create must allocate fresh ids");
    assert_eq!(eval_int(a, "1 + 2"), "3");
    assert_eq!(eval_int(b, "100 + 1"), "101");
    // Editing b did not disturb a's runtime.
    assert_eq!(eval_int(a, "1 + 2"), "3");
}

#[test]
fn realize_recovers_a_dangling_id() {
    // The reload path: a saved program mentions an id this session never created.
    let saved_id: FumolaInstanceId = 4242;
    assert!(!fumola_has(saved_id));
    assert!(fumola_realize(saved_id), "should report a new realization");
    assert!(fumola_has(saved_id));
    assert!(!fumola_realize(saved_id), "second realize is a no-op");
    // Re-evaluating the saved text restores the model/sigma invariant.
    assert_eq!(eval_int(saved_id, "1 + 2"), "3");
    // A realized id must not later be handed out again by create.
    assert_ne!(fumola_create(), saved_id);
}

#[test]
fn dropping_an_instance_removes_it() {
    let id = fumola_create();
    assert_eq!(eval_int(id, "1 + 2"), "3");
    fumola_drop(id);
    assert!(!fumola_has(id));
    let raw = fumola_eval(id, "1 + 2");
    assert!(raw.contains("\"ok\":false"), "expected failure, got {}", raw);
}

#[test]
fn syntax_errors_are_reported_not_panicked() {
    let id = fumola_create();
    let raw = fumola_eval(id, "1 +");
    assert!(raw.contains("\"ok\":false"), "expected failure, got {}", raw);
    // The instance survives a bad edit and still evaluates afterwards.
    assert_eq!(eval_int(id, "1 + 2"), "3");
}

#[test]
fn non_integer_results_translate() {
    let id = fumola_create();
    let v = eval_ok(id, "true");
    assert_eq!(v["tag"], serde_json::json!("Bool"));
    assert_eq!(v["value"], serde_json::json!(true));
}
