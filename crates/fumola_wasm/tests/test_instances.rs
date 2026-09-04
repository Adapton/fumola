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

/// A livelit's program text is a Hazel string literal, and Hazel strings have
/// no escapes, so `prim "adaptonPointer"` cannot be written in one. The
/// prelude makes the adapton primitives reachable without any quotes.
#[test]
fn the_prelude_avoids_needing_quotes() {
    let id = fumola_create();
    // Nothing here contains a double quote.
    assert_eq!(eval_int(id, "1 := 2; get(1)"), "2");
}

/// `:=` coerces its left side into a pointer but `@` does not, so reading back
/// what `1 := 2` wrote is not `@(1)`. `get` does the conversion, which is what
/// lets state written in one edit be read in the next.
#[test]
fn state_written_in_one_edit_is_read_in_the_next() {
    let id = fumola_create();
    eval_ok(id, "1 := 2");
    assert_eq!(eval_int(id, "get(1)"), "2");

    // Overwrite in a third edit, and see the new value in a fourth.
    eval_ok(id, "1 := 7");
    assert_eq!(eval_int(id, "get(1)"), "7");
}

/// Symbols name cells here too, so a named cell survives edits the same way.
#[test]
fn named_cells_survive_edits() {
    let id = fumola_create();
    eval_ok(id, "`counter := 41");
    assert_eq!(eval_int(id, "get(`counter) + 1"), "42");
}

/// `peek` reads without recording a dependency, and answers null for a name
/// that was never written rather than failing.
#[test]
fn peek_reports_a_missing_cell() {
    let id = fumola_create();
    eval_ok(id, "1 := 2");
    // A name that was never written peeks as null...
    let raw = fumola_eval(id, "peek(404)");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], serde_json::json!(true), "peek failed: {}", raw);
    assert_eq!(v["tag"], serde_json::json!("Null"));

    // ...and one that was written peeks as an option carrying the value.
    let raw = fumola_eval(id, "peek(1)");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], serde_json::json!(true), "peek failed: {}", raw);
    assert_eq!(v["tag"], serde_json::json!("Option"));
    assert_eq!(v["value"], serde_json::json!({"tag":"Int","value":"2"}));
}

/// Two runtimes must not see each other's cells, even under the same name.
#[test]
fn cells_are_per_instance() {
    let a = fumola_create();
    let b = fumola_create();
    eval_ok(a, "1 := 2");
    eval_ok(b, "1 := 99");
    assert_eq!(eval_int(a, "get(1)"), "2");
    assert_eq!(eval_int(b, "get(1)"), "99");
}
