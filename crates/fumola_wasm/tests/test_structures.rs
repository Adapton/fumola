//! Structural translation: Fumola results reaching Hazel as Hazel values.
//!
//! The point is that a Fumola tuple arrives as a *tuple*, not as a wrapper
//! Hazel has to take apart.

use fumola_wasm::*;
use serde_json::json;

fn eval_value(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    let raw = fumola_eval(id, src);
    let v: serde_json::Value = serde_json::from_str(&raw).expect("invalid JSON");
    assert_eq!(v["ok"], json!(true), "eval failed: {}", raw);
    v
}

/// The motivating case: cells written by earlier edits, read back as a pair.
#[test]
fn a_pair_of_cells_reads_back_as_a_pair() {
    let id = fumola_create();
    eval_value(id, "1 := 10");
    eval_value(id, "2 := 20");

    let v = eval_value(id, "(get(1), get(2))");
    assert_eq!(v["tag"], json!("Tuple"));
    assert_eq!(
        v["value"],
        json!([
            {"tag": "Int", "value": "10"},
            {"tag": "Int", "value": "20"}
        ])
    );
}

#[test]
fn tuples_nest() {
    let id = fumola_create();
    let v = eval_value(id, "(1, (2, 3))");
    assert_eq!(v["tag"], json!("Tuple"));
    assert_eq!(v["value"][1]["tag"], json!("Tuple"));
    assert_eq!(v["value"][1]["value"][1], json!({"tag":"Int","value":"3"}));
}

#[test]
fn tuples_may_mix_types() {
    let id = fumola_create();
    let v = eval_value(id, "(1, true)");
    assert_eq!(
        v["value"],
        json!([
            {"tag": "Int", "value": "1"},
            {"tag": "Bool", "value": true}
        ])
    );
}

#[test]
fn records_translate_by_field_name() {
    let id = fumola_create();
    let v = eval_value(id, "{x = 1; y = 2}");
    assert_eq!(v["tag"], json!("Record"));
    assert_eq!(v["value"]["x"], json!({"tag":"Int","value":"1"}));
    assert_eq!(v["value"]["y"], json!({"tag":"Int","value":"2"}));
}

/// Fumola holds record fields in a HashMap, whose iteration order is not
/// stable. Without sorting, a record's fields could come back in a different
/// order on each evaluation and the livelit's result would flicker.
#[test]
fn record_fields_come_back_in_a_stable_order() {
    let id = fumola_create();
    let src = "{delta = 4; alpha = 1; charlie = 3; bravo = 2}";
    let first = eval_value(id, src);
    let keys: Vec<String> = first["value"]
        .as_object()
        .expect("record is an object")
        .keys()
        .cloned()
        .collect();
    assert_eq!(keys, vec!["alpha", "bravo", "charlie", "delta"]);
}

#[test]
fn variants_translate_with_and_without_a_payload() {
    let id = fumola_create();

    let v = eval_value(id, "#some(3)");
    assert_eq!(v["tag"], json!("Variant"));
    assert_eq!(v["value"]["name"], json!("some"));
    assert_eq!(v["value"]["value"], json!({"tag":"Int","value":"3"}));

    let v = eval_value(id, "#none");
    assert_eq!(v["tag"], json!("Variant"));
    assert_eq!(v["value"]["name"], json!("none"));
    assert_eq!(v["value"]["value"], json!(null));
}

#[test]
fn records_and_tuples_compose() {
    let id = fumola_create();
    let v = eval_value(id, "{pair = (1, 2); flag = true}");
    assert_eq!(v["value"]["pair"]["tag"], json!("Tuple"));
    assert_eq!(v["value"]["flag"], json!({"tag":"Bool","value":true}));
}

/// An untranslatable component fails the whole translation, rather than the
/// tuple being reported as complete with a piece quietly dropped.
#[test]
fn an_untranslatable_component_fails_the_whole_value() {
    let id = fumola_create();
    let raw = fumola_eval(id, "(1, 1 + `x)");
    assert!(
        raw.contains("\"ok\":false"),
        "expected the tuple to fail, got {}",
        raw
    );
}
