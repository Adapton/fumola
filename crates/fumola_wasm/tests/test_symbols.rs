//! Symbol translation: Fumola symbols <-> Hazel-facing JSON.
//!
//! The important property is not that the JSON looks right, but that a symbol
//! rendered to source and fed back through Fumola denotes the same name. Those
//! tests go through a real instance rather than asserting on strings.

use fumola_wasm::symbol::{symbol_from_json, symbol_to_json, symbol_to_source};
use fumola_wasm::*;
use serde_json::json;

fn to_source(j: serde_json::Value) -> Result<String, String> {
    symbol_to_source(&symbol_from_json(&j)?)
}

#[test]
fn renders_the_four_translated_forms() {
    assert_eq!(to_source(json!({"tag":"Num","value":"3"})).unwrap(), "3");
    assert_eq!(to_source(json!({"tag":"Name","value":"x"})).unwrap(), "`x");
    assert_eq!(
        to_source(json!({
            "tag":"Call",
            "fun": {"tag":"Name","value":"adapton"},
            "arg": {"tag":"Name","value":"settings"}
        }))
        .unwrap(),
        "`adapton(`settings)"
    );
    assert_eq!(
        to_source(json!({
            "tag":"Dot",
            "left": {"tag":"Name","value":"a"},
            "right": {"tag":"Name","value":"b"}
        }))
        .unwrap(),
        "`a.`b"
    );
}

/// Chained calls are how Fumola's own namespaces are written, e.g.
/// `adapton(`settings)(`forceEndForgetsResult).
#[test]
fn renders_chained_calls() {
    let chained = json!({
        "tag":"Call",
        "fun": {"tag":"Call",
                "fun": {"tag":"Name","value":"adapton"},
                "arg": {"tag":"Name","value":"settings"}},
        "arg": {"tag":"Name","value":"forceEndForgetsResult"}
    });
    assert_eq!(
        to_source(chained).unwrap(),
        "`adapton(`settings)(`forceEndForgetsResult)"
    );
}

/// The round trip that matters: a symbol built on the Hazel side names a cell,
/// and Fumola agrees it is that cell. Written through `fumola_eval` so the
/// symbol is exercised by the real parser and store.
#[test]
fn a_translated_symbol_names_a_real_cell() {
    let id = fumola_create();

    // Write through a symbol rendered from JSON...
    let name = to_source(json!({"tag":"Name","value":"answer"})).unwrap();
    let raw = fumola_eval(id, "`topLevel", &format!("let p = {} := 42; @ p", name));
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "eval failed: {}", raw);
    assert_eq!(v["value"], json!("42"));

    // ...and read it back with fumola_get, which takes the symbol as JSON.
    let raw = fumola_get(id, &json!({"tag":"Name","value":"answer"}).to_string());
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "get failed: {}", raw);
    assert_eq!(v["tag"], json!("Int"));
    assert_eq!(v["value"], json!("42"));
}

/// Numbers are symbols too, and act as names just like identifiers do.
#[test]
fn numeric_symbols_name_cells() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "let p = 7 := 99; @ p");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "eval failed: {}", raw);

    let raw = fumola_get(id, &json!({"tag":"Num","value":"7"}).to_string());
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "get failed: {}", raw);
    assert_eq!(v["value"], json!("99"));
}

/// A symbol coming *out* of Fumola must be translatable, so that a Hazel
/// program can hold a name it did not itself construct.
#[test]
fn symbols_translate_outward() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "`x");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "eval failed: {}", raw);
    assert_eq!(v["tag"], json!("Symbol"));
    assert_eq!(v["value"], json!({"tag":"Name","value":"x"}));

    // And back in again: the outbound JSON is valid inbound JSON.
    assert_eq!(to_source(v["value"].clone()).unwrap(), "`x");
}

#[test]
fn structured_symbols_translate_outward() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "`adapton(`settings)");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], json!(true), "eval failed: {}", raw);
    assert_eq!(v["tag"], json!("Symbol"));
    assert_eq!(to_source(v["value"].clone()).unwrap(), "`adapton(`settings)");
}

/// Names that would not survive Fumola's lexer are refused, so that a Hazel
/// string cannot inject arbitrary text into a generated program.
#[test]
fn rejects_names_that_are_not_names() {
    for bad in ["", "1x", "a b", "a;b", "x`y", "a)", "évident"] {
        let result = to_source(json!({"tag":"Name","value": bad}));
        assert!(result.is_err(), "expected {:?} to be rejected", bad);
    }
}

#[test]
fn rejects_malformed_symbol_json() {
    assert!(symbol_from_json(&json!({"value":"x"})).is_err(), "no tag");
    assert!(symbol_from_json(&json!({"tag":"Nope"})).is_err(), "bad tag");
    assert!(
        symbol_from_json(&json!({"tag":"Call","fun":{"tag":"Name","value":"f"}})).is_err(),
        "missing arg"
    );
    assert!(
        symbol_from_json(&json!({"tag":"Num","value":"not a number"})).is_err(),
        "bad number"
    );
}

/// Negative numbers fall back to Symbol::Int rather than failing.
#[test]
fn negative_numbers_use_int() {
    let sym = symbol_from_json(&json!({"tag":"Num","value":"-5"})).unwrap();
    assert_eq!(symbol_to_json(&sym).unwrap(), json!({"tag":"Num","value":"-5"}));
}

#[test]
fn untranslatable_symbol_forms_are_reported() {
    // `1 + `x is a symbolic BinOp, not an addition.
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "1 + `x");
    assert!(
        raw.contains("\"ok\":false") && raw.contains("BinOp"),
        "expected a BinOp translation failure, got {}",
        raw
    );
}
