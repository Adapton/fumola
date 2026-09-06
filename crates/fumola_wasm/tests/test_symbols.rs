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

/// `1 + `x` is a symbolic BinOp, not an addition. It used to be reported as
/// untranslatable; it now crosses, carrying the operator as its source text
/// because there is no Hazel-side datatype for a Fumola operator.
#[test]
fn operator_symbols_translate() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "1 + `x");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], true, "got {}", raw);
    assert_eq!(v["tag"], "Symbol");
    assert_eq!(
        v["value"],
        json!({
            "tag": "BinOp",
            "op": "+",
            "left": {"tag": "Num", "value": "1"},
            "right": {"tag": "Name", "value": "x"},
        })
    );
}

/// Symbols built with an operator used to have no source rendering at all,
/// so any pointer named with one failed to cross -- `symbol has no source
/// rendering yet`. They are ordinary in the library: levelTree names a space
/// `` `merge-`symbol ``, combining a literal symbol with one it was passed.
#[test]
fn operator_symbols_render_and_round_trip() {
    // Each operand carries its own backtick, which is what parses and what
    // prints back as merge-symbol. fumola_symbol_of parses and re-renders,
    // so this is the round trip in one call.
    let v: serde_json::Value =
        serde_json::from_str(&fumola_symbol_of("`merge-`symbol")).unwrap();
    assert_eq!(v["ok"], true, "should parse and render: {}", v);
    assert_eq!(v["source"], "`merge-`symbol");

    // A unary one too.
    let u: serde_json::Value = serde_json::from_str(&fumola_symbol_of("-`x")).unwrap();
    assert_eq!(u["ok"], true, "got {}", u);
    assert_eq!(u["source"], "-`x");
}

#[test]
fn a_pointer_named_with_an_operator_symbol_crosses() {
    let id = fumola_create();
    // This is the shape that failed: the value is a pointer, and translating
    // it needs both the source rendering and the JSON.
    let raw = fumola_eval_top(id, "(`merge-`symbol) := 1");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], true, "got {}", raw);
    assert_eq!(v["tag"], "AdaptonPointer");
    assert_eq!(v["value"]["source"], "`merge-`symbol");
    assert_eq!(v["value"]["symbol"]["tag"], "BinOp");
    assert_eq!(v["value"]["symbol"]["op"], "-");
}

/// Every symbol form now crosses. This matters more than it sounds: one
/// untranslatable name anywhere in a value fails the whole value, so a single
/// quoted-AST name in the adapton graph made the entire event log
/// untranslatable, and the events panel showed an error instead of a table.
#[test]
fn no_symbol_form_fails_a_whole_value() {
    let id = fumola_create();
    fumola_ensure_mode(id, "graphical");
    // levelTree names nodes with operator and quoted-AST symbols.
    let raw = fumola_eval_top(
        id,
        "import M \"fumola/collections/levelTree\"; M.testFromList()",
    );
    assert!(raw.contains("\"ok\":true"), "the test itself should run: {}", raw);

    let events = fumola_eval_top(id, "Adapton.peekEvents()");
    assert!(
        events.contains("\"ok\":true"),
        "the whole event log must translate, got {}",
        &events[..events.len().min(220)]
    );
}
