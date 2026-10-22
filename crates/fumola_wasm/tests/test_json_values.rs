//! A host's values in as values: `fumola_define_json` and
//! `fumola_eval_top_with` read JSON straight into Fumola values, with no source
//! text in between (json_in says how JSON reads).

use fumola_wasm::*;

fn reply(raw: String) -> serde_json::Value {
    serde_json::from_str(&raw).expect("reply is JSON")
}

fn ok(raw: String) -> serde_json::Value {
    let v = reply(raw);
    assert_eq!(v["ok"], serde_json::json!(true), "not ok: {}", v);
    v
}

fn not_ok(raw: String) -> serde_json::Value {
    let v = reply(raw);
    assert_eq!(
        v["ok"],
        serde_json::json!(false),
        "expected a failure: {}",
        v
    );
    v
}

#[test]
fn each_kind_of_json_value_reads_as_its_fumola_value() {
    let id = fumola_create();
    ok(fumola_define_json(
        id,
        "x",
        r#"{"nat": 3, "int": -2, "float": 1.5, "text": "hi", "yes": true, "nothing": null,
            "list": [10, 20, 30], "way": {"tag": "Way", "value": "E"}, "done": {"tag": "Done"},
            "rec": {"tag": "Cell", "kind": "grass"}}"#,
    ));
    assert_eq!(ok(fumola_eval_top(id, "x.nat + 1"))["value"], "4");
    // Nat and Int stay apart in Fumola: a negative whole number is an Int
    let int = ok(fumola_eval_top(id, "x.int"));
    assert_eq!(
        (int["tag"].as_str(), int["value"].as_str()),
        (Some("Int"), Some("-2"))
    );
    assert_eq!(ok(fumola_eval_top(id, "x.float"))["tag"], "Float");
    assert_eq!(ok(fumola_eval_top(id, "x.text # \"!\""))["value"], "hi!");
    assert_eq!(ok(fumola_eval_top(id, "x.yes"))["value"], true);
    assert_eq!(ok(fumola_eval_top(id, "x.nothing"))["tag"], "Null");
    assert_eq!(
        ok(fumola_eval_top(id, "x.list[1] + x.list.size()"))["value"],
        "23"
    );
    // {tag, value} is a variant; a pattern takes it apart
    assert_eq!(
        ok(fumola_eval_top(
            id,
            "switch (x.way) { case (#Way(d)) { d }; case _ { \"\" } }"
        ))["value"],
        "E"
    );
    let done = ok(fumola_eval_top(id, "x.done"));
    assert_eq!(done["tag"], "Variant");
    assert_eq!(done["value"]["name"], "Done");
    // any key but `value` beside `tag` makes the object a record
    assert_eq!(ok(fumola_eval_top(id, "x.rec.kind"))["value"], "grass");
}

#[test]
fn a_binding_lasts_like_a_top_level_let() {
    let id = fumola_create();
    ok(fumola_define_json(id, "cells", "[1, 2, 3]"));
    ok(fumola_eval_top(
        id,
        "func total(xs) { var s = 0; for (x in xs.vals()) { s := s + x }; s }",
    ));
    assert_eq!(ok(fumola_eval_top(id, "total(cells)"))["value"], "6");
    // defined again, it is the new value
    ok(fumola_define_json(id, "cells", "[4, 5]"));
    assert_eq!(ok(fumola_eval_top(id, "total(cells)"))["value"], "9");
}

#[test]
fn a_call_takes_its_arguments_as_json() {
    let id = fumola_create();
    ok(fumola_eval_top(
        id,
        "func find_at(cs, i, n, c, r) { if (i == n) { null } else { let x = cs[i]; if (x.col == c and x.row == r) { ?x } else { find_at(cs, i + 1, n, c, r) } } }",
    ));
    let cells: Vec<serde_json::Value> = (0..4)
        .flat_map(|r| (0..4).map(move |c| serde_json::json!({"col": c, "row": r, "kind": "grass"})))
        .collect();
    let bindings = serde_json::json!({"cs": cells, "c": 2, "r": 3}).to_string();
    let v = ok(fumola_eval_top_with(
        id,
        "switch (find_at(cs, 0, cs.size(), c, r)) { case (?x) { x.col * 10 + x.row }; case null { 99 } }",
        &bindings,
    ));
    assert_eq!(v["value"], "23");
}

#[test]
fn values_in_as_json_take_no_steps_to_build() {
    // the same array, written as source text against given as JSON: the text's literal is
    // stepped into a value element by element; the JSON is a value before the program runs
    let id = fumola_create();
    let xs: Vec<u64> = (1..=1000).collect();
    let as_text = format!(
        "[{}].size()",
        xs.iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ")
    );
    let text_steps = ok(fumola_eval_top(id, &as_text))["counts"]["step"]
        .as_u64()
        .unwrap();
    let json_steps = ok(fumola_eval_top_with(
        id,
        "xs.size()",
        &serde_json::json!({ "xs": xs }).to_string(),
    ))["counts"]["step"]
        .as_u64()
        .unwrap();
    assert!(
        text_steps > 1000,
        "the text literal took only {} steps",
        text_steps
    );
    assert!(json_steps < 50, "the JSON value took {} steps", json_steps);
}

#[test]
fn a_program_that_fails_leaves_no_binding_behind() {
    let id = fumola_create();
    not_ok(fumola_eval_top_with(id, "noSuchFunction(y)", r#"{"y": 1}"#));
    let after = not_ok(fumola_eval_top(id, "y"));
    let said = after["error"].as_str().unwrap_or("");
    assert!(
        said.contains("not bound"),
        "y survived the failed program: {}",
        after
    );
}

#[test]
fn what_cannot_be_read_is_an_error_not_a_trap() {
    let id = fumola_create();
    not_ok(fumola_define_json(id, "x", "{not json"));
    not_ok(fumola_define_json(id, "two words", "1"));
    not_ok(fumola_define_json(id, "1x", "1"));
    not_ok(fumola_define_json(4_000_000, "x", "1"));
    not_ok(fumola_eval_top_with(id, "0", "[1, 2]"));
    not_ok(fumola_eval_top_with(id, "0", r#"{"no good": 1}"#));
    // and the instance still works
    assert_eq!(ok(fumola_eval_top(id, "1 + 1"))["value"], "2");
}
