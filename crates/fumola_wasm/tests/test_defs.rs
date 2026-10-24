//! Every definition an instance holds, with its exact source: the outline a
//! host draws (ticket 589).

use fumola_wasm::{fumola_create, fumola_defs, fumola_drop, fumola_eval_top};

const COMMON: &str = "module Common = {
  public type Side = {#L; #R};
  public type Pair<A> = (A, A);
  public let limit : Nat = 3;
  public func flip2(a : Side, b : Side) : Pair<Side> { (b, a) };
  public func first<A>(p : Pair<A>) : A { p.0 };
};
0";

fn defs(src: &str) -> serde_json::Value {
    let id = fumola_create();
    let r: serde_json::Value = serde_json::from_str(&fumola_eval_top(id, src)).unwrap();
    assert_eq!(r["ok"], true, "the program ran: {}", r);
    let v: serde_json::Value = serde_json::from_str(&fumola_defs(id)).unwrap();
    fumola_drop(id);
    assert_eq!(v["ok"], true, "{}", v);
    v
}

fn common(v: &serde_json::Value) -> serde_json::Value {
    v["contexts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|c| c["name"] == "Common")
        .unwrap_or_else(|| panic!("a context named Common: {}", v))
        .clone()
}

#[test]
fn types_in_source_order_with_their_parameters_and_sources() {
    let c = common(&defs(COMMON));
    let types = c["types"].as_array().unwrap();
    let names: Vec<_> = types.iter().map(|t| t["name"].as_str().unwrap()).collect();
    assert_eq!(names, vec!["Side", "Pair"]);
    assert_eq!(types[0]["source"]["line"], 2);
    assert_eq!(types[0]["name_source"]["line"], 2);
    assert_eq!(types[0]["vis"], "public");
    assert_eq!(types[1]["params"], serde_json::json!(["A"]));
    assert!(
        types[0]["text"].as_str().unwrap().contains("#L"),
        "{}",
        types[0]
    );
}

#[test]
fn members_in_source_order_with_what_a_host_shows() {
    let c = common(&defs(COMMON));
    let ms = c["members"].as_array().unwrap();
    let names: Vec<_> = ms.iter().map(|m| m["name"].as_str().unwrap()).collect();
    assert_eq!(names, vec!["limit", "flip2", "first"]);
    // a let: its annotation and the name's own place
    assert_eq!(ms[0]["kind"], "static value");
    assert_eq!(ms[0]["annot"]["text"], "Nat");
    assert_eq!(ms[0]["name_source"]["line"], 4);
    // a function: its parameters and result as written, each with its source
    assert_eq!(ms[1]["kind"], "function");
    assert_eq!(ms[1]["name_source"]["line"], 5);
    assert!(
        ms[1]["params"]["text"].as_str().unwrap().contains("Side"),
        "{}",
        ms[1]
    );
    assert_eq!(ms[1]["params"]["source"]["line"], 5);
    assert!(
        ms[1]["result"]["text"].as_str().unwrap().contains("Pair"),
        "{}",
        ms[1]
    );
    assert_eq!(ms[2]["type_params"], serde_json::json!(["A"]));
}

#[test]
fn a_source_selects_exactly_its_text() {
    let v = defs(COMMON);
    let c = common(&v);
    let side = &c["types"][0]["name_source"];
    let (a, b) = (
        side["start"].as_u64().unwrap() as usize,
        side["end"].as_u64().unwrap() as usize,
    );
    assert_eq!(&COMMON[a..b], "Side");
    let flip2 = &c["members"][1]["name_source"];
    let (a, b) = (
        flip2["start"].as_u64().unwrap() as usize,
        flip2["end"].as_u64().unwrap() as usize,
    );
    assert_eq!(&COMMON[a..b], "flip2");
}

#[test]
fn an_unknown_instance_says_so() {
    let v: serde_json::Value = serde_json::from_str(&fumola_defs(987654)).unwrap();
    assert_eq!(v["ok"], false);
}
