//! Tokens for syntax highlighting, and access to the library's source.

use fumola_wasm::{fumola_module_source, fumola_modules, fumola_tokens};

/// (kind, the text that span actually covers) for each token.
fn kinds(src: &str) -> Vec<(String, String)> {
    let v: serde_json::Value = serde_json::from_str(&fumola_tokens(src)).unwrap();
    assert_eq!(v["ok"], true, "lexing failed: {}", v);
    v["tokens"]
        .as_array()
        .unwrap()
        .iter()
        .map(|t| {
            let start = t["start"].as_u64().unwrap() as usize;
            let len = t["len"].as_u64().unwrap() as usize;
            (
                t["kind"].as_str().unwrap().to_string(),
                src[start..start + len].to_string(),
            )
        })
        .collect()
}

#[test]
fn kinds_match_the_theme() {
    let got = kinds("`x := 1; // c\nType.foo(2) @ p");
    let want = vec![
        ("symbol", "`x"),     // the backtick and the name are one thing
        ("space", " "),
        ("effect", ":="),     // put, coloured apart from other operators
        ("space", " "),
        ("literal", "1"),
        ("punct", ";"),
        ("space", " "),
        ("comment", "// c"),
        ("space", "\n"),
        ("type", "Type"),     // an initial capital is a type, as the theme has it
        ("punct", "."),
        ("ident", "foo"),
        ("bracket", "("),
        ("literal", "2"),
        ("bracket", ")"),
        ("space", " "),
        ("effect", "@"),      // get
        ("space", " "),
        ("ident", "p"),
    ];
    let got: Vec<(&str, &str)> = got.iter().map(|(k, t)| (k.as_str(), t.as_str())).collect();
    assert_eq!(got, want);
}

/// The spans have to tile the source exactly, or a highlighter built on them
/// drops or duplicates text.
#[test]
fn spans_tile_the_source() {
    for src in [
        "1 + 2",
        "`x := 1; // c\nType.foo(2)",
        "// leading\nlet x = 1",
        "/* block */ let y = 2 // and a line\nlet z = 3",
        "let s = \"text with // not a comment\"; 1",
    ] {
        let v: serde_json::Value = serde_json::from_str(&fumola_tokens(src)).unwrap();
        let mut at = 0usize;
        for t in v["tokens"].as_array().unwrap() {
            let start = t["start"].as_u64().unwrap() as usize;
            let len = t["len"].as_u64().unwrap() as usize;
            assert_eq!(start, at, "gap or overlap in {:?} at {}", src, at);
            at = start + len;
        }
        assert_eq!(at, src.len(), "tokens do not reach the end of {:?}", src);
    }
}

/// Regression: spans used to be measured from the start of each segment
/// between comments, so everything after the first comment was offset.
#[test]
fn spans_stay_absolute_after_a_comment() {
    let src = "// c\nType";
    let got = kinds(src);
    assert!(
        got.contains(&("type".to_string(), "Type".to_string())),
        "got {:?}",
        got
    );
}

#[test]
fn the_library_is_readable() {
    let v: serde_json::Value = serde_json::from_str(&fumola_modules()).unwrap();
    let mods = v["modules"].as_array().unwrap();
    assert!(mods.len() > 10, "expected the whole library, got {}", mods.len());
    assert!(mods.iter().any(|m| m == "fumola/system/prelude"));

    let p: serde_json::Value =
        serde_json::from_str(&fumola_module_source("fumola/system/prelude")).unwrap();
    assert_eq!(p["ok"], true);
    assert!(p["source"].as_str().unwrap().contains("public func get"));

    let missing: serde_json::Value =
        serde_json::from_str(&fumola_module_source("fumola/nope")).unwrap();
    assert_eq!(missing["ok"], false);
}
