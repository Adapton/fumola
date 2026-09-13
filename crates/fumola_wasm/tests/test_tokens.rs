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
        ("operator", ";"),
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

/// Fumola's own keywords must colour as keywords. `lexer::is_keyword` is
/// inherited from Motoko and has none of them, so the highlighter carries the
/// theme's list; a regression here would leave `thunk` and `force` looking
/// like ordinary variables, which is most of what Fumola code says.
#[test]
fn the_adapton_forms_are_keywords() {
    let got = kinds("do within space `s { force(thunk { 1 }) }");
    let keywords: Vec<&str> = got
        .iter()
        .filter(|(k, _)| k == "keyword")
        .map(|(_, t)| t.as_str())
        .collect();
    assert_eq!(keywords, vec!["do", "within", "space", "force", "thunk"]);
}

/// Variant tags are one token, not `#` plus a name. Adapton's own output is
/// almost entirely variants, so this is most of what makes a large result
/// readable.
#[test]
fn variant_tags_are_one_token() {
    let got = kinds("#addNode((#Symbol(`n), #Now, 1))");
    let tags: Vec<&str> = got
        .iter()
        .filter(|(k, _)| k == "variant")
        .map(|(_, t)| t.as_str())
        .collect();
    assert_eq!(tags, vec!["#addNode", "#Symbol", "#Now"]);
    // A bare `#` is the string operator, and stays one.
    let bare = kinds("a # b");
    assert!(bare.iter().any(|(k, t)| k == "operator" && t == "#"), "got {:?}", bare);
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
    assert!(mods.iter().any(|m| m["path"] == "fumola/system/prelude"));

    // Each module appears once, under the path its file is actually at.
    // `collections/`, `examples/` and `examples/mergeSort/` used to hold
    // symlinks to their dependencies -- a module could only import what was
    // registered beside it -- so `adapton` was listed three times and a host
    // had to filter the copies out by a `link` flag. Imports can say
    // `../../system/adapton` now, and the listing is the library.
    let paths: Vec<&str> = mods.iter().map(|m| m["path"].as_str().unwrap()).collect();
    for path in &paths {
        assert!(
            !mods.iter().any(|m| m.get("link").is_some()),
            "{} still carries a link flag; the symlinked copies are gone",
            path
        );
    }
    let mut unique = paths.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(unique.len(), paths.len(), "a module is listed twice: {:?}", paths);
    for gone in [
        "fumola/collections/adapton",
        "fumola/examples/adapton",
        "fumola/examples/mergeSort/adapton",
        "fumola/system/hashMap",
    ] {
        assert!(!paths.contains(&gone), "{} was a symlink and should be gone", gone);
    }
    // The real homes are all there.
    for real in [
        "fumola/system/adapton",
        "fumola/collections/hashMap",
        "fumola/examples/mergeSort/mergeSort",
        "fumola/system/prelude",
    ] {
        assert!(paths.contains(&real), "{} is missing from the listing", real);
    }

    let p: serde_json::Value =
        serde_json::from_str(&fumola_module_source("fumola/system/prelude")).unwrap();
    assert_eq!(p["ok"], true);
    assert!(p["source"].as_str().unwrap().contains("public func get"));

    let missing: serde_json::Value =
        serde_json::from_str(&fumola_module_source("fumola/nope")).unwrap();
    assert_eq!(missing["ok"], false);
}

/// An evaluation mode is coloured as part of its form.
///
/// `big` is an ordinary identifier to the parser -- reserving it would take
/// `` `big `` away from symbols, and symbols are how a program names cells. It
/// is a keyword only here, the way `space` and `time` are, so that a reader
/// sees one form rather than a call.
#[test]
fn an_evaluation_mode_is_a_keyword() {
    let got = kinds("do big { 1 }");
    let keywords: Vec<&str> = got
        .iter()
        .filter(|(k, _)| k == "keyword")
        .map(|(_, t)| t.as_str())
        .collect();
    assert_eq!(keywords, vec!["do", "big"]);
}
