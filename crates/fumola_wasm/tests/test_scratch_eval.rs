//! Running a program for its answer alone, without letting it touch the
//! instance it ran in.
//!
//! Building a scene is the case this exists for. It is worth running and not
//! worth keeping: `generateSceneFullDemand` opens with `A.reset()`, so asking
//! for a scene from inside a session would wipe the graph that session was
//! about, and the answer runs to megabytes that would then live in the store
//! for as long as the instance does.

use fumola_wasm::*;

fn reply(raw: String) -> serde_json::Value {
    serde_json::from_str(&raw).expect("reply is JSON")
}

fn eval_top_ok(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    let v = reply(fumola_eval_top(id, src));
    assert_eq!(v["ok"], serde_json::json!(true), "eval failed: {}", v);
    v
}

#[test]
fn a_scratch_program_leaves_no_bindings_behind() {
    let id = fumola_create();
    let v = reply(fumola_eval_scratch(id, "let onlyInScratch = 7; onlyInScratch"));
    assert_eq!(v["ok"], serde_json::json!(true), "{}", v);
    assert_eq!(v["value"], serde_json::json!("7"));

    // The binding went with the branch.
    let after = reply(fumola_eval_top(id, "onlyInScratch"));
    assert_eq!(after["ok"], serde_json::json!(false), "the binding survived: {}", after);
    assert!(
        after["error"].as_str().unwrap_or("").contains("UnboundIdentifer"),
        "expected an unbound identifier, got {}",
        after
    );
}

#[test]
fn a_scratch_program_leaves_the_store_alone() {
    let id = fumola_create();
    eval_top_ok(id, "`kept := 41");
    // Writes in scratch are invisible afterwards...
    reply(fumola_eval_scratch(id, "`kept := 999; `alsoScratch := 1"));
    let v = eval_top_ok(id, "peek(`kept)");
    assert_eq!(v["value"]["value"], serde_json::json!("41"), "the write escaped: {}", v);
    let gone = eval_top_ok(id, "peek(`alsoScratch)");
    assert_eq!(gone["tag"], serde_json::json!("Null"), "a scratch cell survived: {}", gone);
}

/// The one that matters: a scene reset would otherwise destroy the session.
#[test]
fn building_a_scene_in_scratch_does_not_reset_the_session() {
    let id = fumola_create();
    fumola_ensure_mode(id, "graphical");
    eval_top_ok(id, "`before := 11");

    let scene = reply(fumola_eval_scratch(
        id,
        r#"import M "fumola/examples/mergeSort/mergeSort";
           import A "fumola/system/adapton";
           A.IntoJSON.fromScene(M.generateSceneFullDemand(10, 4, null).sceneData)"#,
    ));
    assert_eq!(scene["ok"], serde_json::json!(true), "{}", scene);
    assert_eq!(scene["tag"], serde_json::json!("String"));
    let text = scene["value"].as_str().expect("a String value");
    assert!(text.len() > 10_000, "scene looks too small: {} bytes", text.len());
    for key in ["objects", "events", "nodes", "edges", "outline"] {
        assert!(text.contains(key), "scene is missing {}", key);
    }

    // `generateSceneFullDemand` calls A.reset(). On the branch, that is fine.
    let survived = eval_top_ok(id, "peek(`before)");
    assert_eq!(
        survived["value"]["value"],
        serde_json::json!("11"),
        "the scene's reset reached the session: {}",
        survived
    );
}

/// Repeating it must not accumulate. The branch is dropped each time, so the
/// tenth scene costs what the first did rather than the tenth of a heap.
#[test]
fn scenes_do_not_accumulate_across_scratch_runs() {
    let id = fumola_create();
    fumola_ensure_mode(id, "graphical");
    let program = r#"import M "fumola/examples/mergeSort/mergeSort";
                     import A "fumola/system/adapton";
                     A.IntoJSON.fromScene(M.generateSceneFullDemand(10, 4, null).sceneData)"#;
    let mut sizes = Vec::new();
    for _ in 0..5 {
        let v = reply(fumola_eval_scratch(id, program));
        assert_eq!(v["ok"], serde_json::json!(true), "{}", v);
        sizes.push(v["value"].as_str().expect("a String").len());
    }
    // Every run produces the same scene, from an instance in the same state.
    assert!(
        sizes.windows(2).all(|w| w[0] == w[1]),
        "scene size drifted across runs, so the branch is not being discarded: {:?}",
        sizes
    );
}

/// The case that matters more than the scene generators: outlining a session
/// that someone actually typed.
///
/// `Outline.outline` takes a pointer and walks the DCG from it, so it knows
/// nothing about level trees or merge networks -- it works on any force tree.
/// But it memoises itself with `force(`outline(root) := thunk {...})`, so it
/// writes to the store as it reads it. Scratch is how you look without that
/// showing up in what you are looking at.
#[test]
fn outlining_an_ordinary_session_leaves_it_unmarked() {
    let id = fumola_create();
    fumola_ensure_mode(id, "graphical");
    eval_top_ok(
        id,
        "`x := 3; `y := 4; force(`t := thunk { let a = force(`u := thunk { get(`x) * 2 }); a + get(`y) })",
    );

    let v = reply(fumola_eval_scratch(
        id,
        r#"import A "fumola/system/adapton"; A.IntoText.outline(A.Outline.outline(pointer(`t)))"#,
    ));
    assert_eq!(v["ok"], serde_json::json!(true), "{}", v);
    let text = v["value"].as_str().expect("a String value");
    // The arrow grammar replayground draws, over a hand-written computation.
    assert!(text.contains("══▷»"), "no force edge in the outline: {}", text);
    assert!(text.contains("───>"), "no read edge in the outline: {}", text);
    assert!(text.contains("〜〜>"), "no result row in the outline: {}", text);
    assert!(text.contains('u'), "the nested force is missing: {}", text);

    // Outlining wrote `outline(`t) on the branch. It is not here.
    let trace = eval_top_ok(id, "peek(`outline(pointer(`t)))");
    assert_eq!(
        trace["tag"],
        serde_json::json!("Null"),
        "outlining left its memo in the session: {}",
        trace
    );
}
