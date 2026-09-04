//! Round-trip tests for the Fumola livelit's external store.
//!
//! These run natively; nothing here needs a browser or a wasm host.

use fumola_wasm::*;

fn eval_ok(id: FumolaInstanceId, src: &str) -> serde_json::Value {
    let raw = fumola_eval(id, "`topLevel", src);
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
    let raw = fumola_eval(id, "`topLevel", "1 + 2");
    assert!(raw.contains("\"ok\":false"), "expected failure, got {}", raw);
}

#[test]
fn syntax_errors_are_reported_not_panicked() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "1 +");
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
    let raw = fumola_eval(id, "`topLevel", "peek(404)");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], serde_json::json!(true), "peek failed: {}", raw);
    assert_eq!(v["tag"], serde_json::json!("Null"));

    // ...and one that was written peeks as an option carrying the value.
    let raw = fumola_eval(id, "`topLevel", "peek(1)");
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

/// The Fumola library is compiled in, so a host with no filesystem still has
/// the modules that make the language useful.
#[test]
fn library_modules_are_bound_at_the_top_level() {
    let id = fumola_create();
    // gcd is the smallest thing with an observable answer.
    assert_eq!(eval_int(id, "Gcd.gcd(12, 18)"), "6");
}

#[test]
fn the_adapton_module_is_available() {
    let id = fumola_create();
    // Reaching a name inside the module is enough; it is 894 lines of types
    // and functions, and loading it at all is what is being checked.
    let raw = fumola_eval(id, "`topLevel", "Adapton");
    assert!(
        !raw.contains("\"ok\":false") || !raw.contains("not defined"),
        "Adapton should be bound: {}",
        raw
    );
}

/// Every module is registered, not only the bound ones, so anything in the
/// library can still be imported by path.
#[test]
fn unbound_modules_can_still_be_imported() {
    let id = fumola_create();
    let raw = fumola_eval(id, "`topLevel", "import D \"fumola/examples/deriveCompare\"; 1");
    assert!(raw.contains("\"ok\":true"), "import failed: {}", raw);
}

/// The bindings are made once when the instance is created, and top-level
/// bindings persist, so they are still there after later edits.
#[test]
fn bindings_survive_edits() {
    let id = fumola_create();
    assert_eq!(eval_int(id, "1 + 1"), "2");
    assert_eq!(eval_int(id, "Gcd.gcd(48, 18)"), "6");
    assert_eq!(eval_int(id, "1 := 2; get(1)"), "2");
    assert_eq!(eval_int(id, "Gcd.gcd(100, 75)"), "25");
}

/// A realized instance gets the library too, so a saved program reloading
/// into a fresh session is not missing it.
#[test]
fn a_realized_instance_has_the_library() {
    let id: FumolaInstanceId = 909;
    assert!(fumola_realize(id));
    assert_eq!(eval_int(id, "Gcd.gcd(9, 6)"), "3");
}

/// Arrays reach the host as lists. Adapton's introspection returns them, so
/// without this most of what the Adapton module reports is untranslatable.
#[test]
fn arrays_translate_as_lists() {
    let id = fumola_create();
    let v = eval_ok(id, "[1, 2, 3]");
    assert_eq!(v["tag"], serde_json::json!("List"));
    assert_eq!(v["value"][2], serde_json::json!({"tag":"Int","value":"3"}));

    // Adapton's own introspection returns arrays too, but their elements
    // carry adapton spaces and times, which have no translation yet -- so
    // peekEvents still fails, on its contents rather than on being an array.
    let raw = fumola_eval(id, "`topLevel", "Adapton.peekEvents()");
    assert!(raw.contains("no Hazel translation"), "unexpected: {}", raw);
}

/// A failed program must leave the instance exactly as it was. Some failures
/// do not merely fail: an AssertionFailure inside a force was observed to
/// lose every top-level binding, so one bad edit made all later edits fail.
#[test]
fn a_failed_program_does_not_break_the_instance() {
    let id = fumola_create();
    assert_eq!(eval_int(id, "Gcd.gcd(12, 18)"), "6");

    // Fails, and used to take the instance's bindings with it.
    let raw = fumola_eval(id, "`topLevel", "1 := 2; Adapton.peekForce(pointer(1))");
    assert!(raw.contains("\"ok\":false"), "expected a failure, got {}", raw);

    // The library is still there, and so is the store.
    assert_eq!(eval_int(id, "Gcd.gcd(12, 18)"), "6");
    assert_eq!(eval_int(id, "1 := 5; get(1)"), "5");
}

/// A failed program must not commit its writes either.
#[test]
fn a_failed_program_does_not_write() {
    let id = fumola_create();
    eval_ok(id, "`kept := 1");
    let raw = fumola_eval(id, "`topLevel", "`kept := 2; 1 +");
    assert!(raw.contains("\"ok\":false"), "expected a failure, got {}", raw);
    assert_eq!(eval_int(id, "get(`kept)"), "1");
}

/// Two programs sharing a runtime must not share a thunk name, or each would
/// overwrite the other's thunk and neither would keep its history.
#[test]
fn thunks_with_different_names_do_not_clobber_each_other() {
    let id = fumola_create();

    // Two thunks in one runtime, each edited, each keeping its own result.
    let raw = fumola_eval(id, "`a", "1 + 1");
    assert!(raw.contains("\"value\":\"2\""), "a: {}", raw);
    let raw = fumola_eval(id, "`b", "10 + 10");
    assert!(raw.contains("\"value\":\"20\""), "b: {}", raw);

    // Editing b leaves a alone, and vice versa.
    let raw = fumola_eval(id, "`b", "10 + 11");
    assert!(raw.contains("\"value\":\"21\""), "b edited: {}", raw);
    let raw = fumola_eval(id, "`a", "1 + 1");
    assert!(raw.contains("\"value\":\"2\""), "a after b's edit: {}", raw);
}

/// The two share a runtime, so state written at the top level is visible
/// inside a thunk and the other way round.
#[test]
fn top_level_and_thunk_share_the_runtime() {
    let id = fumola_create();

    // Written at the top level...
    let raw = fumola_eval_top(id, "`handoff := 7");
    assert!(raw.contains("\"ok\":true"), "top-level write: {}", raw);

    // ...read from inside a thunk.
    let raw = fumola_eval(id, "`reader", "get(`handoff)");
    assert!(raw.contains("\"value\":\"7\""), "read in thunk: {}", raw);

    // And a binding made at the top level is in scope for the thunk.
    let raw = fumola_eval_top(id, "let bound = 3");
    assert!(raw.contains("\"ok\":true"), "binding: {}", raw);
    let raw = fumola_eval(id, "`reader2", "bound + 1");
    assert!(raw.contains("\"value\":\"4\""), "use binding: {}", raw);
}

/// A thunk is named by Fumola source for a symbol, evaluated in a scratch
/// runtime. So every form the language spells as a symbol works, without this
/// crate knowing about any of them.
#[test]
fn a_thunk_is_named_by_a_fumola_symbol() {
    let id = fumola_create();
    for name in ["`myThunk", "7", "`a(`b)", " `spaced "] {
        let raw = fumola_eval(id, name, "1 + 1");
        assert!(
            raw.contains("\"value\":\"2\""),
            "expected {:?} to name a thunk, got {}",
            name,
            raw
        );
    }
}

/// Text that does not denote a symbol is refused, rather than being
/// interpolated into the program.
#[test]
fn a_name_that_is_not_a_symbol_is_refused() {
    let id = fumola_create();
    for bad in ["", "   ", "1 +", "`a}); (", "true"] {
        let raw = fumola_eval(id, bad, "1");
        assert!(
            raw.contains("\"ok\":false"),
            "expected {:?} to be refused, got {}",
            bad,
            raw
        );
    }
    // The instance still works afterwards.
    let raw = fumola_eval(id, "`fine", "1 + 1");
    assert!(raw.contains("\"value\":\"2\""), "{}", raw);
}

/// Naming happens in a runtime of its own, so a name with an effect cannot
/// touch the instance it names a thunk in, nor accumulate between uses.
#[test]
fn naming_a_thunk_has_no_effect_on_the_instance() {
    let id = fumola_create();
    eval_ok(id, "`cell := 1");

    // A name that writes: the write lands in the scratch runtime, not here.
    let raw = fumola_eval(id, "`n := 99", "get(`cell)");
    assert!(raw.contains("\"value\":\"1\""), "cell should be untouched: {}", raw);

    // And the scratch runtime did not keep it either.
    let raw = fumola_symbol_of("get(`n)");
    assert!(raw.contains("\"ok\":false"), "scratch should not accumulate: {}", raw);
}

/// Operations that cannot run inside a force work at the top level. reset
/// clears the store the enclosing force is still inside, and peekForce
/// asserts there.
#[test]
fn adapton_operations_that_need_the_top_level() {
    let id = fumola_create();
    fumola_eval(id, "`setup", "1 := 2");

    let wrapped = fumola_eval(id, "`t", "Adapton.reset()");
    assert!(wrapped.contains("\"ok\":false"), "expected failure: {}", wrapped);

    let top = fumola_eval_top(id, "Adapton.reset()");
    assert!(top.contains("\"ok\":true"), "reset at top level: {}", top);
}

/// Errors carry a kind. A syntax error is what a half-typed program looks
/// like; anything else is a program that parsed and then went wrong, which a
/// host may want to show rather than swallow.
#[test]
fn errors_say_what_kind_they_are() {
    let id = fumola_create();

    let raw = fumola_eval(id, "`t", "1 +");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], serde_json::json!(false));
    assert_eq!(v["kind"], serde_json::json!("syntax"));

    // Parses, then fails: reset cannot run inside a force.
    let raw = fumola_eval(id, "`t", "Adapton.reset()");
    let v: serde_json::Value = serde_json::from_str(&raw).unwrap();
    assert_eq!(v["ok"], serde_json::json!(false));
    assert_eq!(v["kind"], serde_json::json!("runtime"));
    assert!(
        v["error"].as_str().unwrap().contains("Adapton"),
        "the message should describe the failure: {}",
        v["error"]
    );
}
