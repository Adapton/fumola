//! JavaScript bindings for incremental Fumola instances.
//!
//! This crate exists to back the Hazel Fumola livelit. The livelit's Hazel-side
//! model is a pair `(instance_id, program_text)`; the runtime that the id names
//! lives here, outside Hazel's value domain, in the `INSTANCES` map below. That
//! map is the store the design doc calls
//!
//! ```text
//! sigma : FumolaInstanceId -> FumolaRuntimeState
//! ```
//!
//! Editing a livelit does *not* build a fresh runtime. It re-evaluates the new
//! program text against the same persistent [`State`], so Fumola's adapton store
//! is carried across edits. The edit itself is expressed in Fumola rather than
//! through any repair API: the source text is wrapped as
//!
//! ```text
//! force(`topLevel := thunk { <program text> })
//! ```
//!
//! Re-assigning the same `topLevel` name and re-forcing it is what gives the
//! edit its incremental meaning; `:=` and `thunk` do the work.

use std::cell::RefCell;
use std::collections::HashMap;

use fumola::state::State;
use fumola_semantics::value::Value;
use fumola_semantics::vm_types::{LocalPointer, Pointer, ScheduleChoice};
use wasm_bindgen::prelude::*;

pub mod symbol;
use symbol::{symbol_from_json, symbol_to_json, symbol_to_source};

/// A Fumola instance id. Opaque to Hazel: locally unique with respect to this
/// module's `INSTANCES` map, not globally persistent.
pub type FumolaInstanceId = u32;

thread_local! {
    /// The external store, sigma.
    static INSTANCES: RefCell<HashMap<FumolaInstanceId, State>> =
        RefCell::new(HashMap::new());
    /// Source of fresh ids. Kept ahead of every id ever realized so that
    /// `create` never collides with an id recovered from a saved program.
    static NEXT_ID: RefCell<FumolaInstanceId> = const { RefCell::new(1) };
}

/// Wrap Hazel-supplied source in the top-level thunk assignment that gives
/// re-evaluation its incremental meaning.
fn wrap(program_text: &str) -> String {
    format!("force(`topLevel := thunk {{ {} }})", program_text)
}

fn bump_next_id_past(id: FumolaInstanceId) {
    NEXT_ID.with(|n| {
        let mut n = n.borrow_mut();
        if *n <= id {
            *n = id + 1;
        }
    });
}

/// Allocate a fresh instance id and realize a runtime for it.
///
/// Ids are generative: duplicating a livelit should call this rather than
/// reusing the original's id, so that two livelits never share one runtime.
#[wasm_bindgen]
pub fn fumola_create() -> FumolaInstanceId {
    let id = NEXT_ID.with(|n| {
        let mut n = n.borrow_mut();
        let id = *n;
        *n += 1;
        id
    });
    INSTANCES.with(|m| m.borrow_mut().insert(id, State::empty()));
    id
}

/// Whether sigma currently has an entry for `id`.
#[wasm_bindgen]
pub fn fumola_has(id: FumolaInstanceId) -> bool {
    INSTANCES.with(|m| m.borrow().contains_key(&id))
}

/// `get_or_realize`: ensure `id` names a runtime, creating an empty one if it
/// does not. This is the reload path — a saved Hazel program may mention
/// `FumolaInstance 17` in a session whose sigma has no entry 17.
///
/// Returns true if a new runtime was realized, false if one already existed.
/// A realized runtime starts with no execution history; the caller is expected
/// to re-evaluate the program text to restore the synchronization invariant
/// between the model's text and sigma(id).
#[wasm_bindgen]
pub fn fumola_realize(id: FumolaInstanceId) -> bool {
    bump_next_id_past(id);
    INSTANCES.with(|m| {
        let mut m = m.borrow_mut();
        if m.contains_key(&id) {
            false
        } else {
            m.insert(id, State::empty());
            true
        }
    })
}

/// Discard the runtime named by `id`.
#[wasm_bindgen]
pub fn fumola_drop(id: FumolaInstanceId) {
    INSTANCES.with(|m| m.borrow_mut().remove(&id));
}

/// How many runtimes sigma currently holds. Exposed for tests and debugging.
#[wasm_bindgen]
pub fn fumola_instance_count() -> usize {
    INSTANCES.with(|m| m.borrow().len())
}

/// Evaluate `program_text` against the runtime named by `id`, and return the
/// current result as JSON.
///
/// On success: `{"ok": true, "tag": "<tag>", "value": <json>}`.
/// On failure: `{"ok": false, "error": "<message>"}`.
///
/// The runtime is *not* rebuilt; the same persistent `State` is reused, so the
/// adapton store survives the edit.
#[wasm_bindgen]
pub fn fumola_eval(id: FumolaInstanceId, program_text: &str) -> String {
    INSTANCES.with(|m| {
        let mut m = m.borrow_mut();
        let state = match m.get_mut(&id) {
            Some(state) => state,
            None => return error_json(&format!("no Fumola instance with id {}", id)),
        };
        // Mirrors the REPL: without this, a continuation left stuck by an
        // earlier edit would still be in place and the next eval would try to
        // resume it instead of running the new program.
        state.semantic_state.clear_cont();
        match state.eval(&wrap(program_text)) {
            Ok(value) => value_to_json(&value),
            Err(e) => error_json(&format!("{:?}", e)),
        }
    })
}

fn error_json(message: &str) -> String {
    serde_json::json!({ "ok": false, "error": message }).to_string()
}

/// Translate a Fumola value into the JSON that the Hazel side decodes.
///
/// Only the first-order cases the MVP needs are translated. Anything else is
/// reported as an untranslatable tag rather than being silently coerced;
/// values whose meaning depends on the runtime (functions, thunks, pointers)
/// would need opaque handles back into this instance, which is out of scope.
fn value_to_json(value: &Value) -> String {
    let translated = match value {
        Value::Nat(n) => Ok(Some(("Int", serde_json::json!(n.to_string())))),
        Value::Int(i) => Ok(Some(("Int", serde_json::json!(i.to_string())))),
        Value::Bool(b) => Ok(Some(("Bool", serde_json::json!(b)))),
        Value::Text(t) => Ok(Some(("String", serde_json::json!(t.to_string())))),
        Value::Unit => Ok(Some(("Unit", serde_json::Value::Null))),
        // Symbols are first-order data, so they cross the boundary as
        // structure rather than as a handle into this runtime.
        //
        // The coercion is Fumola's own (`into_sym_or`) rather than a match on
        // Value::Symbol, because a symbol written in expression position --
        // `x -- evaluates to a QuotedAst and only becomes a Symbol when
        // something needs one. Matching the variant would miss the common
        // case. Numeric and textual values are handled above, so this arm
        // sees only values whose point is to be a name.
        _ if value.into_sym_or(()).is_ok() => {
            let symbol = value.into_sym_or(()).expect("just checked");
            symbol_to_json(&symbol).map(|j| Some(("Symbol", j)))
        }
        // A pointer is a name that has been allocated in the store. It
        // travels outward so a Hazel program can see which cell it is
        // looking at; there is no surface syntax for injecting a raw
        // pointer back, so cells are addressed by their symbol instead.
        Value::Pointer(p) | Value::Opaque(p) => Ok(Some(("Pointer", pointer_to_json(p)))),
        _ => Ok(None),
    };
    match translated {
        Ok(Some((tag, value))) => {
            serde_json::json!({ "ok": true, "tag": tag, "value": value }).to_string()
        }
        Ok(None) => error_json("Fumola value has no Hazel translation"),
        Err(message) => error_json(&message),
    }
}

fn pointer_to_json(pointer: &Pointer) -> serde_json::Value {
    let owner = match &pointer.owner {
        ScheduleChoice::Agent => serde_json::json!("Agent"),
        ScheduleChoice::Actor(id) => serde_json::json!({ "Actor": format!("{:?}", id) }),
    };
    let local = match &pointer.local {
        LocalPointer::Numeric(n) => serde_json::json!({ "Numeric": n.0 }),
        LocalPointer::Named(name) => {
            serde_json::json!({ "Named": name.0.string.to_string() })
        }
    };
    serde_json::json!({ "owner": owner, "local": local })
}

/// Render a symbol, given as JSON, into the Fumola source text that denotes
/// it. Exposed so the Hazel side can check a symbol it built before using it.
#[wasm_bindgen]
pub fn fumola_symbol_source(symbol_json: &str) -> String {
    match parse_symbol(symbol_json).and_then(|s| symbol_to_source(&s)) {
        Ok(source) => serde_json::json!({ "ok": true, "source": source }).to_string(),
        Err(message) => error_json(&message),
    }
}

fn parse_symbol(symbol_json: &str) -> Result<fumola_semantics::value::Symbol, String> {
    let json: serde_json::Value =
        serde_json::from_str(symbol_json).map_err(|e| format!("invalid symbol JSON: {}", e))?;
    symbol_from_json(&json)
}

/// Read the cell named by `symbol_json` in the runtime named by `id`, and
/// return its current value the same way `fumola_eval` does.
///
/// This is the engine behind addressing Fumola's store from a Hazel program:
/// Hazel builds a symbol, and gets back whatever that name currently holds.
///
/// `@` dereferences a pointer, not a symbol, so the symbol is converted
/// explicitly with `prim "adaptonPointer"`. (Fumola coerces implicitly where
/// a symbol is required, as `:=` does, but a read is not such a position.)
/// The read goes through `@` rather than `adaptonPeek` so that it records a
/// dependency, which is the point of reading from an incremental store.
#[wasm_bindgen]
pub fn fumola_get(id: FumolaInstanceId, symbol_json: &str) -> String {
    let source = match parse_symbol(symbol_json).and_then(|s| symbol_to_source(&s)) {
        Ok(source) => source,
        Err(message) => return error_json(&message),
    };
    INSTANCES.with(|m| {
        let mut m = m.borrow_mut();
        let state = match m.get_mut(&id) {
            Some(state) => state,
            None => return error_json(&format!("no Fumola instance with id {}", id)),
        };
        state.semantic_state.clear_cont();
        match state.eval(&format!("@ (prim \"adaptonPointer\" ({}))", source)) {
            Ok(value) => value_to_json(&value),
            Err(e) => error_json(&format!("{:?}", e)),
        }
    })
}
