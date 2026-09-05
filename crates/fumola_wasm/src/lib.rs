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
use fumola_semantics::adapton::{Space, Time};
use fumola_semantics::value::Value;
use fumola_syntax::ast::Id;
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
    /// Which Adapton semantics each instance is running.
    ///
    /// Recorded rather than queried so that asking for the mode an instance
    /// already has can be a no-op. A Hazel livelit re-expands on every edit,
    /// and a reset is destructive -- it would discard the execution history
    /// that other livelits sharing the instance have built up.
    static MODES: RefCell<HashMap<FumolaInstanceId, String>> =
        RefCell::new(HashMap::new());
}

include!(concat!(env!("OUT_DIR"), "/modules.rs"));

/// The Fumola library modules bound at the top level of every instance.
///
/// Chosen from what `fumola/` actually defines and what its own scripts use.
/// The paths are the canonical homes: `adapton` lives in `system/` and is
/// symlinked into `collections/`, `hashMap` the other way around, and the
/// whole of `examples/mergeSort/` is symlinks -- so a name is bound once,
/// from the file that really holds it.
static PRELUDE_MODULES: &[(&str, &str)] = &[
    ("Adapton", "fumola/system/adapton"),
    ("List", "fumola/collections/List"),
    ("LazyList", "fumola/collections/LazyList"),
    ("PureList", "fumola/collections/pureList"),
    ("LevelTree", "fumola/collections/levelTree"),
    ("HashMap", "fumola/collections/hashMap"),
    ("Counters", "fumola/collections/Counters"),
    ("RandomInput", "fumola/collections/randomInput"),
    ("MergeSort", "fumola/examples/mergeSort/mergeSort"),
    ("Gcd", "fumola/examples/gcd"),
    ("DelayedPut", "fumola/examples/delayedPut"),
];

/// A new runtime with the Fumola library registered and its modules bound.
///
/// Done once, when the instance is made, rather than in the per-evaluation
/// prelude: top-level bindings persist across evaluations in one State, so
/// paying for this on every keystroke would be waste.
///
/// Every module is registered, including the symlinked duplicates, because a
/// module's imports resolve relative to its own directory. Only the names in
/// PRELUDE_MODULES are bound; the rest are reachable by importing them.
fn new_state() -> State {
    new_state_with(DEFAULT_MODE)
}

/// The semantics a fresh instance runs unless a program asks for another.
///
/// Simple, not Adapton's graphical default: two incremental layers meet in
/// the Hazel integration, and until how they compose is understood, the
/// predictable one is the better default. Intended to become "graphical",
/// matching Fumola itself, with simple asked for explicitly.
pub const DEFAULT_MODE: &str = "simple";

fn mode_is_known(mode: &str) -> bool {
    mode == "simple" || mode == "graphical"
}

fn new_state_with(mode: &str) -> State {
    let mut state = State::empty();
    for (path, source) in MODULES {
        if let Err(e) = state.set_module(None, path.to_string(), source) {
            // A library module that does not load is worth knowing about, but
            // it must not stop the instance from existing: a program that
            // never touches it should still run.
            let _ = e;
        }
    }
    if let Err(e) = state.eval(PRELUDE) {
        let _ = e;
    }
    for (name, path) in PRELUDE_MODULES {
        let binding = format!("import {} \"{}\";", name, path);
        if let Err(e) = state.eval(&binding) {
            let _ = e;
        }
    }
    // Set the Adapton semantics last, so that nothing the prelude did leaves
    // state from the other mode behind. See DEFAULT_MODE above for why the
    // default is not Adapton's own.
    if let Err(e) = state.eval(&format!("prim \"adaptonReset\" (#{})", mode)) {
        let _ = e;
    }
    state
}

/// Definitions made available to every program the livelit runs.
///
/// These exist because a livelit's program text is stored as a Hazel string
/// literal, and Hazel strings have no escapes -- `Token.is_string` allows at
/// most two quote characters in the whole token. So `prim "adaptonPointer"`
/// cannot be written in a livelit at all, and every adapton primitive needs
/// one.
///
/// `get` also papers over an asymmetry that is easy to trip on: `:=` coerces
/// its left side into a pointer, but `@` does not -- it requires something
/// that is already a pointer. So reading back what `1 := 2` wrote is not
/// `@(1)`; the symbol has to be converted first. `get` does that conversion,
/// so a program can write `1 := 2` in one edit and `get(1)` in the next.
const PRELUDE: &str = concat!(
    r#"func pointer(s) { prim "adaptonPointer" (s) }; "#,
    r#"func get(s) { @(prim "adaptonPointer" (s)) }; "#,
    r#"func peek(s) { prim "adaptonPeek" (prim "adaptonPointer" (s)) }; "#,
);

/// Wrap Hazel-supplied source in the top-level thunk assignment that gives
/// re-evaluation its incremental meaning, after the prelude.
fn wrap(thunk_symbol: &str, program_text: &str) -> String {
    format!("force({} := thunk {{ {} }})", thunk_symbol, program_text)
}

thread_local! {
    /// A runtime kept only for turning text into symbols.
    ///
    /// Fumola already knows how to parse a symbol, so a host does not need to
    /// encode one: it sends the text and this evaluates it. Separate from any
    /// real instance, and evaluated against a throwaway copy, so a name that
    /// happens to write to a cell cannot touch anything that matters or
    /// accumulate here. It holds no module library, since naming a symbol
    /// needs none.
    static SCRATCH: RefCell<State> = RefCell::new(State::empty());
}

/// The Fumola symbol denoted by `source`, rendered back as source text.
///
/// Going through the runtime rather than parsing here means a host writes a
/// symbol the way Fumola spells one -- `myThunk, 7, `a(`b) -- and every form
/// the language supports works without this crate knowing about it. The
/// rendering is checked on the way out, so nothing a host writes can inject
/// text into a program.
fn symbol_source_of(source: &str) -> Result<String, String> {
    if source.trim().is_empty() {
        return Err("a thunk needs a name".to_string());
    }
    SCRATCH.with(|scratch| {
        let mut attempt = scratch.borrow().clone();
        attempt.semantic_state.clear_cont();
        match attempt.eval(source) {
            Ok(value) => match value.into_sym_or(()) {
                Ok(symbol) => symbol_to_source(&symbol),
                Err(()) => Err(format!("`{}` does not name a symbol", source)),
            },
            Err(e) => Err(format!("`{}` is not a symbol: {:?}", source, e)),
        }
    })
}

/// Exposed so a host can tell whether a name is usable before running
/// anything with it.
#[wasm_bindgen]
pub fn fumola_symbol_of(source: &str) -> String {
    match symbol_source_of(source) {
        Ok(rendered) => {
            serde_json::json!({ "ok": true, "source": rendered }).to_string()
        }
        Err(message) => error_json(&message),
    }
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
    INSTANCES.with(|m| m.borrow_mut().insert(id, new_state()));
    MODES.with(|m| m.borrow_mut().insert(id, DEFAULT_MODE.to_string()));
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
            m.insert(id, new_state());
            MODES.with(|d| d.borrow_mut().insert(id, DEFAULT_MODE.to_string()));
            true
        }
    })
}

/// Ensure `id` names a runtime running `mode`, and say what happened.
///
/// This is what the `fumola_new` livelit calls. It is idempotent on purpose:
/// a livelit re-expands on every edit, and asking for the mode an instance
/// already runs must not reset it -- a reset discards the execution history
/// that the put_force and eval livelits sharing this instance have built.
///
/// Changing the mode of an existing instance does reset it, since that is
/// what asking for a different semantics means.
///
/// The reset runs at the top level, never inside a force: a reset is an
/// editor operation and asking for one from inside a DCG computation is not
/// a meaningful request. Adapton refuses it with UnreachableForceEnd.
#[wasm_bindgen]
pub fn fumola_ensure_mode(id: FumolaInstanceId, mode: &str) -> String {
    if !mode_is_known(mode) {
        return error_json_of_kind(
            "syntax",
            &format!("unknown Adapton semantics: {}", mode),
        );
    }
    bump_next_id_past(id);

    let existed = INSTANCES.with(|m| m.borrow().contains_key(&id));
    if !existed {
        INSTANCES.with(|m| m.borrow_mut().insert(id, new_state_with(mode)));
        MODES.with(|m| m.borrow_mut().insert(id, mode.to_string()));
        return serde_json::json!({
            "ok": true, "mode": mode, "created": true, "reset": false
        })
        .to_string();
    }

    let current = MODES.with(|m| m.borrow().get(&id).cloned());
    if current.as_deref() == Some(mode) {
        return serde_json::json!({
            "ok": true, "mode": mode, "created": false, "reset": false
        })
        .to_string();
    }

    let reset = INSTANCES.with(|m| {
        let mut m = m.borrow_mut();
        match m.get_mut(&id) {
            None => false,
            Some(state) => state
                .eval(&format!("prim \"adaptonReset\" (#{})", mode))
                .is_ok(),
        }
    });
    if reset {
        MODES.with(|m| m.borrow_mut().insert(id, mode.to_string()));
    }
    serde_json::json!({
        "ok": reset, "mode": mode, "created": false, "reset": reset
    })
    .to_string()
}

/// The semantics `id` is running, or null if there is no such instance.
#[wasm_bindgen]
pub fn fumola_mode(id: FumolaInstanceId) -> String {
    match MODES.with(|m| m.borrow().get(&id).cloned()) {
        Some(mode) => serde_json::json!({"ok": true, "mode": mode}).to_string(),
        None => serde_json::json!({"ok": true, "mode": null}).to_string(),
    }
}

/// Discard the runtime named by `id`.
#[wasm_bindgen]
pub fn fumola_drop(id: FumolaInstanceId) {
    INSTANCES.with(|m| m.borrow_mut().remove(&id));
    MODES.with(|m| m.borrow_mut().remove(&id));
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
/// Evaluate `program_text` as the body of a named thunk, which is what gives
/// an edit its incremental meaning: re-assigning the same name and forcing it
/// again reuses that thunk's execution history.
///
/// The name distinguishes one thunk from another. Two programs sharing a
/// runtime must not share a name, or each would overwrite the other's thunk
/// and neither would keep its history.
#[wasm_bindgen]
pub fn fumola_eval(
    id: FumolaInstanceId,
    thunk_name: &str,
    program_text: &str,
) -> String {
    match symbol_source_of(thunk_name) {
        Ok(symbol) => eval_in(id, &wrap(&symbol, program_text)),
        Err(message) => error_json(&message),
    }
}

/// Evaluate `program_text` at the top level of the runtime named by `id`,
/// with no thunk around it.
///
/// The wrapping that `fumola_eval` does is what gives an edit its incremental
/// meaning, but it also puts the program inside a `force`, and some adapton
/// operations cannot run there: `reset` clears the store the enclosing force
/// is still inside, and `peekForce` asserts. Those belong at the top level,
/// as do `import` and any binding meant to outlive the program that made it.
///
/// The same runtime either way, so a program evaluated here is visible to one
/// evaluated in a thunk, and the two can be used together.
#[wasm_bindgen]
pub fn fumola_eval_top(id: FumolaInstanceId, program_text: &str) -> String {
    eval_in(id, program_text)
}

fn eval_in(id: FumolaInstanceId, program: &str) -> String {
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

        // Evaluated against a copy, which replaces the original only if it
        // succeeds. Some failures leave the runtime unusable rather than
        // merely unchanged -- an AssertionFailure inside a force was observed
        // to lose every top-level binding in the instance, so a program that
        // failed once made all later programs fail too. An edit that does not
        // work should cost nothing.
        let mut attempt = state.clone();
        match attempt.eval(program) {
            Ok(value) => {
                let json = value_to_json(&value);
                *state = attempt;
                json
            }
            Err(e) => error_of(&e),
        }
    })
}

fn error_json(message: &str) -> String {
    error_json_of_kind("runtime", message)
}

/// Errors carry a kind, because a host wants to treat them differently. A
/// syntax error is what a half-typed program looks like and is worth staying
/// quiet about; anything else is a program that parsed and then went wrong,
/// which is worth saying out loud.
fn error_json_of_kind(kind: &str, message: &str) -> String {
    serde_json::json!({ "ok": false, "kind": kind, "error": message }).to_string()
}

fn error_of(e: &fumola::Error) -> String {
    match e {
        fumola::Error::SyntaxError(_) | fumola::Error::SyntaxErrorCode(_) => {
            error_json_of_kind("syntax", &format!("{:?}", e))
        }
        _ => error_json_of_kind("runtime", &format!("{:?}", e)),
    }
}

/// Translate a Fumola value into the JSON that the Hazel side decodes.
///
/// Only the first-order cases the MVP needs are translated. Anything else is
/// reported as an untranslatable tag rather than being silently coerced;
/// values whose meaning depends on the runtime (functions, thunks, pointers)
/// would need opaque handles back into this instance, which is out of scope.
fn value_to_json(value: &Value) -> String {
    match translate(value) {
        Ok(Some((tag, value))) => {
            serde_json::json!({ "ok": true, "tag": tag, "value": value }).to_string()
        }
        Ok(None) => error_json("Fumola value has no Hazel translation"),
        Err(message) => error_json(&message),
    }
}

type Translated = Result<Option<(&'static str, serde_json::Value)>, String>;

/// Translate a value that sits inside another one, as `{tag, value}`. An
/// untranslatable component fails the whole translation rather than being
/// quietly replaced, so a tuple is never reported as though it were complete
/// when part of it was dropped.
fn nested(value: &Value) -> Result<serde_json::Value, String> {
    match translate(value)? {
        Some((tag, value)) => Ok(serde_json::json!({"tag": tag, "value": value})),
        None => Err("Fumola value has no Hazel translation".to_string()),
    }
}

fn translate(value: &Value) -> Translated {
    match value {
        Value::Nat(n) => Ok(Some(("Int", serde_json::json!(n.to_string())))),
        Value::Int(i) => Ok(Some(("Int", serde_json::json!(i.to_string())))),
        // As text, like the integers above: a JSON number would go through a
        // f64 round trip in the serializer, and Hazel parses the text anyway.
        Value::Float(f) => Ok(Some(("Float", serde_json::json!(f.0.to_string())))),
        Value::Bool(b) => Ok(Some(("Bool", serde_json::json!(b)))),
        Value::Text(t) => Ok(Some(("String", serde_json::json!(t.to_string())))),
        Value::Unit => Ok(Some(("Unit", serde_json::Value::Null))),
        // `peek` answers null for a name that was never written, and ?v for
        // one that was, so both turn up whenever a program peeks.
        Value::Null => Ok(Some(("Null", serde_json::Value::Null))),
        Value::Option(inner) => Ok(Some(("Option", nested(inner)?))),
        // Structural values cross the boundary as structure, so that a Fumola
        // tuple arrives in Hazel as a Hazel tuple rather than as something
        // Hazel has to take apart.
        // Arrays reach Hazel as lists. Adapton's own introspection returns
        // them -- peekEvents, and the edge lists inside peekInfo -- so
        // without this most of what the Adapton module reports is
        // untranslatable.
        Value::Array(_, items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items.iter() {
                out.push(nested(item)?);
            }
            Ok(Some(("List", serde_json::Value::Array(out))))
        }
        Value::Tuple(items) => {
            let mut out = Vec::with_capacity(items.len());
            for item in items.iter() {
                out.push(nested(item)?);
            }
            Ok(Some(("Tuple", serde_json::Value::Array(out))))
        }
        Value::Object(fields) => {
            // Sorted by field name: Fumola holds these in a HashMap, whose
            // iteration order is not stable, and a record whose fields came
            // back in a different order on each evaluation would make the
            // livelit's result flicker.
            let mut names: Vec<&Id> = fields.keys().collect();
            names.sort_by(|a, b| a.string.cmp(&b.string));
            let mut out = serde_json::Map::new();
            for name in names {
                let field = fields.get(name).expect("key from this map");
                out.insert(name.string.to_string(), nested(&field.val)?);
            }
            Ok(Some(("Record", serde_json::Value::Object(out))))
        }
        Value::Variant(name, payload) => Ok(Some((
            "Variant",
            serde_json::json!({
                "name": name.string.to_string(),
                "value": match payload {
                    Some(v) => nested(v)?,
                    None => serde_json::Value::Null,
                },
            }),
        ))),
        // A pointer names a cell in this runtime's store. It travels as the
        // source text of the symbol that names it, so the host can build the
        // expression that reads it -- get(`x) -- rather than receiving an
        // opaque handle it can do nothing with.
        //
        // Matched before the symbol arm below, which would otherwise claim
        // it: `into_sym_or` turns an AdaptonPointer into the symbol it was
        // allocated from, so a pointer would arrive indistinguishable from a
        // plain symbol and lose the fact that it points at anything.
        Value::AdaptonPointer(_) => match value.into_sym_or(()) {
            Ok(symbol) => {
                let source = symbol_to_source(&symbol)?;
                Ok(Some((
                    "AdaptonPointer",
                    serde_json::json!({
                        "source": source,
                        "symbol": symbol_to_json(&symbol)?,
                    }),
                )))
            }
            // A pointer whose space is not a symbol cannot be named, so
            // there is no expression that would read it.
            Err(()) => Err("Fumola pointer has no symbolic name".to_string()),
        },
        // A space and a time are each mostly just a symbol, and cross as a
        // variant of one -- matching how Fumola defines them, so a host can
        // give them types of the same shape and take them apart. The symbol
        // travels as a symbol, and so arrives as its text.
        //
        // Space's third case, an expression rather than a name, is left
        // untranslated: there is no use for it on the other side.
        Value::AdaptonSpace(space) => match space {
            Space::Symbol(symbol) => Ok(Some((
                "Variant",
                serde_json::json!({
                    "name": "Symbol",
                    "value": {"tag": "Symbol", "value": symbol_to_json(symbol)?},
                }),
            ))),
            Space::Here => Ok(Some((
                "Variant",
                serde_json::json!({"name": "Here", "value": serde_json::Value::Null}),
            ))),
            Space::Exp_(..) => Err(
                "this adapton space is an expression rather than a name, and \
                 has no translation"
                    .to_string(),
            ),
        },
        Value::AdaptonTime(time) => match time {
            Time::Symbol(symbol) => Ok(Some((
                "Variant",
                serde_json::json!({
                    "name": "Symbol",
                    "value": {"tag": "Symbol", "value": symbol_to_json(symbol)?},
                }),
            ))),
            Time::Now => Ok(Some((
                "Variant",
                serde_json::json!({"name": "Now", "value": serde_json::Value::Null}),
            ))),
        },
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
            Err(e) => error_of(&e),
        }
    })
}
