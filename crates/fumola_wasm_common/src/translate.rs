//! A Fumola value as the JSON the Hazel side decodes.
//!
//! Shared with fumola_canister, whose `/eval.json` answers in
//! this same shape so that Hazel turns a remote result into a Hazel value
//! exactly as it does a local one. Nothing here may depend on wasm-bindgen:
//! a canister that imported its glue would not install.

use fumola_semantics::adapton::{Space, Time};
use fumola_semantics::format::format_one_line;
use fumola_semantics::value::Value;
use fumola_semantics::vm_types::{LocalPointer, Pointer, ScheduleChoice};
use fumola_syntax::ast::Id;

use crate::symbol::{symbol_to_json, symbol_to_source};

fn untranslatable(message: &str) -> String {
    serde_json::json!({ "ok": false, "kind": "runtime", "error": message }).to_string()
}

/// Translate a Fumola value into the JSON that the Hazel side decodes.
///
/// Only the first-order cases the MVP needs are translated. Anything else is
/// reported as an untranslatable tag rather than being silently coerced;
/// values whose meaning depends on the runtime (functions, thunks, pointers)
/// would need opaque handles back into this instance, which is out of scope.
pub fn value_to_json(value: &Value) -> String {
    match translate(value) {
        Ok(Some((tag, value))) => {
            serde_json::json!({ "ok": true, "tag": tag, "value": value }).to_string()
        }
        Ok(None) => untranslatable("Fumola value has no Hazel translation"),
        Err(message) => untranslatable(&message),
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
        // Everything else crosses as opaque: Hazel has no value of its own
        // for it, so it carries Fumola's printed form instead.
        //
        // Not an error, which is what this used to be. A thunk is a perfectly
        // good Fumola value and "@thunk ({ Gcd.gcd(12, 18) })" says exactly
        // what it is; failing the translation instead meant that a tuple
        // holding one failed entirely, and the host had nothing to show but
        // an error where a value should be.
        _ => Ok(Some(("Opaque", serde_json::json!(format_one_line(value))))),
    }
}

pub fn pointer_to_json(pointer: &Pointer) -> serde_json::Value {
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
