//! A host's values in, as values: JSON read straight into Fumola values.
//!
//! Without this the only way in is source text: a host that passes a map to a
//! function writes the map out as a Fumola literal, which is then parsed and
//! stepped back into a value at every call. A 64x64 map is 281,427 characters
//! that way, and most of the call's 195,144 steps go to rebuilding it
//! (Matthew's Hazel reports, ^report(27) and ^report(29)). Read from JSON it is
//! a parse and one walk, with no steps.
//!
//! How JSON reads, the way Hazel's JS library reads a JS value:
//!
//! - a number: a `Nat` when it is a whole number from 0 up, an `Int` when a
//!   whole number below 0, a `Float` otherwise (`3` is a Nat, `3.0` a Float);
//! - text, `true` and `false`, and `null` (Fumola's null);
//! - an array: an array, `[...]`;
//! - an object whose keys are `tag` (text) and, optionally, `value`: a variant,
//!   `#tag` or `#tag(value)`, so `{"tag": "Way", "value": "E"}` is
//!   `#Way("E")`;
//! - any other object: a record, `{ field = ...; ... }`.
//!
//! Arrays and records come in immutable, as their literals make them.
//! `serde_json` stops at 128 levels of nesting, so a value read here is never
//! deeper than that.

use fumola_semantics::value::{FieldValue, Text, Value, Value_};
use fumola_semantics::Share;
use fumola_syntax::ast::{Id, Mut};
use im_rc::{HashMap, Vector};
use num_bigint::{BigInt, BigUint};
use ordered_float::OrderedFloat;

/// The Fumola value a JSON text stands for.
pub fn value_of_json_text(json: &str) -> Result<Value_, String> {
    let j: serde_json::Value =
        serde_json::from_str(json).map_err(|e| format!("not JSON: {}", e))?;
    value_of_json(&j)
}

/// The Fumola value a JSON value stands for.
pub fn value_of_json(j: &serde_json::Value) -> Result<Value_, String> {
    use serde_json::Value as J;
    let v = match j {
        J::Null => Value::Null,
        J::Bool(b) => Value::Bool(*b),
        J::Number(n) => {
            if let Some(u) = n.as_u64() {
                Value::Nat(BigUint::from(u))
            } else if let Some(i) = n.as_i64() {
                Value::Int(BigInt::from(i))
            } else {
                match n.as_f64() {
                    Some(f) => Value::Float(OrderedFloat(f)),
                    None => return Err(format!("a number Fumola cannot hold: {}", n)),
                }
            }
        }
        J::String(s) => Value::Text(Text::new(s.clone())),
        J::Array(items) => {
            let mut out = Vector::new();
            for item in items {
                out.push_back(value_of_json(item)?);
            }
            Value::Array(Mut::Const, out)
        }
        J::Object(fields) => match variant_of(fields) {
            Some((tag, payload)) => {
                let payload = match payload {
                    Some(p) => Some(value_of_json(p)?),
                    None => None,
                };
                Value::Variant(Id::new(tag.to_string()), payload)
            }
            None => {
                let mut out = HashMap::new();
                for (name, field) in fields {
                    out.insert(
                        Id::new(name.clone()),
                        FieldValue {
                            mut_: Mut::Const,
                            val: value_of_json(field)?,
                        },
                    );
                }
                Value::Object(out)
            }
        },
    };
    Ok(v.share())
}

/// `{"tag": t}` or `{"tag": t, "value": v}` with `t` text: a variant's name
/// and payload. Any other key makes the object a record.
fn variant_of(
    fields: &serde_json::Map<String, serde_json::Value>,
) -> Option<(&str, Option<&serde_json::Value>)> {
    let tag = fields.get("tag")?.as_str()?;
    match fields.len() {
        1 => Some((tag, None)),
        2 => fields.get("value").map(|v| (tag, Some(v))),
        _ => None,
    }
}

/// Whether `name` can be written in a Fumola program to mean what is bound to
/// it: a letter or `_`, then letters, digits and `_`.
pub fn is_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}
