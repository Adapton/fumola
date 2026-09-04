//! Translation between Fumola symbols and the JSON that Hazel exchanges.
//!
//! Fumola symbols are first-order, inductive data -- not opaque runtime
//! handles. A number is a symbol, an identifier is a symbol, and symbols
//! compose:
//!
//! ```text
//! `x                                            Symbol::Id
//! 1                                             Symbol::Nat
//! `adapton(`settings)(`forceBeginAlwaysMisses)  Symbol::Call, chained
//! `a.`b                                         Symbol::Dot
//! ```
//!
//! Because they are structure rather than handles, they translate in both
//! directions, which is what lets a Hazel program hold a *name* for a Fumola
//! cell and hand it back later.
//!
//! The JSON mirrors the Hazel-side datatype:
//!
//! ```text
//! {"tag": "Num",  "value": "3"}
//! {"tag": "Name", "value": "x"}
//! {"tag": "Call", "fun": <symbol>, "arg": <symbol>}
//! {"tag": "Dot",  "left": <symbol>, "right": <symbol>}
//! ```
//!
//! `UnOp`, `BinOp` and `QuotedAst` are deliberately not translated yet: they
//! are reported as untranslatable rather than approximated.

use fumola_semantics::value::Symbol;
use fumola_syntax::ast::Id;
use fumola_syntax::shared::Shared;
use num_bigint::{BigInt, BigUint};
use serde_json::{json, Value as Json};

/// Translate a Fumola symbol into Hazel-facing JSON.
pub fn symbol_to_json(symbol: &Symbol) -> Result<Json, String> {
    match symbol {
        Symbol::Nat(n) => Ok(json!({"tag": "Num", "value": n.to_string()})),
        Symbol::Int(i) => Ok(json!({"tag": "Num", "value": i.to_string()})),
        Symbol::Id(id) => Ok(json!({"tag": "Name", "value": id.string.to_string()})),
        Symbol::Call(f, a) => Ok(json!({
            "tag": "Call",
            "fun": symbol_to_json(f)?,
            "arg": symbol_to_json(a)?,
        })),
        Symbol::Dot(l, r) => Ok(json!({
            "tag": "Dot",
            "left": symbol_to_json(l)?,
            "right": symbol_to_json(r)?,
        })),
        Symbol::UnOp(..) => Err("symbol uses UnOp, which has no Hazel translation yet".into()),
        Symbol::BinOp(..) => Err("symbol uses BinOp, which has no Hazel translation yet".into()),
        Symbol::QuotedAst(_) => {
            Err("symbol uses QuotedAst, which has no Hazel translation yet".into())
        }
    }
}

fn field<'a>(json: &'a Json, name: &str) -> Result<&'a Json, String> {
    json.get(name)
        .ok_or_else(|| format!("symbol is missing field `{}`", name))
}

/// Translate Hazel-facing JSON into a Fumola symbol.
pub fn symbol_from_json(json: &Json) -> Result<Symbol, String> {
    let tag = json
        .get("tag")
        .and_then(|t| t.as_str())
        .ok_or_else(|| "symbol is missing a string `tag`".to_string())?;
    match tag {
        "Num" => {
            let text = field(json, "value")?
                .as_str()
                .ok_or_else(|| "Num value must be a decimal string".to_string())?;
            // Prefer Nat, which is what an unsigned literal parses to; fall
            // back to Int so that negative numbers still round-trip.
            match text.parse::<BigUint>() {
                Ok(n) => Ok(Symbol::Nat(n)),
                Err(_) => text
                    .parse::<BigInt>()
                    .map(Symbol::Int)
                    .map_err(|_| format!("Num value is not an integer: {}", text)),
            }
        }
        "Name" => {
            let text = field(json, "value")?
                .as_str()
                .ok_or_else(|| "Name value must be a string".to_string())?;
            check_name(text)?;
            Ok(Symbol::Id(Id::new(text.to_string())))
        }
        "Call" => Ok(Symbol::Call(
            Shared::new(symbol_from_json(field(json, "fun")?)?),
            Shared::new(symbol_from_json(field(json, "arg")?)?),
        )),
        "Dot" => Ok(Symbol::Dot(
            Shared::new(symbol_from_json(field(json, "left")?)?),
            Shared::new(symbol_from_json(field(json, "right")?)?),
        )),
        other => Err(format!("unknown symbol tag `{}`", other)),
    }
}

/// Reject names that would not survive a trip through Fumola's lexer. Without
/// this a Hazel string could inject arbitrary text into a generated program.
fn check_name(name: &str) -> Result<(), String> {
    let mut chars = name.chars();
    match chars.next() {
        None => return Err("symbol name is empty".into()),
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        Some(c) => {
            return Err(format!(
                "symbol name must start with a letter or underscore, found `{}`",
                c
            ))
        }
    }
    for c in chars {
        if !(c.is_ascii_alphanumeric() || c == '_') {
            return Err(format!("symbol name contains `{}`, which is not allowed", c));
        }
    }
    Ok(())
}

/// Render a symbol as Fumola source text that parses back to the same symbol.
///
/// This is not `Symbol::doc`: the pretty-printer omits the leading backtick on
/// identifiers, so its output is not re-parseable as a symbol.
pub fn symbol_to_source(symbol: &Symbol) -> Result<String, String> {
    match symbol {
        Symbol::Nat(n) => Ok(n.to_string()),
        Symbol::Int(i) => Ok(i.to_string()),
        Symbol::Id(id) => {
            let name = id.string.to_string();
            check_name(&name)?;
            Ok(format!("`{}", name))
        }
        Symbol::Call(f, a) => Ok(format!(
            "{}({})",
            symbol_to_source(f)?,
            symbol_to_source(a)?
        )),
        Symbol::Dot(l, r) => Ok(format!("{}.{}", symbol_to_source(l)?, symbol_to_source(r)?)),
        Symbol::UnOp(..) | Symbol::BinOp(..) | Symbol::QuotedAst(_) => {
            Err("symbol has no source rendering yet".into())
        }
    }
}
