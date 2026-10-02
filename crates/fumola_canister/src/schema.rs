//! Hazel's own datatypes, for the values whose types the canister knows.
//!
//! Stage 2 stores every saved S-expression as a generic `#atom` / `#list`
//! value. For a key whose OCaml type is written down here, the value is
//! instead decoded into Fumola values shaped like that type -- a record as an
//! object with its field names, a variant as `#Ctor` or `#Ctor(arg)`, an
//! option as `null` / `?x` -- so a program reads `s.core.evaluation` rather
//! than walking lists. A read encodes it back, in the schema's field order.
//!
//! The encodings are `ppx_sexp_conv`'s:
//!
//! ```text
//! bool, int, string       an atom
//! option                  ()  or  (x)
//! list                    (a b ...)
//! tuple                   (a b ...)
//! record                  ((field value) ...)   in declaration order
//! variant                 Ctor  or  (Ctor arg)
//! ```
//!
//! A schema mirrors Hazel's source and can drift from it. So a value is
//! stored typed only when encoding it again gives back exactly the
//! S-expression it came from; otherwise it is stored generically, as before,
//! and still reads back byte for byte.

use crate::sexp::Sexp;
use fumola_semantics::value::{FieldValue, Value, Value_};
use fumola_syntax::ast::{Id, Mut};
use im_rc::{HashMap, Vector};
use num_bigint::BigInt;

pub enum Ty {
    Bool,
    Int,
    Text,
    Option(Box<Ty>),
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Record(Vec<(&'static str, Ty)>),
    Variant(Vec<(&'static str, Option<Ty>)>),
}

fn id(s: &str) -> Id {
    Id::new(s.to_string())
}

fn atom(s: &Sexp) -> Option<&str> {
    match s {
        Sexp::Atom(a) => Some(a),
        _ => None,
    }
}

fn items(s: &Sexp) -> Option<&Vec<Sexp>> {
    match s {
        Sexp::List(xs) => Some(xs),
        _ => None,
    }
}

pub fn decode(ty: &Ty, s: &Sexp) -> Option<Value_> {
    Some(match ty {
        Ty::Bool => match atom(s)? {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => return None,
        }
        .into(),
        Ty::Int => Value::Int(atom(s)?.parse::<BigInt>().ok()?).into(),
        Ty::Text => Value::Text(atom(s)?.into()).into(),
        Ty::Option(t) => match items(s)?.as_slice() {
            [] => Value::Null.into(),
            [x] => Value::Option(decode(t, x)?).into(),
            _ => return None,
        },
        Ty::List(t) => {
            let vals: Option<Vector<Value_>> = items(s)?.iter().map(|x| decode(t, x)).collect();
            Value::Array(Mut::Const, vals?).into()
        }
        Ty::Tuple(ts) => {
            let xs = items(s)?;
            if xs.len() != ts.len() {
                return None;
            }
            let vals: Option<Vector<Value_>> =
                ts.iter().zip(xs).map(|(t, x)| decode(t, x)).collect();
            Value::Tuple(vals?).into()
        }
        Ty::Record(fields) => {
            let xs = items(s)?;
            if xs.len() != fields.len() {
                return None;
            }
            let mut obj = HashMap::new();
            for ((name, t), x) in fields.iter().zip(xs) {
                match items(x)?.as_slice() {
                    [k, v] if atom(k)? == *name => {
                        obj.insert(
                            id(name),
                            FieldValue {
                                mut_: Mut::Const,
                                val: decode(t, v)?,
                            },
                        );
                    }
                    _ => return None,
                }
            }
            Value::Object(obj).into()
        }
        Ty::Variant(ctors) => match s {
            Sexp::Atom(a) => match ctors.iter().find(|(n, arg)| n == a && arg.is_none()) {
                Some((n, _)) => Value::Variant(id(n), None).into(),
                None => return None,
            },
            Sexp::List(xs) => match xs.as_slice() {
                [k, x] => {
                    let name = atom(k)?;
                    match ctors.iter().find(|(n, _)| *n == name)? {
                        (n, Some(t)) => Value::Variant(id(n), Some(decode(t, x)?)).into(),
                        _ => return None,
                    }
                }
                _ => return None,
            },
        },
    })
}

pub fn encode(ty: &Ty, v: &Value) -> Option<Sexp> {
    Some(match (ty, v) {
        (Ty::Bool, Value::Bool(b)) => Sexp::Atom(b.to_string()),
        (Ty::Int, Value::Int(i)) => Sexp::Atom(i.to_string()),
        (Ty::Int, Value::Nat(n)) => Sexp::Atom(n.to_string()),
        (Ty::Text, Value::Text(t)) => Sexp::Atom(t.to_string()),
        (Ty::Option(_), Value::Null) => Sexp::List(vec![]),
        (Ty::Option(t), Value::Option(x)) => Sexp::List(vec![encode(t, x)?]),
        (Ty::List(t), Value::Array(_, xs)) => Sexp::List(
            xs.iter()
                .map(|x| encode(t, x))
                .collect::<Option<Vec<_>>>()?,
        ),
        (Ty::Tuple(ts), Value::Tuple(xs)) if ts.len() == xs.len() => Sexp::List(
            ts.iter()
                .zip(xs.iter())
                .map(|(t, x)| encode(t, x))
                .collect::<Option<Vec<_>>>()?,
        ),
        (Ty::Record(fields), Value::Object(obj)) if obj.len() == fields.len() => Sexp::List(
            fields
                .iter()
                .map(|(name, t)| {
                    let f = obj.get(&id(name))?;
                    Some(Sexp::List(vec![
                        Sexp::Atom(name.to_string()),
                        encode(t, &f.val)?,
                    ]))
                })
                .collect::<Option<Vec<_>>>()?,
        ),
        (Ty::Variant(ctors), Value::Variant(tag, arg)) => {
            let name = tag.string.as_str();
            match (ctors.iter().find(|(n, _)| *n == name)?, arg) {
                ((n, None), None) => Sexp::Atom(n.to_string()),
                ((n, Some(t)), Some(x)) => {
                    Sexp::List(vec![Sexp::Atom(n.to_string()), encode(t, x)?])
                }
                _ => return None,
            }
        }
        _ => return None,
    })
}

/// Decoded only if encoding it again gives back the same S-expression.
pub fn decode_exact(ty: &Ty, s: &Sexp) -> Option<Value_> {
    let v = decode(ty, s)?;
    if encode(ty, &v).as_ref() == Some(s) {
        Some(v)
    } else {
        None
    }
}

// ---- Hazel's types --------------------------------------------------------

fn r(fields: Vec<(&'static str, Ty)>) -> Ty {
    Ty::Record(fields)
}
fn nullary(names: &[&'static str]) -> Ty {
    Ty::Variant(names.iter().map(|n| (*n, None)).collect())
}
fn opt(t: Ty) -> Ty {
    Ty::Option(Box::new(t))
}
fn list(t: Ty) -> Ty {
    Ty::List(Box::new(t))
}
fn bools(names: &[&'static str]) -> Vec<(&'static str, Ty)> {
    names.iter().map(|n| (*n, Ty::Bool)).collect()
}

/// `OpenRouter.AvailableLLMs.Model.llm_info` (src/util/OpenRouter.re).
fn llm_info() -> Ty {
    r(vec![
        ("id", Ty::Text),
        ("name", Ty::Text),
        (
            "pricing",
            r(vec![("prompt", Ty::Text), ("completion", Ty::Text)]),
        ),
        ("context_length", opt(Ty::Int)),
        ("supports_reasoning", Ty::Bool),
    ])
}

/// `Settings.Model.t` (src/web/Settings.re) and the types it holds, in the
/// order Hazel declares, and so writes, their fields.
pub fn settings() -> Ty {
    // Language.CoreSettings.Evaluation.t
    let evaluation = r(bools(&[
        "show_case_clauses",
        "show_fn_bodies",
        "show_fixpoints",
        "show_ascription_steps",
        "show_ascriptions",
        "show_case_steps",
        "show_lookup_steps",
        "show_stepper_filters",
        "stepper_history",
        "show_settings",
        "show_hidden_steps",
        "enable_proof",
        "project_tables",
    ]));
    // Language.CoreSettings.t
    let mut core = bools(&[
        "statics",
        "elaborate",
        "assist",
        "dynamics",
        "probe_all",
        "auto_reindent",
    ]);
    core.push((
        "format_shortcut",
        nullary(&["Nothing", "Indent", "Spaces", "Breaks"]),
    ));
    core.extend(bools(&[
        "indentation_ux",
        "flip_animations",
        "display_warnings",
        "selection_chunkiness",
    ]));
    core.push(("evaluation", evaluation));
    // ExplainThisModel.Settings.t; One carries an Id.t, a UUID atom
    let explain_this = r(vec![
        ("show", Ty::Bool),
        ("show_feedback", Ty::Bool),
        (
            "highlight",
            Ty::Variant(vec![
                ("NoHighlight", None),
                ("One", Some(Ty::Text)),
                ("All", None),
            ]),
        ),
    ]);
    // SidebarModel.Settings.t
    let category = nullary(&["Syntax", "Hole", "Static", "Warning", "Projector"]);
    let sidebar = r(vec![
        ("show", Ty::Bool),
        (
            "panel",
            nullary(&[
                "LanguageDocumentation",
                "HelpfulAssistant",
                "Probes",
                "Projectors",
                "LogControl",
                "Problems",
                "TaskReference",
                "DebugInfo",
            ]),
        ),
        (
            "problems",
            r(vec![
                ("collapsed", list(Ty::Tuple(vec![Ty::Text, category]))),
                ("collapsed_editors", list(Ty::Text)),
                ("flat", Ty::Bool),
                ("expanded", list(Ty::Text)),
            ]),
        ),
        ("debug_show_raw", Ty::Bool),
        ("debug_expanded", list(Ty::Text)),
        (
            "worker_encodings",
            list(nullary(&["Direct", "Marshal", "Sexp"])),
        ),
    ]);
    // AgentGlobals.Model.t
    let agent_globals = r(vec![
        (
            "active_screen",
            nullary(&["MainMenu", "AgentChatInterface"]),
        ),
        ("api_key", opt(Ty::Text)),
        ("active_llm", opt(llm_info())),
        ("available_llms", list(llm_info())),
        ("model_filter", Ty::Text),
        ("only_free_models", Ty::Bool),
        ("reasoning_effort", opt(nullary(&["Low", "Medium", "High"]))),
        ("show_thinking", Ty::Bool),
        ("session_mode", nullary(&["Converse", "Edit", "Plan"])),
        ("collapse_top_bar", Ty::Bool),
    ]);
    let mut fields = bools(&["captions", "secondary_icons"]);
    fields.push(("core", r(core)));
    fields.extend(bools(&[
        "async_evaluation",
        "context_inspector",
        "instructor_mode",
        "benchmark",
        "show_log_panel",
        "show_debug_panel",
    ]));
    fields.push(("explainThis", explain_this));
    fields.push(("sidebar", sidebar));
    fields.extend(bools(&["quiver_flagpole", "quiver"]));
    // Haz3lcore.AutoProbe.t, written by hand as a bare atom
    fields.push(("autoprobe_mode", nullary(&["Off", "Caret", "All"])));
    fields.push(("agent_globals", agent_globals));
    fields.extend(bools(&[
        "line_numbers",
        "relative_line_numbers",
        "cap_undo_stack",
        "show_row_lines",
        "show_incremental_deco",
    ]));
    fields.push((
        "shortcut_overrides",
        list(Ty::Tuple(vec![Ty::Text, opt(Ty::Text)])),
    ));
    fields.push(("simple_indication", Ty::Bool));
    r(fields)
}

/// The schema for a Hazel key, if the canister knows its type.
pub fn for_key(key: &str) -> Option<Ty> {
    match key {
        "SETTINGS" => Some(settings()),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::sexp::{parse, print};

    /// Hazel's SETTINGS as saved in the demo's backend on 2026-10-02.
    const SAVED: &str = "((captions true)(secondary_icons false)(core((statics true)(elaborate false)(assist true)(dynamics true)(probe_all false)(auto_reindent true)(format_shortcut Spaces)(indentation_ux true)(flip_animations true)(display_warnings true)(selection_chunkiness false)(evaluation((show_case_clauses true)(show_fn_bodies false)(show_fixpoints false)(show_ascription_steps false)(show_ascriptions false)(show_case_steps false)(show_lookup_steps false)(show_stepper_filters false)(stepper_history false)(show_settings false)(show_hidden_steps false)(enable_proof false)(project_tables false)))))(async_evaluation false)(context_inspector false)(instructor_mode false)(benchmark false)(show_log_panel false)(show_debug_panel false)(explainThis((show true)(show_feedback false)(highlight NoHighlight)))(sidebar((show true)(panel TaskReference)(problems((collapsed())(collapsed_editors())(flat false)(expanded())))(debug_show_raw false)(debug_expanded())(worker_encodings(Marshal))))(quiver_flagpole false)(quiver true)(autoprobe_mode Off)(agent_globals((active_screen MainMenu)(api_key())(active_llm())(available_llms())(model_filter\"\")(only_free_models false)(reasoning_effort())(show_thinking true)(session_mode Edit)(collapse_top_bar false)))(line_numbers false)(relative_line_numbers false)(cap_undo_stack false)(show_row_lines false)(show_incremental_deco false)(shortcut_overrides())(simple_indication false))";

    #[test]
    fn saved_settings_decode_and_print_back_byte_for_byte() {
        let s = parse(SAVED).unwrap();
        let v = decode_exact(&settings(), &s).expect("SETTINGS decodes");
        assert_eq!(print(&encode(&settings(), &v).unwrap()), SAVED);
    }

    #[test]
    fn options_lists_tuples_and_variants_with_arguments() {
        let with_values = SAVED
            .replace("(api_key())", "(api_key(sk-123))")
            .replace(
                "(active_llm())",
                "(active_llm(((id a/b)(name\"A B\")(pricing((prompt 0)(completion 0)))(context_length(8192))(supports_reasoning true))))",
            )
            .replace("(highlight NoHighlight)", "(highlight(One 3c1a0000-0000-4000-8000-000000000000))")
            .replace("(reasoning_effort())", "(reasoning_effort(High))")
            .replace("(collapsed())", "(collapsed((ed Static)))")
            .replace("(shortcut_overrides())", "(shortcut_overrides((save())(undo(C-z))))");
        let s = parse(&with_values).unwrap();
        let v = decode_exact(&settings(), &s).expect("decodes");
        assert_eq!(print(&encode(&settings(), &v).unwrap()), with_values);
    }

    #[test]
    fn a_value_the_schema_does_not_fit_is_not_decoded() {
        // An extra field (a newer Hazel) or a missing one (an older one).
        let extra = SAVED.replacen("(captions true)", "(captions true)(new_field 1)", 1);
        assert!(decode_exact(&settings(), &parse(&extra).unwrap()).is_none());
        let missing = SAVED.replacen("(captions true)", "", 1);
        assert!(decode_exact(&settings(), &parse(&missing).unwrap()).is_none());
    }
}
