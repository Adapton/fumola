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
    /// An OCaml float, printed back as Sexplib prints it (`ocaml_float`).
    Float,
    Text,
    Option(Box<Ty>),
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Record(Vec<(&'static str, Ty)>),
    /// A record whose fields may be left out when they hold their default,
    /// written as an S-expression (`[@sexp.default d] [@sexp_drop_default]`):
    /// a field with `Some(d)` decodes to `d` when absent, and is left out
    /// when encoding gives exactly `d`.
    Fields(Vec<(&'static str, Ty, Option<&'static str>)>),
    Variant(Vec<(&'static str, Option<Ty>)>),
    /// Any constructor, by name: `Ctor` is `#Ctor`, `(Ctor x)` is `#Ctor(x)`,
    /// `(Ctor x y)` is `#Ctor((x, y))`, the arguments open enums too. For a
    /// type with too many constructors to copy (Hazel's form families), or
    /// one that grows often; Fumola's variants need no declaration.
    Enum,
    /// A named type, built once: how a type refers to itself.
    Lazy(fn() -> &'static Ty),
    /// A part whose type is not written down here: kept as the generic
    /// `#atom` / `#list` value (`sexp::to_value`), so the record around it
    /// can still be typed.
    Any,
}

/// OCaml's `%.<p>G`: the shorter of fixed and exponent notation, at `p`
/// significant digits, trailing zeros dropped, exponent as `E+NN`.
fn format_g(x: f64, p: usize) -> String {
    if x == 0.0 {
        return if x.is_sign_negative() {
            "-0".into()
        } else {
            "0".into()
        };
    }
    if x.is_nan() {
        return "NAN".into();
    }
    if x.is_infinite() {
        return if x < 0.0 { "-INF".into() } else { "INF".into() };
    }
    let sci = format!("{:.*e}", p - 1, x);
    let (mant, exp) = sci.split_once('e').unwrap_or((&sci, "0"));
    let exp: i32 = exp.parse().unwrap_or(0);
    let trim = |s: &str| -> String {
        if s.contains('.') {
            s.trim_end_matches('0').trim_end_matches('.').to_string()
        } else {
            s.to_string()
        }
    };
    if exp < -4 || exp >= p as i32 {
        format!(
            "{}E{}{:02}",
            trim(mant),
            if exp < 0 { '-' } else { '+' },
            exp.abs()
        )
    } else {
        trim(&format!("{:.*}", (p as i32 - 1 - exp).max(0) as usize, x))
    }
}

/// How Sexplib prints a float: `%.15G` if that reads back as the same
/// float, else `%.17G`.
pub fn ocaml_float(x: f64) -> String {
    let short = format_g(x, 15);
    if short.parse::<f64>().ok() == Some(x) {
        short
    } else {
        format_g(x, 17)
    }
}

fn capitalized(a: &str) -> bool {
    a.chars().next().map_or(false, |c| c.is_ascii_uppercase())
}

fn decode_enum(s: &Sexp) -> Option<Value_> {
    Some(match s {
        Sexp::Atom(a) if capitalized(a) => Value::Variant(id(a), None).into(),
        Sexp::List(xs) => match xs.as_slice() {
            [Sexp::Atom(c), arg] if capitalized(c) => {
                Value::Variant(id(c), Some(decode_enum(arg)?)).into()
            }
            [Sexp::Atom(c), args @ ..] if capitalized(c) && args.len() >= 2 => {
                let vals: Option<Vector<Value_>> = args.iter().map(decode_enum).collect();
                Value::Variant(id(c), Some(Value::Tuple(vals?).into())).into()
            }
            _ => return None,
        },
        _ => return None,
    })
}

fn encode_enum(v: &Value) -> Option<Sexp> {
    match v {
        Value::Variant(tag, None) => Some(Sexp::Atom(tag.string.to_string())),
        Value::Variant(tag, Some(arg)) => {
            let mut out = vec![Sexp::Atom(tag.string.to_string())];
            match &**arg {
                Value::Tuple(xs) if xs.len() >= 2 => {
                    for x in xs.iter() {
                        out.push(encode_enum(x)?);
                    }
                }
                other => out.push(encode_enum(other)?),
            }
            Some(Sexp::List(out))
        }
        _ => None,
    }
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
        // A non-negative int is a Nat, as Fumola's own numeric literals are,
        // so a program can compare it with one: Nat 16 is not Int 16.
        Ty::Int => {
            let i = atom(s)?.parse::<BigInt>().ok()?;
            match i.to_biguint() {
                Some(n) => Value::Nat(n).into(),
                None => Value::Int(i).into(),
            }
        }
        Ty::Float => Value::Float(atom(s)?.parse::<f64>().ok()?.into()).into(),
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
        Ty::Fields(fields) => {
            let xs = items(s)?;
            let mut j = 0;
            let mut obj = HashMap::new();
            for (name, t, default) in fields {
                let present = match xs.get(j).and_then(items).map(|kv| kv.as_slice()) {
                    Some([k, v]) if atom(k) == Some(*name) => Some(v),
                    _ => None,
                };
                let val = match (present, default) {
                    (Some(v), _) => {
                        j += 1;
                        decode(t, v)?
                    }
                    (None, Some(d)) => decode(t, &crate::sexp::parse(d)?)?,
                    (None, None) => return None,
                };
                obj.insert(
                    id(name),
                    FieldValue {
                        mut_: Mut::Const,
                        val,
                    },
                );
            }
            if j != xs.len() {
                return None;
            }
            Value::Object(obj).into()
        }
        Ty::Enum => decode_enum(s)?,
        Ty::Any => crate::sexp::to_value(s),
        Ty::Lazy(f) => decode(f(), s)?,
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
        (Ty::Float, Value::Float(f)) => Sexp::Atom(ocaml_float(f.0)),
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
        (Ty::Fields(fields), Value::Object(obj)) if obj.len() == fields.len() => {
            let mut out = Vec::new();
            for (name, t, default) in fields {
                let e = encode(t, &obj.get(&id(name))?.val)?;
                if let Some(d) = default {
                    if crate::sexp::parse(d).as_ref() == Some(&e) {
                        continue;
                    }
                }
                out.push(Sexp::List(vec![Sexp::Atom(name.to_string()), e]));
            }
            Sexp::List(out)
        }
        (Ty::Enum, v) => encode_enum(v)?,
        (Ty::Any, v) => crate::sexp::from_value(v)?,
        (Ty::Lazy(f), v) => encode(f(), v)?,
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

// ---- A document's items ---------------------------------------------------

/// `Base.segment` (src/haz3lcore/tiles/Base.re): a list of pieces, the
/// program text Hazel saves one top-level item at a time (ItemPersist).
pub fn segment() -> &'static Ty {
    static T: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    T.get_or_init(|| list(piece()))
}

fn piece() -> Ty {
    let seg = || Ty::Lazy(segment);
    // Base.tile: sort, shards and children are left out at their defaults,
    // "the complete arity-1 tile, the common case".
    let tile = Ty::Fields(vec![
        ("id", Ty::Text, None),
        (
            "form",
            // Form.t; a family is one of FormId's many constructors
            Ty::Variant(vec![
                ("Compound", Some(Ty::Enum)),
                ("Tok", Some(Ty::Text)),
                ("TokInfix", Some(Ty::Text)),
            ]),
            None,
        ),
        // Language.Sort.t: Exp, Pat, ..., Drv(DrvSort.t)
        ("sort", Ty::Enum, Some("Exp")),
        ("shards", list(Ty::Int), Some("(0)")),
        ("children", list(seg()), Some("()")),
    ]);
    // Grout.t
    let grout = r(vec![
        ("id", Ty::Text),
        ("shape", nullary(&["Convex", "Concave"])),
    ]);
    // Language.Secondary.t
    let secondary = r(vec![
        ("id", Ty::Text),
        (
            "content",
            Ty::Variant(vec![
                ("Whitespace", Some(Ty::Text)),
                ("Comment", Some(Ty::Text)),
            ]),
        ),
    ]);
    // ProjectorCore.t(segment): placement and show_syntax have defaults on
    // reading but are always written
    let projector = r(vec![
        ("id", Ty::Text),
        ("kind", Ty::Enum),
        ("syntax", seg()),
        ("model", Ty::Text),
        ("placement", Ty::Enum),
        ("show_syntax", Ty::Bool),
    ]);
    // Base.splice
    let splice = r(vec![("id", Ty::Text), ("content", seg())]);
    Ty::Variant(vec![
        ("Tile", Some(tile)),
        ("Grout", Some(grout)),
        ("Secondary", Some(secondary)),
        ("Projector", Some(projector)),
        ("Splice", Some(splice)),
    ])
}

/// `ItemPersist.roster`: which items a document has, in order.
pub fn roster() -> Ty {
    list(r(vec![("r_id", Ty::Text), ("r_pieces", Ty::Int)]))
}

// ---- A document's editor state, and the deck's index ------------------------

/// `ScratchModel.Scratchpad.kind_persistent` (src/web/view/ScratchModel.re),
/// saved under `doc:<slide name>`: a slide's editor and its agent chat. The
/// chat (`Agent.Persistent.t`), a derivation exercise, and the stepper's
/// step tree are kept generic (`Any`); the editor around them is typed.
pub fn doc_state() -> Ty {
    // Editor.Model.persistent, which CodeEditable persists; the zipper is
    // saved as text (PersistentZipper.t), the program itself as items
    let editor = r(vec![
        ("root", Ty::Enum),
        (
            "zipper",
            r(vec![("zipper", Ty::Text), ("backup_text", Ty::Text)]),
        ),
    ]);
    // StepperView.Model.persistent; its root is StepperBase.persistent_step
    let stepper = r(vec![("root", Ty::Any)]);
    // EvalResult.Model.persistent, with Theorems.Model.persistent: an
    // Id.Map of theorems, each a stepper view
    let result = r(vec![
        ("stepper", opt(stepper)),
        (
            "theorems",
            r(vec![(
                "thm_map",
                list(Ty::Tuple(vec![
                    Ty::Text,
                    r(vec![("stepper_view", r(vec![("root", Ty::Any)]))]),
                ])),
            )]),
        ),
    ]);
    // CellEditor.Model.persistent
    let cell = r(vec![("editor", editor), ("result", result)]);
    Ty::Variant(vec![
        (
            "CodePersist",
            Some(r(vec![("editor", opt(cell)), ("agent", Ty::Lazy(agent))])),
        ),
        ("DrvPersist", Some(Ty::Any)),
    ])
}

/// `ScratchPersist.slide_meta`, saved under `doc:_meta`: which slide is
/// current and the deck's slide names, in order.
pub fn slide_meta() -> Ty {
    r(vec![
        ("current", Ty::Int),
        ("names", list(Ty::Text)),
        ("known_defaults", list(Ty::Text)),
    ])
}

// ---- A slide's probes, pins, view and collapsed rows ----------------------

/// `Refractors.RefractorList.t` (src/haz3lcore/zipper/Refractors.re), saved
/// under `doc:<slide>:probes`: the slide's manual probes, each by the id of
/// the term it is anchored to. A probe's model is a string to Hazel too (a
/// projector's own serialization), so it stays `Text`.
pub fn probes() -> Ty {
    list(Ty::Tuple(vec![
        Ty::Text,
        r(vec![("kind", Ty::Enum), ("model", Ty::Text)]),
    ]))
}

/// `OutlineTree.path`: an outline row, as the labels of the rows down to it,
/// each with its index among same-labeled siblings.
fn outline_path() -> Ty {
    list(r(vec![("s_label", Ty::Text), ("s_occ", Ty::Int)]))
}

/// `ScratchPersist.pins_file`, saved under `doc:<slide>:pins`: the outline
/// rows pinned open, and whether each runs.
pub fn pins() -> Ty {
    list(r(vec![("pin_path", outline_path()), ("pin_run", Ty::Bool)]))
}

/// `ScratchPersist.view_file`, saved under `doc:<slide>:view`: the row the
/// outline is zoomed to, and whether the slide is parked.
pub fn view() -> Ty {
    r(vec![
        ("vf_zoom", opt(outline_path())),
        ("vf_parked", Ty::Bool),
    ])
}

/// `ScratchPersist.collapse_file`, saved under `doc:<slide>:collapse`: the
/// outline rows collapsed.
pub fn collapse() -> Ty {
    list(outline_path())
}

// ---- The editor mode and the ExplainThis model -----------------------------

/// `Editors.Model.mode` (src/web/app/editors/Editors.re), saved under `MODE`:
/// which of Hazel's modes is open. Hazel reads a legacy `Derivations` as
/// `Scratch`; that value is not one of these, so it is stored generically.
pub fn mode() -> Ty {
    nullary(&["Scratch", "Documentation", "Exercises", "Config", "Tutorial"])
}

/// `ExplainThisModel.feedback_option`.
fn feedback() -> Ty {
    nullary(&["ThumbsUp", "ThumbsDown"])
}

/// `ExplainThisModel.t` (src/web/app/explainthis/ExplainThisModel.re), saved
/// under `ExplainThisModel`: the reader's thumbs on explanations and
/// examples, and which form of each group is selected. Form, group and
/// example ids (`ExplainThisForm.form_id`, `example_id`) are open enums: some
/// carry a pattern form or an operator, `(FunctionExp Var)`, and the families
/// grow with the language.
pub fn explain_this() -> Ty {
    r(vec![
        ("specificity_open", Ty::Bool),
        (
            "forms",
            list(r(vec![
                ("group", Ty::Enum),
                ("form", Ty::Enum),
                ("explanation_feedback", opt(feedback())),
                (
                    "examples",
                    list(r(vec![("sub_id", Ty::Enum), ("feedback", feedback())])),
                ),
            ])),
        ),
        (
            "groups",
            list(r(vec![("group", Ty::Enum), ("selected", Ty::Enum)])),
        ),
    ])
}

// ---- The agent chat --------------------------------------------------------

/// `OpenRouter.Message.Model.t`: a message as sent to the API.
fn api_message() -> Ty {
    r(vec![
        (
            "role",
            Ty::Variant(vec![
                ("System", None),
                ("Developer", None),
                ("User", None),
                ("Assistant", None),
                ("Tool", Some(Ty::Any)),
            ]),
        ),
        ("content", Ty::Text),
        ("tool_calls", list(Ty::Any)),
        ("cache_anchor", Ty::Bool),
    ])
}

/// `Message.Model.role`, with its `system_kind`.
fn message_role() -> Ty {
    let system_kind = Ty::Variant(vec![
        ("ApiFailure", None),
        ("DeveloperNotes", None),
        ("Prompt", None),
        ("Context", None),
        ("RetryNote", None),
        ("ResponseCancelled", None),
        ("SlashCommandOutput", Some(Ty::Any)),
        ("CompactionSummary", Some(Ty::Text)),
    ]);
    Ty::Variant(vec![
        ("Agent", Some(opt(Ty::Any))),
        ("ToolResult", Some(Ty::Any)),
        ("User", None),
        ("System", Some(system_kind)),
    ])
}

/// `Message.Model.t`; the reasoning fields have defaults but are written.
fn message() -> Ty {
    r(vec![
        ("id", Ty::Text),
        ("content", Ty::Text),
        ("timestamp", Ty::Float),
        ("role", message_role()),
        ("api_message", opt(api_message())),
        ("children", list(Ty::Text)),
        ("current_child", opt(Ty::Text)),
        ("reasoning", opt(Ty::Text)),
        ("reasoning_duration_ms", opt(Ty::Int)),
    ])
}

/// `AgentModel.Model.t` (src/web/view/agentCore/AgentModel.re), which is
/// `Agent.Persistent.t`: saved under `doc:<slide>:agent` and inside each
/// slide's state. Its chats, messages, workbench and prompting are typed;
/// the deep unions -- a tool result, an LLM usage report, a slash command's
/// output, a tool call, a workbench task, a tool's JSON -- are kept generic.
pub fn agent() -> &'static Ty {
    static T: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    T.get_or_init(|| {
        // AgentWorkbench.Model.t; a task is deep, so the task_dict is generic
        let workbench = r(vec![
            ("active_task", opt(Ty::Text)),
            ("task_dict", Ty::Any),
            (
                "t_ui",
                r(vec![
                    ("active_view", nullary(&["Chat", "Todos"])),
                    ("display_task", opt(Ty::Text)),
                    ("show_archive", Ty::Bool),
                ]),
            ),
        ]);
        // Chat.Model.t
        let chat = r(vec![
            ("id", Ty::Text),
            ("title", Ty::Text),
            ("message_map", list(Ty::Tuple(vec![Ty::Text, message()]))),
            ("root", Ty::Text),
            ("agent_view", r(vec![("expanded_paths", list(Ty::Text))])),
            ("agent_workbench", workbench),
            ("context", opt(message())),
            ("created_at", Ty::Float),
            (
                "current_view",
                nullary(&[
                    "Messages",
                    "Workbench",
                    "AgentEditorView",
                    "StaticErrors",
                    "Prompt",
                    "DeveloperNotes",
                    "Tools",
                ]),
            ),
            ("pending_send_queue", list(Ty::Text)),
        ]);
        // ChatSystem.Model.t
        let chat_system = r(vec![
            ("chat_map", list(Ty::Tuple(vec![Ty::Text, chat]))),
            ("current", Ty::Text),
            (
                "ui",
                r(vec![
                    ("active_screen", nullary(&["Chat", "History"])),
                    ("current_text_box_content", Ty::Text),
                    (
                        "slash_menu",
                        opt(r(vec![("filter", Ty::Text), ("selected_index", Ty::Int)])),
                    ),
                ]),
            ),
        ]);
        r(vec![
            ("chat_system", chat_system),
            (
                "prompting",
                r(vec![
                    ("system_prompt", Ty::Text),
                    ("dev_notes", Ty::Text),
                    ("tools", list(Ty::Any)),
                    ("disabled_tool_names", list(Ty::Text)),
                ]),
            ),
            ("active_timeline_node", opt(Ty::Int)),
            ("awaiting_response", opt(Ty::Text)),
            ("restore_editor_state", opt(Ty::Lazy(segment))),
            ("last_empty_retry_attempt", opt(Ty::Int)),
            ("last_active_task_nudge_attempt", opt(Ty::Int)),
            ("tools_view_expanded", list(Ty::Text)),
            ("compaction_in_progress", opt(Ty::Text)),
            ("compaction_method_override", opt(Ty::Text)),
            ("main_llm_seq", Ty::Int),
            ("compaction_llm_seq", Ty::Int),
            ("pending_ignore_main_reply_seq", opt(Ty::Int)),
            ("pending_ignore_compaction_reply_seq", opt(Ty::Int)),
            ("pending_dispatch_send", opt(Ty::Text)),
            ("pending_assistant_content", Ty::Text),
            ("pending_assistant_reasoning", Ty::Text),
        ])
    })
}

/// The schema for a Hazel key, if the canister knows its type.
/// What follows a slide key's namespace. Documentation saves under `doc:`
/// and Scratch under `scratch:`, through the same `ScratchMode.Persist`, so
/// one set of schemas serves both.
fn slide_key(key: &str) -> Option<&str> {
    key.strip_prefix("doc:")
        .or_else(|| key.strip_prefix("scratch:"))
}

pub fn for_key(key: &str) -> Option<&'static Ty> {
    static SETTINGS: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static ROSTER: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static DOC: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static META: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static PROBES: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static PINS: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static VIEW: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static COLLAPSE: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static MODE: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    static EXPLAIN_THIS: std::sync::OnceLock<Ty> = std::sync::OnceLock::new();
    if key == "SETTINGS" {
        Some(SETTINGS.get_or_init(settings))
    } else if key == "MODE" {
        Some(MODE.get_or_init(mode))
    } else if key == "ExplainThisModel" {
        Some(EXPLAIN_THIS.get_or_init(explain_this))
    } else if key.contains(":items:item:") {
        Some(segment())
    } else if key.ends_with(":items:roster") {
        Some(ROSTER.get_or_init(roster))
    } else if let Some(rest) = slide_key(key) {
        if rest.ends_with(":agent") {
            Some(agent())
        } else if rest.ends_with(":probes") {
            Some(PROBES.get_or_init(probes))
        } else if rest.ends_with(":pins") {
            Some(PINS.get_or_init(pins))
        } else if rest.ends_with(":view") {
            Some(VIEW.get_or_init(view))
        } else if rest.ends_with(":collapse") {
            Some(COLLAPSE.get_or_init(collapse))
        } else if rest == "_meta" {
            Some(META.get_or_init(slide_meta))
        } else {
            // Any other key may be a slide's state, `doc:<slide name>`; a
            // slide name can hold anything, so the schema itself decides: a
            // value that does not decode is stored generically, as ever.
            Some(DOC.get_or_init(doc_state))
        }
    } else {
        None
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
    fn an_item_decodes_into_pieces_and_prints_back() {
        // The smallest item of the Kids' Choice slide, as Hazel saved it.
        let item = include_str!("../tests/item.sexp").trim_end();
        let s = parse(item).unwrap();
        let v = decode_exact(segment(), &s).expect("the item decodes");
        assert_eq!(print(&encode(segment(), &v).unwrap()), item);
    }

    #[test]
    fn form_families_with_arguments_are_open_enums() {
        for item in [
            "((Tile((id a)(form(Compound(SInt Plus)))(sort(Drv Exp))(shards(0 1))(children(())))))",
            "((Grout((id g)(shape Concave)))(Splice((id s)(content()))))",
            "((Projector((id p)(kind Livelit)(syntax((Tile((id t)(form(Tok x))))))(model\"\")(placement Inline)(show_syntax false))))",
        ] {
            let s = parse(item).unwrap();
            assert!(decode_exact(segment(), &s).is_some(), "{}", item);
        }
    }

    #[test]
    fn a_slides_editor_state_decodes_and_prints_back() {
        // The Parameters slide's state as Hazel saved it.
        let saved = include_str!("../tests/doc-parameters.sexp").trim_end();
        let s = parse(saved).unwrap();
        let v = decode_exact(&doc_state(), &s).expect("the slide's state decodes");
        assert_eq!(print(&encode(&doc_state(), &v).unwrap()), saved);
    }

    #[test]
    fn a_doc_value_that_is_not_a_slide_is_not_decoded() {
        // A caret, saved as `0 0`, is not even an S-expression; pins and
        // probes are other types. None of them decodes as a slide.
        assert!(decode_exact(&doc_state(), &parse("((a b))").unwrap()).is_none());
        assert!(decode_exact(&doc_state(), &parse("()").unwrap()).is_none());
    }

    fn round_trips(ty: &Ty, saved: &str) {
        let s = parse(saved).unwrap();
        let v = decode_exact(ty, &s).unwrap_or_else(|| panic!("does not decode: {}", saved));
        assert_eq!(print(&encode(ty, &v).unwrap()), saved);
    }

    #[test]
    fn probes_pins_views_and_collapses_print_back() {
        // Kids' Choice's probes as Hazel saved them: one probe, its model
        // a nested S-expression in a string.
        round_trips(&probes(), "((22e20000-0000-4000-8e52-a8afd7996918((kind Probe)(model\"((active_renderer())(drawer_mode false)(dropdown_redraw 0)(auto_rich false)(rich_off false))\"))))");
        round_trips(&probes(), "()");
        round_trips(&pins(), "()");
        round_trips(&pins(), "(((pin_path(((s_label face)(s_occ 0))((s_label ^kid_face)(s_occ 1))((s_label\"two words\")(s_occ 0))))(pin_run true)))");
        round_trips(&view(), "((vf_zoom())(vf_parked false))");
        round_trips(
            &view(),
            "((vf_zoom((((s_label head)(s_occ 0)))))(vf_parked true))",
        );
        round_trips(&collapse(), "((((s_label tests)(s_occ 2))))");
    }

    #[test]
    fn floats_print_as_sexplib_prints_them() {
        assert_eq!(ocaml_float(1790969258449.0), "1790969258449");
        assert_eq!(ocaml_float(0.1), "0.1");
        assert_eq!(ocaml_float(1.5), "1.5");
        assert_eq!(ocaml_float(1e20), "1E+20");
        assert_eq!(ocaml_float(1.0 / 3.0), "0.33333333333333331");
        assert_eq!(ocaml_float(2.0), "2");
    }

    #[test]
    fn the_mode_and_the_explainthis_model_print_back() {
        round_trips(&mode(), "Documentation");
        assert!(decode_exact(&mode(), &parse("Derivations").unwrap()).is_none());
        round_trips(&explain_this(), "((specificity_open false)(forms())(groups()))");
        // Ids with arguments, nested: a pattern form, an operator, an example.
        round_trips(
            &explain_this(),
            "((specificity_open true)(forms(((group(FunctionExp Var))(form(FunctionExp Base))\
             (explanation_feedback(ThumbsUp))(examples(((sub_id(List Cons1))(feedback ThumbsDown)))))\
             ((group(BinOpExp(Int Plus)))(form(BinOpExp(Int Plus)))(explanation_feedback())(examples()))))\
             (groups(((group(FunctionExp Var))(selected(FunctionExp Base))))))",
        );
    }

    #[test]
    fn scratch_slides_use_the_same_schemas_as_documentation() {
        for suffix in ["", ":agent", ":probes", ":pins", ":view", ":collapse"] {
            let doc = for_key(&format!("doc:Scratchpad 1{}", suffix)).map(|t| t as *const Ty);
            let scratch = for_key(&format!("scratch:Scratchpad 1{}", suffix)).map(|t| t as *const Ty);
            assert!(scratch.is_some() && scratch == doc, "{}", suffix);
        }
        round_trips(
            for_key("scratch:_meta").unwrap(),
            "((current 0)(names(\"Scratchpad 1\"))(known_defaults()))",
        );
        round_trips(for_key("scratch:Scratchpad 1:view").unwrap(), "((vf_zoom())(vf_parked false))");
    }

    #[test]
    fn the_agent_chat_decodes_and_prints_back() {
        // Kids' Choice's agent chat, saved alone and inside the slide's state.
        let saved = include_str!("../tests/agent.sexp").trim_end();
        round_trips(agent(), saved);
        let doc = include_str!("../tests/doc-kids.sexp").trim_end();
        round_trips(&doc_state(), doc);
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
