//! Hazel's saved values as Fumola values.
//!
//! Hazel saves every value as `Sexplib.Sexp.to_string` of an OCaml value:
//! one S-expression, atoms and lists. Stage 2 stores that structure in the
//! DCG instead of the text, as
//!
//! ```text
//! atom   #atom("text")
//! list   #list([v1, v2, ...])
//! ```
//!
//! so a Fumola program can walk any of Hazel's state. A read prints the value
//! back as an S-expression for Hazel's deserializer. Printing need not
//! reproduce the saved text byte for byte -- Sexplib reads any quoting -- only
//! the same S-expression, so the round trip that matters is
//! `parse(print(parse(s))) == parse(s)`.
//!
//! Text that is not one S-expression is not Hazel's sexp format; it is kept
//! as a plain Fumola `Text`. Comments (`;`, `#|...|#`, `#;`) are not parsed,
//! since `Sexp.to_string` never writes them: a value with one is kept as text.
//!
//! This is the generic layer. Mapping a sexp into Hazel's own datatypes --
//! records with their field names, variants with their constructors -- comes
//! later, per datatype.

use fumola_semantics::value::{Value, Value_};
use fumola_syntax::ast::{Id, Mut};
use im_rc::Vector;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sexp {
    Atom(String),
    List(Vec<Sexp>),
}

// ---- Parsing --------------------------------------------------------------

struct Parser<'a> {
    s: &'a [u8],
    i: usize,
}

fn is_space(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\n' | b'\r' | b'\x0c')
}

impl<'a> Parser<'a> {
    fn skip_space(&mut self) {
        while self.i < self.s.len() && is_space(self.s[self.i]) {
            self.i += 1;
        }
    }

    fn sexp(&mut self) -> Option<Sexp> {
        self.skip_space();
        match *self.s.get(self.i)? {
            b'(' => {
                self.i += 1;
                let mut items = Vec::new();
                loop {
                    self.skip_space();
                    match *self.s.get(self.i)? {
                        b')' => {
                            self.i += 1;
                            return Some(Sexp::List(items));
                        }
                        _ => items.push(self.sexp()?),
                    }
                }
            }
            b')' => None,
            b'"' => self.quoted(),
            _ => self.bare(),
        }
    }

    /// An unquoted atom runs to whitespace, a paren or a quote. A `;` or a
    /// `#|` would start a comment, which this parser does not read.
    fn bare(&mut self) -> Option<Sexp> {
        let start = self.i;
        while self.i < self.s.len() {
            let c = self.s[self.i];
            if is_space(c) || c == b'(' || c == b')' || c == b'"' {
                break;
            }
            if c == b';' || (c == b'#' && matches!(self.s.get(self.i + 1), Some(b'|') | Some(b';')))
            {
                return None;
            }
            self.i += 1;
        }
        String::from_utf8(self.s[start..self.i].to_vec())
            .ok()
            .map(Sexp::Atom)
    }

    /// A quoted atom, with OCaml's escapes, as Sexplib reads them.
    fn quoted(&mut self) -> Option<Sexp> {
        self.i += 1;
        let mut out: Vec<u8> = Vec::new();
        loop {
            let c = *self.s.get(self.i)?;
            self.i += 1;
            match c {
                b'"' => return String::from_utf8(out).ok().map(Sexp::Atom),
                b'\\' => {
                    let e = *self.s.get(self.i)?;
                    self.i += 1;
                    match e {
                        b'n' => out.push(b'\n'),
                        b't' => out.push(b'\t'),
                        b'r' => out.push(b'\r'),
                        b'b' => out.push(b'\x08'),
                        b' ' => out.push(b' '),
                        b'\\' | b'"' | b'\'' => out.push(e),
                        b'\n' => {
                            // A backslash-newline continues the string; the
                            // next line's leading blanks are not part of it.
                            while matches!(self.s.get(self.i), Some(b' ') | Some(b'\t')) {
                                self.i += 1;
                            }
                        }
                        b'0'..=b'9' => {
                            let d = self.s.get(self.i - 1..self.i + 2)?;
                            if !d.iter().all(|c| c.is_ascii_digit()) {
                                return None;
                            }
                            let n = (d[0] - b'0') as u32 * 100
                                + (d[1] - b'0') as u32 * 10
                                + (d[2] - b'0') as u32;
                            out.push(u8::try_from(n).ok()?);
                            self.i += 2;
                        }
                        b'x' => {
                            let h = std::str::from_utf8(self.s.get(self.i..self.i + 2)?).ok()?;
                            out.push(u8::from_str_radix(h, 16).ok()?);
                            self.i += 2;
                        }
                        // Sexplib keeps an unknown escape as written.
                        other => {
                            out.push(b'\\');
                            out.push(other);
                        }
                    }
                }
                _ => out.push(c),
            }
        }
    }
}

/// One S-expression, and nothing after it but whitespace.
pub fn parse(text: &str) -> Option<Sexp> {
    let mut p = Parser {
        s: text.as_bytes(),
        i: 0,
    };
    let sexp = p.sexp()?;
    p.skip_space();
    if p.i == p.s.len() {
        Some(sexp)
    } else {
        None
    }
}

// ---- Printing -------------------------------------------------------------

/// Sexplib's own test (`must_escape`): an atom is quoted when it is empty or
/// holds a delimiter, a quote, a backslash, a control or non-ASCII byte, or a
/// `#|` / `|#` that would read as a comment.
fn must_quote(a: &str) -> bool {
    let b = a.as_bytes();
    b.is_empty()
        || b.iter().enumerate().any(|(i, &c)| match c {
            b'"' | b'(' | b')' | b';' | b'\\' => true,
            b'|' => b.get(i + 1) == Some(&b'#'),
            b'#' => b.get(i + 1) == Some(&b'|'),
            0..=32 | 127..=255 => true,
            _ => false,
        })
}

/// OCaml's `String.escaped`, which Sexplib quotes with.
fn escaped(a: &str) -> String {
    let mut out = String::new();
    for &c in a.as_bytes() {
        match c {
            b'"' => out.push_str("\\\""),
            b'\\' => out.push_str("\\\\"),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            b'\x08' => out.push_str("\\b"),
            32..=126 => out.push(c as char),
            _ => out.push_str(&format!("\\{:03}", c)),
        }
    }
    out
}

/// As `Sexp.to_string` prints: no spaces but between two unquoted atoms.
pub fn print(sexp: &Sexp) -> String {
    let mut out = String::new();
    print_into(sexp, &mut out);
    out
}

fn print_into(sexp: &Sexp, out: &mut String) {
    match sexp {
        Sexp::Atom(a) => {
            if must_quote(a) {
                out.push('"');
                out.push_str(&escaped(a));
                out.push('"');
            } else {
                out.push_str(a);
            }
        }
        Sexp::List(items) => {
            out.push('(');
            // Sexplib's machine printing: a space only between two atoms
            // that are both unquoted. A quote, like a paren, delimits.
            let mut prev_bare = false;
            for item in items {
                let bare = matches!(item, Sexp::Atom(a) if !must_quote(a));
                if prev_bare && bare {
                    out.push(' ');
                }
                print_into(item, out);
                prev_bare = bare;
            }
            out.push(')');
        }
    }
}

// ---- To and from Fumola values ----------------------------------------------

pub fn to_value(sexp: &Sexp) -> Value_ {
    match sexp {
        Sexp::Atom(a) => Value::Variant(
            Id::new("atom".to_string()),
            Some(Value::Text(a.as_str().into()).into()),
        )
        .into(),
        Sexp::List(items) => {
            let vals: Vector<Value_> = items.iter().map(to_value).collect();
            Value::Variant(
                Id::new("list".to_string()),
                Some(Value::Array(Mut::Const, vals).into()),
            )
            .into()
        }
    }
}

/// The sexp a value encodes, if it is one: what a read prints back.
pub fn from_value(v: &Value) -> Option<Sexp> {
    match v {
        Value::Variant(tag, Some(arg)) => match (tag.string.as_str(), &**arg) {
            ("atom", Value::Text(t)) => Some(Sexp::Atom(t.to_string())),
            ("list", Value::Array(_, items)) => items
                .iter()
                .map(|x| from_value(x))
                .collect::<Option<Vec<_>>>()
                .map(Sexp::List),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trips(s: &str) {
        let a = parse(s).unwrap_or_else(|| panic!("does not parse: {}", s));
        let printed = print(&a);
        assert_eq!(
            parse(&printed),
            Some(a.clone()),
            "{} printed as {}",
            s,
            printed
        );
        assert_eq!(from_value(&to_value(&a)), Some(a), "{} through a value", s);
    }

    #[test]
    fn hazel_shapes_round_trip() {
        round_trips("Documentation");
        round_trips("((mode Scratch)(slide 3))");
        round_trips("(Editors(Scratch(Workspace(RestoreCaret((row 0)(col 0))))))");
        round_trips("(a \"\" \"two words\" \"q\\\"uote\" \"back\\\\slash\" \"line\\nbreak\")");
        round_trips("(\"caf\\195\\169\" \"tab\\tend\")");
        round_trips("(x \"|#\" \"#|y\")");
        round_trips("()");
    }

    #[test]
    fn printing_matches_sexplib() {
        assert_eq!(print(&parse("( a  b (c d) e )").unwrap()), "(a b(c d)e)");
        assert_eq!(print(&Sexp::Atom("two words".into())), "\"two words\"");
        assert_eq!(print(&Sexp::Atom("".into())), "\"\"");
        assert_eq!(print(&Sexp::Atom("é".into())), "\"\\195\\169\"");
        // Measured against Hazel's saved SETTINGS: no space before a quote.
        assert_eq!(
            print(&parse("((model_filter \"\")(only_free_models false))").unwrap()),
            "((model_filter\"\")(only_free_models false))"
        );
        assert_eq!(
            print(&parse("(\"a b\" c \"d e\")").unwrap()),
            "(\"a b\"c\"d e\")"
        );
    }

    #[test]
    fn not_a_sexp_is_none() {
        assert_eq!(parse("(a b"), None);
        assert_eq!(parse("a b"), None);
        assert_eq!(parse("(a ; comment\n b)"), None);
        assert_eq!(parse(""), None);
    }
}
