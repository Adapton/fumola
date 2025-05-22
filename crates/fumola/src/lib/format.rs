// Reference: https://github.com/dfinity/candid/blob/master/rust/candid/src/bindings/candid.rs

use crate::adapton::Space;
use crate::ast::{
    AdaptonNav, AdaptonNavDim, BinOp, BindSort, Case, CasesPos, Dec, DecField, DecFieldsPos, Dec_,
    Delim, Exp, ExpField, Exp_, Function, Id, IdPos, Literal, Loc, Mut, NodeData, ObjSort, Pat,
    PrimType, QuotedAst, RelOp, Stab, Type, TypeBind, TypeField, TypeTag, TypeTag_, UnOp, Unquote,
    Vis,
};
use crate::format_utils::*;
use crate::lexer::is_keyword;
use crate::lexer_types::{GroupType, Token, TokenTree};
use crate::shared::Shared;
use crate::value::{Closed, FieldValue, Pointer, Symbol, Value, Value_};
use crate::vm_types::def::Module;
use crate::vm_types::{def::CtxId, Env, LocalPointer, ScheduleChoice};
use pretty::RcDoc;

pub fn format_(doc: RcDoc, width: usize) -> String {
    let mut w = Vec::new();
    doc.render(width, &mut w).unwrap();
    String::from_utf8(w).unwrap()
}

pub fn format_pretty(to_doc: &dyn ToDoc, width: usize) -> String {
    format_(to_doc.doc(), width)
}

pub fn format_one_line(to_doc: &dyn ToDoc) -> String {
    format_(to_doc.doc().group(), usize::MAX)
}

pub trait ToDoc {
    fn doc(&self) -> RcDoc;
}

// impl fmt::Display for dyn ToDoc {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", format_pretty(self, usize::MAX))
//     }
// }

fn object<'a>(m: &'a im_rc::HashMap<Id, FieldValue>) -> RcDoc<'a> {
    let mut out: Vec<_> = m.iter().collect();
    out.sort_by(|a, b| a.0.cmp(b.0));
    strict_concat(
        out.iter().map(|(fi, fv)| match fv.mut_ {
            Mut::Const => fi.doc().append(" = ").append(fv.val.doc()),
            Mut::Var => str("var ")
                .append(fi.doc())
                .append(" = ")
                .append(fv.val.doc()),
        }),
        ";",
    )
}

fn hashmap<'a>(m: &'a im_rc::HashMap<Value_, Value_>) -> RcDoc<'a> {
    let data = m.iter().collect::<Vec<_>>();
    // to do -- sort general values (but only handle common cases for now)
    // data.sort_by(|a, b| a.0.cmp(&b.0));
    enclose(
        "[",
        strict_concat(
            data.iter()
                .map(|(fk, fv)| enclose("(", fk.doc().append(", ").append(fv.doc()), ")")),
            ";",
        ),
        "]",
    )
}

// optional delimiter on the right
fn delim<'a, T: ToDoc + Clone>(d: &'a Delim<T>, sep: &'a str) -> RcDoc<'a> {
    let doc = strict_concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        doc.append(sep)
    } else {
        doc
    }
}

// optional delimiter on the left
#[allow(dead_code)]
fn delim_left<'a, T: ToDoc + Clone>(d: &'a Delim<T>, sep: &'a str) -> RcDoc<'a> {
    let doc = strict_concat(d.vec.iter().map(|x| x.doc()), sep);
    if d.has_trailing {
        str(sep).append(RcDoc::space()).append(doc)
    } else {
        doc
    }
}

fn optional_paren<'a, T: ToDoc + Clone>(o: &'a Option<T>) -> RcDoc<'a> {
    match o {
        None => RcDoc::nil(),
        Some(value) => enclose("(", value.doc(), ")"),
    }
}

fn vector<'a, T: ToDoc + Clone>(v: &'a im_rc::Vector<T>, sep: &'a str) -> RcDoc<'a> {
    strict_concat(v.iter().map(|x| x.doc()), sep)
}

fn variant_vector<'a>(v: &'a im_rc::Vector<TypeTag_>) -> RcDoc<'a> {
    strict_concat(v.iter().map(|x| str("#").append(x.doc())), ";")
}

fn vector_tuple<'a, T: ToDoc + Clone>(v: &'a im_rc::Vector<T>) -> RcDoc<'a> {
    enclose("(", vector(v, ","), ")")
}

fn block<'a, T: ToDoc + Clone>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose_space("{", delim(d, ";"), "}")
}

fn tuple<'a, T: ToDoc + Clone>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose("(", delim(d, ","), ")")
}

fn field_block<'a, T: ToDoc + Clone>(d: &'a Delim<T>) -> RcDoc<'a> {
    enclose("{", delim(d, ","), "}")
}

fn array<'a, T: ToDoc + Clone>(m: &'a Mut, d: &'a Delim<T>) -> RcDoc<'a> {
    enclose("[", m.doc().append(delim(d, ",")), "]")
}

fn bind<'a, T: ToDoc + Clone>(d: &'a Delim<T>) -> RcDoc<'a> {
    if d.vec.is_empty() {
        RcDoc::nil()
    } else {
        enclose("<", delim(d, ","), ">")
    }
}

fn bin_op<'a, E: ToDoc + Clone>(e1: &'a E, b: RcDoc<'a>, e2: &'a E) -> RcDoc<'a> {
    e1.doc()
        .append(RcDoc::space())
        .append(b)
        .append(RcDoc::space())
        .append(e2.doc())
}

impl ToDoc for String {
    fn doc(&self) -> RcDoc {
        str(self)
    }
}

impl<T: ToDoc + Clone> ToDoc for Loc<T> {
    fn doc(&self) -> RcDoc {
        let Loc(t, _) = self;
        t.doc()
    }
}

impl<T: ToDoc + Clone> ToDoc for Box<T> {
    fn doc(&self) -> RcDoc {
        self.as_ref().doc()
    }
}

impl<T: ToDoc + Clone> ToDoc for Shared<T> {
    fn doc(&self) -> RcDoc {
        self.as_ref().doc()
    }
}

impl<T: ToDoc + Clone> ToDoc for NodeData<T> {
    fn doc(&self) -> RcDoc {
        self.0.doc()
    }
}

impl<T: ToDoc + Clone> ToDoc for Option<T> {
    fn doc(&self) -> RcDoc {
        match self {
            None => RcDoc::nil(),
            Some(value) => value.doc(),
        }
    }
}

impl ToDoc for Symbol {
    fn doc(&self) -> RcDoc {
        match self {
            Symbol::UnOp(u, s) => u.doc().append(s.doc()),
            Symbol::Call(s1, s2) => s1.doc().append(enclose("(", s2.doc(), ")")),
            Symbol::Nat(n) => RcDoc::text(n.to_string()),
            Symbol::Int(i) => RcDoc::text(i.to_string()),
            Symbol::QuotedAst(q) => q.doc(),
            Symbol::BinOp(l, b, r) => l.doc().append(b.doc().append(r.doc())),
        }
    }
}

impl ToDoc for Closed<Exp_> {
    fn doc(&self) -> RcDoc {
        RcDoc::text("<TODO-Closed<Exp>>")
    }
}

impl ToDoc for Space {
    fn doc(&self) -> RcDoc {
        kwd("here").append(enclose(
            "(",
            match self {
                Space::Here => RcDoc::nil(),
                Space::Exp_(c) => c.doc(),
                Space::Symbol(s) => s.doc(),
            },
            ")",
        ))
    }
}

impl ToDoc for CtxId {
    fn doc(&self) -> RcDoc {
        RcDoc::text(format!("{}", self.0))
    }
}

impl ToDoc for Env {
    fn doc(&self) -> RcDoc {
        let data = self.iter().collect::<Vec<_>>();
        // to do -- sort general values (but only handle common cases for now)
        // data.sort_by(|a, b| a.0.cmp(&b.0));
        enclose(
            "[",
            strict_concat(
                data.iter()
                    .map(|(fk, fv)| enclose("(", fk.doc().append(", ").append(fv.doc()), ")")),
                ";",
            ),
            "]",
        )
    }
}

impl ToDoc for ScheduleChoice {
    fn doc(&self) -> RcDoc {
        match self {
            ScheduleChoice::Agent => RcDoc::text("#agent"),
            ScheduleChoice::Actor(actor_id) => RcDoc::text(format!("#actor({:?})", actor_id)),
        }
    }
}

impl ToDoc for LocalPointer {
    fn doc(&self) -> RcDoc {
        match self {
            LocalPointer::Numeric(numeric_pointer) => {
                RcDoc::text(format!("{:?}", numeric_pointer.0))
            }
            LocalPointer::Named(named_pointer) => RcDoc::text(format!("{:?}", named_pointer.0)),
        }
    }
}

impl ToDoc for Pointer {
    fn doc(&self) -> RcDoc {
        kwd("pointer").append(enclose(
            "(",
            self.local
                .doc()
                .append(RcDoc::text(","))
                .append(self.owner.doc()),
            ")",
        ))
    }
}

impl ToDoc for Module {
    fn doc(&self) -> RcDoc {
        kwd("module").append(enclose(
            "(",
            self.context
                .doc()
                .append(RcDoc::text(","))
                .append(self.fields.doc()),
            ")",
        ))
    }
}

impl ToDoc for Value {
    fn doc(&self) -> RcDoc {
        match self {
            Value::Null => str("null"),
            Value::Bool(b) => RcDoc::text(b.to_string()),
            Value::Unit => str("()"),
            Value::Nat(n) => RcDoc::text(n.to_string()),
            Value::Int(i) => RcDoc::text(i.to_string()),
            Value::Float(_) => todo!(),
            Value::Char(_) => todo!(),
            Value::Text(t) => RcDoc::text(format!("{:?}", t.to_string())),
            Value::Blob(_) => todo!(),
            Value::Array(Mut::Const, vs) => enclose("[", vector(vs, ","), "]"),
            Value::Array(Mut::Var, vs) => enclose("[var ", vector(vs, ","), "]"),
            Value::Tuple(vs) => vector_tuple(vs),
            Value::Object(fs) => enclose("{", object(fs), "}"),
            Value::Option(v) => str("?").append(v.doc()),
            Value::Variant(n, v) => {
                // Can we skip using a paren around optional value?
                if v.as_ref().is_some_and(|v| match &**v {
                    Value::Unit => true,
                    Value::Tuple(_) => true,
                    Value::Object(_) => true,
                    _ => false,
                }) {
                    str("#").append(n.doc()).append(v.doc())
                } else {
                    str("#").append(n.doc()).append(optional_paren(v))
                }
            }
            Value::Module(m) => m.doc(),
            Value::AdaptonPointer(p) => p.doc(),
            Value::Thunk(c) => kwd("thunk")
                .append(c.ctx.doc())
                .append(c.env.doc())
                .append(c.content.doc()),
            Value::Pointer(p) => p.doc(),
            Value::Symbol(s) => s.doc(),
            Value::Opaque(_) => todo!(),
            Value::Index(_, _) => todo!(),
            Value::Function(f) => enclose("<", f.0.content.doc(), ">"),
            Value::PrimFunction(f) => kwd("prim").append(RcDoc::text(format!("\"{:?}\"", f))),
            Value::Collection(c) => match c {
                crate::value::Collection::HashMap(m) => hashmap(m),
                crate::value::Collection::FastRandIter(_) => todo!(),
            },
            Value::Dynamic(_) => todo!(),
            Value::Actor(_) => todo!(),
            Value::ActorMethod(_) => todo!(),
            Value::QuotedAst(q) => q.doc(),
        }
    }
}

impl ToDoc for QuotedAst {
    fn doc(&self) -> RcDoc {
        match self {
            QuotedAst::Id(i) => str("`").append(i.doc()),
            QuotedAst::Literal(l) => str("`").append(l.doc()),
            QuotedAst::Empty => str("`()"),
            QuotedAst::Decs(ds) => str("`do ").append(block(ds)),
            QuotedAst::TupleExps(es) => str("`").append(tuple(es)),
            QuotedAst::RecordExps((None, Some(fs))) => str("`").append(block(fs)),
            QuotedAst::Types(ts) => str("`").append("type").append(space()).append(enclose(
                "(",
                vector(&ts.vec, ","),
                ")",
            )),
            _ => todo!(),
        }
    }
}

impl ToDoc for Literal {
    fn doc(&self) -> RcDoc {
        use Literal::*;
        str(match self {
            Null => "null",
            Bool(true) => "true",
            Bool(false) => "false",
            Unit => "()",
            Nat(n) => n,
            // Int(i) => i,
            Float(f) => f,
            Text(t) => t,
            Char(c) => c,
            Blob(_) => unimplemented!(),
            // _ => text("Display-TODO={:?}", self),
        })
    }
}

impl ToDoc for UnOp {
    fn doc(&self) -> RcDoc {
        use UnOp::*;
        str(match self {
            Pos => "+",
            Neg => "-",
            Not => "^",
        })
    }
}

impl ToDoc for BinOp {
    fn doc(&self) -> RcDoc {
        use BinOp::*;
        str(match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Pow => "**",
            And => "and",
            Or => "or",
            BitAnd => "&",
            BitOr => "|",
            Xor => "^",
            ShL => "<<",
            ShR => " >>",
            RotL => "<<>",
            RotR => "<>>",
            WAdd => "+%",
            WSub => "-%",
            WMul => "*%",
            WPow => "**%",
            Cat => "#",
        })
    }
}

impl ToDoc for RelOp {
    fn doc(&self) -> RcDoc {
        use RelOp::*;
        str(match self {
            Eq => "==",
            Neq => "!=",
            Lt => "<",
            Gt => ">",
            Le => "<=",
            Ge => ">=",
        })
    }
}

impl ToDoc for Id {
    fn doc(&self) -> RcDoc {
        str(&self.string)
    }
}

impl ToDoc for IdPos {
    fn doc(&self) -> RcDoc {
        self.id.doc()
    }
}

impl ToDoc for CasesPos {
    fn doc(&self) -> RcDoc {
        match self {
            CasesPos::Cases(cs) => enclose_space("{", delim(cs, ";"), "}"),
            CasesPos::Unquote(uq) => uq.doc(),
        }
    }
}

impl ToDoc for DecFieldsPos {
    fn doc(&self) -> RcDoc {
        match self {
            DecFieldsPos::DecFields(dfs) => block(dfs),
            DecFieldsPos::Unquote(uq) => uq.doc(),
        }
    }
}
impl ToDoc for Unquote {
    fn doc(&self) -> RcDoc {
        str("~").append(self.id.doc())
    }
}

impl ToDoc for AdaptonNav {
    fn doc(&self) -> RcDoc {
        match self {
            AdaptonNav::Goto(d, e) => kwd("goto").append(d.doc()).append(e.doc()),
            AdaptonNav::Within(d, e) => kwd("within").append(d.doc()).append(e.doc()),
        }
    }
}

impl ToDoc for AdaptonNavDim {
    fn doc(&self) -> RcDoc {
        match self {
            AdaptonNavDim::Space => kwd("space"),
            AdaptonNavDim::Time => kwd("time"),
        }
    }
}

impl ToDoc for Exp {
    fn doc(&self) -> RcDoc {
        use Exp::*;
        match self {
            Hole => str("_?_"),
            Return(e) => kwd("return").append(e.doc()),
            Literal(l) => l.doc(),
            Un(u, e2) => u.doc().append(e2.doc()),
            Bin(e1, b, e2) => bin_op(e1, b.doc(), e2),
            Tuple(es) => tuple(es),
            Prim(_) => unimplemented!(),
            Var(id) => id.doc(),
            ActorUrl(_) => todo!(),
            Rel(e1, r, e2) => bin_op(e1, r.doc(), e2),
            Show(e) => kwd("debug_show").append(e.doc()),
            ToCandid(_) => todo!(),
            FromCandid(_) => todo!(),
            //            Proj(e1, n) => e1.doc().append(format!(".{}", n)),
            Opt(e) => str("?").append(e.doc()),
            DoOpt(e) => kwd("do ?").append(e.doc()),
            Bang(e) => e.doc().append("!"),
            ObjectBlock(s, d) => s.doc().append(RcDoc::space()).append(d.doc()),
            //            Object(fs) => block(fs),
            Variant(id, e) => str("#").append(id.doc()).append(match e {
                None => RcDoc::nil(),
                Some(e) => RcDoc::space().append(e.doc()),
            }),
            Dot(e, s) => e.doc().append(".").append(s.doc()),
            Assign(from, to) => from.doc().append(str(" := ")).append(to.doc()),
            BinAssign(from, BinOp::Add, to) => from.doc().append(str(" += ")).append(to.doc()),
            BinAssign(_from, _, _to) => todo!(),
            Array(m, es) => array(m, es),
            Index(e, idx) => e.doc().append("[").append(idx.doc()).append("]"),
            Function(f) => f.doc(),
            Call(e, b, a) => e
                .doc()
                .append(b.as_ref().map(bind).unwrap_or(RcDoc::nil()))
                .append(enclose("(", a.doc(), ")")),
            Block(decs) => block(decs),
            Do(e) => kwd("do").append(e.doc()),
            DoAdaptonNav(nav, e) => kwd("do").append(vector(nav, " ")).append(e.doc()),
            Not(e) => kwd("not").append(e.doc()),
            And(e1, e2) => bin_op(e1, str("and"), e2),
            Or(e1, e2) => bin_op(e1, str("or"), e2),
            If(e1, e2, e3) => kwd("if")
                .append(e1.doc())
                .append(RcDoc::space())
                .append(e2.doc())
                .append(match e3 {
                    None => RcDoc::nil(),
                    Some(e3) => RcDoc::space().append(kwd("else")).append(e3.doc()),
                }),
            Switch(e, cs) => kwd("switch")
                .append(e.doc())
                .append(RcDoc::space())
                .append(cs.doc()),
            While(c, e) => kwd("while")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Loop(e, w) => kwd("loop").append(e.doc()).append(match w {
                None => RcDoc::nil(),
                Some(w) => RcDoc::space().append(w.doc()),
            }),
            For(p, c, e) => kwd("for")
                .append(p.doc())
                .append(" of ")
                .append(c.doc())
                .append(RcDoc::space())
                .append(e.doc()),
            Label(id, t, e) => kwd("label")
                .append(id.doc())
                .append(match t {
                    None => RcDoc::nil(),
                    Some(t) => str(" : ").append(t.doc()),
                })
                .append(RcDoc::space())
                .append(e.doc()),
            Break(id, e) => kwd("break").append(id.doc()).append(match e {
                None => RcDoc::nil(),
                Some(e) => RcDoc::space().append(e.doc()),
            }),
            Debug(e) => kwd("debug").append(e.doc()),
            //            Async(_, _) => todo!(),
            Await(e) => kwd("await").append(e.doc()),
            Assert(e) => kwd("assert").append(e.doc()),
            //            Annot(e, t) => e.doc().append(" : ").append(t.doc()),
            Import(s) => kwd("import").append(s.doc()), // new permissive syntax?
            Throw(e) => kwd("throw").append(e.doc()),
            Try(_e, _cs) => {
                todo!()
                // let mut doc = kwd("try").append(e.doc());
                // // ?????
                // for c in cs {
                //     doc = doc
                //         .append(RcDoc::line())
                //         .append(kwd("catch"))
                //         .append(c.0.pat.doc())
                //         .append(RcDoc::space())
                //         .append(c.0.exp.doc())
                // }
                // doc
            }
            Ignore(e) => kwd("ignore").append(e.doc()),
            Paren(e) => enclose("(", e.doc(), ")"),
            Value_(_) => todo!(),
            Proj(_, _) => todo!(),
            Object(_) => todo!(),
            DebugShow(_) => todo!(),
            Async(_) => todo!(),
            AsyncStar(_) => todo!(),
            AwaitStar(_) => todo!(),
            Annot(_, _, _) => todo!(),
            QuotedAst(q) => q.doc(),
            Unquote(e) => kwd("~").append(e.doc()),
            Thunk(e) => kwd("thunk").append(e.doc()),
            Force(e) => kwd("force").append(e.doc()),
            GetAdaptonPointer(e) => kwd("@").append(e.doc()),
        }
        // _ => text("Display-TODO={:?}", self),
    }
}

impl ToDoc for Delim<Dec_> {
    fn doc(&self) -> RcDoc {
        delim(self, ";")
    }
}

fn exp_is_block(e: &Exp) -> bool {
    match e {
        Exp::Block(_) => true,
        _ => false,
    }
}

impl ToDoc for Function {
    fn doc(&self) -> RcDoc {
        // todo -- check self.sugar, and print the sugared form.
        kwd("func")
            .append(self.name.doc())
            .append(self.input.doc())
            .append(if exp_is_block(&self.exp.0) {
                self.exp.doc()
            } else {
                enclose("{", self.exp.doc(), "}")
            })
    }
}

impl ToDoc for Dec {
    fn doc(&self) -> RcDoc {
        use Dec::*;
        match self {
            Attrs(_a, _d) => todo!(),
            Exp(e) => e.doc(),
            Let(p, e) => kwd("let")
                .append(p.doc())
                .append(str(" = "))
                .append(e.doc()),
            LetModule(_id, _, _dec) => kwd("let-module-TODO"),
            LetActor(_id, _, _dec) => kwd("let-actor-TODO"),
            LetImport(pat, _, import_string) => {
                kwd("import").append(pat.doc()).append(str(import_string))
            }
            Func(f) => f.doc(),
            Var(p, e) => kwd("var")
                .append(p.doc())
                .append(str(" = "))
                .append(e.doc()),
            Type(i, Some(b), t) => kwd("type")
                .append(i.doc())
                .append(bind(b))
                .append(" = ")
                .append(t.doc()),
            Type(i, None, t) => kwd("type").append(i.doc()).append(" = ").append(t.doc()),
            Class(_) => todo!(),
            _ => todo!(),
        }
    }
}

impl ToDoc for TypeTag {
    fn doc(&self) -> RcDoc {
        let tail = if self.typ.is_some() {
            space()
                .append(str(":"))
                .append(space())
                .append(self.typ.doc())
        } else {
            nil()
        };
        self.id.doc().append(tail)
    }
}

impl ToDoc for Type {
    fn doc(&self) -> RcDoc {
        use Type::*;
        match self {
            Prim(p) => p.doc(),
            Object(s, fs) => s.doc().append(RcDoc::space()).append(field_block(fs)),
            Array(m, t) => enclose("[", m.doc().append(t.doc()), "]"),
            Optional(t) => str("?").append(t.doc()),
            Tuple(d) => tuple(d),
            Function(_s, args, src, tgt) => {
                let args = args.as_ref().map(|t| vector(&t.vec, ","));
                let args = match args {
                    None => nil(),
                    Some(a) => a.clone(),
                };
                args.append(src.doc())
                    .append(space())
                    .append(str("->"))
                    .append(space().append(tgt.doc()))
            }
            Async(t) => kwd("async").append(t.doc()),
            //Async(_, _) => unimplemented!(), // scope?
            And(e1, e2) => bin_op(e1, str("and"), e2),
            Or(e1, e2) => bin_op(e1, str("or"), e2),
            Paren(e) => enclose("(", e.doc(), ")"),
            Unknown(id) => id.doc(),
            Known(id, t) => id.doc().append(" : ").append(t.doc()),
            Path(..) => todo!(),
            Item(i, t) => i
                .doc()
                .append(space())
                .append(str(":"))
                .append(space())
                .append(t.doc()),
            Variant(ts) => enclose("{", variant_vector(&ts.vec), "}"),
        }
    }
}

impl ToDoc for PrimType {
    fn doc(&self) -> RcDoc {
        use PrimType::*;
        str(match self {
            Null => "Null",
            Unit => "()",
            Bool => "Bool",
            Nat => "Nat",
            Nat8 => "Nat8",
            Nat16 => "Nat16",
            Nat32 => "Nat32",
            Nat64 => "Nat64",
            Int => "Int",
            Int8 => "Int8",
            Int16 => "Int16",
            Int32 => "Int32",
            Int64 => "Int64",
            Float => "Float",
            Char => "Char",
            Text => "Text",
            Principal => "Principal",
        })
    }
}

impl ToDoc for TypeBind {
    fn doc(&self) -> RcDoc {
        use BindSort::*;
        match self.sort {
            Scope => str("$"), // ?
            Type => RcDoc::nil(),
        }
        .append(self.var.doc())
        .append(" : ")
        .append(self.bound.doc())
    }
}

impl ToDoc for Pat {
    fn doc(&self) -> RcDoc {
        use Pat::*;
        match self {
            Wild => str("_"),
            Var(s) => s.doc(),
            UnOpLiteral(_u, _l) => todo!(),
            Literal(l) => l.doc(),
            Tuple(ps) => tuple(ps),
            Object(_) => todo!(),
            Optional(p) => str("?").append(p.doc()),
            Variant(s, p) => str("#")
                .append(s.doc())
                .append(p.as_ref().map(|p| p.doc()).unwrap_or(RcDoc::nil())),
            //            Alt(d) => delim_left(d, " |"),
            Annot(_t) => todo!(),
            AnnotPat(_p, _t) => todo!(),
            Paren(p) => enclose("(", p.doc(), ")"),
            Or(_, _) => todo!(),
            Unquote(_) => todo!(),
            TempVar(_) => todo!(),
        }
    }
}

impl ToDoc for Case {
    fn doc(&self) -> RcDoc {
        kwd("case")
            .append(self.pat.doc())
            .append(RcDoc::line())
            .append(self.exp.doc())
            .group()
    }
}

impl ToDoc for TypeField {
    fn doc(&self) -> RcDoc {
        match self {
            TypeField::Val(vtf) => vtf.id.doc().append(" : ").append(vtf.typ.doc()),
            _ => todo!(),
        }
    }
}

impl ToDoc for DecField {
    fn doc(&self) -> RcDoc {
        match &self.vis {
            None => RcDoc::nil(),
            Some(v) => v.doc().append(RcDoc::space()),
        }
        .append(match &self.stab {
            None => RcDoc::nil(),
            Some(s) => s.doc().append(RcDoc::space()),
        })
        .append(self.dec.doc())
    }
}

impl ToDoc for ExpField {
    fn doc(&self) -> RcDoc {
        self.mut_
            .doc()
            .append(self.id.doc())
            .append(match &self.typ {
                None => RcDoc::nil(),
                Some(typ) => str(" : ").append(typ.doc()),
            })
            .append(" = ")
            .append(self.exp.doc())
    }
}

impl ToDoc for Vis {
    fn doc(&self) -> RcDoc {
        use Vis::*;
        match self {
            Public(Some(_)) => todo!(), // ??
            Public(None) => str("public"),
            Private => str("private"),
            System => str("system"),
        }
    }
}

impl ToDoc for Stab {
    fn doc(&self) -> RcDoc {
        use Stab::*;
        str(match self {
            Stable => "stable",
            Flexible => "flexible",
        })
    }
}

impl ToDoc for Mut {
    fn doc(&self) -> RcDoc {
        use Mut::*;
        match self {
            Var => kwd("var"), // includes space after keyword
            Const => RcDoc::nil(),
        }
    }
}

impl ToDoc for ObjSort {
    fn doc(&self) -> RcDoc {
        str(match self {
            ObjSort::Object => "object",
            ObjSort::Actor => "actor",
            ObjSort::Module => "module",
        })
    }
}

// ---- Token-based code formatter ----

fn filter_whitespace(trees: &[TokenTree]) -> Vec<&TokenTree> {
    let mut results = vec![];
    filter_whitespace_(trees, &mut results);
    results
}

fn filter_whitespace_<'a>(trees: &'a [TokenTree], results: &mut Vec<&'a TokenTree>) {
    let len = trees.len();
    for (i, tt) in trees.iter().enumerate() {
        if match tt {
            TokenTree::Token(Loc(Token::Space(_), _))
            | TokenTree::Token(Loc(Token::Line(_), _)) if i == 0 || i + 1 == len
            // | TokenTree::Token(Loc(Token::MultiLine(_), _))
            => false,
            // TokenTree::Token(Loc(Token::Line(_), _)) if i == 0 || i == trees.len() - 1 => false,
            _ => true,
        } {
            results.push(tt);
        }
    }
}

fn get_space_between<'a>(a: &'a TokenTree, b: &'a TokenTree) -> RcDoc<'a> {
    use crate::lexer_types::Token::*;
    use GroupType::*;
    use TokenTree::*;
    match (a, b) {
        // TODO: refactor these rules to a text-based configuration file
        (Token(Loc(Space(_), _)), _) | (_, Token(Loc(Space(_), _))) => nil(),
        (Token(Loc(Line(_), _)), _) | (_, Token(Loc(Line(_), _))) => nil(),
        (Token(Loc(LineComment(_), _)), _) => RcDoc::hardline(),
        (Token(Loc(MultiLine(_), _)), _) | (_, Token(Loc(MultiLine(_), _))) => nil(),
        (Token(Loc(Ident(s), _)), Group(_, g, _))
            if !is_keyword(s) && (g == &Paren || g == &Square || g == &Angle) =>
        {
            nil()
        }
        (Token(Loc(Open(_), _)), _) | (_, Token(Loc(Close(_), _))) => nil(),
        (_, Token(Loc(Delim(_), _))) => nil(),
        (Token(Loc(Delim(_), _)), _) => line(),
        (Token(Loc(Dot(_), _)), _) => nil(),
        (Token(Loc(Operator(s), _)), _) if s.eq("?") => nil(),
        (_, Token(Loc(Operator(s), _))) if s.eq("!") => nil(),
        (_, Token(Loc(Operator(s), _))) if s.starts_with(' ') => nil(),
        (Token(Loc(Operator(s), _)), Token(Loc(Ident(_), _))) if s.eq("#") => nil(),
        (_, Token(Loc(Dot(_), _))) => wrap_(),
        (Token(Loc(Assign(_), _)), _) => wrap(),
        (_, Group(_, Comment, _)) => wrap(),
        (Group(_, Comment, _), _) => wrap(),
        _ => space(),
    }
}

impl ToDoc for TokenTree {
    fn doc(&self) -> RcDoc {
        use GroupType::*;
        use TokenTree::*;
        match self {
            Token(t) => t.doc(),
            Group(trees, sort, pair) => {
                let trees = filter_whitespace(trees);
                let doc = match trees.first() {
                    None => RcDoc::nil(),
                    Some(tt) => {
                        let mut doc = tt.doc();
                        for i in 0..trees.len() - 1 {
                            let (a, b) = (trees[i], trees[i + 1]);
                            doc = doc.append(get_space_between(a, b)).append(b.doc());
                        }
                        doc
                    }
                };
                // let concat = RcDoc::concat(docs);
                let (open, close) = if let Some((Loc(open, _), Loc(close, _))) = pair {
                    (&open.data().unwrap()[..], &close.data().unwrap()[..])
                } else {
                    ("", "") // TODO refactor GroupType into Option
                };
                match sort {
                    Unenclosed => doc,
                    Curly => enclose_space(open, doc, close),
                    Paren | Square | Angle => enclose(open, doc, close),
                    Comment => RcDoc::as_string(format!("{}", self)),
                }
            }
        }
    }
}

impl ToDoc for Token {
    fn doc(&self) -> RcDoc {
        use Token::*;
        match self {
            &Line(_) => RcDoc::hardline(),
            &MultiLine(_) => RcDoc::hardline().append(RcDoc::hardline()),
            t => str(t.data().unwrap()),
        }
        // str(self.data().unwrap())
    }
}
