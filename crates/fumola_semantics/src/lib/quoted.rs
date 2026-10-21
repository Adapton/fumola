use crate::type_mismatch;
use crate::vm_types::{Env, Interruption};
use fumola_syntax::ast::{
    AdaptonNav, Case, CasesPos, Class, Dec, DecField, DecFieldsPos, Delim, Exp, ExpField, Function,
    IdPos, NodeData, Pat, PatField, QuotedAst, Type, Unquote,
};
use fumola_syntax::shared::{Share, Shared};
use im_rc::Vector;

/// Closing a quotation: replacing each unquote inside it (`~x`) with what
/// `x` holds in the environment, and leaving everything else as written.
///
/// Every form is closed by closing its parts. The parts that cannot hold an
/// unquote -- types, operators, literals, type instantiations, evaluation
/// modes, attributes -- are copied.
///
/// Scope is not tracked: a variable bound inside the quotation does not
/// shadow the environment's binding of the same name (see the note on
/// `Dec::Let`). That is how `let` has always been closed here, and binders
/// of every other kind -- function parameters, case patterns, `for` loops --
/// now follow the same rule.
pub trait QuotedClose {
    fn quoted_close(&self, env: &Env) -> Result<Self, Interruption>
    where
        Self: Sized;
}

impl<T: QuotedClose + Clone> QuotedClose for Shared<T> {
    fn quoted_close(&self, env: &Env) -> Result<Shared<T>, Interruption> {
        Ok(Shared::new(self.as_ref().quoted_close(env)?))
    }
}

impl<T: QuotedClose + Clone> QuotedClose for Delim<T> {
    fn quoted_close(&self, env: &Env) -> Result<Delim<T>, Interruption> {
        Ok(Delim {
            vec: self.vec.quoted_close(env)?,
            has_trailing: self.has_trailing,
        })
    }
}

impl<T: QuotedClose + Clone> QuotedClose for Vector<T> {
    fn quoted_close(&self, env: &Env) -> Result<Vector<T>, Interruption> {
        self.iter().map(|item| item.quoted_close(env)).try_collect()
    }
}

impl<T: QuotedClose + Clone> QuotedClose for NodeData<T> {
    fn quoted_close(&self, env: &Env) -> Result<NodeData<T>, Interruption> {
        // to do -- sometimes we want to keep the source information.
        //          sometimes we want to clear it.
        //
        // use a special component of Env to indicate that we should "clear" out the source position information, if set.
        //
        Ok(NodeData(self.0.quoted_close(env)?, self.1.clone()))
    }
}

impl QuotedClose for IdPos {
    fn quoted_close(&self, env: &Env) -> Result<IdPos, Interruption> {
        if self.unquote {
            match env.get(&self.id.0) {
                None => Ok(self.clone()),
                Some(v) => Ok(IdPos {
                    unquote: false,
                    id: v.unquote_id()?,
                }),
            }
        } else {
            Ok(self.clone())
        }
    }
}

impl<T1: QuotedClose + Clone, T2: QuotedClose + Clone> QuotedClose for (T1, T2) {
    fn quoted_close(&self, env: &Env) -> Result<(T1, T2), Interruption> {
        Ok((self.0.quoted_close(env)?, self.1.quoted_close(env)?))
    }
}

impl<T: QuotedClose + Clone> QuotedClose for Option<T> {
    fn quoted_close(&self, env: &Env) -> Result<Option<T>, Interruption> {
        match &self {
            Some(x) => Ok(Some(x.quoted_close(env)?)),
            None => Ok(None),
        }
    }
}

impl QuotedClose for Pat {
    fn quoted_close(&self, env: &Env) -> Result<Pat, Interruption> {
        match &self {
            Pat::Wild => Ok(Pat::Wild),
            Pat::Var(x) => Ok(Pat::Var(x.clone())),
            Pat::Literal(l) => Ok(Pat::Literal(l.clone())),
            // `-1`: an operator and a literal, neither of which can be unquoted.
            Pat::UnOpLiteral(op, l) => Ok(Pat::UnOpLiteral(op.clone(), l.clone())),
            Pat::Tuple(ps) => Ok(Pat::Tuple(ps.quoted_close(env)?)),
            Pat::Object(fs) => Ok(Pat::Object(fs.quoted_close(env)?)),
            Pat::Optional(p) => Ok(Pat::Optional(p.quoted_close(env)?)),
            Pat::Variant(id, pat) => {
                Ok(Pat::Variant(id.quoted_close(env)?, pat.quoted_close(env)?))
            }
            Pat::Or(p1, p2) => Ok(Pat::Or(p1.quoted_close(env)?, p2.quoted_close(env)?)),
            Pat::AnnotPat(p, t) => Ok(Pat::AnnotPat(p.quoted_close(env)?, t.clone())),
            Pat::Annot(t) => Ok(Pat::Annot(t.clone())),
            Pat::Paren(p) => Ok(Pat::Paren(p.quoted_close(env)?)),
            Pat::Unquote(i) => match env.get(&i.id.0) {
                Some(v) => {
                    if v.is_quoted_id() {
                        Ok(Pat::Var(v.unquote_id()?))
                    } else {
                        Ok(v.unquote_pat()?.0.clone())
                    }
                }
                None => Err(Interruption::UnboundIdentifer(i.id.0.clone())),
            },
            // Made by the desugarer, never written, so never unquoted.
            Pat::TempVar(n) => Ok(Pat::TempVar(*n)),
        }
    }
}

impl QuotedClose for Dec {
    fn quoted_close(&self, env: &Env) -> Result<Dec, Interruption> {
        match &self {
            Dec::Attrs(a, d) => Ok(Dec::Attrs(a.clone(), d.quoted_close(env)?)),
            Dec::Exp(e) => Ok(Dec::Exp(e.quoted_close(env)?)),
            Dec::Let(p, e) => {
                // to do -- remove vars bound in p from env
                Ok(Dec::Let(p.quoted_close(env)?, e.quoted_close(env)?))
            }
            Dec::LetImport(p, sugar, path) => Ok(Dec::LetImport(
                p.quoted_close(env)?,
                sugar.clone(),
                path.clone(),
            )),
            Dec::LetModule(id, sugar, fields) => Ok(Dec::LetModule(
                id.quoted_close(env)?,
                sugar.clone(),
                fields.quoted_close(env)?,
            )),
            Dec::LetActor(id, sugar, fields) => Ok(Dec::LetActor(
                id.quoted_close(env)?,
                sugar.clone(),
                fields.quoted_close(env)?,
            )),
            Dec::LetObject(id, sugar, fields) => Ok(Dec::LetObject(
                id.quoted_close(env)?,
                sugar.clone(),
                fields.quoted_close(env)?,
            )),
            Dec::Func(f) => Ok(Dec::Func(f.quoted_close(env)?)),
            Dec::Var(p, e) => Ok(Dec::Var(p.quoted_close(env)?, e.quoted_close(env)?)),
            // A type declaration holds only types, which are not closed.
            Dec::Type(id, binds, t) => Ok(Dec::Type(id.clone(), binds.clone(), t.clone())),
            Dec::Class(c) => Ok(Dec::Class(c.quoted_close(env)?)),
        }
    }
}

impl QuotedClose for Function {
    fn quoted_close(&self, env: &Env) -> Result<Function, Interruption> {
        Ok(Function {
            name: self.name.quoted_close(env)?,
            shared: self.shared.clone(),
            binds: self.binds.clone(),
            input: self.input.quoted_close(env)?,
            output: self.output.clone(),
            sugar: self.sugar.clone(),
            exp: self.exp.quoted_close(env)?,
        })
    }
}

impl QuotedClose for Class {
    fn quoted_close(&self, env: &Env) -> Result<Class, Interruption> {
        Ok(Class {
            shared: self.shared.clone(),
            sort: self.sort.clone(),
            typ_id: self.typ_id.clone(),
            binds: self.binds.clone(),
            input: self.input.quoted_close(env)?,
            typ: self.typ.clone(),
            name: self.name.clone(),
            fields: self.fields.quoted_close(env)?,
        })
    }
}

impl QuotedClose for ExpField {
    fn quoted_close(&self, env: &Env) -> Result<ExpField, Interruption> {
        Ok(ExpField {
            id: self.id.quoted_close(env)?,
            mut_: self.mut_.clone(),
            typ: self.typ.quoted_close(env)?,
            exp: self.exp.quoted_close(env)?,
        })
    }
}

impl QuotedClose for Type {
    fn quoted_close(&self, _env: &Env) -> Result<Type, Interruption> {
        Ok(self.clone()) // to do
    }
}

impl QuotedClose for PatField {
    fn quoted_close(&self, env: &Env) -> Result<PatField, Interruption> {
        Ok(PatField {
            id: self.id.quoted_close(env)?,
            pat: self.pat.quoted_close(env)?,
        })
    }
}

impl QuotedClose for DecField {
    fn quoted_close(&self, env: &Env) -> Result<DecField, Interruption> {
        Ok(DecField {
            attrs: self.attrs.clone(),
            dec: self.dec.quoted_close(env)?,
            vis: self.vis.clone(),
            stab: self.stab.clone(),
        })
    }
}

/// The quotation an unquote in a list position names.
///
/// `None` when the name is unbound here: the unquote is left standing, to be
/// closed by an enclosing quotation or reported by the evaluator (see
/// `CasesPos::cases`), as an unbound `~x` in a name position is.
fn unquoted_ast(env: &Env, u: &Unquote) -> Result<Option<QuotedAst>, Interruption> {
    match env.get(&u.id.0) {
        None => Ok(None),
        Some(v) => Ok(Some(v.unquote_ast()?)),
    }
}

impl QuotedClose for CasesPos {
    fn quoted_close(&self, env: &Env) -> Result<CasesPos, Interruption> {
        match self {
            CasesPos::Cases(cs) => Ok(CasesPos::Cases(cs.quoted_close(env)?)),
            // `switch e ~cases`, where `cases` holds a quotation like `{case 0 0}`.
            CasesPos::Unquote(u) => match unquoted_ast(env, &u.0)? {
                None => Ok(self.clone()),
                Some(QuotedAst::Cases(cs)) => Ok(CasesPos::Cases(cs)),
                Some(QuotedAst::Empty) => Ok(CasesPos::Cases(Delim::new())),
                Some(_) => type_mismatch!(file!(), line!()),
            },
        }
    }
}

impl QuotedClose for Case {
    fn quoted_close(&self, env: &Env) -> Result<Case, Interruption> {
        Ok(Case {
            pat: self.pat.quoted_close(env)?,
            exp: (self.exp.quoted_close(env)?),
        })
    }
}

impl QuotedClose for DecFieldsPos {
    fn quoted_close(&self, env: &Env) -> Result<DecFieldsPos, Interruption> {
        match self {
            DecFieldsPos::DecFields(dfs) => Ok(DecFieldsPos::DecFields(dfs.quoted_close(env)?)),
            // `module { ~fields }`, and the object and actor forms like it.
            DecFieldsPos::Unquote(u) => match unquoted_ast(env, &u.0)? {
                None => Ok(self.clone()),
                Some(QuotedAst::DecFields(dfs)) => Ok(DecFieldsPos::DecFields(dfs)),
                Some(QuotedAst::Empty) => Ok(DecFieldsPos::DecFields(Delim::new())),
                Some(_) => type_mismatch!(file!(), line!()),
            },
        }
    }
}

impl QuotedClose for AdaptonNav {
    fn quoted_close(&self, env: &Env) -> Result<AdaptonNav, Interruption> {
        match self {
            AdaptonNav::Goto(dim, e) => Ok(AdaptonNav::Goto(dim.clone(), e.quoted_close(env)?)),
            AdaptonNav::Within(dim, e) => Ok(AdaptonNav::Within(dim.clone(), e.quoted_close(env)?)),
        }
    }
}

impl QuotedClose for Exp {
    fn quoted_close(&self, env: &Env) -> Result<Exp, Interruption> {
        use Exp::*;
        match &self {
            // Exp::Value_(_) => todo!(),
            Exp::Hole => Ok(Hole),
            Exp::Prim(_) => Ok(self.clone()),
            Exp::Var(x) => {
                if x.0.unquote {
                    match env.get(x.0.id_ref()) {
                        None => type_mismatch!(file!(), line!()),
                        Some(v) => {
                            if v.is_quoted_ast() {
                                Ok(v.unquote_exp()?.0.clone())
                            } else {
                                type_mismatch!(file!(), line!())
                            }
                        }
                    }
                } else {
                    match env.get(x.0.id_ref()) {
                        Some(v) => match v.unquote_ast() {
                            Ok(ast) => Ok(Exp::QuotedAst(ast)), // leave quoted (unquote is false).
                            Err(_) => Ok(Exp::Var(x.clone())), // to do -- mark as "free var forever"
                        },
                        None => Ok(Exp::Var(x.clone())), // to do -- mark as "free var forever"
                    }
                }
            }
            Exp::Literal(l) => Ok(Exp::Literal(l.clone())),
            Exp::ActorUrl(e) => Ok(ActorUrl(e.quoted_close(env)?)),
            Exp::Un(op, e) => Ok(Un(op.clone(), e.quoted_close(env)?)),
            Exp::Bin(e1, b, e2) => Ok(Exp::Bin(
                e1.quoted_close(env)?,
                b.clone(),
                e2.quoted_close(env)?,
            )),
            Exp::Rel(e1, r, e2) => Ok(Rel(e1.quoted_close(env)?, r.clone(), e2.quoted_close(env)?)),
            Exp::Show(e) => Ok(Show(e.quoted_close(env)?)),
            Exp::ToCandid(es) => Ok(ToCandid(es.quoted_close(env)?)),
            Exp::FromCandid(e) => Ok(FromCandid(e.quoted_close(env)?)),
            Exp::Tuple(es) => Ok(Tuple(es.quoted_close(env)?)),
            Exp::Proj(e, i) => Ok(Proj(e.quoted_close(env)?, i.clone())),
            Exp::Opt(e) => Ok(Opt(e.quoted_close(env)?)),
            Exp::DoOpt(e) => Ok(DoOpt(e.quoted_close(env)?)),
            Exp::DoAdaptonNav(navs, e) => {
                Ok(DoAdaptonNav(navs.quoted_close(env)?, e.quoted_close(env)?))
            }
            Exp::DoAdaptonPutForceThunk(e1, e2) => Ok(DoAdaptonPutForceThunk(
                e1.quoted_close(env)?,
                e2.quoted_close(env)?,
            )),
            Exp::Bang(e) => Ok(Bang(e.quoted_close(env)?)),
            Exp::ObjectBlock(sort, fields) => {
                Ok(ObjectBlock(sort.clone(), fields.quoted_close(env)?))
            }
            Exp::Object(body) => Ok(Object(body.quoted_close(env)?)),
            Exp::Variant(id, e) => Ok(Variant(id.quoted_close(env)?, e.quoted_close(env)?)),
            Exp::Dot(e, id) => Ok(Dot(e.quoted_close(env)?, id.quoted_close(env)?)),
            Exp::Assign(e1, e2) => Ok(Assign(e1.quoted_close(env)?, e2.quoted_close(env)?)),
            Exp::BinAssign(e1, b, e2) => Ok(BinAssign(
                e1.quoted_close(env)?,
                b.clone(),
                e2.quoted_close(env)?,
            )),
            Exp::Array(m, es) => Ok(Exp::Array(m.clone(), es.quoted_close(env)?)),
            Exp::Index(e1, e2) => Ok(Index(e1.quoted_close(env)?, e2.quoted_close(env)?)),
            Exp::Function(f) => Ok(Exp::Function(f.quoted_close(env)?)),
            Exp::Call(fun, inst, args) => Ok(Call(
                fun.quoted_close(env)?,
                inst.clone(),
                args.quoted_close(env)?,
            )),
            Exp::Block(b) => Ok(Block(b.quoted_close(env)?)),
            Exp::Do(e) => Ok(Do(e.quoted_close(env)?)),
            Exp::DoMode(m, e) => Ok(DoMode(m.clone(), e.quoted_close(env)?)),
            Exp::Not(e) => Ok(Not(e.quoted_close(env)?)),
            Exp::And(e1, e2) => Ok(And(e1.quoted_close(env)?, e2.quoted_close(env)?)),
            Exp::Or(e1, e2) => Ok(Or(e1.quoted_close(env)?, e2.quoted_close(env)?)),
            Exp::If(c, t, e) => Ok(If(
                c.quoted_close(env)?,
                t.quoted_close(env)?,
                e.quoted_close(env)?,
            )),
            Exp::Switch(e, cases) => Ok(Switch(e.quoted_close(env)?, cases.quoted_close(env)?)),
            Exp::While(c, body) => Ok(While(c.quoted_close(env)?, body.quoted_close(env)?)),
            Exp::Loop(body, cond) => Ok(Loop(body.quoted_close(env)?, cond.quoted_close(env)?)),
            Exp::For(p, e, body) => Ok(For(
                p.quoted_close(env)?,
                e.quoted_close(env)?,
                body.quoted_close(env)?,
            )),
            Exp::Label(id, t, e) => Ok(Label(id.clone(), t.clone(), e.quoted_close(env)?)),
            Exp::Break(id, e) => Ok(Break(id.clone(), e.quoted_close(env)?)),
            Exp::Return(e) => Ok(Return(e.quoted_close(env)?)),
            Exp::Debug(e) => Ok(Debug(e.quoted_close(env)?)),
            Exp::DebugShow(e) => Ok(DebugShow(e.quoted_close(env)?)),
            Exp::Async(e) => Ok(Async(e.quoted_close(env)?)),
            Exp::AsyncStar(e) => Ok(AsyncStar(e.quoted_close(env)?)),
            Exp::Await(e) => Ok(Await(e.quoted_close(env)?)),
            Exp::AwaitStar(e) => Ok(AwaitStar(e.quoted_close(env)?)),
            Exp::Assert(e) => Ok(Assert(e.quoted_close(env)?)),
            Exp::Annot(h, e, t) => Ok(Annot(h.clone(), e.quoted_close(env)?, t.clone())),
            Exp::Import(path) => Ok(Import(path.clone())),
            Exp::Throw(e) => Ok(Throw(e.quoted_close(env)?)),
            Exp::Try(e, case) => Ok(Try(e.quoted_close(env)?, case.quoted_close(env)?)),
            Exp::Ignore(e) => Ok(Ignore(e.quoted_close(env)?)),
            Exp::Paren(e) => Ok(Paren(e.quoted_close(env)?)),
            Exp::QuotedAst(q) => Ok(QuotedAst(q.quoted_close(env)?)),
            Exp::Unquote(e) => Ok(Unquote(e.quoted_close(env)?)),
            Exp::Thunk(e) => Ok(Thunk(e.quoted_close(env)?)),
            Exp::GetAdaptonPointer(e) => Ok(GetAdaptonPointer(e.quoted_close(env)?)),
            Exp::Force(e) => Ok(Force(e.quoted_close(env)?)),
        }
    }
}

impl QuotedClose for QuotedAst {
    fn quoted_close(&self, env: &Env) -> Result<QuotedAst, Interruption> {
        use QuotedAst::*;
        Ok(match &self {
            QuotedAst::Empty => Empty,
            QuotedAst::Id_(i) => Id_(i.clone()),
            QuotedAst::Id(i) => Id(i.clone()),
            QuotedAst::Literal(l) => Literal(l.clone()),
            QuotedAst::TupleExps(es) => TupleExps(es.quoted_close(env)?),
            QuotedAst::TuplePats(ps) => TuplePats(ps.quoted_close(env)?),
            QuotedAst::RecordExps(es) => RecordExps(es.quoted_close(env)?),
            QuotedAst::RecordPats(ps) => RecordPats(ps.quoted_close(env)?),
            QuotedAst::Cases(cs) => Cases(cs.quoted_close(env)?),
            QuotedAst::Decs(ds) => Decs(ds.quoted_close(env)?),
            QuotedAst::DecFields(dfs) => DecFields(dfs.quoted_close(env)?),
            QuotedAst::Types(ts) => Types(ts.clone()),
            QuotedAst::Attrs(atts) => Attrs(atts.clone()),
        })
    }
}

pub fn append(
    first: &QuotedAst,
    other: &QuotedAst,
) -> Result<QuotedAst, crate::vm_types::Interruption> {
    use QuotedAst::*;
    match (first, other) {
        (Empty, _) => Ok(other.clone()),
        (_, Empty) => Ok(first.clone()),
        (TupleExps(es1), TupleExps(es2)) => Ok(TupleExps(es1.append(es2))),
        (RecordExps((None, None)), RecordExps((es1, es2))) => {
            Ok(RecordExps((es1.clone(), es2.clone())))
        }
        (RecordExps((es1, None)), RecordExps((None, Some(es3)))) => {
            Ok(RecordExps((es1.clone(), Some(es3.clone()))))
        }
        (RecordExps((es1, es2)), RecordExps((None, es3))) => Ok(match (es2, es3) {
            (Some(es2), None) => RecordExps((es1.clone(), Some(es2.clone()))),
            (None, es3) => RecordExps((es1.clone(), es3.clone())),
            (Some(es2), Some(es3)) => RecordExps((es1.clone(), Some(es2.append(es3)))),
        }),
        (Cases(cs1), Cases(cs2)) => Ok(Cases(cs1.append(cs2))),
        (Id_(i1), Id_(i2)) => Ok(Id_(NodeData(
            fumola_syntax::ast::Id::new(format!("{}{}", i1.0.as_str(), i2.0.as_str())),
            fumola_syntax::ast::Source::Evaluation,
        )
        .share())),
        (Decs(ds1), Decs(ds2)) => Ok(Decs(ds1.append(ds2))),
        // The three lists below append as the others do: in order, as written.
        (TuplePats(ps1), TuplePats(ps2)) => Ok(TuplePats(ps1.append(ps2))),
        (RecordPats(fs1), RecordPats(fs2)) => Ok(RecordPats(fs1.append(fs2))),
        (DecFields(dfs1), DecFields(dfs2)) => Ok(DecFields(dfs1.append(dfs2))),
        (_, _) => crate::type_mismatch!(file!(), line!()),
    }
}
