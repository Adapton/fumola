use crate::vm_types::{Env, Interruption};
use crate::{nyi, type_mismatch};
use fumola_syntax::ast::{
    Case, CasesPos, Dec, DecField, DecFieldsPos, Delim, Exp, ExpField, IdPos, NodeData, Pat,
    PatField, QuotedAst, Type,
};
use fumola_syntax::shared::{Share, Shared};
use im_rc::Vector;

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
        let vec: Result<Vector<_>, _> = self
            .vec
            .iter()
            .map(|item| item.quoted_close(env))
            .try_collect();
        Ok(Delim {
            vec: vec?,
            has_trailing: self.has_trailing,
        })
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
            Pat::UnOpLiteral(..) => nyi!(line!(), "a negated literal pattern inside a quotation"),
            Pat::Tuple(ps) => {
                // to do -- Ok(Pat::Tuple(ps.map(|x| x.quoted_close(env))))
                Ok(Pat::Tuple(ps.clone()))
            }
            Pat::Object(_) => Ok(self.clone()), // to do -- FIX ME!
            Pat::Optional(p) => Ok(Pat::Optional(p.quoted_close(env)?)),
            Pat::Variant(id, pat) => {
                Ok(Pat::Variant(id.quoted_close(env)?, pat.quoted_close(env)?))
            }
            Pat::Or(..) => nyi!(line!(), "an or-pattern inside a quotation"),
            Pat::AnnotPat(p, t) => Ok(Pat::AnnotPat(p.quoted_close(env)?, t.clone())),
            Pat::Annot(..) => nyi!(line!(), "a type-annotation pattern inside a quotation"),
            Pat::Paren(..) => nyi!(line!(), "a parenthesized pattern inside a quotation"),
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
            Pat::TempVar(..) => nyi!(line!(), "a temporary-variable pattern inside a quotation"),
        }
    }
}

impl QuotedClose for Dec {
    fn quoted_close(&self, env: &Env) -> Result<Dec, Interruption> {
        match &self {
            Dec::Attrs(a, d) => Ok(Dec::Attrs(a.clone(), d.quoted_close(env)?)),
            Dec::Exp(e) => Ok(Dec::Exp(e.quoted_close(env)?)),
            Dec::Let(p, e) => {
                let p = p.quoted_close(env)?;
                // to do -- remove vars bound in p from env
                Ok(Dec::Let(p.quoted_close(env)?, e.quoted_close(env)?))
            }
            Dec::LetImport(..) => nyi!(line!(), "an import declaration inside a quotation"),
            Dec::LetModule(..) => nyi!(line!(), "a module declaration inside a quotation"),
            Dec::LetActor(..) => nyi!(line!(), "an actor declaration inside a quotation"),
            Dec::LetObject(..) => nyi!(line!(), "an object declaration inside a quotation"),
            Dec::Func(f) => Ok(Dec::Func(f.clone())), // to do -- fix me
            Dec::Var(..) => nyi!(line!(), "a var declaration inside a quotation"),
            Dec::Type(..) => nyi!(line!(), "a type declaration inside a quotation"),
            Dec::Class(..) => nyi!(line!(), "a class declaration inside a quotation"),
        }
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
    fn quoted_close(&self, _env: &Env) -> Result<PatField, Interruption> {
        nyi!(line!(), "an object-pattern field inside a quotation")
    }
}

impl QuotedClose for DecField {
    fn quoted_close(&self, _env: &Env) -> Result<DecField, Interruption> {
        nyi!(line!(), "a declaration field inside a quotation")
    }
}

impl QuotedClose for CasesPos {
    fn quoted_close(&self, _env: &Env) -> Result<CasesPos, Interruption> {
        nyi!(line!(), "switch cases inside a quotation")
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
    fn quoted_close(&self, _env: &Env) -> Result<DecFieldsPos, Interruption> {
        nyi!(line!(), "declaration fields inside a quotation")
    }
}

impl QuotedClose for Exp {
    fn quoted_close(&self, env: &Env) -> Result<Exp, Interruption> {
        use Exp::*;
        match &self {
            // Exp::Value_(_) => todo!(),
            Exp::Hole => nyi!(line!(), "a hole inside a quotation"),
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
            Exp::ActorUrl(..) => nyi!(line!(), "an actor URL inside a quotation"),
            Exp::Un(..) => nyi!(line!(), "a unary operation inside a quotation"),
            Exp::Bin(e1, b, e2) => Ok(Exp::Bin(
                e1.quoted_close(env)?,
                b.clone(),
                e2.quoted_close(env)?,
            )),
            Exp::Rel(..) => nyi!(line!(), "a comparison inside a quotation"),
            Exp::Show(..) => nyi!(line!(), "a show expression inside a quotation"),
            Exp::ToCandid(..) => nyi!(line!(), "a to-Candid expression inside a quotation"),
            Exp::FromCandid(..) => nyi!(line!(), "a from-Candid expression inside a quotation"),
            Exp::Tuple(es) => Ok(Tuple(es.quoted_close(env)?)),
            Exp::Proj(..) => nyi!(line!(), "a tuple projection inside a quotation"),
            Exp::Opt(..) => nyi!(line!(), "an option expression inside a quotation"),
            Exp::DoOpt(..) => nyi!(line!(), "a do-option block inside a quotation"),
            Exp::DoAdaptonNav(..) => nyi!(line!(), "an Adapton navigation inside a quotation"),
            Exp::DoAdaptonPutForceThunk(..) => {
                nyi!(line!(), "an Adapton put-force-thunk inside a quotation")
            }
            Exp::Bang(..) => nyi!(line!(), "a null-check (!) inside a quotation"),
            Exp::ObjectBlock(..) => nyi!(line!(), "an object block inside a quotation"),
            Exp::Object(..) => nyi!(line!(), "an object literal inside a quotation"),
            Exp::Variant(..) => nyi!(line!(), "a variant inside a quotation"),
            Exp::Dot(..) => nyi!(line!(), "a field projection inside a quotation"),
            Exp::Assign(..) => nyi!(line!(), "an assignment inside a quotation"),
            Exp::BinAssign(..) => nyi!(line!(), "a compound assignment inside a quotation"),
            Exp::Array(m, es) => Ok(Exp::Array(m.clone(), es.quoted_close(env)?)),
            Exp::Index(..) => nyi!(line!(), "an index expression inside a quotation"),
            Exp::Function(f) => Ok(Exp::Function(f.clone())), // to do -- fix me
            Exp::Call(fun, inst, args) => Ok(Call(
                fun.quoted_close(env)?,
                inst.clone(),
                args.quoted_close(env)?,
            )),
            Exp::Block(b) => Ok(Block(b.quoted_close(env)?)),
            Exp::Do(e) => Ok(Do(e.quoted_close(env)?)),
            Exp::DoMode(m, e) => Ok(DoMode(m.clone(), e.quoted_close(env)?)),
            Exp::Not(..) => nyi!(line!(), "a negation inside a quotation"),
            Exp::And(..) => nyi!(line!(), "an and expression inside a quotation"),
            Exp::Or(..) => nyi!(line!(), "an or expression inside a quotation"),
            Exp::If(..) => nyi!(line!(), "an if expression inside a quotation"),
            Exp::Switch(..) => nyi!(line!(), "a switch expression inside a quotation"),
            Exp::While(..) => nyi!(line!(), "a while loop inside a quotation"),
            Exp::Loop(..) => nyi!(line!(), "a loop inside a quotation"),
            Exp::For(..) => nyi!(line!(), "a for loop inside a quotation"),
            Exp::Label(..) => nyi!(line!(), "a label inside a quotation"),
            Exp::Break(..) => nyi!(line!(), "a break inside a quotation"),
            Exp::Return(..) => nyi!(line!(), "a return inside a quotation"),
            Exp::Debug(..) => nyi!(line!(), "a debug block inside a quotation"),
            Exp::DebugShow(..) => nyi!(line!(), "a debugShow expression inside a quotation"),
            Exp::Async(..) => nyi!(line!(), "an async block inside a quotation"),
            Exp::AsyncStar(..) => nyi!(line!(), "an async* block inside a quotation"),
            Exp::Await(..) => nyi!(line!(), "an await inside a quotation"),
            Exp::AwaitStar(..) => nyi!(line!(), "an await* inside a quotation"),
            Exp::Assert(..) => nyi!(line!(), "an assertion inside a quotation"),
            Exp::Annot(h, e, t) => Ok(Annot(h.clone(), e.quoted_close(env)?, t.clone())),
            Exp::Import(..) => nyi!(line!(), "an import expression inside a quotation"),
            Exp::Throw(..) => nyi!(line!(), "a throw inside a quotation"),
            Exp::Try(..) => nyi!(line!(), "a try expression inside a quotation"),
            Exp::Ignore(..) => nyi!(line!(), "an ignore expression inside a quotation"),
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
        (TuplePats(_), TuplePats(_)) => nyi!(line!(), "appending two quoted tuple patterns"),
        (RecordPats(_), RecordPats(_)) => nyi!(line!(), "appending two quoted record patterns"),
        (DecFields(_), DecFields(_)) => nyi!(line!(), "appending two quoted declaration fields"),
        (_, _) => crate::type_mismatch!(file!(), line!()),
    }
}
