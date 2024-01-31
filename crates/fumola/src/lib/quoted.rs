use crate::ast::{
    Case, CasesPos, Dec, DecField, DecFieldsPos, Delim, Exp, ExpField, IdPos, NodeData, Pat,
    PatField, QuotedAst, Type,
};
use crate::shared::Shared;
use crate::type_mismatch;
use crate::vm_types::{Env, Interruption};
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
            Pat::Wild => todo!(),
            Pat::Var(x) => Ok(Pat::Var(x.clone())),
            Pat::Literal(_) => todo!(),
            Pat::UnOpLiteral(_, _) => todo!(),
            Pat::Tuple(_) => todo!(),
            Pat::Object(_) => todo!(),
            Pat::Optional(_) => todo!(),
            Pat::Variant(_, _) => todo!(),
            Pat::Or(_, _) => todo!(),
            Pat::AnnotPat(_, _) => todo!(),
            Pat::Annot(_) => todo!(),
            Pat::Paren(_) => todo!(),
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
            Pat::TempVar(_) => todo!(),
        }
    }
}

impl QuotedClose for Dec {
    fn quoted_close(&self, env: &Env) -> Result<Dec, Interruption> {
        match &self {
            Dec::Exp(e) => Ok(Dec::Exp(e.quoted_close(env)?)),
            Dec::Let(p, e) => Ok(Dec::Let(p.quoted_close(env)?, e.quoted_close(env)?)), // to do -- remove pattern vars from Env
            Dec::LetImport(_, _, _) => todo!(),
            Dec::LetModule(_, _, _) => todo!(),
            Dec::LetActor(_, _, _) => todo!(),
            Dec::LetObject(_, _, _) => todo!(),
            Dec::Func(_) => todo!(),
            Dec::Var(_, _) => todo!(),
            Dec::Type(_, _, _) => todo!(),
            Dec::Class(_) => todo!(),
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
        todo!()
    }
}

impl QuotedClose for DecField {
    fn quoted_close(&self, _env: &Env) -> Result<DecField, Interruption> {
        todo!()
    }
}

impl QuotedClose for CasesPos {
    fn quoted_close(&self, _env: &Env) -> Result<CasesPos, Interruption> {
        todo!()
    }
}

impl QuotedClose for Case {
    fn quoted_close(&self, _env: &Env) -> Result<Case, Interruption> {
        todo!()
    }
}

impl QuotedClose for DecFieldsPos {
    fn quoted_close(&self, _env: &Env) -> Result<DecFieldsPos, Interruption> {
        todo!()
    }
}

impl QuotedClose for Exp {
    fn quoted_close(&self, env: &Env) -> Result<Exp, Interruption> {
        use Exp::*;
        match &self {
            Exp::Value_(_) => todo!(),
            Exp::Hole => todo!(),
            Exp::Prim(_) => todo!(),
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
            Exp::ActorUrl(_) => todo!(),
            Exp::Un(_, _) => todo!(),
            Exp::Bin(e1, b, e2) => Ok(Exp::Bin(
                e1.quoted_close(env)?,
                b.clone(),
                e2.quoted_close(env)?,
            )),
            Exp::Rel(_, _, _) => todo!(),
            Exp::Show(_) => todo!(),
            Exp::ToCandid(_) => todo!(),
            Exp::FromCandid(_) => todo!(),
            Exp::Tuple(_) => todo!(),
            Exp::Proj(_, _) => todo!(),
            Exp::Opt(_) => todo!(),
            Exp::DoOpt(_) => todo!(),
            Exp::Bang(_) => todo!(),
            Exp::ObjectBlock(_, _) => todo!(),
            Exp::Object(_) => todo!(),
            Exp::Variant(_, _) => todo!(),
            Exp::Dot(_, _) => todo!(),
            Exp::Assign(_, _) => todo!(),
            Exp::BinAssign(_, _, _) => todo!(),
            Exp::Array(_, _) => todo!(),
            Exp::Index(_, _) => todo!(),
            Exp::Function(_) => todo!(),
            Exp::Call(_, _, _) => todo!(),
            Exp::Block(b) => Ok(Block(b.quoted_close(env)?)),
            Exp::Do(e) => Ok(Do(e.quoted_close(env)?)),
            Exp::Not(_) => todo!(),
            Exp::And(_, _) => todo!(),
            Exp::Or(_, _) => todo!(),
            Exp::If(_, _, _) => todo!(),
            Exp::Switch(_, _) => todo!(),
            Exp::While(_, _) => todo!(),
            Exp::Loop(_, _) => todo!(),
            Exp::For(_, _, _) => todo!(),
            Exp::Label(_, _, _) => todo!(),
            Exp::Break(_, _) => todo!(),
            Exp::Return(_) => todo!(),
            Exp::Debug(_) => todo!(),
            Exp::DebugShow(_) => todo!(),
            Exp::Async(_) => todo!(),
            Exp::AsyncStar(_) => todo!(),
            Exp::Await(_) => todo!(),
            Exp::AwaitStar(_) => todo!(),
            Exp::Assert(_) => todo!(),
            Exp::Annot(h, e, t) => Ok(Annot(h.clone(), e.quoted_close(env)?, t.clone())),
            Exp::Import(_) => todo!(),
            Exp::Throw(_) => todo!(),
            Exp::Try(_, _) => todo!(),
            Exp::Ignore(_) => todo!(),
            Exp::Paren(e) => Ok(Paren(e.quoted_close(env)?)),
            Exp::QuotedAst(q) => Ok(QuotedAst(q.quoted_close(env)?)),
            Exp::Unquote(e) => Ok(Unquote(e.quoted_close(env)?)),
        }
    }
}

impl QuotedClose for QuotedAst {
    fn quoted_close(&self, env: &Env) -> Result<QuotedAst, Interruption> {
        use QuotedAst::*;
        Ok(match &self {
            QuotedAst::Empty => Empty,
            QuotedAst::Id(i) => Id(i.clone()),
            QuotedAst::Literal(l) => Literal(l.clone()),
            QuotedAst::TupleExps(es) => TupleExps(es.quoted_close(env)?),
            QuotedAst::TuplePats(ps) => TuplePats(ps.quoted_close(env)?),
            QuotedAst::RecordExps(es) => RecordExps(es.quoted_close(env)?),
            QuotedAst::RecordPats(ps) => RecordPats(ps.quoted_close(env)?),
            QuotedAst::Cases(cs) => Cases(cs.quoted_close(env)?),
            QuotedAst::Decs(ds) => Decs(ds.quoted_close(env)?),
            QuotedAst::DecFields(dfs) => DecFields(dfs.quoted_close(env)?),
        })
    }
}
