use crate::ast::{
    Case, CasesPos, Dec, DecField, DecFieldsPos, Delim, Exp, ExpField, NodeData, Pat, PatField,
    QuotedAst,
};
use crate::shared::Shared;
use crate::value::Value;
use crate::vm_types::Env;
use im_rc::Vector;

trait QuotedClose {
    fn quoted_close(&self, env: &Env) -> Self;
}

impl<T: QuotedClose + Clone> QuotedClose for Shared<T> {
    fn quoted_close(&self, env: &Env) -> Shared<T> {
        Shared::new(self.as_ref().quoted_close(env))
    }
}

impl<T: QuotedClose + Clone> QuotedClose for Delim<T> {
    fn quoted_close(&self, env: &Env) -> Delim<T> {
        let vec: Vector<_> = self.vec.iter().map(|item| item.quoted_close(env)).collect();
        Delim {
            vec,
            has_trailing: self.has_trailing,
        }
    }
}

impl<T: QuotedClose + Clone> QuotedClose for NodeData<T> {
    fn quoted_close(&self, env: &Env) -> NodeData<T> {
        NodeData(self.0.quoted_close(env), self.1.clone())
    }
}

impl QuotedClose for Value {
    fn quoted_close(&self, env: &Env) -> Value {
        self.clone()
    }
}

impl<T1: QuotedClose + Clone, T2: QuotedClose + Clone> QuotedClose for (T1, T2) {
    fn quoted_close(&self, env: &Env) -> (T1, T2) {
        (self.0.quoted_close(env), self.1.quoted_close(env))
    }
}

impl<T: QuotedClose + Clone> QuotedClose for Option<T> {
    fn quoted_close(&self, env: &Env) -> Option<T> {
        match &self {
            Some(_) => todo!(),
            None => todo!(),
        }
    }
}

impl QuotedClose for Pat {
    fn quoted_close(&self, env: &Env) -> Pat {
        match &self {
            Pat::Wild => todo!(),
            Pat::Var(_) => todo!(),
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
            Pat::Unquote(_) => todo!(),
            Pat::TempVar(_) => todo!(),
        }
    }
}

impl QuotedClose for Dec {
    fn quoted_close(&self, env: &Env) -> Dec {
        match &self {
            Dec::Exp(_) => todo!(),
            Dec::Let(_, _) => todo!(),
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
    fn quoted_close(&self, env: &Env) -> ExpField {
        todo!()
    }
}

impl QuotedClose for PatField {
    fn quoted_close(&self, env: &Env) -> PatField {
        todo!()
    }
}

impl QuotedClose for DecField {
    fn quoted_close(&self, env: &Env) -> DecField {
        todo!()
    }
}

impl QuotedClose for CasesPos {
    fn quoted_close(&self, env: &Env) -> CasesPos {
        todo!()
    }
}

impl QuotedClose for Case {
    fn quoted_close(&self, env: &Env) -> Case {
        todo!()
    }
}

impl QuotedClose for DecFieldsPos {
    fn quoted_close(&self, env: &Env) -> DecFieldsPos {
        todo!()
    }
}

impl QuotedClose for Exp {
    fn quoted_close(&self, env: &Env) -> Exp {
        match &self {
            Exp::Value_(_) => todo!(),
            Exp::Hole => todo!(),
            Exp::Prim(_) => todo!(),
            Exp::Var(_) => todo!(),
            Exp::Literal(_) => todo!(),
            Exp::ActorUrl(_) => todo!(),
            Exp::Un(_, _) => todo!(),
            Exp::Bin(_, _, _) => todo!(),
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
            Exp::Block(_) => todo!(),
            Exp::Do(_) => todo!(),
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
            Exp::Annot(_, _, _) => todo!(),
            Exp::Import(_) => todo!(),
            Exp::Throw(_) => todo!(),
            Exp::Try(_, _) => todo!(),
            Exp::Ignore(_) => todo!(),
            Exp::Paren(_) => todo!(),
            Exp::QuotedAst(_) => todo!(),
            Exp::Unquote(_) => todo!(),
        }
    }
}

impl QuotedClose for QuotedAst {
    fn quoted_close(&self, env: &Env) -> QuotedAst {
        use QuotedAst::*;
        match &self {
            QuotedAst::Empty => Empty,
            QuotedAst::Id(i) => Id(i.clone()),
            QuotedAst::Literal(l) => Literal(l.clone()),
            QuotedAst::TupleExps(es) => TupleExps(es.quoted_close(env)),
            QuotedAst::TuplePats(ps) => TuplePats(ps.quoted_close(env)),
            QuotedAst::RecordExps(es) => RecordExps(es.quoted_close(env)),
            QuotedAst::RecordPats(ps) => RecordPats(ps.quoted_close(env)),
            QuotedAst::Cases(cs) => Cases(cs.quoted_close(env)),
            QuotedAst::Decs(ds) => Decs(ds.quoted_close(env)),
            QuotedAst::DecFields(dfs) => DecFields(dfs.quoted_close(env)),
        }
    }
}
