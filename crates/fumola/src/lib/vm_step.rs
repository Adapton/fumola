use crate::adapton::Navigation as AdaptonNav;
use crate::ast::{
    AdaptonNav as AdaptonNavAst, AdaptonNavDim, AdaptonNav_, Dec, Dec_, Delim, Exp, ExpField_,
    Exp_, Id, IdPos_, Literal, Pat, Pat_, Source, Type,
};
use crate::shared::{FastClone, Share};
use crate::value::{ActorId, Closed, ClosedFunction, Value, Value_};
use crate::vm_types::{
    def::{Def, Field as FieldDef},
    stack::{FieldContext, Frame, FrameCont},
    Active, ActiveBorrow, Breakpoint, Cont, Interruption, Limit, Limits, ModulePath, Step,
};
use im_rc::{vector, HashMap, Vector};

pub use crate::{nyi, type_mismatch, type_mismatch_};

pub use crate::vm_stack_cont::call_function_def;
use crate::vm_stack_cont::stack_cont;

pub fn unit_step<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    *active.cont() = Cont::Value_(Value::Unit.share());
    Ok(Step {})
}

pub fn return_step<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    *active.cont() = Cont::Value_(v);
    Ok(Step {})
}

pub fn literal_step<A: Active>(active: &mut A, l: &Literal) -> Result<Step, Interruption> {
    // TODO: partial evaluation would now be highly efficient due to value sharing
    *active.cont() = cont_value(Value::from_literal(l).map_err(Interruption::ValueError)?);
    Ok(Step {})
}

fn var_idpos_step<A: Active>(active: &mut A, x: &IdPos_) -> Result<Step, Interruption> {
    var_step(active, &x.0.id())
}

pub fn var_step<A: Active>(active: &mut A, x: &Id) -> Result<Step, Interruption> {
    match active.env().get(&x) {
        None => {
            if x.string.starts_with("@") {
                let f = crate::value::PrimFunction::AtSignVar(x.to_string());
                let v = Value::PrimFunction(f).share();
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            } else {
                let ctx = active.defs().active_ctx.clone();
                let fd = crate::vm_def::resolve_def(active.defs(), &ctx, false, x)?;
                let v = crate::vm_def::def_as_value(active.defs(), &x, &fd.def)?;
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
        }
        Some(v) => {
            *active.cont() = Cont::Value_(v.fast_clone());
            Ok(Step {})
        }
    }
}

pub fn tuple_step<A: Active>(active: &mut A, es: &Vector<Exp_>) -> Result<Step, Interruption> {
    let mut es: Vector<_> = es.fast_clone();
    match es.pop_front() {
        None => unit_step(active),
        Some(e1) => exp_conts(active, FrameCont::Tuple(Vector::new(), es), &e1),
    }
}

pub fn object_step<A: Active>(
    active: &mut A,
    bases: &Option<Delim<Exp_>>,
    fields: &Option<Delim<ExpField_>>,
) -> Result<Step, Interruption> {
    if let Some(_bases) = bases {
        return nyi!(line!());
    };
    if let Some(fields) = fields {
        let mut fs: Vector<_> = fields.vec.fast_clone();
        match fs.pop_front() {
            None => {
                *active.cont() = cont_value(Value::Object(HashMap::new()));
                Ok(Step {})
            }
            Some(f1) => {
                let fc = FieldContext {
                    mut_: f1.0.mut_.clone(),
                    id: f1.0.id.0.id_(),
                    typ: f1.0.typ.fast_clone(),
                };
                exp_conts(
                    active,
                    FrameCont::Object(Vector::new(), fc, fs),
                    &f1.0.exp_(),
                )
            }
        }
    } else {
        *active.cont() = cont_value(Value::Object(HashMap::new()));
        Ok(Step {})
    }
}

pub fn closed<A: Active, Content>(active: &mut A, content: Content) -> Closed<Content> {
    let env = active.env().fast_clone();
    Closed {
        ctx: active.defs().active_ctx.clone(),
        env,
        content,
    }
}

// step_adapton_nav
//
// Why? -- the parser was designed with fine-grained AST elements, for better or worse.
// the interface to the adapton module uses a more "normalized" format, which is simpler to pattern match in Rust.
// step_adapton_nav bridges that "syntantic gap" step by step, as expressions are normalized into each symbolic coordinate.
pub fn step_adapton_nav<A: Active>(
    active: &mut A,
    nav_done: Vector<(AdaptonNav, Value_)>,
    mut nav: Vector<AdaptonNav_>,
    body: &Exp_,
) -> Result<Step, Interruption> {
    let first = nav.pop_front().unwrap();
    let (tag, e) = match &first.0 {
        AdaptonNavAst::Goto(Some(d), e) => match d.0 {
            AdaptonNavDim::Time => (AdaptonNav::GotoTime, e),
            AdaptonNavDim::Space => (AdaptonNav::GotoSpace, e),
        },
        AdaptonNavAst::Goto(None, _) => type_mismatch!(file!(), line!()),
        AdaptonNavAst::Within(Some(d), e) => match d.0 {
            AdaptonNavDim::Time => (AdaptonNav::WithinTime, e),
            AdaptonNavDim::Space => (AdaptonNav::WithinSpace, e),
        },
        AdaptonNavAst::Within(None, _) => type_mismatch!(file!(), line!()),
    };
    exp_conts(
        active,
        FrameCont::DoAdaptonNav1(nav_done, tag, nav, body.clone()),
        e,
    )
}

pub fn exp_step<A: Active>(active: &mut A, exp: Exp_) -> Result<Step, Interruption> {
    use Exp::*;
    let source = exp.1.clone();
    match &exp.0 {
        Value_(v) => {
            *active.cont() = cont_value((**v).clone());
            Ok(Step {})
        }
        QuotedAst(q) => {
            use crate::quoted::QuotedClose;
            *active.cont() = cont_value(Value::QuotedAst(q.quoted_close(active.env())?));
            Ok(Step {})
        }
        Unquote(e) => exp_conts(active, FrameCont::Unquote, e),
        Literal(l) => literal_step(active, l),
        Function(f) => {
            *active.cont() = cont_value(Value::Function(ClosedFunction(closed(active, f.clone()))));
            Ok(Step {})
        }
        Thunk(e) => {
            *active.cont() = cont_value(Value::Thunk(closed(active, e.fast_clone())));
            Ok(Step {})
        }
        Call(e1, inst, e2) => {
            exp_conts(active, FrameCont::Call1(inst.clone(), e2.fast_clone()), e1)
        }
        Return(None) => return_(active, Value::Unit.share()),
        Return(Some(e)) => exp_conts(active, FrameCont::Return, e),
        Var(x) => var_idpos_step(active, x),
        Bin(e1, binop, e2) => exp_conts(
            active,
            FrameCont::BinOp1(binop.clone(), e2.fast_clone()),
            e1,
        ),
        Un(un, e) => exp_conts(active, FrameCont::UnOp(un.clone()), e),
        Paren(e) => exp_conts(active, FrameCont::Paren, e),
        Variant(id, None) => {
            // TODO: cache and share variants?
            *active.cont() = cont_value(Value::Variant(id.0.id(), None));
            Ok(Step {})
        }
        Variant(id, Some(e)) => exp_conts(active, FrameCont::Variant(id.0.id_()), e),
        Switch(e1, cases) => exp_conts(active, FrameCont::Switch(cases.cases().clone()), e1),
        Block(decs) => exp_conts_(
            active,
            source.clone(),
            FrameCont::Block,
            Cont::Decs(decs.vec.clone()),
            source,
        ),
        Do(e) => exp_conts(active, FrameCont::Do, e),
        Assert(e) => exp_conts(active, FrameCont::Assert, e),
        Object((bases, fields)) => object_step(active, bases, fields),
        Tuple(es) => tuple_step(active, &es.vec),
        Array(mut_, es) => {
            let mut es: Vector<_> = es.vec.fast_clone();
            match es.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Array(mut_.clone(), Vector::new()));
                    Ok(Step {})
                }
                Some(e1) => exp_conts(
                    active,
                    FrameCont::Array(mut_.clone(), Vector::new(), es),
                    &e1,
                ),
            }
        }
        Index(e1, e2) => exp_conts(active, FrameCont::Idx1(e2.fast_clone()), e1),
        Annot(_, e, t) => {
            match &t.0 {
                Type::Prim(pt) => *active.cont_prim_type() = Some(pt.clone()),
                _ => {}
            };
            exp_conts(active, FrameCont::Annot(t.fast_clone()), e)
        }
        Assign(e1, e2) => exp_conts(active, FrameCont::Assign1(e2.fast_clone()), e1),
        BinAssign(e1, b, e2) => exp_conts(
            active,
            FrameCont::BinAssign1(b.clone(), e2.fast_clone()),
            e1,
        ),
        Proj(e1, i) => exp_conts(active, FrameCont::Proj(i.clone()), e1),
        Dot(e1, f) => exp_conts(active, FrameCont::Dot(f.0.id_()), e1),
        If(e1, e2, e3) => exp_conts(active, FrameCont::If(e2.fast_clone(), e3.fast_clone()), e1),
        Rel(e1, relop, e2) => exp_conts(
            active,
            FrameCont::RelOp1(relop.clone(), e2.fast_clone()),
            e1,
        ),
        While(e1, e2) => exp_conts(
            active,
            FrameCont::While1(e1.fast_clone(), e2.fast_clone()),
            e1,
        ),
        For(p, e1, e2) => exp_conts(active, FrameCont::For1(p.fast_clone(), e2.fast_clone()), e1),
        And(e1, e2) => exp_conts(active, FrameCont::And1(e2.fast_clone()), e1),
        Or(e1, e2) => exp_conts(active, FrameCont::Or1(e2.fast_clone()), e1),
        Not(e) => exp_conts(active, FrameCont::Not, e),
        Opt(e) => exp_conts(active, FrameCont::Opt, e),
        DoOpt(e) => exp_conts(active, FrameCont::DoOpt, e),
        Bang(e) => exp_conts(active, FrameCont::Bang, e),
        Ignore(e) => exp_conts(active, FrameCont::Ignore, e),
        Debug(e) => exp_conts(active, FrameCont::Debug, e),
        Prim(p) => {
            *active.cont() = cont_value(Value::PrimFunction(
                p.clone()
                    .map_err(|s| Interruption::UnrecognizedPrim(s.to_string()))?,
            ));
            Ok(Step {})
        }
        Hole => nyi!(line!(), "Hole"),
        Force(e) => exp_conts(active, FrameCont::Force1, e),
        GetAdaptonPointer(e) => exp_conts(active, FrameCont::GetAdaptonPointer, e),
        DoAdaptonNav(nav, e) => step_adapton_nav(active, vector!(), nav.clone(), e),
        DoAdaptonPutForceThunk(_, _) => nyi!(line!(), "step case: do-@"),
        Import(path) => {
            let m = crate::vm_def::def::import(active, &path)?;
            *active.cont() = cont_value(Value::Module(m));
            Ok(Step {})
        }
        Loop(_e1, _e2) => nyi!(line!(), "step case: Loop"),
        Label(_label, _type, _e) => nyi!(line!(), "step case: Label"),
        Break(_label, _e) => nyi!(line!(), "step case: Break"),
        ActorUrl(_e) => nyi!(line!(), "step case: ActorUrl"),
        Show(_e) => nyi!(line!(), "step case: Show"),
        ToCandid(_e) => nyi!(line!(), "step case: ToCandid"),
        FromCandid(_e) => nyi!(line!(), "step case: FromCandid"),
        ObjectBlock(_obj_sort, _dec_fields_pos) => nyi!(line!(), "step case: ObjectBlock"),
        DebugShow(_e) => nyi!(line!(), "step case: DebugShow"),
        Async(_e) => nyi!(line!(), "step case: Async"),
        AsyncStar(_e) => nyi!(line!(), "step case: AsyncStar"),
        Await(_e) => nyi!(line!(), "step case: Await"),
        AwaitStar(_e) => nyi!(line!(), "step case: AwaitStar"),
        Throw(_e) => nyi!(line!(), "step case: Throw"),
        Try(_e, _case) => nyi!(line!(), "step case: Try"),
    }
}

// To advance the active Motoko state by a single step, after all limits are checked.
fn active_step_<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    active_trace(active);
    let cont = active.cont().clone();
    match cont {
        Cont::Frame(_, _) => unreachable!(
            "VM logic is broken. Old frame continuation (for debugging) should be replaced by now."
        ),
        Cont::Exp_(e, decs) => {
            if decs.is_empty() {
                exp_step(active, e)
            } else {
                let source = crate::ast::source_from_decs(&decs);
                let env = active.env().fast_clone();
                let context = active.defs().active_ctx.clone();

                active.stack().push_front(Frame {
                    context,
                    env,
                    cont: FrameCont::Decs(decs),
                    source,
                    cont_prim_type: None,
                });
                exp_step(active, e)
            }
        }
        Cont::LetVarRet(_, i) => {
            match i {
                Some(i) => {
                    *active.cont() = Cont::Value_(
                        active
                            .env()
                            .get(&i.0)
                            .ok_or(Interruption::Impossible)?
                            .fast_clone(),
                    )
                }
                None => *active.cont() = cont_value(Value::Unit),
            };
            Ok(Step {})
        }
        Cont::Value_(v) => {
            match &*v {
                Value::Pointer(p) => {
                    // Are we assigning to this pointer?
                    // If not, we are implicitly dereferencing it here.
                    match &active.stack().front() {
                        // Case: Let-binding the pointer.
                        Some(Frame {
                            cont: FrameCont::Let(_, _),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::Assign1(_),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Binary-op + Assignment to a pointer.
                        Some(Frame {
                            cont: FrameCont::BinAssign1(..),
                            ..
                        }) => return stack_cont(active, v),
                        // Case: Array-indexing with a pointer.
                        Some(Frame {
                            cont: FrameCont::Idx1(_),
                            ..
                        }) => return stack_cont(active, v),
                        _ => (),
                    };
                    // Final case: Implicit dereferencing of pointer:
                    let v = active.deref(p)?;
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                _ => stack_cont(active, v),
            }
        }
        Cont::Decs(decs) => decs_step(active, decs), //_ => unimplemented!(),
    }
}

pub fn decs_step<A: Active>(active: &mut A, mut decs: Vector<Dec_>) -> Result<Step, Interruption> {
    if decs.is_empty() {
        *active.cont() = cont_value(Value::Unit);
        *active.cont_source() = Source::Evaluation;
        Ok(Step {})
    } else {
        let dec_ = decs.pop_front().unwrap();
        match &dec_.0 {
            Dec::Type(..) => {
                *active.cont() = Cont::Decs(decs);
                Ok(Step {})
            }
            Dec::Exp(e) => {
                *active.cont_source() = dec_.1.clone();
                *active.cont() = Cont::Exp_(e.fast_clone(), decs);
                Ok(Step {})
            }
            Dec::Let(p, e) => {
                if decs.is_empty() {
                    let i = match &p.0 {
                        Pat::Var(i) => Some(i.fast_clone()),
                        _ => None,
                    };
                    let source = active.cont_source().clone();
                    exp_conts(
                        active,
                        FrameCont::Let(p.fast_clone(), Cont::LetVarRet(source, i)),
                        e,
                    )
                } else {
                    exp_conts(active, FrameCont::Let(p.fast_clone(), Cont::Decs(decs)), e)
                }
            }
            Dec::LetActor(i, _, dfs) => {
                let v = match i {
                    /* Are we upgrading a local Actor? */
                    None => todo!(),
                    Some(local_name) => {
                        let ctx_id = active.defs().active_ctx.clone();
                        let old_def = ctx_id
                            .get_field(active, local_name.0.id_ref())
                            .map(|x| x.clone());

                        let id = ActorId::Local(local_name.0.id());
                        match old_def {
                            None => crate::vm_def::def::actor(
                                active,
                                format!("<anonymous@{}>", dec_.1),
                                &id,
                                dec_.1.clone(),
                                None,
                                None,
                                dfs.dec_fields(),
                            )?,
                            Some(FieldDef {
                                def: Def::Actor(old_def),
                                ..
                            }) => crate::vm_def::def::actor_upgrade(
                                active,
                                format!("<anonymous@{}>", dec_.1),
                                &id,
                                dec_.1.clone(),
                                None,
                                None,
                                dfs.dec_fields(),
                                &old_def,
                            )?,
                            _ => unreachable!(),
                        }
                    }
                };
                match i {
                    None => (),
                    Some(i) => {
                        active.env().insert(i.0.id(), v);
                    }
                };
                *active.cont() = Cont::Decs(decs);
                Ok(Step {})
            }
            Dec::LetObject(_id, _, _dfs) => {
                nyi!(line!())
            }
            Dec::LetModule(id, _, dfs) => {
                let v = crate::vm_def::def::module(
                    active,
                    ModulePath {
                        package_name: None,
                        local_path: format!("<anonymous@{}>", dec_.1),
                    },
                    &id.clone().map(|i| i.0.id_()),
                    dec_.1.clone(),
                    None,
                    None,
                    dfs.dec_fields(),
                    None,
                )?;
                match id {
                    None => (),
                    Some(i) => {
                        active.env().insert(i.0.id(), v);
                    }
                };
                *active.cont() = Cont::Decs(decs);
                Ok(Step {})
            }
            Dec::LetImport(pattern, _, path) => {
                let_import(active, pattern, path)?;
                *active.cont() = Cont::Decs(decs);
                Ok(Step {})
            }
            Dec::Var(p, e) => {
                if let Some(x) = crate::vm_match::get_pat_var(&p.0) {
                    exp_conts(active, FrameCont::Var(x.fast_clone(), Cont::Decs(decs)), e)
                } else {
                    nyi!(line!(), "Dec::Var({:?}, _)", p)
                }
            }
            Dec::Func(f) => {
                let id = f.name.clone();
                let v = Value::Function(ClosedFunction(Closed {
                    ctx: active.defs().active_ctx.clone(),
                    env: active.env().fast_clone(),
                    content: f.clone(),
                }))
                .share();
                if let Some(i) = id {
                    active.env().insert(i.0.id(), v.fast_clone());
                };
                if decs.is_empty() {
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                } else {
                    *active.cont() = Cont::Decs(decs);
                    Ok(Step {})
                }
            }
            d => nyi!(line!(), "{:?}", d),
        }
    }
}

pub fn let_import<A: Active>(
    active: &mut A,
    pattern: &Pat_,
    path: &String,
) -> Result<(), Interruption> {
    let m = crate::vm_def::def::import(active, &path)?;
    let fields = crate::vm_def::module_project(active.defs(), &m, &pattern.0)?;
    for (x, def) in fields {
        let val = crate::vm_def::def_as_value(active.defs(), &x.0, &def)?;
        active.env().insert(x.0.clone(), val);
    }
    Ok(())
}

// TODO: possibly refactor to `Cont::Value(Value)` and `Cont::Value_(Value_)`
#[inline(always)]
pub fn cont_value(value: Value) -> Cont {
    // TODO: memoize (), true, false, null, variants, etc.
    Cont::Value_(value.share())
}

// TODO: possibly refactor to `Cont::Value(Value)` and `Cont::Value_(Value_)`
#[inline(always)]
pub fn cont_value_(value: Value_) -> Cont {
    // TODO: memoize (), true, false, null, variants, etc.
    Cont::Value_(value)
}

pub fn return_<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    let mut stack = active.stack().fast_clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::Call3 => {
                    active.defs().active_ctx = fr.context;
                    *active.env() = fr.env;
                    *active.stack() = stack;
                    *active.cont() = Cont::Value_(v);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::MisplacedReturn);
        }
    }
}

fn stack_cont_has_redex<A: ActiveBorrow>(active: &A, v: &Value) -> Result<bool, Interruption> {
    if active.stack().is_empty() {
        Ok(false)
    } else {
        use FrameCont::*;
        let frame = active.stack().front().unwrap();
        let r = match &frame.cont {
            Respond(_) => true,
            UnOp(_) => true,
            RelOp1(_, _) => false,
            RelOp2(_, _) => true,
            BinOp1(_, _) => false,
            BinOp2(_, _) => true,
            Assign1(_) => false,
            Assign2(_) => true,
            BinAssign1(..) => false,
            BinAssign2(..) => true,
            Idx1(_) => false,
            Idx2(_) => true,
            Let(_, _) => true,
            Var(_, _) => true,
            Paren => false,
            Variant(_) => false,
            Switch(_) => true,
            Block => false,
            Decs(_) => false,
            Do => true,
            Assert => true,
            Ignore => true,
            Tuple(_, _) => false,
            Array(..) => false,
            Object(..) => false,
            Annot(..) => false,
            Proj(..) => true,
            Dot(..) => true,
            Debug => false,
            If(_, _) => true,
            While1(_, _) => true,
            While2(_, _) => false,
            For1(_, _) => false,
            For2(_, _, _) => true,
            For3(_, _, _) => false,
            ForOpaqueIter(_, _, _) => true,
            And1(_) => false,
            And2 => true,
            Or1(_) => match v {
                Value::Bool(b) => *b,
                _ => true,
            },
            Or2 => true,
            Not => true,
            Opt => false,
            DoOpt => false,
            Bang => true,
            Call1(..) => false,
            Call2(..) => true,
            Call3 => false,
            Return => true,
            Unquote => true,
            DoAdaptonNav1(_vector, _, _vector11, _) => true,
            DoAdaptonNav2(_) => true,
            GetAdaptonPointer => true,
            Force1 => true,
            ForceAdaptonPointer => true,
            ForceThunk => true,
        };
        Ok(r)
    }
}

// Returns `Some(span)` if the limits include the breakpoint.
fn check_for_breakpoint<A: ActiveBorrow>(active: &A, limits: &Limits) -> Option<Breakpoint> {
    let cont_span = &active.cont_source().span();
    if let Some(span) = cont_span {
        if limits.breakpoints.contains(span) {
            Some(span.clone())
        } else {
            None
        }
    } else {
        None
    }
}

fn check_for_redex<A: ActiveBorrow>(active: &A, limits: &Limits) -> Result<usize, Interruption> {
    let mut redex_bump = 0;
    if let Cont::Value_(ref v) = active.cont() {
        if stack_cont_has_redex(active, v)? {
            redex_bump = 1;
            if let Some(redex_limit) = limits.redex {
                if active.counts().redex >= redex_limit {
                    // if =, adding 1 will exceed limit, so do not.
                    return Err(Interruption::Limit(Limit::Redex));
                }
            }
        }
    }
    Ok(redex_bump)
}

pub fn active_step<A: Active>(active: &mut A, limits: &Limits) -> Result<Step, Interruption> {
    /* to do -- check for pending send. */
    if let Some(break_span) = check_for_breakpoint(active, limits) {
        return Err(Interruption::Breakpoint(break_span));
    }
    if let Some(step_limit) = limits.step {
        if active.counts().step >= step_limit {
            return Err(Interruption::Limit(Limit::Step));
        }
    }
    let redex_bump = check_for_redex(active, limits)?;
    let ret = active_step_(active)?;
    active.counts().step += 1;
    active.counts().redex += redex_bump;
    Ok(ret)
}

fn active_trace<A: ActiveBorrow>(active: &A) {
    use log::trace;
    trace!(
        "# {:?} step {} (redex {})",
        active.schedule_choice(),
        active.counts().step,
        active.counts().redex
    );
    trace!(" - cont = {:?}", active.cont());
    trace!("   - cont_source = {:?}", active.cont_source());
    trace!("   - env = {:?}", active.env());
    trace!(" - stack = {:#?}", active.stack());
    trace!(" - store = {:#?}", active.store());
    trace!(" - defs  = {:#?}", active.defs());
}

pub fn exp_conts_<A: Active>(
    active: &mut A,
    source: Source,
    frame_cont: FrameCont,
    cont: Cont,
    cont_source: Source,
) -> Result<Step, Interruption> {
    let env = active.env().fast_clone();
    let cont_prim_type = active.cont_prim_type().clone();
    let context = active.defs().active_ctx.clone();
    active.stack().push_front(Frame {
        context,
        env,
        cont: frame_cont,
        cont_prim_type,
        source,
    });
    *active.cont() = cont;
    *active.cont_source() = cont_source;
    Ok(Step {})
}

/* continuation separates into stack frame cont and immediate cont. */
pub fn exp_conts<A: Active>(
    active: &mut A,
    frame_cont: FrameCont,
    cont: &Exp_,
) -> Result<Step, Interruption> {
    let cont_source = cont.1.clone();
    exp_conts_(
        active,
        cont_source.clone(),
        frame_cont,
        Cont::Exp_(cont.fast_clone(), Vector::new()),
        cont_source,
    )
}

/* continuation uses same stack frame. */
pub fn exp_cont<A: Active>(active: &mut A, cont: &Exp_) -> Result<Step, Interruption> {
    *active.cont_source() = cont.1.clone();
    *active.cont() = Cont::Exp_(cont.fast_clone(), Vector::new());
    Ok(Step {})
}
