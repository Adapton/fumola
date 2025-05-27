//use crate::{ast::{Mut, ProjIndex}, shared::FastClone, type_mismatch, vm_types::{stack::{FieldContext, FieldValue, Frame, FrameCont}, Active, Cont, Step}, Interruption, Value, Value_};

use crate::adapton::{self, AdaptonState};
use crate::ast::{Cases, Exp_, Inst, Literal, Mut, Pat, Pat_, ProjIndex, QuotedAst};
use crate::shared::{FastClone, Share};
use crate::value::{
    ActorMethod, ClosedFunction, CollectionFunction, FastRandIter, FastRandIterFunction,
    HashMapFunction, PrimFunction, Symbol, Value, Value_,
};
use crate::vm_types::OptionCoreSource;
use crate::vm_types::{
    def::Function as FunctionDef,
    stack::{FieldContext, FieldValue, Frame, FrameCont},
    Active, Cont, DebugPrintLine, Env, Interruption, Pointer, Response, Step,
};
use crate::{nyi, type_mismatch_, vm_step, Shared};
use im_rc::{HashMap, Vector};

use crate::vm_step::{
    cont_value, decs_step, exp_cont, exp_conts, exp_step, literal_step, object_step, return_,
    return_step, tuple_step, type_mismatch, unit_step, var_step,
};

// continue execution using the top-most stack frame, if any.
pub fn stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    if active.stack().is_empty() {
        *active.cont() = Cont::Value_(v.fast_clone());
        Err(Interruption::Done(v))
    } else if let Some(&Frame {
        cont: FrameCont::ForOpaqueIter(ref pat, ref ptr, ref body),
        ..
    }) = active.stack().front()
    {
        let pat = pat.fast_clone();
        let ptr = ptr.fast_clone();
        let body = body.fast_clone();
        let env = active.stack().front().unwrap().env.fast_clone();
        /* fast-path: avoid popping top stack frame. */
        match &*v {
            Value::Unit => (),
            _ => type_mismatch!(file!(), line!()),
        };
        match opaque_iter_next(active, &ptr)? {
            None => {
                active.stack().pop_front();
                unit_step(active)
            }
            Some(v_) => {
                if let Some(env) = crate::vm_match::pattern_matches(env, &pat.0, v_.fast_clone()) {
                    *active.env() = env;
                    exp_cont(active, &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
        }
    } else {
        // common cases: need to pop top stack frame, then pattern-match it.
        nonempty_stack_cont(active, v)
    }
}

fn nonempty_stack_cont<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
    use FrameCont::*;
    let frame = active.stack().pop_front().unwrap();
    match &frame.cont {
        Decs(_) => { /* decs in same block share an environment. */ }
        _ => {
            *active.env() = frame.env;
        }
    }
    // Save frame continuation as "active continuation" temporarily,
    // in case there is an Interruption and user inspects the active components.
    // otherwise, the "current frame" is always missing from this diagostic.
    *active.cont() = Cont::Frame(v.clone(), Box::new(frame.cont.clone()));
    active.defs().active_ctx = frame.context;
    *active.cont_prim_type() = frame.cont_prim_type;
    *active.cont_source() = frame.source;
    match frame.cont {
        ForOpaqueIter(..) => unreachable!(),
        Respond(target) => Err(Interruption::Response(Response { target, value: v })),
        UnOp(un) => {
            *active.cont() = cont_value(crate::vm_ops::unop(un, v)?);
            Ok(Step {})
        }
        RelOp1(relop, e2) => exp_conts(active, RelOp2(v, relop), &e2),
        RelOp2(v1, rel) => {
            let v = crate::vm_ops::relop(&active.cont_prim_type(), rel, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        BinOp1(binop, e2) => exp_conts(active, BinOp2(v, binop), &e2),
        BinOp2(v1, bop) => {
            let v = crate::vm_ops::binop(&active.cont_prim_type(), bop, v1, v)?;
            *active.cont() = cont_value(v);
            Ok(Step {})
        }
        Assign1(e2) => exp_conts(active, Assign2(v), &e2),
        BinAssign1(b, e2) => exp_conts(active, BinAssign2(v, b), &e2),
        Assign2(v1) => match &*v1 {
            Value::Pointer(p) => {
                active.store().mutate(p.clone(), v)?;
                unit_step(active)
            }
            Value::Index(p, i) => {
                active.store().mutate_index(p.clone(), i.fast_clone(), v)?;
                unit_step(active)
            }
            Value::AdaptonPointer(name) => {
                active.adapton().put_pointer(name.clone(), v)?;
                return_step(active, Value::AdaptonPointer(name.clone()).share())
            }
            Value::Symbol(symbol) => {
                let p = active.adapton().put_symbol(symbol.clone(), v)?;
                return_step(active, Value::AdaptonPointer(p).share())
            }
            Value::Tuple(vs) => match (vs.get(0), vs.get(1)) {
                (Some(v11), Some(v12)) => {
                    let time = v12.into_time_or(Interruption::TypeMismatch(OptionCoreSource(
                        Some(crate::vm_types::CoreSource {
                            name: Some("adapton put, delayed".to_owned()),
                            description: Some("Expected a symbol or time value in second component of assigned pair.".to_owned()),
                            file: file!().to_string(),
                            line: line!(),
                        }),
                    )))?;
                    if let Value::AdaptonPointer(ref pointer) = &**v11 {
                        active
                            .adapton()
                            .put_pointer_delay(pointer.clone(), time, v)?;
                        unit_step(active)
                    } else if let Ok(symbol) = v11.into_sym_or(()) {
                        active.adapton().put_symbol_delay(symbol, time, v)?;
                        unit_step(active)
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                }
                _ => type_mismatch!(file!(), line!()),
            },
            v1 => {
                if let Ok(symbol) = v1.into_sym_or(()) {
                    let p = active.adapton().put_symbol(symbol.clone(), v)?;
                    return_step(active, Value::AdaptonPointer(p).share())
                } else {
                    return Err(crate::Interruption::TypeMismatch(
                        crate::vm_types::OptionCoreSource(Some(crate::vm_types::CoreSource {
                            description: Some("assignment to non-assignable value.".to_string()),
                            name: Some("assignment".to_string()),
                            file: (file!()).to_string(),
                            line: (line!()),
                        })),
                    ));
                }
            }
        },
        BinAssign2(v1, bop) => {
            // to do -- generalize to work with Adapton pointers.
            let v1d = match &*v1 {
                Value::Pointer(p) => active.deref(p)?,
                x => {
                    return nyi!(
                        line!(),
                        "BinAssign2: expected Value::Pointer, but got {:?}",
                        x
                    )
                }
            };
            let v3 = crate::vm_ops::binop(&active.cont_prim_type(), bop, v1d.clone(), v.clone())?;
            match &*v1 {
                Value::Pointer(p) => {
                    active.store().mutate(p.clone(), v3.share())?;
                    unit_step(active)
                }
                _ => return nyi!(line!()),
            }
        }
        Idx1(e2) => exp_conts(active, Idx2(v), &e2),
        Idx2(v1) => {
            if let Some(Frame {
                cont: FrameCont::Assign1(_), // still need to evaluate RHS of assignment.
                ..
            }) = active.stack().get(0)
            {
                match &*v1 {
                    Value::Pointer(p) => {
                        // save array pointer and offset until after RHS is evaluated.
                        *active.cont() = cont_value(Value::Index(p.clone(), v.fast_clone()));
                        Ok(Step {})
                    }
                    Value::Dynamic(_) => Err(Interruption::Other(
                        "Dynamic Rust value without a pointer".to_string(),
                    )),
                    _ => type_mismatch!(file!(), line!()),
                }
            } else {
                let v1 = active.deref_value(v1)?;
                match (&*v1, &*v) {
                    (Value::Array(_mut, a), Value::Nat(i)) => {
                        let i = crate::value::usize_from_biguint(i)?;
                        *active.cont() =
                            cont_value((**a.get(i).ok_or(Interruption::IndexOutOfBounds)?).clone());
                        Ok(Step {})
                    }
                    (Value::Dynamic(d), _) => {
                        *active.cont() =
                            cont_value((*d.dynamic().get_index(active.store(), v)?).clone());
                        Ok(Step {})
                    }
                    _ => type_mismatch!(file!(), line!()),
                }
            }
        }
        Let(p, cont) => {
            use crate::quoted::QuotedClose;
            let p = p.quoted_close(active.env())?;
            if let Some(env) =
                crate::vm_match::pattern_matches(active.env().clone(), p.as_ref().data_ref(), v)
            {
                *active.env() = env;
                *active.cont_source() = crate::vm_types::source_from_cont(&cont);
                *active.cont() = cont;
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Var(x, cont) => {
            let ptr = active.alloc(v);
            active
                .env()
                .insert(x.as_ref().data_ref().clone(), Value::Pointer(ptr).share());
            *active.cont_source() = crate::vm_types::source_from_cont(&cont);
            *active.cont() = cont;
            Ok(Step {})
        }
        Paren => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Variant(i) => {
            *active.cont() = cont_value(Value::Variant(i.0.clone(), Some(v)));
            Ok(Step {})
        }
        Switch(cases) => switch(active, v, cases),
        Block => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Decs(decs) => {
            match decs.front() {
                None => {
                    // return final value from block.
                    *active.cont() = Cont::Value_(v);
                    Ok(Step {})
                }
                Some(_) => {
                    *active.cont() = Cont::Decs(decs);
                    Ok(Step {})
                }
            }
        }
        Do => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Assert => match &*v {
            Value::Bool(true) => unit_step(active),
            Value::Bool(false) => Err(Interruption::AssertionFailure),
            _ => type_mismatch!(file!(), line!()),
        },
        Ignore => unit_step(active),
        Tuple(mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    *active.cont() = cont_value(Value::Tuple(done));
                    Ok(Step {})
                }
                Some(next) => exp_conts(active, Tuple(done, rest), &next),
            }
        }
        Array(mut_, mut done, mut rest) => {
            done.push_back(v);
            match rest.pop_front() {
                None => {
                    if let Mut::Const = mut_ {
                        *active.cont() = cont_value(Value::Array(mut_, done));
                        Ok(Step {})
                    } else {
                        let arr = Value::Array(mut_, done);
                        let ptr = active.alloc(arr.share());
                        *active.cont() = cont_value(Value::Pointer(ptr));
                        Ok(Step {})
                    }
                }
                Some(next) => exp_conts(active, Array(mut_, done, rest), &next),
            }
        }
        Object(mut done, ctx, mut rest) => {
            done.push_back(FieldValue {
                mut_: ctx.mut_,
                id: ctx.id,
                typ: ctx.typ,
                val: v,
            });
            match rest.pop_front() {
                None => {
                    let mut hm = HashMap::new();
                    for f in done.into_iter() {
                        let id = f.id.0.clone();
                        let val = match f.mut_ {
                            Mut::Const => f.val,
                            Mut::Var => Value::Pointer(active.alloc(f.val)).share(),
                        };
                        hm.insert(id, crate::value::FieldValue { mut_: f.mut_, val });
                    }
                    *active.cont() = cont_value(Value::Object(hm));
                    Ok(Step {})
                }
                Some(next) => exp_conts(
                    active,
                    Object(
                        done,
                        FieldContext {
                            mut_: next.0.mut_.clone(),
                            id: next.0.id.0.id_(),
                            typ: next.0.typ.fast_clone(),
                        },
                        rest,
                    ),
                    &next.0.exp_(),
                ),
            }
        }
        Annot(_t) => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Proj(i) => match &*v {
            Value::Tuple(vs) => {
                if let ProjIndex::Usize(i) = i {
                    if let Some(vi) = vs.get(i) {
                        *active.cont() = Cont::Value_(vi.fast_clone());
                        Ok(Step {})
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                } else {
                    nyi!(line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Dot(f) => match &*v {
            Value::Object(fs) => {
                if let Some(f) = fs.get(&f.0) {
                    *active.cont() = Cont::Value_(f.val.fast_clone());
                    Ok(Step {})
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            Value::Module(m) => {
                let fd = crate::vm_def::resolve_def(active.defs(), &m.fields, true, &f.0)?;
                let v = crate::vm_def::def_as_value(active.defs(), &f.0, &fd.def)?;
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            }
            Value::Dynamic(d) => {
                let f = d.dynamic().get_field(active.store(), f.0.as_str())?;
                *active.cont() = Cont::Value_(f);
                Ok(Step {})
            }
            Value::Actor(a) => {
                // to do -- get defs from actor n
                // look up definition for f
                // is it a public function?
                // if not public, give error.
                // if not available, type mismatch.
                *active.cont() = Cont::Value_(
                    Value::ActorMethod(ActorMethod {
                        actor: a.id.clone(),
                        method: f.0.clone(),
                    })
                    .share(),
                );
                // do projection, representing function with special value.
                Ok(Step {})
            }
            v => Err(type_mismatch_!(
                file!(),
                line!(),
                "dot-operator-is-matching-operand",
                format!("{:?} @ {}", v, active.cont_source())
            )),
        },
        Debug => match &*v {
            Value::Unit => unit_step(active),
            _ => type_mismatch!(file!(), line!()),
        },
        If(e2, e3) => match &*v {
            Value::Bool(b) => {
                *active.cont() = if *b {
                    Cont::Exp_(e2, Vector::new())
                } else {
                    match e3 {
                        Some(e3) => Cont::Exp_(e3, Vector::new()),
                        None => cont_value(Value::Unit),
                    }
                };
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While1(e1, e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::While2(e1, e2.fast_clone()), &e2)
                } else {
                    unit_step(active)
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        While2(e1, e2) => match &*v {
            Value::Unit => exp_conts(active, FrameCont::While1(e1.fast_clone(), e2), &e1),
            _ => type_mismatch!(file!(), line!()),
        },
        For1(p, body) => {
            /* for-loop state: iterator object is value v */
            match &*v {
                Value::Opaque(ptr) => {
                    /* enter ForOpaqueIter loop (a "fast path"):
                    no need to evaluate general Motoko code for iterator. */
                    let env = active.env().fast_clone();
                    let source = active.cont_source().clone();
                    let context = active.defs().active_ctx.clone();

                    active.stack().push_front(Frame {
                        context,
                        env,
                        cont: FrameCont::ForOpaqueIter(p, ptr.clone(), body),
                        cont_prim_type: None,
                        source,
                    });
                    unit_step(active)
                }
                _ => cont_for_call_dot_next(active, p, v, body),
            }
        }
        For2(p, v_iter, body) => match &*v {
            Value::Null => unit_step(active),
            Value::Option(v_) => {
                if let Some(env) = crate::vm_match::pattern_matches(
                    active.env().fast_clone(),
                    &p.0,
                    v_.fast_clone(),
                ) {
                    *active.env() = env;
                    exp_conts(active, FrameCont::For3(p, v_iter, body.fast_clone()), &body)
                } else {
                    type_mismatch!(file!(), line!())
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        For3(p, v_iter, body) => match &*v {
            Value::Unit => cont_for_call_dot_next(active, p, v_iter, body),
            _ => type_mismatch!(file!(), line!()),
        },
        And1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    exp_conts(active, FrameCont::And2, &e2)
                } else {
                    *active.cont() = cont_value(Value::Bool(false));
                    Ok(Step {})
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        And2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or1(e2) => match &*v {
            Value::Bool(b) => {
                if *b {
                    *active.cont() = cont_value(Value::Bool(true));
                    Ok(Step {})
                } else {
                    exp_conts(active, FrameCont::Or2, &e2)
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Or2 => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(*b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Not => match &*v {
            Value::Bool(b) => {
                *active.cont() = cont_value(Value::Bool(!b));
                Ok(Step {})
            }
            _ => type_mismatch!(file!(), line!()),
        },
        Opt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        DoOpt => {
            *active.cont() = cont_value(Value::Option(v));
            Ok(Step {})
        }
        Bang => match &*v {
            Value::Option(v) => {
                *active.cont() = Cont::Value_(v.fast_clone());
                Ok(Step {})
            }
            Value::Null => bang_null(active),
            _ => type_mismatch!(file!(), line!()),
        },
        Call1(inst, e2) => exp_conts(active, FrameCont::Call2(v, inst), &e2),
        Call2(f, inst) => call_cont(active, f, inst, v),
        Call3 => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        Unquote => match &*v {
            Value::QuotedAst(cq) => match cq {
                QuotedAst::Literal(l) => literal_step(active, &l.0),
                QuotedAst::Empty => unit_step(active),
                QuotedAst::Id_(i) => var_step(active, &i.0),
                QuotedAst::Id(id) => var_step(active, id),
                QuotedAst::TupleExps(es) => match es.vec.len() {
                    0 => unit_step(active),
                    1 => exp_conts(active, FrameCont::Paren, &es.vec[0]),
                    _ => tuple_step(active, &es.vec),
                },
                QuotedAst::RecordExps((bases, fields)) => object_step(active, bases, fields),
                QuotedAst::Cases(_) => type_mismatch!(file!(), line!()),
                QuotedAst::TuplePats(_) => type_mismatch!(file!(), line!()),
                QuotedAst::RecordPats(_) => type_mismatch!(file!(), line!()),
                QuotedAst::DecFields(_dfs) => type_mismatch!(file!(), line!()),
                QuotedAst::Decs(d) => decs_step(active, d.vec.clone()),
                QuotedAst::Types(_ts) => type_mismatch!(file!(), line!()),
                QuotedAst::Attrs(_a) => type_mismatch!(file!(), line!()),
            },
            _ => type_mismatch!(file!(), line!()),
        },
        Return => return_(active, v),
        DoAdaptonNav1(mut nav_done, nav_here, nav_next, ref body) => {
            nav_done.push_back((nav_here.clone(), v));
            let end_len = nav_done.len();
            match nav_next.front() {
                Some(_) => vm_step::step_adapton_nav(active, nav_done, nav_next, body),
                None => {
                    for (nav, symbol) in nav_done {
                        let symbol = symbol
                            .into_sym_or(Interruption::TypeMismatch(OptionCoreSource(None)))?;
                        active.adapton().navigate_begin(nav, symbol)?;
                    }
                    exp_conts(active, FrameCont::DoAdaptonNav2(end_len), body)
                }
            }
        }
        DoAdaptonNav2(end_count) => {
            for _i in 0..end_count {
                active.adapton().navigate_end()?;
            }
            *active.cont() = cont_value(v.get());
            Ok(Step {})
        }
        GetAdaptonPointer => {
            if let Value::AdaptonPointer(p) = v.get() {
                let v = active.adapton().get_pointer(p)?;
                *active.cont() = Cont::Value_(v);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        Force1 => {
            if let Value::AdaptonPointer(ref p) = *v {
                let thunk_body = active.adapton().force_begin(p.clone())?;
                let env = active.env().fast_clone();
                let context = active.defs().active_ctx.clone();
                active.stack().push_front(Frame {
                    context,
                    env,
                    cont: FrameCont::ForceAdaptonPointer,
                    cont_prim_type: None,
                    source: crate::vm_types::Source::Evaluation,
                });
                *active.env() = thunk_body.env;
                *active.ctx_id() = thunk_body.ctx;
                exp_step(active, thunk_body.content)
            } else if let Value::Thunk(ref thunk_body) = *v {
                let env = active.env().fast_clone();
                let context = active.defs().active_ctx.clone();
                active.stack().push_front(Frame {
                    context,
                    env,
                    cont: FrameCont::ForceThunk,
                    cont_prim_type: None,
                    source: crate::vm_types::Source::Evaluation,
                });
                *active.env() = thunk_body.env.fast_clone();
                *active.ctx_id() = thunk_body.ctx.clone();
                exp_step(active, thunk_body.content.fast_clone())
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        ForceThunk => {
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
        ForceAdaptonPointer => {
            active.adapton().force_end(v.clone())?;
            *active.cont() = Cont::Value_(v);
            Ok(Step {})
        }
    }
}

mod collection {
    pub mod fastranditer {
        use super::super::*;
        use crate::{shared::Share, value::Collection};

        pub fn new<A: Active>(
            active: &mut A,
            _targs: Option<Inst>,
            v: Value_,
        ) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let size: Option<u32> = crate::vm_match::assert_value_is_option_u32(&args[0])?;
                let seed: u32 = crate::vm_match::assert_value_is_u32(&args[1])?;
                let ptr = active.alloc(
                    Value::Collection(Collection::FastRandIter(FastRandIter::new(size, seed)))
                        .share(),
                );
                *active.cont() = cont_value(Value::Opaque(ptr));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }

        pub fn next<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            let ptr = crate::vm_match::assert_value_is_opaque_pointer(&v)?;
            match &*active.deref(&ptr)? {
                Value::Collection(Collection::FastRandIter(fri)) => {
                    let mut fri = fri.clone();
                    let n = match fri.next() {
                        Some(n) => Value::Option(n.share()),
                        None => Value::Null,
                    };
                    let i = Value::Collection(Collection::FastRandIter(fri));
                    active.store().mutate(ptr, i.share())?;
                    *active.cont() = cont_value(n);
                    Ok(Step {})
                }
                _ => type_mismatch!(file!(), line!()),
            }
        }
    }

    pub mod hashmap {
        use super::super::*;
        use crate::value::Collection;
        use im_rc::vector;

        pub fn new<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(_) = crate::vm_match::pattern_matches_temps(&Pat::Literal(Literal::Unit), v)
            {
                *active.cont() = cont_value(Value::Collection(Collection::HashMap(HashMap::new())));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn put<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(3), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let v = &args[2];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.insert(k.fast_clone(), v.fast_clone()) {
                            None => (hm, Value::Null),
                            Some(old) => (hm, Value::Option(old)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                // Note for later: immutable map updates are adding extra overhead here
                // We could probably just tolerate this and use `Dynamic` values for performance-critical situations
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn get<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let ret = {
                    if let Value::Collection(Collection::HashMap(hm)) = hm.get() {
                        match hm.get(k) {
                            None => Value::Null,
                            Some(v) => Value::Option(v.fast_clone()),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        pub fn remove<A: Active>(active: &mut A, v: Value_) -> Result<Step, Interruption> {
            if let Some(args) =
                crate::vm_match::pattern_matches_temps(&crate::vm_match::pattern::temps(2), v)
            {
                let hm = &args[0];
                let k = &args[1];
                let (hm, old) = {
                    if let Value::Collection(Collection::HashMap(mut hm)) = hm.get() {
                        match hm.remove(k) {
                            None => (hm, Value::Null),
                            Some(v) => (hm, Value::Option(v)),
                        }
                    } else {
                        type_mismatch!(file!(), line!());
                    }
                };
                let hm = Value::Collection(Collection::HashMap(hm));
                let ret = Value::Tuple(vector![hm.share(), old.share()]);
                *active.cont() = cont_value(ret);
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
    }
}

pub fn opaque_iter_next<A: Active>(
    active: &mut A,
    p: &Pointer,
) -> Result<Option<Value_>, Interruption> {
    use crate::value::Collection;
    let iter_value = active.deref(p)?;
    // dispatch based on iterator value (as opposed to primitive function being given in source).
    // one case for each inbuilt iterator value.
    // to do -- integrate "dynamic" iterators too
    match &*iter_value {
        Value::Collection(Collection::FastRandIter(fri)) => {
            let mut fri = fri.clone();
            let n = fri.next();
            active.store().mutate(
                p.clone(),
                Value::Collection(Collection::FastRandIter(fri)).share(),
            )?;
            Ok(n.map(|v| v.share()))
        }
        _ => type_mismatch!(file!(), line!()),
    }
}

fn cont_for_call_dot_next<A: Active>(
    active: &mut A,
    p: Pat_,
    v: Value_,
    body: Exp_,
) -> Result<Step, Interruption> {
    let deref_v = active.deref_value(v.fast_clone())?; // Only used for `Dynamic` case
    match &*deref_v {
        Value::Dynamic(d) => {
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            *active.cont() = Cont::Value_(d.dynamic_mut().iter_next(active.store())?);
            Ok(Step {})
        }
        _ => {
            let v_next_func = v.get_field_or("next", type_mismatch_!(file!(), line!()))?;
            let env = active.env().fast_clone();
            let source = active.cont_source().clone();
            let context = active.defs().active_ctx.clone();
            active.stack().push_front(Frame {
                context,
                env,
                cont: FrameCont::For2(p, v, body),
                cont_prim_type: None,
                source,
            });
            call_cont(active, v_next_func, None, Value::Unit.share())
        }
    }
}

fn call_prim_function<A: Active>(
    active: &mut A,
    pf: &PrimFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        AtSignVar(v) => nyi!(line!(), "call_prim_function({})", v),
        DebugPrint => match &*args {
            Value::Text(s) => {
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}, {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    s
                );

                active.debug_print_out().push_back(DebugPrintLine {
                    text: s.clone(),
                    schedule_choice,
                });
                unit_step(active)
            }
            v => {
                //let v = v.to_motoko()?;
                let txt = crate::format::format_pretty(v, 80);
                let schedule_choice = active.schedule_choice().clone();
                log::info!(
                    "DebugPrint: {:?}: {}: {:?}",
                    schedule_choice,
                    active.cont_source(),
                    txt
                );
                let schedule_choice = active.schedule_choice().clone();
                active.debug_print_out().push_back(DebugPrintLine {
                    text: crate::value::Text::from(txt),
                    schedule_choice,
                });
                unit_step(active)
            }
        },
        NatToText => match &*args {
            Value::Nat(n) => {
                *active.cont() = cont_value(Value::Text(format!("{}", n).into()));
                Ok(Step {})
            }
            v => {
                *active.cont() = cont_value(Value::Text(format!("{:?}", v).into()));
                Ok(Step {})
            }
        },
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "value-reflection")]
        ReifyValue => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(args.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "value-reflection")]
        ReflectValue => {
            *active.cont() = cont_value(args.to_rust::<Value>().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "to-motoko")]
        #[cfg(feature = "core-reflection")]
        ReifyActive => {
            use crate::value::ToMotoko;
            *active.cont() = cont_value(active.to_motoko().map_err(Interruption::ValueError)?);
            Ok(Step {})
        }
        #[cfg(feature = "core-reflection")]
        ReflectActive => {
            *active = args.to_rust::<Active>().map_err(Interruption::ValueError)?;
            Ok(Step {})
        }
        Collection(cf) => call_collection_function(active, cf, targs, args),
        AdaptonNow => {
            *active.cont() = cont_value(Value::AdaptonTime(active.adapton().now()));
            Ok(Step {})
        }
        AdaptonHere => {
            *active.cont() = cont_value(Value::AdaptonSpace(active.adapton().here()));
            Ok(Step {})
        }
        AdaptonSpace => {
            if let Ok(symbol) = args.into_sym_or(()) {
                *active.cont() = cont_value(Value::AdaptonSpace(adapton::Space::Symbol(symbol)));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        AdaptonTime => {
            if let Ok(symbol) = args.into_sym_or(()) {
                *active.cont() = cont_value(Value::AdaptonTime(adapton::Time::Symbol(symbol)));
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
    }
}

fn call_collection_function<A: Active>(
    active: &mut A,
    cf: &CollectionFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use CollectionFunction::*;
    match cf {
        HashMap(hmf) => call_hashmap_function(active, hmf, targs, args),
        FastRandIter(frif) => call_fastranditer_function(active, frif, targs, args),
    }
}

fn call_fastranditer_function<A: Active>(
    active: &mut A,
    frif: &FastRandIterFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use FastRandIterFunction::*;
    match frif {
        New => collection::fastranditer::new(active, targs, args),
        Next => collection::fastranditer::next(active, args),
    }
}

fn call_hashmap_function<A: Active>(
    active: &mut A,
    hmf: &HashMapFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use HashMapFunction::*;
    match hmf {
        New => collection::hashmap::new(active, args),
        Put => collection::hashmap::put(active, args),
        Get => collection::hashmap::get(active, args),
        Remove => collection::hashmap::remove(active, args),
    }
}

pub fn call_function_def<A: Active>(
    active: &mut A,
    actor_env: Env,
    fndef: &FunctionDef,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) = crate::vm_match::pattern_matches(actor_env, &fndef.function.input.0, args) {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        fndef
            .function
            .name
            .fast_clone()
            .map(|f| active.env().insert(f.0.id(), fndef.rec_value.fast_clone()));
        active.defs().active_ctx = fndef.context.clone();
        *active.cont() = Cont::Exp_(fndef.function.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_function<A: Active>(
    active: &mut A,
    value: Value_,
    cf: &ClosedFunction,
    _targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    if let Some(env_) =
        crate::vm_match::pattern_matches(cf.0.env.fast_clone(), &cf.0.content.input.0, args)
    {
        let source = active.cont_source().clone();
        let env_saved = active.env().fast_clone();
        *active.env() = env_;
        cf.0.content
            .name
            .fast_clone()
            .map(|f| active.env().insert(f.0.id(), value));
        *active.cont() = Cont::Exp_(cf.0.content.exp.fast_clone(), Vector::new());
        let context = active.defs().active_ctx.clone();
        active.defs().active_ctx = cf.0.ctx.clone();
        active.stack().push_front(Frame {
            context,
            source,
            env: env_saved,
            cont: FrameCont::Call3,
            cont_prim_type: None, /* to do */
        }); // to match with Return, if any.
        Ok(Step {})
    } else {
        type_mismatch!(file!(), line!())
    }
}

fn call_cont<A: Active>(
    active: &mut A,
    func_value: Value_,
    inst: Option<Inst>,
    args_value: Value_,
) -> Result<Step, Interruption> {
    match &*func_value {
        Value::Function(cf) => call_function(active, func_value.fast_clone(), cf, inst, args_value),
        Value::PrimFunction(pf) => call_prim_function(active, pf, inst, args_value),
        _ => {
            let func_value = active.deref_value(func_value)?; // Account for dynamic value pointers
            match &*func_value {
                Value::Dynamic(d) => {
                    let result =
                        d.dynamic_mut()
                            .call(active.store(), &inst, args_value.fast_clone())?;
                    *active.cont() = Cont::Value_(result);
                    Ok(Step {})
                }
                Value::ActorMethod(am) => Err(Interruption::Send(am.clone(), inst, args_value)),
                v => {
                    if let (Ok(s1), Ok(s2)) = (v.into_sym_or(()), args_value.into_sym_or(())) {
                        *active.cont() =
                            Cont::Value_(Value::Symbol(Shared::new(Symbol::Call(s1, s2))).share());
                        Ok(Step {})
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                }
            }
        }
    }
}

fn switch<A: Active>(active: &mut A, v: Value_, cases: Cases) -> Result<Step, Interruption> {
    for case in cases.vec.into_iter() {
        if let Some(env) = crate::vm_match::pattern_matches(
            active.env().fast_clone(),
            &case.0.pat.0,
            v.fast_clone(),
        ) {
            *active.env() = env;
            *active.cont_source() = case.0.exp.1.clone();
            *active.cont() = Cont::Exp_(case.0.exp.fast_clone(), Vector::new());
            return Ok(Step {});
        }
    }
    Err(Interruption::NoMatchingCase)
}

fn bang_null<A: Active>(active: &mut A) -> Result<Step, Interruption> {
    let mut stack = active.stack().clone();
    loop {
        if let Some(fr) = stack.pop_front() {
            match fr.cont {
                FrameCont::DoOpt => {
                    *active.stack() = stack;
                    *active.cont() = cont_value(Value::Null);
                    return Ok(Step {});
                }
                _ => {}
            }
        } else {
            return Err(Interruption::NoDoQuestBangNull);
        }
    }
}
