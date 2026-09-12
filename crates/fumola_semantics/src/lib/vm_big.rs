//! Evaluating a region on the Rust stack instead of on the heap.
//!
//! The machine in `vm_step` and `vm_stack_cont` materializes every
//! intermediate state: a `Frame` carrying an environment, a `FrameCont`, and a
//! `Cont` rebuilt in place, once per redex. That is what makes the state
//! inspectable at any `Interruption`, and it is not free.
//!
//! `do big { .. }` asks for the same meaning with the intermediate states on
//! the Rust call stack, where they cost a stack frame and no allocation.
//!
//! # Why this file is small
//!
//! It handles a closed set of forms -- binders, blocks, operators, the two
//! loops, assignment -- and hands every other node back to the machine, in
//! place, by [`delegate`]. That is not a fallback and nothing is retried: the
//! machine steps the very same `Core` this evaluator is holding, against a
//! watermark taken when the region was entered, and returns the value it
//! leaves in `cont`.
//!
//! So there is never anything to undo. Every adapton bracket -- force, the
//! navigations, repair, scratch -- keeps exactly one implementation, the one in
//! `vm_stack_cont`, and a graph built inside a region is the graph the machine
//! would have built. That claim is structural rather than audited case by case,
//! which is the only reason it is worth making.
//!
//! # Counting
//!
//! The machine charges one step per `Cont` transition and one redex per
//! transition that retires one (`stack_cont_has_redex`). This evaluator makes
//! the same transitions without writing them down, so it charges them by hand:
//! every arm below carries the two numbers and cites the frame arm it mirrors.
//! The counts are what the differential test compares, and for the recursive
//! set they are the only oracle there is -- none of these forms touches the
//! graph, so graph equality says nothing about them.

use crate::value::{Closed, ClosedFunction, Value, Value_};
use crate::vm_types::{Active, Cont, Interruption, Step, def::CtxId};
use fumola_syntax::ast::{Dec, Dec_, Exp, Exp_, Pat, PrimType, Source};
use fumola_syntax::shared::{FastClone, Share};
use im_rc::Vector;

use crate::{impossible_, type_mismatch_};

/// Why a recursive evaluation stopped short of a value.
pub enum Ctl {
    /// Exactly the interruption the machine would have raised.
    Interrupt(Interruption),
    /// A non-local exit has already left the region.
    ///
    /// `return_` and `bang_null` find their target by scanning the real stack
    /// and writing stack, env, context and cont directly. When the target is
    /// below the watermark, the region is over and the Rust frames standing
    /// above here are precisely the continuation that exit discards. Unwinding
    /// them is the answer, not an error to report.
    Escaped,
}

impl From<Interruption> for Ctl {
    fn from(i: Interruption) -> Ctl {
        Ctl::Interrupt(i)
    }
}

/// What a frame pop restores.
///
/// A frame saves these four on the way in and writes them back on the way out.
/// This evaluator pushes no frame, so it carries them in a local and restores
/// them at the same points -- before evaluating each subsequent subexpression,
/// which is when the pop would have happened.
struct Ambient {
    env: crate::vm_types::Env,
    context: CtxId,
    cont_prim_type: Option<PrimType>,
    source: Source,
}

/// Past this many nested forms, hand the node to the machine.
///
/// The machine's stack is on the heap and grows until memory runs out. This
/// one is bounded by the native stack -- about a megabyte under wasm -- and a
/// stack overflow is not catchable: it aborts natively and traps in wasm. So
/// depth is spent rather than risked, and running out of it is the same
/// demotion an unsupported form gets, not a failure.
///
/// Unreachable as the set stands, since calls are delegated and `while`
/// iterates rather than recurses. It is here for the sets that come later.
const MAX_DEPTH: usize = 256;

/// Evaluate the body of a region, and answer with its value un-dereferenced.
///
/// Raw because the machine is about to pop the region's own `Do` frame, and the
/// implicit-deref rule is decided against that frame, not against this call.
pub fn eval_region<A: Active>(active: &mut A, body: &Exp_, watermark: usize) -> Result<Value_, Ctl> {
    eval_exp_no_deref(active, body, 0, watermark)
}

fn ambient<A: Active>(active: &mut A) -> Ambient {
    Ambient {
        env: active.env().fast_clone(),
        context: active.defs().active_ctx.clone(),
        cont_prim_type: active.cont_prim_type().clone(),
        source: active.cont_source().clone(),
    }
}

fn restore<A: Active>(active: &mut A, amb: &Ambient) {
    *active.env() = amb.env.fast_clone();
    active.defs().active_ctx = amb.context.clone();
    *active.cont_prim_type() = amb.cont_prim_type.clone();
    *active.cont_source() = amb.source.clone();
}

fn bump<A: Active>(active: &mut A, redex: usize) {
    active.counts().step += 1;
    active.counts().redex += redex;
}

/// Evaluate a subexpression in a position where a pointer is dereferenced.
///
/// Restores what the frame pop would have restored, writes the subexpression's
/// own span the way `exp_conts` does, evaluates, and then applies the implicit
/// deref. `redex` is the redex value of the frame that would be standing over
/// this value, because the deref is its own transition and is charged against
/// that frame.
fn sub<A: Active>(
    active: &mut A,
    amb: &Ambient,
    e: &Exp_,
    depth: usize,
    watermark: usize,
    redex: usize,
) -> Result<Value_, Ctl> {
    restore(active, amb);
    *active.cont_source() = e.1.clone();
    let v = eval_exp_no_deref(active, e, depth, watermark)?;
    deref_value(active, v, redex)
}

/// As [`sub`], for the positions where the machine keeps the pointer: the
/// `Let`, `Assign1`, `BinAssign1` and `Idx1` frames. Two are reachable from the
/// set below -- a `let`'s right-hand side and an assignment's left -- and the
/// other two belong to forms that are delegated whole.
fn sub_no_deref<A: Active>(
    active: &mut A,
    amb: &Ambient,
    e: &Exp_,
    depth: usize,
    watermark: usize,
) -> Result<Value_, Ctl> {
    restore(active, amb);
    *active.cont_source() = e.1.clone();
    eval_exp_no_deref(active, e, depth, watermark)
}

/// As [`sub`], minus the span write.
///
/// One caller: the branch an `if` chose. That arm sets `Cont::Exp_` itself
/// rather than calling `exp_conts`, so it leaves the span alone.
fn sub_tail<A: Active>(
    active: &mut A,
    amb: &Ambient,
    e: &Exp_,
    depth: usize,
    watermark: usize,
    redex: usize,
) -> Result<Value_, Ctl> {
    restore(active, amb);
    let v = eval_exp_no_deref(active, e, depth, watermark)?;
    deref_value(active, v, redex)
}

fn deref_value<A: Active>(active: &mut A, v: Value_, redex: usize) -> Result<Value_, Ctl> {
    if let Value::Pointer(p) = &*v {
        let p = p.clone();
        let d = active.deref(&p).map_err(Ctl::Interrupt)?;
        bump(active, redex);
        return Ok(d);
    }
    Ok(v)
}

/// Take back the value from a machine reduction this evaluator reuses, and
/// refuse rather than guess if it did anything else.
///
/// These helpers answer by writing `cont`. If one of them grows a stack frame
/// later, or answers with something other than a value, the reading below would
/// be wrong -- so it becomes a loud `impossible!` here instead.
fn value_after<A: Active>(
    active: &mut A,
    depth0: usize,
    r: Result<Step, Interruption>,
) -> Result<Value_, Ctl> {
    r.map_err(Ctl::Interrupt)?;
    if active.stack().len() != depth0 {
        return Err(Ctl::Interrupt(impossible_!(
            line!(),
            "a reduction the big-step evaluator reuses pushed a stack frame"
        )));
    }
    match active.cont() {
        Cont::Value_(v) => Ok(v.fast_clone()),
        _ => Err(Ctl::Interrupt(impossible_!(
            line!(),
            "a reduction the big-step evaluator reuses did not answer with a value"
        ))),
    }
}

/// Hand one node back to the machine, in place.
///
/// The machine steps the same `Core`. It is finished with the node when the
/// stack is back to the watermark and `cont` holds a value; it has left the
/// region entirely when the stack is below it. Both tests are made *before*
/// each step, which is what makes `len < watermark` an exact reading of
/// "crossed the boundary" and what keeps the value raw -- one more step would
/// have dereferenced it against the wrong frame.
fn delegate<A: Active>(active: &mut A, e: &Exp_, watermark: usize) -> Result<Value_, Ctl> {
    crate::vm_step::exp_cont(active, e).map_err(Ctl::Interrupt)?;
    loop {
        let len = active.stack().len();
        if len < watermark {
            return Err(Ctl::Escaped);
        }
        if len == watermark {
            if let Cont::Value_(v) = active.cont() {
                return Ok(v.fast_clone());
            }
        }
        step_once(active)?;
    }
}

/// One machine step, with the counts `active_step` charges and without the
/// limit checks it makes.
///
/// A region is atomic, and `enter_eval_mode` declined to enter one at all when
/// a limit or a breakpoint could have asked it to pause. So there is nothing
/// left to check here, and the counts still have to be right.
fn step_once<A: Active>(active: &mut A) -> Result<Step, Ctl> {
    let redex = crate::vm_step::redex_bump(active).map_err(Ctl::Interrupt)?;
    let ret = match crate::vm_step::active_step_(active) {
        // A region nested inside a delegated node. The limit decision was
        // already made at the outer boundary and cannot come out differently.
        Err(Interruption::EnterEvalMode(mode, e)) => {
            crate::vm_step::enter_eval_mode_unlimited(active, mode, &e).map_err(Ctl::Interrupt)?
        }
        other => other.map_err(Ctl::Interrupt)?,
    };
    active.counts().step += 1;
    active.counts().redex += redex;
    Ok(ret)
}

fn eval_exp_no_deref<A: Active>(
    active: &mut A,
    e: &Exp_,
    depth: usize,
    watermark: usize,
) -> Result<Value_, Ctl> {
    if depth >= MAX_DEPTH {
        return delegate(active, e, watermark);
    }
    let depth = depth + 1;
    let w = watermark;
    use Exp::*;
    match &e.0 {
        // 1 step, 0 redex -- `literal_step`.
        Literal(l) => {
            let d0 = active.stack().len();
            let r = crate::vm_step::literal_step(active, l);
            let v = value_after(active, d0, r)?;
            bump(active, 0);
            Ok(v)
        }

        // 1 step, 0 redex -- `var_step`, called rather than copied, so the
        // environment miss path (the `@`-prim fallback, then the definition
        // context) is not reimplemented here.
        Var(x) => {
            let d0 = active.stack().len();
            let r = crate::vm_step::var_step(active, &x.0.id());
            let v = value_after(active, d0, r)?;
            bump(active, 0);
            Ok(v)
        }

        // Paren => false.
        Paren(e1) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 0)?;
            restore(active, &amb);
            bump(active, 0);
            Ok(v)
        }

        // Not => true.
        Not(e1) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 1)?;
            restore(active, &amb);
            match &*v {
                Value::Bool(b) => {
                    let b = !b;
                    bump(active, 1);
                    Ok(Value::Bool(b).share())
                }
                _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
            }
        }

        // And1 => false, And2 => true.
        And(e1, e2) => {
            let amb = ambient(active);
            bump(active, 0);
            let v1 = sub(active, &amb, e1, depth, w, 0)?;
            restore(active, &amb);
            match &*v1 {
                Value::Bool(false) => {
                    bump(active, 0);
                    Ok(Value::Bool(false).share())
                }
                Value::Bool(true) => {
                    bump(active, 0);
                    let v2 = sub(active, &amb, e2, depth, w, 1)?;
                    restore(active, &amb);
                    match &*v2 {
                        Value::Bool(b) => {
                            let b = *b;
                            bump(active, 1);
                            Ok(Value::Bool(b).share())
                        }
                        _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
                    }
                }
                _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
            }
        }

        // Or1 is the one value-dependent entry in the redex table: it retires a
        // redex when the left operand short-circuits, and also when it is not a
        // boolean at all, which is the step that then fails.
        Or(e1, e2) => {
            let amb = ambient(active);
            bump(active, 0);
            let v1 = sub(active, &amb, e1, depth, w, 0)?;
            restore(active, &amb);
            match &*v1 {
                // The one value-dependent entry in the redex table: a left
                // operand that short-circuits retires a redex, and one that does
                // not simply hands over to the right operand.
                Value::Bool(true) => {
                    bump(active, 1);
                    Ok(Value::Bool(true).share())
                }
                Value::Bool(false) => {
                    bump(active, 0);
                    let v2 = sub(active, &amb, e2, depth, w, 1)?;
                    restore(active, &amb);
                    match &*v2 {
                        Value::Bool(b) => {
                            let b = *b;
                            bump(active, 1);
                            Ok(Value::Bool(b).share())
                        }
                        _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
                    }
                }
                _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
            }
        }

        // UnOp => true.
        Un(u, e1) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 1)?;
            restore(active, &amb);
            let v = crate::vm_ops::unop(u.clone(), v).map_err(Ctl::Interrupt)?;
            bump(active, 1);
            Ok(v.share())
        }

        // BinOp1 => false, BinOp2 => true. The second operand is evaluated with
        // the ambient restored, which is what makes `cont_prim_type` -- the
        // annotation that decides whether `+%` wraps -- exact.
        Bin(e1, b, e2) => {
            let amb = ambient(active);
            bump(active, 0);
            let v1 = sub(active, &amb, e1, depth, w, 0)?;
            restore(active, &amb);
            bump(active, 0);
            let v2 = sub(active, &amb, e2, depth, w, 1)?;
            restore(active, &amb);
            let cpt = active.cont_prim_type().clone();
            let v = crate::vm_ops::binop(&cpt, b.clone(), v1, v2).map_err(Ctl::Interrupt)?;
            bump(active, 1);
            Ok(v.share())
        }

        // RelOp1 => false, RelOp2 => true.
        Rel(e1, r, e2) => {
            let amb = ambient(active);
            bump(active, 0);
            let v1 = sub(active, &amb, e1, depth, w, 0)?;
            restore(active, &amb);
            bump(active, 0);
            let v2 = sub(active, &amb, e2, depth, w, 1)?;
            restore(active, &amb);
            let cpt = active.cont_prim_type().clone();
            let v = crate::vm_ops::relop(&cpt, r.clone(), v1, v2).map_err(Ctl::Interrupt)?;
            bump(active, 1);
            Ok(v.share())
        }

        // Assert => true.
        Assert(e1) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 1)?;
            restore(active, &amb);
            match &*v {
                Value::Bool(true) => {
                    bump(active, 1);
                    Ok(Value::Unit.share())
                }
                Value::Bool(false) => Err(Ctl::Interrupt(Interruption::AssertionFailure)),
                _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
            }
        }

        // Tuple => false. The empty tuple is unit, and takes the one step
        // `tuple_step` takes to say so.
        Tuple(es) => {
            let es = &es.vec;
            if es.is_empty() {
                bump(active, 0);
                return Ok(Value::Unit.share());
            }
            let amb = ambient(active);
            bump(active, 0);
            let mut done: Vector<Value_> = Vector::new();
            for e1 in es.iter() {
                let v = sub(active, &amb, e1, depth, w, 0)?;
                restore(active, &amb);
                bump(active, 0);
                done.push_back(v);
            }
            Ok(Value::Tuple(done).share())
        }

        // If => true. The chosen branch is the one place the machine sets
        // `Cont::Exp_` without going through `exp_conts`, so it is also the one
        // place here that does not write the span.
        If(e1, e2, e3) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 1)?;
            restore(active, &amb);
            match &*v {
                Value::Bool(true) => {
                    bump(active, 1);
                    sub_tail(active, &amb, e2, depth, w, 0)
                }
                Value::Bool(false) => {
                    bump(active, 1);
                    match e3 {
                        Some(e3) => sub_tail(active, &amb, e3, depth, w, 0),
                        None => Ok(Value::Unit.share()),
                    }
                }
                _ => Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
            }
        }

        // While1 => true, While2 => false. A Rust `loop`, so the iteration
        // count never reaches the native stack.
        While(e1, e2) => {
            let amb = ambient(active);
            bump(active, 0);
            loop {
                let c = sub(active, &amb, e1, depth, w, 1)?;
                restore(active, &amb);
                match &*c {
                    Value::Bool(false) => {
                        bump(active, 1);
                        return Ok(Value::Unit.share());
                    }
                    Value::Bool(true) => bump(active, 1),
                    _ => return Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
                }
                let b = sub(active, &amb, e2, depth, w, 0)?;
                restore(active, &amb);
                match &*b {
                    Value::Unit => bump(active, 0),
                    _ => return Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
                }
            }
        }

        // Assign1 => false, Assign2 => true. The reduction is the machine's own
        // `assign_to`, so the adapton puts are shared code rather than a copy.
        // Assign1 => false, Assign2 => true. The reduction is the machine's own
        // `assign_to`, so the adapton puts are shared code rather than a copy.
        //
        // Only a plain variable target, because an indexed one is not a local
        // decision: the `Idx2` frame arm looks *underneath itself* for an
        // `Assign1` frame, and answers with an array pointer and an offset when
        // it finds one rather than with the element. There is no such frame
        // here, so `a[i] := v` would read the element and assign to that. The
        // whole node goes back to the machine instead.
        Assign(e1, e2) if matches!(&e1.0, Exp::Var(..)) => {
            let amb = ambient(active);
            bump(active, 0);
            let v1 = sub_no_deref(active, &amb, e1, depth, w)?;
            restore(active, &amb);
            bump(active, 0);
            let v2 = sub(active, &amb, e2, depth, w, 1)?;
            restore(active, &amb);
            let d0 = active.stack().len();
            let r = crate::vm_stack_cont::assign_to(active, v1, v2);
            let v = value_after(active, d0, r)?;
            bump(active, 1);
            Ok(v)
        }

        // Block => false, and the block's value is dereferenced before that pop
        // because `Block` is not one of the four keep-the-pointer frames.
        Block(decs) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = eval_decs(active, &decs.vec, depth, w)?;
            let v = deref_value(active, v, 0)?;
            restore(active, &amb);
            bump(active, 0);
            Ok(v)
        }

        // Do => true.
        Do(e1) => {
            let amb = ambient(active);
            bump(active, 0);
            let v = sub(active, &amb, e1, depth, w, 1)?;
            restore(active, &amb);
            bump(active, 1);
            Ok(v)
        }

        _ => delegate(active, e, watermark),
    }
}

/// Evaluate a declaration list, mirroring `decs_step` and the `Decs` frame.
///
/// The ambient here is not the ordinary one. The machine builds a `Decs` frame
/// with `cont_prim_type` hardcoded to `None`, and its pop deliberately does not
/// restore the environment -- declarations in one block share an environment,
/// which is the whole point of a block. So this loop threads the environment
/// forward and restores only the context and the primitive type.
fn eval_decs<A: Active>(
    active: &mut A,
    decs: &Vector<Dec_>,
    depth: usize,
    watermark: usize,
) -> Result<Value_, Ctl> {
    let w = watermark;
    let mut rest: Vector<Dec_> = decs.fast_clone();
    loop {
        if rest.is_empty() {
            bump(active, 0);
            return Ok(Value::Unit.share());
        }
        let dec_ = match rest.pop_front() {
            Some(d) => d,
            None => {
                return Err(Ctl::Interrupt(impossible_!(
                    line!(),
                    "a declaration list just checked non-empty is empty"
                )));
            }
        };
        let last = rest.is_empty();
        // What the `Decs` frame saves and gives back. The environment is not
        // part of it, on purpose.
        let context = active.defs().active_ctx.clone();
        let dec_amb = Ambient {
            env: active.env().fast_clone(),
            context: context.clone(),
            cont_prim_type: None,
            source: dec_.1.clone(),
        };
        match &dec_.0 {
            Dec::Type(..) => {
                bump(active, 0);
            }

            Dec::Exp(e) => {
                bump(active, 0);
                *active.cont_source() = dec_.1.clone();
                let v = eval_exp_no_deref(active, e, depth, w)?;
                if last {
                    return Ok(v);
                }
                // Decs => false, and the pop does not restore the environment.
                let v = deref_value(active, v, 0)?;
                let _ = v;
                active.defs().active_ctx = context;
                *active.cont_prim_type() = None;
                bump(active, 0);
            }

            // Let => true. The right-hand side keeps a pointer.
            Dec::Let(p, e) => {
                bump(active, 0);
                let v = sub_no_deref(active, &dec_amb, e, depth, w)?;
                *active.cont_prim_type() = None;
                active.defs().active_ctx = context;
                use crate::quoted::QuotedClose;
                let p = p.quoted_close(active.env()).map_err(Ctl::Interrupt)?;
                let env = active.env().clone();
                match crate::vm_match::pattern_matches(env, p.as_ref().data_ref(), v) {
                    Some(env) => {
                        *active.env() = env;
                        bump(active, 1);
                    }
                    None => return Err(Ctl::Interrupt(type_mismatch_!(file!(), line!()))),
                }
                if last {
                    // `Cont::LetVarRet`: a block whose last declaration binds a
                    // plain variable answers with that variable, and otherwise
                    // with unit.
                    bump(active, 0);
                    return match &p.as_ref().data_ref() {
                        Pat::Var(i) => match active.env().get(&i.0) {
                            Some(v) => Ok(v.fast_clone()),
                            None => Err(Ctl::Interrupt(impossible_!(
                                line!(),
                                "let-var return names {}, which its own environment does not bind",
                                i.0.as_str()
                            ))),
                        },
                        _ => Ok(Value::Unit.share()),
                    };
                }
            }

            // Var => true, and the right-hand side does not keep a pointer.
            // A block ending in `var` answers with unit, one step later.
            Dec::Var(p, e) => {
                let x = match crate::vm_match::get_pat_var(&p.0) {
                    Some(x) => x.fast_clone(),
                    None => return delegate_decs(active, dec_, rest, w),
                };
                bump(active, 0);
                let v = sub(active, &dec_amb, e, depth, w, 1)?;
                *active.cont_prim_type() = None;
                active.defs().active_ctx = context;
                let ptr = active.alloc(v);
                bump(active, 1);
                active
                    .env()
                    .insert(x.as_ref().data_ref().clone(), Value::Pointer(ptr).share());
            }

            // 1 step, 0 redex.
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
                bump(active, 0);
                if last {
                    return Ok(v);
                }
            }

            // A declaration's continuation is the rest of the list, so a
            // declaration outside the set hands over the whole remainder rather
            // than one node. Modules, actors, imports and object declarations
            // all arrive here; each of them reaches state that no `Active`
            // accessor can see, which is the reason they are not in the set.
            _ => return delegate_decs(active, dec_, rest, w),
        }
    }
}

/// Hand the rest of a declaration list to the machine, from this declaration on.
fn delegate_decs<A: Active>(
    active: &mut A,
    dec_: Dec_,
    rest: Vector<Dec_>,
    watermark: usize,
) -> Result<Value_, Ctl> {
    let mut decs = rest;
    decs.push_front(dec_);
    *active.cont() = Cont::Decs(decs);
    loop {
        let len = active.stack().len();
        if len < watermark {
            return Err(Ctl::Escaped);
        }
        if len == watermark {
            if let Cont::Value_(v) = active.cont() {
                return Ok(v.fast_clone());
            }
        }
        step_once(active)?;
    }
}
