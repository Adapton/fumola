use crate::adapton::AdaptonState;
use crate::{adapton, nyi};
use crate::value::{
    CollectionFunction, FastRandIter, FastRandIterFunction,
    HashMapFunction, PrimFunction, Text, Value, Value_,
};
use crate::vm_types::{
    Active, Cont, DebugPrintLine, Interruption, Step,
};
use crate::ast::{Inst, Literal, Pat};

use crate::vm_step::{
    cont_value, type_mismatch, unit_step,
};

use im_rc::HashMap;
use std::collections::hash_map;
use std::hash::{Hash, Hasher};

pub fn call_prim_function<A: Active>(
    active: &mut A,
    pf: &PrimFunction,
    targs: Option<Inst>,
    args: Value_,
) -> Result<Step, Interruption> {
    use PrimFunction::*;
    match pf {
        SymbolLevel => {
            if let Ok(symbol) = args.as_ref().into_sym_or(()) {
                let mut hasher = hash_map::DefaultHasher::new();
                symbol.as_ref().hash(&mut hasher);
                let hash = hasher.finish();
                *active.cont() = Cont::Value_(Value::Nat(hash.into()).into());
                Ok(Step {})
            } else {
                type_mismatch!(file!(), line!())
            }
        }
        WriteFile => match &*args {
            Value::Tuple(vs) => {
                if vs.len() != 2 {
                    type_mismatch!(file!(), line!())
                } else {
                    if let Ok(symbol) = vs[0].into_sym_or(()) {
                        if let Ok(text) = vs[1].into_text_or(()) {
                            active.output_files().insert(symbol.as_ref().clone(), text);
                            unit_step(active)
                        } else {
                            type_mismatch!(file!(), line!())
                        }
                    } else {
                        type_mismatch!(file!(), line!())
                    }
                }
            }
            _ => type_mismatch!(file!(), line!()),
        },
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
        RustDebugText => {
            *active.cont() = cont_value(Value::Text(Text::new(format!("{:?}", args))));
            Ok(Step {})
        }
        AdaptonPointer => todo!(),
        AdaptonPeek => todo!(),
        AdaptonPoke => todo!(),
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
        use crate::{Share, shared::FastClone, value::Collection};
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

