use std::cmp::Ordering;

use crate::value::{Symbol, Value, Value_};
use crate::vm_types::Interruption;
use crate::{Shared, quoted, type_mismatch_};
use fumola_syntax::ast::{BinOp, PrimType, RelOp, UnOp};
use num_bigint::{BigInt, BigUint, Sign, ToBigInt};
use num_traits::ToPrimitive;
use ordered_float::OrderedFloat;

use crate::{nyi, type_mismatch};

/// A whole number as Fumola keeps it: 0 and up a Nat, below 0 an Int, so a
/// number has one representation, and `==`, a map's keys and printing agree
/// on it (^report(28)'s traps; Adapton/fumola, ticket 571 of Matthew's work
/// notes).
pub fn whole(i: BigInt) -> Value {
    match i.to_biguint() {
        Some(n) => Value::Nat(n),
        None => Value::Int(i),
    }
}

/// A whole number's value, whichever way it is kept.
fn whole_of(v: &Value) -> Option<BigInt> {
    match v {
        Value::Nat(n) => Some(BigInt::from_biguint(Sign::Plus, n.clone())),
        Value::Int(i) => Some(i.clone()),
        _ => None,
    }
}

/// A number's value as a float: a Float, or a whole number.
fn float_of(v: &Value) -> Option<f64> {
    match v {
        Value::Float(f) => Some(**f),
        _ => whole_of(v).and_then(|i| i.to_f64()),
    }
}

/// Equality as `==` and `!=` (and literal patterns) see it: whole numbers by
/// value, however kept; a Float and a whole number by value; anything else as
/// the values are.
pub fn values_eq(v1: &Value, v2: &Value) -> bool {
    use Value::*;
    match (v1, v2) {
        (Nat(_) | Int(_), Nat(_) | Int(_)) => whole_of(v1) == whole_of(v2),
        (Float(_), Nat(_) | Int(_)) | (Nat(_) | Int(_), Float(_)) => float_of(v1) == float_of(v2),
        (v1, v2) => v1 == v2,
    }
}

/// The order `<`, `<=`, `>` and `>=` use: numbers by value, whatever kind
/// (whole numbers exactly; a Float against either as floats), text by code
/// point. None: no order between these two kinds. (Symbols are ordered in
/// relop, where two that are not comparable answer false.)
fn order(v1: &Value, v2: &Value) -> Option<Ordering> {
    use Value::*;
    match (v1, v2) {
        (Nat(_) | Int(_), Nat(_) | Int(_)) => Some(whole_of(v1)?.cmp(&whole_of(v2)?)),
        (Float(_) | Nat(_) | Int(_), Float(_) | Nat(_) | Int(_)) => float_of(v1)?.partial_cmp(&float_of(v2)?),
        (Text(t1), Text(t2)) => Some(t1.to_string().cmp(&t2.to_string())),
        _ => None,
    }
}

pub fn unop(un: UnOp, v: Value_) -> Result<Value, Interruption> {
    match (&un, &*v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(whole(-n.to_bigint().unwrap())),
        (UnOp::Neg, Value::Int(i)) => Ok(whole(-i)),
        (UnOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
        (unop, v) => {
            if let Ok(symbol) = v.into_sym_or(()) {
                Ok(Value::Symbol(Shared::new(Symbol::UnOp(
                    unop.clone(),
                    symbol,
                ))))
            } else {
                crate::nyi!(line!())
            }
        }
    }
}

pub fn try_symbolic_binop(binop: &BinOp, v1: &Value, v2: &Value) -> Option<Value> {
    match (v1.into_sym_or(()), v2.into_sym_or(())) {
        (Ok(s1), Ok(s2)) => Some(Value::Symbol(Shared::new(Symbol::BinOp(
            s1,
            binop.clone(),
            s2,
        )))),
        _ => None,
    }
}

pub fn binop(
    cont_prim_type: &Option<PrimType>,
    binop: BinOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use BinOp::*;
    use Value::*;
    if let Unit = &*v1 {
        type_mismatch!(file!(), line!());
    };
    if let Unit = &*v2 {
        type_mismatch!(file!(), line!());
    };
    match binop {
        Add => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 + n2)),
            (Nat(_) | Int(_), Nat(_) | Int(_)) => Ok(whole(whole_of(&v1).unwrap() + whole_of(&v2).unwrap())),
            (Float(f1), Float(f2)) => Ok(Float(*f1 + *f2)),
            (Float(_), Nat(_) | Int(_)) | (Nat(_) | Int(_), Float(_)) => Ok(Float(OrderedFloat(float_of(&v1).unwrap() + float_of(&v2).unwrap()))),

            // _ => nyi!(line!()),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Div => match (&*v1, &*v2) {
            // (a whole number divided by 0 is the type mismatch it was, not a panic)
            (Nat(_) | Int(_), Nat(n2)) if n2 == &BigUint::from(0u8) => type_mismatch!(file!(), line!()),
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 / n2)),
            (Nat(_) | Int(_), Nat(_) | Int(_)) => Ok(whole(whole_of(&v1).unwrap() / whole_of(&v2).unwrap())),
            (Float(f1), Float(f2)) => Ok(Float(*f1 / *f2)),
            (Float(_), Nat(_) | Int(_)) | (Nat(_) | Int(_), Float(_)) => Ok(Float(OrderedFloat(float_of(&v1).unwrap() / float_of(&v2).unwrap()))),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Sub => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => {
                if n2 > n1 {
                    Ok(Int(n1.to_bigint().unwrap() - n2.to_bigint().unwrap()))
                } else {
                    Ok(Nat(n1 - n2))
                }
            }
            (Nat(_) | Int(_), Nat(_) | Int(_)) => Ok(whole(whole_of(&v1).unwrap() - whole_of(&v2).unwrap())),
            (Float(f1), Float(f2)) => Ok(Float(*f1 - *f2)),
            (Float(_), Nat(_) | Int(_)) | (Nat(_) | Int(_), Float(_)) => Ok(Float(OrderedFloat(float_of(&v1).unwrap() - float_of(&v2).unwrap()))),

            // _ => nyi!(line!()),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Mul => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Nat(_) | Int(_), Nat(_) | Int(_)) => Ok(whole(whole_of(&v1).unwrap() * whole_of(&v2).unwrap())),
            (Float(f1), Float(f2)) => Ok(Float(*f1 * *f2)),
            (Float(_), Nat(_) | Int(_)) | (Nat(_) | Int(_), Float(_)) => Ok(Float(OrderedFloat(float_of(&v1).unwrap() * float_of(&v2).unwrap()))),

            // _ => nyi!(line!()),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        WAdd => match (cont_prim_type, &*v1, &*v2) {
            (None, _, _) => Err(Interruption::AmbiguousOperation),
            (Some(t), Value::Nat(n1), Value::Nat(n2)) => match t {
                PrimType::Nat => Ok(Value::Nat(n1 + n2)),
                PrimType::Nat8 => Ok(Value::Nat(
                    (n1 + n2) % BigUint::parse_bytes(b"256", 10).unwrap(),
                )),
                _ => nyi!(line!()),
            },
            _ => nyi!(line!()),
        },
        Cat => match (cont_prim_type, &*v1, &*v2) {
            (_, Value::Text(t1), Value::Text(t2)) => Ok(Value::Text(t1.append(t2))),
            (_, Value::QuotedAst(q1), Value::QuotedAst(q2)) => {
                Ok(Value::QuotedAst(quoted::append(q1, q2)?))
            }
            (_, Value::Array(xm, xs), Value::Array(_ym, ys)) => {
                let mut xs = xs.clone();
                xs.append(ys.clone());
                Ok(Value::Array(xm.clone(), xs))
            }
            (_, v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Mod => match (&*v1, &*v2) {
            (Nat(_) | Int(_), Nat(n2)) if n2 == &BigUint::from(0u8) => type_mismatch!(file!(), line!()),
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 % n2)),
            (Nat(_) | Int(_), Nat(_) | Int(_)) => Ok(whole(whole_of(&v1).unwrap() % whole_of(&v2).unwrap())),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },

        Pow | And | Or | Xor | ShL | ShR | RotL | RotR | WSub | WMul | WPow | BitOr | BitAnd => {
            try_symbolic_binop(&binop, &*v1, &*v2).ok_or(type_mismatch_!())
        }
    }
}

pub fn relop(
    _cont_prim_xotype: &Option<PrimType>,
    relop: RelOp,
    v1: Value_,
    v2: Value_,
) -> Result<Value, Interruption> {
    use RelOp::*;
    use Value::*;
    // (symbols by their partial order: two that are not comparable, distinct quoted ASTs, answer false to all four, which
    // says they are independent in time; anything else without an order stops, as before)
    let ordered = |op: &str| -> Result<Option<Ordering>, Interruption> {
        match (&*v1, &*v2) {
            (Symbol(s1), Symbol(s2)) => Ok(s1.partial_cmp(s2)),
            _ => match order(&v1, &v2) {
                Some(o) => Ok(Some(o)),
                None => nyi!(line!(), "{:?} {} {:?}", v1, op, v2),
            },
        }
    };
    Ok(Bool(match relop {
        // (one equality for both, so they can never disagree again: text != was inverted)
        Eq => values_eq(&v1, &v2),
        Neq => !values_eq(&v1, &v2),
        // (() against () keeps its old answer, false, for all four)
        Lt => !matches!((&*v1, &*v2), (Unit, Unit)) && ordered("<")? == Some(Ordering::Less),
        Le => !matches!((&*v1, &*v2), (Unit, Unit)) && matches!(ordered("<=")?, Some(Ordering::Less | Ordering::Equal)),
        Gt => !matches!((&*v1, &*v2), (Unit, Unit)) && ordered(">")? == Some(Ordering::Greater),
        Ge => !matches!((&*v1, &*v2), (Unit, Unit)) && matches!(ordered(">=")?, Some(Ordering::Greater | Ordering::Equal)),
    }))
}
