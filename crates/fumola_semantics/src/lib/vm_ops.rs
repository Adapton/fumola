use std::cmp::Ordering;

use fumola_syntax::ast::{BinOp, PrimType, RelOp, UnOp};
use crate::value::{Symbol, Value, Value_};
use crate::vm_types::Interruption;
use crate::{Shared, quoted, type_mismatch_};
use num_bigint::{BigUint, ToBigInt};

use crate::{nyi, type_mismatch};


pub fn unop(un: UnOp, v: Value_) -> Result<Value, Interruption> {
    match (&un, &*v) {
        (UnOp::Neg, Value::Nat(n)) => Ok(Value::Int(-n.to_bigint().unwrap())),
        (UnOp::Neg, Value::Int(i)) => Ok(Value::Int(-i)),
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
            (Int(i1), Int(i2)) => Ok(Int(i1 + i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 + *f2)),
            // _ => nyi!(line!()),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Div => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 / n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 / i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 / *f2)),
            // _ => nyi!(line!()),
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
            (Int(i1), Int(i2)) => Ok(Int(i1 - i2)),
            (Int(i1), Nat(n2)) => Ok(Int(i1 - n2.to_bigint().unwrap())),
            (Nat(n1), Int(i2)) => Ok(Int(n1.to_bigint().unwrap() - i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 - *f2)),
            // _ => nyi!(line!()),
            (v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Mul => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 * n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 * i2)),
            (Float(f1), Float(f2)) => Ok(Float(*f1 * *f2)),
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
            (_, Value::QuotedAst(q1), Value::QuotedAst(q2)) => Ok(Value::QuotedAst(quoted::append(q1, q2)?)),
            (_, Value::Array(xm, xs), Value::Array(_ym, ys)) => {
                let mut xs = xs.clone();
                xs.append(ys.clone());
                Ok(Value::Array(xm.clone(), xs))
            }
            (_, v1, v2) => try_symbolic_binop(&binop, v1, v2).ok_or(type_mismatch_!()),
        },
        Mod => match (&*v1, &*v2) {
            (Nat(n1), Nat(n2)) => Ok(Nat(n1 % n2)),
            (Int(i1), Int(i2)) => Ok(Int(i1 % i2)),
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
    Ok(Bool(match relop {
        Eq => match (&*v1, &*v2) {
            (Unit, Unit) => true,
            (Bool(b1), Bool(b2)) => b1 == b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 == n2,
            (Int(i1), Int(i2)) => i1 == i2,
            (v1, v2) => v1 == v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        Neq => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Bool(b1), Bool(b2)) => b1 != b2,
            (Text(t1), Text(t2)) => t1 == t2,
            (Nat(n1), Nat(n2)) => n1 != n2,
            (Int(i1), Int(i2)) => i1 != i2,
            (v1, v2) => v1 != v2, //            _ => nyi!(line!(), "{:?} == {:?}", v1, v2)?,
        },
        Lt => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 < n2,
            (Int(i1), Int(i2)) => i1 < i2,
            (Int(i), Nat(n)) => i.lt(&n.to_bigint().unwrap()),
            (Nat(n), Int(i)) => n.to_bigint().unwrap().lt(i),
            (Symbol(s1), Symbol(s2)) => s1.partial_cmp(s2) == Some(Ordering::Less),
            _ => nyi!(line!(), "{:?} < {:?}", v1, v2)?,
        },
        Le => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 <= n2,
            (Int(i1), Int(i2)) => i1 <= i2,
            (Symbol(s1), Symbol(s2)) => match s1.partial_cmp(s2) {
                Some(Ordering::Less) => true,
                Some(Ordering::Equal) => true,
                _ => false,
            },
            _ => nyi!(line!(), "{:?} <= {:?}", v1, v2)?,
        },
        Gt => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 > n2,
            (Int(i1), Int(i2)) => i1 > i2,
            (Symbol(s1), Symbol(s2)) => s1.partial_cmp(s2) == Some(Ordering::Greater),
            _ => nyi!(line!(), "{:?} > {:?}", v1, v2)?,
        },
        Ge => match (&*v1, &*v2) {
            (Unit, Unit) => false,
            (Nat(n1), Nat(n2)) => n1 >= n2,
            (Int(i1), Int(i2)) => i1 >= i2,
            (Symbol(s1), Symbol(s2)) => match s1.partial_cmp(s2) {
                Some(Ordering::Greater) => true,
                Some(Ordering::Equal) => true,
                _ => false,
            },
            _ => nyi!(line!(), "{:?} >= {:?}", v1, v2)?,
        },
        //        _ => nyi!(line!(), "relop({:?}, {:?}, {:?})", relop, v1, v2)?,
    }))
}
