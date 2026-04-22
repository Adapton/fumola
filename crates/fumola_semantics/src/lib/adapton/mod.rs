use crate::Shared;
use crate::value::{Closed, Symbol, Symbol_, ThunkBody, Value_};
use fumola_syntax::ast::Exp_;
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;

mod reserved;
use reserved::ReservedSymbol;

mod graphical;
pub mod peek_value;
mod simple;

pub mod state;

pub enum Strategy {
    Simple,
    Graphical,
}

pub trait AdaptonState {
    fn new(strategy: Strategy) -> Self
    where
        Self: Sized;
    fn reset(&mut self, strategy: Strategy) -> Res<()>;
    fn now(&self) -> Time;
    fn here(&self) -> Space;
    fn put_pointer(&mut self, pointer: Pointer, value: Value_) -> Res<()>;
    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<Pointer>;
    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_>;
    fn put_pointer_delay(&mut self, pointer: Pointer, time: Time, value: Value_) -> Res<()>;
    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer>;
    fn force_begin(&mut self, pointer: Pointer) -> Res<ForceBeginResult>;
    fn force_end(&mut self, value: Value_) -> Res<()>;
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()>;
    fn navigate_end(&mut self) -> Res<()>;
    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>>;
    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_>;
    fn peek_events(&mut self) -> Res<Value_>;
    fn poke(&mut self, pointer: Pointer, time: Option<Time>, value: Value_) -> Res<()>;
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Error {
    Internal(u32),     // gives line number.
    TypeMismatch(u32), // gives line number.
    UndefinedNow(Pointer),
    Unreachable,
    UnreachableForceEnd,
    DanglingPointer(Space),
    CannotPutReadOnlyReservedSymbol(ReservedSymbol),
    CannotPutFutureReservedSymbol(Symbol_),
}

/// A force either results in a cache hit, or a cache miss; the execution of each situation continues differently.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum ForceBeginResult {
    CacheMiss(ThunkBody),
    CacheHit(MetaTime, Value_),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MetaTime(pub BigUint);
use std::ops::Add;

impl MetaTime {
    pub fn new() -> Self {
        MetaTime(BigUint::from(0 as usize))
    }
    pub fn incr(&mut self) {
        self.0 = self.0.clone().add(1 as usize);
    }
    pub fn pair(begin: Option<Self>, end: Self) -> (Self, Self) {
        if let Some(begin) = begin {
            (begin, end)
        } else {
            (end.clone(), end)
        }
    }
}

pub type Res<Ok> = Result<Ok, Error>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Navigation {
    GotoTime,
    GotoSpace,
    WithinTime,
    WithinSpace,
}

// the value of a (named) pointer does not include its times.
pub type Pointer = Space;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Time {
    Symbol(Symbol_),
    Now,
}

/// Node names.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Space {
    Symbol(Symbol_),
    Exp_(Option<Symbol_>, Closed<Exp_>),
    Here,
}

impl Time {
    pub fn apply(&self, symbol: Symbol_) -> Time {
        match self {
            Time::Now => Time::Symbol(symbol),
            Time::Symbol(ambient) => {
                Time::Symbol(Shared::new(Symbol::Call(ambient.clone(), symbol)))
            }
        }
    }

    pub fn into_symbol(&self) -> Res<Symbol_> {
        match self {
            Time::Now => todo!(),
            Time::Symbol(s) => Ok(s.clone()),
        }
    }
}

impl Space {
    pub fn apply(&self, symbol: Symbol_) -> Space {
        match self {
            Space::Here => Space::Symbol(symbol),
            Space::Symbol(ambient) => {
                Space::Symbol(Shared::new(Symbol::Call(ambient.clone(), symbol)))
            }
            Space::Exp_(Some(ambient), closed) => Space::Exp_(
                Some(Shared::new(Symbol::Call(ambient.clone(), symbol))),
                closed.clone(),
            ),
            Space::Exp_(None, closed) => Space::Exp_(Some(symbol), closed.clone()),
        }
    }

    pub fn into_symbol(&self) -> Res<Symbol_> {
        match self {
            Space::Here => todo!(),
            Space::Symbol(s) => Ok(s.clone()),
            Space::Exp_(_, _) => todo!(),
        }
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            // We choose non-equal Ids (including symbols) to be incomparable.
            // That permits their use to express certain kinds of independence/parallelism in the time ordering.
            (Symbol::Id(i1), Symbol::Id(i2)) => {
                if i1 == i2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            (Symbol::QuotedAst(q1), Symbol::QuotedAst(q2)) => {
                // We choose non-equal QuotedAsts (including symbols) to be incomparable.
                // That permits their use to express certain kinds of independence/parallelism in the time ordering.
                if q1 == q2 {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            (Symbol::Nat(n1), Symbol::Nat(n2)) => Some(n1.cmp(n2)),
            (Symbol::Nat(_), Symbol::QuotedAst(_)) => Some(Ordering::Less),
            (Symbol::QuotedAst(_), Symbol::Nat(_)) => Some(Ordering::Greater),
            (Symbol::Nat(_), Symbol::UnOp(_, _)) => Some(Ordering::Less),
            (Symbol::Nat(_), Symbol::BinOp(_, _, _)) => Some(Ordering::Less),
            (Symbol::Nat(_), Symbol::Call(_, _)) => Some(Ordering::Less),
            (Symbol::Call(_, _), Symbol::Nat(_)) => Some(Ordering::Greater),
            (Symbol::Call(_, _), Symbol::Int(_)) => Some(Ordering::Greater),
            (Symbol::Call(s11, s12), Symbol::Call(s21, s22)) => {
                let cmp1 = s11.get().partial_cmp(s21);
                if let Some(Ordering::Equal) = cmp1 {
                    s12.get().partial_cmp(s22)
                } else {
                    cmp1
                }
            }
            (Symbol::Call(s1, _), s2) => s1.get().partial_cmp(s2),
            (s1, Symbol::Call(s2, _)) => s1.partial_cmp(&*s2),
            (Symbol::BinOp(s11, b1, s12), Symbol::BinOp(s21, b2, s22)) => {
                let cmp1 = s11.get().partial_cmp(s21);
                if let Some(Ordering::Equal) = cmp1 {
                    let cmp2 = s12.get().partial_cmp(s22);
                    if let Some(Ordering::Equal) = cmp2 {
                        b1.partial_cmp(b2)
                    } else {
                        cmp2
                    }
                } else {
                    cmp1
                }
            }
            (s1, s2) => {
                todo!("{:?} <= {:?}", s1, s2)
            }
        }
    }
}

impl PartialOrd for Time {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Time::Now, Time::Now) => Some(Ordering::Equal),
            (Time::Now, _) => Some(Ordering::Less),
            (_, Time::Now) => Some(Ordering::Greater),
            (Time::Symbol(s1), Time::Symbol(s2)) => s1.partial_cmp(s2),
        }
    }
}
