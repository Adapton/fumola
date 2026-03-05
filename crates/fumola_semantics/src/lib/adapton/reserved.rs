use serde::{Deserialize, Serialize};

use crate::value::Symbol;

///
/// Any symbol with a prefix of `adapton` is reserved for future use.
///
pub fn is_future_reserved_symbol(symbol: &Symbol) -> bool {
    match symbol {
        Symbol::Id(x) => x.as_str() == "adapton",
        Symbol::Call(x, _) => is_future_reserved_symbol(x),
        Symbol::Dot(x, _) => is_future_reserved_symbol(x),
        Symbol::BinOp(x, _, _) => is_future_reserved_symbol(x),
        Symbol::Nat(_) => false,
        Symbol::Int(_) => false,
        Symbol::QuotedAst(_) => false,
        Symbol::UnOp(_, x) => is_future_reserved_symbol(x),
    }
}

// Current Reserved Symbols:
//
// `adapton(`state)
// `adapton(`settings)(`forceBeginAlwaysMisses).
// `adapton(`settings)(`forceEndForgetsResult).
// `adapton(`counts)(`cells)
//                  (`nonThunkCells)
//                  (`thunkCells)
//                  (`put)
//                  (`putDelay`)
//                  (`get)
//                  (`forceBegin)
//                  (`forceEnd)
//                  (`forceBeginCacheHit)
//                  (`forceBeginCacheMiss)
//
// Operations on these symbols have special meaning:
//  - They are not part of the graphical representation of dependencies.
//  - Every put acts like a poke; every get acts like a peek.
//
// There are peek/poke mode restrictions:
//  1. `settings(*) permit both peek and poke modes.
//  2. `state and `counts(*) permits only peek mode.
//
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum ReservedSymbol {
    State,
    Settings,
    SettingsForceBeginAlwaysMisses,
    SettingsForceEndForgetsResult,
    Counts,
    CountsCells,
    CountsThunkCells,
    CountsNonThunkCells,
    CountsPut,
    CountsPutDelay,
    CountsGet,
    CountsForceBegin,
    CountsForceEnd,
    CountsForceBeginCacheHit,
    CountsForceBeginCacheMiss,
}

pub fn into_reserved_symbol(symbol: &Symbol) -> Option<ReservedSymbol> {
    match symbol {
        Symbol::Call(x, y) => match x.get() {
            Symbol::Id(id) => {
                if id.as_str() == "adapton" {
                    match y.get() {
                        Symbol::Id(id) => match id.as_str() {
                            "state" => Some(ReservedSymbol::State),
                            "settings" => Some(ReservedSymbol::Settings),
                            "counts" => Some(ReservedSymbol::Counts),
                            _ => None,
                        },
                        _ => None,
                    }
                } else {
                    None
                }
            }
            Symbol::Call(x1, x2) => match (x1.get(), x2.get(), y.get()) {
                (Symbol::Id(x), Symbol::Id(y), Symbol::Id(z)) => {
                    if x.as_str() != "adapton" {
                        return None;
                    };
                    match y.as_str() {
                        "counts" => match z.as_str() {
                            "cells" => Some(ReservedSymbol::CountsCells),
                            "nonThunkCells" => Some(ReservedSymbol::CountsNonThunkCells),
                            "thunkCells" => Some(ReservedSymbol::CountsThunkCells),
                            "put" => Some(ReservedSymbol::CountsPut),
                            "putDelay" => Some(ReservedSymbol::CountsPutDelay),
                            "get" => Some(ReservedSymbol::CountsGet),
                            "forceBegin" => Some(ReservedSymbol::CountsForceBegin),
                            "forceEnd" => Some(ReservedSymbol::CountsForceEnd),
                            "forceBeginCacheHit" => Some(ReservedSymbol::CountsForceBeginCacheHit),
                            "forceBeginCacheMiss" => {
                                Some(ReservedSymbol::CountsForceBeginCacheMiss)
                            }
                            _ => None,
                        },
                        "settings" => match z.as_str() {
                            "forceBeginAlwaysMisses" => {
                                Some(ReservedSymbol::SettingsForceBeginAlwaysMisses)
                            }
                            "forceEndForgetsResult" => {
                                Some(ReservedSymbol::SettingsForceEndForgetsResult)
                            }
                            _ => None,
                        },
                        _ => None,
                    }
                }
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}
