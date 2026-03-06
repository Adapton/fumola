use crate::adapton::{
    AdaptonState, Error, ForceBeginResult, Navigation, Pointer, Res, Space, Strategy, Time,
};
use serde::{Deserialize, Serialize};

use crate::adapton::graphical;
use crate::adapton::reserved::{self, ReservedSymbol};
use crate::adapton::simple::{self};

use crate::ToMotoko;
use crate::value::{Symbol_, Value_};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum InnerState {
    Simple(simple::SimpleState),
    Graphical(graphical::GraphicalState),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct State {
    inner: InnerState,
    settings: Settings,
    counts: Counts,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Counts {
    pub cells: u64,
    pub non_thunk_cells: u64,
    pub thunk_cells: u64,
    pub put: u64,
    pub put_delay: u64,
    pub get: u64,
    pub force_begin: u64,
    pub force_end: u64,
    pub force_begin_cache_hit: u64,
    pub force_begin_cache_miss: u64,
}

impl Counts {
    pub fn new() -> Self {
        Counts {
            cells: 0,
            non_thunk_cells: 0,
            thunk_cells: 0,
            put: 0,
            put_delay: 0,
            get: 0,
            force_begin: 0,
            force_end: 0,
            force_begin_cache_hit: 0,
            force_begin_cache_miss: 0,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Settings {
    pub force_begin_always_misses: bool,
    pub force_end_forgets_result: bool,
}

impl Settings {
    pub fn new() -> Self {
        Settings {
            force_begin_always_misses: false,
            force_end_forgets_result: false,
        }
    }
}

/// Internal version of `AdaptonState` trait, for different caching strategies to each implement.
pub trait CacheState {
    fn new() -> Self
    where
        Self: Sized;
    fn reset(&mut self) -> Res<()>;
    fn now(&self) -> Time;
    fn here(&self) -> Space;
    fn put_pointer(&mut self, counts: &mut Counts, _pointer: Pointer, _value: Value_) -> Res<()>;
    fn put_symbol(&mut self, counts: &mut Counts, _symbol: Symbol_, _value: Value_)
    -> Res<Pointer>;
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_>;
    fn put_pointer_delay(&mut self, pointer: Pointer, time: Time, value: Value_) -> Res<()>;
    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer>;
    fn force_begin(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        _pointer: Pointer,
    ) -> Res<ForceBeginResult>;
    fn force_end(&mut self, settings: &Settings, _value: Value_) -> Res<()>;
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()>;
    fn navigate_end(&mut self) -> Res<()>;
    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>>;
    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_>;
    fn poke(&mut self, pointer: Pointer, time: Option<Time>, value: Value_) -> Res<()>;
}

impl State {
    fn put_reserved_symbol(&mut self, symbol: ReservedSymbol, value: Value_) -> Res<()> {
        match symbol {
            ReservedSymbol::SettingsForceBeginAlwaysMisses => {
                self.settings.force_begin_always_misses =
                    value.as_ref().into_bool_or(Error::TypeMismatch(line!()))?;
                Ok(())
            }
            ReservedSymbol::SettingsForceEndForgetsResult => {
                self.settings.force_end_forgets_result =
                    value.as_ref().into_bool_or(Error::TypeMismatch(line!()))?;
                Ok(())
            }
            _ => Err(Error::CannotPutReadOnlyReservedSymbol(symbol)),
        }
    }

    fn get_reserved_symbol(&mut self, symbol: ReservedSymbol) -> Res<Value_> {
        match symbol {
            ReservedSymbol::State => self.inner.clone().to_motoko_shared(),
            ReservedSymbol::SettingsForceBeginAlwaysMisses => {
                self.settings.force_begin_always_misses.to_motoko_shared()
            }
            ReservedSymbol::SettingsForceEndForgetsResult => {
                self.settings.force_end_forgets_result.to_motoko_shared()
            }
            ReservedSymbol::CountsCells => self.counts.cells.to_motoko_shared(),
            ReservedSymbol::CountsThunkCells => self.counts.thunk_cells.to_motoko_shared(),
            ReservedSymbol::CountsNonThunkCells => self.counts.non_thunk_cells.to_motoko_shared(),
            ReservedSymbol::CountsPut => self.counts.put.to_motoko_shared(),
            ReservedSymbol::CountsPutDelay => self.counts.put_delay.to_motoko_shared(),
            ReservedSymbol::CountsGet => self.counts.get.to_motoko_shared(),
            ReservedSymbol::CountsForceBegin => self.counts.force_begin.to_motoko_shared(),
            ReservedSymbol::CountsForceEnd => self.counts.force_end.to_motoko_shared(),
            ReservedSymbol::CountsForceBeginCacheHit => {
                self.counts.force_begin_cache_hit.to_motoko_shared()
            }
            ReservedSymbol::CountsForceBeginCacheMiss => {
                self.counts.force_begin_cache_miss.to_motoko_shared()
            }
            ReservedSymbol::Settings => self.settings.clone().to_motoko_shared(),
            ReservedSymbol::Counts => self.counts.clone().to_motoko_shared(),
        }
        .map_err(|_e| Error::Unreachable)
    }
}

impl AdaptonState for State {
    fn new(strategy: Strategy) -> Self
    where
        Self: Sized,
    {
        let inner = match strategy {
            Strategy::Simple => InnerState::Simple(simple::SimpleState::new()),
            Strategy::Graphical => InnerState::Graphical(graphical::GraphicalState::new()),
        };
        State {
            inner,
            settings: Settings {
                force_begin_always_misses: false,
                force_end_forgets_result: false,
            },
            counts: Counts::new(),
        }
    }

    fn reset(&mut self, s: Strategy) -> Res<()> {
        *self = Self::new(s);
        Ok(())
    }

    fn put_pointer(&mut self, pointer: Pointer, value: Value_) -> Res<()> {
        // check if pointer is reserved.
        if let Some(reserved_symbol) = reserved::into_reserved_symbol(&*pointer.into_symbol()?) {
            self.put_reserved_symbol(reserved_symbol, value)
        } else if reserved::is_future_reserved_symbol(pointer.into_symbol()?.as_ref()) {
            Err(Error::CannotPutFutureReservedSymbol(pointer.into_symbol()?))
        } else {
            self.counts.put += 1;
            match &mut self.inner {
                InnerState::Simple(s) => s.put_pointer(&mut self.counts, pointer, value),
                InnerState::Graphical(g) => g.put_pointer(&mut self.counts, pointer, value),
            }
        }
    }

    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<Pointer> {
        if let Some(reserved_symbol) = reserved::into_reserved_symbol(&symbol) {
            self.put_reserved_symbol(reserved_symbol, value)?;
            Ok(Space::Symbol(symbol))
        } else if reserved::is_future_reserved_symbol(&symbol) {
            Err(Error::CannotPutFutureReservedSymbol(symbol))
        } else {
            self.counts.put += 1;
            match &mut self.inner {
                InnerState::Simple(s) => s.put_symbol(&mut self.counts, symbol, value),
                InnerState::Graphical(g) => g.put_symbol(&mut self.counts, symbol, value),
            }
        }
    }

    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        // check if pointer is reserved.
        if let Some(reserved_symbol) = reserved::into_reserved_symbol(&*pointer.into_symbol()?) {
            self.get_reserved_symbol(reserved_symbol)
        } else {
            self.counts.get += 1;
            match &mut self.inner {
                InnerState::Simple(s) => s.get_pointer(pointer),
                InnerState::Graphical(g) => g.get_pointer(pointer),
            }
        }
    }

    fn force_begin(&mut self, pointer: Pointer) -> Res<ForceBeginResult> {
        self.counts.force_begin += 1;
        match &mut self.inner {
            InnerState::Simple(s) => s.force_begin(&self.settings, &mut self.counts, pointer),
            InnerState::Graphical(g) => g.force_begin(&self.settings, &mut self.counts, pointer),
        }
    }

    fn force_end(&mut self, value: Value_) -> Res<()> {
        self.counts.force_end += 1;
        match &mut self.inner {
            InnerState::Simple(s) => s.force_end(&self.settings, value),
            InnerState::Graphical(g) => g.force_end(&self.settings, value),
        }
    }

    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        match &mut self.inner {
            InnerState::Simple(s) => s.navigate_begin(nav, symbol),
            InnerState::Graphical(g) => g.navigate_begin(nav, symbol),
        }
    }

    fn navigate_end(&mut self) -> Res<()> {
        match &mut self.inner {
            InnerState::Simple(s) => s.navigate_end(),
            InnerState::Graphical(g) => g.navigate_end(),
        }
    }

    fn now(&self) -> Time {
        match &self.inner {
            InnerState::Simple(s) => s.now(),
            InnerState::Graphical(g) => g.now(),
        }
    }

    fn here(&self) -> Space {
        match &self.inner {
            InnerState::Simple(s) => s.here(),
            InnerState::Graphical(g) => g.here(),
        }
    }

    fn put_pointer_delay(&mut self, pointer: Pointer, time: Time, value: Value_) -> Res<()> {
        self.counts.put_delay += 1;
        match &mut self.inner {
            InnerState::Simple(s) => s.put_pointer_delay(pointer, time, value),
            InnerState::Graphical(g) => g.put_pointer_delay(pointer, time, value),
        }
    }

    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer> {
        self.counts.put_delay += 1;
        match &mut self.inner {
            InnerState::Simple(s) => s.put_symbol_delay(symbol, time, value),
            InnerState::Graphical(g) => g.put_symbol_delay(symbol, time, value),
        }
    }

    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>> {
        match &mut self.inner {
            InnerState::Simple(s) => s.peek(pointer),
            InnerState::Graphical(g) => g.peek(pointer),
        }
    }

    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_> {
        match &mut self.inner {
            InnerState::Simple(s) => s.peek_cell(pointer),
            InnerState::Graphical(g) => g.peek_cell(pointer),
        }
    }

    fn poke(&mut self, pointer: Pointer, time: Option<Time>, value: Value_) -> Res<()> {
        match &mut self.inner {
            InnerState::Simple(s) => s.poke(pointer, time, value),
            InnerState::Graphical(g) => g.poke(pointer, time, value),
        }
    }
}
