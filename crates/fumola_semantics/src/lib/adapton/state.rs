use crate::adapton::{
    AdaptonState, Error, ForceBeginResult, Navigation, Pointer, RepairStep, Res, Space, Strategy,
    Time,
};
use serde::{Deserialize, Serialize};

use crate::adapton::MetaTime;
use crate::adapton::graphical;
use crate::adapton::graphical::EdgeId;
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
    /// Puts of what a cell already held: no new version, nothing signaled.
    pub put_matched: u64,
    /// Signaling traversals: puts that changed a cell with readers.
    pub signalings: u64,
    /// Edges marked signaled by those traversals.
    pub edges_signaled: u64,
    /// Repairs: forces of a thunk whose trace held a signaled edge.
    pub repairs: u64,
    /// Edges a repair found consistent and marked aligned again.
    pub edges_aligned: u64,
    /// Thunks a repair re-evaluated.
    pub reevaluations: u64,
    /// Puts refused because one evaluation had used the name for something else.
    pub double_uses: u64,
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
            put_matched: 0,
            signalings: 0,
            edges_signaled: 0,
            repairs: 0,
            edges_aligned: 0,
            reevaluations: 0,
            double_uses: 0,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Settings {
    pub force_begin_always_misses: bool,
    pub force_end_forgets_result: bool,
    /// A put of what a cell already holds keeps the cell and its readers as they are, and
    /// records only the allocation edge -- Nominal Adapton's `Eval-refClean` / `Eval-thunkClean`.
    /// Off, every put makes a new version and signals the readers of the old one.
    pub put_matches_equal_values: bool,
    /// A put made by a computation, of different contents, under a name that a
    /// thunk still being evaluated had already observed or allocated, is an
    /// error (`Error::DoubleUse`) rather than a quiet edit. Off, it signals
    /// like any other put and the next force repairs whatever it reached.
    pub check_double_use: bool,
}

impl Settings {
    pub fn new() -> Self {
        Settings {
            force_begin_always_misses: false,
            force_end_forgets_result: false,
            put_matches_equal_values: true,
            check_double_use: true,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum PutBeh {
    Put,
    Poke,
    Undelay,
}

/// Internal version of `AdaptonState` trait, for different caching strategies to each implement.
pub trait CacheState {
    fn new() -> Self
    where
        Self: Sized;
    fn now(&self) -> Time;
    fn here(&self) -> Space;
    fn put_pointer(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        _pointer: Pointer,
        value: Value_,
        beh: PutBeh,
    ) -> Res<()>;
    fn put_symbol(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        _symbol: Symbol_,
        value: Value_,
    ) -> Res<Pointer>;
    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_>;
    fn put_pointer_delay(&mut self, pointer: Pointer, time: Time, value: Value_) -> Res<()>;
    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer>;
    fn force_begin(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        _pointer: Pointer,
    ) -> Res<ForceBeginResult>;
    fn force_end(&mut self, settings: &Settings, value: Value_) -> Res<()>;
    fn repair_step(&mut self, counts: &mut Counts) -> Res<RepairStep>;
    fn repair_resume(&mut self, counts: &mut Counts, value: Value_) -> Res<()>;
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()>;
    fn navigate_end(&mut self) -> Res<()>;
    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>>;
    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_>;
    fn peek_events(&mut self) -> Res<Value_>;
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
            ReservedSymbol::SettingsPutMatchesEqualValues => {
                self.settings.put_matches_equal_values =
                    value.as_ref().into_bool_or(Error::TypeMismatch(line!()))?;
                Ok(())
            }
            ReservedSymbol::SettingsCheckDoubleUse => {
                self.settings.check_double_use =
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
            ReservedSymbol::SettingsPutMatchesEqualValues => {
                self.settings.put_matches_equal_values.to_motoko_shared()
            }
            ReservedSymbol::SettingsCheckDoubleUse => {
                self.settings.check_double_use.to_motoko_shared()
            }
            ReservedSymbol::CountsPutMatched => self.counts.put_matched.to_motoko_shared(),
            ReservedSymbol::CountsSignalings => self.counts.signalings.to_motoko_shared(),
            ReservedSymbol::CountsEdgesSignaled => self.counts.edges_signaled.to_motoko_shared(),
            ReservedSymbol::CountsRepairs => self.counts.repairs.to_motoko_shared(),
            ReservedSymbol::CountsEdgesAligned => self.counts.edges_aligned.to_motoko_shared(),
            ReservedSymbol::CountsReevaluations => self.counts.reevaluations.to_motoko_shared(),
            ReservedSymbol::CountsDoubleUses => self.counts.double_uses.to_motoko_shared(),
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

impl State {
    /// Put a saved state back, keeping the counters that name things
    /// monotone.
    ///
    /// Everything that *describes* the session is restored: the nodes, the
    /// edges, the stack, the cursor, the settings. Everything that *names* a
    /// node or an edge goes forward instead -- `meta_time` and `next_edge_id`
    /// keep the larger of the saved and the reached value.
    ///
    /// The distinction is the whole correctness of this operation. A value
    /// computed on a branch can carry node ids and edge ids out with it. If
    /// the counters are rolled back, the session re-issues those exact ids to
    /// different nodes, and the escaped value then reads back somebody else's
    /// content with no error of any kind -- a wrong picture rather than a
    /// failure. Carried forward, an escaped id names nothing, which peeks as
    /// null and is a thing you can find.
    pub fn restore(&mut self, saved: State) {
        let reached = self.naming_high_water();
        *self = saved;
        self.advance_naming_to(reached);
    }

    /// The counters that hand out identities, as they now stand.
    fn naming_high_water(&self) -> Option<(MetaTime, EdgeId)> {
        match &self.inner {
            InnerState::Graphical(g) => Some((g.meta_time.clone(), g.next_edge_id.clone())),
            // The simple semantics keeps no graph, so it names nothing that
            // could escape.
            InnerState::Simple(_) => None,
        }
    }

    fn advance_naming_to(&mut self, reached: Option<(MetaTime, EdgeId)>) {
        let (meta_time, next_edge_id) = match reached {
            Some(pair) => pair,
            None => return,
        };
        if let InnerState::Graphical(g) = &mut self.inner {
            if meta_time.0 > g.meta_time.0 {
                g.meta_time = meta_time;
            }
            if next_edge_id.0 > g.next_edge_id.0 {
                g.next_edge_id = next_edge_id;
            }
        }
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
            settings: Settings::new(),
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
            let beh = PutBeh::Put;
            match &mut self.inner {
                InnerState::Simple(s) => {
                    s.put_pointer(&self.settings, &mut self.counts, pointer, value, beh)
                }
                InnerState::Graphical(g) => {
                    g.put_pointer(&self.settings, &mut self.counts, pointer, value, beh)
                }
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
                InnerState::Simple(s) => {
                    s.put_symbol(&self.settings, &mut self.counts, symbol, value)
                }
                InnerState::Graphical(g) => {
                    g.put_symbol(&self.settings, &mut self.counts, symbol, value)
                }
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

    fn repair_step(&mut self) -> Res<RepairStep> {
        match &mut self.inner {
            InnerState::Simple(s) => s.repair_step(&mut self.counts),
            InnerState::Graphical(g) => g.repair_step(&mut self.counts),
        }
    }

    fn repair_resume(&mut self, value: Value_) -> Res<()> {
        match &mut self.inner {
            InnerState::Simple(s) => s.repair_resume(&mut self.counts, value),
            InnerState::Graphical(g) => g.repair_resume(&mut self.counts, value),
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

    fn peek_events(&mut self) -> Res<Value_> {
        match &mut self.inner {
            InnerState::Simple(s) => s.peek_events(),
            InnerState::Graphical(g) => g.peek_events(),
        }
    }

    fn poke(&mut self, pointer: Pointer, time: Option<Time>, value: Value_) -> Res<()> {
        match time {
            None => {
                let beh = PutBeh::Poke;
                match &mut self.inner {
                    InnerState::Simple(s) => {
                        s.put_pointer(&self.settings, &mut self.counts, pointer, value, beh)
                    }
                    InnerState::Graphical(g) => {
                        g.put_pointer(&self.settings, &mut self.counts, pointer, value, beh)
                    }
                }
            }
            Some(time) => {
                match &mut self.inner {
                    InnerState::Simple(s) => s.put_pointer_delay(
                        /* &mut self.counts, */ pointer, time, value, /*  , beh */
                    ),
                    InnerState::Graphical(g) => g.put_pointer_delay(
                        /* &mut self.counts, */ pointer, time, value, /* , beh*/
                    ),
                }
            }
        }
    }
}
