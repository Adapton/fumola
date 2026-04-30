use crate::adapton::peek_value::PeekValue;
use crate::adapton::state::{CacheState, Counts, PutBeh, Settings};
use crate::adapton::{Error, ForceBeginResult, MetaTime, Navigation, Pointer, Res, Space, Time};

use crate::ToMotoko;
use crate::value::{Symbol_, ThunkBody, Value, Value_};
use im_rc::{HashMap, Vector, vector};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Cell {
    NonThunk(Value_),
    Thunk(ThunkCell),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ThunkCell {
    pub body: ThunkBody,
    pub space: Space,
    pub result: Option<Value_>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SimpleState {
    pub space_time: SpaceTime,
    pub time_space: TimeSpace,
    pub stack: Vector<SimpleFrame>,
    pub time: Time,
    pub space: Space,
    pub thunk_pointer: Option<Pointer>, // None ==> stack(i).thunk_pointer == None, for all i.
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SimpleFrame {
    pub ambient_space: Space,
    pub ambient_time: Time,
    pub thunk_pointer: Option<Pointer>, // None ==> stack(i).thunk_pointer == None, for all i.
}

pub type SpaceTime = HashMap<Space, CellByTime>;
pub type CellByTime = HashMap<Time, Cell>;

pub type TimeSpace = HashMap<Time, CellsBySpace>;
pub type CellsBySpace = HashMap<Space, Cell>;

impl Cell {
    pub fn is_thunk(&self) -> bool {
        match self {
            Cell::Thunk(_) => true,
            Cell::NonThunk(_) => false,
        }
    }
    pub fn get_value(&self) -> Res<Value_> {
        match self {
            Cell::NonThunk(v) => Ok(v.clone()),
            Cell::Thunk(tc) => Ok(Value::Thunk(tc.body.clone()).into()),
        }
    }
    pub fn set_cache_value(&mut self, v: Value_) -> Res<()> {
        match self {
            Cell::NonThunk(_) => Err(Error::Unreachable),
            Cell::Thunk(t) => {
                t.result = Some(v);
                Ok(())
            }
        }
    }
}

impl SimpleState {
    #[allow(dead_code)]
    fn get_cell_by_time_mut<'a>(&'a mut self, p: &Pointer) -> &'a mut CellByTime {
        let exists = self.space_time.get(p) != None;
        if !exists {
            self.space_time.insert(p.clone(), HashMap::new());
        }
        self.space_time.get_mut(p).unwrap() // always succeeds, because of check above.
    }
    fn get_cell_by_time<'a>(&'a mut self, p: &Pointer) -> &'a CellByTime {
        let not_exists = self.space_time.get(p) == None;
        if not_exists {
            assert_eq!(self.space_time.insert(p.clone(), HashMap::new()), None);
        }
        self.space_time.get(p).unwrap()
    }
    fn get_cell_by_space<'a>(&'a mut self, t: &Time) -> &'a CellsBySpace {
        let not_exists = self.time_space.get(t) == None;
        if not_exists {
            assert_eq!(self.time_space.insert(t.clone(), HashMap::new()), None);
        }
        self.time_space.get(t).unwrap()
    }
    fn get_cell_mut<'a>(&'a mut self, p: &Pointer, t: &Time) -> Option<&'a mut Cell> {
        self.get_cell_by_time_mut(p).get_mut(t)
    }
    fn new_cell(space: Space, value: Value_) -> Cell {
        match &*value {
            Value::Thunk(e) => Cell::Thunk(ThunkCell {
                body: e.clone(),
                space: space,
                result: None,
            }),
            _ => Cell::NonThunk(value),
        }
    }
    fn push_stack(&mut self) {
        self.stack.push_back(SimpleFrame {
            ambient_space: self.space.clone(),
            ambient_time: self.time.clone(),
            thunk_pointer: self.thunk_pointer.clone(),
        });
    }
    fn pop_stack(&mut self) -> Res<SimpleFrame> {
        if let Some(frame) = self.stack.pop_back() {
            let r = frame.clone();
            self.time = frame.ambient_time;
            self.thunk_pointer = frame.thunk_pointer;
            self.space = frame.ambient_space;
            Ok(r)
        } else {
            Err(Error::Internal(line!()))
        }
    }

    fn undelay(&mut self) -> Res<()> {
        let delayed_cells = self
            .time_space
            .get(&self.time)
            .map(|x| x.clone())
            .unwrap_or(HashMap::new());
        self.time_space.insert(self.time.clone(), HashMap::new());
        for (pointer, cell) in delayed_cells.iter() {
            let cell_value = cell.get_value()?;
            let mut dummy: Counts = Counts::new();
            self.put_pointer(&mut dummy, pointer.clone(), cell_value, PutBeh::Undelay)?;
        }
        Ok(())
    }

    // get_cell --
    // find the cellsbytime.
    // does the exact time exist?
    // if so, use it.
    // if not, do a linear scan and find the nearest time, if any exist before the current time.
    //
    // (doing a log time search is possible in the future: would require an ordered representation,
    // and the boilerplate for Serialize/Deserialize for it)
    fn get_cell<'a>(&'a self, pointer: &Pointer) -> Res<&'a Cell> {
        if let Some(cells_by_time) = self.space_time.get(pointer) {
            if let Some(cell) = cells_by_time.get(&self.time) {
                Ok(cell)
            } else {
                let mut time = None;
                let mut cell = None;
                for (time_, cell_) in cells_by_time.iter() {
                    if &self.time >= time_ && time == None {
                        // Initialize with a viable cell's time.
                        time = Some(time_);
                        cell = Some(cell_);
                    } else if let Some(t) = time {
                        // This cell's time is closer to the current moment.
                        // It shadows the earlier cell we found.
                        if t < time_ && time_ <= &self.time {
                            time = Some(time_);
                            cell = Some(cell_);
                        }
                    }
                }
                match cell {
                    Some(c) => Ok(c),
                    None => Err(Error::UndefinedNow(pointer.clone())),
                }
            }
        } else {
            Err(Error::DanglingPointer(pointer.clone()))
        }
    }
}

impl CacheState for SimpleState {
    fn new() -> Self {
        SimpleState {
            time: Time::Now,
            space: Space::Here,
            space_time: HashMap::new(),
            time_space: HashMap::new(),
            stack: Vector::new(),
            thunk_pointer: None,
        }
    }

    fn put_symbol(&mut self, counts: &mut Counts, symbol: Symbol_, value: Value_) -> Res<Pointer> {
        let p: Pointer = self.space.apply(symbol);
        self.put_pointer(counts, p.clone(), value, PutBeh::Put)?;
        Ok(p)
    }
    fn put_pointer(
        &mut self,
        counts: &mut Counts,
        pointer: Pointer,
        value: Value_,
        beh: PutBeh,
    ) -> Res<()> {
        let time = self.time.clone();
        let space = self.space.clone();
        let cells = self.get_cell_by_time(&pointer);
        let cells_orig_len = cells.len();
        let new_cell = Self::new_cell(space, value);
        let is_thunk = new_cell.is_thunk();
        let cells = cells.update(time, new_cell);
        if beh == PutBeh::Put {
            if cells.len() > cells_orig_len {
                counts.cells += 1;
                if is_thunk {
                    counts.thunk_cells += 1
                } else {
                    counts.non_thunk_cells += 1;
                }
            }
        };
        self.space_time.insert(pointer, cells);
        Ok(())
    }
    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        let cell = self.get_cell(&pointer)?;
        cell.get_value()
    }
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        self.push_stack();
        match &nav {
            Navigation::GotoSpace => self.space = Space::Symbol(symbol),
            Navigation::WithinSpace => self.space = self.space.apply(symbol),
            Navigation::GotoTime => self.time = Time::Symbol(symbol),
            Navigation::WithinTime => self.time = self.time.apply(symbol),
        };
        match &nav {
            Navigation::GotoTime | Navigation::WithinTime => self.undelay()?,
            _ => (),
        }
        Ok(())
    }

    fn navigate_end(&mut self) -> Res<()> {
        let time0 = self.time.clone();
        self.pop_stack()?;
        if self.time != time0 {
            self.undelay()?;
        };
        Ok(())
    }
    fn force_begin(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        pointer: Pointer,
    ) -> Res<ForceBeginResult> {
        let cell = self.get_cell(&pointer)?.clone();
        if let Cell::Thunk(tc) = cell {
            if let Some(cache_value) = tc.result
                && !settings.force_begin_always_misses
            {
                counts.force_begin_cache_hit += 1;
                Ok(ForceBeginResult::CacheHit(MetaTime::new(), cache_value))
            } else {
                counts.force_begin_cache_miss += 1;
                self.push_stack();
                self.thunk_pointer = Some(pointer);
                self.space = tc.space.clone();
                Ok(ForceBeginResult::CacheMiss(tc.body.clone()))
            }
        } else {
            Err(Error::TypeMismatch(line!()))
        }
    }
    fn force_end(&mut self, settings: &Settings, value: Value_) -> Res<()> {
        let cell = self
            .get_cell_mut(&self.thunk_pointer.clone().unwrap(), &self.now())
            .ok_or(Error::Unreachable)?;
        if !settings.force_end_forgets_result {
            cell.set_cache_value(value)?;
        }
        let _fr = self.pop_stack()?;
        Ok(())
    }

    fn now(&self) -> Time {
        self.time.clone()
    }

    fn here(&self) -> Space {
        self.space.clone()
    }

    fn put_pointer_delay(&mut self, pointer: Pointer, time: Time, value: Value_) -> Res<()> {
        let space = self.space.clone();
        let updated = self
            .get_cell_by_space(&time)
            .update(pointer, Self::new_cell(space, value));
        self.time_space.insert(time, updated);
        Ok(())
    }

    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer> {
        let pointer = self.space.apply(symbol);
        self.put_pointer_delay(pointer.clone(), time, value)?;
        Ok(pointer)
    }

    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>> {
        match self.get_cell(&pointer) {
            Ok(c) => Ok(Some(c.get_value()?)),
            Err(_) => Ok(None),
        }
    }

    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_> {
        use crate::adapton::peek_value::PeekValue;
        match self.get_cell(&pointer) {
            Ok(c) => Ok(Some(c.clone()).into_value_()),
            Err(_) => None::<Value_>
                .to_motoko_shared()
                .map_err(|_| Error::Unreachable),
        }
    }

    fn peek_events(&mut self) -> Res<Value_> {
        let empty: Vector<Value_> = vector!();
        Ok(empty.into_value_())
    }
}
