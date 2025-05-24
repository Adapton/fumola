use crate::ast::Exp_;
use crate::value::{Closed, Symbol, Symbol_, ThunkBody, Value, Value_};
use crate::Shared;
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Node {
    NonThunk(Value_),
    Thunk(ThunkNode),
}

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

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ThunkNode {
    pub body: ThunkBody,
    pub space: Space,
    pub trace: Vector<EdgeId>,
    pub result: Option<Value_>,
}

pub type Cells = HashMap<Space, CellByTime>;
pub type CellByTime = HashMap<Time, Cell>;

pub type Nodes = HashMap<Space, NodeByTime>;
pub type NodeByTime = HashMap<(Time, MetaTime), Node>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphicalState {
    pub meta_time: MetaTime,
    pub nodes: Nodes,
    pub edges: Edges,
    pub stack: Vector<Frame>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SimpleState {
    pub cells: Cells,
    pub stack: Vector<SimpleFrame>,
    pub time: Time,
    pub space: Space,
    pub thunk_pointer: Option<Pointer>, // None ==> stack(i).thunk_pointer == None, for all i.
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FrameKind {
    DoWithin,
    Eval(NodeId),
    Clean(NodeId),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Frame {
    pub kind: FrameKind,
    pub space: Space,
    pub time: Time,
    pub trace: Vector<EdgeId>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SimpleFrame {
    pub ambient_space: Space,
    pub ambient_time: Time,
    pub thunk_pointer: Option<Pointer>, // None ==> stack(i).thunk_pointer == None, for all i.
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MetaTime(BigUint);

// the value of a (named) pointer does not include its times.
pub type Pointer = Space;

// the full identity of a node includes a Time and MetaTime.
pub type NodeId = (Space, Time, MetaTime);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeId(pub BigUint);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Time {
    Symbol(Symbol_),
    Now,
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
}

/// Node names.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Space {
    Symbol(Symbol_),
    Exp_(Option<Symbol_>, Closed<Exp_>),
    Here,
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
}

pub type Edges = HashMap<EdgeId, Edge>;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Edge {
    pub source: NodeId,
    pub target: NodeId,
    pub action: Action,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Action {
    Force(Exp_, Value_),
    Put(Value_),
    Get(Value_),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum State {
    Simple(SimpleState),
    Graphical(GraphicalState),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub enum Error {
    Internal(u32),
}

pub type Res<Ok> = Result<Ok, Error>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Navigation {
    GotoTime,
    GotoSpace,
    WithinTime,
    WithinSpace,
}

pub trait AdaptonState {
    fn new() -> Self
    where
        Self: Sized;
    fn put_pointer(&mut self, _pointer: Pointer, _value: Value_) -> Res<()>;
    fn put_symbol(&mut self, _symbol: Symbol_, _value: Value_) -> Res<Pointer>;
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_>;
    fn force_begin(&mut self, _pointer: Pointer) -> Res<()>;
    fn force_end(&mut self, _value: Value_) -> Res<()>;
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()>;
    fn navigate_end(&mut self) -> Res<()>;
}

impl AdaptonState for State {
    fn new() -> Self
    where
        Self: Sized,
    {
        State::Simple(SimpleState::new())
    }

    fn put_pointer(&mut self, pointer: Pointer, value: Value_) -> Res<()> {
        match self {
            Self::Simple(s) => s.put_pointer(pointer, value),
            Self::Graphical(g) => g.put_pointer(pointer, value),
        }
    }

    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<Pointer> {
        match self {
            Self::Simple(s) => s.put_symbol(symbol, value),
            Self::Graphical(g) => g.put_symbol(symbol, value),
        }
    }

    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_> {
        todo!()
    }

    fn force_begin(&mut self, _pointer: Pointer) -> Res<()> {
        todo!()
    }

    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }

    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        match self {
            Self::Simple(s) => s.navigate_begin(nav, symbol),
            Self::Graphical(g) => g.navigate_begin(nav, symbol),
        }
    }

    fn navigate_end(&mut self) -> Res<()> {
        todo!()
    }
}

impl SimpleState {
    fn get_cell_by_time_mut<'a>(&'a mut self, p: &Pointer) -> &'a mut CellByTime {
        let exists = self.cells.get(p) != None;
        if !exists {
            self.cells.insert(p.clone(), HashMap::new());
        }
        self.cells.get_mut(p).unwrap() // always succeeds, because of check above.
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
}

impl AdaptonState for SimpleState {
    fn new() -> Self {
        SimpleState {
            time: Time::Now,
            space: Space::Here,
            cells: HashMap::new(),
            stack: Vector::new(),
            thunk_pointer: None,
        }
    }
    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<Pointer> {
        // to do -- include current space within pointer as well
        let p: Pointer = Space::Symbol(symbol);
        self.put_pointer(p.clone(), value)?;
        Ok(p)
    }
    fn put_pointer(&mut self, pointer: Pointer, value: Value_) -> Res<()> {
        let time = self.time.clone();
        let space = self.space.clone();
        let cells = self.get_cell_by_time_mut(&pointer);
        cells.insert(time, Self::new_cell(space, value));
        Ok(())
    }
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_> {
        todo!()
        // find the cellsbytime.
        // does the exact time exist?
        // if so, use it.
        // if not, do a linear scan and find the nearest time, if any exist before the current time.
    }
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        self.stack.push_back(SimpleFrame {
            ambient_space: self.space.clone(),
            ambient_time: self.time.clone(),
            thunk_pointer: self.thunk_pointer.clone(),
        });
        match nav {
            Navigation::GotoSpace => self.space = Space::Symbol(symbol),
            Navigation::GotoTime => self.time = Time::Symbol(symbol),
            Navigation::WithinTime => self.time = self.time.apply(symbol),
            Navigation::WithinSpace => self.space = self.space.apply(symbol),
        };
        Ok(())
    }

    fn navigate_end(&mut self) -> Res<()> {
        if let Some(frame) = self.stack.pop_back() {
            self.time = frame.ambient_time;
            self.thunk_pointer = frame.thunk_pointer;
            self.space = frame.ambient_space;
            Ok(())
        } else {
            Err(Error::Internal(line!()))
        }
    }
    fn force_begin(&mut self, _pointer: Pointer) -> Res<()> {
        // get the thunk body.
        // save current time and space on stack.
        // current space becomes thunk pointer's space.
        // return thunk body.
        todo!()
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        // pop ambient time and space from stack and restore them.
        // set the value of the thunk cell, for diagnostics (not memoization-based caching).
        todo!()
    }
}

impl AdaptonState for GraphicalState {
    fn new() -> Self {
        GraphicalState {
            meta_time: MetaTime(BigUint::from(0 as usize)),
            nodes: HashMap::new(),
            edges: HashMap::new(),
            stack: Vector::new(),
        }
    }

    // todo -- use result monad.
    // todo -- introduce Result -- interruptions vs normal return types.

    fn put_pointer(&mut self, _pointer: Pointer, _value: Value_) -> Res<()> {
        todo!()
    }
    fn put_symbol(&mut self, _symbol: Symbol_, _value: Value_) -> Res<Pointer> {
        todo!()
    }
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_> {
        todo!()
    }
    fn force_begin(&mut self, _pointer: Pointer) -> Res<()> {
        todo!()
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }

    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        todo!()
    }

    fn navigate_end(&mut self) -> Res<()> {
        todo!()
    }
}
