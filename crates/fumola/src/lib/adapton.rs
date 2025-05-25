use std::cmp::Ordering;
use std::convert::TryInto;

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
    TypeMismatch(u32),
    UndefinedNow(Pointer),
}

pub type Res<Ok> = Result<Ok, Error>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Navigation {
    GotoTime,
    GotoSpace,
    WithinTime,
    WithinSpace,
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
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

pub trait AdaptonState {
    fn new() -> Self
    where
        Self: Sized;
    fn put_pointer(&mut self, _pointer: Pointer, _value: Value_) -> Res<()>;
    fn put_symbol(&mut self, _symbol: Symbol_, _value: Value_) -> Res<Pointer>;
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_>;
    fn force_begin(&mut self, _pointer: Pointer) -> Res<ThunkBody>;
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

    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        match self {
            Self::Simple(s) => s.get_pointer(pointer),
            Self::Graphical(g) => g.get_pointer(pointer),
        }
    }

    fn force_begin(&mut self, pointer: Pointer) -> Res<ThunkBody> {
        match self {
            Self::Simple(s) => s.force_begin(pointer),
            Self::Graphical(g) => g.force_begin(pointer),
        }
    }

    fn force_end(&mut self, value: Value_) -> Res<()> {
        match self {
            Self::Simple(s) => s.force_end(value),
            Self::Graphical(g) => g.force_end(value),
        }
    }

    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        match self {
            Self::Simple(s) => s.navigate_begin(nav, symbol),
            Self::Graphical(g) => g.navigate_begin(nav, symbol),
        }
    }

    fn navigate_end(&mut self) -> Res<()> {
        match self {
            Self::Simple(s) => s.navigate_end(),
            Self::Graphical(g) => g.navigate_end(),
        }
    }
}

impl SimpleState {
    #[allow(dead_code)]
    fn get_cell_by_time_mut<'a>(&'a mut self, p: &Pointer) -> &'a mut CellByTime {
        let exists = self.cells.get(p) != None;
        if !exists {
            self.cells.insert(p.clone(), HashMap::new());
        }
        self.cells.get_mut(p).unwrap() // always succeeds, because of check above.
    }
    fn get_cell_by_time<'a>(&'a mut self, p: &Pointer) -> &'a CellByTime {
        let not_exists = self.cells.get(p) == None;
        if not_exists {
            assert_eq!(self.cells.insert(p.clone(), HashMap::new()), None);
        }
        self.cells.get(p).unwrap()
    }
    fn _get_cell_mut<'a>(&'a mut self, p: &Pointer, t: &Time) -> Option<&'a mut Cell> {
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
    fn pop_stack(&mut self) -> Res<()> {
        if let Some(frame) = self.stack.pop_back() {
            self.time = frame.ambient_time;
            self.thunk_pointer = frame.thunk_pointer;
            self.space = frame.ambient_space;
            Ok(())
        } else {
            Err(Error::Internal(line!()))
        }
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
        if let Some(cells_by_time) = self.cells.get(pointer) {
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
            Err(Error::Internal(line!()))
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
        let p: Pointer = self.space.apply(symbol);
        self.put_pointer(p.clone(), value)?;
        Ok(p)
    }
    fn put_pointer(&mut self, pointer: Pointer, value: Value_) -> Res<()> {
        let time = self.time.clone();
        let space = self.space.clone();
        let updated = self
            .get_cell_by_time(&pointer)
            .update(time, Self::new_cell(space, value));
        self.cells.insert(pointer, updated);
        Ok(())
    }
    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        let cell = self.get_cell(&pointer)?;
        match cell {
            Cell::NonThunk(v) => Ok(v.clone()),
            Cell::Thunk(tc) => Ok(Value::Thunk(tc.body.clone()).into()),
        }
    }
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        self.push_stack();
        match nav {
            Navigation::GotoSpace => self.space = Space::Symbol(symbol),
            Navigation::GotoTime => self.time = Time::Symbol(symbol),
            Navigation::WithinTime => self.time = self.time.apply(symbol),
            Navigation::WithinSpace => self.space = self.space.apply(symbol),
        };
        Ok(())
    }

    fn navigate_end(&mut self) -> Res<()> {
        self.pop_stack()
    }
    fn force_begin(&mut self, pointer: Pointer) -> Res<ThunkBody> {
        let cell = self.get_cell(&pointer)?.clone();
        if let Cell::Thunk(tc) = cell {
            self.push_stack();
            self.thunk_pointer = Some(pointer);
            Ok(tc.body.clone())
        } else {
            Err(Error::TypeMismatch(line!()))
        }
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        // to do -- save _value
        self.pop_stack()
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
    fn force_begin(&mut self, _pointer: Pointer) -> Res<ThunkBody> {
        todo!()
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }

    fn navigate_begin(&mut self, _nav: Navigation, _symbol: Symbol_) -> Res<()> {
        todo!()
    }

    fn navigate_end(&mut self) -> Res<()> {
        todo!()
    }
}
