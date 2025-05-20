use crate::ast::Exp_;
use crate::value::{Closed, Symbol_, Value_};
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

/// Node names.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Name {
    Exp_(Closed<Exp_>),
    Symbol(Symbol_),
}

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
    pub body: Exp_,
    pub space: Space,
    pub result: Option<Value_>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ThunkNode {
    pub body: Exp_,
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
    pub space: Space,
    pub time: Time,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MetaTime(BigUint);

// the value of a (named) pointer does not include its times.
pub type NamedPointer = Space;

// the full identity of a node includes a Time and MetaTime.
pub type NodeId = (Space, Time, MetaTime);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeId(pub BigUint);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Time(pub Symbol_);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Space(pub Name);

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
pub enum Error {}

pub type Res<Ok> = Result<Ok, Error>;

pub trait AdaptonState {
    fn new() -> Self
    where
        Self: Sized;
    fn put_pointer(&mut self, _pointer: NamedPointer, _value: Value_) -> Res<()>;
    fn put_symbol(&mut self, _symbol: Symbol_, _value: Value_) -> Res<NamedPointer>;
    fn get_pointer(&mut self, _pointer: NamedPointer) -> Res<Value_>;
    fn force_begin(&mut self, _pointer: NamedPointer) -> Res<()>;
    fn force_end(&mut self, _value: Value_) -> Res<()>;
}

impl AdaptonState for State {
    fn new() -> Self
    where
        Self: Sized,
    {
        State::Simple(SimpleState::new())
    }

    fn put_pointer(&mut self, pointer: NamedPointer, value: Value_) -> Res<()> {
        match self {
            Self::Simple(s) => s.put_pointer(pointer, value),
            Self::Graphical(g) => g.put_pointer(pointer, value),
        }
    }

    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<NamedPointer> {
        match self {
            Self::Simple(s) => s.put_symbol(symbol, value),
            Self::Graphical(g) => g.put_symbol(symbol, value),
        }
    }

    fn get_pointer(&mut self, _pointer: NamedPointer) -> Res<Value_> {
        todo!()
    }

    fn force_begin(&mut self, _pointer: NamedPointer) -> Res<()> {
        todo!()
    }

    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }
}

impl AdaptonState for SimpleState {
    fn new() -> Self {
        SimpleState {
            cells: HashMap::new(),
            stack: Vector::new(),
        }
    }
    fn put_symbol(&mut self, symbol: Symbol_, value: Value_) -> Res<NamedPointer> {
        // to do
        Ok(Space(Name::Symbol(symbol)))
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }
    fn force_begin(&mut self, _pointer: NamedPointer) -> Res<()> {
        todo!()
    }
    fn get_pointer(&mut self, _pointer: NamedPointer) -> Res<Value_> {
        todo!()
    }
    fn put_pointer(&mut self, _pointer: NamedPointer, _value: Value_) -> Res<()> {
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

    fn put_pointer(&mut self, _pointer: NamedPointer, _value: Value_) -> Res<()> {
        todo!()
    }
    fn put_symbol(&mut self, _symbol: Symbol_, _value: Value_) -> Res<NamedPointer> {
        todo!()
    }
    fn get_pointer(&mut self, _pointer: NamedPointer) -> Res<Value_> {
        todo!()
    }
    fn force_begin(&mut self, _pointer: NamedPointer) -> Res<()> {
        todo!()
    }
    fn force_end(&mut self, _value: Value_) -> Res<()> {
        todo!()
    }
}
