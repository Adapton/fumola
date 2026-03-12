use crate::adapton::state::{CacheState, Counts, Settings};
use crate::adapton::{ForceBeginResult, Navigation, Pointer, Res, Space, Time};
use crate::value::{Symbol_, ThunkBody, Value_};
use fumola_syntax::ast::Exp_;
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

// -----------------------------------------------------------------------------------------
// Graphical

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Node {
    NonThunk(Value_),
    Thunk(ThunkNode),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ThunkNode {
    pub body: ThunkBody,
    pub space: Space,
    pub trace: Vector<EdgeId>,
    pub result: Option<Value_>,
}

pub type Nodes = HashMap<Space, NodeByTime>;
pub type NodeByTime = HashMap<(Time, MetaTime), Node>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphicalState {
    pub meta_time: MetaTime,
    pub nodes: Nodes,
    pub edges: Edges,
    pub stack: Vector<Frame>,
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

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MetaTime(BigUint);

// the full identity of a node includes a Time and MetaTime.
pub type NodeId = (Space, Time, MetaTime);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeId(pub BigUint);

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

impl CacheState for GraphicalState {
    fn new() -> Self {
        GraphicalState {
            meta_time: MetaTime(BigUint::from(0 as usize)),
            nodes: HashMap::new(),
            edges: HashMap::new(),
            stack: Vector::new(),
            space: Space::Here,
            time: Time::Now,
            thunk_pointer: None,
        }
    }
    fn put_pointer(&mut self, _counts: &mut Counts, _pointer: Pointer, _value: Value_) -> Res<()> {
        todo!()
    }
    fn put_symbol(
        &mut self,
        _counts: &mut Counts,
        _symbol: Symbol_,
        _value: Value_,
    ) -> Res<Pointer> {
        todo!()
    }
    fn get_pointer(&mut self, _pointer: Pointer) -> Res<Value_> {
        todo!()
    }
    fn force_begin(
        &mut self,
        _settings: &Settings,
        _counts: &mut Counts,
        _pointer: Pointer,
    ) -> Res<ForceBeginResult> {
        todo!()
    }
    fn force_end(&mut self, _settings: &Settings, _value: Value_) -> Res<()> {
        todo!()
    }

    fn navigate_begin(&mut self, _nav: Navigation, _symbol: Symbol_) -> Res<()> {
        todo!()
    }

    fn navigate_end(&mut self) -> Res<()> {
        todo!()
    }

    fn now(&self) -> Time {
        todo!()
    }

    fn here(&self) -> Space {
        todo!()
    }

    fn put_pointer_delay(&mut self, _pointer: Pointer, _time: Time, _value: Value_) -> Res<()> {
        todo!()
    }

    fn put_symbol_delay(&mut self, _symbol: Symbol_, _time: Time, _value: Value_) -> Res<Pointer> {
        todo!()
    }

    fn poke(&mut self, _pointer: Pointer, _time: Option<Time>, _value: Value_) -> Res<()> {
        todo!()
    }
    fn peek(&mut self, _pointer: Pointer) -> Res<Option<Value_>> {
        todo!()
    }

    fn peek_cell(&mut self, _pointer: Pointer) -> Res<Value_> {
        todo!()
    }
}
