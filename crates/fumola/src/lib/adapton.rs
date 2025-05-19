use crate::ast::Exp_;
use crate::value::{Closed, Symbol_, Value_};
use im_rc::{HashMap, OrdMap, Vector};
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
pub struct ThunkNode {
    pub body: Exp_,
    pub space: Space,
    pub trace: Vector<EdgeId>,
    pub result: Option<Value_>,
}

#[derive(Clone)]
pub struct Nodes(HashMap<Space, NodeByTime>);

#[derive(Clone)]
pub struct NodeByTime(OrdMap<(MetaTime, Time), Node>);

pub struct State {
    pub meta_time: MetaTime,
    pub nodes: Nodes,
    pub edges: Edges,
    pub stack: Vector<Frame>,
}

pub enum FrameKind {
    DoWithin,
    Eval(EdgeSource),
    Clean(EdgeSource),
}

pub struct Frame {
    pub kind: FrameKind,
    pub space: Space,
    pub time: Time,
    pub trace: Vector<EdgeId>,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MetaTime(BigUint);

// the value of a pointer does not include its time.
pub type PointerValue = Space;

// the target of an edge does not include the "meta time" -- it's always referencing the "latest meta time"
// ????
pub type EdgeTarget = (Space, Time);

// the source of an edge has all coordinates' components
pub type EdgeSource = (Space, Time, MetaTime);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeId(BigUint);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Time(Symbol_);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Space(Name);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Edges(HashMap<EdgeId, Edge>);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Edge {
    pub source: EdgeSource,
    pub target: EdgeTarget,
    pub action: Action,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Action {
    Force(Exp_, Value_),
    Put(Value_),
    Get(Value_),
}
