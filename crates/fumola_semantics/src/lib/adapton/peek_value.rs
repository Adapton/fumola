use fumola_syntax::ast::{Id, Mut};
use fumola_syntax::shared::Share;

use crate::adapton::graphical::{
    Action, Edge, EdgeHistoryItem, EdgeId, Event, EventHistoryItem, History, Node, NodeHistoryItem,
    NodeInfo, ThunkNode,
};
use crate::adapton::simple::{Cell, ThunkCell};
use crate::adapton::{MetaTime, Space, Time};
use crate::{Value, Value_, value::ThunkBody};
use im_rc::{Vector, vector};

pub trait PeekValue {
    fn into_value_(self) -> Value_;
}

impl PeekValue for Cell {
    fn into_value_(self) -> Value_ {
        match self {
            Cell::NonThunk(nt) => Value::Variant(Id::new("nonThunk".to_string()), Some(nt)).share(),
            Cell::Thunk(thunk_cell) => {
                let pv = thunk_cell.into_value_();
                Value::Variant(Id::new("thunk_".to_string()), Some(pv)).share()
            }
        }
    }
}

impl PeekValue for ThunkCell {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("body", self.body.into_value_()),
                ("space", self.space.into_value_()),
                ("result", self.result.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

impl PeekValue for Node {
    fn into_value_(self) -> Value_ {
        match self {
            Node::NonThunk(nt) => Value::Variant(Id::new("nonThunk".to_string()), Some(nt)).share(),
            Node::Thunk(thunk_node) => {
                let pv = thunk_node.into_value_();
                Value::Variant(Id::new("thunk_".to_string()), Some(pv)).share()
            }
        }
    }
}

impl PeekValue for NodeInfo {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("node", self.node.into_value_()),
                ("nodeId", self.node_id.into_value_()),
                ("incomingEdges", self.incoming_edges.into_value_()),
                ("outgoingEdges", self.outgoing_edges.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

impl<X: PeekValue, Y: PeekValue> PeekValue for (X, Y) {
    fn into_value_(self) -> Value_ {
        Value::Tuple(vector!(self.0.into_value_(), self.1.into_value_())).share()
    }
}

impl<X: PeekValue, Y: PeekValue, Z: PeekValue> PeekValue for (X, Y, Z) {
    fn into_value_(self) -> Value_ {
        Value::Tuple(vector!(
            self.0.into_value_(),
            self.1.into_value_(),
            self.2.into_value_()
        ))
        .share()
    }
}

impl PeekValue for MetaTime {
    fn into_value_(self) -> Value_ {
        Value::Nat(self.0.clone()).share()
    }
}

impl PeekValue for Edge {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("source", self.source.into_value_()),
                ("target", self.target.into_value_()),
                ("action", self.action.into_value_()),
                ("metaTimes", self.meta_times.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

fn variant(tag: &str, payload: Value_) -> Value_ {
    Value::Variant(Id::new(tag.to_string()), Some(payload)).share()
}

impl PeekValue for Action {
    fn into_value_(self) -> Value_ {
        match self {
            Action::Force(closed_exp, v) => variant(
                "force_",
                Value::Tuple(vector![Value::Thunk(closed_exp).share(), v]).share(),
            ),
            Action::Put(v) => variant("put", v),
            Action::Get(v) => variant("get", v),
        }
    }
}

impl PeekValue for Value_ {
    fn into_value_(self) -> Value_ {
        self
    }
}
impl PeekValue for ThunkNode {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("body", self.body.into_value_()),
                ("space", self.space.into_value_()),
                ("trace", self.trace.into_value_()),
                ("result", self.result.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}
impl PeekValue for Space {
    fn into_value_(self) -> Value_ {
        Value::AdaptonSpace(self).share()
    }
}
impl<T: PeekValue + Clone> PeekValue for Vector<T> {
    fn into_value_(self) -> Value_ {
        Value::Array(
            Mut::Const,
            self.iter().map(|x| x.clone().into_value_()).collect(),
        )
        .share()
    }
}
impl PeekValue for EdgeId {
    fn into_value_(self) -> Value_ {
        Value::Variant(
            Id::new("edgeId".to_owned()),
            Some(Value::Nat(self.0).share()),
        )
        .share()
    }
}
impl PeekValue for Time {
    fn into_value_(self) -> Value_ {
        Value::AdaptonTime(self).share()
    }
}

impl<T: PeekValue> PeekValue for Option<T> {
    fn into_value_(self) -> Value_ {
        match self {
            Some(v) => Value::Option(v.into_value_()),
            None => Value::Null,
        }
        .share()
    }
}

impl PeekValue for ThunkBody {
    fn into_value_(self) -> Value_ {
        Value::Thunk(self).share()
    }
}

impl PeekValue for EventHistoryItem {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("metaTime", self.meta_time.into_value_()),
                ("event", self.event.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

impl PeekValue for Event {
    fn into_value_(self) -> Value_ {
        match self {
            Event::AddNode(v) => variant("addNode", v.into_value_()),
            Event::AddEdge(edge_id) => variant("addEdge", edge_id.into_value_()),
            Event::RemoveEdge(edge_id) => variant("removeEdge", edge_id.into_value_()),
            Event::ForceBegin(node_id) => variant("forceBegin", node_id.into_value_()),
            Event::ForceEnd(node_id, edge_id) => {
                variant("forceEnd", (node_id, edge_id).into_value_())
            }
        }
    }
}

impl PeekValue for History {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("events", self.events.into_value_()),
                ("nodes", self.nodes.into_value_()),
                ("edges", self.edges.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

impl PeekValue for NodeHistoryItem {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("metaTime", self.meta_time.into_value_()),
                ("nodeId", self.node_id.into_value_()),
                ("node", self.node.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}

impl PeekValue for EdgeHistoryItem {
    fn into_value_(self) -> Value_ {
        Value::object_from(
            [
                ("metaTime", self.meta_time.into_value_()),
                ("edgeId", self.edge_id.into_value_()),
                ("edge", self.edge.into_value_()),
            ]
            .iter(),
        )
        .share()
    }
}
