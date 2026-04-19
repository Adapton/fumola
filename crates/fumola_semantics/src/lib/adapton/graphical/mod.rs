use crate::ToMotoko;
use crate::Value;
use crate::adapton::MetaTime;
use crate::adapton::peek_value::PeekValue;
use crate::adapton::state::PutBeh;
use crate::adapton::state::{CacheState, Counts, Settings};
use crate::adapton::{Error, ForceBeginResult, Navigation, Pointer, Res, Space, Time};
use crate::value::{Symbol_, ThunkBody, Value_};
use im_rc::vector;
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Event {
    AddNode(NodeId),
    AddEdge(EdgeId),
    RemoveEdge(EdgeId),
    ForceBegin(NodeId),
    ForceEnd(NodeId, EdgeId),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EventItem {
    pub meta_time: MetaTime,
    pub event: Event,
}

/// Node and its incident edges
pub struct NodeInfo {
    pub node_id: NodeId,
    pub node: Node,
    /// The edges that target this node, not ordered.
    pub incoming_edges: Vector<(EdgeId, Edge)>,
    /// The edges where this node is the source, in node's trace order.
    pub outgoing_edges: Vector<(EdgeId, Edge)>,
}

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
    pub begin: Option<MetaTime>,
    pub result: Option<(MetaTime, Value_)>,
}

pub type SpaceTime = HashMap<Space, NodesByTime>;
pub type NodesByTime = HashMap<Time, NodesByMetaTime>;
pub type NodesByMetaTime = HashMap<MetaTime, Node>;

pub type TimeSpace = HashMap<Time, NodesBySpace>;
pub type NodesBySpace = HashMap<(Space, MetaTime), Node>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphicalState {
    pub meta_time: MetaTime,
    pub next_edge_id: EdgeId,
    pub space_time: SpaceTime,
    pub time_space: TimeSpace,
    pub edges: EdgesByEdgeId,
    pub edges_by_target: EdgeIdsByTarget,
    pub stack: Vector<Frame>,
    // Cursor state: current_node, trace, space, time.
    pub current_node: NodeId, // None ==> stack(i).thunk_pointer == None, for all i.
    pub trace: Vector<EdgeId>,
    pub space: Space,
    pub time: Time,
    pub events: Vector<EventItem>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FrameKind {
    Navigation(Navigation),
    Force(NodeId),
    Clean(NodeId),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Frame {
    pub kind: FrameKind,
    pub meta_time: MetaTime,
    pub ambient_space: Space,
    pub ambient_time: Time,
    pub current_node: NodeId,
    pub trace: Vector<EdgeId>,
}

// the full identity of a node includes a Time and MetaTime.
pub type NodeId = (Space, Time, MetaTime);

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeId(pub BigUint);

pub type EdgesByEdgeId = HashMap<EdgeId, Edge>;
pub type EdgeIdsByTarget = HashMap<NodeId, Vector<EdgeId>>;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Edge {
    pub source: NodeId,
    pub target: NodeId,
    pub action: Action,
    pub meta_times: (MetaTime, MetaTime),
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Action {
    Force(ThunkBody, Value_),
    Put(Value_),
    Get(Value_),
}

use std::ops::Add;

impl EdgeId {
    pub fn next(&self) -> Self {
        EdgeId(self.0.clone().add(BigUint::from(1u64)))
    }
}

impl Node {
    pub fn is_thunk(&self) -> bool {
        match self {
            Node::Thunk(_) => true,
            Node::NonThunk(_) => false,
        }
    }
    pub fn get_value(&self) -> Res<Value_> {
        match self {
            Node::NonThunk(v) => Ok(v.clone()),
            Node::Thunk(tc) => Ok(Value::Thunk(tc.body.clone()).into()),
        }
    }
    pub fn force_action(&mut self) -> Res<Action> {
        match self {
            Node::NonThunk(_) => Err(Error::Unreachable),
            Node::Thunk(t) => Ok(Action::Force(t.body.clone(), t.result.clone().unwrap().1)),
        }
    }
    pub fn set_cache_value(
        &mut self,
        meta_time: MetaTime,
        v: Value_,
        trace: Vector<EdgeId>,
    ) -> Res<()> {
        match self {
            Node::NonThunk(_) => Err(Error::Unreachable),
            Node::Thunk(t) => {
                t.result = Some((meta_time, v));
                t.trace = trace;
                Ok(())
            }
        }
    }
}

impl GraphicalState {
    fn event(&mut self, ev: Event) {
        self.events.push_back(EventItem {
            meta_time: self.meta_time.clone(),
            event: ev,
        });
    }

    #[allow(dead_code)]
    fn get_node_by_time_mut<'a>(&'a mut self, p: &Pointer) -> &'a mut NodesByTime {
        let exists = self.space_time.get(p) != None;
        if !exists {
            self.space_time.insert(p.clone(), HashMap::new());
        }
        self.space_time.get_mut(p).unwrap() // always succeeds, because of check above.
    }
    fn get_node_by_space<'a>(&'a mut self, t: &Time) -> &'a NodesBySpace {
        let not_exists = self.time_space.get(t) == None;
        if not_exists {
            assert_eq!(self.time_space.insert(t.clone(), HashMap::new()), None);
        }
        self.time_space.get(t).unwrap()
    }
    fn root_node() -> NodeId {
        (Space::Here, Time::Now, MetaTime(BigUint::from(0u64)))
    }
    fn get_node_mut<'a>(
        &'a mut self,
        p: &Pointer,
        t: &Time,
    ) -> Option<(&'a MetaTime, &'a mut Node)> {
        let res = self.get_node_by_time_mut(p).get_mut(t);
        if res == None {
            return None;
        };
        let res = res.unwrap();
        let mut latest: Option<(&MetaTime, &'a mut Node)> = None;
        for (m, n) in res.iter_mut() {
            if let Some((m0, _)) = latest {
                if m0.0 < m.0 {
                    latest = Some((m, n));
                }
            } else {
                latest = Some((m, n));
            }
        }
        latest
    }
    fn current_node(&self) -> NodeId {
        self.current_node.clone()
    }
    fn new_node(space: Space, value: Value_) -> Node {
        match &*value {
            Value::Thunk(e) => Node::Thunk(ThunkNode {
                body: e.clone(),
                space: space,
                begin: None,
                result: None,
                trace: Vector::new(),
            }),
            _ => Node::NonThunk(value),
        }
    }
    fn new_edge_helper(
        &mut self,
        action: Action,
        target: NodeId,
        meta_time_begin: Option<MetaTime>,
    ) -> Res<EdgeId> {
        let id = self.next_edge_id.clone();
        self.next_edge_id = self.next_edge_id.next();
        self.trace.push_back(id.clone());
        let source = self.current_node();
        match self.edges_by_target.get_mut(&target) {
            Some(edge_ids) => {
                if !edge_ids.contains(&id) {
                    edge_ids.push_back(id.clone());
                }
            }
            None => {
                self.edges_by_target
                    .insert(target.clone(), vector!(id.clone()));
            }
        };
        let meta_time = self.meta_time.clone();
        self.edges.insert(
            id.clone(),
            Edge {
                source,
                target,
                action,
                meta_times: MetaTime::pair(meta_time_begin, meta_time),
            },
        );
        Ok(id)
    }
    fn new_edge_to_pointer(
        &mut self,
        action: Action,
        target: Pointer,
        meta_time_begin: Option<MetaTime>,
    ) -> Res<EdgeId> {
        let time = self.time.clone();
        let meta_time = self.meta_time.clone();
        let edge_id = self.new_edge_helper(action, (target, time, meta_time), meta_time_begin)?;
        Ok(edge_id)
    }
    fn push_stack(&mut self, frame_kind: FrameKind) {
        let saved_trace = self.trace.clone(); // to do -- do a move.
        self.trace = vector!();
        self.stack.push_back(Frame {
            kind: frame_kind,
            trace: saved_trace,
            meta_time: self.meta_time.clone(),
            ambient_space: self.space.clone(),
            ambient_time: self.time.clone(),
            current_node: self.current_node.clone(),
        });
    }
    fn pop_stack(&mut self) -> Res<Frame> {
        if let Some(mut frame) = self.stack.pop_back() {
            let r = frame.clone();
            self.time = frame.ambient_time;
            self.current_node = frame.current_node;
            self.space = frame.ambient_space;
            frame.trace.append(self.trace.clone());
            self.trace = frame.trace;
            Ok(r)
        } else {
            Err(Error::Internal(line!()))
        }
    }

    fn undelay(&mut self) -> Res<()> {
        let delayed_nodes = self
            .time_space
            .get(&self.time)
            .map(|x| x.clone())
            .unwrap_or(HashMap::new());
        self.time_space.insert(self.time.clone(), HashMap::new());
        for ((pointer, _meta_time), node) in delayed_nodes.iter() {
            let node_value = node.get_value()?;
            let mut dummy: Counts = Counts::new();
            self.put_pointer(&mut dummy, pointer.clone(), node_value, PutBeh::Undelay)?;
        }
        Ok(())
    }

    fn get_incoming_edges<'a>(&'a self, pointer: &Pointer) -> Res<Vector<(EdgeId, Edge)>> {
        let (nid, _) = self.get_node(pointer)?;

        let edge_ids = self.edges_by_target.get(&nid).unwrap_or(&vector!()).clone();
        Ok(edge_ids
            .into_iter()
            .map(|edge_id| {
                let edge = self.edges.get(&edge_id).unwrap();
                (edge_id.clone(), edge.clone())
            })
            .collect())
    }
    fn get_outgoing_edges<'a>(&'a self, pointer: &Pointer) -> Res<Vector<(EdgeId, Edge)>> {
        let (_nid, node) = self.get_node(pointer)?;
        match node {
            Node::NonThunk(_) => Ok(vector!()),
            Node::Thunk(thunk_node) => {
                let edges = thunk_node
                    .trace
                    .clone()
                    .into_iter()
                    .map(|edge_id| {
                        let edge = self.edges.get(&edge_id).unwrap();
                        (edge_id.clone(), edge.clone())
                    })
                    .collect();
                Ok(edges)
            }
        }
    }

    // get_node --
    // find the nodesbytime.
    // does the exact time exist?
    // if so, use it.
    // if not, do a linear scan and find the nearest time, if any exist before the current time.
    //
    // (doing a log time search is possible in the future: would require an ordered representation,
    // and the boilerplate for Serialize/Deserialize for it)
    fn get_node<'a>(&'a self, pointer: &Pointer) -> Res<(NodeId, &'a Node)> {
        if let Some(nodes_by_time) = self.space_time.get(pointer) {
            if let Some(nodes) = nodes_by_time.get(&self.time) {
                let mut latest: Option<(&MetaTime, &'a Node)> = None;
                for (m, n) in nodes.iter() {
                    if let Some((m0, _)) = latest {
                        if m0.0 <= m.0 {
                            latest = Some((m, n));
                        }
                    } else {
                        latest = Some((m, n))
                    }
                }
                let latest = latest.unwrap();
                let node_id = (pointer.clone(), self.time.clone(), latest.0.clone());
                Ok((node_id, latest.1))
            } else {
                let mut time = None;
                let mut meta_time = None;
                let mut node = None;
                for (time_, nodes_) in nodes_by_time.iter() {
                    for (meta_time_, node_) in nodes_.iter() {
                        if &self.time >= time_ && time == None {
                            // Initialize with a viable node's time.
                            time = Some(time_);
                            meta_time = Some(meta_time_);
                            node = Some(node_);
                        } else if let Some(t) = time {
                            // This node's time is closer to the current moment.
                            // It shadows the earlier node we found.
                            if t < time_ && time_ <= &self.time {
                                time = Some(time_);
                                meta_time = Some(meta_time_);
                                node = Some(node_);
                            }
                        }
                    }
                }
                match node {
                    Some(node) => Ok((
                        (
                            pointer.clone(),
                            time.unwrap().clone(),
                            meta_time.unwrap().clone(),
                        ),
                        node,
                    )),
                    None => Err(Error::UndefinedNow(pointer.clone())),
                }
            }
        } else {
            Err(Error::DanglingPointer(pointer.clone()))
        }
    }
}

impl CacheState for GraphicalState {
    fn new() -> Self {
        GraphicalState {
            next_edge_id: EdgeId(BigUint::from(1001 as usize)),
            meta_time: MetaTime(BigUint::from(0 as usize)),
            space_time: HashMap::new(),
            time_space: HashMap::new(),
            edges: HashMap::new(),
            edges_by_target: HashMap::new(),
            trace: Vector::new(),
            stack: Vector::new(),
            space: Space::Here,
            time: Time::Now,
            current_node: Self::root_node(),
            events: Vector::new(),
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
        self.meta_time.incr();
        let space = self.space.clone();
        let new_node = Self::new_node(space, value.clone());
        let is_thunk = new_node.is_thunk();
        let mut node_by_meta_time0 = HashMap::new();
        node_by_meta_time0.insert(self.meta_time.clone(), new_node);
        let nodes_by_time = self.space_time.get(&pointer);
        match nodes_by_time {
            Some(nodes_by_time) => {
                self.space_time = self.space_time.update(
                    pointer.clone(),
                    nodes_by_time.update_with(self.time.clone(), node_by_meta_time0, |old, new| {
                        old.union(new)
                    }),
                );
            }
            None => {
                self.space_time = self.space_time.update(
                    pointer.clone(),
                    HashMap::new().update(self.time.clone(), node_by_meta_time0),
                )
            }
        };
        if beh == PutBeh::Put {
            counts.cells += 1;
            if is_thunk {
                counts.thunk_cells += 1
            } else {
                counts.non_thunk_cells += 1;
            }
            let put_action = Action::Put(value.clone());
            let target = (pointer.clone(), self.time.clone(), self.meta_time.clone());
            self.event(Event::AddNode(target.clone()));
            let edge_id = self.new_edge_helper(put_action, target, None)?;
            self.event(Event::AddEdge(edge_id));
        };
        Ok(())
    }

    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        self.meta_time.incr();
        let (_, node) = self.get_node(&pointer)?;
        let value = node.get_value()?;
        let edge_id = self.new_edge_to_pointer(Action::Get(value.clone()), pointer, None)?;
        self.event(Event::AddEdge(edge_id));
        Ok(value)
    }
    fn navigate_begin(&mut self, nav: Navigation, symbol: Symbol_) -> Res<()> {
        self.push_stack(FrameKind::Navigation(nav.clone()));
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
        self.meta_time.incr();
        let (node_id, node) = {
            let (i, n) = self.get_node(&pointer)?.clone();
            (i, n.clone())
        };
        self.event(Event::ForceBegin(node_id.clone()));
        if let Node::Thunk(tc) = node.clone() {
            if let Some(cache_value) = tc.result.clone()
                && !settings.force_begin_always_misses
            {
                counts.force_begin_cache_hit += 1;
                // TODO -- clean.
                let action = node.clone().force_action()?;
                let edge_id = self.new_edge_to_pointer(action, pointer, None)?;
                self.event(Event::ForceEnd(node_id.clone(), edge_id));
                Ok(ForceBeginResult::CacheHit(cache_value.0, cache_value.1))
            } else {
                counts.force_begin_cache_miss += 1;
                self.push_stack(FrameKind::Force(node_id.clone()));
                self.current_node = node_id;
                self.space = tc.space.clone();
                // to do -- log
                Ok(ForceBeginResult::CacheMiss(tc.body.clone()))
            }
        } else {
            Err(Error::TypeMismatch(line!()))
        }
    }
    fn force_end(&mut self, settings: &Settings, value: Value_) -> Res<()> {
        self.meta_time.incr();
        let trace = self.trace.clone();
        let meta_time = self.meta_time.clone();
        let (_, node) = self
            .get_node_mut(&self.current_node.0.clone(), &self.now())
            .ok_or(Error::UnreachableForceEnd)?;
        if !settings.force_end_forgets_result {
            node.set_cache_value(meta_time, value, trace)?;
        }
        let action = node.force_action()?;
        let target = self.current_node.0.clone();
        self.trace = Vector::new(); // trace was cached above. Now clear it.
        let fr = self.pop_stack()?;
        let edge_id = self.new_edge_to_pointer(action, target, Some(fr.meta_time))?;
        self.event(Event::AddEdge(edge_id.clone()));
        match fr.kind {
            FrameKind::Force(node_id) => self.event(Event::ForceEnd(node_id, edge_id)),
            _ => unreachable!(),
        };
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
        let meta_time = self.meta_time.clone();
        let updated = self
            .get_node_by_space(&time)
            .update((pointer, meta_time), Self::new_node(space, value));
        self.time_space.insert(time, updated);
        Ok(())
    }

    fn put_symbol_delay(&mut self, symbol: Symbol_, time: Time, value: Value_) -> Res<Pointer> {
        let pointer = self.space.apply(symbol);
        self.put_pointer_delay(pointer.clone(), time, value)?;
        Ok(pointer)
    }

    fn peek(&mut self, pointer: Pointer) -> Res<Option<Value_>> {
        match self.get_node(&pointer) {
            Ok((_, node)) => Ok(Some(node.get_value()?)),
            Err(_) => Ok(None),
        }
    }

    fn peek_cell(&mut self, pointer: Pointer) -> Res<Value_> {
        use crate::adapton::peek_value::PeekValue;
        match self.get_node(&pointer) {
            Ok((node_id, node)) => {
                let incoming_edges = self.get_incoming_edges(&pointer)?;
                let outgoing_edges = self.get_outgoing_edges(&pointer)?;
                let node = node.clone();
                let info = NodeInfo {
                    node_id,
                    node,
                    incoming_edges,
                    outgoing_edges,
                };
                Ok(Some(info).into_value_())
            }
            Err(_) => None::<Value_>
                .to_motoko_shared()
                .map_err(|_| Error::Unreachable),
        }
    }

    fn peek_events(&mut self) -> Res<Value_> {
        Ok(self.events.clone().into_value_())
    }
}
