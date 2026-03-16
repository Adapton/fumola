use std::ops::Add;

use crate::ToMotoko;
use crate::Value;
use crate::adapton::state::{CacheState, Counts, Settings};
use crate::adapton::{Error, ForceBeginResult, Navigation, Pointer, Res, Space, Time};
use crate::value::{Symbol_, ThunkBody, Value_};
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

// Implement ToMotoko for this if peek_cell it works better.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Node {
    NonThunk(Value_),
    Thunk(ThunkNode),
}

// Implement ToMotoko
// - Each ThunkNode field injects into the Value type without re-encoding it.
// - EdgeIds are tagged Nats, but each is resolved to a triple as well.
// - prim "adaptonPeekCell" returns this more useful encoding, permitting DCG traversals
//   by repeated use.  Also returns incoming edges, which we need to index.
//
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct ThunkNode {
    pub body: ThunkBody,
    pub space: Space,
    pub trace: Vector<EdgeId>,
    pub result: Option<Value_>,
}

pub type SpaceTime = HashMap<Space, NodesByTime>;
pub type NodesByTime = HashMap<(Time, MetaTime), Node>;

pub type TimeSpace = HashMap<Time, NodesBySpace>;
pub type NodesBySpace = HashMap<(Space, MetaTime), Node>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GraphicalState {
    pub meta_time: MetaTime,
    pub next_edge_id: EdgeId,
    pub space_time: SpaceTime,
    pub time_space: TimeSpace,
    pub edges: Edges,
    pub stack: Vector<Frame>,
    // Cursor state: current_node, trace, space, time.
    pub current_node: NodeId, // None ==> stack(i).thunk_pointer == None, for all i.
    pub trace: Vector<EdgeId>,
    pub space: Space,
    pub time: Time,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FrameKind {
    Navigation(Navigation),
    Eval(NodeId),
    Clean(NodeId),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Frame {
    pub kind: FrameKind,
    pub ambient_space: Space,
    pub ambient_time: Time,
    pub current_node: NodeId,
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
    Force(ThunkBody, Value_),
    Put(Value_),
    Get(Value_),
}

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
            Node::Thunk(t) => Ok(Action::Force(t.body.clone(), t.result.clone().unwrap())),
        }
    }
    pub fn set_cache_value(&mut self, v: Value_, trace: Vector<EdgeId>) -> Res<()> {
        match self {
            Node::NonThunk(_) => Err(Error::Unreachable),
            Node::Thunk(t) => {
                t.result = Some(v);
                t.trace = trace;
                Ok(())
            }
        }
    }
}

impl GraphicalState {
    #[allow(dead_code)]
    fn get_node_by_time_mut<'a>(&'a mut self, p: &Pointer) -> &'a mut NodesByTime {
        let exists = self.space_time.get(p) != None;
        if !exists {
            self.space_time.insert(p.clone(), HashMap::new());
        }
        self.space_time.get_mut(p).unwrap() // always succeeds, because of check above.
    }
    fn get_node_by_time<'a>(&'a mut self, p: &Pointer) -> &'a NodesByTime {
        let not_exists = self.space_time.get(p) == None;
        if not_exists {
            assert_eq!(self.space_time.insert(p.clone(), HashMap::new()), None);
        }
        self.space_time.get(p).unwrap()
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
    fn get_node_mut<'a>(&'a mut self, p: &Pointer, t: &Time) -> Option<&'a mut Node> {
        let meta_time = self.meta_time.clone();
        self.get_node_by_time_mut(p)
            .get_mut(&(t.clone(), meta_time))
    }
    fn current_node(&self) -> NodeId {
        self.current_node.clone()
    }
    fn new_node(space: Space, value: Value_) -> Node {
        match &*value {
            Value::Thunk(e) => Node::Thunk(ThunkNode {
                body: e.clone(),
                space: space,
                result: None,
                trace: Vector::new(),
            }),
            _ => Node::NonThunk(value),
        }
    }
    fn new_edge(&mut self, action: Action, target: NodeId) -> Res<EdgeId> {
        let id = self.next_edge_id.clone();
        self.next_edge_id = self.next_edge_id.next();
        self.trace.push_back(id.clone());
        let source = self.current_node();
        self.edges.insert(
            id.clone(),
            Edge {
                source,
                target,
                action,
            },
        );
        Ok(id)
    }
    fn new_edge_to_pointer(&mut self, action: Action, target: Pointer) -> Res<EdgeId> {
        let time = self.time.clone();
        let meta_time = self.meta_time.clone();
        let edge_id = self.new_edge(action, (target, time, meta_time))?;
        Ok(edge_id)
    }
    fn push_stack(&mut self, frame_kind: FrameKind) {
        self.stack.push_back(Frame {
            kind: frame_kind,
            trace: Vector::new(),
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
            self.put_pointer(&mut dummy, pointer.clone(), node_value)?;
        }
        Ok(())
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
            if let Some(node) = nodes_by_time.get(&(self.time.clone(), self.meta_time.clone())) {
                let node_id = (pointer.clone(), self.time.clone(), self.meta_time.clone());
                Ok((node_id, node))
            } else {
                let mut time = None;
                let mut meta_time = None;
                let mut node = None;
                for ((time_, meta_time_), node_) in nodes_by_time.iter() {
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
            next_edge_id: EdgeId(BigUint::from(0 as usize)),
            meta_time: MetaTime(BigUint::from(0 as usize)),
            space_time: HashMap::new(),
            time_space: HashMap::new(),
            edges: HashMap::new(),
            trace: Vector::new(),
            stack: Vector::new(),
            space: Space::Here,
            time: Time::Now,
            current_node: Self::root_node(),
        }
    }

    fn put_symbol(&mut self, counts: &mut Counts, symbol: Symbol_, value: Value_) -> Res<Pointer> {
        let p: Pointer = self.space.apply(symbol);
        self.put_pointer(counts, p.clone(), value)?;
        Ok(p)
    }
    fn put_pointer(&mut self, counts: &mut Counts, pointer: Pointer, value: Value_) -> Res<()> {
        let time = self.time.clone();
        let meta_time = self.meta_time.clone();
        let space = self.space.clone();
        let nodes = self.get_node_by_time(&pointer);
        let nodes_orig_len = nodes.len();
        let new_node = Self::new_node(space, value.clone());
        let is_thunk = new_node.is_thunk();
        let nodes = nodes.update((time.clone(), meta_time.clone()), new_node);
        if nodes.len() > nodes_orig_len {
            counts.cells += 1;
            if is_thunk {
                counts.thunk_cells += 1
            } else {
                counts.non_thunk_cells += 1;
            }
        };
        self.space_time.insert(pointer.clone(), nodes);
        let _ = self.new_edge_to_pointer(Action::Put(value.clone()), pointer);
        Ok(())
    }
    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        let (_, node) = self.get_node(&pointer)?;
        let value = node.get_value()?;
        let _ = self.new_edge_to_pointer(Action::Get(value.clone()), pointer);
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
        let (node_id, node) = self.get_node(&pointer)?;
        let node = node.clone();
        if let Node::Thunk(tc) = node {
            if let Some(cache_value) = tc.result.clone()
                && !settings.force_begin_always_misses
            {
                counts.force_begin_cache_hit += 1;
                // TODO -- clean.
                Ok(ForceBeginResult::CacheHit(cache_value))
            } else {
                counts.force_begin_cache_miss += 1;
                self.push_stack(FrameKind::Eval(node_id.clone()));
                self.current_node = node_id;
                self.space = tc.space.clone();
                Ok(ForceBeginResult::CacheMiss(tc.body.clone()))
            }
        } else {
            Err(Error::TypeMismatch(line!()))
        }
    }
    fn force_end(&mut self, settings: &Settings, value: Value_) -> Res<()> {
        let trace = self.trace.clone();
        let node = self
            .get_node_mut(&self.current_node.0.clone(), &self.now())
            .ok_or(Error::Unreachable)?;
        if !settings.force_end_forgets_result {
            node.set_cache_value(value, trace)?;
        }
        let action = node.force_action()?;
        // TODO -- compare the NodeIds of each Edge to that of the Node.
        self.new_edge_to_pointer(action, self.current_node.0.clone())?;
        self.trace = Vector::new(); // trace was cached above. Now clear it.
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
        match self.get_node(&pointer) {
            Ok((_, node)) => Some(node)
                .to_motoko_shared()
                .map_err(|_| Error::Unreachable),
            Err(_) => None::<Value_>
                .to_motoko_shared()
                .map_err(|_| Error::Unreachable),
        }
    }

    fn poke(&mut self, pointer: Pointer, time: Option<Time>, value: Value_) -> Res<()> {
        let mut dummy = Counts::new();
        match time {
            None => self.put_pointer(&mut dummy, pointer, value),
            Some(time) => self.put_pointer_delay(pointer, time, value),
        }
    }
}
