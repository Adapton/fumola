//! The graphical cache: Fumola's demanded computation graph (DCG), its construction, and its
//! realignment after an edit.
//!
//! # Reading guide
//!
//! Three formal accounts stand behind this file, and the comments below refer to all three:
//!
//! - **Adapton** (Hammer, Phang, Hicks, Foster; PLDI 2014), Algorithm 1 in section 5.2:
//!   `dirty` (lines 1-5) and `propagate` (lines 6-15), pseudocode over a DCG whose edges carry
//!   a dirty bit and a label -- the value the edge observed.
//! - **Nominal Adapton** (Hammer, Dunfield, Headley, Labich, Foster, Hicks, Van Horn;
//!   OOPSLA 2015), Figures 5, 6 and 8: a graph semantics with allocation by name.
//!   `Eval-refDirty` / `Eval-thunkDirty` overwrite an existing name and `dirty-paths-in` marks
//!   every path to it; `Eval-refClean` / `Eval-thunkClean` re-allocate an unchanged name and
//!   touch nothing; `Eval-forceClean` reuses a thunk when `all-clean-out` holds;
//!   `Eval-scrubEdge` cleans one edge whose target is up to date and whose action is
//!   `consistent-action`; `Eval-computeDep` re-evaluates after `del-edges-out`. Figure 8 is
//!   well-formedness: every edge into the source of a dirty edge is dirty (`Grwf-dirtyEdge`),
//!   and a clean edge's action is consistent with an all-clean target (`Grwf-cleanEdge`).
//! - **The Adapton recipe** (`adapton-recipe.ott`, in progress): the current formalism, with
//!   symbolic space and time, meta-moments as node versions, and the rules `dirtyNode`, `DT-*`
//!   (dirtying a trace), `CN-aligned` / `CN-misaligned` (cleaning a node), `CT-*` (cleaning a
//!   trace) and `IE-emptyForce` / `IE-cleanForce`. The recipe is written to agree with this
//!   code: where the two differ, this code is the reference, and the difference is listed below
//!   as something for the recipe to take up.
//!
//! # Terms
//!
//! Fumola's words differ from the papers' -- in terms, not in algorithms:
//!
//! | papers                   | here         |
//! |--------------------------|--------------|
//! | dirtying                 | signaling    |
//! | dirty (an edge's state)  | signaled     |
//! | clean (an edge's state)  | aligned      |
//! | cleaning                 | repair       |
//! | change propagation       | realignment  |
//!
//! A `put` that changes a cell with readers *signals*; a `force` of a thunk whose trace holds
//! a signaled edge *repairs*; and the whole, from an edit to the next consistent demand, is
//! *realignment*, made of signal and repair steps. `Align` is the edge status.
//! `Event::SignalingBegin`..`SignalingEnd` and `Event::RepairBegin`..`RepairEnd` bracket the two
//! traversals in the history, as `ForceBegin`..`ForceEnd` brackets a force.
//!
//! # Where this code and the formalisms differ
//!
//! 1. **Edge targets.** An edge's `target` is typed `NodeId`, but its meta-time is the moment
//!    of the access, not the version read (see `new_edge_to_pointer`). The recipe agrees in
//!    substance: its edges target a full pointer `qq` = (Space, Moment), with no meta-moment.
//!    Readers are therefore indexed by `(Space, Time)` -- `edges_by_target` -- which is the
//!    recipe's `incomingEdges`.
//! 2. **Re-evaluation makes a new version.** The recipe's `CN-misaligned` re-evaluates at
//!    `nextMetaMoment`; Nominal Adapton's `Eval-computeDep` overwrites the node in place after
//!    `del-edges-out`; Algorithm 1 clears `node.outgoinglist`. This code does both halves: a new
//!    version (recipe) and the old trace's edges removed from the live graph (papers). The
//!    history keeps the removed edges, so `history` is the recipe's lossless graph and
//!    `edges` / `edges_by_target` are the live one.
//! 3. **What signaling marks.** Algorithm 1 marks every incoming edge; Nominal Adapton's
//!    `dirty-paths-in` marks every path, allocation edges included; the recipe marks an edge
//!    only when its action is `misaligned(Space, v)` with the new contents, and past the first
//!    hop follows `forces` only. This code follows the recipe: a `Put` edge is never signaled,
//!    an allocation being no observation of what the cell later holds. Nominal Adapton's
//!    `dirty-paths-in` marks every path instead, allocation edges included, and its
//!    `all-clean-out` check on the popped node then reads a stale allocation as the *double
//!    use* of one name for two things. This code makes that check directly instead --
//!    `check_double_use`, at the put -- rather than by marking; see item 8.
//! 4. **A matched put.** Nominal Adapton's `Eval-refClean` / `Eval-thunkClean` add an
//!    allocation edge and change nothing else when the contents are unchanged. This code does
//!    the same while `Settings::put_matches_equal_values` holds (the default), and counts it in
//!    `Counts::put_matched`. The recipe's prose implies it (every action would be aligned with
//!    the update) but has no graphical `put` rule yet.
//! 5. **`get` of a thunk pointer** yields the thunk here; the recipe's `E-getThunk` yields the
//!    pair (space, thunk). Signaling compares a recorded `Get` value with the new contents, so
//!    the two agree only because a thunk's space is not part of what `get` observes here.
//! 6. **Undelay.** A node re-homed by `undelay` neither matches nor signals
//!    (`PutBeh::Undelay`). The recipe says undelay "uses dirtying"; that remains to be written
//!    on both sides.
//! 7. **No cycle check.** A thunk that forces itself, through any path, is not detected here or
//!    in the recipe (whose `putImmediate` side condition, `∉ Path`, is the nearest thing).
//!    Repair adds no new way to form one -- it re-forces only what a from-scratch run forced.
//! 8. **Double use, within one evaluation.** Nominal Adapton refuses a run that allocates two
//!    different things under one name: `Eval-thunkDirty` dirties every path into a re-allocated
//!    pointer, and the `all-clean-out(G2', q)` premise of `Eval-computeDep`, checked as the node
//!    is popped, then fails. `check_double_use` here answers the same question at the moment the
//!    put happens, by asking whether any thunk *whose body is running* has an allocation edge to
//!    this cell recording different contents. Two differences from the paper, both stated in
//!    `check_double_use`: the error names the put rather than the later force, and the check
//!    covers one evaluation rather than the whole graph -- an allocation collision between two
//!    thunks that have both returned is not caught, and needs the marking. The recipe has no
//!    such rule at all.
//!
use crate::ToMotoko;
use crate::Value;
use crate::adapton::MetaTime;
use crate::adapton::peek_value::PeekValue;
use crate::adapton::state::PutBeh;
use crate::adapton::state::{CacheState, Counts, Settings};
use crate::adapton::{Error, ForceBeginResult, Navigation, Pointer, RepairStep, Res, Space, Time};
use crate::value::{Symbol_, ThunkBody, Value_};
use im_rc::vector;
use im_rc::{HashMap, Vector};
use num_bigint::BigUint;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Event {
    AddNode(NodeId),
    AddEdge(EdgeId),
    UpdateEdge(EdgeId),
    /// An edge leaving the live graph: a re-evaluated thunk's old trace (Nominal Adapton's
    /// `del-edges-out`; Algorithm 1 line 13). Defined before repair existed, emitted only now.
    RemoveEdge(EdgeId),
    ForceBegin(EdgeId, Option<MetaTime>),
    ForceEnd(EdgeId),
    /// Signaling: a put changed a cell that has readers. Between these two, the edges that
    /// observed the old contents, and the force edges upstream of them, are marked `Signaled`.
    /// The node is the new version the put made.
    SignalingBegin(NodeId),
    SignalingEnd(NodeId),
    /// An edge a signaling traversal marked `Signaled` (Algorithm 1 line 4).
    EdgeSignaled(EdgeId),
    /// Repair: a thunk with a cached result was forced while its trace held a signaled edge.
    /// Between these two its edges are checked in order, and either all realign or the thunk is
    /// re-evaluated; the outcome says which.
    RepairBegin(NodeId),
    RepairEnd(NodeId, RepairOutcome),
    /// An edge a repair found consistent again and marked `Aligned` (Algorithm 1 line 9, when
    /// line 12 finds the values equal; Nominal Adapton's `Eval-scrubEdge`).
    EdgeAligned(EdgeId),
}

/// How a repair ended.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum RepairOutcome {
    /// Every edge realigned; the cached result stands (the recipe's `CN-aligned`).
    Aligned,
    /// An edge could not be realigned; a new version was evaluated (`CN-misaligned`).
    Reevaluated,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EventHistoryItem {
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
pub struct NodeHistoryItem {
    pub meta_time: MetaTime,
    pub node_id: NodeId,
    pub node: Node,
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

    // History is only an output structure; not used algorithmically by GraphicalState.
    // Imposes at worst O(1) logging overhead to any operation.
    // Eventually, offer a flag to toggle it off, for maximum performance.
    pub history: History,
}

// PartialEq, Eq and Hash so that a history can be a `Value::AdaptonHistory` -- a value form
// carries them, and every field already has them.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct History {
    pub events: Vector<EventHistoryItem>,
    pub nodes: Vector<NodeHistoryItem>,
    pub edges: Vector<EdgeHistoryItem>,
}

impl History {
    pub fn new() -> Self {
        History {
            events: Vector::new(),
            nodes: Vector::new(),
            edges: Vector::new(),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum FrameKind {
    Navigation(Navigation),
    Force(NodeId, EdgeId),
    Repair(RepairFrame),
}

/// A repair in progress: Algorithm 1's `propagate(node)` with its loop counter kept on the
/// stack, because the loop body (line 11) can force a thunk, and forcing runs Fumola code that
/// only the VM can run. `repair_step` advances the walk; `repair_resume` receives what the VM
/// forced.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RepairFrame {
    /// The thunk being repaired.
    pub node_id: NodeId,
    /// The edge by which it was demanded: a new edge from the current node for an ordinary
    /// force, or the edge an enclosing repair is checking when this force is on its behalf.
    /// It receives the `ForceEnd`, and it is the edge a re-evaluation's `force_end` completes.
    pub demand_edge: EdgeId,
    /// The trace as it stood when repair began, walked in order (Algorithm 1 line 7).
    pub trace: Vector<EdgeId>,
    /// The next edge to check.
    pub index: usize,
    /// The edge whose target the VM is forcing on this repair's behalf, and the value the edge
    /// recorded, for the comparison of Algorithm 1 line 12 when the value comes back.
    pub awaiting: Option<(EdgeId, Value_)>,
    /// Set when an edge could not be realigned: the next step re-evaluates.
    pub misaligned: bool,
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
/// The readers of a cell: every live edge whose target is any version of the pointer, at that
/// time. Keyed by pointer rather than node because an edge reads whichever version is latest
/// when it is made, and a later version supersedes it for every reader at once. This is the
/// recipe's `incomingEdges`, and what signaling walks.
pub type EdgeIdsByTarget = HashMap<(Space, Time), Vector<EdgeId>>;

/// The status of an edge: whether what it recorded is known to agree with the graph.
///
/// The papers' dirty bit -- `edge.dirty` in Algorithm 1, `b ::= clean | dirty` in Nominal
/// Adapton and the recipe -- under Fumola's names. An edge is made `Aligned`; a put that changes
/// what it observed makes it `Signaled`; repair makes it `Aligned` again or removes it.
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Align {
    Aligned,
    Signaled,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Edge {
    pub source: NodeId,
    pub target: NodeId,
    pub action: Action,
    pub meta_times: (MetaTime, MetaTime),
    pub align: Align,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EdgeHistoryItem {
    pub meta_time: MetaTime,
    pub edge_id: EdgeId,
    pub edge: Edge,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Action {
    ForceBegin(ThunkBody),
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
            Node::Thunk(t) => match t.result.clone() {
                Some((_, value)) => Ok(Action::Force(t.body.clone(), value)),
                // A force of a thunk with no cached result is a force of
                // something that has not run; `force_begin_action` is the one
                // to take then.
                None => Err(Error::Unreachable),
            },
        }
    }
    pub fn force_begin_action(&mut self) -> Res<Action> {
        match self {
            Node::NonThunk(_) => Err(Error::Unreachable),
            Node::Thunk(t) => Ok(Action::ForceBegin(t.body.clone())),
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
    fn extend_history_with_event(&mut self, ev: Event) {
        self.history.events.push_back(EventHistoryItem {
            meta_time: self.meta_time.clone(),
            event: ev,
        });
    }
    fn extend_history_with_node_update(&mut self, node_id: NodeId, node: Node) {
        self.history.nodes.push_back(NodeHistoryItem {
            meta_time: self.meta_time.clone(),
            node_id,
            node,
        });
    }
    fn extend_history_with_edge_update(&mut self, edge_id: EdgeId) -> Res<()> {
        let edge = self
            .edges
            .get(&edge_id)
            .ok_or(Error::Internal(line!()))?
            .clone();
        self.extend_history_with_event(Event::UpdateEdge(edge_id.clone()));
        self.history.edges.push_back(EdgeHistoryItem {
            meta_time: self.meta_time.clone(),
            edge_id,
            edge,
        });
        Ok(())
    }
    fn extend_history_with_new_node(&mut self, p: &Pointer, t: Option<&Time>, n: &Node) {
        let time = match t {
            None => self.time.clone(),
            Some(t) => t.clone(),
        };
        let node_id = (p.clone(), time, self.meta_time.clone());
        self.extend_history_with_node_update(node_id, n.clone());
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
        let res = self.get_node_by_time_mut(p).get_mut(t)?;
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
        let readers_key = (target.0.clone(), target.1.clone());
        match self.edges_by_target.get_mut(&readers_key) {
            Some(edge_ids) => {
                if !edge_ids.contains(&id) {
                    edge_ids.push_back(id.clone());
                }
            }
            None => {
                self.edges_by_target
                    .insert(readers_key, vector!(id.clone()));
            }
        };
        let meta_time = self.meta_time.clone();
        let edge = Edge {
            source,
            target,
            action,
            meta_times: MetaTime::pair(meta_time_begin, meta_time),
            // Every edge is made aligned: it records what it observed just now.
            align: Align::Aligned,
        };
        self.history.edges.push_back(EdgeHistoryItem {
            meta_time: self.meta_time.clone(),
            edge_id: id.clone(),
            edge: edge.clone(),
        });
        self.edges.insert(id.clone(), edge);
        Ok(id)
    }
    /// An edge from the current node to the cell at `target`.
    ///
    /// The target's meta-time is the moment of this access, not the meta-time of the version
    /// read: an edge reads whichever version is latest, and the readers index keys on the
    /// pointer and time alone (see `EdgeIdsByTarget`). The recipe's edges target `qq`, a full
    /// pointer with no meta-moment, and mean the same thing.
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
    fn update_edge(
        &mut self,
        edge_id: &EdgeId,
        updated_action: Action,
        meta_time_begin: Option<MetaTime>,
    ) -> Res<()> {
        let edge = self
            .edges
            .get(edge_id)
            .ok_or(Error::Internal(line!()))?
            .clone();
        let meta_time = self.meta_time.clone();
        self.edges = self.edges.update(
            edge_id.clone(),
            Edge {
                source: edge.source,
                target: edge.target,
                action: updated_action,
                meta_times: MetaTime::pair(meta_time_begin, meta_time),
                // A completed force records the result it just saw, so the edge is aligned
                // whatever it was before -- this is how a repair's re-force of a target
                // realigns the edge it was checking.
                align: Align::Aligned,
            },
        );
        Ok(())
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
            // Undelay neither matches nor signals (module notes, item 6), so the settings it
            // is handed do not matter.
            self.put_pointer(
                &Settings::new(),
                &mut dummy,
                pointer.clone(),
                node_value,
                PutBeh::Undelay,
            )?;
        }
        Ok(())
    }

    fn get_incoming_edges<'a>(&'a self, pointer: &Pointer) -> Res<Vector<(EdgeId, Edge)>> {
        let (nid, _) = self.get_node(pointer)?;
        // Every reader of the cell, whichever version each read (see `EdgeIdsByTarget`).
        let readers_key = (nid.0, nid.1);
        let edge_ids = self
            .edges_by_target
            .get(&readers_key)
            .unwrap_or(&vector!())
            .clone();
        edge_ids
            .into_iter()
            .map(|edge_id| {
                let edge = self.edges.get(&edge_id).ok_or(Error::Internal(line!()))?;
                Ok((edge_id.clone(), edge.clone()))
            })
            .collect()
    }
    fn get_outgoing_edges<'a>(&'a self, pointer: &Pointer) -> Res<Vector<(EdgeId, Edge)>> {
        let (_nid, node) = self.get_node(pointer)?;
        match node {
            Node::NonThunk(_) => Ok(vector!()),
            Node::Thunk(thunk_node) => thunk_node
                .trace
                .clone()
                .into_iter()
                .map(|edge_id| {
                    let edge = self.edges.get(&edge_id).ok_or(Error::Internal(line!()))?;
                    Ok((edge_id.clone(), edge.clone()))
                })
                .collect(),
        }
    }

    /// The node a `NodeId` names, exactly: that pointer, that time, that version.
    fn node_by_id(&self, id: &NodeId) -> Option<&Node> {
        self.space_time.get(&id.0)?.get(&id.1)?.get(&id.2)
    }

    /// The latest version of the cell at `pointer` in the current time, if it has one there.
    /// Unlike `get_node` this does not fall back to an earlier time: a put makes a version at
    /// the current time, and the version it supersedes is the one already there.
    fn latest_version_at_now(&self, pointer: &Pointer) -> Option<(MetaTime, Node)> {
        let versions = self.space_time.get(pointer)?.get(&self.time)?;
        let mut latest: Option<(&MetaTime, &Node)> = None;
        for (m, n) in versions.iter() {
            match latest {
                Some((m0, _)) if m0.0 >= m.0 => {}
                _ => latest = Some((m, n)),
            }
        }
        latest.map(|(m, n)| (m.clone(), n.clone()))
    }

    /// Record `node` as a new version of `pointer` at `time`, stamped with the current
    /// meta-time -- the recipe's `nextMetaMoment`.
    fn insert_version(&mut self, pointer: &Pointer, time: &Time, node: Node) -> NodeId {
        let mut by_meta = HashMap::new();
        by_meta.insert(self.meta_time.clone(), node);
        let by_time = match self.space_time.get(pointer) {
            Some(by_time) => by_time.update_with(time.clone(), by_meta, |old, new| old.union(new)),
            None => HashMap::new().update(time.clone(), by_meta),
        };
        self.space_time = self.space_time.update(pointer.clone(), by_time);
        (pointer.clone(), time.clone(), self.meta_time.clone())
    }

    /// The bookkeeping of a put that made a node: the counts, the `AddNode` event and the `Put`
    /// edge from the current node. A poke or an undelay makes the node and no edge.
    fn count_and_link_put(
        &mut self,
        counts: &mut Counts,
        value: &Value_,
        is_thunk: bool,
        beh: PutBeh,
        node_id: &NodeId,
    ) -> Res<()> {
        if beh == PutBeh::Put {
            counts.cells += 1;
            if is_thunk {
                counts.thunk_cells += 1
            } else {
                counts.non_thunk_cells += 1;
            }
            self.extend_history_with_event(Event::AddNode(node_id.clone()));
            let edge_id =
                self.new_edge_helper(Action::Put(value.clone()), node_id.clone(), None)?;
            self.extend_history_with_event(Event::AddEdge(edge_id));
        }
        Ok(())
    }

    /// Set an edge's status, and say so in the history: an `EdgeSignaled` or `EdgeAligned`
    /// event, and the edge as it now stands.
    fn set_align(&mut self, edge_id: &EdgeId, align: Align) {
        let edge = match self.edges.get(edge_id) {
            Some(edge) => edge.clone(),
            None => return,
        };
        let edge = Edge {
            align: align.clone(),
            ..edge
        };
        self.edges = self.edges.update(edge_id.clone(), edge.clone());
        let event = match align {
            Align::Signaled => Event::EdgeSignaled(edge_id.clone()),
            Align::Aligned => Event::EdgeAligned(edge_id.clone()),
        };
        self.extend_history_with_event(event);
        self.history.edges.push_back(EdgeHistoryItem {
            meta_time: self.meta_time.clone(),
            edge_id: edge_id.clone(),
            edge,
        });
    }

    /// Take an edge out of the live graph -- `edges` and the readers index -- leaving it in
    /// the history, with a `RemoveEdge` event to say when it left.
    fn remove_edge(&mut self, edge_id: &EdgeId) {
        let edge = match self.edges.get(edge_id) {
            Some(edge) => edge.clone(),
            None => return,
        };
        let readers_key = (edge.target.0.clone(), edge.target.1.clone());
        if let Some(ids) = self.edges_by_target.get(&readers_key) {
            let kept: Vector<EdgeId> = ids.iter().filter(|id| *id != edge_id).cloned().collect();
            self.edges_by_target.insert(readers_key, kept);
        }
        self.edges.remove(edge_id);
        self.extend_history_with_event(Event::RemoveEdge(edge_id.clone()));
    }

    /// The double use of one name, within one evaluation: Nominal Adapton's condition, asked at
    /// the put.
    ///
    /// A `Put` edge records what its source allocated under this name. If a thunk whose body is
    /// *running right now* holds such an edge to this cell, and the cell is about to hold
    /// something else, then this one evaluation has used the name for two different things --
    /// and everything it computes from here could have come from either, which is the
    /// unsoundness Nominal Adapton's `all-clean-out` premise exists to refuse. The usual shape
    /// is a loop or a recursion that mints a name it has already used.
    ///
    /// Answered at the put, so the error names the put that did it. The paper answers it as the
    /// node is popped, which catches one more case -- two thunks that have both returned,
    /// having allocated different things under one name -- at the cost of marking every
    /// allocation edge and of reporting the collision at a force far from its cause. That case
    /// is not caught here (module notes, item 8).
    ///
    /// Three things are deliberately *not* a double use:
    ///
    /// - An **editor's** put, made outside every force. From there a put is an edit, and the
    ///   editor owns the cells it edits whoever allocated them -- which is what the whole
    ///   realignment machinery is for.
    /// - A put of **what the cell already holds**: allocating the same thing twice under one
    ///   name is Nominal Adapton's `Eval-refClean`, and here a matched put. This is never
    ///   reached for one, since `put_pointer` returns first.
    /// - A **re-evaluation's** re-put of what it allocated last time, under a name a *previous
    ///   version* of the same thunk allocated. The version being re-evaluated is the one on the
    ///   stack, the old version's edges left the graph when repair dropped its trace, and the
    ///   old version's node id is not the new one's -- so nothing matches.
    fn check_double_use(&self, pointer: &Pointer, new_value: &Value_) -> Res<()> {
        // The thunks whose bodies are running. Empty means the put is an editor's.
        let evaluating: Vec<&NodeId> = self
            .stack
            .iter()
            .filter_map(|frame| match &frame.kind {
                FrameKind::Force(node_id, _) => Some(node_id),
                _ => None,
            })
            .collect();
        if evaluating.is_empty() {
            return Ok(());
        }
        let readers_key = (pointer.clone(), self.time.clone());
        let ids = match self.edges_by_target.get(&readers_key) {
            Some(ids) => ids,
            None => return Ok(()),
        };
        for id in ids.iter() {
            let edge = match self.edges.get(id) {
                Some(edge) => edge,
                None => continue,
            };
            match &edge.action {
                Action::Put(allocated) if allocated != new_value => {
                    if evaluating.contains(&&edge.source) {
                        return Err(Error::DoubleUse {
                            pointer: pointer.clone(),
                            observer: edge.source.clone(),
                        });
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Signaling: the traversal an edit starts.
    ///
    /// Algorithm 1 (PLDI 2014) lines 1-5: from the changed node, follow the incoming edges,
    /// mark each that is not already marked, and recurse at its source. The recipe's
    /// `dirtyNode` and `DT-*` rules say the same with two refinements, both kept here:
    ///
    /// - An edge is signaled only when its recorded action is misaligned with the new contents
    ///   (`DT-cleanIntoDirty` under the pattern `misaligned(Space, v)`): a `Get` that recorded
    ///   the very value the cell holds again stays aligned (`DT-stillClean`), and so does every
    ///   `Put`, an allocation rather than an observation. Nominal Adapton's `dirty-paths-in`
    ///   marks every path instead, allocation edges included; see the module notes, item 3.
    /// - Past the first hop the pattern is `forces`: the source of a signaled edge is a thunk
    ///   whose *result* may now differ, and only its forcers observed that result. Its getters
    ///   observed its body, which nothing here has changed.
    ///
    /// Stopping at an edge already signaled (`DT-alreadyDirty`; Algorithm 1 line 3) is sound
    /// by the invariant Nominal Adapton states as `Grwf-dirtyEdge`: every edge into the source
    /// of a signaled edge is already signaled. The traversal is bracketed by `SignalingBegin`
    /// and `SignalingEnd`, naming the new version that started it, whenever the cell has a
    /// reader at all.
    fn signal(&mut self, counts: &mut Counts, pointer: &Pointer, new_value: &Value_, from: NodeId) {
        let start = (pointer.clone(), self.time.clone());
        let observed = |action: &Action| {
            matches!(
                action,
                Action::Get(_) | Action::Force(..) | Action::ForceBegin(_)
            )
        };
        let has_readers = self
            .edges_by_target
            .get(&start)
            .map(|ids| {
                ids.iter()
                    .any(|id| self.edges.get(id).map_or(false, |e| observed(&e.action)))
            })
            .unwrap_or(false);
        if !has_readers {
            return;
        }
        counts.signalings += 1;
        self.extend_history_with_event(Event::SignalingBegin(from.clone()));
        // (readers of this pointer, past the first hop?)
        let mut work: Vec<((Space, Time), bool)> = vec![(start, false)];
        while let Some((readers_key, forces_only)) = work.pop() {
            let ids = self
                .edges_by_target
                .get(&readers_key)
                .cloned()
                .unwrap_or_default();
            for id in ids.iter() {
                let edge = match self.edges.get(id) {
                    Some(edge) => edge.clone(),
                    None => continue,
                };
                if edge.align == Align::Signaled {
                    continue; // DT-alreadyDirty
                }
                let misaligned = match &edge.action {
                    Action::Put(_) => false, // DT-stillClean: an allocation is not an observation
                    Action::Get(seen) => !forces_only && seen != new_value, // misaligned(Space, v)
                    Action::Force(..) | Action::ForceBegin(_) => true, // both patterns match a force
                };
                if !misaligned {
                    continue;
                }
                self.set_align(id, Align::Signaled); // DT-cleanIntoDirty ...
                counts.edges_signaled += 1;
                // ... whose recursive premise dirties the `forces` into the edge's source.
                work.push(((edge.source.0.clone(), edge.source.1.clone()), true));
            }
        }
        self.extend_history_with_event(Event::SignalingEnd(from));
    }

    /// The edge a repair is checking by forcing its target, if the topmost frame is a repair
    /// paused on such a check. A force made on its behalf records no new edge.
    fn repair_awaiting_edge(&self) -> Option<EdgeId> {
        match self.stack.back().map(|frame| &frame.kind) {
            Some(FrameKind::Repair(rf)) => rf.awaiting.as_ref().map(|(edge_id, _)| edge_id.clone()),
            _ => None,
        }
    }

    /// Put an updated repair frame back on top of the stack, in place of the one there.
    fn replace_repair_frame(&mut self, rf: RepairFrame) {
        if let Some(mut frame) = self.stack.pop_back() {
            frame.kind = FrameKind::Repair(rf);
            self.stack.push_back(frame);
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
                // `nodes` is a non-empty map, so the loop above found one.
                let latest = latest.ok_or(Error::UndefinedNow(pointer.clone()))?;
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
                match (node, time, meta_time) {
                    (Some(node), Some(time), Some(meta_time)) => {
                        Ok(((pointer.clone(), time.clone(), meta_time.clone()), node))
                    }
                    _ => Err(Error::UndefinedNow(pointer.clone())),
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
            history: History::new(),
        }
    }

    fn put_symbol(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        symbol: Symbol_,
        value: Value_,
    ) -> Res<Pointer> {
        let p: Pointer = self.space.apply(symbol);
        self.put_pointer(settings, counts, p.clone(), value, PutBeh::Put)?;
        Ok(p)
    }
    fn put_pointer(
        &mut self,
        settings: &Settings,
        counts: &mut Counts,
        pointer: Pointer,
        value: Value_,
        beh: PutBeh,
    ) -> Res<()> {
        self.meta_time.incr();
        let space = self.space.clone();
        let new_node = Self::new_node(space, value.clone());
        let is_thunk = new_node.is_thunk();
        let time = self.time.clone();

        // Allocation by a name already taken at this time. Nominal Adapton (Figure 5) has two
        // rules for it, and this is where they part:
        //
        // - `Eval-refClean` / `Eval-thunkClean`: the contents are what the cell already holds.
        //   Nothing in the graph changes; only the allocation edge is added. A *matched put*.
        // - `Eval-refDirty` / `Eval-thunkDirty`: the contents differ. The node is overwritten
        //   and `dirty-paths-in` marks every path to it. Here the overwrite is a new version
        //   (the recipe's meta-moments) and `signal` marks the readers.
        //
        // An undelay re-homes a delayed node and does neither (module notes, item 6).
        if beh != PutBeh::Undelay {
            if let Some((old_meta, old_node)) = self.latest_version_at_now(&pointer) {
                let same = match (&old_node, &new_node) {
                    (Node::NonThunk(a), Node::NonThunk(b)) => a == b,
                    (Node::Thunk(a), Node::Thunk(b)) => a.body == b.body && a.space == b.space,
                    _ => false,
                };
                if same && settings.put_matches_equal_values {
                    counts.put_matched += 1;
                    if beh == PutBeh::Put {
                        let target = (pointer, time, old_meta);
                        let edge_id = self.new_edge_helper(Action::Put(value), target, None)?;
                        self.extend_history_with_event(Event::AddEdge(edge_id));
                    }
                    return Ok(());
                }
                // Before the graph changes, so a refused put leaves no trace of itself.
                if settings.check_double_use {
                    if let Err(e) = self.check_double_use(&pointer, &value) {
                        counts.double_uses += 1;
                        return Err(e);
                    }
                }
                self.extend_history_with_new_node(&pointer, None, &new_node);
                let node_id = self.insert_version(&pointer, &time, new_node);
                self.count_and_link_put(counts, &value, is_thunk, beh, &node_id)?;
                self.signal(counts, &pointer, &value, node_id);
                return Ok(());
            }
        }
        self.extend_history_with_new_node(&pointer, None, &new_node);
        let node_id = self.insert_version(&pointer, &time, new_node);
        self.count_and_link_put(counts, &value, is_thunk, beh, &node_id)
    }

    fn get_pointer(&mut self, pointer: Pointer) -> Res<Value_> {
        self.meta_time.incr();
        let (_, node) = self.get_node(&pointer)?;
        let value = node.get_value()?;
        let edge_id = self.new_edge_to_pointer(Action::Get(value.clone()), pointer, None)?;
        self.extend_history_with_event(Event::AddEdge(edge_id));
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
            let (i, n) = self.get_node(&pointer)?;
            (i, n.clone())
        };
        let tc = match &node {
            Node::Thunk(tc) => tc.clone(),
            Node::NonThunk(_) => return Err(Error::TypeMismatch(line!())),
        };
        // A force that repair makes to check one of its edges records no edge of its own: the
        // edge under check already states the dependency, and what repair does is update that
        // edge's status (Nominal Adapton's `Eval-scrubEdge`; the recipe's `CT-dirtyIntoClean`).
        // Every other force records a new edge from the current node, as before.
        let repairing_for = self.repair_awaiting_edge();
        let cached = if settings.force_begin_always_misses {
            None
        } else {
            tc.result.clone()
        };
        match cached {
            Some((cache_meta, cache_value)) => {
                let edge_id = match &repairing_for {
                    Some(edge_id) => edge_id.clone(),
                    None => {
                        let action = node.clone().force_action()?;
                        self.new_edge_to_pointer(action, pointer, None)?
                    }
                };
                self.extend_history_with_event(Event::ForceBegin(
                    edge_id.clone(),
                    Some(cache_meta.clone()),
                ));
                // Nominal Adapton's `Eval-forceClean` asks `all-clean-out`: is every outgoing
                // edge aligned? Then the cached result stands -- Algorithm 1's `propagate` finds
                // no dirty edge to follow. Otherwise the thunk is repaired before its result can
                // be used (the recipe's `IE-cleanForce`: clean first, then read the cache), and
                // the VM drives that, one `RepairStep` at a time.
                let signaled = tc.trace.iter().any(|id| {
                    self.edges
                        .get(id)
                        .map_or(false, |e| e.align == Align::Signaled)
                });
                if !signaled {
                    counts.force_begin_cache_hit += 1;
                    self.extend_history_with_event(Event::ForceEnd(edge_id));
                    Ok(ForceBeginResult::CacheHit(cache_meta, cache_value))
                } else {
                    counts.repairs += 1;
                    self.extend_history_with_event(Event::RepairBegin(node_id.clone()));
                    self.push_stack(FrameKind::Repair(RepairFrame {
                        node_id,
                        demand_edge: edge_id,
                        trace: tc.trace.clone(),
                        index: 0,
                        awaiting: None,
                        misaligned: false,
                    }));
                    Ok(ForceBeginResult::Repair)
                }
            }
            None => {
                let edge_id = match &repairing_for {
                    Some(edge_id) => edge_id.clone(),
                    None => {
                        let action = node.clone().force_begin_action()?;
                        let edge_id = self.new_edge_to_pointer(action, pointer, None)?;
                        self.extend_history_with_event(Event::AddEdge(edge_id.clone()));
                        edge_id
                    }
                };
                self.extend_history_with_event(Event::ForceBegin(edge_id.clone(), None));
                counts.force_begin_cache_miss += 1;
                self.push_stack(FrameKind::Force(node_id.clone(), edge_id));
                self.current_node = node_id;
                self.space = tc.space.clone();
                Ok(ForceBeginResult::CacheMiss(tc.body.clone()))
            }
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
        if true {
            let node_copy = node.clone();
            let node_id = self.current_node().clone();
            self.extend_history_with_node_update(node_id, node_copy);
        }
        self.trace = Vector::new(); // trace was cached above. Now clear it.
        let fr = self.pop_stack()?;
        match fr.kind {
            FrameKind::Force(_node_id, edge_id) => {
                self.extend_history_with_event(Event::ForceEnd(edge_id.clone()));
                self.update_edge(&edge_id, action, Some(fr.meta_time))?;
                self.extend_history_with_edge_update(edge_id)?;
            }
            // `force_end` is reached only from the force this pops.
            _ => return Err(Error::UnreachableForceEnd),
        };
        Ok(())
    }

    /// Repair: Algorithm 1's `propagate(node)`, lines 6-15, as far as the graph can take it
    /// on its own.
    ///
    /// The trace is walked in order (line 7). An aligned edge is passed over (line 8; the
    /// recipe's `CT-alreadyClean`). A signaled `Get` edge is checked on the spot: the target is
    /// a cell, so there is nothing to bring up to date first (line 10 skips `propagate` for an
    /// aref), only the comparison of line 12. A signaled `Force` edge needs its target brought
    /// up to date before the comparison (lines 10-11; the recipe's `clean(qq)`), and that is a
    /// force, which may run Fumola code -- so the walk pauses, remembers the edge and what it
    /// recorded, and asks the VM (`RepairStep::Force`); `repair_resume` finishes the check.
    ///
    /// When every edge is aligned the node is aligned (`CN-aligned`; `all-clean-out` holds and
    /// `Eval-forceClean` applies) and its cached result is the answer. When an edge cannot be
    /// realigned the node is misaligned (`CT-misaligned`, `CN-misaligned`) and is re-evaluated:
    /// Algorithm 1 lines 13-15 clear the outgoing edges and evaluate; Nominal Adapton's
    /// `Eval-computeDep` is `del-edges-out` then evaluation; the recipe evaluates into a new
    /// version at `nextMetaMoment`. All three, here: the old trace's edges leave the live graph
    /// (`RemoveEdge`; the history keeps them), and the evaluation runs as the cache miss of a
    /// new version of the node -- the same `Force` frame an ordinary miss pushes, on the same
    /// demand edge, so `force_end` completes it exactly as it would a miss.
    fn repair_step(&mut self, counts: &mut Counts) -> Res<RepairStep> {
        let mut rf = match self.stack.back().map(|frame| frame.kind.clone()) {
            Some(FrameKind::Repair(rf)) => rf,
            _ => return Err(Error::Internal(line!())),
        };
        while !rf.misaligned && rf.index < rf.trace.len() {
            let edge_id = rf.trace[rf.index].clone();
            let edge = match self.edges.get(&edge_id) {
                Some(edge) => edge.clone(),
                None => return Err(Error::Internal(line!())),
            };
            if edge.align == Align::Aligned {
                rf.index += 1; // CT-alreadyClean; also every Put edge, which signaling never marks
                continue;
            }
            match edge.action.clone() {
                Action::Put(_) => {
                    rf.index += 1;
                }
                Action::Get(seen) => {
                    // The recipe's `CT-dirtyIntoClean` asks `clean(qq)` of the target first and
                    // has no rule for a non-thunk `qq`; this is that rule: nothing to do.
                    let current = {
                        let (_, target) = self.get_node(&edge.target.0)?;
                        target.get_value()?
                    };
                    if current == seen {
                        self.set_align(&edge_id, Align::Aligned); // line 12 equal: CT-dirtyIntoClean
                        counts.edges_aligned += 1;
                        rf.index += 1;
                    } else {
                        rf.misaligned = true; // CT-misaligned
                    }
                }
                Action::Force(_, seen) => {
                    rf.awaiting = Some((edge_id, seen));
                    self.replace_repair_frame(rf);
                    return Ok(RepairStep::Force(edge.target.0));
                }
                Action::ForceBegin(_) => {
                    // A force that never completed recorded no value to compare against.
                    rf.misaligned = true;
                }
            }
        }
        if !rf.misaligned {
            let (meta, value) = match self.node_by_id(&rf.node_id) {
                Some(Node::Thunk(tc)) => tc.result.clone().ok_or(Error::Internal(line!()))?,
                _ => return Err(Error::Internal(line!())),
            };
            self.pop_stack()?;
            self.extend_history_with_event(Event::RepairEnd(rf.node_id, RepairOutcome::Aligned));
            self.extend_history_with_event(Event::ForceEnd(rf.demand_edge));
            return Ok(RepairStep::Aligned(meta, value));
        }
        let (space, body) = match self.node_by_id(&rf.node_id) {
            Some(Node::Thunk(tc)) => (tc.space.clone(), tc.body.clone()),
            _ => return Err(Error::Internal(line!())),
        };
        self.pop_stack()?;
        for edge_id in rf.trace.iter() {
            self.remove_edge(edge_id); // del-edges-out; Algorithm 1 line 13
        }
        self.meta_time.incr();
        let new_node = Node::Thunk(ThunkNode {
            body: body.clone(),
            space: space.clone(),
            trace: Vector::new(),
            begin: Some(self.meta_time.clone()),
            result: None,
        });
        let pointer = rf.node_id.0.clone();
        let time = rf.node_id.1.clone();
        self.extend_history_with_new_node(&pointer, Some(&time), &new_node);
        let new_id = self.insert_version(&pointer, &time, new_node);
        self.extend_history_with_event(Event::AddNode(new_id.clone()));
        self.extend_history_with_event(Event::RepairEnd(rf.node_id, RepairOutcome::Reevaluated));
        counts.reevaluations += 1;
        self.push_stack(FrameKind::Force(new_id.clone(), rf.demand_edge));
        self.current_node = new_id;
        self.space = space;
        Ok(RepairStep::Reevaluate(body))
    }

    /// The target of the edge under check is up to date now (`repair_step` asked for it to be
    /// forced): does it still hold what the edge recorded? Algorithm 1 line 12. If so the edge
    /// is aligned (`Eval-scrubEdge`; `CT-dirtyIntoClean`) and the walk goes on; if not the node
    /// is misaligned (`CT-misaligned`) and the next step re-evaluates it.
    fn repair_resume(&mut self, counts: &mut Counts, value: Value_) -> Res<()> {
        let mut rf = match self.stack.back().map(|frame| frame.kind.clone()) {
            Some(FrameKind::Repair(rf)) => rf,
            _ => return Err(Error::Internal(line!())),
        };
        let (edge_id, seen) = rf.awaiting.take().ok_or(Error::Internal(line!()))?;
        if value == seen {
            self.set_align(&edge_id, Align::Aligned);
            counts.edges_aligned += 1;
            rf.index += 1;
        } else {
            rf.misaligned = true;
        }
        self.replace_repair_frame(rf);
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
        let new_node = Self::new_node(space, value);
        self.extend_history_with_new_node(&pointer, Some(&time), &new_node);
        let updated = self
            .get_node_by_space(&time)
            .update((pointer, meta_time), new_node);
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
        Ok(self.history.clone().into_value_())
    }

    fn history(&self) -> Res<History> {
        // Three `im_rc::Vector` clones: reference bumps, whatever the history holds.
        Ok(self.history.clone())
    }
}
