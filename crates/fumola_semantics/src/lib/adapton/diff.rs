//! Comparing two demanded computation graphs, without leaving Rust.
//!
//! # Why this exists
//!
//! Measuring realignment means diffing two runs' node maps: for each pointer both runs named,
//! did they agree? The Fumola library has done this since #43 -- `A.Diff.nodeValsFromNodes` and
//! `A.Diff.nodeValsDiff` -- and it works, but every node has to cross the boundary first. A
//! node's value is a thunk body, which is a whole closed expression with its environment;
//! `peek_value` turns each one into a `Value::Thunk`, the Fumola code puts it in a `hashMap`
//! collection, and the comparison then runs in the interpreter. At n = 1000 that is ~19,000
//! nodes converted and boxed per run, twice per sample, before a single comparison happens.
//!
//! The graph is already in Rust, in `im_rc` structures that clone by reference. This module
//! reads it where it lies. `mergeSort` at n = 1000, seed 10, removing the highest-level cell;
//! release build, medians of three, `/usr/bin/time`:
//!
//! | | wall | peak RSS |
//! |---|---|---|
//! | two runs, no diff | 28.78 s | 478 MB |
//! | + the Fumola-side diff | 31.85 s | 1472 MB |
//! | + this one | 28.93 s | 575 MB |
//!
//! So the diffing itself went from 3.07 s and 994 MB to **0.15 s and 97 MB**: 20x faster, and a
//! tenth of the memory. On a whole sample the two runs dominate the clock, so a sample is only
//! 1.10x faster -- what it is, is 2.56x smaller in peak memory, and memory is what killed the
//! sweeps. `docs/native-diff.md` has the other sizes, the noise floor, and the harness.
//!
//! # What it does not replace
//!
//! The Fumola-side diff answers with the four *maps*, and `webPlay.pair` draws from them: which
//! cells differ is what a comparative example colours. This module answers with counts. Keeping
//! both is deliberate -- drawing wants the maps, and measurement wants numbers and no
//! allocation -- and `A.Native` says so where it is defined.

use im_rc::HashMap;
use serde::{Deserialize, Serialize};

use crate::adapton::Pointer;
use crate::adapton::graphical::{History, Node};
use crate::value::{ThunkBody, Value_};

/// What a node is worth comparing by: its contents, with the transient parts left out.
///
/// The Rust twin of `A.Diff.NodeVal`, and the same reading of it -- edge ids and meta-times are
/// transient and decide no reuse, so a node is its value, or its body and the result it cached.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum NodeVal {
    NonThunk(Value_),
    Thunk(ThunkBody, Option<Value_>),
}

impl NodeVal {
    pub fn of_node(node: &Node) -> Self {
        match node {
            Node::NonThunk(v) => NodeVal::NonThunk(v.clone()),
            Node::Thunk(t) => {
                NodeVal::Thunk(t.body.clone(), t.result.as_ref().map(|(_, v)| v.clone()))
            }
        }
    }
}

/// Every pointer a run named, and what it last held.
///
/// A wrapper rather than a bare `HashMap`, so that it can be a `Value` variant: the derives a
/// value form needs live here.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeVals {
    #[serde(with = "crate::serde_utils::im_rc_hashmap")]
    pub map: HashMap<Pointer, NodeVal>,
}

impl NodeVals {
    /// The node map of a history: the last version of every pointer it recorded.
    ///
    /// Walks the node history in order and lets a later version supersede an earlier one, which
    /// is what `A.Diff.nodeValsFromNodes`'s `Map.put` loop does.
    pub fn of_history(history: &History) -> Self {
        let mut map = HashMap::new();
        for item in history.nodes.iter() {
            map.insert((item.node_id.0).clone(), NodeVal::of_node(&item.node));
        }
        NodeVals { map }
    }

    pub fn size(&self) -> usize {
        self.map.len()
    }
}

/// What two runs' node maps say when compared, as numbers.
///
/// The four categories are `A.Diff.NodeValsDiff`'s, and `not_equal` is split three ways, which
/// the Fumola side cannot do without walking the pairs itself. The split is the distinction
/// realignment turns on: a thunk whose *body* differs was named again for a different closure,
/// so a repair must evaluate it like a cell with no counterpart; a thunk whose body is the same
/// and whose *result* differs is repair proper. Measured on `mergeSort` at n = 1000, 99% of
/// `not_equal` is `not_equal_new_body`, which is why the distinction was worth having.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiffCounts {
    pub left: usize,
    pub right: usize,
    pub only_left: usize,
    pub only_right: usize,
    pub equal: usize,
    pub not_equal: usize,
    /// Both named it; both are thunks; the bodies differ. A re-allocation under one name.
    pub not_equal_new_body: usize,
    /// Both named it; both are thunks; same body, different cached result. Repair's own case.
    pub not_equal_same_body: usize,
    /// Both named it and at least one is not a thunk: a cell whose contents changed, or a
    /// pointer that was a cell in one run and a thunk in the other.
    pub not_equal_non_thunk: usize,
}

/// The name a pointer hangs under: the leftmost leaf of its symbol.
///
///    `do within space `mergeSort { ... }` makes every name inside it
/// `mergeSort(...)`, nested to whatever depth the program opened, so the
/// leftmost leaf of the symbol says which phase of a run a node belongs to --
/// the input tree, the merge network, a stage thunk. Descending the left of
/// every composite form, since `within` composes with `Call` but a program may
/// have used any of them.
pub fn head_symbol(space: &Pointer) -> Option<crate::value::Symbol_> {
    fn head(s: &crate::value::Symbol_) -> crate::value::Symbol_ {
        use crate::value::Symbol::*;
        match s.as_ref() {
            Call(x, _) | Dot(x, _) | BinOp(x, _, _) => head(x),
            UnOp(_, x) => head(x),
            _ => s.clone(),
        }
    }
    match space {
        Pointer::Symbol(s) => Some(head(s)),
        _ => None,
    }
}

/// The same comparison, split by the name each pointer hangs under.
///
///    For asking which *phase* of a run an edit disturbed, which a single set
/// of totals cannot say. Sorted by the head's debug rendering, so the order is
/// the same from one run to the next.
pub fn diff_by_space(
    left: &NodeVals,
    right: &NodeVals,
) -> Vec<(Option<crate::value::Symbol_>, DiffCounts)> {
    use std::collections::HashMap as Std;
    let mut groups: Std<Option<String>, (Option<crate::value::Symbol_>, DiffCounts)> = Std::new();
    let empty = || DiffCounts {
        left: 0,
        right: 0,
        only_left: 0,
        only_right: 0,
        equal: 0,
        not_equal: 0,
        not_equal_new_body: 0,
        not_equal_same_body: 0,
        not_equal_non_thunk: 0,
    };
    let mut bump =
        |groups: &mut Std<Option<String>, (Option<crate::value::Symbol_>, DiffCounts)>,
         space: &Pointer,
         f: &dyn Fn(&mut DiffCounts)| {
            let h = head_symbol(space);
            let key = h.as_ref().map(|s| format!("{:?}", s));
            let entry = groups.entry(key).or_insert_with(|| (h, empty()));
            f(&mut entry.1);
        };
    for (pointer, l) in left.map.iter() {
        bump(&mut groups, pointer, &|c| c.left += 1);
        match right.map.get(pointer) {
            None => bump(&mut groups, pointer, &|c| c.only_left += 1),
            Some(r) if l == r => bump(&mut groups, pointer, &|c| c.equal += 1),
            Some(r) => {
                bump(&mut groups, pointer, &|c| c.not_equal += 1);
                match (l, r) {
                    (NodeVal::Thunk(lb, _), NodeVal::Thunk(rb, _)) => {
                        if lb == rb {
                            bump(&mut groups, pointer, &|c| c.not_equal_same_body += 1)
                        } else {
                            bump(&mut groups, pointer, &|c| c.not_equal_new_body += 1)
                        }
                    }
                    _ => bump(&mut groups, pointer, &|c| c.not_equal_non_thunk += 1),
                }
            }
        }
    }
    for (pointer, _) in right.map.iter() {
        bump(&mut groups, pointer, &|c| c.right += 1);
        if !left.map.contains_key(pointer) {
            bump(&mut groups, pointer, &|c| c.only_right += 1);
        }
    }
    let mut out: Vec<(Option<String>, (Option<crate::value::Symbol_>, DiffCounts))> =
        groups.into_iter().collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out.into_iter().map(|(_, v)| v).collect()
}

/// Compare two node maps.
///
/// One pass over the left map, deciding each pointer, and one over the right counting what only
/// it named -- the shape of `A.Diff.nodeValsDiff`, with no maps built.
pub fn diff(left: &NodeVals, right: &NodeVals) -> DiffCounts {
    let mut counts = DiffCounts {
        left: left.size(),
        right: right.size(),
        only_left: 0,
        only_right: 0,
        equal: 0,
        not_equal: 0,
        not_equal_new_body: 0,
        not_equal_same_body: 0,
        not_equal_non_thunk: 0,
    };
    for (pointer, l) in left.map.iter() {
        match right.map.get(pointer) {
            None => counts.only_left += 1,
            Some(r) if l == r => counts.equal += 1,
            Some(r) => {
                counts.not_equal += 1;
                match (l, r) {
                    (NodeVal::Thunk(lb, _), NodeVal::Thunk(rb, _)) => {
                        if lb == rb {
                            counts.not_equal_same_body += 1
                        } else {
                            counts.not_equal_new_body += 1
                        }
                    }
                    _ => counts.not_equal_non_thunk += 1,
                }
            }
        }
    }
    for pointer in right.map.keys() {
        if !left.map.contains_key(pointer) {
            counts.only_right += 1;
        }
    }
    counts
}
