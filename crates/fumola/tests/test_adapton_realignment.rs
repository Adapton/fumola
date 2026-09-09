//! Realignment: signaling when a put changes a cell that has readers, and repair when a thunk
//! whose trace holds a signaled edge is demanded. Each program is the smallest that shows one
//! behaviour; the counts it checks are the reserved symbols under `` `adapton(`counts) ``.
//!
//! The vocabulary is Fumola's: signaling / signaled / aligned / repair / realignment for the
//! papers' dirtying / dirty / clean / cleaning / change propagation. Algorithm 1 is the PLDI 2014
//! Adapton paper's; the `Eval-*` rules are Nominal Adapton's (OOPSLA 2015, Figure 5).
use fumola::check::assert_vm_eval as assert_;
use fumola_semantics::adapton::{MetaTime, Space, Time};
use fumola_semantics::value::Symbol;
use fumola_semantics::vm_types::Interruption;
use fumola_syntax::ast::Id;

/// The space a bare name denotes: `` `k `` is `Space::Symbol(Symbol::Id("k"))`.
fn adapton_space(name: &str) -> Space {
    Space::Symbol(fumola_semantics::Shared::new(Symbol::Id(Id::new(
        name.to_owned(),
    ))))
}

/// The spreadsheet of the PLDI 2014 paper's Figure 1, in three cells: change an input, demand
/// the output.
#[test]
fn a_changed_cell_reaches_its_reader_on_the_next_force() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { (@ x) + 1 }; let before = force t; x := 5; (before, force t)",
        "(2, 6)",
    )
}

/// Algorithm 1 line 12, with equal values: `a` re-evaluates to what it was, so the edge from `t`
/// to `a` realigns and `t` is not re-evaluated.
#[test]
fn repair_stops_where_a_value_is_unchanged() {
    assert_(
        "let x = `x := 1; let a = `a := thunk { (@ x) * 0 }; let t = `t := thunk { (force a) + 1 };
         let before = force t; x := 7; let after = force t;
         (before, after, @(`adapton(`counts)(`reevaluations)), @(`adapton(`counts)(`edgesAligned)))",
        "(1, 1, 1, 1)",
    )
}

/// `Eval-refClean`: a put of what the cell holds adds an allocation edge and nothing else.
#[test]
fn a_matched_put_makes_no_version_and_signals_nothing() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { @ x }; let _ = force t; x := 1;
         (@(`adapton(`counts)(`putMatched)), @(`adapton(`counts)(`signalings)),
          @(`adapton(`counts)(`cells)), force t, @(`adapton(`counts)(`repairs)))",
        "(1, 0, 2, 1, 0)",
    )
}

/// The setting turns the match off: an equal put is then a new version (three cells, not two)
/// and starts a signaling traversal -- but the reader's edge recorded the value the cell holds
/// again, so the recipe's `DT-stillClean` leaves it aligned, and the next force is a plain hit.
/// Signaling compares; the match only spares the version.
#[test]
fn without_matching_an_equal_put_is_a_version_but_signals_no_edge() {
    assert_(
        "`adapton(`settings)(`putMatchesEqualValues) := false;
         let x = `x := 1; let t = `t := thunk { @ x }; let _ = force t; x := 1;
         (@(`adapton(`counts)(`putMatched)), @(`adapton(`counts)(`signalings)),
          @(`adapton(`counts)(`edgesSignaled)), @(`adapton(`counts)(`cells)), force t,
          @(`adapton(`counts)(`repairs)))",
        "(0, 1, 0, 3, 1, 0)",
    )
}

/// A cell nobody has read has nothing to signal.
#[test]
fn a_put_to_a_cell_without_readers_signals_nothing() {
    assert_("`x := 1; `x := 2; @(`adapton(`counts)(`signalings))", "0")
}

/// Algorithm 1 line 3: signaling stops at an edge already signaled. Two edits before one demand
/// both start a traversal, but the second finds the reader's edge marked and marks nothing.
#[test]
fn two_edits_before_a_demand_mark_each_edge_once() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { @ x }; let _ = force t; x := 2; x := 3;
         (@(`adapton(`counts)(`signalings)), @(`adapton(`counts)(`edgesSignaled)), force t)",
        "(2, 2, 3)",
    )
}

/// A diamond: `t` forces `a` and `b`, both of which read `x`. One edit; each of the three is
/// re-evaluated exactly once -- `b` from inside `t`'s re-evaluation, whose own force of `b`
/// meets it signaled (Algorithm 1 line 14 runs the body, and the body forces).
#[test]
fn a_diamond_reevaluates_each_node_once() {
    assert_(
        "let x = `x := 1; let a = `a := thunk { (@ x) + 1 }; let b = `b := thunk { (@ x) + 2 };
         let t = `t := thunk { (force a) + (force b) }; let before = force t; x := 10;
         (before, force t, @(`adapton(`counts)(`reevaluations)))",
        "(5, 23, 3)",
    )
}

/// `Eval-thunkDirty`: a thunk put again with a different body signals its forcers, which see
/// the new body on their next force.
#[test]
fn a_thunk_put_again_with_a_new_body_is_seen_by_its_forcers() {
    assert_(
        "let a = `a := thunk { 1 }; let t = `t := thunk { (force a) * 10 }; let before = force t;
         `a := thunk { 2 }; (before, force t)",
        "(10, 20)",
    )
}

/// A chain of three thunks over one cell: the edit reaches the far end, and every thunk is
/// re-evaluated once, innermost first (Algorithm 1 line 11 before line 12).
#[test]
fn a_chain_repairs_from_the_input_outward() {
    assert_(
        "let x = `x := 1; let a = `a := thunk { (@ x) + 1 }; let b = `b := thunk { (force a) + 1 };
         let c = `c := thunk { (force b) + 1 }; let before = force c; x := 100;
         (before, force c, @(`adapton(`counts)(`reevaluations)))",
        "(4, 103, 3)",
    )
}

/// `del-edges-out`: a re-evaluated thunk's old trace leaves the live graph, and the history
/// says so with `removeEdge` events -- one here, for `t`'s single `get`.
#[test]
fn reevaluation_removes_the_old_trace() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { @ x }; let _ = force t; x := 2; let _ = force t;
         let g = switch (@(`adapton(`state))) { case (#Graphical(g)) g };
         var removed = 0;
         for (e in g.history.events.vals()) {
           switch (e.event) { case (#RemoveEdge(_)) { removed += 1 }; case _ {} }
         };
         removed",
        "1",
    )
}

/// The two traversals are bracketed in the history, and the brackets nest the way the work
/// does: one signaling for the edit, then a repair of `t` that re-evaluates.
#[test]
fn the_history_brackets_signaling_and_repair() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { (@ x) + 1 }; let _ = force t; x := 5; let _ = force t;
         let g = switch (@(`adapton(`state))) { case (#Graphical(g)) g };
         var tags = \"\";
         for (e in g.history.events.vals()) {
           switch (e.event) {
             case (#SignalingBegin(_)) { tags #= \"S[\" };
             case (#EdgeSignaled(_)) { tags #= \"s\" };
             case (#SignalingEnd(_)) { tags #= \"]\" };
             case (#RepairBegin(_)) { tags #= \"R[\" };
             case (#EdgeAligned(_)) { tags #= \"a\" };
             case (#RemoveEdge(_)) { tags #= \"x\" };
             case (#RepairEnd(_, #Aligned)) { tags #= \"=]\" };
             case (#RepairEnd(_, #Reevaluated)) { tags #= \"!]\" };
             case _ {}
           }
         };
         tags",
        "\"S[ss]R[x!]\"",
    )
}

/// A thunk that writes the cell it has just read edits its own input, so its second force is
/// not a cache hit: the write signals the thunk's own read edge, and the force repairs it by
/// re-evaluation. This program was `test_adapton::force_simple_cache_hit`, expecting `(1, 0)`
/// -- the answer before repair existed. `(1, 1)` is what a from-scratch run gives, and what the
/// recipe's `redoForce` gives.
#[test]
fn a_thunk_that_edits_its_own_input_is_repaired_on_its_next_force() {
    assert_(
        "let count = 1 := 0; let myThunk = 2 := thunk { let orig = @ count; count := 1 + (@ count); orig }; force(myThunk); (@ count, force(myThunk))",
        "(1, 1)",
    )
}

/// The simple strategy keeps no graph, so it has nothing to signal and never repairs: the same
/// program hands back the stale cache. Pinned so the divergence is a stated fact rather than a
/// surprise -- the simple strategy is a memo table, not the recipe's reference semantics, whose
/// `redoForce` would re-run.
#[test]
fn the_simple_strategy_returns_the_stale_cache_for_that_program() {
    assert_(
        "prim \"adaptonReset\" (#simple); let count = 1 := 0; let myThunk = 2 := thunk { let orig = @ count; count := 1 + (@ count); orig }; force(myThunk); (@ count, force(myThunk))",
        "(1, 0)",
    )
}

// ---- the double use of one name ------------------------------------------------
//
// Nominal Adapton (OOPSLA 2015) refuses a run that allocates two different things under one
// name: `Eval-thunkDirty` dirties every path into the re-allocated pointer, and the
// `all-clean-out` premise of `Eval-computeDep` then fails as the node is popped. The graphical
// cache asks the same question at the put -- does a thunk whose body is running hold an
// allocation of this name recording something else? -- so the error names the put that did it.
//
// These programs bind their pointers from `:=` and read them with `@`, because the bare `eval`
// harness has no prelude to supply `peek`, and a bare `` `k `` is a QuotedAst rather than a
// Symbol, which `@` does not coerce.

/// The shape the check exists for: one evaluation minting a name it has already used.
#[test]
fn a_thunk_that_allocates_one_name_twice_is_refused() {
    fumola::check::assert_vm_interruption(
        "force(`t := thunk { `k := 1; `k := 2 })",
        &Interruption::AdaptonError(fumola_semantics::adapton::Error::DoubleUse {
            pointer: adapton_space("k"),
            observer: (adapton_space("t"), Time::Now, MetaTime(1u32.into())),
        }),
    )
}

/// Allocating *the same* thing twice under one name is not a double use: it is Nominal
/// Adapton's `Eval-refClean`, and here a matched put, which returns before the check.
#[test]
fn allocating_the_same_value_twice_is_a_matched_put() {
    assert_(
        "let v = force(`t := thunk { `k := 1; let p = `k := 1; @ p });
         (v, @(`adapton(`counts)(`putMatched)), @(`adapton(`counts)(`doubleUses)))",
        "(1, 1, 0)",
    )
}

/// An editor's put over a thunk's allocation is an edit, not a double use. The editor owns the
/// cells it edits, whoever allocated them -- which is what realignment is for.
#[test]
fn an_editors_put_over_a_thunks_allocation_is_an_edit() {
    assert_(
        "let t = `t := thunk { `k := 1; 7 }; let _ = force t; let k = `k := 2;
         (@ k, @(`adapton(`counts)(`doubleUses)))",
        "(2, 0)",
    )
}

/// And a computation's put over the *editor's* allocation is not one either: the editor's
/// allocation edge leaves the root node, which no repair can ever re-run. (Marking it was the
/// first thing tried here, and `adapton.testPeekEvents` caught it: the events it added were
/// events for a repair that can never happen.)
#[test]
fn a_computations_put_over_the_editors_allocation_is_not_a_double_use() {
    assert_(
        "let k = `k := 1; let _ = force(`t := thunk { `k := 2; 7 });
         (@ k, @(`adapton(`counts)(`doubleUses)))",
        "(2, 0)",
    )
}

/// A re-evaluation re-putting what it allocated last time is not a double use. The version on
/// the stack is the new one, and the old version's edges left the graph when repair dropped its
/// trace -- so nothing matches. This is the case that would have made the check unusable had it
/// matched on the pointer rather than on the node version.
#[test]
fn a_reevaluation_re_putting_its_own_allocation_is_not_a_double_use() {
    assert_(
        "let x = `x := 1; let t = `t := thunk { let p = `k := (@ x) * 10; @ p };
         let before = force t; x := 2; let after = force t;
         (before, after, @(`adapton(`counts)(`doubleUses)), @(`adapton(`counts)(`reevaluations)))",
        "(10, 20, 0, 1)",
    )
}

/// The gap, pinned rather than hidden: two thunks that have each returned, having allocated
/// different things under one name, are *not* caught. Nominal Adapton catches this through the
/// marking it does of allocation edges and the check it makes as a node is popped; this cache
/// marks no allocation edge (module notes, items 3 and 8). The same gap covers an inner thunk's
/// allocation that its own caller later overwrites.
#[test]
fn two_returned_thunks_colliding_on_one_name_are_not_yet_caught() {
    assert_(
        "let a = force(`a := thunk { `k := 1; 1 });
         let b = force(`b := thunk { let p = `k := 2; @ p });
         (a, b, @(`adapton(`counts)(`doubleUses)))",
        "(1, 2, 0)",
    )
}

/// The setting turns the refusal off, and the put goes through as an edit would.
#[test]
fn the_check_can_be_turned_off() {
    assert_(
        "`adapton(`settings)(`checkDoubleUse) := false;
         force(`t := thunk { `k := 1; let p = `k := 2; @ p })",
        "2",
    )
}
