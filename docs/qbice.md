# The same lineage, without the names

A note on QBICE, read against `crates/fumola_semantics/src/lib/adapton`.

QBICE -- the Query-Based Incremental Computation Engine,
[github.com/Simmypeet/qbice](https://github.com/Simmypeet/qbice), MIT, first
released 2025-12-09 and at 0.6.9 since 2026-08-29 -- is a Rust incremental
computation framework whose README names Adapton and Salsa as its inspirations.
Its book credits Adapton with the demanded computation graph itself, and with
dirtying. It is the closest thing to a contemporary of Fumola that exists on
crates.io, and it is worth reading precisely because it takes the same starting
point and arrives somewhere else.

Everything below is from its book (`book/src/`) and its engine
(`crates/qbice/src/engine/`), at commit `cc151f3`. Nothing here was run.

## Not the same kind of artifact

QBICE is a library. You embed an `Engine`, you declare each computation as a
Rust type implementing `Query`, and an `Executor` says how to compute it. The
graph is the engine's private index; `visualization.rs` renders it as HTML when
you want to debug.

Fumola is a language whose execution model is the graph. The graph is what a
program leaves behind, printable, diffable, serializable as a Scene.

Most of what follows is that difference, worked out.

| | QBICE | Fumola |
|---|---|---|
| identity | `QueryID` = stable type id + 128-bit hash of contents | `Space::Symbol` -- a name the program chose |
| cutoff | fingerprints at repair; `Firewall` and `Projection` opt-ins | compared at the `put`; a `Put` edge never signals |
| repair | `async fn repair_query`, suspended by Tokio | `RepairStep`, stepped by the VM |
| the first traversal | eager, on a work-stealing pool, after a session commits | synchronous, at the put |
| concurrency | async throughout, `Arc<Engine>`, snapshots, lock manager | single-threaded, `Rc` and `im_rc` |
| across runs | RocksDB or fjall, stable hashes | in memory; the history is what persists |
| cycles | detected, reported | not detected (`graphical/mod.rs`, difference 7) |

## Identity is derived, not chosen

`QueryID::new` builds a key from `Q::STABLE_TYPE_ID` and a 128-bit hash of the
query's own contents. That is the whole of identity in QBICE. A program cannot
say *allocate this under the name `n`*, because there is no place to put the
name: the key is a function of what the thing is.

For the workload QBICE targets this costs nothing. A compiler's keys are stable
on their own -- a file id, an item id -- and hashing them is as good as naming
them. The cost appears exactly where Nominal Adapton appeared: a divide and
conquer over a sequence that gets edited, where the intermediate cells need an
identity that survives an edit the content hash does not preserve.

So `docs/merge-naming.md` has no counterpart in QBICE. Inductive and Cartesian
are two answers to a question a QBICE program cannot ask, and the reason the
question is interesting here -- that nothing else about the program changes when
you change the name -- is the same reason it cannot be asked there. There is
nothing to vary.

The other half of that trade: QBICE cannot raise `Error::DoubleUse`. One key is
one content, by construction, so a run cannot use one name for two things. It
also cannot deliberately reuse a name for a changed thing, which is how a
Fumola edit finds the work it should reuse.

## Firewalls, and what they are standing in for

`input_session.rs` fingerprints a new input against the stored one and only
dirties when they differ, so the first hop agrees with Fumola: a put that
changes nothing signals nothing.

Past that hop the two part. QBICE marks transitively along every backward edge.
`ExecutionStyle::Firewall` is how a program declares a node where that marking
should stop until the value is known to have changed, and the book motivates it
by cost: a global analysis with a hundred dependents otherwise marks all
hundred, and all hundred then need verifying. Fumola never signals a `Put` edge
at all, and past the first hop follows forces only -- a distinction the reading
guide justifies as meaning rather than as tuning, an allocation being no
observation of what the cell later holds.

`ExecutionStyle::Projection` is the tell. It exists so that a large firewall
result -- their example is one type table -- does not over-invalidate readers
who each want one slice of it. Nominal Adapton's answer to that problem is not
to put the table in one cell: spread it over named cells and the graph is
fine-grained already. Projection is what you need when you cannot name the
parts. That it then required a third concept, the transitive firewall edge, to
stay correct when a firewall blocks propagation, is the usual shape of a
workaround acquiring its own workaround.

None of this makes their engine wrong. It makes the firewall an opt-in,
per-node, declared version of a thing Fumola does at every put, for free,
because it has names.

## Repair cannot finish inside the graph

Both hit the same wall, the one `ForceBeginResult` is commented with: checking
a force edge means forcing its target, which runs user code, in the middle of a
graph traversal.

QBICE writes repair as an `async fn` and lets Rust suspend it. Fumola reifies
the step -- `RepairStep::Aligned | Force | Reevaluate`, with `repair_resume`
handing back the value -- and the VM stays the driver.

Theirs is less code. Ours is the one that shows up in the history between
`RepairBegin` and `RepairEnd`, that a Scene can draw, and that `do big` could
be checked against. For a language whose point is that the graph is visible,
the reified version is the design, not the overhead.

## What they have that we do not

Results that survive a restart: stable hashing over a pluggable key-value
store, RocksDB or fjall. And real parallelism -- a work-stealing pool
(crossbeam `Injector` and `Stealer`) for the dirtying traversal, callee checks
fanned out across a `JoinSet`, a lock manager and snapshots underneath.

Fumola is `Rc`-bound and reaches neither without becoming a different program.
Neither is a gap in what Fumola is for. But when someone asks why Fumola is not
fast, this is the honest shape of the answer: they built an engine for a
compiler, we built a language for looking at the graph, and the costs land in
different places on purpose.

## A word we now share

Their glossary defines **Repairation** for the lazy verify-and-recompute pass.
Someone reached, independently, for *repair* over *clean*. That is mild
evidence the vocabulary in the reading guide is the natural one, and also a
near-collision to know about before publishing: *repair* now means a related
but narrower thing in a released Rust crate. Realignment, signaling and aligned
remain ours alone.

## What to take

Nothing structural. The firewall and the projection are both answers to
over-invalidation that names already answer here, and adopting either would be
adopting the problem with it.

Two smaller things are worth a look. Their `statistic.rs` counts the dirtying
traversal separately from execution, which is the split `docs/repair-cost`
cares about. And their persistence question -- what it takes for a cached
result to be valid in a later process -- is one Fumola has not had to ask, and
will have to if a Scene is ever restored rather than replayed.

## Provenance

Read on 2026-09-12 from a clone of `Simmypeet/qbice` at `cc151f3`: the book,
`crates/qbice/src/query.rs`, and `engine/computation_graph/` -- chiefly
`repair.rs`, `dirty_worker.rs` and `input_session.rs`. 33 published versions
since 2025-12-09, 907 lifetime downloads, essentially one author (49 of the
last 50 commits).
