// (ticket 590) What the Core keeps of each definition, for an outline: a
// type declaration (kept in its context's own namespace, as written), a
// `let`'s binder with its annotation, and a function with its parameters, each
// with the parser's source (line and column, start and end).
use fumola::state::State;
use fumola_semantics::vm_types::def::Ctx;
use fumola_syntax::ast::{Pat, Source, ToId};

use test_log::test; // enable logging output for tests by default.

const M: &str = "module M {
  public type Side = {#L; #R};
  public type Pair<A> = (A, A);
  public let limit : Nat = 3;
  public func flip(s : Side) : Side { switch s { case (#L) #R; case (#R) #L } };
};
0";

fn module_ctx(core: &State) -> Ctx {
    core.semantic_state
        .defs
        .map
        .values()
        .find(|c| c.fields.contains_key(&"flip".to_id()))
        .expect("the module's context")
        .clone()
}

fn lines(s: &Source) -> (usize, usize, usize, usize) {
    match s {
        Source::Known(k) => (k.start_line, k.start_col, k.end_line, k.end_col),
        other => panic!("a parser's source, not {:?}", other),
    }
}

#[test]
fn a_type_declaration_is_kept_with_its_source() {
    let mut core = State::empty();
    core.eval(M).expect("the module defined");
    let ctx = module_ctx(&core);
    let side = ctx.types.get(&"Side".to_id()).expect("type Side kept");
    assert_eq!(lines(&side.source).0, 2);
    assert_eq!(lines(&side.name.1).0, 2);
    assert!(side.vis.is_some());
    let pair = ctx.types.get(&"Pair".to_id()).expect("type Pair kept");
    assert_eq!(lines(&pair.source).0, 3);
    let binds = pair.binds.as_ref().expect("its type parameter kept");
    assert_eq!(binds.vec.len(), 1);
    assert_eq!(binds.vec[0].0.var.0, "A".to_id());
    assert_eq!(lines(&binds.vec[0].1).0, 3);
    // types are not values: no field named for them
    assert!(!ctx.fields.contains_key(&"Side".to_id()));
}

#[test]
fn a_let_keeps_its_binder_and_annotation() {
    let mut core = State::empty();
    core.eval(M).expect("the module defined");
    let ctx = module_ctx(&core);
    let limit = ctx.fields.get(&"limit".to_id()).expect("limit");
    assert_eq!(lines(&limit.source).0, 4);
    let pat = limit.pat.as_ref().expect("its binder kept");
    assert!(
        matches!(pat.0, Pat::AnnotPat(..)),
        "with its annotation: {:?}",
        pat
    );
    let (l, c, _, _) = lines(&pat.1);
    assert_eq!((l, c > 0), (4, true));
}

#[test]
fn a_function_keeps_its_parameters_and_result_with_sources() {
    let mut core = State::empty();
    core.eval(M).expect("the module defined");
    let ctx = module_ctx(&core);
    let flip = ctx.fields.get(&"flip".to_id()).expect("flip");
    assert_eq!(lines(&flip.source).0, 5);
    match &flip.def {
        fumola_semantics::vm_types::def::Def::Func(f) => {
            let name = f.function.name.as_ref().expect("named");
            assert_eq!(lines(&name.1).0, 5);
            assert_eq!(lines(&f.function.input.1).0, 5);
            let out = f.function.output.as_ref().expect("its result annotation");
            assert_eq!(lines(&out.1).0, 5);
        }
        other => panic!("a function, not {}", other.kind_name()),
    }
}

#[test]
fn a_type_declared_twice_is_ambiguous() {
    let mut core = State::empty();
    let r = core.eval("module M { public type T = Nat; public type T = Int }; 0");
    assert!(r.is_err(), "two types of one name in one module");
}

#[test]
fn a_generic_function_keeps_and_prints_its_type_parameters() {
    let mut core = State::empty();
    core.eval("module G { public func first<A>(p : (A, A)) : A { p.0 } }; 0")
        .expect("defined");
    let ctx = core
        .semantic_state
        .defs
        .map
        .values()
        .find(|c| c.fields.contains_key(&"first".to_id()))
        .expect("G's context")
        .clone();
    match &ctx.fields.get(&"first".to_id()).expect("first").def {
        fumola_semantics::vm_types::def::Def::Func(f) => {
            let b = f.function.binds.as_ref().expect("<A> kept");
            assert_eq!(b.vec[0].0.var.0, "A".to_id());
        }
        other => panic!("a function, not {}", other.kind_name()),
    }
    // and it still runs as before
    assert_eq!(
        core.eval("G.first<Nat>((1, 2))").expect("called"),
        fumola::eval::eval("1").expect("1")
    );
}
