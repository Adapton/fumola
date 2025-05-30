use fumola::shared::Share;
use fumola::value::Value;
use fumola::vm_types::{Core, Limits};
use fumola_proc_macro::parse_static;
use num_bigint::BigUint;

use test_log::test; // enable logging output for tests by default.

#[ignore]
#[test]
fn test_eval_open_block() {
    let mut core = Core::new(parse_static!("let x = 666; let y = 777;").clone());
    core.run(&Limits::none()).unwrap();
    core.eval_open_block(
        vec![
            ("x", Value::Nat(BigUint::from(1_u32)).share()),
            ("y", Value::Nat(BigUint::from(2_u32)).share()),
        ],
        parse_static!("var z = x + y").clone(),
    )
    .unwrap();
    let r = core.eval_prog(parse_static!("x + y").clone()).unwrap();
    assert_eq!(r, Value::Nat(BigUint::from(666 + 777_u32)).share());
}

#[ignore]
#[test]
fn test_hashmap_performance_steps() {
    // init core with empty hashmap.
    let mut core = Core::new(
        parse_static!(
            "
      var map = prim \"hashMapNew\" ();
   "
        )
        .clone(),
    );
    core.run(&Limits::none()).unwrap();

    // generate initial data / batch random put.
    let size = 10;
    core.eval_open_block(
        vec![("size", Value::Nat(BigUint::from(size as u32)).share())],
        parse_static!(
            "
      var i = prim \"fastRandIterNew\" (?size, 1);
      let j = {
        next = func () { prim \"fastRandIterNext\" i }
        };
      for (x in j) {
        let s = prim \"natToText\" x;
        let (m, _) = prim \"hashMapPut\" (map, x, s);
        map := m;
      }
    "
        )
        .clone(),
    )
    .unwrap();
}
