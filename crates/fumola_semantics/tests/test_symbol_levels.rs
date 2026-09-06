//! Level-tree levels must be the same on every target.
//!
//! `prim "symbolLevel"` turns a symbol's hash into a level, and a level
//! decides a level tree's shape. So a hash that varies by target does not
//! merely reshuffle hash-map buckets -- it builds a different DCG from the
//! same program. It did: `#[derive(Hash)]` writes an enum's discriminant as an
//! `isize`, eight bytes natively and four on wasm32, and the mergeSort and
//! levelTree suites disagreed between the CLI and the browser because of it.
//!
//! These values are pinned because three assertions in the Fumola library are
//! downstream of them:
//!
//!   fumola/collections/levelTree.fumola          testGeom2d
//!   fumola/examples/mergeSort/mergeSort.fumola   testDcgDiffRemove_10_8_4
//!   fumola/examples/mergeSort/mergeSort.fumola   testGenerateSceneFullDemand
//!
//! If this test fails, the hash changed and those three need new numbers.
//! That coupling is the point: it should not be possible to change how a
//! symbol hashes without being told which library tests move with it.

use fumola_semantics::value::Symbol;
use fumola_semantics::vm_prim::geometric_pack;
use num_bigint::BigUint;

/// `symbolLevel` for the symbols `1 .. `10, measured identically on
/// x86_64-unknown-linux-gnu and wasm32-unknown-unknown.
const LEVELS: [u64; 10] = [
    291008, 274496, 1581569, 262212, 286788, 266305, 20481, 12480, 12288, 4,
];

#[test]
fn levels_are_pinned_across_targets() {
    let found: Vec<u64> = (1u32..=10)
        .map(|n| geometric_pack(Symbol::Nat(BigUint::from(n)).portable_hash()))
        .collect();
    assert_eq!(
        found,
        LEVELS.to_vec(),
        "symbol levels moved; fumola/collections/levelTree.fumola and \
         fumola/examples/mergeSort/mergeSort.fumola assert on values derived \
         from these and need updating together"
    );
}

/// The property that makes the levels portable: nothing of the target's
/// pointer width reaches the hasher. Hashing the source rendering is how that
/// is achieved, so a symbol and its rendering must agree.
#[test]
fn the_hash_is_of_the_source_rendering() {
    use std::hash::Hasher;
    let symbol = Symbol::Nat(BigUint::from(1729u32));
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    hasher.write(fumola_semantics::format::format_one_line(&symbol).as_bytes());
    assert_eq!(symbol.portable_hash(), hasher.finish());
}

/// Two symbols that render differently must hash differently, or a level tree
/// would degenerate. (Collisions are permitted -- a level is a hint, never an
/// identity -- but not for values this ordinary.)
#[test]
fn distinct_small_symbols_get_distinct_hashes() {
    let hashes: std::collections::HashSet<u64> = (0u32..64)
        .map(|n| Symbol::Nat(BigUint::from(n)).portable_hash())
        .collect();
    assert_eq!(hashes.len(), 64, "small Nat symbols collided");
}
