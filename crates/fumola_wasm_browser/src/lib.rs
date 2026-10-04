//! Fumola's runtime for JavaScript: each function of `fumola_wasm_common`
//! exported with `#[wasm_bindgen]`, and nothing more. The behaviour, and the
//! documentation of each function, are in that crate; the Internet Computer
//! canister (`fumola_canister`) serves the same functions over HTTP.
//!
//! The published artifacts keep the name `fumola_wasm.js` /
//! `fumola_wasm_bg.wasm` (wasm-bindgen `--out-name fumola_wasm`): every Hazel
//! build loads them by those URLs.

use fumola_wasm_common as common;
use fumola_wasm_common::FumolaInstanceId;
use wasm_bindgen::prelude::*;

/// Each line exports the `fumola_wasm_common` function of that name and
/// signature to JavaScript, unchanged.
macro_rules! export {
    ($(fn $name:ident($($arg:ident: $ty:ty),*) $(-> $ret:ty)?;)*) => {
        $(
            #[wasm_bindgen]
            pub fn $name($($arg: $ty),*) $(-> $ret)? {
                common::$name($($arg),*)
            }
        )*
    };
}

export! {
    fn fumola_symbol_of(source: &str) -> String;
    fn fumola_create() -> FumolaInstanceId;
    fn fumola_has(id: FumolaInstanceId) -> bool;
    fn fumola_realize(id: FumolaInstanceId) -> bool;
    fn fumola_ensure_mode(id: FumolaInstanceId, mode: &str) -> String;
    fn fumola_mode(id: FumolaInstanceId) -> String;
    fn fumola_drop(id: FumolaInstanceId);
    fn fumola_steps_taken(id: FumolaInstanceId) -> usize;
    fn fumola_instance_count() -> usize;
    fn fumola_stats(id: FumolaInstanceId) -> String;
    fn fumola_instances() -> String;
    fn fumola_eval(id: FumolaInstanceId, thunk_name: &str, program_text: &str) -> String;
    fn fumola_eval_top(id: FumolaInstanceId, program_text: &str) -> String;
    fn fumola_eval_scratch(id: FumolaInstanceId, program_text: &str) -> String;
    fn fumola_eval_top_limited(id: FumolaInstanceId, program_text: &str, steps: usize) -> String;
    fn fumola_resume(id: FumolaInstanceId, steps: usize) -> String;
    fn fumola_abandon(id: FumolaInstanceId) -> String;
    fn fumola_reset(id: FumolaInstanceId) -> bool;
    fn fumola_symbol_source(symbol_json: &str) -> String;
    fn fumola_get(id: FumolaInstanceId, symbol_json: &str) -> String;
    fn fumola_tokens(source: &str) -> String;
    fn fumola_modules() -> String;
    fn fumola_module_source(path: &str) -> String;
}
