//! Named Fumola instances, run here for a page that sends its program over.
//!
//! The browser runtime keeps instances by number and lets the page name them
//! (Hazel's `fumola … as myInstance`). Here the name is the address --
//! `POST /i/<name>/<op>` -- and the number is this canister's business, made
//! on first use. Every op is the browser runtime's own function from
//! fumola_wasm_common, so a program answers here exactly as it does in the
//! page; only where it runs is different.
//!
//! These instances are separate from the store (`store.rs`) that holds the
//! spaces' keys, and are not kept across an upgrade: a Fumola state is not
//! serializable, and an instance is rebuilt by running its program again,
//! which is what its page does anyway.

use std::cell::RefCell;
use std::collections::BTreeMap;

use fumola_wasm_common as common;
use fumola_wasm_common::FumolaInstanceId;

thread_local! {
    static NAMES: RefCell<BTreeMap<String, FumolaInstanceId>> = RefCell::new(BTreeMap::new());
}

/// The same rule as a space's name: it travels in a URL path.
pub fn valid_name(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 64
        && s.chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_')
}

fn id_of(name: &str) -> FumolaInstanceId {
    NAMES.with(|n| {
        *n.borrow_mut()
            .entry(name.to_string())
            .or_insert_with(common::fumola_create)
    })
}

/// Run `op` against the instance `name` with the request body as its
/// argument. None for an op there is no such function for.
pub fn call(name: &str, op: &str, body: &str) -> Option<String> {
    let id = id_of(name);
    Some(match op {
        "eval_top" => common::fumola_eval_top(id, body),
        "eval_scratch" => common::fumola_eval_scratch(id, body),
        "ensure_mode" => common::fumola_ensure_mode(id, body),
        "mode" => common::fumola_mode(id),
        "stats" => common::fumola_stats(id),
        "get" => common::fumola_get(id, body),
        "reset" => serde_json::json!({ "reset": common::fumola_reset(id) }).to_string(),
        _ => return None,
    })
}

/// Every instance's name and stats, for `GET /stats`.
pub fn stats() -> Vec<(String, serde_json::Value)> {
    NAMES.with(|n| {
        n.borrow()
            .iter()
            .map(|(name, id)| {
                let v: serde_json::Value = serde_json::from_str(&common::fumola_stats(*id))
                    .unwrap_or(serde_json::Value::Null);
                (name.clone(), v)
            })
            .collect()
    })
}

/// Every instance, by name, for `GET /i`.
pub fn names() -> Vec<String> {
    NAMES.with(|n| n.borrow().keys().cloned().collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_instance_is_made_on_first_use_and_keeps_its_bindings() {
        let v: serde_json::Value =
            serde_json::from_str(&call("t1", "eval_top", "let x = 20").unwrap()).unwrap();
        assert_eq!(v["ok"], true);
        let v: serde_json::Value =
            serde_json::from_str(&call("t1", "eval_top", "x * 2").unwrap()).unwrap();
        assert_eq!(v["value"], "40");
        assert!(names().contains(&"t1".to_string()));
    }

    #[test]
    fn instances_do_not_share_bindings() {
        call("t2", "eval_top", "let y = 1").unwrap();
        let v: serde_json::Value =
            serde_json::from_str(&call("t3", "eval_top", "y").unwrap()).unwrap();
        assert_eq!(v["ok"], false);
    }

    #[test]
    fn an_unknown_op_is_none() {
        assert!(call("t4", "launch", "").is_none());
    }
}
