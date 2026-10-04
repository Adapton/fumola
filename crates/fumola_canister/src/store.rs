//! Hazel's key-value table, kept as cells in an Adapton DCG.
//!
//! Stage 2: a cell holds the saved value as a Fumola value, not its text.
//! Hazel saves S-expressions, which `sexp` turns into `#atom` / `#list`
//! values on the way in and prints back on the way out; a value that is not
//! one S-expression is kept as a `Text`.
//!
//! One Fumola interpreter state lives for the canister's life. Each Hazel
//! key gets a number, the first time it is saved, and its value lives in the
//! cell named `` `hazel(N) ``: a save is `` `hazel(N) := hazelValue ``, run in
//! that state with the text bound to `hazelValue` from Rust, so Hazel's text
//! is never spliced into Fumola source. Saving to a name that already has a
//! cell makes a new version of it, and anything that read it is signaled, as
//! for any Adapton put. A read forces the cell.
//!
//! Symbols are first-order data -- numbers, names, calls, dots -- and not
//! text, which is why keys are numbered rather than used as names.
//!
//! The DCG lives on the heap. Across upgrades the canister saves the
//! key-to-text pairs (`snapshot`) and replays them as puts (`restore`); the
//! cells' histories are not kept.

use crate::schema;
use crate::sexp;
use fumola::state::State;
use fumola_semantics::value::{Value, Value_};
use std::collections::BTreeMap;

/// A front end's stored keys live in a SPACE: the canister holds many, one
/// per front end or per shared document set, and a key is only a key within
/// its space. Cell numbers are global, so `` `hazel(N) `` names one cell in
/// the one DCG whatever its space, and a Fumola program reads any space's
/// cells the same way.
pub type Space = str;

/// The space Hazel's own front end uses, and where the canister's paths
/// without a space (`/kv`, `/log`, the Candid methods) read and write.
pub const DEFAULT_SPACE: &str = "hazel";

pub struct Store {
    fumola: State,
    /// Each (space, key)'s number, in the order keys were first saved.
    index: BTreeMap<(String, String), u64>,
    /// The reference each put answered, which is how a read finds the cell.
    cells: BTreeMap<(String, String), Value_>,
    next: u64,
}

impl Store {
    pub fn new() -> Store {
        Store {
            fumola: State::empty(),
            index: BTreeMap::new(),
            cells: BTreeMap::new(),
            next: 0,
        }
    }

    fn run(&mut self, program: &str) -> Result<Value_, String> {
        // As the REPL does: a continuation an earlier failure left behind
        // would otherwise be resumed instead of running this program.
        self.fumola.semantic_state.clear_cont();
        self.fumola.eval(program).map_err(|e| format!("{:?}", e))
    }

    fn number_of(&mut self, space: &Space, key: &str) -> u64 {
        let k = (space.to_string(), key.to_string());
        if let Some(n) = self.index.get(&k) {
            return *n;
        }
        let n = self.next;
        self.next += 1;
        self.index.insert(k, n);
        n
    }

    pub fn put(&mut self, space: &Space, key: &str, value: &str) -> Result<(), String> {
        let n = self.number_of(space, key);
        // Hazel's own datatype where the canister knows it (schema), else a
        // generic S-expression value, else the text.
        let structured: Value_ = match sexp::parse(value) {
            Some(s) => schema::for_key(key)
                .and_then(|ty| schema::decode_exact(ty, &s))
                .unwrap_or_else(|| sexp::to_value(&s)),
            None => Value::Text(value.into()).into(),
        };
        self.fumola.semantic_state.define("hazelValue", structured);
        let cell = self.run(&format!("`hazel({}) := hazelValue", n))?;
        // Bound by number too, so a program can read any of Hazel's cells,
        // not only the last one read: `@ hazelCell3`. GET /index maps keys
        // to numbers.
        self.fumola
            .semantic_state
            .define(format!("hazelCell{}", n), cell.clone());
        self.cells
            .insert((space.to_string(), key.to_string()), cell);
        Ok(())
    }

    pub fn get(&mut self, space: &Space, key: &str) -> Option<String> {
        let cell = self
            .cells
            .get(&(space.to_string(), key.to_string()))?
            .clone();
        self.fumola.semantic_state.define("hazelCell", cell);
        let v = self.run("@ hazelCell").ok()?;
        if let Some(s) = schema::for_key(key).and_then(|ty| schema::encode(ty, &v)) {
            return Some(sexp::print(&s));
        }
        match sexp::from_value(&v) {
            Some(s) => Some(sexp::print(&s)),
            None => match &*v {
                Value::Text(t) => Some(t.to_string()),
                _ => None,
            },
        }
    }

    /// The key keeps its number, so a later save reuses the same cell name.
    pub fn remove(&mut self, space: &Space, key: &str) {
        self.cells.remove(&(space.to_string(), key.to_string()));
    }

    /// Empties one space; the others are untouched.
    pub fn clear(&mut self, space: &Space) {
        self.cells.retain(|(s, _), _| s != space);
    }

    /// Each key's cell number in [space]: `` `hazel(N) `` names its cell.
    pub fn index(&self, space: &Space) -> Vec<(String, u64)> {
        self.cells
            .keys()
            .filter(|(s, _)| s == space)
            .filter_map(|k| self.index.get(k).map(|n| (k.1.clone(), *n)))
            .collect()
    }

    pub fn keys(&self, space: &Space) -> Vec<String> {
        self.cells
            .keys()
            .filter(|(s, _)| s == space)
            .map(|(_, k)| k.clone())
            .collect()
    }

    pub fn all(&mut self, space: &Space) -> Vec<(String, String)> {
        let keys = self.keys(space);
        keys.into_iter()
            .filter_map(|k| self.get(space, &k).map(|v| (k, v)))
            .collect()
    }

    /// Every space holding a key, with how many it holds.
    pub fn spaces(&self) -> Vec<(String, usize)> {
        let mut counts: BTreeMap<String, usize> = BTreeMap::new();
        for (s, _) in self.cells.keys() {
            *counts.entry(s.clone()).or_insert(0) += 1;
        }
        counts.into_iter().collect()
    }

    /// How many keys there are, over every space.
    pub fn key_count(&self) -> usize {
        self.cells.len()
    }

    /// How many values are stored as Hazel's own datatypes, how many as
    /// generic S-expression values, and how many as plain text.
    pub fn shapes(&mut self) -> (usize, usize, usize) {
        let keys: Vec<(String, String)> = self.cells.keys().cloned().collect();
        let mut typed = 0;
        let mut values = 0;
        let mut texts = 0;
        for k in keys {
            let cell = match self.cells.get(&k) {
                Some(c) => c.clone(),
                None => continue,
            };
            self.fumola.semantic_state.define("hazelCell", cell);
            if let Ok(v) = self.run("@ hazelCell") {
                if schema::for_key(&k.1)
                    .and_then(|ty| schema::encode(&ty, &v))
                    .is_some()
                {
                    typed += 1;
                } else if sexp::from_value(&v).is_some() {
                    values += 1;
                } else {
                    texts += 1;
                }
            }
        }
        (typed, values, texts)
    }

    /// How many cells the DCG has made, versions included.
    pub fn cell_count(&mut self) -> String {
        match self.run("@(`adapton(`counts)(`cells))") {
            Ok(v) => fumola_semantics::format::format_one_line(&*v),
            Err(e) => e,
        }
    }

    /// Run a Fumola program in the same state, so it can read Hazel's cells.
    pub fn eval(&mut self, program: &str) -> String {
        match self.run(program) {
            Ok(v) => fumola_semantics::format::format_one_line(&*v),
            Err(e) => format!("error: {}", e),
        }
    }

    /// Every space's key-to-text pairs, for an upgrade.
    pub fn snapshot(&mut self) -> Vec<(String, Vec<(String, String)>)> {
        let spaces: Vec<String> = self.spaces().into_iter().map(|(s, _)| s).collect();
        spaces
            .into_iter()
            .map(|s| {
                let pairs = self.all(&s);
                (s, pairs)
            })
            .collect()
    }

    pub fn restore(&mut self, spaces: Vec<(String, Vec<(String, String)>)>) {
        for (s, pairs) in spaces {
            for (k, v) in pairs {
                let _ = self.put(&s, &k, &v);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const D: &str = DEFAULT_SPACE;

    #[test]
    fn a_save_is_a_cell_and_a_read_forces_it() {
        let mut s = Store::new();
        s.put(D, "settings", "{\"a\": \"quote\\\" and newline\\n\"}")
            .unwrap();
        assert_eq!(
            s.get(D, "settings").as_deref(),
            Some("{\"a\": \"quote\\\" and newline\\n\"}")
        );
        s.put(D, "settings", "two").unwrap();
        assert_eq!(s.get(D, "settings").as_deref(), Some("two"));
        s.put(D, "editors/a b", "x").unwrap();
        assert_eq!(s.all(D).len(), 2);
        // Two keys, three puts: the second put to `settings` is a version.
        assert_eq!(s.cell_count(), "3");
    }

    #[test]
    fn a_fumola_program_reads_hazel_cells() {
        let mut s = Store::new();
        s.put(D, "k", "hello").unwrap();
        // A read binds the cell as hazelCell; a program can then build on it.
        assert_eq!(s.get(D, "k").as_deref(), Some("hello"));
        assert_eq!(
            s.eval(
                "let t = `view := thunk { switch (@ hazelCell) { case (#atom(x)) { x }; case _ { \"?\" } } }; force t"
            ),
            "\"hello\""
        );
    }

    #[test]
    fn a_sexp_is_stored_as_a_value_and_printed_back() {
        let mut s = Store::new();
        s.put(D, "MODE", "Documentation").unwrap();
        s.put(D, "SETTINGS", "((theme Dark)(probes(\"a b\" c)))")
            .unwrap();
        s.put(D, "NOTE", "not ( a sexp").unwrap();
        assert_eq!(s.get(D, "MODE").as_deref(), Some("Documentation"));
        // Printed as Sexplib prints it: no space after a quoted atom.
        assert_eq!(
            s.get(D, "SETTINGS").as_deref(),
            Some("((theme Dark)(probes(\"a b\"c)))")
        );
        assert_eq!(s.get(D, "NOTE").as_deref(), Some("not ( a sexp"));
        // A program walks the structure: the settings list has two entries,
        // and the theme is the second atom of the first.
        s.get(D, "SETTINGS");
        assert_eq!(
            s.eval(
                "switch (@ hazelCell) { case (#list(xs)) { switch (xs[0]) { \
                 case (#list(kv)) { switch (kv[1]) { case (#atom(t)) { (xs.size(), t) }; \
                 case _ { (0, \"?\") } } }; case _ { (0, \"?\") } } }; case _ { (0, \"?\") } }"
            ),
            "(2, \"Dark\")"
        );
    }

    #[test]
    fn a_program_reads_any_cell_by_number() {
        let mut s = Store::new();
        s.put(D, "MODE", "Documentation").unwrap();
        s.put(D, "SETTINGS", "((a 1)(b 2)(c 3))").unwrap();
        // MODE is one of Hazel's types; this made-up SETTINGS is not.
        assert_eq!(s.shapes(), (1, 1, 0));
        assert_eq!(
            s.eval("switch (@ hazelCell1) { case (#list(xs)) { xs.size() }; case _ { 0 } }"),
            "3"
        );
    }

    #[test]
    fn settings_are_stored_as_hazels_own_record() {
        let mut s = Store::new();
        let saved = include_str!("../tests/settings.sexp").trim_end();
        s.put(D, "SETTINGS", saved).unwrap();
        assert_eq!(s.get(D, "SETTINGS").as_deref(), Some(saved));
        assert_eq!(s.shapes(), (1, 0, 0));
        // A program reads fields by name.
        assert_eq!(
            s.eval("let v = @ hazelCell0; (v.core.format_shortcut, v.sidebar.panel, v.agent_globals.api_key, v.quiver)"),
            "(#Spaces, #TaskReference, null, true)"
        );
    }

    #[test]
    fn remove_then_save_reuses_the_name() {
        let mut s = Store::new();
        s.put(D, "k", "1").unwrap();
        s.remove(D, "k");
        assert_eq!(s.get(D, "k"), None);
        s.put(D, "k", "2").unwrap();
        assert_eq!(s.get(D, "k").as_deref(), Some("2"));
    }

    #[test]
    fn spaces_hold_the_same_key_apart() {
        let mut s = Store::new();
        s.put("alice", "MODE", "Scratch").unwrap();
        s.put("bob", "MODE", "Documentation").unwrap();
        assert_eq!(s.get("alice", "MODE").as_deref(), Some("Scratch"));
        assert_eq!(s.get("bob", "MODE").as_deref(), Some("Documentation"));
        assert_eq!(s.get(D, "MODE"), None);
        // each its own cell, numbered globally
        assert_eq!(s.index("alice"), vec![("MODE".to_string(), 0)]);
        assert_eq!(s.index("bob"), vec![("MODE".to_string(), 1)]);
        s.clear("alice");
        assert_eq!(s.get("alice", "MODE"), None);
        assert_eq!(s.get("bob", "MODE").as_deref(), Some("Documentation"));
        assert_eq!(s.spaces(), vec![("bob".to_string(), 1)]);
        // an upgrade carries every space
        let snap = s.snapshot();
        let mut t = Store::new();
        t.restore(snap);
        assert_eq!(t.get("bob", "MODE").as_deref(), Some("Documentation"));
    }
}
