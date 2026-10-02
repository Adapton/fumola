//! Hazel's key-value table, kept as cells in an Adapton DCG.
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

use fumola::state::State;
use fumola_semantics::value::{Value, Value_};
use std::collections::BTreeMap;

pub struct Store {
    fumola: State,
    /// Each key's number, in the order keys were first saved.
    index: BTreeMap<String, u64>,
    /// The reference each put answered, which is how a read finds the cell.
    cells: BTreeMap<String, Value_>,
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

    fn number_of(&mut self, key: &str) -> u64 {
        if let Some(n) = self.index.get(key) {
            return *n;
        }
        let n = self.next;
        self.next += 1;
        self.index.insert(key.to_string(), n);
        n
    }

    pub fn put(&mut self, key: &str, value: &str) -> Result<(), String> {
        let n = self.number_of(key);
        self.fumola
            .semantic_state
            .define("hazelValue", Value::Text(value.into()));
        let cell = self.run(&format!("`hazel({}) := hazelValue", n))?;
        self.cells.insert(key.to_string(), cell);
        Ok(())
    }

    pub fn get(&mut self, key: &str) -> Option<String> {
        let cell = self.cells.get(key)?.clone();
        self.fumola.semantic_state.define("hazelCell", cell);
        match self.run("@ hazelCell") {
            Ok(v) => match &*v {
                Value::Text(t) => Some(t.to_string()),
                _ => None,
            },
            Err(_) => None,
        }
    }

    /// The key keeps its number, so a later save reuses the same cell name.
    pub fn remove(&mut self, key: &str) {
        self.cells.remove(key);
    }

    pub fn clear(&mut self) {
        self.cells.clear();
    }

    pub fn keys(&self) -> Vec<String> {
        self.cells.keys().cloned().collect()
    }

    pub fn all(&mut self) -> Vec<(String, String)> {
        let keys = self.keys();
        keys.into_iter()
            .filter_map(|k| self.get(&k).map(|v| (k, v)))
            .collect()
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

    pub fn snapshot(&mut self) -> Vec<(String, String)> {
        self.all()
    }

    pub fn restore(&mut self, pairs: Vec<(String, String)>) {
        for (k, v) in pairs {
            let _ = self.put(&k, &v);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_save_is_a_cell_and_a_read_forces_it() {
        let mut s = Store::new();
        s.put("settings", "{\"a\": \"quote\\\" and newline\\n\"}")
            .unwrap();
        assert_eq!(
            s.get("settings").as_deref(),
            Some("{\"a\": \"quote\\\" and newline\\n\"}")
        );
        s.put("settings", "two").unwrap();
        assert_eq!(s.get("settings").as_deref(), Some("two"));
        s.put("editors/a b", "x").unwrap();
        assert_eq!(s.all().len(), 2);
        // Two keys, three puts: the second put to `settings` is a version.
        assert_eq!(s.cell_count(), "3");
    }

    #[test]
    fn a_fumola_program_reads_hazel_cells() {
        let mut s = Store::new();
        s.put("k", "hello").unwrap();
        // A read binds the cell as hazelCell; a program can then build on it.
        assert_eq!(s.get("k").as_deref(), Some("hello"));
        assert_eq!(
            s.eval("let t = `view := thunk { @ hazelCell }; force t"),
            "\"hello\""
        );
    }

    #[test]
    fn remove_then_save_reuses_the_name() {
        let mut s = Store::new();
        s.put("k", "1").unwrap();
        s.remove("k");
        assert_eq!(s.get("k"), None);
        s.put("k", "2").unwrap();
        assert_eq!(s.get("k").as_deref(), Some("2"));
    }
}
