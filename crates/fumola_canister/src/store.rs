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
        // Hazel's own datatype where the canister knows it (schema), else a
        // generic S-expression value, else the text.
        let structured: Value_ = match sexp::parse(value) {
            Some(s) => schema::for_key(key)
                .and_then(|ty| schema::decode_exact(&ty, &s))
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
        self.cells.insert(key.to_string(), cell);
        Ok(())
    }

    pub fn get(&mut self, key: &str) -> Option<String> {
        let cell = self.cells.get(key)?.clone();
        self.fumola.semantic_state.define("hazelCell", cell);
        let v = self.run("@ hazelCell").ok()?;
        if let Some(s) = schema::for_key(key).and_then(|ty| schema::encode(&ty, &v)) {
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
    pub fn remove(&mut self, key: &str) {
        self.cells.remove(key);
    }

    pub fn clear(&mut self) {
        self.cells.clear();
    }

    /// Each key's cell number: `` `hazel(N) `` names its cell.
    pub fn index(&self) -> Vec<(String, u64)> {
        self.cells
            .keys()
            .filter_map(|k| self.index.get(k).map(|n| (k.clone(), *n)))
            .collect()
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

    /// How many values are stored as Hazel's own datatypes, how many as
    /// generic S-expression values, and how many as plain text.
    pub fn shapes(&mut self) -> (usize, usize, usize) {
        let keys = self.keys();
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
                if schema::for_key(&k)
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
            s.eval(
                "let t = `view := thunk { switch (@ hazelCell) { case (#atom(x)) { x }; case _ { \"?\" } } }; force t"
            ),
            "\"hello\""
        );
    }

    #[test]
    fn a_sexp_is_stored_as_a_value_and_printed_back() {
        let mut s = Store::new();
        s.put("MODE", "Documentation").unwrap();
        s.put("SETTINGS", "((theme Dark)(probes(\"a b\" c)))")
            .unwrap();
        s.put("NOTE", "not ( a sexp").unwrap();
        assert_eq!(s.get("MODE").as_deref(), Some("Documentation"));
        // Printed as Sexplib prints it: no space after a quoted atom.
        assert_eq!(
            s.get("SETTINGS").as_deref(),
            Some("((theme Dark)(probes(\"a b\"c)))")
        );
        assert_eq!(s.get("NOTE").as_deref(), Some("not ( a sexp"));
        // A program walks the structure: the settings list has two entries,
        // and the theme is the second atom of the first.
        s.get("SETTINGS");
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
        s.put("MODE", "Documentation").unwrap();
        s.put("SETTINGS", "((a 1)(b 2)(c 3))").unwrap();
        assert_eq!(s.shapes(), (0, 2, 0));
        assert_eq!(
            s.eval("switch (@ hazelCell1) { case (#list(xs)) { xs.size() }; case _ { 0 } }"),
            "3"
        );
    }

    #[test]
    fn settings_are_stored_as_hazels_own_record() {
        let mut s = Store::new();
        let saved = include_str!("../tests/settings.sexp").trim_end();
        s.put("SETTINGS", saved).unwrap();
        assert_eq!(s.get("SETTINGS").as_deref(), Some(saved));
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
        s.put("k", "1").unwrap();
        s.remove("k");
        assert_eq!(s.get("k"), None);
        s.put("k", "2").unwrap();
        assert_eq!(s.get("k").as_deref(), Some("2"));
    }
}
