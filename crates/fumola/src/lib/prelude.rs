//! The prelude: which module it is, what it exports, and how a host binds it.
//!
//! Two hosts bind these names at the top level of a runtime -- the CLI in
//! `bin/fumola.rs`, and `fumola_wasm` for its JavaScript hosts -- and they had
//! each kept their own list. The lists drifted the moment `print` was added to
//! `fumola/system/prelude.fumola`: the web REPL bound it and the CLI did not,
//! so `print("hello")` worked in one and was an unbound identifier in the
//! other. One list, in one place, so that cannot happen again. `names_match`
//! is the guard that keeps it honest against the `.fumola` file itself.

/// The prelude's path, as an `import` expression writes it.
pub const PRELUDE_PATH: &str = "fumola/system/prelude";

/// How to recognise the prelude among registered modules.
///
/// `set_module` strips the `.fumola` extension, so this is the same name an
/// import uses. Matched as a suffix, since the registered path depends on how
/// the file was named on the command line.
pub const PRELUDE_MODULE_SUFFIX: &str = "system/prelude";

/// Every name the prelude exports, in the order the file declares them.
///
/// Keep this in step with `fumola/system/prelude.fumola`; the test in this
/// module fails if it drifts.
pub const PRELUDE_NAMES: &[&str] = &["pointer", "get", "peek", "print"];

/// A program that imports the prelude and binds each of its names
/// unqualified, so that `get(`x)` means the same thing in every host.
pub fn binding() -> String {
    let mut out = format!("import Prelude \"{}\"; ", PRELUDE_PATH);
    for name in PRELUDE_NAMES {
        out.push_str(&format!("let {} = Prelude.{}; ", name, name));
    }
    out
}

/// The `public func` names the prelude source actually declares.
///
/// A deliberately dumb scan rather than a parse: this exists to catch a name
/// added to the file and not to `PRELUDE_NAMES`, and a scan cannot itself be
/// wrong about the file in a way that hides that.
pub fn names_in(source: &str) -> Vec<String> {
    source
        .lines()
        .filter_map(|line| {
            let rest = line.trim().strip_prefix("public func ")?;
            let end = rest.find(['(', '<', ' '])?;
            Some(rest[..end].to_string())
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The list above and the prelude file must agree. If this fails, a name
    /// was added to one and not the other -- which is exactly the drift that
    /// left `print` bound in the browser and unbound in the CLI.
    #[test]
    fn names_match_the_prelude_source() {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../fumola/system/prelude.fumola"
        );
        let source = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("could not read {}: {}", path, e));
        let found = names_in(&source);
        let expected: Vec<String> = PRELUDE_NAMES.iter().map(|s| s.to_string()).collect();
        assert_eq!(
            found, expected,
            "PRELUDE_NAMES and fumola/system/prelude.fumola disagree"
        );
    }

    #[test]
    fn binding_mentions_every_name() {
        let program = binding();
        for name in PRELUDE_NAMES {
            assert!(
                program.contains(&format!("let {} = Prelude.{};", name, name)),
                "{} is not bound by the prelude binding program",
                name
            );
        }
    }
}
