//! The CLI binds the prelude when the library is imported.
//!
//! Exercised through the binary rather than the library, because the binding
//! lives in `main` -- it is a property of how the CLI starts up, not of the
//! evaluator.

use std::path::{Path, PathBuf};
use std::process::Command;

/// The repo root, two levels above this crate.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

/// Every library file, the way the helper scripts pass them.
fn library(dir: &Path, out: &mut Vec<String>) {
    for entry in std::fs::read_dir(dir).expect("readable directory") {
        let path = entry.expect("directory entry").path();
        let meta = match std::fs::metadata(&path) {
            Ok(m) => m,
            Err(_) => continue, // a broken symlink
        };
        if meta.is_dir() {
            library(&path, out);
        } else if path.extension().and_then(|e| e.to_str()) == Some("fumola") {
            // Relative to the repo root, which is the process's working
            // directory: modules register under the path they are given, and
            // `import "fumola/system/prelude"` has to match.
            let relative = path.strip_prefix(root()).expect("under the root");
            out.push(relative.to_string_lossy().into_owned());
        }
    }
}

fn run(args: &[&str], imports: bool) -> String {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_fumola"));
    cmd.current_dir(root()).args(args);
    if imports {
        let mut files = Vec::new();
        library(&root().join("fumola"), &mut files);
        files.sort();
        cmd.arg("--import").args(&files);
    }
    let out = cmd.output().expect("run the fumola binary");
    if !out.status.success() {
        panic!(
            "fumola exited with {}:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
    }
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn prelude_is_bound_when_the_library_is_imported() {
    let out = run(
        &["--log-warn", "eval", "`x := 41; (get(`x), peek(`x), pointer(`x))"],
        true,
    );
    assert_eq!(out, "(41, ?41, x)", "the prelude should be in scope");
}

#[test]
fn no_library_means_no_prelude_and_no_change() {
    // The CLI has no library of its own; without --import it behaves exactly
    // as it did before the prelude was bound at startup.
    assert_eq!(run(&["--log-warn", "eval", "1 + 2"], false), "3");
    let out = run(&["--log-warn", "eval", "force(`t := thunk { 6 * 7 })"], false);
    assert_eq!(out, "42");
}
