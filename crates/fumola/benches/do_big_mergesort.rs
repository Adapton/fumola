//! `mergeSort` under both modes.
//!
//! The first cut of `do big` could not move this program at all: every function
//! body was entered through a delegated `Call`, so `do big { M.runAll() }` was
//! one delegation and no recursive work. With calls in the recursive set it is
//! the opposite -- almost the whole program runs on the Rust stack -- and this
//! is where that shows up or does not.
//!
//! It loads the real module tree the way the CLI does, rather than a synthetic
//! program, because the question is about this example specifically: it is what
//! issue #122 names, and the adapton graph it builds is the thing a region must
//! not disturb.
//!
//! The step count is reported beside the time and asserted equal between the
//! modes. On a program this size that is the strongest correctness check
//! available -- roughly a million transitions that have to line up exactly, in
//! code that allocates, forces thunks, repairs the graph and recurses.
//!
//! Run with:
//!
//! ```text
//! cargo bench -p fumola --bench do_big_mergesort
//! ```

use fumola::state::State;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

const RUNS: usize = 3;

/// The repository root, from this crate's manifest.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .canonicalize()
        .expect("the repository root is where the manifest says")
}

/// Every `.fumola` file under `fumola/`, which is what `--import` gets on the
/// command line.
fn module_paths(root: &Path) -> Vec<PathBuf> {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|e| e == "fumola") {
                out.push(path);
            }
        }
    }
    let mut out = vec![];
    walk(&root.join("fumola"), &mut out);
    // The walk order is the filesystem's; sorting keeps a run reproducible.
    out.sort();
    out
}

/// A session with the library loaded, as the CLI builds one.
fn loaded(root: &Path, modules: &[PathBuf]) -> State {
    let mut state = State::empty();
    for path in modules {
        let local = path
            .strip_prefix(root)
            .expect("a module under the root")
            .to_string_lossy()
            .into_owned();
        let content = std::fs::read_to_string(path).expect("a readable module");
        state
            .set_module(None, local, &content)
            .expect("the module loaded");
    }
    state
}

fn time_min(root: &Path, modules: &[PathBuf], program: &str) -> (Duration, usize) {
    let mut best = Duration::from_secs(u64::MAX);
    let mut steps = 0;
    for _ in 0..RUNS {
        let mut state = loaded(root, modules);
        // The import is outside the timed region and outside the mode, so the
        // two runs differ only in how the call is evaluated.
        state
            .eval("import M \"fumola/examples/mergeSort/mergeSort\";")
            .expect("mergeSort imported");
        let before = state.steps_taken();
        let start = Instant::now();
        let r = state.eval(program);
        let elapsed = start.elapsed();
        r.expect("mergeSort ran");
        steps = state.steps_taken() - before;
        if elapsed < best {
            best = elapsed;
        }
    }
    (best, steps)
}

fn main() {
    let root = repo_root();
    let modules = module_paths(&root);
    assert!(!modules.is_empty(), "no modules found under fumola/");

    // Sizes rather than `runAll()`, so the figure is one computation at a known
    // input size instead of a report that also prints.
    //
    // The call is bound to a name rather than projected from directly. That is
    // not cosmetic: `M.gen(..).sceneData` parses as `Dot(Call(..), sceneData)`,
    // and `Dot` is not in the recursive set -- so the outermost node would be
    // delegated and take the entire computation with it. The first run of this
    // benchmark measured exactly that and read as "calls did not help".
    let workloads: Vec<(String, String)> = vec![8, 16, 23, 44]
        .into_iter()
        .map(|size| {
            (
                format!("scene, size {}", size),
                format!(
                    "let r = M.generateSceneFullDemand(10, {}, null); r.sceneData",
                    size
                ),
            )
        })
        .collect();

    println!();
    println!("modules loaded: {}", modules.len());
    println!();
    println!(
        "{:<18} {:>10} {:>12} {:>12} {:>8} {:>8}",
        "workload", "steps", "small (ms)", "big (ms)", "big", "same"
    );
    println!("{}", "-".repeat(74));

    for (name, body) in &workloads {
        let small_src = format!("do small {{ {} }}", body);
        let big_src = format!("do big {{ {} }}", body);

        let (small, steps) = time_min(&root, &modules, &small_src);
        let (big, big_steps) = time_min(&root, &modules, &big_src);
        let (small_again, _) = time_min(&root, &modules, &small_src);

        assert_eq!(
            steps, big_steps,
            "{}: the modes disagree on step count ({} vs {}), so the timing means nothing",
            name, steps, big_steps
        );

        println!(
            "{:<18} {:>10} {:>12.1} {:>12.1} {:>8.2} {:>8.2}",
            name,
            steps,
            small.as_secs_f64() * 1000.0,
            big.as_secs_f64() * 1000.0,
            big.as_secs_f64() / small.as_secs_f64(),
            small_again.as_secs_f64() / small.as_secs_f64(),
        );
    }

    println!();
    println!("step counts are asserted equal between the modes, not merely reported.");
    println!("`same` is the noise floor: the same program, the same mode, timed again.");
    println!();
}
