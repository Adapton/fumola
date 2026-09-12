//! What a region on the Rust stack is worth, measured.
//!
//! Issue #122 asks whether the machine's explicit continuation -- a `Frame`
//! carrying an environment and a boxed `FrameCont`, allocated once per redex --
//! is where evaluation time actually goes. `do big { .. }` makes the question
//! answerable: the same program, the same graph, the same step count, with the
//! intermediate states on the Rust call stack instead of on the heap.
//!
//! So this is not a benchmark of two languages. It is one program run twice,
//! differing by a single token, and the two runs are already known to agree on
//! their answer, their step count and their redex count -- `test_do_big` checks
//! all three. What is left to differ is time.
//!
//! # Reading the output
//!
//! `same` is the noise floor: `do small { W }` timed against itself. A ratio in
//! the `big` column that is not further from 1.0 than `same` is not a result.
//! Report it with the floor beside it or not at all.
//!
//! `recursive` says how much of each workload the recursive evaluator actually
//! handles. The set it covers is binders, blocks, operators, the two loops and
//! assignment to a plain variable; calls, arrays, indexing, objects, switches
//! and every adapton operation are handed back to the machine, which does the
//! same work at the same cost. A workload marked `no` is expected to show
//! nothing, and showing nothing is the measurement -- it is what sizes the next
//! step rather than a disappointment.
//!
//! Run with:
//!
//! ```text
//! cargo bench -p fumola --bench do_big
//! ```

use fumola_semantics::vm_types::{ActiveBorrow, Core, Limits};
use std::time::{Duration, Instant};

/// How many times each program is run. The reported figure is the minimum,
/// which is the least noisy summary of a deterministic computation: it is the
/// run that was interrupted least.
const RUNS: usize = 7;

struct Workload {
    name: &'static str,
    /// Whether the recursive evaluator covers the forms this workload is made
    /// of, or hands them back.
    recursive: Recursive,
    source: String,
}

enum Recursive {
    /// Every node is in the recursive set.
    Yes,
    /// The shape is in the set but the work inside it is not.
    Partly,
    /// The workload is delegated, node for node.
    No,
}

impl Recursive {
    fn label(&self) -> &'static str {
        match self {
            Recursive::Yes => "yes",
            Recursive::Partly => "partly",
            Recursive::No => "no",
        }
    }
}

fn time_min(source: &str) -> (Duration, usize) {
    let prog = fumola::check::parse(source).expect("benchmark program did not parse");
    let mut best = Duration::from_secs(u64::MAX);
    let mut steps = 0;
    for _ in 0..RUNS {
        let mut core = Core::new(prog.clone());
        let start = Instant::now();
        let r = core.run(&Limits::none());
        let elapsed = start.elapsed();
        r.expect("benchmark program failed");
        steps = core.counts().step;
        if elapsed < best {
            best = elapsed;
        }
    }
    (best, steps)
}

/// A long run of `let` bindings, each reading the ones before it.
///
/// The binder is the unit of cost: every `let` is a `Let` frame pushed and
/// popped, and every variable read is a step of its own.
fn binders(n: usize) -> String {
    let mut s = String::from("let x0 = 1; ");
    for i in 1..n {
        s.push_str(&format!("let x{} = x{} + {}; ", i, i - 1, i));
    }
    s.push_str(&format!("x{}", n - 1));
    s
}

/// One expression, nested to depth `n`: depth without breadth.
fn nested(n: usize) -> String {
    let mut s = String::from("1");
    for i in 0..n {
        s = format!("({} + {})", s, i % 7);
    }
    s
}

/// A `while` loop whose body does `reads` array reads and `calls` calls per
/// iteration, on top of the arithmetic that is always there.
///
/// This is the instrument. The loop, the binders, the arithmetic and the
/// assignments are all in the recursive set; every array read and every call is
/// handed back to the machine. Holding the loop fixed and raising the delegated
/// share is what separates "big-step is faster" from "big-step is faster at the
/// things it actually does".
fn loop_with(iters: usize, reads: usize, calls: usize) -> String {
    let mut body = String::from("s := s + i");
    for k in 0..reads {
        body.push_str(&format!(" + a[{}]", k % 16));
    }
    for _ in 0..calls {
        body.push_str(" + f(i)");
    }
    format!(
        "func f(x : Nat) : Nat {{ x + 1 }}; \
         let a = [var {}]; \
         var i = 0; var s = 0; \
         while (i < {}) {{ {}; i := i + 1 }}; s",
        (0..16).map(|i| i.to_string()).collect::<Vec<_>>().join(", "),
        iters,
        body,
    )
}

/// Calls nested inside calls, with no loop and no binders around them.
///
/// The outermost node is a `Call`, so the whole expression is one delegation
/// and the recursive evaluator does nothing at all. This is the control: it
/// measures what handing a node back costs, and the answer should be nothing.
fn nested_calls(n: usize) -> String {
    let mut s = String::from("0");
    for _ in 0..n {
        s = format!("f({})", s);
    }
    format!("func f(x : Nat) : Nat {{ x + 1 }}; {}", s)
}

/// The shape real code has: binders and nesting inside a loop.
fn mixed(n: usize) -> String {
    format!(
        "var i = 0; var total = 0; \
         while (i < {}) {{ \
           let a = i + 1; let b = a * 2; let c = (a + b) * (b - a); \
           let d = if (c > 10) (c - 10) else (c + 10); \
           total := total + d; i := i + 1 \
         }}; total",
        n
    )
}

fn main() {
    let workloads = vec![
        Workload {
            name: "binders (800 lets)",
            recursive: Recursive::Yes,
            source: binders(800),
        },
        Workload {
            name: "nested (depth 200)",
            recursive: Recursive::Yes,
            source: nested(200),
        },
        Workload {
            name: "mixed (5k iters)",
            recursive: Recursive::Yes,
            source: mixed(5_000),
        },
        // The series. One loop, a rising delegated share.
        Workload {
            name: "loop, 0 reads",
            recursive: Recursive::Yes,
            source: loop_with(20_000, 0, 0),
        },
        Workload {
            name: "loop, 1 read",
            recursive: Recursive::Partly,
            source: loop_with(20_000, 1, 0),
        },
        Workload {
            name: "loop, 4 reads",
            recursive: Recursive::Partly,
            source: loop_with(20_000, 4, 0),
        },
        Workload {
            name: "loop, 16 reads",
            recursive: Recursive::Partly,
            source: loop_with(20_000, 16, 0),
        },
        Workload {
            name: "loop, 1 call",
            recursive: Recursive::Partly,
            source: loop_with(20_000, 0, 1),
        },
        Workload {
            name: "loop, 4 calls",
            recursive: Recursive::Partly,
            source: loop_with(20_000, 0, 4),
        },
        // The control: one delegation, nothing else.
        Workload {
            name: "nested calls (5k)",
            recursive: Recursive::No,
            source: nested_calls(5_000),
        },
    ];

    println!();
    println!(
        "{:<22} {:>8} {:>11} {:>11} {:>8} {:>8}",
        "workload", "steps", "small (ms)", "big (ms)", "big", "same"
    );
    println!("{}", "-".repeat(74));

    for w in &workloads {
        let small_src = format!("do small {{ {} }}", w.source);
        let big_src = format!("do big {{ {} }}", w.source);

        let (small, steps) = time_min(&small_src);
        let (big, big_steps) = time_min(&big_src);
        // The noise floor: the same program, the same mode, timed again.
        let (small_again, _) = time_min(&small_src);

        assert_eq!(
            steps, big_steps,
            "{}: the two modes disagree on step count, so the timing is meaningless",
            w.name
        );

        let ratio = big.as_secs_f64() / small.as_secs_f64();
        let floor = small_again.as_secs_f64() / small.as_secs_f64();

        println!(
            "{:<22} {:>8} {:>11.2} {:>11.2} {:>8.2} {:>8.2}   [{}]",
            w.name,
            steps,
            small.as_secs_f64() * 1000.0,
            big.as_secs_f64() * 1000.0,
            ratio,
            floor,
            w.recursive.label(),
        );
    }

    println!();
    println!("ratio < 1.00 means big-step was faster. `same` is the noise floor:");
    println!("a `big` ratio no further from 1.00 than `same` is not a result.");
    println!("[recursive: yes = the evaluator handles it; no = handed to the machine]");
    println!();
}
