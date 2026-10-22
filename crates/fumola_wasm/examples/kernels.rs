//! MEASURING ONLY (^ticket(545), branch bench/545-contrast): the Hazel/Fumola kernels (bench/545/kernels.fum) run
//! natively through the very entry point the wasm build exports, fumola_eval_top, so that native and wasm differ only
//! in the compilation target. Prints a row per kernel and size: the median ms of a call and Fumola's steps for it.
//!   cargo run --release -p fumola_wasm --no-default-features --example kernels -- bench/545/kernels.fum out.json
use fumola_wasm::{fumola_create, fumola_eval_top};
use serde_json::{json, Value};
use std::time::Instant;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let src = std::fs::read_to_string(&args[1]).expect("the kernels' source");
    let id = fumola_create();
    let run = |t: &str| -> Value {
        let s = fumola_eval_top(id, t);
        let v: Value = serde_json::from_str(&s).expect("an answer in JSON");
        if v["ok"] != json!(true) {
            panic!("{}: {}", &t[..t.len().min(80)], &s[..s.len().min(300)]);
        }
        v
    };
    run(&src);
    let plan: [(&str, &[u64]); 8] = [
        ("count", &[100, 1000, 5000]),
        ("fib", &[10, 13, 16]),
        ("sumbuild", &[100, 1000, 3000]),
        ("mapinc", &[100, 1000, 3000]),
        ("skip", &[100, 1000, 3000]),
        ("h_id", &[100, 1000, 3000]),
        ("h_ref", &[100, 1000, 3000]),
        ("h_walk", &[100, 1000, 3000]),
    ];
    let mut rows = vec![];
    for (k, ns) in plan.iter() {
        for &n in ns.iter() {
            let text = if k.starts_with("h_") {
                // as in kernels.mjs: one text ending in 0, so that the answer is not the held list itself
                run(&format!("let held = build({n}, null); func h_id(k) {{ k }}; func h_ref(k) {{ let u = held; k }}; func h_walk(k) {{ len(held) }}; 0"));
                format!("{k}(7)")
            } else if *k == "skip" {
                let xs: Vec<String> = (1..=n).map(|i| i.to_string()).collect();
                format!("skip([{}])", xs.join(", "))
            } else {
                format!("{k}({n})")
            };
            for _ in 0..3 {
                run(&text);
            }
            let (mut ts, t00, mut steps) = (vec![], Instant::now(), Value::Null);
            for i in 0..60 {
                let t = Instant::now();
                let r = run(&text);
                ts.push(t.elapsed().as_secs_f64() * 1000.0);
                steps = r["counts"]["step"].clone();
                if t00.elapsed().as_secs_f64() > 2.5 && i >= 4 {
                    break;
                }
            }
            let reps = ts.len();
            ts.sort_by(|a, b| a.partial_cmp(b).unwrap());
            let ms = (ts[reps / 2] * 10000.0).round() / 10000.0;
            let row = json!({"sys": "fumola_native", "k": k, "n": n, "ms": ms, "reps": reps, "steps": steps, "text_chars": text.len()});
            println!("{}", row);
            rows.push(row);
        }
    }
    std::fs::write(&args[2], serde_json::to_string_pretty(&rows).unwrap()).unwrap();
}
