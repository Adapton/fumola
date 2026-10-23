//! `fumola_format`: a program's text laid out by Fumola's printer.
//!
//! The property that matters is that formatting keeps the program: the
//! formatted text parses, and formatting it again changes nothing.

use fumola_wasm::*;
use serde_json::Value;

fn format(source: &str, width: usize) -> Value {
    serde_json::from_str(&fumola_format(source, width)).unwrap()
}

fn formatted(source: &str, width: usize) -> String {
    let reply = format(source, width);
    assert_eq!(reply["ok"], true, "{}", reply);
    reply["source"].as_str().unwrap().to_string()
}

/// The shape of program ^fumola_wip sends: cells written with `:=`, a thunk
/// forced, a cell read back.
const LIVELIT: &str = "do { `myLivelit (`input) := (40); `myLivelit (`output) := force (`myLivelit (`compute) := thunk { let input = @ (`myLivelit (`input)); input + 1 }); @ (`myLivelit (`output)) }";

#[test]
fn breaks_a_long_program_into_lines() {
    let out = formatted(LIVELIT, 60);
    assert!(out.contains('\n'), "no line breaks:\n{}", out);
    for line in out.lines() {
        assert!(
            line.chars().count() <= 60 || !line.contains(' '),
            "too wide: {:?}",
            line
        );
    }
}

#[test]
fn formatting_is_idempotent() {
    let once = formatted(LIVELIT, 60);
    assert_eq!(formatted(&once, 60), once);
}

#[test]
fn formatted_text_still_parses_and_means_the_same() {
    let once = formatted(LIVELIT, 40);
    // Printed on one line, the reformatted program and the original agree.
    assert_eq!(formatted(&once, usize::MAX), formatted(LIVELIT, usize::MAX));
}

#[test]
fn a_short_program_stays_on_one_line() {
    assert_eq!(formatted("1 + 2", 80), "1 + 2");
}

#[test]
fn text_that_does_not_parse_is_a_syntax_error() {
    let reply = format("let x = ;", 80);
    assert_eq!(reply["ok"], false);
    assert_eq!(reply["kind"], "syntax");
}
