#[allow(clippy::all)]
pub mod parser {
    include!(concat!(env!("OUT_DIR"), "/src/lib/parser.rs"));
}
pub mod parser_types;
mod parser_utils;
