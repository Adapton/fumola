use fumola_semantics::vm_types::Interruption;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct SyntaxError {
    pub package_name: Option<String>,
    pub local_path: String,
    pub code: fumola_parser::parser_types::SyntaxError,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Error {
    ValueError,
    Interruption(Interruption),
    SyntaxError(SyntaxError),
}

pub mod check;
pub mod eval;
pub mod package;
