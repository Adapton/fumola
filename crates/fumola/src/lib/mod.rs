use fumola_semantics::vm_types::Interruption;
use serde::{Deserialize, Serialize};

pub type SyntaxErrorCode = fumola_parser::parser_types::SyntaxError;

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct SyntaxError {
    pub package_name: Option<String>,
    pub local_path: String,
    pub code: SyntaxErrorCode,
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum Error {
    ValueError,
    Interruption(Interruption),
    SyntaxError(SyntaxError),
    SyntaxErrorCode(fumola_parser::parser_types::SyntaxError),
}

impl From<SyntaxErrorCode> for Error {
    fn from(x: SyntaxErrorCode) -> Error {
        Error::SyntaxErrorCode(x)
    }
}

impl From<SyntaxError> for Error {
    fn from(x: SyntaxError) -> Error {
        Error::SyntaxError(x)
    }
}

impl From<Interruption> for Error {
    fn from(x: Interruption) -> Error {
        Error::Interruption(x)
    }
}
pub mod check;
pub mod eval;
pub mod package;
pub mod state;
