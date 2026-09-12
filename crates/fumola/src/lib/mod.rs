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

/// One line a person can read, for each way the whole system reports failure.
///
/// The point of this impl is that a host has one of these to print, whatever
/// went wrong -- a parse that did not go through, a value that would not
/// convert, or a step the VM could not take.
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ValueError => write!(f, "a value could not be converted"),
            Error::Interruption(i) => write!(f, "{}", i),
            Error::SyntaxError(e) => write!(f, "a syntax error in {}: {:?}", e.local_path, e.code),
            Error::SyntaxErrorCode(e) => write!(f, "a syntax error: {:?}", e),
        }
    }
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
pub mod prelude;
pub mod state;
