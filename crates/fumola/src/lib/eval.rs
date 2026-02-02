use fumola_semantics::value::Value_;
use fumola_semantics::vm_types::{Core, Limits};
use crate::{Error, SyntaxError};

/// Used for tests in check module.
pub fn eval_limit(prog: &str, limits: &Limits) -> Result<Value_, Error> {
    info!("eval_limit:");
    info!("  - prog = {}", prog);
    info!("  - limits = {:#?}", limits);
    //use crate::vm_types::Interruption::SyntaxError;
    let package_name = None;
    let local_path = "<anonymous>".to_string();
    let p = crate::check::parse(prog).map_err(|code| {
        Error::SyntaxError(SyntaxError {
            code,
            local_path,
            package_name,
        })
    })?;
    info!("eval_limit: parsed.");
    let mut c = Core::new(p);
    let r = c.run(limits);
    use log::info;
    info!("eval_limit: result: {:#?}", r);
    r.map_err(|e| Error::Interruption(e))
}

pub fn eval(prog: &str) -> Result<Value_, Error> {
    eval_limit(prog, &Limits::none())
}

pub fn eval_into<T: serde::de::DeserializeOwned>(prog: &str) -> Result<T, Error> {
    eval(prog)?.to_rust().map_err(|_|Error::ValueError)
}
