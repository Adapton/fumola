use candid::Int;
use fumola::{Interruption, ToMotoko, Value_};
use structopt::StructOpt;

use log::{error, info, trace};
use std::io;
use structopt::{clap, clap::Shell};

use fumola::format::{format_one_line, format_pretty, ToDoc};
use fumola::vm_types::{Active, Core, Limits};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::fs;
use std::path::Path;

pub type OurResult<X> = Result<X, OurError>;

impl From<()> for OurError {
    fn from(_: ()) -> Self {
        OurError::Unknown
    }
}

impl From<fumola::vm_types::Error> for OurError {
    fn from(err: fumola::vm_types::Error) -> Self {
        OurError::VM(err)
    }
}

impl From<fumola::parser_types::SyntaxError> for OurError {
    fn from(err: fumola::parser_types::SyntaxError) -> Self {
        OurError::Syntax(err)
    }
}

#[derive(Debug, Clone)]
pub enum OurError {
    Unknown,
    String(String),
    VM(fumola::vm_types::Error),
    Syntax(fumola::parser_types::SyntaxError),
}

/// Fumola tools in Rust.
#[derive(StructOpt, Debug, Clone)]
#[structopt(
    name = "fumola",
    setting = clap::AppSettings::DeriveDisplayOrder
)]
pub struct CliOpt {
    /// Trace-level logging (most verbose)
    #[structopt(short = "t", long = "trace-log")]
    pub log_trace: bool,
    /// Debug-level logging (medium verbose)
    #[structopt(short = "d", long = "debug-log")]
    pub log_debug: bool,
    /// Coarse logging information (not verbose)
    #[structopt(short = "L", long = "log")]
    pub log_info: bool,

    #[structopt(subcommand)]
    pub command: CliCommand,
}

#[derive(StructOpt, Debug, Clone)]
pub enum CliCommand {
    #[structopt(
        name = "completions",
        about = "Generate shell scripts for auto-completions."
    )]
    Completions {
        shell: Shell,
    },
    Check {
        input: String,
    },
    Echo {
        input: String,
    },
    Format {
        input: String,
        #[structopt(short = "w")]
        width: usize,
    },
    Eval {
        #[structopt(short = "s", long = "step-limit")]
        step_limit: Option<usize>,

        input: String,
    },
    Repl {},
}

fn init_log(level_filter: log::LevelFilter) {
    use env_logger::{Builder, WriteStyle};
    let mut builder = Builder::new();
    builder
        .filter(None, level_filter)
        .write_style(WriteStyle::Always)
        .init();
}

fn main() -> OurResult<()> {
    info!("Starting...");
    let cli_opt = CliOpt::from_args();
    info!("Init log...");
    init_log(
        match (cli_opt.log_trace, cli_opt.log_debug, cli_opt.log_info) {
            (true, _, _) => log::LevelFilter::Trace,
            (_, true, _) => log::LevelFilter::Debug,
            (_, _, true) => log::LevelFilter::Info,
            (_, _, _) => log::LevelFilter::Warn,
        },
    );
    info!("Evaluating CLI command: {:?} ...", &cli_opt.command);
    let () = match cli_opt.command {
        CliCommand::Completions { shell: s } => {
            // see also: https://clap.rs/effortless-auto-completion/
            CliOpt::clap().gen_completions_to("caniput", s, &mut io::stdout());
            info!("done");
        }
        CliCommand::Check { input } => {
            let _ = fumola::check::parse(&input)?;
            println!("check::parse: okay.");
        }
        CliCommand::Echo { input } => {
            let p = fumola::check::parse(&input)?;
            println!("{}", format_one_line(&p));
        }
        CliCommand::Format { input, width } => {
            let p = fumola::lexer::create_token_tree(&input)?;
            println!("{}", format_pretty(&p, width));
        }
        CliCommand::Eval { input, step_limit } => {
            let limits = match step_limit {
                None => Limits::none(),
                Some(limit) => Limits::none().step(limit),
            };
            let v = fumola::vm::eval_limit(&input, &limits);
            println!("final value: {:?}", v)
        }
        CliCommand::Repl {} => repl(),
    };
    Ok(())
}

fn repl() {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    use fumola::vm_types::Core;
    let mut core = Core::empty();
    loop {
        let readline = rl.readline("fumola> ");
        match readline {
            Ok(line) => {
                core.clear_cont();
                let v = core.eval_str(&line);
                for line in core.debug_print_out.iter() {
                    println!("{}", line.text.to_string())
                }
                core.debug_print_out = im_rc::vector::Vector::new();
                rl.add_history_entry(line.as_str());
                inspect_result(&mut core, v, 0)
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}

fn inspect_result(core: &mut Core, result: Result<Value_, Interruption>, depth: usize) {
    match result {
        Ok(v) => {
            println!("{}", fumola::format::format_(v.doc(), 80))
        }
        Err(Interruption::ModuleFileNotFound(path)) => {
            if depth > 0 {
                return report_error(core, Interruption::ModuleFileNotFound(path));
            }
            if let None = path.package_name {
                if let Some(content) = read_file_if_exists(&path.local_path) {
                    match core.set_module(
                        path.package_name.clone(),
                        path.local_path.clone(),
                        &content,
                    ) {
                        Err(e) => {
                            error!("Error: {:?}", e);
                            return;
                        }
                        Ok(()) => (),
                    }
                    let result = core.resume(format!("import {:?}", path.local_path).as_str());
                    inspect_result(core, result, depth + 1);
                } else {
                    report_error(core, Interruption::ModuleFileNotFound(path))
                }
            } else {
                report_error(core, Interruption::ModuleFileNotFound(path))
            }
        }
        Err(e) => report_error(core, e),
    }
}

fn report_error(core: &mut Core, error: Interruption) {
    error!("Error: {:?}", error);
    info!("  Hint: Inspect lastError for details.");
    info!("  For example, lastError.core.agent.active.cont, if lastError.core.scedule_choice == #Agent");
    /* dump core, without chaining with any prior core dump. */
    if let Some(_) = core.get_var("lastInterruption") {
        core.define("lastInterruptionCore", fumola::Value::Unit);
        core.define("lastInterruption", fumola::Value::Unit);
        core.define("lastError", fumola::Value::Unit);
    };
    core.define("lastInterruptionCore", core.clone().to_motoko().unwrap());
    core.clear_cont();
    core.define("lastInterruption", error.to_motoko().unwrap());
    core.eval_str("let lastError = {interruption=lastInterruption; core=lastInterruptionCore}")
        .expect("define errorInfo");
}

fn read_file_if_exists(path: &str) -> Option<String> {
    trace!("Checking for module file at path: {}", path);
    let path = Path::new(path);
    if path.exists() && path.is_file() {
        trace!("...Module file found.");
        let contents = fs::read_to_string(path).ok()?;
        Some(contents)
    } else {
        trace!("...Module file not found.");
        None
    }
}
