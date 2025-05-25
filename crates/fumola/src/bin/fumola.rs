use fumola::{Interruption, ToMotoko, Value_};

use log::{error, info, trace};

use fumola::format::{format_one_line, format_pretty, ToDoc};
use fumola::vm_types::{Core, Limits};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::fs;
use std::path::Path;

use clap::{Parser, Subcommand};

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
    FileNotFound(String),
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliOpt {
    /// Trace-level logging (most verbose)
    #[arg(global = true, long)]
    pub log_trace: bool,
    /// Debug-level logging (medium verbose)
    #[arg(global = true, long)]
    pub log_debug: bool,
    /// Coarse logging information (only warnings and errors)
    #[arg(global = true, long)]
    pub log_warn: bool,

    /// Import one or more module files
    #[arg(short, long, value_name = "FILE", num_args = 1.., action = clap::ArgAction::Append, global=true)]
    pub import: Vec<String>,

    #[command(subcommand)]
    pub command: CliCommand,
}

#[derive(Subcommand, Debug)]
pub enum CliCommand {
    Check {
        input: String,
    },
    Echo {
        input: String,
    },
    Format {
        input: String,
        #[arg(short)]
        width: usize,
    },
    Eval {
        #[arg(short, long)]
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
    use fumola::vm_types::Core;
    let mut core = Core::empty();
    let cli_opt = CliOpt::parse();
    info!("Init log...");
    init_log(
        match (cli_opt.log_trace, cli_opt.log_debug, cli_opt.log_warn) {
            (true, _, _) => log::LevelFilter::Trace,
            (_, true, _) => log::LevelFilter::Debug,
            (_, _, true) => log::LevelFilter::Warn,
            (_, _, _) => log::LevelFilter::Info,
        },
    );
    info!("Importing files: {:?}", cli_opt.import);
    for path in cli_opt.import.iter() {
        let file_content = read_file_if_exists(path);
        if let Some(file_content) = file_content {
            let res = core.set_module(None, path.clone(), file_content.as_str());
            if res.is_err() {
                return Err(OurError::String(format!("{:?}", res)));
            }
        } else {
            println!("Error: Import path not found: {:?}", path);
            return Err(OurError::FileNotFound(path.clone()));
        }
    }
    info!("{:?} ...", &cli_opt.command);
    let () = match cli_opt.command {
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
            let _limits = match step_limit {
                None => Limits::none(),
                Some(limit) => Limits::none().step(limit),
            };
            let v = core.eval_str(&input); // to do -- use _limits
            if let Ok(v) = &v {
                println!("{}", format_pretty(v, 80))
            } else {
                println!("Error: {:?}", v)
            }
        }
        CliCommand::Repl {} => repl(&mut core),
    };
    Ok(())
}

fn repl(core: &mut Core) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
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
                inspect_result(core, v, 0)
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
