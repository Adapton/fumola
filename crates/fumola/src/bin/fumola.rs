use fumola_semantics::{Interruption, Value_};

use log::{error, info, trace};

use fumola::state::State;
use fumola_parser::parser_types;
use fumola_semantics::format::{format_one_line, format_pretty, ToDoc};
use fumola_semantics::vm_types::{self, Limits};

use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

use clap::{Parser, Subcommand};

pub type OurResult<X> = Result<X, OurError>;

impl From<()> for OurError {
    fn from(_: ()) -> Self {
        OurError::Unknown
    }
}

impl From<vm_types::Error> for OurError {
    fn from(err: vm_types::Error) -> Self {
        OurError::VM(err)
    }
}

impl From<parser_types::SyntaxError> for OurError {
    fn from(err: parser_types::SyntaxError) -> Self {
        OurError::Syntax(err)
    }
}

#[derive(Debug, Clone)]
pub enum OurError {
    Unknown,
    String(String),
    VM(vm_types::Error),
    Syntax(parser_types::SyntaxError),
    FileNotFound(String),
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct CliOpt {
    /// Log at trace level, most verbose.
    #[arg(global = true, long)]
    pub log_trace: bool,
    /// Log at debug level, medium verbose.
    #[arg(global = true, long)]
    pub log_debug: bool,
    /// Do not log, except for warnings/errors.
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
    let mut state = fumola::state::State::empty();
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
            let res = state.set_module(None, path.clone(), file_content.as_str());
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
            let p = fumola_syntax::lexer::create_token_tree(&input)?;
            println!("{}", format_pretty(&p, width));
        }
        CliCommand::Eval { input, step_limit } => {
            let _limits = match step_limit {
                None => Limits::none(),
                Some(limit) => Limits::none().step(limit),
            };
            let v = state.eval(&input); // to do -- use _limits
            if let Ok(v) = &v {
                println!("{}", format_pretty(v, 80))
            } else {
                println!("Error: {:?}", v)
            }
        }
        CliCommand::Repl {} => repl(&mut state),
    };
    Ok(())
}

fn repl(state: &mut State) {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    loop {
        let readline = rl.readline("fumola> ");
        match readline {
            Ok(line) => {
                state.semantic_state.clear_cont();
                let v = state.eval(&line);
                for line in state.semantic_state.debug_print_out.iter() {
                    println!("{}", line.text.to_string())
                }
                state.semantic_state.debug_print_out = im_rc::vector::Vector::new();
                for (path, content) in state.semantic_state.output_files.iter() {
                    let path_string = format_one_line(path).replace("`", "");
                    let content = content.to_string();
                    info!(
                        "writing file `{}` with \"{}\"",
                        path_string,
                        truncate_with_ellipsis(content.as_str(), 69)
                    );
                    let mut file = File::create(path_string).expect("opening output file");
                    let content_to_write = expand_escapes(content.as_str());
                    file.write_all(content_to_write.to_string().as_bytes())
                        .expect("writing output file");
                    file.flush().expect("flush output file")
                }
                state.semantic_state.output_files = im_rc::hashmap::HashMap::new();
                rl.add_history_entry(line.as_str());
                inspect_result(state, v, 0)
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

fn inspect_result(state: &mut State, result: Result<Value_, fumola::Error>, depth: usize) {
    match result {
        Ok(v) => {
            println!("{}", fumola_semantics::format::format_(v.doc(), 80))
        }
        Err(fumola::Error::Interruption(fumola_semantics::Interruption::ModuleFileNotFound(
            path,
        ))) => {
            if depth > 0 {
                return report_error(state, Interruption::ModuleFileNotFound(path).into());
            }
            if let None = path.package_name {
                if let Some(content) = read_file_if_exists(&path.local_path) {
                    match state.set_module(
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
                    let result = state.resume(format!("import {:?}", path.local_path).as_str());
                    inspect_result(state, result, depth + 1);
                } else {
                    report_error(state, Interruption::ModuleFileNotFound(path).into())
                }
            } else {
                report_error(state, Interruption::ModuleFileNotFound(path).into())
            }
        }
        Err(e) => report_error(state, e),
    }
}

fn report_error(_state: &mut State, error: fumola::Error) {
    error!("Error: {:?}", error);
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

fn truncate_with_ellipsis(s: &str, max_len: usize) -> String {
    if s.chars().count() <= max_len {
        s.to_string()
    } else if max_len <= 3 {
        // Not enough space for even one character plus ellipsis
        ".".repeat(max_len)
    } else {
        let truncated: String = s.chars().take(max_len - 3).collect();
        format!("{}...", truncated)
    }
}

fn expand_escapes(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek() {
                Some('n') => {
                    result.push('\n');
                    chars.next();
                }
                Some('t') => {
                    result.push('\t');
                    chars.next();
                }
                Some('r') => {
                    result.push('\r');
                    chars.next();
                }
                Some('\\') => {
                    result.push('\\');
                    chars.next();
                }
                Some('"') => {
                    result.push('"');
                    chars.next();
                }
                Some('\'') => {
                    result.push('\'');
                    chars.next();
                }
                Some(other) => {
                    result.push('\\');
                    result.push(*other);
                    chars.next();
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}
