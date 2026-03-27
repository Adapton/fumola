use fumola::Error;
use fumola_semantics::{Interruption, Share, Value, Value_};

use im_rc::HashMap;
use log::{debug, error, info, trace};

use fumola::state::State;
use fumola_parser::parser_types;
use fumola_semantics::format::{format_one_line, format_pretty, ToDoc};
use fumola_semantics::vm_types::{self, Active, Limits};

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
    Test {},
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
            let result = state.eval(&input); // to do -- use _limits
            post_eval(&mut state, result)
        }
        CliCommand::Repl {} => repl(&mut state),
        CliCommand::Test {} => test(&mut state),
    };
    Ok(())
}

fn test(state: &mut State) {
    let mut state_ = state.clone();

    let files = state_.semantic_state.module_files().map.clone();
    for (path, _file) in files.iter() {
        let res = state_.eval(format!("import _ \"{}\";", path.local_path).as_str());
        if let Ok(_) = res {
            info!("Imported {:?}", path.local_path);
        } else {
            error!("Could not import {:?}", path.local_path);
            return;
        }
    }

    let tests = state_.semantic_state.test_suite.clone();
    let mut errors = 0;
    let mut passed = 0;

    // WIP -- This was meant to avoid duplicate tests, but does not work currently
    //        to avoid re-doing the duplicates that arise because of symlinks we use in fumola code library.
    //        more investigation required. 2026-03-27.
    let mut test_func_defs = HashMap::new();

    for (test, ()) in tests.iter() {
        debug!("Testing {}", format_one_line(&test.0 .1));
        let defs = test.0 .0.clone();
        let dec_field = test.0 .2.clone();
        let ctx_id = test.0 .1.clone();
        let func_def = test.0 .3.clone();
        if !test_func_defs.contains_key(&func_def.function.exp) {
            test_func_defs.insert(func_def.function.exp.clone(), ());
            match dec_field.dec.0 {
                fumola_syntax::ast::Dec::Func(ref function) => {
                    let mut state__ = state_.clone(); // sandbox test
                                                      // shadow these names, to avoid ambiguity or typos below:
                    state__.semantic_state.clear_cont();

                    let res = state__
                        .semantic_state
                        .call_function_def(func_def, Value::Unit.share());

                    match res {
                        Ok(_) => {
                            let def = defs.map.get(&ctx_id).unwrap();

                            if let Some(local_id) = &def.local_id {
                                info!(
                                    "✅ {}/{}/???.{}",
                                    &defs.active_path.clone().unwrap(),
                                    format_one_line(local_id),
                                    format_one_line(&function.name)
                                );
                            } else {
                                info!(
                                    "✅ {}/???.{}",
                                    &defs.active_path.clone().unwrap(),
                                    format_one_line(&function.name)
                                );
                            }
                            if false {
                                info!(
                                    "✅ {}/{:?}: {}: {}",
                                    defs.active_path.unwrap(),
                                    ctx_id,
                                    dec_field.dec.1,
                                    format_one_line(&function.name)
                                );
                            };

                            passed += 1;
                        }
                        Err(error) => {
                            error!(
                                "❌ {}/{:?}: {}: {}",
                                defs.active_path.unwrap(),
                                ctx_id,
                                dec_field.dec.1,
                                format_one_line(&function.name)
                            );
                            report_error(&mut state__, error.into());
                            errors += 1;
                        }
                    }
                }
                _ => {
                    error!(
                        "Couldn't test non-function declaration. {}",
                        &dec_field.dec.1
                    )
                }
            }
        }
    }
    info!(
        "{} Tests: {} passed and {} failed.",
        tests.len(),
        passed,
        errors
    )
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
                let result = state.eval(&line);
                post_eval(state, result);
                rl.add_history_entry(line.as_str());
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

fn post_eval(state: &mut State, result: Result<Value_, Error>) {
    for line in state.semantic_state.debug_print_out.iter() {
        println!("{}", line.text.to_string())
    }
    state.semantic_state.debug_print_out = im_rc::vector::Vector::new();
    for (path, content) in state.semantic_state.output_files.iter() {
        let path_string = format_one_line(path).replace("`", "");
        let content = content.to_string();
        debug!(
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

    inspect_result(state, result, 0)
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

fn truncate_debug<T: std::fmt::Debug>(value: &T, max_len: usize) -> String {
    let s = format!("{:?}", value);
    if s.chars().count() > max_len {
        s.chars().take(max_len).collect::<String>() + "..."
    } else {
        s
    }
}

fn report_error(state: &mut State, error: fumola::Error) {
    let cont = state.semantic_state.cont().clone();
    let cont_source = state.semantic_state.cont_source().clone();
    eprintln!("");
    error!("{:?}", error);
    eprintln!("");
    eprintln!(
        "[{:_>17}]: {}",
        &format!("{}", cont_source),
        truncate_debug(&cont, 63)
    );
    if let Ok(stack) = state.semantic_state.agent_stack() {
        for frame in stack.iter() {
            eprintln!(
                "[{:_>17}]: {}",
                &format!("{}", &frame.source),
                truncate_debug(&frame.cont, 63)
            );
        }
    } else {
        error!("(No stack available to print)\n{:?}", error);
    }
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
