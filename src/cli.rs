use clap::{App, Arg, ArgMatches};
use log::LevelFilter;
use simplelog::*;

use crate::{
    compiler::{CompilerDisplay, CompilerDisplayError},
    diagnostics::config::TracingConfig,
    StringTable,
};

// Exit Codes for different types of errors
pub const ERR_TYPE_CHECK: i32 = 1;
pub const ERR_NO_AST: i32 = 2;
pub const ERR_PARSER_ERROR: i32 = 3;
pub const ERR_LLVM_IR_ERROR: i32 = 4;
pub const ERR_LEXER_ERROR: i32 = 5;
pub const ERR_IMPORT_ERROR: i32 = 6;
pub const ERR_MANIFEST_WRITE_ERROR: i32 = 7;

pub fn print_errs<E: CompilerDisplay>(st: &StringTable, errs: &[E]) {
    for e in errs {
        println!("Error: {}", e.fmt(st).unwrap());
    }
}

#[derive(PartialEq)]
pub enum Stage {
    Lexer,
    Parser,
    Semantic,
}

pub fn configure_cli() -> clap::App<'static, 'static> {
    let app = App::new("Braid Compiler")
        .version("0.1.0")
        .author("Erich Ess")
        .about("Compiles Braid language files into x86 assembly for use by the NASM assembler")
        .arg(
            Arg::with_name("input")
                .short("i")
                .long("input")
                .takes_value(true)
                .required(true)
                .help("Source code file to compile"),
        )
        .arg(
            Arg::with_name("import")
                .short("m")
                .long("import")
                .takes_value(true)
                .required(false)
                .help("Comma separated list of projects that this project is dependent upon."),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .takes_value(true)
                .required(true)
                .help("Name the output file that the assembly will be written to"),
        )
        .arg(
            Arg::with_name("llvm")
                .long("llvm")
                .help("When set, then compiler will emit LLVM IR rather than x86 IR")
        )
        .arg(
            Arg::with_name("emit")
                .long("emit")
                .possible_values(&["llvm-ir"])
                .takes_value(true)
                .help("When set, this will output different types of IR (LLVM, assembly, etc.)")
        )
        .arg(
            Arg::with_name("manifest")
                .long("manifest")
                .takes_value(false)
                .help("Write a manifest file for this project. The manifest can then be used by other projects to import items from this project.")
        )
        .arg(
            Arg::with_name("platform")
                .short("p")
                .long("platform")
                .possible_values(&["linux", "machos"])
                .takes_value(true)
                .required(true)
                .help("The target Operation System that this will be compiled for: Linux or Mac (Mac is still unreliable and being worked on)"),
        )
        .arg(
            Arg::with_name("log")
                .long("log")
                .possible_values(&["debug", "info", "error"])
                .takes_value(true)
                .help("Set the logging filter (default level is error) to gain insight into what the compiler is doing."),
        )
        .arg(
            Arg::with_name("trace-parser")
                .long("trace-parser")
                .takes_value(true)
                .help("Prints out a trace of all the steps the parser follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the parser when adding new syntactical elements.")
        )
        .arg(
            Arg::with_name("trace-lexer")
                .long("trace-lexer")
                .takes_value(true)
                .help("Prints out a trace of all the steps the lexer follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the lexer when adding new tokens.")
        )
        .arg(
            Arg::with_name("trace-semantic-node")
                .long("trace-symbol-table")
                .takes_value(true)
                .help("Traces the transformation from Parser AST to Semantic AST")
        )
        .arg(
            Arg::with_name("trace-canonization")
                .long("trace-canonization")
                .takes_value(true)
                .help("Shows the transformation of relative paths to canonized/absolute paths.")
        )
        .arg(
            Arg::with_name("trace-type-resolver")
                .long("trace-type-resolver")
                .takes_value(true)
                .help("Traces the type resolution unit")
        )
        .arg(
            Arg::with_name("stage")
            .long("stage")
            .takes_value(true)
            .help("Will run the compiler to the given stage (lexer, parser, semantic).  This is \
            is used for validating source code files. No assembly or LLVM IR will be emitted.")
        );
    app
}

pub fn get_imports<'a>(args: &'a ArgMatches) -> Vec<&'a str> {
    match args.value_of("import") {
        None => vec![],
        Some(imports) => imports.split(",").collect(),
    }
}

pub fn get_log_level<'a>(args: &'a ArgMatches) -> Option<LevelFilter> {
    match args.value_of("log") {
        None => None,
        Some(level) => match level.to_lowercase().as_str() {
            "debug" => Some(LevelFilter::Debug),
            "info" => Some(LevelFilter::Info),
            "error" => Some(LevelFilter::Error),
            _ => None,
        },
    }
}

pub fn get_lexer_tracing<'a>(args: &'a ArgMatches) -> TracingConfig {
    TracingConfig::parse(args.value_of("trace-lexer"))
}

pub fn get_parser_tracing<'a>(args: &'a ArgMatches) -> TracingConfig {
    TracingConfig::parse(args.value_of("trace-parser"))
}

pub fn get_semantic_node_tracing<'a>(args: &'a ArgMatches) -> TracingConfig {
    TracingConfig::parse(args.value_of("trace-semantic-node"))
}

pub fn get_canonization_tracing<'a>(args: &'a ArgMatches) -> TracingConfig {
    TracingConfig::parse(args.value_of("trace-canonization"))
}

pub fn get_type_resolver_tracing<'a>(args: &'a ArgMatches) -> TracingConfig {
    TracingConfig::parse(args.value_of("trace-type-resolver"))
}

pub fn get_stage<'a>(args: &'a ArgMatches) -> Result<Option<Stage>, String> {
    if let Some(stage) = args.value_of("stage") {
        match stage {
            "lexer" => Ok(Some(Stage::Lexer)),
            "parser" => Ok(Some(Stage::Parser)),
            "semantic" => Ok(Some(Stage::Semantic)),
            _ => Err(format!("Unrecognized stage: {}", stage)),
        }
    } else {
        Ok(None)
    }
}

pub fn configure_logging(level: LevelFilter) -> Result<(), log::SetLoggerError> {
    CombinedLogger::init(vec![TermLogger::new(
        level,
        Config::default(),
        TerminalMode::Mixed,
        ColorChoice::Auto,
    )])
}

impl CompilerDisplay for String {
    /// Allow for errors from the Lexer (which do not include StringIds since the lexer generates the
    /// [StringTable]) to be printed using the same error printing functions as all other errors
    fn fmt(&self, _: &StringTable) -> Result<String, CompilerDisplayError> {
        Ok(format!("{}", self))
    }
}
