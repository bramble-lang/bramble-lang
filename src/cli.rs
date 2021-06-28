use clap::{App, Arg, ArgMatches};

// Exit Codes for different types of errors
pub const ERR_TYPE_CHECK: i32 = 1;
pub const ERR_NO_AST: i32 = 2;
pub const ERR_PARSER_ERROR: i32 = 3;
pub const ERR_LLVM_IR_ERROR: i32 = 4;
pub const ERR_LEXER_ERROR: i32 = 5;

pub fn print_errs(errs: &[String]) {
    for e in errs {
        println!("{}", e);
    }
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
            Arg::with_name("trace-parser")
                .long("trace-parser")
                .takes_value(true)
                .help("Prints out a trace of all the steps the parser follows as it converts the token vector into an AST.  The current token is printed next to the step.
                This is for debugging the parser when adding new syntactical elements.")
        )
        .arg(
            Arg::with_name("trace-reg-assigner")
                .long("trace-reg-assigner")
                .takes_value(true)
                .help("Prints out a trace of all the nodes in the AST and their register assignment annotation data.")
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
            Arg::with_name("trace-type-resolver")
                .long("trace-type-resolver")
                .takes_value(true)
                .help("Traces the type resolution unit")
        )
        .arg(
            Arg::with_name("trace-path")
                .long("trace-path")
                .takes_value(true)
                .help("Prints out the current module path at the current line of code.")
        );
    app
}

pub fn get_imports<'a>(args: &'a ArgMatches) -> Vec<&'a str> {
    match args.value_of("import") {
        None => vec![],
        Some(imports) => imports.split(",").collect(),
    }
}
