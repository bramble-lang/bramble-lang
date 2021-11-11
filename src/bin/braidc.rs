extern crate log;
extern crate simplelog;

use std::cell::RefCell;
use std::rc::Rc;
use std::{path::Path, process::exit};

use braid_lang::compiler::diagnostics::Logger;
use braid_lang::diagnostics::ConsoleWriter;
use inkwell::context::Context;

use braid_lang::project::*;
use braid_lang::*;

use crate::compiler::ast::MAIN_MODULE;

const BRAID_FILE_EXT: &str = "br";
const USER_MAIN_FN: &str = "my_main";

fn main() {
    let config = configure_cli().get_matches();

    match get_log_level(&config) {
        Some(level) => configure_logging(level).expect("Failed to configure logger."),
        None => (),
    }

    let string_table = Rc::new(RefCell::new(StringTable::new()));

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let project_name = get_project_name(&src_path).unwrap();
    let sourcemap = build_source_map(&src_path, BRAID_FILE_EXT).unwrap();

    let manifests: Vec<_> = match read_manifests(&config) {
        Ok(imports) => imports,
        Err(errs) => {
            let st = string_table.clone();
            print_errs(&errs, &sourcemap, &st.borrow());
            exit(ERR_IMPORT_ERROR)
        }
    };

    let stop_stage = get_stage(&config).unwrap();

    let mut tracer = Logger::new();
    let console_writer = ConsoleWriter::new(&sourcemap);
    tracer.add_writer(&console_writer);
    if enable_tracing(&config) {
        tracer.enable();
    }

    let token_sets = match tokenize_source_map(
        &sourcemap,
        src_path,
        &mut string_table.borrow_mut(),
        &tracer,
    ) {
        Ok(ts) => ts,
        Err(errs) => {
            let st = string_table.clone();
            print_errs(&errs, &sourcemap, &st.borrow());
            exit(ERR_LEXER_ERROR)
        }
    };

    if stop_stage == Some(Stage::Lexer) {
        return;
    }

    let project_name_id = string_table.borrow_mut().insert(project_name.into());
    let root = match parse_project(
        project_name_id,
        token_sets,
        &sourcemap,
        &mut string_table.borrow_mut(),
        &tracer,
    ) {
        Ok(root) => root,
        Err(errs) => {
            print_errs(&errs, &sourcemap, &string_table.borrow());
            exit(ERR_PARSER_ERROR)
        }
    };

    if stop_stage == Some(Stage::Parser) {
        return;
    }

    // Type Check
    let imports: Result<Vec<_>, _> = manifests
        .into_iter()
        .map(|m| m.to_import(&mut string_table.borrow_mut()))
        .collect();

    let imports = match imports {
        Ok(im) => im,
        Err(msg) => {
            print_errs(&[msg], &sourcemap, &string_table.borrow());
            exit(ERR_IMPORT_ERROR)
        }
    };

    let main_mod_id = string_table.borrow_mut().insert(MAIN_MODULE.into());
    let main_fn_id = string_table.borrow_mut().insert(USER_MAIN_FN.into());
    let semantic_ast =
        match resolve_types_with_imports(&root, main_mod_id, main_fn_id, &imports, &tracer) {
            Ok(ast) => ast,
            Err(msg) => {
                print_errs(&[msg], &sourcemap, &string_table.borrow());
                std::process::exit(ERR_TYPE_CHECK);
            }
        };

    if stop_stage == Some(Stage::Semantic) {
        return;
    }

    // Configure the compiler
    let output_target = config.value_of("output").unwrap_or("./target/output.asm");

    let context = Context::create();
    let st = string_table.clone();
    let st = st.borrow();
    let mut llvm = llvm::IrGen::new(&context, &sourcemap, &st, project_name, &imports);
    match llvm.ingest(&semantic_ast, main_fn_id) {
        Ok(()) => (),
        Err(msg) => {
            println!("LLVM IR translation failed: {}", msg);
            std::process::exit(ERR_LLVM_IR_ERROR);
        }
    }

    if config.is_present("emit") {
        llvm.print(Path::new("./target/output.ll"));
    }

    llvm.emit_object_code(Path::new(output_target)).unwrap();

    if config.is_present("manifest") {
        let manifest =
            Manifest::extract(&semantic_ast, &sourcemap, &string_table.borrow()).unwrap();
        match std::fs::File::create(format!("./target/{}.manifest", project_name))
            .map_err(|e| format!("{}", e))
            .and_then(|mut f| manifest.write(&mut f).map_err(|e| format!("{}", e)))
        {
            Ok(()) => (),
            Err(e) => {
                println!("Failed to write manifest file: {}", e);
                exit(ERR_MANIFEST_WRITE_ERROR)
            }
        }
    }
}
