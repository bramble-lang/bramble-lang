extern crate log;
extern crate simplelog;

use std::{path::Path, process::exit};

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

    let mut string_table = StringTable::new();

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let project_name = get_project_name(&src_path).unwrap();
    //let src_input = read_src_files(&src_path, BRAID_FILE_EXT);
    let sourcemap = build_source_map(&src_path, BRAID_FILE_EXT).unwrap();

    let manifests: Vec<_> = match read_manifests(&config) {
        Ok(imports) => imports,
        Err(errs) => {
            print_errs(&string_table, &errs);
            exit(ERR_IMPORT_ERROR)
        }
    };

    let stop_stage = get_stage(&config).unwrap();

    let trace_lexer = get_lexer_tracing(&config);
    let token_sets = match tokenize_source_map(&mut string_table, sourcemap, trace_lexer, src_path)
    {
        Ok(ts) => ts,
        Err(errs) => {
            print_errs(&string_table, &errs);
            exit(ERR_LEXER_ERROR)
        }
    };

    if stop_stage == Some(Stage::Lexer) {
        return;
    }

    let trace_parser = get_parser_tracing(&config);
    let project_name_id = string_table.insert(project_name.into());
    let root = match parse_project(&mut string_table, project_name_id, token_sets, trace_parser) {
        Ok(root) => root,
        Err(errs) => {
            print_errs(&string_table, &errs);
            exit(ERR_PARSER_ERROR)
        }
    };

    if stop_stage == Some(Stage::Parser) {
        return;
    }

    // Type Check
    let trace_semantic_node = get_semantic_node_tracing(&config);
    let trace_canonization = get_canonization_tracing(&config);
    let trace_type_resolver = get_type_resolver_tracing(&config);

    let imports: Result<Vec<_>, _> = manifests
        .iter()
        .map(|m| m.to_import(&mut string_table))
        .collect();

    let imports = match imports {
        Ok(im) => im,
        Err(msg) => {
            print_errs(&string_table, &[msg]);
            exit(ERR_IMPORT_ERROR)
        }
    };

    let main_mod_id = string_table.insert(MAIN_MODULE.into());
    let main_fn_id = string_table.insert(USER_MAIN_FN.into());
    let semantic_ast = match resolve_types_with_imports(
        &root,
        main_mod_id,
        main_fn_id,
        &imports,
        trace_semantic_node,
        trace_canonization,
        trace_type_resolver,
    ) {
        Ok(ast) => ast,
        Err(msg) => {
            print_errs(&string_table, &[msg]);
            std::process::exit(ERR_TYPE_CHECK);
        }
    };

    if stop_stage == Some(Stage::Semantic) {
        return;
    }

    // Configure the compiler
    let output_target = config.value_of("output").unwrap_or("./target/output.asm");

    let context = Context::create();
    let mut llvm = llvm::IrGen::new(&context, &string_table, project_name, &imports);
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
        let manifest = Manifest::extract(&string_table, &semantic_ast).unwrap();
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
