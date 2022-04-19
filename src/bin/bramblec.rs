extern crate log;
extern crate simplelog;

use std::fs::File;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Instant;

use bramble_lang::compiler::diagnostics::Logger;
use bramble_lang::compiler::import::Import;
use bramble_lang::compiler::semantics::semanticnode::SemanticContext;
use bramble_lang::compiler::{transform, MirProject};
use bramble_lang::diagnostics::{write_source_map, ConsoleWriter, JsonWriter};
use inkwell::context::Context;

use bramble_lang::project::*;
use bramble_lang::*;

use bramble_lang::compiler::ast::{Module, MAIN_MODULE};

const BRAID_FILE_EXT: &str = "br";
const USER_MAIN_FN: &str = "my_main";

fn main() -> Result<(), i32> {
    let config = configure_cli().get_matches();

    if let Some(level) = get_log_level(&config) {
        configure_logging(level).expect("Failed to configure logger.")
    }

    let string_table = StringTable::new();

    let input = config
        .value_of("input")
        .expect("Expected an input source file to compile");
    let src_path = Path::new(input);
    let project_name =
        get_project_name(src_path).unwrap_or_else(|_| panic!("Could not open {:?}", src_path));
    let source_map = build_source_map(src_path, BRAID_FILE_EXT).unwrap();

    let manifests: Vec<_> = match read_manifests(&config) {
        Ok(imports) => imports,
        Err(errs) => {
            print_errs(&errs, &source_map, &string_table);
            return Err(ERR_IMPORT_ERROR);
        }
    };

    let stop_stage = get_stage(&config).unwrap();

    // Setup tracing system
    let mut tracer = Logger::new();
    if enable_tracing(&config) || enable_json_tracing(&config) {
        tracer.enable();
    }

    // Setup trace console writer
    let console_writer = ConsoleWriter::new(&source_map, &string_table);
    if enable_tracing(&config) {
        tracer.add_writer(&console_writer);
    }

    // Setup JSON Trace writer
    let trace_file = File::create("./target/trace.json").unwrap();
    let json_writer = JsonWriter::new(trace_file, &source_map, &string_table);
    if enable_json_tracing(&config) {
        tracer.add_writer(&json_writer);

        let source_map_file = File::create("./target/sourcemap.json").unwrap();
        write_source_map(source_map_file, &source_map);
    }

    let tokenize_time = Instant::now();
    let token_sets = match tokenize_source_map(&source_map, src_path, &string_table, &tracer) {
        Ok(ts) => ts,
        Err(errs) => {
            print_errs(&errs, &source_map, &string_table);
            return Err(ERR_LEXER_ERROR);
        }
    };
    let tokenize_duration = tokenize_time.elapsed();
    eprintln!("Lexer: {}", tokenize_duration.as_secs_f32());

    if stop_stage == Some(Stage::Lexer) {
        return Ok(());
    }

    let parse_time = Instant::now();
    let project_name_id = string_table.insert(project_name.into());
    let root = match parse_project(
        project_name_id,
        token_sets,
        &source_map,
        &string_table,
        &tracer,
    ) {
        Ok(root) => root,
        Err(errs) => {
            print_errs(&errs, &source_map, &string_table);
            return Err(ERR_PARSER_ERROR);
        }
    };
    let parse_duration = parse_time.elapsed();
    eprintln!("Parser: {}", parse_duration.as_secs_f32());

    if stop_stage == Some(Stage::Parser) {
        return Ok(());
    }

    // Type Check
    let imports: Result<Vec<_>, _> = manifests
        .into_iter()
        .map(|m| m.to_import(&string_table))
        .collect();

    let imports = match imports {
        Ok(im) => im,
        Err(msg) => {
            print_errs(&[msg], &source_map, &string_table);
            return Err(ERR_IMPORT_ERROR);
        }
    };

    let main_mod_id = string_table.insert(MAIN_MODULE.into());
    let main_fn_id = string_table.insert(USER_MAIN_FN.into());
    let semantic_time = Instant::now();
    let semantic_ast =
        match resolve_types_with_imports(&root, main_mod_id, main_fn_id, &imports, &tracer) {
            Ok(ast) => ast,
            Err(msg) => {
                print_errs(&[msg], &source_map, &string_table);
                return Err(ERR_TYPE_CHECK);
            }
        };
    let semantic_duration = semantic_time.elapsed();
    eprintln!("Semantic: {}", semantic_duration.as_secs_f32());

    if stop_stage == Some(Stage::Semantic) {
        return Ok(());
    }

    // Configure the compiler
    let output_target = config.value_of("output").unwrap_or("./target/output.asm");
    if !enable_mir_beta(&config) {
        let llvm_time = Instant::now();
        let context = Context::create();
        let mut llvm = llvm::IrGen::new(
            &context,
            project_name,
            &imports,
            &source_map,
            &string_table,
            &tracer,
        );
        match llvm.ingest(&semantic_ast, main_fn_id) {
            Ok(()) => (),
            Err(msg) => {
                println!("LLVM IR translation failed: {}", msg);
                return Err(ERR_LLVM_IR_ERROR);
            }
        }

        if emit_llvm_ir(&config) {
            llvm.emit_llvm_ir(Path::new(&format!("./target/{}.ll", project_name)));
        }

        llvm.emit_object_code(Path::new(output_target), emit_asm(&config))
            .unwrap();

        let llvm_duration = llvm_time.elapsed();
        eprintln!("LLVM: {}", llvm_duration.as_secs_f32());
    } else {
        eprintln!("MIR BETA!! :D");

        let mir_time = Instant::now();
        let mir = gen_mir(&semantic_ast, &imports);
        let mir_duration = mir_time.elapsed();
        eprintln!("MIR Generation: {}", mir_duration.as_secs_f32());

        if emit_mir(&config) {
            println!("=== MIR ===\n\n{}", mir);
        }

        let path = Path::new(output_target);
        let llvm_time = Instant::now();
        gen_llvm(
            project_name,
            &mir,
            main_fn_id,
            &source_map,
            &string_table,
            path,
            emit_llvm_ir(&config),
            emit_asm(&config),
        );

        let llvm_duration = llvm_time.elapsed();
        eprintln!("MIR 2 LLVM: {}", llvm_duration.as_secs_f32());
    }

    if config.is_present("manifest") {
        let manifest = Manifest::extract(&semantic_ast, &source_map, &string_table).unwrap();
        match std::fs::File::create(format!("./target/{}.manifest", project_name))
            .map_err(|e| format!("{}", e))
            .and_then(|mut f| manifest.write(&mut f).map_err(|e| format!("{}", e)))
        {
            Ok(()) => (),
            Err(e) => {
                println!("Failed to write manifest file: {}", e);
                return Err(ERR_MANIFEST_WRITE_ERROR);
            }
        }
    }

    Ok(())
}

fn gen_mir(module: &Module<SemanticContext>, imports: &[Import]) -> MirProject {
    let mut project = MirProject::new();
    transform::transform(module, imports, &mut project).unwrap();
    project
}

fn gen_llvm(
    name: &str,
    mir: &MirProject,
    main_name: StringId,
    sm: &compiler::SourceMap,
    table: &StringTable,
    output: &Path,
    emit_ir: bool,
    emit_asm: bool,
) {
    let context = Context::create();
    let module = context.create_module(name);
    let builder = context.create_builder();

    let mut xfmr = llvm::LlvmProgramBuilder::new(&context, &module, &builder, sm, table, main_name);

    let proj_traverser = compiler::ProgramTraverser::new(mir, sm, table);

    // Traverser is given a MirProject
    // call traverser.map(llvm) this will use the llvm xfmr to map MirProject to LlvmProject
    proj_traverser.map(&mut xfmr);

    let llvm = xfmr.complete();

    if emit_ir {
        llvm.emit_llvm_ir(Path::new(&format!("./target/{}.ll", name)))
            .unwrap();
    }

    let p = PathBuf::from_str(&format!("./target/{}.s", name)).unwrap();
    let asm_file = if emit_asm { Some(p.as_path()) } else { None };

    llvm.emit_object_code(asm_file, output)
}
