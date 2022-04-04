#[cfg(test)]
mod mir2llvm_tests_visual {
    //! A set of unit tests which compile small examples of Bramble code and output
    //! the LLVM IR generated by transforming MIR to LLVM IR.
    //!
    //! These visual tests are used to help with development, testing, and debugging
    //! of the MIR to LLVM transformation process. They also serve to fill in a gap
    //! caused by the complexity that is created if we try to use LLVM's JIT to do
    //! automated unit testing for MIR to LLVM transformation.
    //!
    //! While the value of these tests as automatic tests is less than the unit tests
    //! in other compiler stages, these do help insure that the LLVM libraries are being
    //! used correctly: if LLVM is incorrectly used then it will fault and the unit tests
    //! will fail.

    use inkwell::context::Context;

    use crate::{
        compiler::{
            ast::{Module, MAIN_MODULE},
            diagnostics::Logger,
            lexer::{tokens::Token, LexerError},
            mir::{transform, MirProject, ProgramTraverser},
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerDisplay, CompilerError, Lexer, SourceMap,
        },
        llvm::mir::LlvmProgramBuilder,
        resolve_types, StringTable,
    };

    #[test]
    fn var_declaration() {
        let text = "
            fn test() {
                let x: i64 := 5;
                let b: bool := true;
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn base_numerical_types() {
        let text = "
            fn test() {
                let a: i8 :=  1i8;
                let b: i16 := 2i16;
                let c: i32 := 3i32;
                let d: i64 := 4i64;

                let e: u8 :=  5u8;
                let f: u16 := 6u16;
                let g: u32 := 7u32;
                let h: u64 := 8u64;
                
                let i: f64 := 9.0;

                let bl:bool := true;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    //#[test]
    fn base_array_type() {
        let text = "
            fn test() {
                let a: [i8; 2] :=  [1i8, 2i8];

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn if_expr() {
        compile_and_print_llvm(
            "
            fn test() {
                let x: i64 := if (true) {2} else {3};
                return;
            }
        ",
        );
    }

    #[test]
    fn two_functions() {
        compile_and_print_llvm(
            "
            fn foo() {
                let x: i64 := if (true) {2} else {3};
                return;
            }
           
            mod bats {
                fn bar(a: i64) {
                    let x: i64 := 5;
                    let b: bool := true;
                    return;
                }
            }
        ",
        );
    }

    #[test]
    fn function_parameters() {
        compile_and_print_llvm(
            "
            fn foo() {
                let x: i64 := if (true) {2} else {3};
                return;
            }
           
            fn bar(a: i64, b: bool) {
                let x: i64 := 5;
                return;
            }
        ",
        );
    }

    #[test]
    fn function_return_base_value() {
        compile_and_print_llvm(
            "
            fn foo() -> i64 {
                return 5;
            }
        ",
        );
    }

    #[test]
    fn function_return_array() {
        compile_and_print_llvm(
            "
            fn foo(a: [i32; 4]) -> [i32; 4] {
                return a;
            }
        ",
        );
    }

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile_and_print_llvm(text: &str) {
        let (sm, table, module) = compile(text);
        let mut project = MirProject::new();
        transform::transform(&module, &mut project).unwrap();

        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let mut xfmr = LlvmProgramBuilder::new(&context, &module, &builder, &sm, &table);

        let proj_traverser = ProgramTraverser::new(&project);

        // Traverser is given a MirProject
        // call traverser.map(llvm) this will use the llvm xfmr to map MirProject to LlvmProject
        proj_traverser.map(&mut xfmr);

        let llvm = xfmr.complete();

        // Print LLVM
        println!("=== LLVM IR ===:");
        llvm.print_to_stderr();

        println!("\n\n=== x86 ===");
        llvm.print_asm();
    }

    fn compile(input: &str) -> (SourceMap, StringTable, Module<SemanticContext>) {
        let table = StringTable::new();
        let mut sm = SourceMap::new();
        sm.add_string(input, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let main = table.insert("main".into());
        let main_mod = table.insert(MAIN_MODULE.into());
        let main_fn = table.insert("my_main".into());

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();

        let parser = Parser::new(&logger);
        let ast = match parser.parse(main, &tokens) {
            Ok(ast) => ast.unwrap(),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        };
        match resolve_types(&ast, main_mod, main_fn, &logger) {
            Ok(module) => (sm, table, module),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        }
    }
}
