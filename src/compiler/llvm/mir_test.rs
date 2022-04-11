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

    #[test]
    fn base_arithmetic() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := 2;
                let c: i64 := 3;
                let d: i64 := 4;
                let x: i64 := a + b + c + d + 5;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn int_subtraction() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := 2;
                let c: i64 := 3;
                let d: i64 := 4;
                let x: i64 := a - b - c - d - 5;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn int_multiplication() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := 2;
                let c: i64 := 3;
                let d: i64 := 4;
                let x: i64 := a * b * c * d * 5;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn signed_int_division() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := 2;
                let c: i64 := 3;
                let d: i64 := 4;
                let x: i64 := a / b / c / d / 5;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn unsigned_int_division() {
        let text = "
            fn test() {
                let a: u64 := 1u64;
                let b: u64 := 2u64;
                let x: u64 := a / b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn fp_arithmetic() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let b: f64 := 2.0;
                let c: f64 := 3.0;
                let d: f64 := 4.0;
                let x: f64 := a + b + c + d + 5.0;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn fp_subtraction() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let b: f64 := 2.0;
                let c: f64 := 3.0;
                let d: f64 := 4.0;
                let x: f64 := a - b - c - d - 5.0;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn fp_multiplication() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let b: f64 := 2.0;
                let c: f64 := 3.0;
                let d: f64 := 4.0;
                let x: f64 := a * b * c * d * 5.0;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn fp_division() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let b: f64 := 2.0;
                let c: f64 := 3.0;
                let d: f64 := 4.0;
                let x: f64 := a / b / c / d / 5.0;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn fp_negate() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let x: f64 := -a;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn signed_int_comparison() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := 2;

                a == b;
                a != b;
                a < b;
                a <= b;
                a > b;
                a >= b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn unsigned_int_comparison() {
        let text = "
            fn test() {
                let a: u64 := 1u64;
                let b: u64 := 2u64;

                a == b;
                a != b;
                a < b;
                a <= b;
                a > b;
                a >= b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn float_comparison() {
        let text = "
            fn test() {
                let a: f64 := 1.0;
                let b: f64 := 2.0;

                a == b;
                a != b;
                a < b;
                a <= b;
                a > b;
                a >= b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn signed_int_negate() {
        let text = "
            fn test() {
                let a: i64 := 1;
                let b: i64 := -a;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn bool_not() {
        let text = "
            fn test() {
                let a: bool := true;
                let x: bool := !a;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn bool_and() {
        let text = "
            fn test() {
                let a: bool := true && true;
                let b: bool := true && false;
                let x: bool := a && b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn bool_or() {
        let text = "
            fn test() {
                let a: bool := true || false;
                let b: bool := false || false;
                let x: bool := a || b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn bool_eq() {
        let text = "
            fn test() {
                let a: bool := false == false;
                let b: bool := true ==  false;
                let x: bool := a == b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn bool_neq() {
        let text = "
            fn test() {
                let a: bool := false != true;
                let b: bool := false !=  false;
                let x: bool := a != b;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn raw_pointer() {
        let text = "
            fn test() {
                let a: i64 :=  6;
                let p: *const i64 := @const a;

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn raw_pointer_deref() {
        let text = "
            fn test() -> i64 {
                let a: i64 :=  6;
                let p: *const i64 := @const a;

                return ^p;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn raw_pointer_deref_mutate() {
        let text = "
            fn test() {
                let mut a: i64 :=  6;
                let p: *mut i64 := @mut a;
                mut ^p := 13;
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn raw_pointer_struct() {
        let text = "
            struct S{a: i64}

            fn test() {
                let mut a: S :=  S{a: 5};
                let p: *mut S := @mut a;
                mut (^p).a := 13;
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn raw_pointer_array() {
        let text = "
            fn test() {
                let mut a: [i64; 2] :=  [1, 2];
                let p: *mut [i64; 2] := @mut a;
                mut (^p)[0] := 13;
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn simple_array_expression() {
        let text = "
            fn test() {
                let mut a: [i8; 2] :=  [1i8, 2i8];

                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn base_array_expressions() {
        let text = "
            fn test() -> f64 {
                let mut a: [i8; 2] :=  [1i8, 2i8];
                let b: [i8; 4] := [3i8, 4i8, 5i8, 6i8];
                let c: [f64; 1] := [5.3];

                let e: i8 := a[0];
                mut a[1] := b[3];

                return c[0];
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_def() {
        let text = "
            struct S {a: i64}
            struct S2 {s: S}

            fn test(a: S2) {
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_expression() {
        let text = "
            struct S {a: i64, b: f64}

            fn test() {
                let a: S := S{a: 5, b: 3.0};
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_expression_nested() {
        let text = "
            struct S {a: i64, b: f64}
            struct S2 {x: i64, s: S}

            fn test() {
                let a: S2 := S2{ x: 3, s: S{a: 5, b: 3.0}};
                return;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_member_access() {
        let text = "
            struct S {a: i64, b: f64}

            fn test() -> i64 {
                let a: S := S{a: 5, b: 3.0};
                return a.a;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_function_parameter() {
        let text = "
            struct S {a: i64, b: f64}

            fn test(s: S) -> i64 {
                let a: f64 := s.b;
                return s.a;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_function_return() {
        let text = "
            struct S {a: i64, b: f64}

            fn test() -> S {
                let a: S := S{a: 5, b: 3.0};
                return a;
            }
        ";

        compile_and_print_llvm(text);
    }

    #[test]
    fn struct_mutate_field() {
        let text = "
            struct S {a: i64, b: f64}

            fn test() {
                let mut a: S := S{a: 5, b: 3.0};
                mut a.a := 27;
                mut a.b := 19.0;
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
    fn var_scopes() {
        compile_and_print_llvm(
            "
            fn test() -> i64{
                let x: i64 := 1;

                if (true) {
                    let a: i64 := x;
                    let x: i64 := 2;
                    let b: i64 := x;
                } else {
                    let a: i64 := x;
                    let x: i64 := 3;
                    let b: i64 := x;
                };

                return x;
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

            fn bar() -> f64 {
                return 5.0;
            }
        ",
        );
    }

    #[test]
    fn function_call() {
        compile_and_print_llvm(
            "
            fn blah() {
                goo();
                goo();
                goo();

                return;
            }

            fn goo() {
                foo();

                return;
            }

            fn foo() -> i64 {
                return bar(2);
            }

            fn bar(i: i64) -> i64 {
                return 5;
            }
        ",
        );
    }

    #[test]
    fn function_call_multi_args() {
        compile_and_print_llvm(
            "
            fn foo() -> i64 {
                return bar(2, 1i32);
            }

            fn bar(i: i64, j: i32) -> i64 {
                return i;
            }
        ",
        );
    }

    #[test]
    fn function_array_argument() {
        compile_and_print_llvm(
            "
            fn baz(arr: [i64; 2]) -> i64 {
                return arr[0];
            }
        ",
        );
    }

    #[test]
    fn function_return_array() {
        compile_and_print_llvm(
            "
            fn baz() -> [i64; 2] {
                return [1, 2];
            }
        ",
        );
    }

    #[test]
    fn function_return_array_parameter() {
        compile_and_print_llvm(
            "
            fn baz(a: [i64; 2]) -> [i64; 2] {
                return a;
            }
        ",
        );
    }

    #[test]
    fn function_call_return_array() {
        compile_and_print_llvm(
            "
            fn foo() {
                let a: [i64; 2] := bar();
                return;
            }

            fn bar() -> [i64; 2] {
                return [1, 2];
            }
        ",
        );
    }

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile_and_print_llvm(text: &str) {
        let (sm, table, module) = compile(text);
        let mut project = MirProject::new();
        transform::transform(&module, &mut project).unwrap();

        println!("=== MIR ===:");
        println!("{}\n\n", project);

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
