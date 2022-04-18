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

    use inkwell::{context::Context, execution_engine::JitFunction};

    use crate::{
        compiler::{
            ast::{Element, Module, Path, Type, MAIN_MODULE},
            diagnostics::Logger,
            import::{Import, ImportRoutineDef, ImportStructDef},
            lexer::{tokens::Token, LexerError},
            mir::{transform, MirProject, ProgramTraverser},
            parser::Parser,
            semantics::semanticnode::SemanticContext,
            CompilerDisplay, CompilerError, Lexer, SourceMap,
        },
        llvm::mir::LlvmProgramBuilder,
        resolve_types, resolve_types_with_imports, StringTable,
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn signed_comparisons_bounds() {
        let r: bool = compile_and_run(
            "
            fn test() -> bool {
                let x: i8 := -56i8;
                let y: i8 := 5i8;

                return x < y;
            }
        ",
            "main_test",
        );
        assert_eq!(true, r);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn unsigned_comparisons_bounds() {
        let r: bool = compile_and_run(
            "
            fn test() -> bool {
                let x: u8 := 5u8;
                let y: u8 := 200u8;

                return x < y;
            }
        ",
            "main_test",
        );
        assert_eq!(true, r);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn raw_pointer_cast_from_int() {
        let text = "
            fn test() -> i64 {
                let a: i64 :=  0;
                let p: *const i64 := a as *const i64;

                return ^p;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn raw_pointer_cast_to_int() {
        let text = "
            fn test() -> u64 {
                let a: i64 :=  0;
                let p: *const i64 := @const a;

                return p as u64;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn raw_pointer_arithmetic() {
        let text = "
            fn test() {
                let mut a: i64 :=  6;
                let p: *mut i64 := @mut a;
                let p2: *mut i64 := p@1;
                return;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn self_reference_struct() {
        let text = "
            struct S {ptr: *const S}

            fn test(a: S) {
                return;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn self_reference_struct_indirect() {
        let text = "
            struct S {s: S2, ptr: *mut S2}
            struct S2 {ptr: *const S}

            fn test(a: S) {
                return;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn self_reference_struct_pointer_to_pointer() {
        let text = "
            struct S {ptr: *const *const S}

            fn test(a: S) {
                return;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn simple_array_expression() {
        let text = "
            fn test() {
                let mut a: [i8; 2] :=  [1i8, 2i8];

                return;
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn array_in_struct_expression() {
        let text = "
            fn test() -> i64 {
                let s2: S2 := S2{x: [S{ x:[1, 2]}, S{x: [3, 4]}]};

                return s2.x[0].x[0];
            }

            struct S {
                x: [i64;2],
            }

            struct S2 {
                x: [S; 2],
            }
        ";

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
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

        compile_and_print_llvm(text, &[], &[]);
    }

    #[test]
    fn if_expr() {
        let r: i64 = compile_and_run(
            "
            fn run() -> i64 {
                let a: i64 := test(true);
                let b: i64 := test(false);

                return a + b;
            }

            fn test(b: bool) -> i64 {
                let mut y: i64 := 2;
                let x: i64 := if (b) {mut y := 3; y} else {mut y:= 4; y};
                return x;
            }
        ",
            "main_run",
        );
        assert_eq!(7, r);
    }

    #[test]
    fn if_expr_nested() {
        let r: i64 = compile_and_run(
            "
            fn run() -> i64 {
                let a: i64 := test(true, false);
                let b: i64 := test(true, true);
                let c: i64 := test(false, true);
                let d: i64 := test(false, false);

                return a + b + c + d;
            }

            fn test(b: bool, c: bool) -> i64 {
                let mut y: i64 := 2;
                let x: i64 := if (b) {
                    if (c) {
                        mut y := 3; 
                        y
                    } else {
                        mut y := 4;
                        y
                    }
                } else {
                    if (c) {
                        mut y := 5; 
                        y
                    } else {
                        mut y := 6;
                        y
                    }
                };
                return x;
            }
        ",
            "main_run",
        );
        assert_eq!(18, r);
    }

    #[test]
    fn while_expr() {
        let r: i64 = compile_and_run(
            "
            fn test() -> i64{
                let mut x: i64 := 0;

                while (x < 5) {
                    mut x:= x + 1;
                };

                return x;
            }
        ",
            "main_test",
        );
        assert_eq!(5, r);
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
            &[],
            &[],
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
            &[],
            &[],
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
            &[],
            &[],
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
            &[],
            &[],
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
            &[],
            &[],
        );
    }

    #[test]
    fn function_call_multi_args() {
        let result: i64 = compile_and_run(
            "
            fn foo() -> i64 {
                return bar(2, 1i32);
            }

            fn bar(i: i64, j: i32) -> i64 {
                return i;
            }
        ",
            "main_foo",
        );

        assert_eq!(2, result);
    }

    #[test]
    fn function_array_argument() {
        compile_and_print_llvm(
            "
            fn baz(arr: [i64; 2]) -> i64 {
                return arr[0];
            }
        ",
            &[],
            &[],
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
            &[],
            &[],
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
            &[],
            &[],
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
            &[],
            &[],
        );
    }

    #[test]
    fn extern_fn() {
        let r: u64 = compile_and_run(
            "
            extern fn llabs(i: i64) -> u64;

            fn foo() -> u64 {
                return llabs(-2);
            }
        ",
            "main_foo",
        );

        assert_eq!(2, r);
    }

    #[test]
    fn extern_fn_complex() {
        let r: f64 = compile_and_run(
            "
            extern fn fmod(a: f64, b: f64) -> f64;

            fn foo() -> f64 {
                return fmod(2.2, 3.5);
            }
        ",
            "main_foo",
        );

        assert_eq!(2.2, r);
    }

    #[test]
    fn string_literal() {
        let r: u64 = compile_and_run(
            "
            extern fn printf(s: string, ...);
            extern fn strcat(a: string, b: string) -> string;
            extern fn strlen(s: string) -> u64;

            fn foo() -> u64 {
                let s: string := bar(\"hello\", \", world\");
                printf(\"%s\\n\", s);
                return strlen(s);
            }

            fn bar(a: string, b: string) -> string {
                let s: string := strcat(a, b);
                return s;
            }
        ",
            "main_foo",
        );
        assert_eq!("hello, world".len(), r as usize);
    }

    #[test]
    fn cast_int_to_uint() {
        let r: u64 = compile_and_run(
            "
            fn test() -> u64 {
                let mut x: i64 := -9;

                return x as u64;
            }
        ",
            "main_test",
        );
        assert_eq!(18446744073709551607, r);
    }

    #[test]
    fn cast_int_to_float() {
        let r: f64 = compile_and_run(
            "
            fn test() -> f64 {
                let mut x: i64 := -9;

                return x as f64;
            }
        ",
            "main_test",
        );
        assert_eq!(-9.0, r);
    }

    #[test]
    fn cast_float_to_int() {
        let r: i64 = compile_and_run(
            "
            fn test() -> i64 {
                let mut x: f64 := -9.0;

                return x as i64;
            }
        ",
            "main_test",
        );
        assert_eq!(-9, r);
    }

    #[test]
    fn cast_float_to_uint() {
        let r: u64 = compile_and_run(
            "
            fn test() -> u64 {
                let mut x: f64 := -9.0;

                return x as u64;
            }
        ",
            "main_test",
        );
        assert_eq!(18446744073709551607, r);
    }

    #[test]
    fn cast_float_to_float() {
        let r: f64 = compile_and_run(
            "
            fn test() -> f64 {
                let mut x: f64 := -9.0;

                return x as f64;
            }
        ",
            "main_test",
        );
        assert_eq!(-9.0, r);
    }

    #[test]
    fn import_function() {
        compile_and_print_llvm(
            "
            fn baz() -> bool {
                return project::import::foobar(1u16, 5.0);
            }
        ",
            &[(
                "$import::foobar",
                &[("a", Type::U16), ("b", Type::F64)],
                Type::Bool,
            )],
            &[],
        );
    }

    #[test]
    fn import_structure() {
        compile_and_print_llvm(
            "
            fn baz() -> bool {
                let d: project::import::Data := project::import::Data{x: 5.0, y: 3u64};
                return true;
            }
        ",
            &[],
            &[("$import::Data", &[("x", Type::F64), ("y", Type::U64)])],
        );
    }

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn compile_and_print_llvm(
        text: &str,
        import_funcs: &[(&str, &[(&str, Type)], Type)],
        import_structs: &[(&str, &[(&str, Type)])],
    ) {
        let (sm, table, module, imports) = compile(text, import_funcs, import_structs);
        let mut project = MirProject::new();

        transform::transform(&module, &imports, &mut project).unwrap();

        println!("=== MIR ===:");
        println!("{}\n\n", project);

        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let main_name = table.insert("my_main".into());
        let mut xfmr = LlvmProgramBuilder::new(&context, &module, &builder, &sm, &table, main_name);

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

    fn compile_and_run<R: std::fmt::Debug>(text: &str, func_name: &str) -> R {
        let (sm, table, module, _) = compile(text, &[], &[]);
        let mut project = MirProject::new();
        transform::transform(&module, &[], &mut project).unwrap();

        println!("=== MIR ===:");
        println!("{}\n\n", project);

        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let main_name = table.insert("my_main".into());

        let mut xfmr = LlvmProgramBuilder::new(&context, &module, &builder, &sm, &table, main_name);

        let proj_traverser = ProgramTraverser::new(&project);

        // Traverser is given a MirProject
        // call traverser.map(llvm) this will use the llvm xfmr to map MirProject to LlvmProject
        proj_traverser.map(&mut xfmr);

        let llvm = xfmr.complete();

        // Print LLVM
        println!("=== LLVM IR ===:");
        llvm.print_to_stderr();

        let engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        unsafe {
            let fv: JitFunction<unsafe extern "C" fn() -> R> =
                engine.get_function(func_name).unwrap();
            fv.call()
        }
    }

    fn compile(
        input: &str,
        import_funcs: &[(&str, &[(&str, Type)], Type)],
        import_structs: &[(&str, &[(&str, Type)])],
    ) -> (SourceMap, StringTable, Module<SemanticContext>, Vec<Import>) {
        let table = StringTable::new();

        // Create an import
        let import_funcs = import_funcs
            .into_iter()
            .map(|(p, args, ret)| {
                let path = string_to_path(&table, p).unwrap();
                let args = args
                    .iter()
                    .map(|(n, t)| {
                        let sid = table.insert((*n).into());
                        (sid, t.clone())
                    })
                    .collect();
                ImportRoutineDef::new(path, args, ret.clone())
            })
            .collect();
        let import_structs = import_structs
            .into_iter()
            .map(|(p, fields)| {
                let path = string_to_path(&table, p).unwrap();
                let fields = fields
                    .iter()
                    .map(|(name, ty)| (table.insert((*name).into()), ty.clone()))
                    .collect();
                ImportStructDef::new(path, fields)
            })
            .collect();
        let import = Import {
            structs: import_structs,
            funcs: import_funcs,
        };
        let imports = vec![import];

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
        match resolve_types_with_imports(&ast, main_mod, main_fn, &imports, &logger) {
            Ok(module) => (sm, table, module, imports),
            Err(err) => {
                panic!("{}", err.fmt(&sm, &table).unwrap());
            }
        }
    }

    /// Convert a Manifest file Path string to a Compiler Path value.
    fn string_to_path(st: &StringTable, p: &str) -> Option<Path> {
        /// Tests that an element is a valid identifier
        fn is_element_valid(el: &str) -> Option<()> {
            let cs = el.chars().collect::<Vec<_>>();
            if cs.is_empty() {
                // Element must have at least one character
                None
            } else {
                if !(cs[0].is_alphabetic() || cs[0] == '_') {
                    // Element can only start with a letter or underscore
                    None
                } else {
                    // Element can only contain alphanumerics and _
                    match cs.iter().find(|&&c| !(c.is_alphanumeric() || c == '_')) {
                        Some(c) => None,
                        None => Some(()),
                    }
                }
            }
        }

        // Check if this is a canonical path, and remove the $ if it is
        let (p, is_canonical) = match p.strip_prefix('$') {
            Some(stripped) => (stripped, true),
            None => (p, false),
        };
        let elements = p.split("::");

        let mut path = vec![];
        if is_canonical {
            path.push(Element::CanonicalRoot)
        }

        for el in elements {
            is_element_valid(el)?;

            match el {
                "self" => path.push(Element::Selph),
                "super" => path.push(Element::Super),
                "root" => path.push(Element::FileRoot),
                e => path.push(Element::Id(st.insert(e.into()))),
            }
        }

        Some(path.into())
    }
}
