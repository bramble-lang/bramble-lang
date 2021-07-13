#[cfg(test)]
mod type_resolver_tests {
    use crate::{
        compiler::{
            ast::*, lexer::tokens::Token, parser::parser,
            semantics::semanticnode::SemanticContext, Lexer,
        },
        diagnostics::config::TracingConfig,
        project::manifest::Manifest,
    };

    use super::super::super::type_resolver::*;
    use braid_lang::result::Result;

    #[test]
    pub fn test_identifiers() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> u8 {
                    let k: u8 := 5u8;
                    return k;
                }",
                Ok(Type::U8),
            ),
            (
                "fn main() -> u16 {
                    let k: u16 := 5u16;
                    return k;
                }",
                Ok(Type::U16),
            ),
            (
                "fn main() -> u32 {
                    let k: u32 := 5u32;
                    return k;
                }",
                Ok(Type::U32),
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5u64;
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i16 {
                    let k: i16 := 5i16;
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                "fn main() -> i8 {
                    let k: i8 := 5i8;
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return k;
                }",
                Ok(Type::Bool),
            ),
            (
                "fn main() -> string {
                    let k: string := \"hello\";
                    return k;
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := false;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i64 but got bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i32 but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i16 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i16 but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i8 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i8 but got i64"),
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected u64 but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("test", &tokens)
                .expect(&format!("{}", text))
                .unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_path_to_function() {
        for (ln, text, expected) in vec![
            (
                line!(),
                "mod my_mod{ 
                    fn test() -> i64{ 
                        return 0;
                    } 
                    fn main() {
                        let i: i64 := self::test(); 
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{ 
                    fn test() -> i64{ 
                        return 0;
                    } 
                    fn main() {
                        let k: i64 := test();
                        let i: i64 := self::test(); 
                        let j: i64 := root::my_mod::test();
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{ 
                    fn test() -> i64{ 
                        return 0;
                    } 
                    fn main() {
                        let j: i64 := project::test::my_mod::test();
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                    fn main() {
                        let i: i64 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test ($test::my_mod::my_mod::test)"),
            ),
        ] {
            println!("Test: {}", ln);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("test", &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test]
    pub fn test_path_to_function_in_different_module() {
        for (text,) in vec![
            ("mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                }
                mod main_mod{
                    fn main() {
                        let j: i64 := root::my_mod::test();
                        return;
                    }
                }",),
            ("mod my_mod{ 
                    mod inner {
                        fn test() -> i64{ return 0;} 
                    }
                }
                mod main_mod{
                    fn main() {
                        let j: i64 := root::my_mod::inner::test();
                        return;
                    }
                }",),
            ("
                mod main_mod{
                    fn main() {
                        let j: i64 := root::main_mod::inner::test();
                        let k: i64 := inner::test();
                        let l: i64 := self::inner::test();
                        return;
                    }

                    mod inner {
                        fn test() -> i64{ return 0;} 
                    }
                }",),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("test", &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            assert!(result.is_ok());
        }
    }

    #[test]
    pub fn test_path_to_struct() {
        for (text, expected) in vec![
            (
                "mod my_mod{ 
                    struct test{i: i64}

                    fn main() {
                        let k: test := test{i: 5};
                        let i: self::test := self::test{i: 5}; 
                        let j: root::my_mod::test := root::my_mod::test{i: 5};
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                "mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                    fn main() {
                        let i: i64 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test ($test::my_mod::my_mod::test)"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("test", &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(_) => assert!(result.is_ok(), "Expected Ok got {:?}", result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_expression_renamed_with_canonical_path() {
        let mut test_id = 0;
        for text in vec![
            "
                struct test{i: i64}

                fn main() {
                    let k: test := test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i64}

                fn main() {
                    let k: test := root::test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i64}

                fn main() {
                    let k: test := self::test{i: 5};
                    return;
                }
                ",
        ] {
            test_id += 1;
            println!("Test: {}", test_id);

            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("test", &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { body, .. }) = &result.get_functions()[0] {
                if let Statement::Bind(box b) = &body[0] {
                    if let Expression::StructExpression(_, struct_name, ..) = b.get_rhs() {
                        let expected: Path = vec![CANONICAL_ROOT, "test", "test"].into();
                        assert_eq!(struct_name, &expected)
                    } else {
                        panic!("Not a struct expression")
                    }
                } else {
                    panic!("Not a bind")
                }
            } else {
                panic!("Not a function")
            }
        }
    }

    #[test]
    pub fn test_my_main_signature() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn my_main() -> i64 {
                    return 0;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn my_main() -> i32 {
                    return 0i32;
                }",
                Err("Semantic: L1: my_main must return an i64. It must be of type () -> i64"),
            ),
            (
                line!(),
                "fn my_main(i: i32) -> i64 {
                    return 0;
                }",
                Err("Semantic: L1: my_main must take no parameters. It must be of type () -> i64"),
            ),
            (
                line!(),
                "co my_main() -> i64 {
                    return 0;
                }",
                Err("Semantic: L1: my_main must be a function of type () -> i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match (expected, module) {
                (Ok(expected_ty), Ok(actual)) => {
                    let fn_main = actual.get_functions()[0].to_routine().unwrap();

                    assert_eq!(
                        fn_main.annotation().ty,
                        expected_ty,
                        "Test Case at L:{}",
                        line
                    );
                }
                (Ok(_), Err(actual)) => {
                    assert!(false, "L{}: Expected OK, got Err({})", line, actual);
                }
                (Err(expected), Ok(_)) => {
                    assert!(false, "L{}: Expected Err({}), but got Ok", line, expected);
                }
                (Err(msg), Err(actual)) => {
                    assert_eq!(actual, msg, "Test Case at L:{}", line);
                }
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_function_params_renamed_with_canonical_path() {
        for text in vec![
            "
                struct test{i: i64}

                fn main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { params, .. }) = &result.get_functions()[0] {
                if let Parameter {
                    ty: Type::Custom(ty_path),
                    ..
                } = &params[0]
                {
                    let expected: Path = vec![CANONICAL_ROOT, MAIN_MODULE, "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a function")
            }
        }
    }

    #[test]
    pub fn test_coroutine_params_renamed_with_canonical_path() {
        for text in vec![
            "
                struct test{i: i64}

                co main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { params, .. }) = &result.get_coroutines()[0] {
                if let Type::Custom(ty_path) = &params[0].ty {
                    let expected: Path = vec![CANONICAL_ROOT, MAIN_MODULE, "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a coroutine")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_def_fields_are_converted_to_canonical_paths() {
        for text in vec![
            "
                struct test{i: i64}

                struct test2{t: test}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let result = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Struct(s) = &result.get_structs()[1] {
                let fields = s.get_fields();
                if let Type::Custom(ty_path) = &fields[0].ty {
                    let expected: Path = vec![CANONICAL_ROOT, MAIN_MODULE, "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a structure")
            }
        }
    }

    #[test]
    pub fn test_integer_arithmetic_type_checking() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 1 + 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := (1 + 5i64) * (3 - 4/(2 + 3));
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := 1i32 + 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2i32 + 3i32));
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> i16 {
                    let k: i16 := 1i16 + 5i16;
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                line!(),
                "fn main() -> i16 {
                    let k: i16 := (1i16 + 5i16) * (3i16 - 4i16/(2i16 + 3i16));
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                line!(),
                "fn main() -> i8 {
                    let k: i8 := 1i8 + 5i8;
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                line!(),
                "fn main() -> i8 {
                    let k: i8 := (1i8 + 5i8) * (3i8 - 4i8/(2i8 + 3i8));
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                line!(),
                "fn main() -> u64 {
                    let k: u64 := 1u64 + 5u64;
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                line!(),
                "fn main() -> u8 {
                    let k: u8 := (1u8 + 5u8) * (3u8 - 4u8/(2u8 + 3u8));
                    return k;
                }",
                Ok(Type::U8),
            ),
            (
                line!(),
                "fn main() -> u16 {
                    let k: u16 := (1u16 + 5u16) * (3u16 - 4u16/(2u16 + 3u16));
                    return k;
                }",
                Ok(Type::U16),
            ),
            (
                line!(),
                "fn main() -> u32 {
                    let k: u32 := (1u32 + 5u32) * (3u32 - 4u32/(2u32 + 3u32));
                    return k;
                }",
                Ok(Type::U32),
            ),
            (
                line!(),
                "fn main() -> u64 {
                    let k: u64 := (1u64 + 5u64) * (3u64 - 4u64/(2u64 + 3u64));
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2i32 + 3));
                    return k;
                }",
                Err("Semantic: L2: + expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2 + 3));
                    return k;
                }",
                Err("Semantic: L2: / expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1 + 5i32;
                    return k;
                }",
                Err("Semantic: L2: + expected i64 but found i64 and i32"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i32 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i8 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i8 but found i8 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: u64 := 1u64 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected u64 but found u64 and i64"), // TODO: Change this error message to specify the right operand is wrong
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i16 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i16 but found i16 and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    assert!(module.is_ok(), "Test Case at L:{}", line);
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(
                            bind_stm.annotation().ty,
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                        assert_eq!(
                            b.get_rhs().get_type(),
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(
                            r.get_value().clone().unwrap().get_type(),
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg, "Test Case at L:{}", line);
                }
            }
        }
    }

    #[test]
    pub fn test_unary_ops() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return -k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return -k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i16 {
                    let k: i16 := 5i16;
                    return -k;
                }",
                Ok(Type::I16),
            ),
            (
                "fn main() -> i8 {
                    let k: i8 := 5i8;
                    return -k;
                }",
                Ok(Type::I8),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return -k;
                }",
                Err("Semantic: L3: - expected i32 or i64 but found bool"), // TODO: Change this error message to include i8 and i16
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5u64;
                    return -k;
                }",
                Err("Semantic: L3: - expected i32 or i64 but found u64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return !k;
                }",
                Ok(Type::Bool),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return !k;
                }",
                Err("Semantic: L3: ! expected bool but found i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_add_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 1 + 5;
                    return k + 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 1 + false;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i64 but found i64 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := \"hello\" + 5;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i64 but found string and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k + 3;
                }",
                Err("Semantic: L3: + expected i64 but found bool and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(bind_stm.annotation().ty, Type::I64);
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mul_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 1 * 5;
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 1 * false;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i64 but found i64 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := \"hello\" * 5;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i64 but found string and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k * 3;
                }",
                Err("Semantic: L3: * expected i64 but found bool and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_and_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true && false;
                    return k && true;
                }",
                Ok(Type::Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true && 1;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found bool and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" && false;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k && true;
                }",
                Err("Semantic: L3: && expected bool but found i64 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_or_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true || false;
                    return k || true;
                }",
                Ok(Type::Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true || 1;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found bool and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" || false;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k || true;
                }",
                Err("Semantic: L3: || expected bool but found i64 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_comparison_op() {
        for op in vec!["<", ">", "<=", ">=", "==", "!="] {
            for (text, expected) in vec![
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Ok(Type::Bool),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} true;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected i64 but found i64 and bool",
                        op
                    )),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := false {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected bool but found bool and i64",
                        op
                    )),
                ),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_>>()
                    .unwrap();
                let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
                let module = resolve_types(
                    &ast,
                    "my_main",
                    TracingConfig::Off,
                    TracingConfig::Off,
                    TracingConfig::Off,
                    TracingConfig::Off,
                );
                match expected {
                    Ok(expected_ty) => {
                        let module = module.unwrap();
                        let fn_main = module.get_functions()[0].to_routine().unwrap();

                        let bind_stm = &fn_main.get_body()[0];
                        assert_eq!(bind_stm.get_type(), Type::Bool);

                        // validate that the RHS of the bind is the correct type
                        if let Statement::Bind(box b) = bind_stm {
                            assert_eq!(b.get_rhs().get_type(), expected_ty);
                        } else {
                            panic!("Expected a bind statement");
                        }

                        // Validate that the return statement is the correct type
                        let ret_stm = &fn_main.get_body()[1];
                        assert_eq!(ret_stm.get_type(), expected_ty);
                        if let Statement::Return(box r) = ret_stm {
                            assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                    Err(msg) => {
                        assert_eq!(module.unwrap_err(), msg);
                    }
                }
            }
        }
    }

    #[test]
    pub fn test_array_size_types() {
        for ty in vec!["u64", "u32", "u16", "u8", "i64", "i32", "i16", "i8"] {
            let text = format!(
                "fn main() -> i64 {{
                    let a: [i64; 2{}] := [1, 2,];
                    return 0;
                }}",
                ty
            );
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            let module = module.unwrap();
            let fn_main = module.get_functions()[0].to_routine().unwrap();

            let bind_stm = &fn_main.get_body()[0];
            assert_eq!(bind_stm.annotation().ty, Type::Array(box Type::I64, 2));
        }
    }

    #[test]
    pub fn test_array_element_types() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let a: [i64; 5] := [1, 2, 3, 4, 5];
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i64 {
                    let a: [[i64; 1]; 1] := [[1]];
                    let k: i64 := a[0][0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i8 {
                    let a: [i8; 5] := [1i8, 2i8, 3i8, 4i8, 5i8];
                    let k: i8 := a[0];
                    return k * 3i8;
                }",
                Ok(Type::I8),
            ),
            (
                "fn main() -> u8 {
                    let a: [u8; 5] := [1u8, 2u8, 3u8, 4u8, 5u8];
                    let k: u8 := a[0];
                    return k * 3u8;
                }",
                Ok(Type::U8),
            ),
            (
                "fn main() -> u16 {
                    let a: [u16; 5] := [1u16, 2u16, 3u16, 4u16, 5u16];
                    let k: u16 := a[0];
                    return k * 3u16;
                }",
                Ok(Type::U16),
            ),
            (
                "fn main() -> i16 {
                    let a: [i16; 2] := [1, 2,];
                    let k: i16 := a[0];
                    return k * 3i16;
                }",
                Err("Semantic: L2: Bind expected [i16; 2] but got [i64; 2]"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[1];
                    assert_eq!(bind_stm.annotation().ty, expected_ty);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[2];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_array_at_index() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn main() -> i64 {
                    let a: [i64; 5] := [1, 2, 3, 4, 5];
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let a: [i64; 5] := [[1, 2, 3, 4, 5]][0];
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let a: [[i64; 1]; 1] := [[1]];
                    let k: i64 := a[0][0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i8 {
                    let a: [i8; 5] := [1i8, 2i8, 3i8, 4i8, 5i8];
                    let k: i8 := a[0];
                    return k * 3i8;
                }",
                Ok(Type::I8),
            ),
            (
                line!(),
                "fn main() -> u8 {
                    let a: [u8; 5] := [1u8, 2u8, 3u8, 4u8, 5u8];
                    let k: u8 := a[0i8];
                    return k * 3u8;
                }",
                Ok(Type::U8),
            ),
            (
                line!(),
                "fn main() -> bool {
                    let a: [i64; 5] := [1, 2, 3, 4, 5];
                    let k: i64 := a[false];
                    return k * 3;
                }",
                Err("Semantic: L3: Expected integral type for index but found bool"),
            ),
            (
                line!(),
                "fn main() -> bool {
                    let a: i64 := 1;
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Err("Semantic: L3: Expected array type on LHS of [] but found i64"),
            ),
        ] {
            println!("Test L{}", line);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[1];
                    assert_eq!(bind_stm.annotation().ty, expected_ty);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[2];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_bind_statement() {
        for (ln, text, expected) in vec![
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 5i64;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 5i64;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> [i64;5] {
                    let k: [i64;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Ok(Type::Array(Box::new(Type::I64), 5)),
            ),
            (
                line!(),
                "fn main() -> [i32;5] {
                    let k: [i64;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L3: Return expected [i32; 5] but got [i64; 5]"),
            ),
            (
                line!(),
                "fn main() -> [i32;0] {
                    let k: [i64;0] := [];
                    return k;
                }",
                Err("Semantic: L1: Expected length > 0 for array, but found 0"),
            ),
            (
                line!(),
                "fn main() -> [i32;1] {
                    [];
                    let k: [i64;1] := [1];
                    return k;
                }",
                Err("Semantic: L2: Arrays with 0 length are not allowed"),
            ),
            (
                line!(),
                "fn main() -> [i32;5] {
                    let k: [i32;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L2: Bind expected [i32; 5] but got [i64; 5]"),
            ),
            (
                line!(),
                "fn main() -> [i64;5] {
                    let k: [i64;5] := [1, 2i32, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L2: Inconsistent types in array value"),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i32 := 5;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i32 but got i64"),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 5i32;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i64 but got i32"),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: bool := 5;
                    return k;
                }",
                Err("Semantic: L2: Bind expected bool but got i64"),
            ),
            (
                line!(),
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i64"),
            ),
            (
                line!(),
                // Test recursive definition
                "fn main() -> bool {
                    let k: bool := k;
                    return k;
                }",
                Err("Semantic: L2: k is not defined"),
            ),
            (
                line!(),
                "fn main() -> bool {
                    let k: bool := x;
                    return k;
                }",
                Err("Semantic: L2: x is not defined"),
            ),
        ] {
            println!("Test L{}", ln);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty, "L{}", ln);
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the return statement is typed correctly
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mutate_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let mut k: i64 := 5;
                    mut k := 3;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i64 {
                    let mut k: i64 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: k is of type i64 but is assigned bool"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut k := 3;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut x := false;
                    return k;
                }",
                Err("Semantic: L3: x is not defined"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the mutate statement is typed correctly
                    let mut_stm = &fn_main.get_body()[1];
                    assert_eq!(mut_stm.get_type(), Type::I64);
                    if let Statement::Mutate(box m) = mut_stm {
                        assert_eq!(m.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_return_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    return 5;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> bool {
                    return false;
                }",
                Ok(Type::Bool),
            ),
            (
                "fn main() -> string {
                    return \"hello\";
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() {
                    return;
                }",
                Ok(Type::Unit),
            ),
            (
                "fn main() -> bool {
                    return 5;
                }",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
            (
                "fn main() {
                    return 5;
                }",
                Err("Semantic: L2: Return expected unit but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Type::Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_extern_calls() {
        for (text, expected) in vec![
            (
                "
                extern fn number() -> i64;
                fn main() -> i64 {
                    return number();
                }
                ",
                Ok(Type::I64),
            ),
            (
                "
                extern fn number() -> i32;
                fn main() -> i32 {
                    return number();
                }
                ",
                Ok(Type::I32),
            ),
            (
                "
                extern fn number(i: i64, ...) -> i32;
                fn main() -> i32 {
                    return number(5, 10, 15i32, 8u8, \"hello\");
                }
                ",
                Ok(Type::I32),
            ),
            (
                "
                extern fn number(i: i64, ...) -> i32;
                fn main() -> i32 {
                    return number(5i32, 10, 15i32, 8u8, \"hello\");
                }
                ",
                Err("Semantic: L4: One or more parameters have mismatching types for function $main::number: parameter 1 expected i64 but got i32"),
            ),
            (
                "
                extern fn number(i: i64, ...) -> i32;
                fn main() -> i32 {
                    return number();
                }
                ",
                Err("Semantic: L4: Function $main::number expects at least 1 parameters, but got 0"),
            ),
            (
                "
                extern fn number(i: i64, j: i32, ...) -> i32;
                fn main() -> i32 {
                    return number(5);
                }
                ",
                Err("Semantic: L4: Function $main::number expects at least 2 parameters, but got 1"),
            ),
            (
                "fn main() -> bool {
                    return number();
                }
                extern fn number() -> i64;
                ",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    println!("{}", text);
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Type::Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_calls() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    return number();
                }
                fn number() -> i64 {return 5;}
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return 5i32;}
                ",
                Ok(Type::I32),
            ),
            (
                // test recursion
                "fn main() -> i64 {
                    return number();
                }
                fn number() -> i64 {return number();}
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return 5;}
                ",
                Err("Semantic: L4: Return expected i32 but got i64"),
            ),
            (
                "fn main() -> bool {
                    return number();
                }
                fn number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
            (
                "fn main() -> bool {
                    return bad_fun();
                }
                fn number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: bad_fun is not defined"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, 2);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return add(1i32, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i32 {
                    return add(1, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function $main::add: parameter 1 expected i32 but got i64"),
            ),
            (
                "fn main() -> i64 {
                    return add(1i32, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: Return expected i64 but got i32"),
            ),
            (
                "fn main() -> i64 {
                    return add(false, 2);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function $main::add: parameter 1 expected i64 but got bool"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, true);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function $main::add: parameter 2 expected i64 but got bool"),
            ),
            (
                "fn main() -> i64 {
                    return add(1);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::add. Expected 2 but got 1"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, 2, 3);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::add. Expected 2 but got 3"),
            ),
            (
                "fn main() -> i64 {
                    return add(false);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::add. Expected 2 but got 1"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE,&tokens).unwrap().unwrap();
            let module = resolve_types(&ast, "my_main", TracingConfig::Off,
                TracingConfig::Off,
             TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r.get_value().clone().map(|v| v.get_type().clone()).unwrap_or(Type::Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_init() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {return 5;}
                ",
                Ok(Type::Coroutine(Box::new(Type::I64))),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(3);
                    return;
                }
                co number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::number. Expected 0 but got 1"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5);
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Ok(Type::Coroutine(Box::new(Type::I64))),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::number. Expected 1 but got 0"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5, 3);
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: $main::number. Expected 1 but got 2"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5);
                    return;
                }
                fn number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Expected coroutine but $main::number is a fn (i64) -> i64"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5i32);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Ok(Type::Coroutine(Box::new(Type::I32))),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function $main::number: parameter 1 expected i32 but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE,&tokens).unwrap().unwrap();
            let module = resolve_types(&ast, "my_main", TracingConfig::Off,
                TracingConfig::Off,
             TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement, got {:?}", bind_stm);
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_return_statement() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret false;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i64 but got bool"),
            ),
            (
                line!(),
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i64 but got unit"),
            ),
            /*
                Need to add a symbol for the unit type
            (
                line!(),
                "fn main() {
                    let c: co := init number();
                    return;
                }
                co number() {
                    yret 5;
                    return;
                }
                ",
                Err("Semantic: L6: Yield return expected unit but got i64"),
            ),*/
        ] {
            println!("Test L{}", line);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_coroutines()[0].to_routine().unwrap();

                    let yret_stm = &co_number.get_body()[0];
                    assert_eq!(yret_stm.get_type(), Type::I64);

                    // validate that the RHS of the yield return is the correct type
                    if let Statement::YieldReturn(box yr) = yret_stm {
                        match yr.get_value() {
                            None => panic!("Expected a value"),
                            Some(v) => assert_eq!(v.get_type(), expected_ty),
                        }
                    } else {
                        panic!("Expected a bind statement");
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_statement() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i64 := init number();
                    let i: i64 := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() {
                    let c: bool := false;
                    let i: i64 := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Yield expects co<_> but got bool"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    let i: bool := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Bind expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &co_number.get_body()[1];
                    assert_eq!(bind_stm.get_type(), Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement, got {:?}", bind_stm);
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_definition() {
        for (text, expected) in vec![
            (
                "fn main(i: i64) -> i64 {
                    return i;
                }
                ",
                Ok(Type::I64),
            ),
            (
                "fn main(b: bool) -> i64 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got bool"),
            ),
            (
                "fn main(b: bool) -> i64 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got unit"),
            ),
            (
                "fn main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Type::Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_definition() {
        for (text, expected) in vec![
            (
                "co main(i: i64) -> i64 {
                    return i;
                }
                ",
                Ok(Type::I64),
            ),
            (
                "co main(b: bool) -> i64 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got bool"),
            ),
            (
                "co main(b: bool) -> i64 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got unit"),
            ),
            (
                "co main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_main = module.get_coroutines()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &co_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Type::Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_if_expressions() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let x: i64 := if (true) {1} else {2};
                    return;
                }
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() {
                    let x: i64 := if (4) {1} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: Expected boolean expression in if conditional, got: i64"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {true} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected bool got i64"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {5} else {\"hello\"};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected i64 got string"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {\"true\"} else {\"false\"};
                    return;
                }
                ",
                Err("Semantic: L2: Bind expected i64 but got string"),
            ),
            (
                "fn main() {
                    if (false) {1};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected i64 got unit"),
            ),
            (
                "fn main() {
                    if (false) {};
                    return;
                }
                ",
                Ok(Type::Unit),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE, &tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // Check the return value
                    if expected_ty != Type::Unit {
                        if let Statement::Bind(box b) = bind_stm {
                            let rhs_ty = b.get_rhs().get_type();
                            assert_eq!(rhs_ty, expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_while_expressions() {
        for (text, expected) in vec![
            (
                "fn main() {
                    while (true) {1;};
                    return;
                }
                ",
                Ok(Type::Unit),
            ),
            (
                "fn main() {
                    return while (true) {1;};
                }
                ",
                Ok(Type::Unit),
            ),
            (
                "fn main() {
                    while (5) {1;};
                    return;
                }
                ",
                Err("Semantic: L2: The condition of a while expression must resolve to a unit type, but got: i64"),
            ),
            (
                "fn main() {
                    while (true) {1};
                    return;
                }
                ",
                Err("Semantic: L2: The body of a while expression must resolve to a unit type, but got: i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE,&tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                "my_main",
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // Check the return value
                    if expected_ty != Type::Unit {
                        if let Statement::Bind(box b) = bind_stm {
                            let rhs_ty = b.get_rhs().get_type();
                            assert_eq!(rhs_ty, expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_struct_expression() {
        for (line, text, expected) in vec![
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> root::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> self::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return self::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return root::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := {self::MyStruct{x: 1}};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i64}}
                fn test() -> my_mod::MyStruct 
                {
                    let x: root::my_mod::MyStruct := self::my_mod::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i64}}
                mod fn_mod {
                    fn test() -> self::super::my_mod::MyStruct 
                    {
                        let x: root::my_mod::MyStruct := super::my_mod::MyStruct{x: 1};
                        return x;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{ms: MyStruct}
                fn test() -> MyStruct2
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: root::MyStruct2 := self::MyStruct2{ ms: x};
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test2(ms: MyStruct) -> i64 {return ms.x;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i64 := test2(x);
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test2(ms: MyStruct) -> MyStruct {return ms;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: MyStruct := test2(x);
                    return y.x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                co test2(ms: MyStruct) -> MyStruct { 
                    yret ms; 
                    return ms;
                }
                fn test() -> root::MyStruct
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: co self::MyStruct := init test2(x);
                    let z: MyStruct := yield (y);
                    return z;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{ms: MyStruct}
                fn test2(ms2: MyStruct2) -> i64 {return ms2.ms.x;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i64 := test2(x);
                    return y;
                }",
                Err("Semantic: L7: One or more parameters have mismatching types for function $main::test2: parameter 1 expected $main::MyStruct2 but got $main::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct2 := self::MyStruct{x: 1};
                    return x;
                }",
                Err("Semantic: L5: Bind expected $main::MyStruct2 but got $main::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{x:false};}",
                Err("Semantic: L1: $main::MyStruct.x expects i64 but got bool"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{};}",
                Err("Semantic: L1: expected 1 parameters but found 0"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> i64 {return MyStruct{x:5};}",
                Err("Semantic: L1: Return expected i64 but got $main::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:co i64} fn test(c: co i64) -> MyStruct {return MyStruct{x: c};}",
                Ok(()),
            ),
        ] {
            println!("L{}", line);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(MAIN_MODULE,&tokens).unwrap().unwrap();
            let result = resolve_types(&ast, "my_main", TracingConfig::Off,
                TracingConfig::Off,
             TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(_) => {assert!(result.is_ok(), "\nL{}: {} => {:?}", line, text, result)},
                Err(msg) => assert_eq!(result.err().unwrap(), msg),
            }
        }
    }

    #[test]
    pub fn test_member_access() {
        for (text, expected) in vec![
                ("struct MyStruct{x:i64}
                fn test(ms:MyStruct) -> i64 {
                    return ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i64} fn test(ms:MyStruct) -> i64 {return ms.y;}",
                Err("Semantic: L1: $main::MyStruct does not have member y")),
                ("struct MyStruct{x:i64} fn test(ms:MyStruct) -> bool{return ms.x;}",
                Err("Semantic: L1: Return expected bool but got i64")),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i64 {return ms.ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> MyStruct {return ms.ms;}",
                Ok(())),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i64 {return ms.ms.y;}",
                Err("Semantic: L1: $main::MyStruct does not have member y")),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> bool {return ms.ms.x;}",
                Err("Semantic: L1: Return expected bool but got i64")),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_>>()
                    .unwrap();
                let ast = parser::parse(MAIN_MODULE,&tokens).unwrap().unwrap();
                let result = resolve_types(
                    &ast, 
                    "my_main", 
                    TracingConfig::Off,
                    TracingConfig::Off,
                    TracingConfig::Off, 
                    TracingConfig::Off
                );
                match expected {
                    Ok(_) => assert!(result.is_ok(), "{} -> {:?}", text, result),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
    }

    #[test]
    pub fn test_imported_functions() {
        for (line, text, import_func, expected) in vec![
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test();
                    return;
                }
                ",
                (vec![],(Type::I64)),
                Ok(()),
            ),
            (
                line!(),
                " 
                fn main() {
                    return project::std::test();
                }
                ",
                (vec![],(Type::Unit)),
                Ok(()),
            ),
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test(5);
                    return;
                }
                ",
                (vec![Type::I64], (Type::I64)),
                Ok(()),
            ),
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test(5, true);
                    return;
                }
                ",
                (vec![Type::I64, Type::Bool], (Type::I64)),
                Ok(()),
            ),
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test2();
                    return;
                }
                ",
                (vec![], (Type::I64)),
                Err("Semantic: L3: Could not find item with the given path: $std::test2 ($std::test2)"),
            ),
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test(5);
                    return;
                }
                ",
                (vec![], (Type::I64)),
                Err("Semantic: L3: Incorrect number of parameters passed to routine: $std::test. Expected 0 but got 1"),
            ),
            (
                line!(),
                " 
                fn main() {
                    let k: i64 := project::std::test(5, 2);
                    return;
                }
                ",
                (vec![Type::I64, Type::Bool], (Type::I64)),
                Err("Semantic: L3: One or more parameters have mismatching types for function $std::test: parameter 2 expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse("std",&tokens).unwrap().unwrap();

            let mut import_annotation = SemanticContext::new(0, 0, Type::Unit);
            import_annotation.set_canonical_path(vec![CANONICAL_ROOT, "std", "test"].into());
            let imports = Manifest::new(&vec![RoutineDef{
                context: import_annotation,
                def: RoutineDefType::Function,
                name: "test".into(),
                ret_ty: import_func.1.clone(),
                params: import_func.0.iter().map(|p| Parameter::new(SemanticContext::new(0, 0, p.clone()), "a", p)).collect(),
                body: vec![],
            }], &vec![]);
            let result = resolve_types_with_imports(
                &ast, 
                "my_main", 
                &vec![imports], 
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off, 
                TracingConfig::Off
            );
            match expected {
                Ok(_) => assert!(result.is_ok(), "L{}: {:?} got {:?}", line, expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }
}
