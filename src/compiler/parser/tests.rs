#[cfg(test)]
pub mod tests {
    use core::panic;

    use super::super::parser::*;
    use crate::compiler::{
        ast::*,
        lexer::tokens::Token,
        parser::{expression::*, statement::*, tokenstream::TokenStream},
        Lexer,
    };
    use braid_lang::result::Result;

    #[test]
    fn parse_unary_operators() {
        for (text, expected) in
            vec![("-a", UnaryOperator::Negate), ("!a", UnaryOperator::Not)].iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*operand, Expression::Identifier(1, "a".into()));
            } else {
                panic!("No nodes returned by parser for {:?} => {:?}", text, exp)
            }
        }
    }

    #[test]
    fn parse_double_unary_operators() {
        for (text, expected) in
            vec![("--a", UnaryOperator::Negate), ("!!a", UnaryOperator::Not)].iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(l, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                if let Expression::UnaryOp(l, op, operand) = *operand {
                    assert_eq!(op, *expected);
                    assert_eq!(l, 1);
                    assert_eq!(*operand, Expression::Identifier(1, "a".into()));
                }
            } else {
                panic!("No nodes returned by parser for {:?} => {:?}", text, exp)
            }
        }
    }

    #[test]
    fn parse_arithmetic_expressions() {
        for (text, expected) in vec![
            ("2+2", BinaryOperator::Add),
            ("2-2", BinaryOperator::Sub),
            ("2*2", BinaryOperator::Mul),
            ("2/2", BinaryOperator::Div),
            ("2==2", BinaryOperator::Eq),
            ("2!=2", BinaryOperator::NEq),
            ("2<2", BinaryOperator::Ls),
            ("2<=2", BinaryOperator::LsEq),
            ("2>2", BinaryOperator::Gr),
            ("2>=2", BinaryOperator::GrEq),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap()
            {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Expression::I64(1, 2));
                assert_eq!(*right, Expression::I64(1, 2));
            } else {
                panic!("No nodes returned by parser for {}", text)
            }
        }
    }

    #[test]
    fn parse_boolean_expresions() {
        for (text, expected) in vec![
            ("true && false", BinaryOperator::BAnd),
            ("true || false", BinaryOperator::BOr),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(l, op, left, right)) = expression(&mut stream).unwrap()
            {
                assert_eq!(op, *expected);
                assert_eq!(l, 1);
                assert_eq!(*left, Expression::Boolean(1, true));
                assert_eq!(*right, Expression::Boolean(1, false));
            } else {
                panic!("No nodes returned by parser")
            }
        }
    }

    #[test]
    fn parse_nested_arithmetic_expression() {
        let text = "(2 + 4) * 3";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(l, BinaryOperator::Mul, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            match left.as_ref() {
                Expression::BinaryOp(_, BinaryOperator::Add, ll, lr) => {
                    assert_eq!(**ll, Expression::I64(1, 2));
                    assert_eq!(**lr, Expression::I64(1, 4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(*right, Expression::I64(1, 3));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_boolean_expression() {
        let text = "true || false";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(l, BinaryOperator::BOr, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(*left, Expression::Boolean(1, true));
            assert_eq!(*right, Expression::Boolean(1, false));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_path() {
        for (text, expected) in vec![
            ("thing", Ok(vec!["thing"])),
            ("::thing", Ok(vec![ROOT_PATH, "thing"])),
            ("thing::first", Ok(vec!["thing", "first"])),
            ("thing::first::second", Ok(vec!["thing", "first", "second"])),
            (
                "thing::",
                Err("L1: expect identifier after path separator '::'"),
            ),
            (
                "thing::first::",
                Err("L1: expect identifier after path separator '::'"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::Path(l, path))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) => assert_eq!(path, expected.into()),
                        Err(msg) => assert!(false, "{}", msg),
                    }
                }
                Ok(Some(Expression::Identifier(l, id))) => {
                    assert_eq!(l, 1);
                    match expected {
                        Ok(expected) => {
                            assert_eq!(expected.len(), 1);
                            assert_eq!(id, expected[0]);
                        }
                        Err(msg) => assert!(false, "{}", msg),
                    }
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => match expected {
                    Ok(_) => assert!(false, "{}", msg),
                    Err(expected) => assert_eq!(expected, msg),
                },
            }
        }
    }

    #[test]
    fn parse_multiple_member_access() {
        for text in vec![
            "thing.first.second",
            "(thing).first.second",
            "(thing.first).second",
            "((thing.first).second)",
            "(thing.first.second)",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::MemberAccess(l, left, right))) => {
                    assert_eq!(l, 1);
                    assert_eq!(
                        *left,
                        Expression::MemberAccess(
                            1,
                            Box::new(Expression::Identifier(1, "thing".into())),
                            "first".into()
                        ),
                        "Input: {}",
                        text,
                    );
                    assert_eq!(right, "second");
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {}", text, msg),
            }
        }
    }

    #[test]
    fn parse_bind() {
        let text = "let x:i64 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), false);
                assert_eq!(*b.get_rhs(), Expression::I64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_mut_bind() {
        let text = "let mut x:i64 := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), true);
                assert_eq!(*b.get_rhs(), Expression::I64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_primitives() {
        for (text, expected_ty) in vec![
            ("let x:u8 := 5u8;", Type::U8),
            ("let x:u16 := 5u16;", Type::U16),
            ("let x:u32 := 5u32;", Type::U32),
            ("let x:u64 := 5u64;", Type::U64),
            ("let x:i8 := 5;", Type::I8),
            ("let x:i8 := 5i8;", Type::I8),
            ("let x:i16 := 5;", Type::I16),
            ("let x:i16 := 5i16;", Type::I16),
            ("let x:i32 := 5;", Type::I32),
            ("let x:i32 := 5i32;", Type::I32),
            ("let x:i64 := 5;", Type::I64),
            ("let x: bool := true;", Type::Bool),
            ("let x: string := \"hello\";", Type::StringLiteral),
            (
                "let x: [i32;5] := [1, 2, 3, 4, 5];",
                Type::Array(Box::new(Type::I32), 5),
            ),
        ]
        .iter()
        {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);

            let stm = statement(&mut stream);
            assert!(stm.is_ok(), "{}", text);

            match stm.unwrap().unwrap() {
                Statement::Bind(box b) => {
                    assert_eq!(b.get_id(), "x", "{}", text);
                    assert_eq!(b.get_type(), expected_ty, "{}", text);
                    assert_eq!(b.is_mutable(), false, "{}", text);
                }
                _ => panic!("Not a binding statement"),
            }
        }
    }

    #[test]
    fn parse_mutation() {
        let text = "mut x := 5;";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Mutate(box m) => {
                assert_eq!(m.get_id(), "x");
                assert_eq!(*m.get_rhs(), Expression::I64(1, 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_module_empty() {
        let text = "mod test_mod {}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test", &tokens)
            .unwrap()
            .unwrap()
            .get_module("test_mod")
        {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_mod");
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_function() {
        let text = "mod test_fn_mod { fn test(x:i64) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();

        if let Some(m) = parse("test", &tokens).unwrap() {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test");

            assert_eq!(m.get_modules().len(), 1);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);

            let m = &m.get_modules()[0];
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_fn_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 1);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            if let Item::Routine(RoutineDef {
                context,
                def: RoutineDefType::Function,
                name,
                params,
                ret_ty: ty,
                body,
                ..
            }) = &m.get_functions()[0]
            {
                assert_eq!(*context, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                    _ => panic!("Wrong body, expected unit return"),
                }
            } else {
                panic!("Expected function definition")
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_coroutine() {
        let text = "mod test_co_mod { co test(x:i64) {return;} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test", &tokens)
            .unwrap()
            .unwrap()
            .get_module("test_co_mod")
        {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_co_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 1);
            assert_eq!(m.get_structs().len(), 0);

            if let Some(Item::Routine(RoutineDef {
                context,
                def: RoutineDefType::Coroutine,
                name,
                params,
                ret_ty: ty,
                body,
                ..
            })) = m.get_item("test")
            {
                assert_eq!(*context, 1);
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                    _ => panic!("Wrong body, expected unit return"),
                }
            } else {
                panic!("Expected coroutine definition")
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_struct() {
        let text = "mod test_struct_mod { struct my_struct{x: i64} }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test", &tokens)
            .unwrap()
            .unwrap()
            .get_module("test_struct_mod")
        {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_struct_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 1);

            if let Some(Item::Struct(sd)) = m.get_item("my_struct") {
                assert_eq!(*sd.get_context(), 1);
                assert_eq!(sd.get_name(), "my_struct");
                assert_eq!(sd.get_fields(), &vec![Parameter::new(1, "x", &Type::I64)]);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_extern() {
        let text = "mod test_extern_mod { extern fn my_fn(x: i64) -> i32; }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test", &tokens)
            .unwrap()
            .unwrap()
            .get_module("test_extern_mod")
        {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_extern_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item("my_fn") {
                assert_eq!(*e.get_context(), 1);
                assert_eq!(e.get_name(), "my_fn");
                assert_eq!(e.get_params(), &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(e.get_return_type(), Type::I32);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_extern_with_varargs() {
        let text = "mod test_extern_mod { extern fn my_fn(x: i64, ...) -> i32; }";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test", &tokens)
            .unwrap()
            .unwrap()
            .get_module("test_extern_mod")
        {
            assert_eq!(*m.get_context(), 1);
            assert_eq!(m.get_name(), "test_extern_mod");

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item("my_fn") {
                assert_eq!(*e.get_context(), 1);
                assert_eq!(e.get_name(), "my_fn");
                assert_eq!(e.get_params(), &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(e.get_return_type(), Type::I32);
                assert_eq!(e.has_varargs, true);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_unit_function_def() {
        let text = "fn test(x:i64) {return;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse("test", &tokens).unwrap().unwrap().get_item("test")
        {
            assert_eq!(*l, 1);
            assert_eq!(name, "test");
            assert_eq!(*params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Unit);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => assert_eq!(*r.get_value(), None),
                _ => panic!("Wrong body, expected unit return"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_function_def() {
        let text = "fn test(x:i64) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse("test", &tokens).unwrap().unwrap().get_item("test")
        {
            assert_eq!(*l, 1);
            assert_eq!(name, "test");
            assert_eq!(*params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(*r.get_value(), Some(Expression::Boolean(1, true)));
                }
                _ => panic!("No body"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_missing_fn_token() {
        // This tests that the parser will terminate if it reaches a point
        // where it cannot advance while parsing a set of items.
        let text = "
        struct Test {
        }

        get_data() {
            return;
        }
        ";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        parse("test", &tokens).expect_err("This should fail");
    }

    #[test]
    fn parse_routine_call() {
        let text = "test(x, y)";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec!["test"].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into())
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_routine_by_path_call() {
        let text = "self::test(x, y)";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, 1);
            assert_eq!(name, vec![SELF, "test"].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(1, "x".into()),
                    Expression::Identifier(1, "y".into())
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_def() {
        let text = "co test(x:i64) -> bool {return true;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(m) = parse("test_mod", &tokens).unwrap() {
            assert_eq!(*m.get_context(), 1);
            if let Some(Item::Routine(RoutineDef {
                def: RoutineDefType::Coroutine,
                name,
                params,
                ret_ty: ty,
                body,
                ..
            })) = m.get_item("test")
            {
                assert_eq!(name, "test");
                assert_eq!(params, &vec![Parameter::new(1, "x", &Type::I64)]);
                assert_eq!(ty, &Type::Bool);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => {
                        assert_eq!(*r.get_value(), Some(Expression::Boolean(1, true)));
                    }
                    _ => panic!("No body"),
                }
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_init() {
        let text = "let x:co i64 := init c(1, 2);";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["c"].into(),
                        vec![Expression::I64(1, 1), Expression::I64(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_coroutine_path_init() {
        let text = "let x:co i64 := init a::b::c(1, 2);";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), "x");
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        1,
                        RoutineCall::CoroutineInit,
                        vec!["a", "b", "c"].into(),
                        vec![Expression::I64(1, 1), Expression::I64(1, 2)]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_yield() {
        let text = "fn test(x:i64) -> bool {return yield cor;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse("test", &tokens).unwrap().unwrap().get_item("test")
        {
            assert_eq!(*l, 1);
            assert_eq!(name, "test");
            assert_eq!(*params, vec![Parameter::new(1, "x", &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(
                        *r.get_value(),
                        Some(Expression::Yield(
                            1,
                            Box::new(Expression::Identifier(1, "cor".into()))
                        ))
                    );
                }
                _ => panic!("No body"),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_if_expression() {
        let text = "if (x) {5} else {7}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::If {
            context: l,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::I64(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm {
                assert_eq!(*final_exp, Expression::I64(1, 7));
            } else {
                panic!("Expected Expression block");
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_if_else_if_expression() {
        let text = "if (x) {5} else if (y && z) {7} else {8}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::If {
            context: l,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::I64(1, 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(box Expression::If {
                cond,
                if_arm,
                else_arm,
                ..
            }) = else_arm
            {
                assert_eq!(
                    *cond,
                    Expression::BinaryOp(
                        1,
                        BinaryOperator::BAnd,
                        Box::new(Expression::Identifier(1, "y".into())),
                        Box::new(Expression::Identifier(1, "z".into()))
                    )
                );
                if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                    assert_eq!(*final_exp, Expression::I64(1, 7));
                } else {
                    panic!("Expected Expression block");
                }

                if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm
                {
                    assert_eq!(*final_exp, Expression::I64(1, 8));
                } else {
                    panic!("Expected Expression block");
                }
            } else {
                panic!("Expected if statement in else arm");
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_while_expression() {
        let text = "while (x) {5;}";
        let tokens: Vec<Token> = Lexer::new(&text)
            .tokenize()
            .into_iter()
            .collect::<Result<_>>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::While {
            context: l,
            cond,
            body,
        }) = exp
        {
            assert_eq!(l, 1);
            assert_eq!(*cond, Expression::Identifier(1, "x".into()));
            if let Expression::ExpressionBlock(_l, body, None) = *body {
                assert_eq!(body[0], Statement::Expression(box Expression::I64(1, 5)));
            } else {
                panic!("Expected Expression block, got {:?}", *body);
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_struct_def() {
        for (text, expected) in vec![
            ("struct MyStruct {}", StructDef::new("MyStruct", 1, vec![])),
            (
                "struct MyStruct {x: i64}",
                StructDef::new("MyStruct", 1, vec![Parameter::new(1, "x", &Type::I64)]),
            ),
            (
                "struct MyStruct {x: i64, y: bool}",
                StructDef::new(
                    "MyStruct",
                    1,
                    vec![
                        Parameter::new(1, "x", &Type::I64),
                        Parameter::new(1, "y", &Type::Bool),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            if let Some(m) = parse("test", &tokens).unwrap() {
                assert_eq!(m.get_structs()[0], Item::Struct(expected), "{:?}", text);
            }
        }
    }

    #[test]
    fn parse_struct_init() {
        for (text, expected) in vec![
            (
                "MyStruct{}",
                Expression::StructExpression(1, vec!["MyStruct"].into(), vec![]),
            ),
            (
                "MyStruct{x: 5}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![("x".into(), Expression::I64(1, 5))],
                ),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![
                        ("x".into(), Expression::I64(1, 5)),
                        ("y".into(), Expression::Boolean(1, false)),
                    ],
                ),
            ),
            (
                "MyStruct{x: 5, y: MyStruct2{z:3}}",
                Expression::StructExpression(
                    1,
                    vec!["MyStruct"].into(),
                    vec![
                        ("x".into(), Expression::I64(1, 5)),
                        (
                            "y".into(),
                            Expression::StructExpression(
                                1,
                                vec!["MyStruct2"].into(),
                                vec![("z".into(), Expression::I64(1, 3))],
                            ),
                        ),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let result = expression(&mut stream);
            assert_eq!(result, Ok(Some(expected)), "{:?}", text);
        }
    }

    #[test]
    fn parse_string_literals() {
        for (text, expected) in vec![
            ("fn test() -> String {return \"test\";}", "test"),
            ("fn test() -> String {return \"test 2\";}", "test 2"),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let module = parse("test_mod", &tokens).unwrap();
            match module {
                Some(m) => match &m.get_functions()[0] {
                    Item::Routine(RoutineDef { body, .. }) => match &body[0] {
                        Statement::Return(box r) => assert_eq!(
                            *r.get_value(),
                            Some(Expression::StringLiteral(1, expected.into()))
                        ),
                        _ => assert!(false, "Not a return statement"),
                    },
                    _ => assert!(false, "Not a return statement"),
                },
                _ => assert!(false, "Not a routine, got {:?}", module),
            }
        }
    }
}
