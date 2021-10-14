#[cfg(test)]
pub mod tests {
    use core::panic;

    use super::super::parser::*;
    use crate::{
        compiler::{
            ast::*,
            lexer::{tokens::Token, LexerError},
            parser::{
                expression::*, statement::*, tokenstream::TokenStream, ParserContext, ParserError,
            },
            source::Offset,
            CompilerDisplay, CompilerError, Lexer, Span,
        },
        StringTable,
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn new_span(low: u32, high: u32) -> Span {
        Span::new(Offset::new(low), Offset::new(high))
    }

    fn new_ctx(low: u32, high: u32) -> ParserContext {
        ParserContext::new(1, new_span(low, high))
    }

    #[test]
    fn parse_unary_operators() {
        for (text, expected) in
            vec![("-a", UnaryOperator::Negate), ("!a", UnaryOperator::Not)].iter()
        {
            let mut table = StringTable::new();
            let a = table.insert("a".into());
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(ctx, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(ctx, new_ctx(0, 2));
                assert_eq!(*operand, Expression::Identifier(new_ctx(1, 2), a));
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
            let mut table = StringTable::new();
            let a = table.insert("a".into());

            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let exp = expression(&mut stream).unwrap();
            if let Some(Expression::UnaryOp(ctx, op, operand)) = exp {
                assert_eq!(op, *expected);
                assert_eq!(ctx, new_ctx(0, 3));
                if let Expression::UnaryOp(ctx, op, operand) = *operand {
                    assert_eq!(op, *expected);
                    assert_eq!(ctx, new_ctx(1, 3));
                    assert_eq!(*operand, Expression::Identifier(new_ctx(2, 3), a));
                }
            } else {
                panic!("No nodes returned by parser for {:?} => {:?}", text, exp)
            }
        }
    }

    #[test]
    fn parse_arithmetic_expressions() {
        for (text, expected, l, h) in vec![
            ("2+2", BinaryOperator::Add, 0, 3),
            ("2-2", BinaryOperator::Sub, 0, 3),
            ("2*2", BinaryOperator::Mul, 0, 3),
            ("2/2", BinaryOperator::Div, 0, 3),
            ("2==2", BinaryOperator::Eq, 0, 4),
            ("2!=2", BinaryOperator::NEq, 0, 4),
            ("2<2", BinaryOperator::Ls, 0, 3),
            ("2<=2", BinaryOperator::LsEq, 0, 4),
            ("2>2", BinaryOperator::Gr, 0, 3),
            ("2>=2", BinaryOperator::GrEq, 0, 4),
        ] {
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(ctx, op, left, right)) =
                expression(&mut stream).unwrap()
            {
                assert_eq!(op, expected);
                assert_eq!(ctx, new_ctx(l, h));
                assert_eq!(*left, Expression::I64(new_ctx(0, 1), 2));
                assert_eq!(
                    *right,
                    Expression::I64(new_ctx(text.len() as u32 - 1, text.len() as u32), 2)
                );
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
            let mut table = StringTable::new();
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            if let Some(Expression::BinaryOp(ctx, op, left, right)) =
                expression(&mut stream).unwrap()
            {
                assert_eq!(op, *expected);
                assert_eq!(ctx, new_ctx(0, 13));
                assert_eq!(*left, Expression::Boolean(new_ctx(0, 4), true));
                assert_eq!(*right, Expression::Boolean(new_ctx(8, 13), false));
            } else {
                panic!("No nodes returned by parser")
            }
        }
    }

    #[test]
    fn parse_nested_arithmetic_expression() {
        let text = "(2 + 4) * 3";
        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(ctx, BinaryOperator::Mul, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 11));
            match left {
                box Expression::BinaryOp(ctx, BinaryOperator::Add, ll, lr) => {
                    assert_eq!(ctx, new_ctx(0, 7));
                    assert_eq!(*ll, Expression::I64(new_ctx(1, 2), 2));
                    assert_eq!(*lr, Expression::I64(new_ctx(5, 6), 4));
                }
                _ => panic!("Expected Add syntax"),
            }
            assert_eq!(*right, Expression::I64(new_ctx(10, 11), 3));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_boolean_expression() {
        let text = "true || false";
        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        if let Some(Expression::BinaryOp(ctx, BinaryOperator::BOr, left, right)) =
            expression(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 13));
            assert_eq!(*left, Expression::Boolean(new_ctx(0, 4), true));
            assert_eq!(*right, Expression::Boolean(new_ctx(8, 13), false));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_path() {
        let mut table = StringTable::new();
        let thing = table.insert("thing".into());
        let first = table.insert("first".into());
        let second = table.insert("second".into());

        let mut test = 0;
        for (text, expected) in vec![
            ("thing", Ok(vec![Element::Id(thing)])),
            ("::thing", Ok(vec![Element::FileRoot, Element::Id(thing)])),
            (
                "thing::first",
                Ok(vec![Element::Id(thing), Element::Id(first)]),
            ),
            (
                "thing::first::second",
                Ok(vec![
                    Element::Id(thing),
                    Element::Id(first),
                    Element::Id(second),
                ]),
            ),
            (
                "thing::",
                Err(CompilerError::new(1, ParserError::PathExpectedIdentifier)),
            ),
            (
                "thing::first::",
                Err(CompilerError::new(1, ParserError::PathExpectedIdentifier)),
            ),
        ] {
            test += 1;
            println!("Test #{}", test);
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::Path(ctx, path))) => {
                    assert_eq!(ctx.line(), 1);
                    match expected {
                        Ok(expected) => assert_eq!(path, expected.into()),
                        Err(msg) => assert!(false, "{:?}", msg.fmt(&table)),
                    }
                }
                Ok(Some(Expression::Identifier(ctx, id))) => {
                    assert_eq!(ctx.line(), 1);
                    match expected {
                        Ok(expected) => {
                            assert_eq!(expected.len(), 1);
                            assert_eq!(Element::Id(id), expected[0]);
                        }
                        Err(msg) => assert!(false, "{:?}", msg),
                    }
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => match expected {
                    Ok(_) => assert!(false, "{}", msg.fmt(&table).unwrap()),
                    Err(expected) => assert_eq!(expected, msg),
                },
            }
        }
    }

    #[test]
    fn parse_multiple_member_access() {
        let mut table = StringTable::new();
        let thing = table.insert("thing".into());
        let first = table.insert("first".into());
        let second = table.insert("second".into());

        for (text, ma_l, ma_h, id_l, id_h) in vec![
            ("  thing .first.second", 2, 14, 2, 7),
            ("( thing).first.second", 0, 14, 0, 8),
            (" (thing .first).second", 1, 15, 2, 7),
            ("((thing .first).second)", 1, 15, 2, 7),
            ("( thing .first.second)", 2, 14, 2, 7),
        ] {
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            match expression(&mut stream) {
                Ok(Some(Expression::MemberAccess(ctx, left, right))) => {
                    assert_eq!(ctx.line(), 1); // TODO: This should test the span
                    assert_eq!(
                        *left,
                        Expression::MemberAccess(
                            new_ctx(ma_l, ma_h),
                            Box::new(Expression::Identifier(new_ctx(id_l, id_h), thing)),
                            first,
                        ),
                        "Input: {}",
                        text,
                    );
                    assert_eq!(right, second);
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {:?}", text, msg),
            }
        }
    }

    #[test]
    fn parse_bind() {
        let text = "let x:i64 := 5;";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), x);
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), false);
                assert_eq!(*b.get_rhs(), Expression::I64(new_ctx(13, 14), 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_mut_bind() {
        let text = "let mut x:i64 := 5;";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), x);
                assert_eq!(b.get_type(), Type::I64);
                assert_eq!(b.is_mutable(), true);
                assert_eq!(*b.get_rhs(), Expression::I64(new_ctx(17, 18), 5));
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
            let mut table = StringTable::new();
            let x = table.insert("x".into());

            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);

            let stm = statement(&mut stream);
            assert!(stm.is_ok(), "{}", text);

            match stm.unwrap().unwrap() {
                Statement::Bind(box b) => {
                    assert_eq!(b.get_id(), x, "{}", text);
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Mutate(box m) => {
                assert_eq!(m.get_id(), x);
                assert_eq!(*m.get_rhs(), Expression::I64(new_ctx(9, 10), 5));
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_module_empty() {
        let text = "mod test_mod {}";
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let test_mod = table.insert("test_mod".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test, &tokens).unwrap().unwrap().get_module(test_mod) {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_mod);
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_function() {
        let text = "mod test_fn_mod { fn test(x:i64) {return;} }";
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let test_fn_mod = table.insert("test_fn_mod".into());
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();

        if let Some(m) = parse(test, &tokens).unwrap() {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test);

            assert_eq!(m.get_modules().len(), 1);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);

            let m = &m.get_modules()[0];
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_fn_mod);

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
                assert_eq!(*context, new_ctx(21, 25));
                assert_eq!(*name, test);
                assert_eq!(
                    params,
                    &vec![Parameter::new(new_ctx(26, 27), x, &Type::I64)]
                );
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
        let mut table = StringTable::new();
        let test_co_mod = table.insert("test_co_mod".into());
        let test = table.insert("test".into());
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_co_mod)
        {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_co_mod);

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
            })) = m.get_item(test)
            {
                assert_eq!(*context, new_ctx(21, 25));
                assert_eq!(*name, test);
                assert_eq!(
                    params,
                    &vec![Parameter::new(new_ctx(26, 27), x, &Type::I64)]
                );
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
        let mut table = StringTable::new();
        let test_struct_mod = table.insert("test_struct_mod".into());
        let test = table.insert("test".into());
        let my_struct = table.insert("my_struct".into());
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_struct_mod)
        {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_struct_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 1);

            if let Some(Item::Struct(sd)) = m.get_item(my_struct) {
                assert_eq!(*sd.get_context(), new_ctx(22, 28));
                assert_eq!(sd.get_name(), my_struct);
                assert_eq!(
                    sd.get_fields(),
                    &vec![Parameter::new(new_ctx(39, 40), x, &Type::I64)]
                );
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_extern() {
        let text = "mod test_extern_mod { extern fn my_fn(x: i64) -> i32; }";
        let mut table = StringTable::new();
        let test_extern_mod = table.insert("test_extern_mod".into());
        let test = table.insert("test".into());
        let my_fn = table.insert("my_fn".into());
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_extern_mod)
        {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_extern_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item(my_fn) {
                assert_eq!(*e.get_context(), new_ctx(22, 28));
                assert_eq!(e.get_name(), my_fn);
                assert_eq!(
                    e.get_params(),
                    &vec![Parameter::new(new_ctx(38, 39), x, &Type::I64)]
                );
                assert_eq!(e.get_return_type(), Type::I32);
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_module_with_extern_with_varargs() {
        let text = "mod test_extern_mod { extern fn my_fn(x: i64, ...) -> i32; }";
        let mut table = StringTable::new();
        let test_extern_mod = table.insert("test_extern_mod".into());
        let test = table.insert("test".into());
        let my_fn = table.insert("my_fn".into());
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_extern_mod)
        {
            assert_eq!(*m.get_context(), new_ctx(0, 3));
            assert_eq!(m.get_name(), test_extern_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item(my_fn) {
                assert_eq!(*e.get_context(), new_ctx(22, 28));
                assert_eq!(e.get_name(), my_fn);
                assert_eq!(
                    e.get_params(),
                    &vec![Parameter::new(new_ctx(38, 39), x, &Type::I64)]
                );
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*l, new_ctx(3, 7));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 9), x, &Type::I64)]);
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*l, new_ctx(3, 7));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 9), x, &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(
                        *r.get_value(),
                        Some(Expression::Boolean(new_ctx(31, 35), true))
                    );
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
        let mut table = StringTable::new();
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        parse(test, &tokens).expect_err("This should fail");
    }

    #[test]
    fn parse_routine_call() {
        let text = "test(x, y)";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, new_ctx(0, 4));
            assert_eq!(name, vec![Element::Id(test)].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(new_ctx(5, 6), x),
                    Expression::Identifier(new_ctx(8, 9), y)
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_routine_by_path_call() {
        let text = "self::test(x, y)";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let test = table.insert("test".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        println!("{:?}", table);
        let mut iter = TokenStream::new(&tokens);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            expression(&mut iter).unwrap()
        {
            assert_eq!(l, new_ctx(0, 4));
            assert_eq!(name, vec![Element::Selph, Element::Id(test)].into());
            assert_eq!(
                params,
                vec![
                    Expression::Identifier(new_ctx(11, 12), x),
                    Expression::Identifier(new_ctx(14, 15), y)
                ]
            );
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_coroutine_def() {
        let text = "co test(x:i64) -> bool {return true;}";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let test = table.insert("test".into());
        let test_mod = table.insert("test_mod".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(m) = parse(test_mod, &tokens).unwrap() {
            assert_eq!(*m.get_context(), new_ctx(0, 2));
            if let Some(Item::Routine(RoutineDef {
                def: RoutineDefType::Coroutine,
                name,
                params,
                ret_ty: ty,
                body,
                ..
            })) = m.get_item(test)
            {
                assert_eq!(*name, test);
                assert_eq!(params, &vec![Parameter::new(new_ctx(8, 9), x, &Type::I64)]);
                assert_eq!(ty, &Type::Bool);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(box r) => {
                        assert_eq!(
                            *r.get_value(),
                            Some(Expression::Boolean(new_ctx(31, 35), true))
                        );
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let c = table.insert("c".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box b) => {
                assert_eq!(b.get_id(), x);
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        new_ctx(16, 20),
                        RoutineCall::CoroutineInit,
                        vec![Element::Id(c)].into(),
                        vec![
                            Expression::I64(new_ctx(23, 24), 1),
                            Expression::I64(new_ctx(26, 27), 2)
                        ]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_coroutine_path_init() {
        let text = "let x:co i64 := init a::b::c(1, 2);";

        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let a = table.insert("a".into());
        let b = table.insert("b".into());
        let c = table.insert("c".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let stm = statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(box bind) => {
                assert_eq!(bind.get_id(), x);
                assert_eq!(bind.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *bind.get_rhs(),
                    Expression::RoutineCall(
                        new_ctx(16, 20),
                        RoutineCall::CoroutineInit,
                        vec![Element::Id(a), Element::Id(b), Element::Id(c)].into(),
                        vec![
                            Expression::I64(new_ctx(29, 30), 1),
                            Expression::I64(new_ctx(32, 33), 2)
                        ]
                    )
                );
            }
            _ => panic!("Not a binding statement"),
        }
    }

    #[test]
    fn parse_yield() {
        let text = "fn test(x:i64) -> bool {return yield cor;}";

        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let x = table.insert("x".into());
        let cor = table.insert("cor".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        if let Some(Item::Routine(RoutineDef {
            context: ctx,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*ctx, new_ctx(3, 7));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 9), x, &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(box r) => {
                    assert_eq!(
                        *r.get_value(),
                        Some(Expression::Yield(
                            new_ctx(37, 40),
                            Box::new(Expression::Identifier(new_ctx(37, 40), cor))
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
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
            assert_eq!(l, new_ctx(0, 2));
            assert_eq!(*cond, Expression::Identifier(new_ctx(4, 5), x));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::I64(new_ctx(8, 9), 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm {
                assert_eq!(*final_exp, Expression::I64(new_ctx(17, 18), 7));
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
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let z = table.insert("z".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
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
            assert_eq!(l, new_ctx(0, 2));
            assert_eq!(*cond, Expression::Identifier(new_ctx(4, 5), x));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::I64(new_ctx(8, 9), 5));
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
                        new_ctx(20, 26),
                        BinaryOperator::BAnd,
                        Box::new(Expression::Identifier(new_ctx(20, 21), y)),
                        Box::new(Expression::Identifier(new_ctx(25, 26), z))
                    )
                );
                if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                    assert_eq!(*final_exp, Expression::I64(new_ctx(29, 30), 7));
                } else {
                    panic!("Expected Expression block");
                }

                if let Some(box Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm
                {
                    assert_eq!(*final_exp, Expression::I64(new_ctx(38, 39), 8));
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

        let mut table = StringTable::new();
        let x = table.insert("x".into());

        let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens);
        let exp = expression(&mut stream).unwrap();
        if let Some(Expression::While {
            context: l,
            cond,
            body,
        }) = exp
        {
            assert_eq!(l, new_ctx(0, 14));
            assert_eq!(*cond, Expression::Identifier(new_ctx(7, 8), x));
            if let Expression::ExpressionBlock(_ctx, body, None) = *body {
                assert_eq!(
                    body[0],
                    Statement::Expression(box Expression::I64(new_ctx(11, 12), 5))
                );
            } else {
                panic!("Expected Expression block, got {:?}", *body);
            }
        } else {
            panic!("No nodes returned by parser, got: {:?}", exp)
        }
    }

    #[test]
    fn parse_struct_def() {
        let mut table = StringTable::new();
        let test = table.insert("test".into());
        let my_struct = table.insert("MyStruct".into());
        let x = table.insert("x".into());
        let y = table.insert("y".into());

        for (text, expected) in vec![
            (
                "struct MyStruct {}",
                StructDef::new(my_struct, new_ctx(0, 6), vec![]),
            ),
            (
                "struct MyStruct {x: i64}",
                StructDef::new(
                    my_struct,
                    new_ctx(0, 6),
                    vec![Parameter::new(new_ctx(17, 18), x, &Type::I64)],
                ),
            ),
            (
                "struct MyStruct {x: i64, y: bool}",
                StructDef::new(
                    my_struct,
                    new_ctx(0, 6),
                    vec![
                        Parameter::new(new_ctx(17, 18), x, &Type::I64),
                        Parameter::new(new_ctx(25, 26), y, &Type::Bool),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            if let Some(m) = parse(test, &tokens).unwrap() {
                assert_eq!(m.get_structs()[0], Item::Struct(expected), "{:?}", text);
            }
        }
    }

    #[test]
    fn parse_struct_init() {
        let mut table = StringTable::new();
        let my_struct = table.insert("MyStruct".into());
        let my_struct2 = table.insert("MyStruct2".into());
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let z = table.insert("z".into());

        for (text, expected) in vec![
            (
                "MyStruct{}",
                Expression::StructExpression(
                    new_ctx(0, 8),
                    vec![Element::Id(my_struct)].into(),
                    vec![],
                ),
            ),
            (
                "MyStruct{x: 5}",
                Expression::StructExpression(
                    new_ctx(0, 8),
                    vec![Element::Id(my_struct)].into(),
                    vec![(x, Expression::I64(new_ctx(12, 13), 5))],
                ),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Expression::StructExpression(
                    new_ctx(0, 8),
                    vec![Element::Id(my_struct)].into(),
                    vec![
                        (x, Expression::I64(new_ctx(12, 13), 5)),
                        (y, Expression::Boolean(new_ctx(18, 23), false)),
                    ],
                ),
            ),
            (
                "MyStruct{x: 5, y: MyStruct2{z:3}}",
                Expression::StructExpression(
                    new_ctx(0, 8),
                    vec![Element::Id(my_struct)].into(),
                    vec![
                        (x, Expression::I64(new_ctx(12, 13), 5)),
                        (
                            y,
                            Expression::StructExpression(
                                new_ctx(18, 27),
                                vec![Element::Id(my_struct2)].into(),
                                vec![(z, Expression::I64(new_ctx(30, 31), 3))],
                            ),
                        ),
                    ],
                ),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens);
            let result = expression(&mut stream);
            assert_eq!(result, Ok(Some(expected)), "{:?}", text);
        }
    }

    #[test]
    fn parse_string_literals() {
        let mut table = StringTable::new();
        let test_mod = table.insert("test_mod".into());
        let test = table.insert("test".into());
        let test2 = table.insert("test 2".into());

        for (text, expected, literal_len) in vec![
            ("fn test() -> String {return \"test\";}", test, 6),
            ("fn test() -> String {return \"test 2\";}", test2, 8),
        ] {
            let tokens: Vec<Token> = Lexer::from_str(&mut table, &text)
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let module = parse(test_mod, &tokens).unwrap();
            match module {
                Some(m) => match &m.get_functions()[0] {
                    Item::Routine(RoutineDef { body, .. }) => match &body[0] {
                        Statement::Return(box r) => {
                            assert_eq!(
                                *r.get_value(),
                                Some(Expression::StringLiteral(
                                    new_ctx(28, 28 + literal_len),
                                    expected
                                ))
                            )
                        }
                        _ => assert!(false, "Not a return statement"),
                    },
                    _ => assert!(false, "Not a return statement"),
                },
                _ => assert!(false, "Not a routine, got {:?}", module),
            }
        }
    }
}
