#[cfg(test)]
pub mod tests {
    use core::panic;

    use crate::{
        compiler::{
            ast::*,
            diagnostics::Logger,
            lexer::{
                tokens::{Lex, Token},
                LexerError,
            },
            parser::{tokenstream::TokenStream, Parser, ParserContext, ParserError},
            source::Offset,
            CompilerDisplay, CompilerError, Lexer, SourceMap, Span,
        },
        StringTable,
    };

    type LResult = std::result::Result<Vec<Token>, CompilerError<LexerError>>;

    fn new_span(low: u32, high: u32) -> Span {
        Span::new(Offset::new(low), Offset::new(high))
    }

    fn new_ctx(low: u32, high: u32) -> ParserContext {
        ParserContext::new(new_span(low, high))
    }

    #[test]
    fn parse_unary_operators() {
        for (text, expected) in
            vec![("-a", UnaryOperator::Negate), ("!a", UnaryOperator::Not)].iter()
        {
            let mut table = StringTable::new();
            let a = table.insert("a".into());

            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            let exp = parser.expression(&mut stream).unwrap();
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

            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            let exp = parser.expression(&mut stream).unwrap();
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
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            if let Some(Expression::BinaryOp(ctx, op, left, right)) =
                parser.expression(&mut stream).unwrap()
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
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            if let Some(Expression::BinaryOp(ctx, op, left, right)) =
                parser.expression(&mut stream).unwrap()
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
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        if let Some(Expression::BinaryOp(ctx, BinaryOperator::Mul, left, right)) =
            parser.expression(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 11));
            match *left {
                Expression::BinaryOp(ctx, BinaryOperator::Add, ll, lr) => {
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
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        if let Some(Expression::BinaryOp(ctx, BinaryOperator::BOr, left, right)) =
            parser.expression(&mut stream).unwrap()
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
                Err(CompilerError::new(
                    Span::new(Offset::new(5), Offset::new(7)),
                    ParserError::PathExpectedIdentifier,
                )),
            ),
            (
                "thing::first::",
                Err(CompilerError::new(
                    Span::new(Offset::new(12), Offset::new(14)),
                    ParserError::PathExpectedIdentifier,
                )),
            ),
        ] {
            test += 1;
            println!("Test #{}", test);
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            let sm = SourceMap::new();
            match parser.expression(&mut stream) {
                Ok(Some(Expression::Path(_, path))) => match expected {
                    Ok(expected) => assert_eq!(path, expected.into()),
                    Err(msg) => assert!(false, "{:?}", msg.fmt(&sm, &table)),
                },
                Ok(Some(Expression::Identifier(_, id))) => match expected {
                    Ok(expected) => {
                        assert_eq!(expected.len(), 1);
                        assert_eq!(Element::Id(id), expected[0]);
                    }
                    Err(msg) => assert!(false, "{:?}", msg),
                },
                Ok(Some(n)) => panic!("{} resulted in {:?}, expected {:?}", text, n, expected),
                Ok(None) => panic!("No node returned for {}, expected {:?}", text, expected),
                Err(msg) => match expected {
                    Ok(_) => assert!(false, "{}", msg.fmt(&sm, &table).unwrap()),
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
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            match parser.expression(&mut stream) {
                Ok(Some(Expression::MemberAccess(_, left, right))) => {
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
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let stm = parser.statement(&mut stream).unwrap().unwrap();
        assert_eq!(*stm.context(), new_ctx(0, 15));
        match stm {
            Statement::Bind(b) => {
                assert_eq!(*b.context(), new_ctx(0, 15));
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
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let stm = parser.statement(&mut stream).unwrap().unwrap();
        assert_eq!(*stm.context(), new_ctx(0, 19));
        match stm {
            Statement::Bind(b) => {
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

            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);

            let stm = parser.statement(&mut stream).unwrap().unwrap();

            assert_eq!(*stm.context(), new_ctx(0, text.len() as u32));

            match stm {
                Statement::Bind(b) => {
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let stm = parser.statement(&mut stream).unwrap().unwrap();
        assert_eq!(*stm.context(), new_ctx(0, text.len() as u32));
        match stm {
            Statement::Mutate(m) => {
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser
            .parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_mod)
        {
            assert_eq!(*m.context(), new_ctx(0, 15));
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);

        if let Some(m) = parser.parse(test, &tokens).unwrap() {
            assert_eq!(*m.context(), new_ctx(0, 44));
            assert_eq!(m.get_name(), test);

            assert_eq!(m.get_modules().len(), 1);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);

            let m = &m.get_modules()[0];
            assert_eq!(*m.context(), new_ctx(0, 44));
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
                assert_eq!(*context, new_ctx(18, 42));
                assert_eq!(*name, test);
                assert_eq!(
                    params,
                    &vec![Parameter::new(new_ctx(26, 31), x, &Type::I64)]
                );
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(r) => assert_eq!(*r.get_value(), None),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser
            .parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_co_mod)
        {
            assert_eq!(*m.context(), new_ctx(0, 44));
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
                assert_eq!(*context, new_ctx(18, 42));
                assert_eq!(*name, test);
                assert_eq!(
                    params,
                    &vec![Parameter::new(new_ctx(26, 31), x, &Type::I64)]
                );
                assert_eq!(ty, &Type::Unit);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(r) => assert_eq!(*r.get_value(), None),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser
            .parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_struct_mod)
        {
            assert_eq!(*m.context(), new_ctx(0, 48));
            assert_eq!(m.get_name(), test_struct_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 1);

            if let Some(Item::Struct(sd)) = m.get_item(my_struct) {
                assert_eq!(*sd.context(), new_ctx(22, 46));
                assert_eq!(sd.get_name(), my_struct);
                assert_eq!(
                    sd.get_fields(),
                    &vec![Parameter::new(new_ctx(39, 45), x, &Type::I64)]
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser
            .parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_extern_mod)
        {
            assert_eq!(*m.context(), new_ctx(0, 55));
            assert_eq!(m.get_name(), test_extern_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item(my_fn) {
                assert_eq!(*e.context(), new_ctx(22, 53));
                assert_eq!(e.get_name(), my_fn);
                assert_eq!(
                    e.get_params(),
                    &vec![Parameter::new(new_ctx(38, 44), x, &Type::I64)]
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser
            .parse(test, &tokens)
            .unwrap()
            .unwrap()
            .get_module(test_extern_mod)
        {
            assert_eq!(*m.context(), new_ctx(0, 60));
            assert_eq!(m.get_name(), test_extern_mod);

            assert_eq!(m.get_modules().len(), 0);
            assert_eq!(m.get_functions().len(), 0);
            assert_eq!(m.get_coroutines().len(), 0);
            assert_eq!(m.get_structs().len(), 0);
            assert_eq!(m.get_externs().len(), 1);

            if let Some(Item::Extern(e)) = m.get_item(my_fn) {
                assert_eq!(*e.context(), new_ctx(22, 58));
                assert_eq!(e.get_name(), my_fn);
                assert_eq!(
                    e.get_params(),
                    &vec![Parameter::new(new_ctx(38, 44), x, &Type::I64)]
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parser.parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*l, new_ctx(0, 24));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 13), x, &Type::I64)]);
            assert_eq!(ty, Type::Unit);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(r) => assert_eq!(*r.get_value(), None),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(Item::Routine(RoutineDef {
            context: l,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parser.parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*l, new_ctx(0, 37));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 13), x, &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(r) => {
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        parser.parse(test, &tokens).expect_err("This should fail");
    }

    #[test]
    fn parse_routine_call() {
        let text = "test(x, y)";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let y = table.insert("y".into());
        let test = table.insert("test".into());

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut iter = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            parser.expression(&mut iter).unwrap()
        {
            assert_eq!(l, new_ctx(0, 10));
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        println!("{:?}", table);
        let mut iter = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        if let Some(Expression::RoutineCall(l, RoutineCall::Function, name, params)) =
            parser.expression(&mut iter).unwrap()
        {
            assert_eq!(l, new_ctx(0, 16));
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(m) = parser.parse(test_mod, &tokens).unwrap() {
            assert_eq!(*m.context(), new_ctx(0, 37));
            if let Some(Item::Routine(RoutineDef {
                context,
                def: RoutineDefType::Coroutine,
                name,
                params,
                ret_ty: ty,
                body,
            })) = m.get_item(test)
            {
                assert_eq!(*context, new_ctx(0, 37));
                assert_eq!(*name, test);
                assert_eq!(params, &vec![Parameter::new(new_ctx(8, 13), x, &Type::I64)]);
                assert_eq!(ty, &Type::Bool);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return(r) => {
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let stm = parser.statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(b) => {
                assert_eq!(b.get_id(), x);
                assert_eq!(b.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *b.get_rhs(),
                    Expression::RoutineCall(
                        new_ctx(16, 28),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let stm = parser.statement(&mut stream).unwrap().unwrap();
        match stm {
            Statement::Bind(bind) => {
                assert_eq!(bind.get_id(), x);
                assert_eq!(bind.get_type(), Type::Coroutine(Box::new(Type::I64)));
                assert_eq!(
                    *bind.get_rhs(),
                    Expression::RoutineCall(
                        new_ctx(16, 34),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let parser = Parser::new(&logger);
        if let Some(Item::Routine(RoutineDef {
            context: ctx,
            def: RoutineDefType::Function,
            name,
            params,
            ret_ty: ty,
            body,
            ..
        })) = parser.parse(test, &tokens).unwrap().unwrap().get_item(test)
        {
            assert_eq!(*ctx, new_ctx(0, 42));
            assert_eq!(*name, test);
            assert_eq!(*params, vec![Parameter::new(new_ctx(8, 13), x, &Type::I64)]);
            assert_eq!(ty, Type::Bool);
            assert_eq!(body.len(), 1);
            match &body[0] {
                Statement::Return(r) => {
                    assert_eq!(
                        *r.get_value(),
                        Some(Expression::Yield(
                            new_ctx(31, 40),
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let exp = parser.expression(&mut stream).unwrap();
        if let Some(Expression::If {
            context: l,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(l, new_ctx(0, 19));
            assert_eq!(*cond, Expression::Identifier(new_ctx(4, 5), x));
            if let Expression::ExpressionBlock(_l, _body, Some(final_exp)) = *if_arm {
                assert_eq!(*final_exp, Expression::I64(new_ctx(8, 9), 5));
            } else {
                panic!("Expected Expression block");
            }

            if let Some(Expression::ExpressionBlock(_l, _body, Some(final_exp))) = else_arm {
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let exp = parser.expression(&mut stream).unwrap();
        if let Some(Expression::If {
            context,
            cond,
            if_arm,
            else_arm,
        }) = exp
        {
            assert_eq!(context, new_ctx(0, 40));
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
                context,
            }) = else_arm
            {
                assert_eq!(context, new_ctx(16, 40));
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

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        let exp = parser.expression(&mut stream).unwrap();
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
                    Statement::Expression(box Expression::I64(new_ctx(11, 13), 5))
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
                StructDef::new(my_struct, new_ctx(0, 18), vec![]),
            ),
            (
                "struct MyStruct {x: i64}",
                StructDef::new(
                    my_struct,
                    new_ctx(0, 24),
                    vec![Parameter::new(new_ctx(17, 23), x, &Type::I64)],
                ),
            ),
            (
                "struct MyStruct {x: i64, y: bool}",
                StructDef::new(
                    my_struct,
                    new_ctx(0, 33),
                    vec![
                        Parameter::new(new_ctx(17, 23), x, &Type::I64),
                        Parameter::new(new_ctx(25, 32), y, &Type::Bool),
                    ],
                ),
            ),
        ] {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let parser = Parser::new(&logger);
            if let Some(m) = parser.parse(test, &tokens).unwrap() {
                assert_eq!(m.get_structs()[0], Item::Struct(expected), "{:?}", text);
            }
        }
    }

    #[test]
    fn parse_struct_expression() {
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
                    new_ctx(0, 10),
                    vec![Element::Id(my_struct)].into(),
                    vec![],
                ),
            ),
            (
                "MyStruct{x: 5}",
                Expression::StructExpression(
                    new_ctx(0, 14),
                    vec![Element::Id(my_struct)].into(),
                    vec![(x, Expression::I64(new_ctx(12, 13), 5))],
                ),
            ),
            (
                "MyStruct{x: 5, y: false}",
                Expression::StructExpression(
                    new_ctx(0, 24),
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
                    new_ctx(0, 33),
                    vec![Element::Id(my_struct)].into(),
                    vec![
                        (x, Expression::I64(new_ctx(12, 13), 5)),
                        (
                            y,
                            Expression::StructExpression(
                                new_ctx(18, 32),
                                vec![Element::Id(my_struct2)].into(),
                                vec![(z, Expression::I64(new_ctx(30, 31), 3))],
                            ),
                        ),
                    ],
                ),
            ),
        ] {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            let result = parser.expression(&mut stream);
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
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let parser = Parser::new(&logger);
            let module = parser.parse(test_mod, &tokens).unwrap();
            match module {
                Some(m) => match &m.get_functions()[0] {
                    Item::Routine(RoutineDef { body, .. }) => match &body[0] {
                        Statement::Return(r) => {
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

    #[test]
    fn parse_number() {
        for (text, expected) in vec![
            ("64u8", Expression::U8(new_ctx(0, 4), 64)),
            ("64u16", Expression::U16(new_ctx(0, 5), 64)),
            ("64u32", Expression::U32(new_ctx(0, 5), 64)),
            ("64u64", Expression::U64(new_ctx(0, 5), 64)),
            ("5i8", Expression::I8(new_ctx(0, 3), 5)),
            ("5i16", Expression::I16(new_ctx(0, 4), 5)),
            ("5i32", Expression::I32(new_ctx(0, 4), 5)),
            ("64i64", Expression::I64(new_ctx(0, 5), 64)),
            ("64", Expression::I64(new_ctx(0, 2), 64)),
        ] {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            match parser.number(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_expression() {
        for (text, expected) in vec![
            (
                "[1]",
                Expression::ArrayExpression(
                    new_ctx(0, 3),
                    vec![Expression::I64(new_ctx(1, 2), 1)],
                    1,
                ),
            ),
            (
                "[1u8]",
                Expression::ArrayExpression(
                    new_ctx(0, 5),
                    vec![Expression::U8(new_ctx(1, 4), 1)],
                    1,
                ),
            ),
            (
                "[1,]",
                Expression::ArrayExpression(
                    new_ctx(0, 4),
                    vec![Expression::I64(new_ctx(1, 2), 1)],
                    1,
                ),
            ),
            (
                "[1, 2, 3]",
                Expression::ArrayExpression(
                    new_ctx(0, 9),
                    vec![
                        Expression::I64(new_ctx(1, 2), 1),
                        Expression::I64(new_ctx(4, 5), 2),
                        Expression::I64(new_ctx(7, 8), 3),
                    ],
                    3,
                ),
            ),
            (
                "[1, 2i8, 3]", // This is legal at the parser level (it is illegal semantically)
                Expression::ArrayExpression(
                    new_ctx(0, 11),
                    vec![
                        Expression::I64(new_ctx(1, 2), 1),
                        Expression::I8(new_ctx(4, 7), 2),
                        Expression::I64(new_ctx(9, 10), 3),
                    ],
                    3,
                ),
            ),
            (
                "[[1,],]",
                Expression::ArrayExpression(
                    new_ctx(0, 7),
                    vec![Expression::ArrayExpression(
                        new_ctx(1, 5),
                        vec![Expression::I64(new_ctx(2, 3), 1)],
                        1,
                    )],
                    1,
                ),
            ),
        ] {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            match parser.array_expression(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_fails() {
        for (text, msg) in [
            (
                "[5",
                CompilerError::new(
                    Span::new(Offset::new(2), Offset::new(2)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], None),
                ),
            ),
            (
                "[5 6]",
                CompilerError::new(
                    Span::new(Offset::new(3), Offset::new(4)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], Some(Lex::I64(6))),
                ),
            ),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            assert_eq!(
                parser.array_expression(&mut stream).unwrap_err(),
                *msg,
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_array_at_index() {
        let mut table = StringTable::new();
        let a = table.insert("a".into());
        let b = table.insert("b".into());

        for (text, expected) in vec![
            //
            (
                "a[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 4),
                    array: Box::new(Expression::Identifier(new_ctx(0, 1), a)),
                    index: Box::new(Expression::I64(new_ctx(2, 3), 1)),
                },
            ),
            (
                "(a)[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 6),
                    array: Box::new(Expression::Identifier(new_ctx(0, 3), a)),
                    index: Box::new(Expression::I64(new_ctx(4, 5), 1)),
                },
            ),
            (
                "a.b[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 6),
                    array: Box::new(Expression::MemberAccess(
                        new_ctx(0, 3),
                        Box::new(Expression::Identifier(new_ctx(0, 1), a)),
                        b,
                    )),
                    index: Box::new(Expression::I64(new_ctx(4, 5), 1)),
                },
            ),
            (
                "a[1].b",
                Expression::MemberAccess(
                    new_ctx(0, 6),
                    Box::new(Expression::ArrayAt {
                        context: new_ctx(0, 4),
                        array: Box::new(Expression::Identifier(new_ctx(0, 1), a)),
                        index: Box::new(Expression::I64(new_ctx(2, 3), 1)),
                    }),
                    b,
                ),
            ),
            (
                "a[0].b[1]",
                Expression::ArrayAt {
                    context: new_ctx(0, 9),
                    array: Box::new(Expression::MemberAccess(
                        new_ctx(0, 6),
                        Box::new(Expression::ArrayAt {
                            context: new_ctx(0, 4),
                            array: Box::new(Expression::Identifier(new_ctx(0, 1), a)),
                            index: Box::new(Expression::I64(new_ctx(2, 3), 0)),
                        }),
                        b,
                    )),
                    index: Box::new(Expression::I64(new_ctx(7, 8), 1)),
                },
            ),
            (
                "a[1][2]",
                Expression::ArrayAt {
                    context: new_ctx(0, 7),
                    array: Box::new(Expression::ArrayAt {
                        context: new_ctx(0, 4),
                        array: Box::new(Expression::Identifier(new_ctx(0, 1), a)),
                        index: Box::new(Expression::I64(new_ctx(2, 3), 1)),
                    }),
                    index: Box::new(Expression::I64(new_ctx(5, 6), 2)),
                },
            ),
            (
                "((a)[1])[2]",
                Expression::ArrayAt {
                    context: new_ctx(0, 11),
                    array: Box::new(Expression::ArrayAt {
                        context: new_ctx(0, 8),
                        array: Box::new(Expression::Identifier(new_ctx(1, 4), a)),
                        index: Box::new(Expression::I64(new_ctx(5, 6), 1)),
                    }),
                    index: Box::new(Expression::I64(new_ctx(9, 10), 2)),
                },
            ),
        ] {
            println!("Test: {}", text);
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            match parser.expression(&mut stream) {
                Ok(Some(e)) => assert_eq!(e, expected),
                Ok(t) => panic!("Expected an {:?} but got {:?}", expected, t),
                Err(err) => panic!("Expected {:?}, but got {:?}", expected, err),
            }
        }
    }

    #[test]
    fn parse_array_at_index_fails() {
        for (text, msg) in [
            (
                "a[5",
                CompilerError::new(
                    Span::new(Offset::new(3), Offset::new(3)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], None),
                ),
            ),
            (
                "a[5 6]",
                CompilerError::new(
                    Span::new(Offset::new(4), Offset::new(5)),
                    ParserError::ExpectedButFound(vec![Lex::RBracket], Some(Lex::I64(6))),
                ),
            ),
            (
                "a[]",
                CompilerError::new(
                    Span::new(Offset::new(1), Offset::new(2)),
                    ParserError::IndexOpInvalidExpr,
                ),
            ),
            (
                "a[2 + ]",
                CompilerError::new(
                    Span::new(Offset::new(4), Offset::new(5)),
                    ParserError::ExpectedExprAfter(Lex::Add),
                ),
            ),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let parser = Parser::new(&logger);
            assert_eq!(
                parser.expression(&mut stream).unwrap_err(),
                *msg,
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_member_access() {
        for (text, low, high) in vec![
            (" thing.first", 1, 6),
            ("(thing).first", 0, 7),
            ("(thing.first)", 1, 6),
        ] {
            let mut table = StringTable::new();
            let thing_id = table.insert("thing".into());
            let first_id = table.insert("first".into());

            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();
            let src = sm.get(0).unwrap().read().unwrap();

            let logger = Logger::new();
            let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            match parser.member_access(&mut stream) {
                Ok(Some(Expression::MemberAccess(_, left, right))) => {
                    assert_eq!(
                        *left,
                        Expression::Identifier(new_ctx(low, high), thing_id),
                        "Input: {}",
                        text,
                    );
                    assert_eq!(right, first_id);
                }
                Ok(Some(n)) => panic!("{} resulted in {:?}", text, n),
                Ok(None) => panic!("No node returned for {}", text),
                Err(msg) => panic!("{} caused {:?}", text, msg),
            }
        }
    }

    #[test]
    fn parse_expression_block_oneline() {
        let text = "{5}";
        let mut table = StringTable::new();

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();

        let parser = Parser::new(&logger);
        if let Some(Expression::ExpressionBlock(ctx, body, Some(final_exp))) =
            parser.expression_block(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 3));
            assert_eq!(body.len(), 0);
            assert_eq!(*final_exp, Expression::I64(new_ctx(1, 2), 5));
        } else {
            panic!("No nodes returned by parser")
        }
    }

    #[test]
    fn parse_expression_block_bad() {
        for (text, msg) in [
            (
                "{5 10 51}",
                CompilerError::new(
                    Span::new(Offset::new(3), Offset::new(5)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::I64(10))),
                ),
            ),
            (
                " {5; 10 51}",
                CompilerError::new(
                    Span::new(Offset::new(8), Offset::new(10)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::I64(51))),
                ),
            ),
            (
                "{5; 10 let x:i64 := 5}",
                CompilerError::new(
                    Span::new(Offset::new(7), Offset::new(10)),
                    ParserError::ExpectedButFound(vec![Lex::RBrace], Some(Lex::Let)),
                ),
            ),
            (
                "{let x: i64 := 10 5}",
                CompilerError::new(
                    Span::new(Offset::new(1), Offset::new(17)),
                    ParserError::ExpectedButFound(vec![Lex::Semicolon], Some(Lex::I64(5))),
                ),
            ),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let logger = Logger::new();
            let parser = Parser::new(&logger);
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let tokens = Lexer::new(src, &mut table, &logger)
                .unwrap()
                .tokenize()
                .into_iter()
                .collect::<LResult>()
                .unwrap();
            let mut stream = TokenStream::new(&tokens, &logger).unwrap();
            assert_eq!(
                parser.expression_block(&mut stream).unwrap_err(),
                *msg,
                "{:?}",
                text
            );
        }
    }

    #[test]
    fn parse_expression_block_multiline() {
        let text = "{let x:i64 := 5; f(x); x * x}";
        let mut table = StringTable::new();
        let x = table.insert("x".into());
        let f = table.insert("f".into());

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let logger = Logger::new();
        let tokens: Vec<Token> = Lexer::new(src, &mut table, &logger)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<LResult>()
            .unwrap();
        let mut stream = TokenStream::new(&tokens, &logger).unwrap();
        let parser = Parser::new(&logger);
        if let Some(Expression::ExpressionBlock(ctx, body, Some(final_exp))) =
            parser.expression_block(&mut stream).unwrap()
        {
            assert_eq!(ctx, new_ctx(0, 29));
            assert_eq!(body.len(), 2);
            match &body[0] {
                Statement::Bind(b) => {
                    assert_eq!(b.get_id(), x);
                    assert_eq!(b.get_type(), Type::I64);
                    assert_eq!(*b.get_rhs(), Expression::I64(new_ctx(14, 15), 5));
                }
                _ => panic!("Not a binding statement"),
            }
            match &body[1] {
                Statement::Expression(box Expression::RoutineCall(
                    _,
                    RoutineCall::Function,
                    fn_name,
                    params,
                )) => {
                    assert_eq!(*fn_name, vec![Element::Id(f)].into());
                    assert_eq!(params[0], Expression::Identifier(new_ctx(19, 20), x));
                }
                _ => panic!("No body: {:?}", &body[1]),
            }
            match final_exp {
                box Expression::BinaryOp(_, BinaryOperator::Mul, l, r) => {
                    assert_eq!(*l.as_ref(), Expression::Identifier(new_ctx(23, 24), x));
                    assert_eq!(*r.as_ref(), Expression::Identifier(new_ctx(27, 28), x));
                }
                _ => panic!("No body: {:?}", &body[2]),
            }
        } else {
            panic!("No nodes returned by parser")
        }
    }
}
