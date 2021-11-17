#[cfg(test)]
mod tests {
    use crate::{
        compiler::{
            diagnostics::Logger,
            lexer::{
                tokens::{Lex, Primitive, Token},
                LexerError,
            },
            source::Offset,
            CompilerError, SourceMap, Span,
        },
        StringTable,
    };
    use Lex::*;

    use super::super::lexer::*;

    fn new_span(l: u32, h: u32) -> Span {
        Span::new(Offset::new(l), Offset::new(h))
    }

    #[test]
    fn test_integer() {
        let text = "5";

        let mut sm = SourceMap::new();
        sm.add_string(text.into(), "/tst".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();

        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(I64(5), 1, new_span(0, 1)));
    }

    #[test]
    fn test_integer8() {
        let text = "5i8";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(I8(5), 1, new_span(0, 3)))
    }

    #[test]
    fn test_integer16() {
        let text = "5i16";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(I16(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_integer32() {
        let text = "5i32";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(I32(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_integer64() {
        let text = "5i64";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(I64(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_u8() {
        let text = "5u8";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(U8(5), 1, new_span(0, 3)));
    }

    #[test]
    fn test_u16() {
        let text = "5u16";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(U16(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_u32() {
        let text = "5u32";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(U32(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_u64() {
        let text = "5u64";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(U64(5), 1, new_span(0, 4)));
    }

    #[test]
    fn test_string_literal() {
        let text = "\"text\"";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 1, "{:?}", tokens);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(
            token,
            Token::new(
                StringLiteral(table.insert("text".into())),
                1,
                new_span(0, 6)
            )
        );
    }

    #[test]
    fn test_invalid_string_literal() {
        let text = "\"text";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();

        assert_eq!(tokens.len(), 1, "{:?}", tokens);
        let token = tokens[0].clone().expect_err("Expected error");
        assert_eq!(
            token,
            CompilerError::new(new_span(0, 5), LexerError::UnexpectedEof)
        );
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(
                token,
                Token::new(
                    Identifier(table.insert((*text).into())),
                    1,
                    new_span(0, text.len() as u32)
                )
            );
        }
    }

    #[test]
    fn test_invalid_number() {
        for text in ["5x"].iter() {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 2);
            tokens[0]
                .clone()
                .expect_err("Expected error for invalid identifier");
        }
    }

    #[test]
    fn test_operator() {
        for (text, expected_symbol) in [
            ("...", VarArgs),
            ("*", Mul),
            ("/", Div),
            ("+", Add),
            ("-", Minus),
            ("!", Not),
            ("&&", BAnd),
            ("||", BOr),
            (">", Gr),
            (">=", GrEq),
            ("<", Ls),
            ("<=", LsEq),
            ("==", Eq),
            ("!=", NEq),
            ("{", LBrace),
            ("}", RBrace),
            ("(", LParen),
            (")", RParen),
            ("[", LBracket),
            ("]", RBracket),
            (":=", Assign),
            (".", MemberAccess),
            ("::", PathSeparator),
            ("->", LArrow),
            (":", Colon),
            (",", Comma),
            (";", Semicolon),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);

            let expected_token = Token::new(*expected_symbol, 1, new_span(0, text.len() as u32));
            assert_eq!(tokens[0].clone(), Ok(expected_token), "{}", text);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i64;->yield";
        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();

        let mut table = StringTable::new();
        let src = sm.get(0).unwrap().read().unwrap();
        let logger = Logger::new();
        let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 6);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(
            token,
            Token::new(Identifier(table.insert("x".into())), 1, new_span(0, 1))
        );
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Colon, 1, new_span(1, 2)));
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(
            token,
            Token::new(Primitive(Primitive::I64), 1, new_span(2, 5))
        );
        let token = tokens[3].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Semicolon, 1, new_span(5, 6)));
        let token = tokens[4].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(LArrow, 1, new_span(6, 8)));
        let token = tokens[5].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Yield, 1, new_span(8, 13)));
    }

    #[test]
    fn test_keywords() {
        for (text, expected_symbol) in [
            ("let", Let),
            ("mut", Mut),
            ("return", Return),
            ("yield", Yield),
            ("yret", YieldReturn),
            ("init", Init),
            ("co", CoroutineDef),
            ("fn", FunctionDef),
            ("extern", Extern),
            ("mod", ModuleDef),
            ("struct", Struct),
            ("if", If),
            ("else", Else),
            ("while", While),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);

            let expected_token = Token::new(*expected_symbol, 1, new_span(0, text.len() as u32));
            assert_eq!(tokens[0].clone().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_primitives() {
        for (text, expected_symbol) in [
            ("u8", Primitive(Primitive::U8)),
            ("u16", Primitive(Primitive::U16)),
            ("u32", Primitive(Primitive::U32)),
            ("u64", Primitive(Primitive::U64)),
            ("i8", Primitive(Primitive::I8)),
            ("i16", Primitive(Primitive::I16)),
            ("i32", Primitive(Primitive::I32)),
            ("i64", Primitive(Primitive::I64)),
            ("bool", Primitive(Primitive::Bool)),
            ("string", Primitive(Primitive::StringLiteral)),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);

            let expected_token = Token::new(*expected_symbol, 1, new_span(0, text.len() as u32));
            assert_eq!(tokens[0].clone().unwrap(), expected_token);
        }
    }

    #[test]
    fn test_whitespace_handling() {
        for (text, t1, t2, t3, t4, t5, t6, t7, t8) in [
            (
                "return ( x + 5 || true )",
                (1, (0, 6)),
                (1, (7, 8)),
                (1, (9, 10)),
                (1, (11, 12)),
                (1, (13, 14)),
                (1, (15, 17)),
                (1, (18, 22)),
                (1, (23, 24)),
            ),
            (
                " return ( x + 5|| true ) ",
                (1, (1, 7)),   // return
                (1, (8, 9)),   // (
                (1, (10, 11)), // x
                (1, (12, 13)), // +
                (1, (14, 15)), // 5
                (1, (15, 17)), // ||
                (1, (18, 22)), // true
                (1, (23, 24)), // , // ))
            ),
            (
                "return(x+5||true)",
                (1, (0, 6)),   // return
                (1, (6, 7)),   // (
                (1, (7, 8)),   // x
                (1, (8, 9)),   // +
                (1, (9, 10)),  // 5
                (1, (10, 12)), // ||
                (1, (12, 16)), // true
                (1, (16, 17)), // )
            ),
            (
                "return
            (x+
                5
                ||
                true
            )",
                (1, (0, 6)),   // return
                (2, (19, 20)), // (
                (2, (20, 21)), // x
                (2, (21, 22)), // +
                (3, (39, 40)), // 5
                (4, (57, 59)), // ||
                (5, (76, 80)), // true
                (6, (93, 94)), // )
            ),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 8, "{} => {:?}", text, tokens);
            assert_eq!(
                tokens[0].clone().unwrap(),
                Token::new(Return, t1.0, new_span(t1.1 .0, t1.1 .1))
            );
            assert_eq!(
                tokens[1].clone().unwrap(),
                Token::new(LParen, t2.0, new_span(t2.1 .0, t2.1 .1))
            );
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(
                    Identifier(table.insert("x".into())),
                    t3.0,
                    new_span(t3.1 .0, t3.1 .1)
                )
            );
            assert_eq!(
                tokens[3].clone().unwrap(),
                Token::new(Add, t4.0, new_span(t4.1 .0, t4.1 .1))
            );
            assert_eq!(
                tokens[4].clone().unwrap(),
                Token::new(I64(5), t5.0, new_span(t5.1 .0, t5.1 .1))
            );
            assert_eq!(
                tokens[5].clone().unwrap(),
                Token::new(BOr, t6.0, new_span(t6.1 .0, t6.1 .1))
            );
            assert_eq!(
                tokens[6].clone().unwrap(),
                Token::new(Bool(true), t7.0, new_span(t7.1 .0, t7.1 .1))
            );
            assert_eq!(
                tokens[7].clone().unwrap(),
                Token::new(RParen, t8.0, new_span(t8.1 .0, t8.1 .1))
            );
        }
    }

    #[test]
    fn test_comments() {
        for (text, t1, t2, t3, t4, t5, t6) in [
            (
                "return ( x + 5 || //true )",
                (1, (0, 6)),
                (1, (7, 8)),
                (1, (9, 10)),
                (1, (11, 12)),
                (1, (13, 14)),
                (1, (15, 17)),
            ),
            (
                " return ( x + 5|| /*true )*/ ",
                (1, (1, 7)),
                (1, (8, 9)),
                (1, (10, 11)),
                (1, (12, 13)),
                (1, (14, 15)),
                (1, (15, 17)),
            ),
            (
                "return(x+5||///*true)",
                (1, (0, 6)),
                (1, (6, 7)),
                (1, (7, 8)),
                (1, (8, 9)),
                (1, (9, 10)),
                (1, (10, 12)),
            ),
            (
                "return
            (x+
                5
                ||
                /*true
            )*/",
                (1, (0, 6)),
                (2, (19, 20)),
                (2, (20, 21)),
                (2, (21, 22)),
                (3, (39, 40)),
                (4, (57, 59)),
            ),
        ]
        .iter()
        {
            let mut sm = SourceMap::new();
            sm.add_string(text, "/test".into()).unwrap();

            let mut table = StringTable::new();
            let src = sm.get(0).unwrap().read().unwrap();
            let logger = Logger::new();
            let mut lexer = Lexer::new(src, &mut table, &logger).unwrap();
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 6, "{} => {:?}", text, tokens);
            assert_eq!(
                tokens[0].clone().unwrap(),
                Token::new(Return, t1.0, new_span(t1.1 .0, t1.1 .1))
            );
            assert_eq!(
                tokens[1].clone().unwrap(),
                Token::new(LParen, t2.0, new_span(t2.1 .0, t2.1 .1))
            );
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(
                    Identifier(table.insert("x".into())),
                    t3.0,
                    new_span(t3.1 .0, t3.1 .1)
                )
            );
            assert_eq!(
                tokens[3].clone().unwrap(),
                Token::new(Add, t4.0, new_span(t4.1 .0, t4.1 .1))
            );
            assert_eq!(
                tokens[4].clone().unwrap(),
                Token::new(I64(5), t5.0, new_span(t5.1 .0, t5.1 .1))
            );
            assert_eq!(
                tokens[5].clone().unwrap(),
                Token::new(BOr, t6.0, new_span(t6.1 .0, t6.1 .1))
            );
        }
    }
}
