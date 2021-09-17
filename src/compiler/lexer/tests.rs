#[cfg(test)]
mod tests {
    use crate::{
        compiler::lexer::tokens::{Lex, Primitive, Token},
        StringTable,
    };
    use Lex::*;

    use super::super::lexer::*;

    #[test]
    fn test_integer() {
        let text = "5";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, I64(5)));
    }

    #[test]
    fn test_integer8() {
        let text = "5i8";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, I8(5)));
    }

    #[test]
    fn test_integer16() {
        let text = "5i16";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, I16(5)));
    }

    #[test]
    fn test_integer32() {
        let text = "5i32";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, I32(5)));
    }

    #[test]
    fn test_integer64() {
        let text = "5i64";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, I64(5)));
    }

    #[test]
    fn test_u8() {
        let text = "5u8";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, U8(5)));
    }

    #[test]
    fn test_u16() {
        let text = "5u16";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, U16(5)));
    }

    #[test]
    fn test_u32() {
        let text = "5u32";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, U32(5)));
    }

    #[test]
    fn test_u64() {
        let text = "5u64";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 0, U64(5)));
    }

    #[test]
    fn test_string_literal() {
        let text = "\"text\"";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1, "{:?}", tokens);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(
            token,
            Token::new(1, 0, StringLiteral(table.insert("text".into())))
        );
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(
                token,
                Token::new(1, 0, Identifier(table.insert((*text).into())))
            );
        }
    }

    #[test]
    fn test_invalid_number() {
        for text in ["5x"].iter() {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 2);
            tokens[0]
                .clone()
                .expect_err("Expected error for invalid identifier");
        }
    }

    #[test]
    fn test_operator() {
        for (text, expected_token) in [
            ("...", Token::new(1, 0, VarArgs)),
            ("*", Token::new(1, 0, Mul)),
            ("/", Token::new(1, 0, Div)),
            ("+", Token::new(1, 0, Add)),
            ("-", Token::new(1, 0, Minus)),
            ("!", Token::new(1, 0, Not)),
            ("&&", Token::new(1, 0, BAnd)),
            ("||", Token::new(1, 0, BOr)),
            (">", Token::new(1, 0, Gr)),
            (">=", Token::new(1, 0, GrEq)),
            ("<", Token::new(1, 0, Ls)),
            ("<=", Token::new(1, 0, LsEq)),
            ("==", Token::new(1, 0, Eq)),
            ("!=", Token::new(1, 0, NEq)),
            ("{", Token::new(1, 0, LBrace)),
            ("}", Token::new(1, 0, RBrace)),
            ("(", Token::new(1, 0, LParen)),
            (")", Token::new(1, 0, RParen)),
            ("[", Token::new(1, 0, LBracket)),
            ("]", Token::new(1, 0, RBracket)),
            (":=", Token::new(1, 0, Assign)),
            (".", Token::new(1, 0, MemberAccess)),
            ("::", Token::new(1, 0, PathSeparator)),
            ("->", Token::new(1, 0, LArrow)),
            (":", Token::new(1, 0, Colon)),
            (",", Token::new(1, 0, Comma)),
            (";", Token::new(1, 0, Semicolon)),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone(), Ok(expected_token.clone()), "{}", text);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i64;->yield";
        let mut table = StringTable::new();
        let mut lexer = Lexer::new(&mut table, text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 6);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(
            token,
            Token::new(1, 0, Identifier(table.insert("x".into())))
        );
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 1, Colon));
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 2, Primitive(Primitive::I64)));
        let token = tokens[3].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 5, Semicolon));
        let token = tokens[4].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 6, LArrow));
        let token = tokens[5].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, 8, Yield));
    }

    #[test]
    fn test_keywords() {
        for (text, expected_token) in [
            ("let", Token::new(1, 0, Let)),
            ("mut", Token::new(1, 0, Mut)),
            ("return", Token::new(1, 0, Return)),
            ("yield", Token::new(1, 0, Yield)),
            ("yret", Token::new(1, 0, YieldReturn)),
            ("init", Token::new(1, 0, Init)),
            ("co", Token::new(1, 0, CoroutineDef)),
            ("fn", Token::new(1, 0, FunctionDef)),
            ("extern", Token::new(1, 0, Extern)),
            ("mod", Token::new(1, 0, ModuleDef)),
            ("struct", Token::new(1, 0, Struct)),
            ("if", Token::new(1, 0, If)),
            ("else", Token::new(1, 0, Else)),
            ("while", Token::new(1, 0, While)),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_primitives() {
        for (text, expected_token) in [
            ("u8", Token::new(1, 0, Primitive(Primitive::U8))),
            ("u16", Token::new(1, 0, Primitive(Primitive::U16))),
            ("u32", Token::new(1, 0, Primitive(Primitive::U32))),
            ("u64", Token::new(1, 0, Primitive(Primitive::U64))),
            ("i8", Token::new(1, 0, Primitive(Primitive::I8))),
            ("i16", Token::new(1, 0, Primitive(Primitive::I16))),
            ("i32", Token::new(1, 0, Primitive(Primitive::I32))),
            ("i64", Token::new(1, 0, Primitive(Primitive::I64))),
            ("bool", Token::new(1, 0, Primitive(Primitive::Bool))),
            (
                "string",
                Token::new(1, 0, Primitive(Primitive::StringLiteral)),
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_whitespace_handling() {
        for (text, t1, t2, t3, t4, t5, t6, t7, t8) in [
            (
                "return ( x + 5 || true )",
                (1, 0),
                (1, 7),
                (1, 9),
                (1, 11),
                (1, 13),
                (1, 15),
                (1, 18),
                (1, 23),
            ),
            (
                " return ( x + 5|| true ) ",
                (1, 1),  // return
                (1, 8),  // (
                (1, 10), // x
                (1, 12), // +
                (1, 14), // 5
                (1, 15), // ||
                (1, 18), // true
                (1, 23), // )
            ),
            (
                "return(x+5||true)",
                (1, 0),  // return
                (1, 6),  // (
                (1, 7),  // x
                (1, 8),  // +
                (1, 9),  // 5
                (1, 10), // ||
                (1, 12), // true
                (1, 16), // )
            ),
            (
                "return
            (x+
                5
                ||
                true
            )",
                (1, 0),  // return
                (2, 13), // (
                (2, 14), // x
                (2, 15), // +
                (3, 17), // 5
                (4, 17), // ||
                (5, 17), // true
                (6, 13), // )
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 8, "{} => {:?}", text, tokens);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(t1.0, t1.1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(t2.0, t2.1, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(t3.0, t3.1, Identifier(table.insert("x".into())))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(t4.0, t4.1, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(t5.0, t5.1, I64(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(t6.0, t6.1, BOr));
            assert_eq!(
                tokens[6].clone().unwrap(),
                Token::new(t7.0, t7.1, Bool(true))
            );
            assert_eq!(tokens[7].clone().unwrap(), Token::new(t8.0, t8.1, RParen));
        }
    }

    #[test]
    fn test_comments() {
        for (text, t1, t2, t3, t4, t5, t6) in [
            (
                "return ( x + 5 || //true )",
                (1, 0),
                (1, 7),
                (1, 9),
                (1, 11),
                (1, 13),
                (1, 15),
            ),
            (
                " return ( x + 5|| /*true )*/ ",
                (1, 1),
                (1, 8),
                (1, 10),
                (1, 12),
                (1, 14),
                (1, 15),
            ),
            (
                "return(x+5||///*true)",
                (1, 0),
                (1, 6),
                (1, 7),
                (1, 8),
                (1, 9),
                (1, 10),
            ),
            (
                "return
            (x+
                5
                ||
                /*true
            )*/",
                (1, 0),
                (2, 13),
                (2, 14),
                (2, 15),
                (3, 17),
                (4, 17),
            ),
        ]
        .iter()
        {
            let mut table = StringTable::new();
            let mut lexer = Lexer::new(&mut table, text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 6, "{} => {:?}", text, tokens);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(t1.0, t1.1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(t2.0, t2.1, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(t3.0, t3.1, Identifier(table.insert("x".into())))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(t4.0, t4.1, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(t5.0, t5.1, I64(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(t6.0, t6.1, BOr));
        }
    }
}
