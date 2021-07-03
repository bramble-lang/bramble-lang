#[cfg(test)]
mod tests {
    use crate::compiler::lexer::tokens::{Lex, Primitive, Token};
    use Lex::*;

    use super::super::lexer::*;

    #[test]
    fn test_integer() {
        let text = "5";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, I64(5)));
    }

    #[test]
    fn test_integer8() {
        let text = "5i8";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, I8(5)));
    }

    #[test]
    fn test_integer16() {
        let text = "5i16";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, I16(5)));
    }

    #[test]
    fn test_integer32() {
        let text = "5i32";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, I32(5)));
    }

    #[test]
    fn test_integer64() {
        let text = "5i64";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, I64(5)));
    }

    #[test]
    fn test_u8() {
        let text = "5u8";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, U8(5)));
    }

    #[test]
    fn test_u16() {
        let text = "5u16";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, U16(5)));
    }

    #[test]
    fn test_u32() {
        let text = "5u32";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, U32(5)));
    }

    #[test]
    fn test_u64() {
        let text = "5u64";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, U64(5)));
    }

    #[test]
    fn test_string_literal() {
        let text = "\"text\"";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1, "{:?}", tokens);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, StringLiteral("text".into())));
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(token, Token::new(1, Identifier((*text).into())));
        }
    }

    #[test]
    fn test_invalid_number() {
        for text in ["5x"].iter() {
            let mut lexer = Lexer::new(text);
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
            ("...", Token::new(1, VarArgs)),
            ("*", Token::new(1, Mul)),
            ("/", Token::new(1, Div)),
            ("+", Token::new(1, Add)),
            ("-", Token::new(1, Minus)),
            ("!", Token::new(1, Not)),
            ("&&", Token::new(1, BAnd)),
            ("||", Token::new(1, BOr)),
            (">", Token::new(1, Gr)),
            (">=", Token::new(1, GrEq)),
            ("<", Token::new(1, Ls)),
            ("<=", Token::new(1, LsEq)),
            ("==", Token::new(1, Eq)),
            ("!=", Token::new(1, NEq)),
            ("{", Token::new(1, LBrace)),
            ("}", Token::new(1, RBrace)),
            ("(", Token::new(1, LParen)),
            (")", Token::new(1, RParen)),
            ("[", Token::new(1, LBracket)),
            ("]", Token::new(1, RBracket)),
            (":=", Token::new(1, Assign)),
            (".", Token::new(1, MemberAccess)),
            ("::", Token::new(1, PathSeparator)),
            ("->", Token::new(1, LArrow)),
            (":", Token::new(1, Colon)),
            (",", Token::new(1, Comma)),
            (";", Token::new(1, Semicolon)),
        ]
        .iter()
        {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone(), Ok(expected_token.clone()), "{}", text);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i64;->yield";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 6);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Identifier("x".into())));
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Colon));
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Primitive(Primitive::I64)));
        let token = tokens[3].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Semicolon));
        let token = tokens[4].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, LArrow));
        let token = tokens[5].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Yield));
    }

    #[test]
    fn test_keywords() {
        for (text, expected_token) in [
            ("let", Token::new(1, Let)),
            ("mut", Token::new(1, Mut)),
            ("return", Token::new(1, Return)),
            ("yield", Token::new(1, Yield)),
            ("yret", Token::new(1, YieldReturn)),
            ("init", Token::new(1, Init)),
            ("co", Token::new(1, CoroutineDef)),
            ("fn", Token::new(1, FunctionDef)),
            ("extern", Token::new(1, Extern)),
            ("mod", Token::new(1, ModuleDef)),
            ("struct", Token::new(1, Struct)),
            ("if", Token::new(1, If)),
            ("else", Token::new(1, Else)),
            ("while", Token::new(1, While)),
        ]
        .iter()
        {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_primitives() {
        for (text, expected_token) in [
            ("u8", Token::new(1, Primitive(Primitive::U8))),
            ("u16", Token::new(1, Primitive(Primitive::U16))),
            ("u32", Token::new(1, Primitive(Primitive::U32))),
            ("u64", Token::new(1, Primitive(Primitive::U64))),
            ("i8", Token::new(1, Primitive(Primitive::I8))),
            ("i16", Token::new(1, Primitive(Primitive::I16))),
            ("i32", Token::new(1, Primitive(Primitive::I32))),
            ("i64", Token::new(1, Primitive(Primitive::I64))),
            ("bool", Token::new(1, Primitive(Primitive::Bool))),
            ("string", Token::new(1, Primitive(Primitive::StringLiteral))),
        ]
        .iter()
        {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_whitespace_handling() {
        for (text, t1, t2, t3, t4, t5, t6, t7, t8) in [
            ("return ( x + 5 || true )", 1, 1, 1, 1, 1, 1, 1, 1),
            (" return ( x + 5|| true ) ", 1, 1, 1, 1, 1, 1, 1, 1),
            ("return(x+5||true)", 1, 1, 1, 1, 1, 1, 1, 1),
            (
                "return
            (x+
                5
                ||
                true
            )",
                1,
                2,
                2,
                2,
                3,
                4,
                5,
                6,
            ),
        ]
        .iter()
        {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 8, "{} => {:?}", text, tokens);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(*t1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(*t2, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(*t3, Identifier("x".into()))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(*t4, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, I64(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(*t6, BOr));
            assert_eq!(tokens[6].clone().unwrap(), Token::new(*t7, Bool(true)));
            assert_eq!(tokens[7].clone().unwrap(), Token::new(*t8, RParen));
        }
    }

    #[test]
    fn test_comments() {
        for (text, t1, t2, t3, t4, t5, t6) in [
            ("return ( x + 5 || //true )", 1, 1, 1, 1, 1, 1),
            (" return ( x + 5|| /*true )*/ ", 1, 1, 1, 1, 1, 1),
            ("return(x+5||///*true)", 1, 1, 1, 1, 1, 1),
            (
                "return
            (x+
                5
                ||
                /*true
            )*/",
                1,
                2,
                2,
                2,
                3,
                4,
            ),
        ]
        .iter()
        {
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 6, "{} => {:?}", text, tokens);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(*t1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(*t2, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(*t3, Identifier("x".into()))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(*t4, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, I64(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(*t6, BOr));
        }
    }
}
