// Token - a type which captures the different types of tokens and which is output
// by tokenize
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
    StringLiteral
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::I32 => f.write_str("i32"),
            Primitive::Bool => f.write_str("bool"),
            Primitive::StringLiteral => f.write_str("string"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lex {
    Integer(i32),
    Bool(bool),
    Identifier(String),
    StringLiteral(String),
    Mul,
    Div,
    Add,
    Minus,
    Not,
    BAnd,
    BOr,
    GrEq,
    LsEq,
    Gr,
    Ls,
    Eq,
    NEq,
    Assign,
    Semicolon,
    Comma,
    Let,
    Mut,
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Printi,
    Printiln,
    Printbln,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
    FunctionDef,
    Struct,
    If,
    Else,
    Colon,
    MemberAccess,
    LArrow,
    Primitive(Primitive),
}

impl std::fmt::Display for Lex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Lex::*;
        match self {
            Integer(i) => f.write_str(&format!("literal {}", i)),
            Bool(b) => f.write_str(&format!("literal {}", b)),
            Identifier(id) => f.write_str(&format!("identifier {}", id)),
            StringLiteral(str) => f.write_str(&format!("literal \"{}\"", str)),
            Ls => f.write_str("<"),
            LsEq => f.write_str("<="),
            Gr => f.write_str(">"),
            GrEq => f.write_str(">="),
            Eq => f.write_str("="),
            NEq => f.write_str("!="),
            Mul => f.write_str("*"),
            Div => f.write_str("/"),
            Add => f.write_str("+"),
            Minus => f.write_str("-"),
            Not => f.write_str("!"),
            BAnd => f.write_str("&&"),
            BOr => f.write_str("||"),
            Assign => f.write_str(":="),
            Semicolon => f.write_str(";"),
            Comma => f.write_str(","),
            Let => f.write_str("let"),
            Mut => f.write_str("mut"),
            Return => f.write_str("return"),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            LBrace => f.write_str("{"),
            RBrace => f.write_str("}"),
            Printi => f.write_str("printi"),
            Printiln => f.write_str("printiln"),
            Printbln => f.write_str("printbln"),
            Init => f.write_str("init"),
            Yield => f.write_str("yield"),
            YieldReturn => f.write_str("yret"),
            CoroutineDef => f.write_str("co"),
            FunctionDef => f.write_str("fn"),
            Struct => f.write_str("struct"),
            If => f.write_str("if"),
            Else => f.write_str("else"),
            Colon => f.write_str(":"),
            MemberAccess => f.write_str("."),
            LArrow => f.write_str("->"),
            Primitive(p) => f.write_str(&format!("{}", p)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub l: u32,
    pub s: Lex,
}

impl Token {
    pub fn new(l: u32, s: Lex) -> Token {
        Token { l, s }
    }
}

use Lex::*;

/*
    Better Lexer:
    provide a tokenizer that is more flexible about whitespace (does not tokenize purely on whitespace)

    Starting assumptions:
    1. Only integers (for now)
    2. identifiers are made of only letters
    3. Operators are single characters

    These will change in the future, I just want a quick starting base to improve the Lexer a bit

    high level flow:
    1. consume any whitespace
    2. when a non-whitespace character is found
    3. Is it a number, a letter, or something else
    4. Number => read until a whitespace or operator character is found
    5. Letter => read until a whitespace or an operator character is found
    6. Operator character => read one character and return that as a token

    Repeat the above steps until the end of the string is reached
*/
pub struct Lexer {
    line: u32,
}
impl Lexer {
    pub fn new() -> Lexer {
        Lexer { line: 1 }
    }

    pub fn tokenize(&mut self, text: &str) -> Vec<Result<Token, String>> {
        let mut tokens = vec![];

        let mut cs = text.chars().peekable();

        while cs.peek().is_some() {
            self.consume_whitespace(&mut cs);
            if cs.peek().is_none() {
                break;
            }
            match self.next_token(&mut cs) {
                Ok(Some(t)) => tokens.push(Ok(t)),
                Ok(None) => {cs.next();},
                Err(msg) => tokens.push(Err(msg)),
            }
        }

        tokens
    }

    fn next_token(&mut self, cs: &mut Peekable<std::str::Chars>) -> Result<Option<Token>, String> {
        match self.consume_literal(cs)? {
            Some(i) => Ok(Some(i)),
            None => match self.consume_identifier(cs)? {
                Some(id) => {
                    let tok = self.if_primitive_map(self.if_keyword_map(self.if_boolean_map(id)));
                    Ok(Some(tok))
                }
                None => match self.consume_operator(cs)? {
                    Some(op) => Ok(Some(op)),
                    None => Ok(None),
                },
            },
        }
    }

    fn consume_literal(&mut self, iter: &mut Peekable<std::str::Chars>) -> Result<Option<Token>, String> {
        match self.consume_integer(iter)? {
            Some(i) => Ok(Some(i)),
            None => match self.consume_string_literal(iter)? {
                Some(s) => Ok(Some(s)),
                None => Ok(None),
            }
        }
    }

    pub fn consume_whitespace(&mut self, iter: &mut Peekable<std::str::Chars>) {
        while iter.peek().map_or_else(|| false, |c| c.is_whitespace()) {
            match iter.next() {
                Some('\n') => self.line += 1,
                _ => {}
            }
        }
    }

    pub fn consume_identifier(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) -> Result<Option<Token>, String> {
        let mut id = String::new();
        if iter
            .peek()
            .map_or_else(|| false, |c| c.is_alphabetic() || *c == '_')
        {
            while iter
                .peek()
                .map_or_else(|| false, |c| c.is_alphanumeric() || *c == '_')
            {
                match iter.next() {
                    Some(d) => id.push(d),
                    None => break,
                }
            }
        }

        if id.len() == 0 {
            Ok(None)
        } else {
            Ok(Some(Token::new(self.line, Lex::Identifier(id))))
        }
    }

    fn consume_string_literal(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) -> Result<Option<Token>, String> {
        if let Some(c) = iter.peek() {
           if *c == '"' {
               iter.next();
               let mut s = String::new();
               while let Some(c) = iter.next() {
                   if c == '"' {
                       break;
                   }
                   s.push(c);
               }
            Ok(Some(Token::new(self.line, Lex::StringLiteral(s))))
           } else {
               Ok(None)
           }
        } else {
            Ok(None)
        }
    }

    pub fn consume_integer(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) -> Result<Option<Token>, String> {
        let mut num = String::new();

        while iter.peek().map_or_else(|| false, |c| c.is_numeric()) {
            match iter.next() {
                Some(d) => num.push(d),
                None => break,
            }
        }

        if num.len() == 0 {
            Ok(None)
        } else {
            if let Some(c) = iter.peek() {
                if c.is_alphabetic() {
                    Err(format!(
                        "L{}: Invalid integer, should not contain characters",
                        self.line
                    ))
                } else {
                    Ok(Some(Token::new(
                        self.line,
                        Integer(num.parse::<i32>().unwrap()),
                    )))
                }
            } else {
                Ok(Some(Token::new(
                    self.line,
                    Integer(num.parse::<i32>().unwrap()),
                )))
            }
        }
    }

    pub fn consume_operator(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) -> Result<Option<Token>, String> {
        let mut consume = true;
        let token = match iter.peek() {
            Some('(') => Some(Token::new(self.line, LParen)),
            Some(')') => Some(Token::new(self.line, RParen)),
            Some('{') => Some(Token::new(self.line, LBrace)),
            Some('}') => Some(Token::new(self.line, RBrace)),
            Some('*') => Some(Token::new(self.line, Mul)),
            Some('/') => {
                iter.next();
                match iter.peek() {
                    Some('/') => {
                        // this is a line comment
                        self.consume_line_comment(iter);
                        None
                    },
                    Some('*') => {
                        /* this is a block comment */
                        self.consume_block_comment(iter);
                        None
                    }
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Div))
                    }
                }
            },
            Some('+') => Some(Token::new(self.line, Add)),
            Some(';') => Some(Token::new(self.line, Semicolon)),
            Some(',') => Some(Token::new(self.line, Comma)),
            Some('.') => Some(Token::new(self.line, MemberAccess)),
            Some(':') => {
                iter.next();
                match iter.peek() {
                    Some('=') => Some(Token::new(self.line, Assign)),
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Colon))
                    }
                }
            }
            Some('!') => {
                iter.next();
                match iter.peek() {
                    Some('=') => Some(Token::new(self.line, NEq)),
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Not))
                    }
                }
            }
            Some('=') => {
                iter.next();
                match iter.peek() {
                    Some('=') => Some(Token::new(self.line, Eq)),
                    _ => return Err(format!("L{}: Unexpected '=' character", self.line)),
                }
            }
            Some('>') => {
                iter.next();
                match iter.peek() {
                    Some('=') => Some(Token::new(self.line, GrEq)),
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Gr))
                    }
                }
            }
            Some('<') => {
                iter.next();
                match iter.peek() {
                    Some('=') => Some(Token::new(self.line, LsEq)),
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Ls))
                    }
                }
            }
            Some('-') => {
                iter.next();
                match iter.peek() {
                    Some('>') => Some(Token::new(self.line, LArrow)),
                    _ => {
                        consume = false;
                        Some(Token::new(self.line, Minus))
                    }
                }
            }
            Some('&') => {
                iter.next();
                match iter.peek() {
                    Some('&') => Some(Token::new(self.line, BAnd)),
                    _ => return Err(format!("L{}: Unexpected '-' character", self.line)),
                }
            }
            Some('|') => {
                iter.next();
                match iter.peek() {
                    Some('|') => Some(Token::new(self.line, BOr)),
                    _ => return Err(format!("L{}: Unexpected '-' character", self.line)),
                }
            }
            _ => None,
        };

        if token.is_some() && consume {
            iter.next();
        }
        Ok(token)
    }

    fn consume_line_comment(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) {
        while let Some(c) = iter.next() {
            if c == '\n' {
                break;
            }
        }
    }

    fn consume_block_comment(
        &self,
        iter: &mut Peekable<std::str::Chars>,
    ) {
        while let Some(c) = iter.next() {
            if c == '*' {
                if let Some(c2) = iter.peek() {
                    if *c2 == '/' {
                        iter.next();
                        break;
                    }
                }
            }
        }
    }

    pub fn if_boolean_map(&self, token: Token) -> Token {
        match &token {
            Token {
                l: _,
                s: Identifier(id),
            } => match id.as_str() {
                "true" => Token::new(self.line, Bool(true)),
                "false" => Token::new(self.line, Bool(false)),
                _ => token,
            },
            _ => token,
        }
    }

    pub fn if_primitive_map(&self, token: Token) -> Token {
        match token {
            Token {
                l: _,
                s: Identifier(ref id),
            } => match id.as_str() {
                "i32" => Token::new(self.line, Primitive(Primitive::I32)),
                "bool" => Token::new(self.line, Primitive(Primitive::Bool)),
                "string" => Token::new(self.line, Primitive(Primitive::StringLiteral)),
                _ => Token::new(self.line, Identifier(id.clone())),
            },
            _ => token,
        }
    }

    pub fn if_keyword_map(&self, token: Token) -> Token {
        match token {
            Token {
                l: _,
                s: Identifier(ref id),
            } => match id.as_str() {
                "let" => Token::new(self.line, Let),
                "mut" => Token::new(self.line, Mut),
                "return" => Token::new(self.line, Return),
                "yield" => Token::new(self.line, Yield),
                "yret" => Token::new(self.line, YieldReturn),
                "fn" => Token::new(self.line, FunctionDef),
                "co" => Token::new(self.line, CoroutineDef),
                "struct" => Token::new(self.line, Struct),
                "init" => Token::new(self.line, Init),
                "printi" => Token::new(self.line, Printi),
                "printiln" => Token::new(self.line, Printiln),
                "printbln" => Token::new(self.line, Printbln),
                "if" => Token::new(self.line, If),
                "else" => Token::new(self.line, Else),
                _ => Token::new(self.line, Identifier(id.clone())),
            },
            _ => token,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer() {
        let text = "5";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(text);
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Integer(5)));
    }

    #[test]
    fn test_string_literal() {
        let text = "\"text\"";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(text);
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, StringLiteral("text".into())));
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(token, Token::new(1, Identifier((*text).into())));
        }
    }

    #[test]
    fn test_invalid_number() {
        for text in ["5x"].iter() {
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 2);
            tokens[0]
                .clone()
                .expect_err("Expected error for invalid identifier");
        }
    }

    #[test]
    fn test_operator() {
        for (text, expected_token) in [
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
            (":=", Token::new(1, Assign)),
            (".", Token::new(1, MemberAccess)),
            ("->", Token::new(1, LArrow)),
            (":", Token::new(1, Colon)),
            (",", Token::new(1, Comma)),
            (";", Token::new(1, Semicolon)),
        ]
        .iter()
        {
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i32;->yield";
        let mut lexer = Lexer::new();
        let tokens = lexer.tokenize(text);
        assert_eq!(tokens.len(), 6);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Identifier("x".into())));
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Colon));
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Primitive(Primitive::I32)));
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
            ("struct", Token::new(1, Struct)),
            ("printi", Token::new(1, Printi)),
            ("printiln", Token::new(1, Printiln)),
            ("printbln", Token::new(1, Printbln)),
            ("if", Token::new(1, If)),
            ("else", Token::new(1, Else)),
        ]
        .iter()
        {
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_primitives() {
        for (text, expected_token) in [
            ("i32", Token::new(1, Primitive(Primitive::I32))),
            ("bool", Token::new(1, Primitive(Primitive::Bool))),
            ("string", Token::new(1, Primitive(Primitive::StringLiteral))),
        ]
        .iter()
        {
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
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
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 8);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(*t1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(*t2, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(*t3, Identifier("x".into()))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(*t4, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, Integer(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(*t6, BOr));
            assert_eq!(tokens[6].clone().unwrap(), Token::new(*t7, Bool(true)));
            assert_eq!(tokens[7].clone().unwrap(), Token::new(*t8, RParen));
        }
    }

    #[test]
    fn test_comments() {
        for (text, t1, t2, t3, t4, t5, t6) in [
            ("return ( x + 5 || //true )", 1, 1, 1, 1, 1, 1, ),
            (" return ( x + 5|| /*true )*/ ", 1, 1, 1, 1, 1, 1, ),
            ("return(x+5||///*true)", 1, 1, 1, 1, 1, 1, ),
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
            let mut lexer = Lexer::new();
            let tokens = lexer.tokenize(text);
            assert_eq!(tokens.len(), 6, "{:?}", tokens);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(*t1, Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(*t2, LParen));
            assert_eq!(
                tokens[2].clone().unwrap(),
                Token::new(*t3, Identifier("x".into()))
            );
            assert_eq!(tokens[3].clone().unwrap(), Token::new(*t4, Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, Integer(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(*t6, BOr));
        }
    }
}
