// Token - a type which captures the different types of tokens and which is output
// by tokenize
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Integer(i32),
    Bool(bool),
    Identifier(String),
    Mul,
    Add,
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
    Return,
    LParen,
    RParen,
    LBrace,
    RBrace,
    FunctionDef,
    Printi,
    Printiln,
    Printbln,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
    If,
    Else,
    Colon,
    LArrow,
    Primitive(Primitive),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub l: u32,
    pub s: Symbol,
}

impl Token {
    pub fn new(s: Symbol) -> Token {
        Token{
            l: 0,
            s: s,
        }
    }
}

use Symbol::*;

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
pub fn tokenize(text: &str) -> Vec<Result<Token, &str>> {
    let mut tokens = vec![];

    let mut cs = text.chars().peekable();

    while cs.peek().is_some() {
        consume_whitespace(&mut cs);
        if cs.peek().is_none() {
            break;
        }
        match consume_integer(&mut cs) {
            Ok(Some(i)) => tokens.push(Ok(i)),
            Ok(None) => match consume_identifier(&mut cs) {
                Some(id) => {
                    let tok = if_primitive_map(if_keyword_map(if_boolean_map(id)));
                    tokens.push(Ok(tok));
                }
                None => match consume_operator(&mut cs) {
                    Some(op) => tokens.push(Ok(op)),
                    None => println!("Unexpected character: {:?}", cs.next()),
                },
            },
            Err(msg) => {
                tokens.push(Err(msg));
                break;
            }
        }
    }

    tokens
}

pub fn consume_whitespace(iter: &mut Peekable<std::str::Chars>) {
    while iter.peek().map_or_else(|| false, |c| c.is_whitespace()) {
        iter.next();
    }
}

pub fn consume_identifier(iter: &mut Peekable<std::str::Chars>) -> Option<Token> {
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
        None
    } else {
        Some(Token::new(Symbol::Identifier(id)))
    }
}

pub fn consume_integer(
    iter: &mut Peekable<std::str::Chars>,
) -> Result<Option<Token>, &'static str> {
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
                Err("Invalid integer, should not contain characters")
            } else {
                Ok(Some(Token::new(Integer(num.parse::<i32>().unwrap()))))
            }
        } else {
            Ok(Some(Token::new(Integer(num.parse::<i32>().unwrap()))))
        }
    }
}

pub fn consume_operator(iter: &mut Peekable<std::str::Chars>) -> Option<Token> {
    let mut consume = true;
    let token = match iter.peek() {
        Some('(') => Some(Token::new(LParen)),
        Some(')') => Some(Token::new(RParen)),
        Some('{') => Some(Token::new(LBrace)),
        Some('}') => Some(Token::new(RBrace)),
        Some('*') => Some(Token::new(Mul)),
        Some('+') => Some(Token::new(Add)),
        Some(';') => Some(Token::new(Semicolon)),
        Some(',') => Some(Token::new(Comma)),
        Some(':') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::new(Assign)),
                _ => {
                    consume = false;
                    Some(Token::new(Colon))
                }
            }
        }
        Some('!') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::new(NEq)),
                _ => panic!("Lexer: Unexpected '!' character"),
            }
        }
        Some('=') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::new(Eq)),
                _ => panic!("Lexer: Unexpected '=' character"),
            }
        }
        Some('>') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::new(GrEq)),
                _ => {
                    consume = false;
                    Some(Token::new(Gr))
                }
            }
        }
        Some('<') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::new(LsEq)),
                _ => {
                    consume = false;
                    Some(Token::new(Ls))
                }
            }
        }
        Some('-') => {
            iter.next();
            match iter.peek() {
                Some('>') => Some(Token::new(LArrow)),
                _ => panic!("Lexer: Unexpected '-' character"),
            }
        }
        Some('&') => {
            iter.next();
            match iter.peek() {
                Some('&') => Some(Token::new(BAnd)),
                _ => panic!("Lexer: Unexpected '-' character"),
            }
        }
        Some('|') => {
            iter.next();
            match iter.peek() {
                Some('|') => Some(Token::new(BOr)),
                _ => panic!("Lexer: Unexpected '-' character"),
            }
        }
        _ => None,
    };

    if token.is_some() && consume {
        iter.next();
    }
    token
}

pub fn if_boolean_map(token: Token) -> Token {
    match &token {
        Token{l:_, s: Identifier(id)} => match id.as_str() {
            "true" => Token::new(Bool(true)),
            "false" => Token::new(Bool(false)),
            _ => token,
        },
        _ => token,
    }
}

pub fn if_primitive_map(token: Token) -> Token {
    match token {
        Token{l:_, s: Identifier(ref id)} => match id.as_str() {
            "i32" => Token::new(Primitive(Primitive::I32)),
            "bool" => Token::new(Primitive(Primitive::Bool)),
            _ => Token::new(Identifier(id.clone())),
        },
        _ => token,
    }
}

pub fn if_keyword_map(token: Token) -> Token {
    match token {
        Token{l:_, s: Identifier(ref id)} => match id.as_str() {
            "return" => Token::new(Return),
            "yield" => Token::new(Yield),
            "yret" => Token::new(YieldReturn),
            "fn" => Token::new(FunctionDef),
            "co" => Token::new(CoroutineDef),
            "init" => Token::new(Init),
            "printi" => Token::new(Printi),
            "printiln" => Token::new(Printiln),
            "printbln" => Token::new(Printbln),
            "if" => Token::new(If),
            "else" => Token::new(Else),
            _ => Token::new(Identifier(id.clone())),
        },
        _ => token,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_integer() {
        let text = "5";
        let tokens = tokenize(text);
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Integer(5)));
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(token, Token::new(Identifier((*text).into())));
        }
    }

    #[test]
    fn test_invalid_number() {
        for text in ["5x"].iter() {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            tokens[0]
                .clone()
                .expect_err("Expected error for invalid identifier");
        }
    }

    #[test]
    fn test_operator() {
        for (text, expected_token) in [
            ("*", Token::new(Mul)),
            ("+", Token::new(Add)),
            ("&&", Token::new(BAnd)),
            ("||", Token::new(BOr)),
            (">", Token::new(Gr)),
            (">=", Token::new(GrEq)),
            ("<", Token::new(Ls)),
            ("<=", Token::new(LsEq)),
            ("==", Token::new(Eq)),
            ("!=", Token::new(NEq)),
            ("{", Token::new(LBrace)),
            ("}", Token::new(RBrace)),
            ("(", Token::new(LParen)),
            (")", Token::new(RParen)),
            (":=", Token::new(Assign)),
            ("->", Token::new(LArrow)),
            (":", Token::new(Colon)),
            (",", Token::new(Comma)),
            (";", Token::new(Semicolon)),
        ]
        .iter()
        {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i32;->yield";
        let tokens = tokenize(text);
        assert_eq!(tokens.len(), 6);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Identifier("x".into())));
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Colon));
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Primitive(Primitive::I32)));
        let token = tokens[3].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Semicolon));
        let token = tokens[4].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(LArrow));
        let token = tokens[5].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(Yield));
    }

    #[test]
    fn test_keywords() {
        for (text, expected_token) in [
            ("return", Token::new(Return)),
            ("yield", Token::new(Yield)),
            ("yret", Token::new(YieldReturn)),
            ("init", Token::new(Init)),
            ("co", Token::new(CoroutineDef)),
            ("fn", Token::new(FunctionDef)),
            ("printi", Token::new(Printi)),
            ("printiln", Token::new(Printiln)),
            ("printbln", Token::new(Printbln)),
            ("if", Token::new(If)),
            ("else", Token::new(Else)),
        ]
        .iter()
        {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_primitives() {
        for (text, expected_token) in [
            ("i32", Token::new(Primitive(Primitive::I32))),
            ("bool", Token::new(Primitive(Primitive::Bool))),
        ]
        .iter()
        {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone().unwrap(), *expected_token);
        }
    }

    #[test]
    fn test_whitespace_handling() {
        for text in [
            "return ( x + 5 || true )",
            " return ( x + 5|| true ) ",
            "return(x+5||true)",
            "return
            (x+
                5
                ||
                true
            )",
        ]
        .iter()
        {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 8);
            assert_eq!(tokens[0].clone().unwrap(), Token::new(Return));
            assert_eq!(tokens[1].clone().unwrap(), Token::new(LParen));
            assert_eq!(tokens[2].clone().unwrap(), Token::new(Identifier("x".into())));
            assert_eq!(tokens[3].clone().unwrap(), Token::new(Add));
            assert_eq!(tokens[4].clone().unwrap(), Token::new(Integer(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(BOr));
            assert_eq!(tokens[6].clone().unwrap(), Token::new(Bool(true)));
            assert_eq!(tokens[7].clone().unwrap(), Token::new(RParen));
        }
    }
}
