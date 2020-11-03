// Token - a type which captures the different types of tokens and which is output
// by tokenize
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    I32,
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i32),
    Bool(bool),
    Identifier(String),
    Mul,
    Add,
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
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
    Colon,
    LArrow,
    Primitive(Primitive),
}

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
        Some(Token::Identifier(id))
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
                Ok(Some(Token::Integer(num.parse::<i32>().unwrap())))
            }
        } else {
            Ok(Some(Token::Integer(num.parse::<i32>().unwrap())))
        }
    }
}

pub fn consume_operator(iter: &mut Peekable<std::str::Chars>) -> Option<Token> {
    let mut consume = true;
    let token = match iter.peek() {
        Some('(') => Some(Token::LParen),
        Some(')') => Some(Token::RParen),
        Some('{') => Some(Token::LBrace),
        Some('}') => Some(Token::RBrace),
        Some('*') => Some(Token::Mul),
        Some('+') => Some(Token::Add),
        Some(';') => Some(Token::Semicolon),
        Some(',') => Some(Token::Comma),
        Some(':') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::Assign),
                _ => {
                    consume = false;
                    Some(Token::Colon)
                }
            }
        }
        Some('-') => {
            iter.next();
            match iter.peek() {
                Some('>') => Some(Token::LArrow),
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
        Token::Identifier(id) => match id.as_str() {
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => token,
        },
        _ => token,
    }
}

pub fn if_primitive_map(token: Token) -> Token {
    match token {
        Token::Identifier(ref id) => match id.as_str() {
            "i32" => Token::Primitive(Primitive::I32),
            "bool" => Token::Primitive(Primitive::Bool),
            _ => Token::Identifier(id.clone()),
        },
        _ => token,
    }
}

pub fn if_keyword_map(token: Token) -> Token {
    match token {
        Token::Identifier(ref id) => match id.as_str() {
            "return" => Token::Return,
            "yield" => Token::Yield,
            "yret" => Token::YieldReturn,
            "fn" => Token::FunctionDef,
            "co" => Token::CoroutineDef,
            "init" => Token::Init,
            "printi" => Token::Printi,
            "printiln" => Token::Printiln,
            _ => Token::Identifier(id.clone()),
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
        assert_eq!(token, Token::Integer(5));
    }

    #[test]
    fn test_identifier() {
        for text in ["x", "y", "x_5"].iter() {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 1);
            let token = tokens[0].clone().expect("Expected valid token");
            assert_eq!(token, Token::Identifier((*text).into()));
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
            ("*", Token::Mul),
            ("+", Token::Add),
            ("{", Token::LBrace),
            ("}", Token::RBrace),
            ("(", Token::LParen),
            (")", Token::RParen),
            (":=", Token::Assign),
            ("->", Token::LArrow),
            (":", Token::Colon),
            (",", Token::Comma),
            (";", Token::Semicolon),
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
        assert_eq!(token, Token::Identifier("x".into()));
        let token = tokens[1].clone().expect("Expected valid token");
        assert_eq!(token, Token::Colon);
        let token = tokens[2].clone().expect("Expected valid token");
        assert_eq!(token, Token::Primitive(Primitive::I32));
        let token = tokens[3].clone().expect("Expected valid token");
        assert_eq!(token, Token::Semicolon);
        let token = tokens[4].clone().expect("Expected valid token");
        assert_eq!(token, Token::LArrow);
        let token = tokens[5].clone().expect("Expected valid token");
        assert_eq!(token, Token::Yield);
    }

    #[test]
    fn test_keywords() {
        for (text, expected_token) in [
            ("return", Token::Return),
            ("yield", Token::Yield),
            ("yret", Token::YieldReturn),
            ("init", Token::Init),
            ("co", Token::CoroutineDef),
            ("fn", Token::FunctionDef),
            ("printi", Token::Printi),
            ("printiln", Token::Printiln),
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
            ("i32", Token::Primitive(Primitive::I32)),
            ("bool", Token::Primitive(Primitive::Bool)),
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
            "return ( x + 5 )",
            " return ( x + 5 ) ",
            "return(x+5)",
            "return
            (x+
                5
            )",
        ]
        .iter()
        {
            let tokens = tokenize(text);
            assert_eq!(tokens.len(), 6);
            assert_eq!(tokens[0].clone().unwrap(), Token::Return);
            assert_eq!(tokens[1].clone().unwrap(), Token::LParen);
            assert_eq!(tokens[2].clone().unwrap(), Token::Identifier("x".into()));
            assert_eq!(tokens[3].clone().unwrap(), Token::Add);
            assert_eq!(tokens[4].clone().unwrap(), Token::Integer(5));
            assert_eq!(tokens[5].clone().unwrap(), Token::RParen);
        }
    }
}
