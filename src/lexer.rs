// Token - a type which captures the different types of tokens and which is output
// by tokenize
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Integer(i32),
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
    Print,
    Println,
    Init,
    Yield,
    YieldReturn,
    CoroutineDef,
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
                    let tok = if_keyword_map(id);
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
    let token = match iter.peek() {
        Some('(') => Some(Token::LParen),
        Some(')') => Some(Token::RParen),
        Some('{') => Some(Token::LBrace),
        Some('}') => Some(Token::RBrace),
        Some('*') => Some(Token::Mul),
        Some('+') => Some(Token::Add),
        //Some('=') => Some(Token::Assign),
        Some(';') => Some(Token::Semicolon),
        Some(',') => Some(Token::Comma),
        Some(':') => {
            iter.next();
            match iter.peek() {
                Some('=') => Some(Token::Assign),
                _ => panic!("Lexer: Unexpected character ':'"),
            }
        }
        _ => None,
    };

    if token.is_some() {
        iter.next();
    }
    token
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
            "print" => Token::Print,
            "println" => Token::Println,
            _ => Token::Identifier(id.clone()),
        },
        _ => token,
    }
}

pub mod ops {
    use super::Token;

    #[derive(Debug)]
    pub struct OperatorTrie {
        ch: Option<String>,
        op: Option<Token>,
        children: std::collections::HashMap<String, OperatorTrie>,
    }

    impl OperatorTrie {
        pub fn new(ch: Option<String>, op: Option<Token>) -> OperatorTrie {
            OperatorTrie {
                ch,
                op,
                children: std::collections::HashMap::new(),
            }
        }

        fn add(&mut self, operator: &str, tok: Token) {
            if operator.len() == 0 {
                self.op = Some(tok);
            } else {
                let c = &operator[0..1];
                if let Some(child) = self.children.get_mut(c) {
                    // if yes -> add operator[1..] to the child
                    let rest = &operator[1..];
                    child.add(rest, tok);
                } else {
                    // if no -> create a new child for operator[0] and add operator[1..] to that child
                    let rest = &operator[1..];
                    self.children
                        .insert(c.into(), OperatorTrie::new(Some(c.into()), None));
                    let child = self.children.get_mut(c).unwrap();
                    child.add(rest, tok);
                }
            }
        }

        fn find(&self, operator: &str) -> Option<Token> {
            if operator.len() == 0 {
                self.op.clone()
            } else {
                let c = &operator[0..1];
                self.children
                    .get(c)
                    .and_then(|child| child.find(&operator[1..]))
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_new_trie() {
            {
                let trie = OperatorTrie::new(None, None);
                assert_eq!(trie.ch, None);
                assert_eq!(trie.op, None);
                assert_eq!(trie.children.len(), 0);
            }
            {
                let trie = OperatorTrie::new(Some("+".into()), None);
                assert_eq!(trie.ch, Some("+".into()));
                assert_eq!(trie.op, None);
                assert_eq!(trie.children.len(), 0);
            }
            {
                let trie = OperatorTrie::new(Some("+".into()), Some(Token::Add));
                assert_eq!(trie.ch, Some("+".into()));
                assert_eq!(trie.op, Some(Token::Add));
                assert_eq!(trie.children.len(), 0);
            }
        }

        #[test]
        fn test_add_one_op() {
            {
                let mut trie = OperatorTrie::new(None, None);
                trie.add("+", Token::Add);
                assert_eq!(trie.ch, None);
                assert_eq!(trie.op, None);
                assert_eq!(trie.children.len(), 1);

                assert_eq!(trie.children["+"].ch, Some("+".into()));
                assert_eq!(trie.children["+"].op, Some(Token::Add));
                assert_eq!(trie.children["+"].children.len(), 0);
            }
            {
                let mut trie = OperatorTrie::new(None, None);
                trie.add("*", Token::Mul);
                assert_eq!(trie.ch, None);
                assert_eq!(trie.op, None);
                assert_eq!(trie.children.len(), 1);

                assert_eq!(trie.children["*"].ch, Some("*".into()));
                assert_eq!(trie.children["*"].op, Some(Token::Mul));
                assert_eq!(trie.children["*"].children.len(), 0);
            }
        }

        #[test]
        fn test_add_two_ops() {
            let mut trie = OperatorTrie::new(None, None);
            trie.add("+", Token::Add);
            assert_eq!(trie.ch, None);
            assert_eq!(trie.op, None);
            assert_eq!(trie.children.len(), 1);

            assert_eq!(trie.children["+"].ch, Some("+".into()));
            assert_eq!(trie.children["+"].op, Some(Token::Add));
            assert_eq!(trie.children["+"].children.len(), 0);

            trie.add("*", Token::Mul);
            assert_eq!(trie.ch, None);
            assert_eq!(trie.op, None);
            assert_eq!(trie.children.len(), 2);

            assert_eq!(trie.children["*"].ch, Some("*".into()));
            assert_eq!(trie.children["*"].op, Some(Token::Mul));
            assert_eq!(trie.children["*"].children.len(), 0);
        }

        #[test]
        fn test_add_long_op() {
            let mut trie = OperatorTrie::new(None, None);
            trie.add(":=", Token::Assign);
            assert_eq!(trie.ch, None);
            assert_eq!(trie.op, None);
            assert_eq!(trie.children.len(), 1);

            let child = &trie.children[":"];
            assert_eq!(child.ch, Some(":".into()));
            assert_eq!(child.op, None);
            assert_eq!(child.children.len(), 1);

            assert_eq!(child.children["="].ch, Some("=".into()));
            assert_eq!(child.children["="].op, Some(Token::Assign));
            assert_eq!(child.children["="].children.len(), 0);

            trie.add(":=:", Token::Yield);
            assert_eq!(trie.ch, None);
            assert_eq!(trie.op, None);
            assert_eq!(trie.children.len(), 1);

            let child = &trie.children[":"];
            assert_eq!(child.ch, Some(":".into()));
            assert_eq!(child.op, None);
            assert_eq!(child.children.len(), 1);

            let gchild = &child.children["="];
            assert_eq!(gchild.ch, Some("=".into()));
            assert_eq!(gchild.op, Some(Token::Assign));
            assert_eq!(gchild.children.len(), 1);

            let ggchild = &gchild.children[":"];
            assert_eq!(ggchild.ch, Some(":".into()));
            assert_eq!(ggchild.op, Some(Token::Yield));
            assert_eq!(ggchild.children.len(), 0);

            trie.add(":=>", Token::YieldReturn);
            assert_eq!(trie.ch, None);
            assert_eq!(trie.op, None);
            assert_eq!(trie.children.len(), 1);

            let child = &trie.children[":"];
            assert_eq!(child.ch, Some(":".into()));
            assert_eq!(child.op, None);
            assert_eq!(child.children.len(), 1);

            let gchild = &child.children["="];
            assert_eq!(gchild.ch, Some("=".into()));
            assert_eq!(gchild.op, Some(Token::Assign));
            assert_eq!(gchild.children.len(), 2);

            let ggchild = &gchild.children[">"];
            assert_eq!(ggchild.ch, Some(">".into()));
            assert_eq!(ggchild.op, Some(Token::YieldReturn));
            assert_eq!(ggchild.children.len(), 0);
        }

        #[test]
        fn test_find() {
            let mut trie = OperatorTrie::new(None, None);
            trie.add(":=", Token::Assign);
            trie.add(":=:", Token::Yield);
            trie.add(":=>", Token::YieldReturn);

            assert_eq!(trie.find(":="), Some(Token::Assign));
            assert_eq!(trie.find(":=:"), Some(Token::Yield));
            assert_eq!(trie.find(":=>"), Some(Token::YieldReturn));
            assert_eq!(trie.find(":=="), None);
            assert_eq!(trie.find(":"), None);
            assert_eq!(trie.find(""), None);
        }
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
    fn test_keywords() {
        for (text, expected_token) in [
            ("return", Token::Return),
            ("yield", Token::Yield),
            ("yret", Token::YieldReturn),
            ("init", Token::Init),
            ("co", Token::CoroutineDef),
            ("fn", Token::FunctionDef),
            ("print", Token::Print),
            ("println", Token::Println),
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
