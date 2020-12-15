// Token - a type which captures the different types of tokens and which is output
// by tokenize
use super::tokens::{Lex, Primitive, Token};
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

struct LexerBranch<'a> {
    lexer: &'a mut Lexer,
    index: usize,
    line: u32,
}

impl<'a> LexerBranch<'a> {
    pub fn from(l: &mut Lexer) -> LexerBranch {
        LexerBranch {
            index: l.index,
            line: l.line,
            lexer: l,
        }
    }

    pub fn merge(&mut self) -> String {
        let start = self.lexer.index;
        let stop = self.index;
        let mut s = String::new();

        for i in start..stop {
            s.push(self.lexer.chars[i]);
        }

        self.lexer.index = self.index;
        self.lexer.line = self.line;

        s
    }

    pub fn next(&mut self) -> Option<char> {
        if self.index < self.lexer.chars.len() {
            let c = self.lexer.chars[self.index];
            self.index += 1;
            Some(c)
        } else {
            None
        }
    }

    pub fn next_if(&mut self, t: char) -> bool {
        match self.peek() {
            None => false,
            Some(c) => {
                if c == t {
                    self.next().is_some()
                } else {
                    false
                }
            }
        }
    }

    pub fn next_ifn(&mut self, t: &str) -> bool {
        if self.peek_ifn(t) {
            self.index += t.len();
            true
        } else {
            false
        }
    }

    pub fn peek(&self) -> Option<char> {
        if self.index < self.lexer.chars.len() {
            Some(self.lexer.chars[self.index])
        } else {
            None
        }
    }

    pub fn peek_at(&self, i: usize) -> Option<char> {
        if self.index + i < self.lexer.chars.len() {
            Some(self.lexer.chars[self.index + i])
        } else {
            None
        }
    }

    pub fn peek_if(&self, t: char) -> bool {
        match self.peek() {
            None => false,
            Some(c) => t == c,
        }
    }

    pub fn peek_ifn(&self, t: &str) -> bool {
        let tc: Vec<char> = t.chars().collect();
        self.lexer.chars[self.index..].starts_with(&tc)
    }
}
pub struct Lexer {
    chars: Vec<char>,
    index: usize,
    line: u32,
}

impl Lexer {
    pub fn new(text: &str) -> Lexer {
        Lexer {
            chars: text.chars().collect(),
            index: 0,
            line: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Result<Token, String>> {
        let mut tokens = vec![];

        while self.index < self.chars.len() {
            let prev_index = self.index;
            self.consume_whitespace();
            if self.index >= self.chars.len() {
                break;
            }
            match self.next_token() {
                Ok(Some(t)) => tokens.push(Ok(t)),
                Ok(None) => (),
                Err(msg) => tokens.push(Err(msg)),
            }

            // Can no longer consume the input text
            if prev_index == self.index {
                tokens.push(Err("The lexer is locked and cannot proceed".into()));
                break;
            }
        }

        tokens
    }

    fn next_token(&mut self) -> Result<Option<Token>, String> {
        self.consume_line_comment();
        self.consume_block_comment();

        match self.consume_literal()? {
            Some(i) => Ok(Some(i)),
            None => match self.consume_identifier()? {
                Some(id) => {
                    let tok = self.if_primitive_map(self.if_keyword_map(self.if_boolean_map(id)));
                    Ok(Some(tok))
                }
                None => match self.consume_operator()? {
                    Some(op) => Ok(Some(op)),
                    None => Ok(None),
                },
            },
        }
    }

    fn consume_literal(&mut self) -> Result<Option<Token>, String> {
        match self.consume_integer()? {
            Some(i) => Ok(Some(i)),
            None => match self.consume_string_literal()? {
                Some(s) => Ok(Some(s)),
                None => Ok(None),
            },
        }
    }

    pub fn consume_whitespace(&mut self) {
        while self.index < self.chars.len() && self.chars[self.index].is_whitespace() {
            if self.chars[self.index] == '\n' {
                self.line += 1;
            }
            self.index += 1;
        }
    }

    pub fn consume_identifier(&mut self) -> Result<Option<Token>, String> {
        let mut branch = LexerBranch::from(self);
        if branch
            .peek()
            .map_or_else(|| false, |c| c.is_alphabetic() || c == '_')
        {
            while branch
                .peek()
                .map_or_else(|| false, |c| c.is_alphanumeric() || c == '_')
            {
                match branch.next() {
                    Some(_) => (),
                    None => break,
                }
            }
        }

        let id = branch.merge();
        if id.len() == 0 {
            Ok(None)
        } else {
            Ok(Some(Token::new(self.line, Lex::Identifier(id))))
        }
    }

    fn consume_string_literal(&mut self) -> Result<Option<Token>, String> {
        let mut branch = LexerBranch::from(self);

        if branch.next_if('"') {
            while let Some(c) = branch.next() {
                if c == '"' {
                    break;
                }
            }
            let mut s = branch.merge();
            s.remove(0);
            s.pop();

            Ok(Some(Token::new(self.line, Lex::StringLiteral(s))))
        } else {
            Ok(None)
        }
    }

    pub fn consume_integer(&mut self) -> Result<Option<Token>, String> {
        let mut branch = LexerBranch::from(self);

        if !branch.peek().map_or(false, |c| c.is_numeric()) {
            return Ok(None);
        }

        while let Some(c) = branch.peek() {
            if !c.is_numeric() {
                if c.is_alphabetic() || c == '_' {
                    return Err(format!(
                        "L{}: Invalid integer, should not contain characters",
                        self.line
                    ));
                } else {
                    break;
                }
            }
            branch.next();
        }

        let num = branch.merge();
        Ok(Some(Token::new(
            self.line,
            Integer(num.parse::<i32>().unwrap()),
        )))
    }

    pub fn consume_operator(&mut self) -> Result<Option<Token>, String> {
        let line = self.line;
        let mut branch = LexerBranch::from(self);
        let mut operators = vec![
            ("->", LArrow),
            ("&&", BAnd),
            ("||", BOr),
            (":=", Assign),
            ("!=", NEq),
            ("==", Eq),
            (">=", GrEq),
            ("<=", LsEq),
            ("(", LParen),
            (")", RParen),
            ("{", LBrace),
            ("}", RBrace),
            ("*", Mul),
            ("/", Div),
            ("+", Add),
            (";", Semicolon),
            (":", Colon),
            (",", Comma),
            (".", MemberAccess),
            (">", Gr),
            ("<", Ls),
            ("-", Minus),
            ("!", Not),
        ];
        operators.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

        let mut token = None;
        for (op, t) in operators.iter() {
            if branch.next_ifn(op) {
                token = Some(Token::new(line, t.clone()));
                break;
            }
        }
        branch.merge();
        Ok(token)
    }

    fn consume_line_comment(&mut self) {
        let mut branch = LexerBranch::from(self);
        if branch.next_ifn("//") {
            while let Some(c) = branch.next() {
                if c == '\n' {
                    break;
                }
            }
        }
        branch.merge();
    }

    fn consume_block_comment(&mut self) {
        let mut branch = LexerBranch::from(self);
        if branch.next_ifn("/*") {
            while !branch.next_ifn("*/") {
                branch.next();
            }
        }
        branch.merge();
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
                "prints" => Token::new(self.line, Prints),
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
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
        assert_eq!(tokens.len(), 1);
        let token = tokens[0].clone().expect("Expected valid token");
        assert_eq!(token, Token::new(1, Integer(5)));
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
            let mut lexer = Lexer::new(text);
            let tokens = lexer.tokenize();
            assert_eq!(tokens.len(), 1);
            assert_eq!(tokens[0].clone(), Ok(expected_token.clone()), "{}", text);
        }
    }

    #[test]
    fn test_multiple_tokens() {
        let text = "x:i32;->yield";
        let mut lexer = Lexer::new(text);
        let tokens = lexer.tokenize();
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
            ("prints", Token::new(1, Prints)),
            ("printbln", Token::new(1, Printbln)),
            ("if", Token::new(1, If)),
            ("else", Token::new(1, Else)),
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
            ("i32", Token::new(1, Primitive(Primitive::I32))),
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
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, Integer(5)));
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
            assert_eq!(tokens[4].clone().unwrap(), Token::new(*t5, Integer(5)));
            assert_eq!(tokens[5].clone().unwrap(), Token::new(*t6, BOr));
        }
    }
}
