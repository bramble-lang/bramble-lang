use braid_lang::result::Result;
// Token - a type which captures the different types of tokens and which is output
// by tokenize
use stdext::function_name;

use crate::diagnostics::config::TracingConfig;

use super::tokens::{Lex, Primitive, Token};
use Lex::*;

macro_rules! trace {
    ($ts:expr) => {
        let print_trace = match &$ts.tracing {
            &TracingConfig::Only(ln) => ($ts.line() as usize) == ln,
            &TracingConfig::Before(ln) => ($ts.line() as usize) <= ln,
            &TracingConfig::After(ln) => ($ts.line() as usize) >= ln,
            &TracingConfig::Between(start, end) => {
                ($ts.line() as usize) >= start && ($ts.line() as usize) <= end
            }
            &TracingConfig::All => true,
            &TracingConfig::Off => false,
        };
        if print_trace {
            println!(
                "{} <- L{}:{:?}",
                function_name!(),
                $ts.line(),
                $ts.current_token()
            )
        }
    };
}

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
        let s = self.cut();

        self.lexer.index = self.index;
        self.lexer.line = self.line;

        s
    }

    /// Cuts a string from the current branch from the last
    /// branch or cut point up to where the cursor currently is.
    /// Then advance the start point of the next token in this
    /// branch.  This will NOT update the source.  That must be
    /// done with `merge`.
    pub fn cut(&mut self) -> String {
        let start = self.lexer.index;
        let stop = self.index;
        let mut s = String::new();

        for i in start..stop {
            s.push(self.lexer.chars[i]);
        }

        s
    }

    pub fn next(&mut self) -> Option<char> {
        if self.index < self.lexer.chars.len() {
            let c = self.lexer.chars[self.index];
            self.index += 1;
            if c == '\n' {
                self.line += 1;
            }
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
    tracing: TracingConfig,
}

impl Lexer {
    pub fn new(text: &str) -> Lexer {
        Lexer {
            chars: text.chars().collect(),
            index: 0,
            line: 1,
            tracing: TracingConfig::Off,
        }
    }

    pub fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }

    pub fn line(&self) -> u32 {
        self.line as u32
    }

    pub fn current_token(&self) -> Option<char> {
        if self.index < self.chars.len() {
            Some(self.chars[self.index])
        } else {
            None
        }
    }

    pub fn tokenize(&mut self) -> Vec<Result<Token>> {
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
                tokens.push(Err(format!(
                    "The lexer is locked, at {:?}, and cannot proceed",
                    self.current_token()
                )));
                break;
            }
        }

        tokens
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
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

    fn consume_literal(&mut self) -> Result<Option<Token>> {
        trace!(self);
        match self.consume_integer()? {
            Some(i) => Ok(Some(i)),
            None => match self.consume_string_literal()? {
                Some(s) => Ok(Some(s)),
                None => Ok(None),
            },
        }
    }

    pub fn consume_whitespace(&mut self) {
        trace!(self);
        while self.index < self.chars.len() && self.chars[self.index].is_whitespace() {
            if self.chars[self.index] == '\n' {
                self.line += 1;
            }
            self.index += 1;
        }
    }

    pub fn consume_identifier(&mut self) -> Result<Option<Token>> {
        trace!(self);
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

    fn consume_string_literal(&mut self) -> Result<Option<Token>> {
        trace!(self);
        let mut branch = LexerBranch::from(self);

        if branch.next_if('"') {
            while let Some(c) = branch.next() {
                if c == '"' {
                    break;
                }

                // Parse escape sequence
                if c == '\\' {
                    match branch.next() {
                        Some(c) if Self::is_escape_code(c) => (),
                        Some(c) => return Err(format!("Invalid escape sequence \\{}", c)),
                        None => return Err("Expected escape character after \\".into()),
                    }
                }
            }
            let mut s = branch.merge();

            // Remove the quotes from the string
            s.remove(0);
            s.pop();

            Ok(Some(Token::new(self.line, Lex::StringLiteral(s))))
        } else {
            Ok(None)
        }
    }

    pub fn consume_integer(&mut self) -> Result<Option<Token>> {
        trace!(self);
        let mut branch = LexerBranch::from(self);

        if !branch.peek().map_or(false, |c| c.is_numeric()) {
            return Ok(None);
        }

        // read until a non-digit is hit
        while let Some(c) = branch.peek() {
            if !c.is_numeric() {
                break;
            }
            branch.next();
        }

        let int_token = branch.cut();

        // Check if there is a postfix (i32, i64, etc) on the integer literal if there is
        // no suffix then default to i64
        let type_suffix = Self::consume_int_suffix(&mut branch)?.unwrap_or(Primitive::I64);

        // Check that the current character at the lexer cursor position is a delimiter (we have
        // reached the end of the token); otherwise this is not a valid integer literal and an
        // error should be thrown.
        if branch
            .peek()
            .map(|c| !Self::is_delimiter(c))
            .unwrap_or(false)
        {
            return Err(format!(
                "L{}: Invalid integer, should not contain characters",
                self.line
            ));
        }

        branch.merge();
        Self::create_int_literal(self.line, &int_token, type_suffix)
    }

    fn create_int_literal(line: u32, int_token: &str, prim: Primitive) -> Result<Option<Token>> {
        match prim {
            Primitive::U8 => Ok(Some(Token::new(line, U8(int_token.parse::<u8>().unwrap())))),
            Primitive::U16 => Ok(Some(Token::new(
                line,
                U16(int_token.parse::<u16>().unwrap()),
            ))),
            Primitive::U32 => Ok(Some(Token::new(
                line,
                U32(int_token.parse::<u32>().unwrap()),
            ))),
            Primitive::U64 => Ok(Some(Token::new(
                line,
                U64(int_token.parse::<u64>().unwrap()),
            ))),
            Primitive::I8 => Ok(Some(Token::new(line, I8(int_token.parse::<i8>().unwrap())))),
            Primitive::I16 => Ok(Some(Token::new(
                line,
                I16(int_token.parse::<i16>().unwrap()),
            ))),
            Primitive::I32 => Ok(Some(Token::new(
                line,
                I32(int_token.parse::<i32>().unwrap()),
            ))),
            Primitive::I64 => Ok(Some(Token::new(
                line,
                I64(int_token.parse::<i64>().unwrap()),
            ))),
            Primitive::Bool | Primitive::StringLiteral => {
                Err(format!("Unexpected primitive type after number: {}", prim))
            }
        }
    }

    fn consume_int_suffix(branch: &mut LexerBranch) -> Result<Option<Primitive>> {
        Ok(if branch.next_ifn("i8") {
            Some(Primitive::I8)
        } else if branch.next_ifn("i16") {
            Some(Primitive::I16)
        } else if branch.next_ifn("i32") {
            Some(Primitive::I32)
        } else if branch.next_ifn("i64") {
            Some(Primitive::I64)
        } else if branch.next_ifn("u8") {
            Some(Primitive::U8)
        } else if branch.next_ifn("u16") {
            Some(Primitive::U16)
        } else if branch.next_ifn("u32") {
            Some(Primitive::U32)
        } else if branch.next_ifn("u64") {
            Some(Primitive::U64)
        } else {
            None
        })
    }

    pub fn consume_operator(&mut self) -> Result<Option<Token>> {
        trace!(self);
        let line = self.line;
        let mut branch = LexerBranch::from(self);
        let mut operators = vec![
            ("...", VarArgs),
            ("->", LArrow),
            ("&&", BAnd),
            ("||", BOr),
            ("::", PathSeparator),
            (":=", Assign),
            ("!=", NEq),
            ("==", Eq),
            (">=", GrEq),
            ("<=", LsEq),
            ("(", LParen),
            (")", RParen),
            ("{", LBrace),
            ("}", RBrace),
            ("[", LBracket),
            ("]", RBracket),
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
        trace!(self);
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
        trace!(self);
        let mut branch = LexerBranch::from(self);
        if branch.next_ifn("/*") {
            while !branch.next_ifn("*/") {
                branch.next();
            }
        }
        branch.merge();
    }

    pub fn if_boolean_map(&self, token: Token) -> Token {
        trace!(self);
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
        trace!(self);
        match token {
            Token {
                l: _,
                s: Identifier(ref id),
            } => match id.as_str() {
                "u8" => Token::new(self.line, Primitive(Primitive::U8)),
                "u16" => Token::new(self.line, Primitive(Primitive::U16)),
                "u32" => Token::new(self.line, Primitive(Primitive::U32)),
                "u64" => Token::new(self.line, Primitive(Primitive::U64)),
                "i8" => Token::new(self.line, Primitive(Primitive::I8)),
                "i16" => Token::new(self.line, Primitive(Primitive::I16)),
                "i32" => Token::new(self.line, Primitive(Primitive::I32)),
                "i64" => Token::new(self.line, Primitive(Primitive::I64)),
                "bool" => Token::new(self.line, Primitive(Primitive::Bool)),
                "string" => Token::new(self.line, Primitive(Primitive::StringLiteral)),
                _ => Token::new(self.line, Identifier(id.clone())),
            },
            _ => token,
        }
    }

    pub fn if_keyword_map(&self, token: Token) -> Token {
        trace!(self);
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
                "mod" => Token::new(self.line, ModuleDef),
                "struct" => Token::new(self.line, Struct),
                "extern" => Token::new(self.line, Extern),
                "init" => Token::new(self.line, Init),
                "if" => Token::new(self.line, If),
                "else" => Token::new(self.line, Else),
                "while" => Token::new(self.line, While),
                _ => Token::new(self.line, Identifier(id.clone())),
            },
            _ => token,
        }
    }

    fn is_delimiter(c: char) -> bool {
        c.is_ascii_punctuation() || c.is_whitespace()
    }

    fn is_escape_code(c: char) -> bool {
        c == 'n' || c == 'r' || c == 't' || c == '"' || c == '0' || c == '\\'
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
