//use crate::result::Result;
// Token - a type which captures the different types of tokens and which is output
// by tokenize
use stdext::function_name;

use crate::compiler::LexerResult;
use crate::diagnostics::config::TracingConfig;

use super::super::CompilerError;
use super::{
    stringtable::{StringId, StringTable},
    tokens::{Lex, Primitive, Token},
    LexerError,
};
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

macro_rules! err {
    ($ln: expr, $kind: expr) => {
        Err(CompilerError::new($ln, $kind))
    };
}

struct LexerBranch<'a, 'st> {
    lexer: &'a mut Lexer<'st>,
    index: usize,
    line: u32,
    offset: u32,
}

impl<'a, 'st> LexerBranch<'a, 'st> {
    pub fn from(l: &'a mut Lexer<'st>) -> LexerBranch<'a, 'st> {
        LexerBranch {
            index: l.index,
            line: l.line,
            offset: l.col,
            lexer: l,
        }
    }

    pub fn merge(&mut self) -> (Option<StringId>, u32) {
        let s = self.cut();

        self.lexer.index = self.index;
        self.lexer.line = self.line;
        self.lexer.col = self.offset;

        s
    }

    /// Cuts a string from the current branch from the last
    /// branch or cut point up to where the cursor currently is.
    /// Then advance the start point of the next token in this
    /// branch.  This will NOT update the source.  That must be
    /// done with `merge`.
    pub fn cut(&mut self) -> (Option<StringId>, u32) {
        let start = self.lexer.index;
        let offset = self.lexer.col;
        let stop = self.index;
        let mut s = String::new();

        for i in start..stop {
            s.push(self.lexer.chars[i]);
        }

        let id = if s.len() == 0 {
            None
        } else {
            Some(self.lexer.string_table.insert(s))
        };

        (id, offset)
    }

    pub fn next(&mut self) -> Option<char> {
        if self.index < self.lexer.chars.len() {
            let c = self.lexer.chars[self.index];
            self.index += 1;
            self.offset += 1;
            if c == '\n' {
                self.line += 1;
                self.offset = 0;
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
        // TODO: What if `t` contains a line break?
        if self.peek_ifn(t) {
            self.index += t.len();
            self.offset += t.len() as u32;
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

pub struct Lexer<'a> {
    chars: Vec<char>,
    index: usize,
    line: u32,
    col: u32,
    tracing: TracingConfig,
    string_table: &'a mut StringTable,
}

impl<'a> Lexer<'a> {
    pub fn new(string_table: &'a mut StringTable, text: &str) -> Lexer<'a> {
        Lexer {
            chars: text.chars().collect(),
            index: 0,
            line: 1,
            col: 0,
            tracing: TracingConfig::Off,
            string_table,
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

    pub fn tokenize(&mut self) -> Vec<LexerResult<Token>> {
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
                tokens.push(err!(self.line(), LexerError::Locked(self.current_token())));
                break;
            }
        }

        tokens
    }

    fn next_token(&mut self) -> LexerResult<Option<Token>> {
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

    fn consume_literal(&mut self) -> LexerResult<Option<Token>> {
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
                self.col = 0;
            }
            self.index += 1;
            self.col += 1;
        }
    }

    pub fn consume_identifier(&mut self) -> LexerResult<Option<Token>> {
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

        let (id, offset) = branch.merge();
        match id {
            None => Ok(None),
            Some(id) => Ok(Some(Token::new(self.line, offset, Lex::Identifier(id)))),
        }
    }

    fn consume_string_literal(&mut self) -> LexerResult<Option<Token>> {
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
                        Some(c) => return err!(self.line(), LexerError::InvalidEscapeSequence(c)),

                        None => return err!(self.line(), LexerError::ExpectedEscapeCharacter),
                    }
                }
            }
            let (s, offset) = branch.merge();

            // Remove the quotes from the string
            let mut s: String = self.string_table.get(s.unwrap()).unwrap().into();
            s.remove(0);
            s.pop();
            let id = self.string_table.insert(s);

            Ok(Some(Token::new(self.line, offset, Lex::StringLiteral(id))))
        } else {
            Ok(None)
        }
    }

    pub fn consume_integer(&mut self) -> LexerResult<Option<Token>> {
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

        let (int_token, offset) = branch.cut();

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
            return err!(self.line(), LexerError::InvalidInteger);
        }

        branch.merge();
        let int_text = self.string_table.get(int_token.unwrap()).unwrap();
        Self::create_int_literal(self.line, offset, int_text, type_suffix)
    }

    fn create_int_literal(
        line: u32,
        offset: u32,
        int_token: &str,
        prim: Primitive,
    ) -> LexerResult<Option<Token>> {
        match prim {
            Primitive::U8 => Ok(Some(Token::new(
                line,
                offset,
                U8(int_token.parse::<u8>().unwrap()),
            ))),
            Primitive::U16 => Ok(Some(Token::new(
                line,
                offset,
                U16(int_token.parse::<u16>().unwrap()),
            ))),
            Primitive::U32 => Ok(Some(Token::new(
                line,
                offset,
                U32(int_token.parse::<u32>().unwrap()),
            ))),
            Primitive::U64 => Ok(Some(Token::new(
                line,
                offset,
                U64(int_token.parse::<u64>().unwrap()),
            ))),
            Primitive::I8 => Ok(Some(Token::new(
                line,
                offset,
                I8(int_token.parse::<i8>().unwrap()),
            ))),
            Primitive::I16 => Ok(Some(Token::new(
                line,
                offset,
                I16(int_token.parse::<i16>().unwrap()),
            ))),
            Primitive::I32 => Ok(Some(Token::new(
                line,
                offset,
                I32(int_token.parse::<i32>().unwrap()),
            ))),
            Primitive::I64 => Ok(Some(Token::new(
                line,
                offset,
                I64(int_token.parse::<i64>().unwrap()),
            ))),
            Primitive::Bool | Primitive::StringLiteral => {
                err!(line, LexerError::UnexpectedSuffixType(prim))
            }
        }
    }

    fn consume_int_suffix(branch: &mut LexerBranch) -> LexerResult<Option<Primitive>> {
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

    pub fn consume_operator(&mut self) -> LexerResult<Option<Token>> {
        trace!(self);
        let line = self.line;
        let offset = self.col;
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
                token = Some(Token::new(line, offset, t.clone()));
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
                l,
                c: o,
                s: Identifier(id),
            } => match self.string_table.get(*id).unwrap() {
                "true" => Token::new(*l, *o, Bool(true)),
                "false" => Token::new(*l, *o, Bool(false)),
                _ => token,
            },
            _ => token,
        }
    }

    pub fn if_primitive_map(&self, token: Token) -> Token {
        trace!(self);
        match token {
            Token {
                l,
                c: o,
                s: Identifier(ref id),
            } => match self.string_table.get(*id).unwrap() {
                "u8" => Token::new(l, o, Primitive(Primitive::U8)),
                "u16" => Token::new(l, o, Primitive(Primitive::U16)),
                "u32" => Token::new(l, o, Primitive(Primitive::U32)),
                "u64" => Token::new(l, o, Primitive(Primitive::U64)),
                "i8" => Token::new(l, o, Primitive(Primitive::I8)),
                "i16" => Token::new(l, o, Primitive(Primitive::I16)),
                "i32" => Token::new(l, o, Primitive(Primitive::I32)),
                "i64" => Token::new(l, o, Primitive(Primitive::I64)),
                "bool" => Token::new(l, o, Primitive(Primitive::Bool)),
                "string" => Token::new(l, o, Primitive(Primitive::StringLiteral)),
                _ => Token::new(l, o, Identifier(id.clone())),
            },
            _ => token,
        }
    }

    pub fn if_keyword_map(&self, token: Token) -> Token {
        trace!(self);
        match token {
            Token {
                l,
                c: o,
                s: Identifier(ref id),
            } => match self.string_table.get(*id).unwrap() {
                "let" => Token::new(l, o, Let),
                "mut" => Token::new(l, o, Mut),
                "return" => Token::new(l, o, Return),
                "yield" => Token::new(l, o, Yield),
                "yret" => Token::new(l, o, YieldReturn),
                "fn" => Token::new(l, o, FunctionDef),
                "co" => Token::new(l, o, CoroutineDef),
                "mod" => Token::new(l, o, ModuleDef),
                "struct" => Token::new(l, o, Struct),
                "extern" => Token::new(l, o, Extern),
                "init" => Token::new(l, o, Init),
                "if" => Token::new(l, o, If),
                "else" => Token::new(l, o, Else),
                "while" => Token::new(l, o, While),
                "self" => Token::new(l, o, PathSelf),
                "super" => Token::new(l, o, PathSuper),
                "root" => Token::new(l, o, PathFileRoot),
                "project" => Token::new(l, o, PathProjectRoot),
                _ => Token::new(l, o, Identifier(id.clone())),
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
