// Token - a type which captures the different types of tokens and which is output
// by tokenize
use stdext::function_name;

use crate::compiler::diagnostics::{Event, Logger};
use crate::compiler::source::{Offset, Source};
use crate::compiler::{SourceChar, Span};
use crate::diagnostics::config::TracingConfig;
use crate::{StringId, StringTable};

use super::super::CompilerError;
use super::LexerResult;
use super::{
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
                $ts.current_char()
            )
        }
    };
}

struct LexerBranch<'a, 'st> {
    lexer: &'a mut Lexer<'st>,
    index: usize,
    line: u32,
}

impl<'a, 'st> LexerBranch<'a, 'st> {
    fn from(l: &'a mut Lexer<'st>) -> LexerBranch<'a, 'st> {
        LexerBranch {
            index: l.index,
            line: l.line,
            lexer: l,
        }
    }

    /// Merges this branch back into it's source Lexer.  Merging as the affect
    /// of accepting the current branch as correct and updating the source lexer
    /// to the match the cursor state of the branch.
    fn merge(mut self) -> Option<(StringId, Span)> {
        self.cut().and_then(|cut| {
            self.lexer.index = self.index;
            self.lexer.line = self.line;
            Some(cut)
        })
    }

    /// Cuts a string from the current branch from the last
    /// branch or cut point up to where the cursor currently is.
    /// Then advance the start point of the next token in this
    /// branch.  This will NOT update the source.  That must be
    /// done with `merge`.
    fn cut(&mut self) -> Option<(StringId, Span)> {
        let start = self.lexer.index;
        let stop = self.index;
        let mut s = String::new();

        for i in start..stop {
            s.push(self.lexer.chars[i].char());
        }

        if s.len() == 0 {
            None
        } else {
            // Compute the Span
            let low = self.lexer.chars[start].offset();
            let high = if stop < self.lexer.chars.len() {
                self.lexer.chars[stop].offset()
            } else {
                self.lexer.end_offset
            };
            let span = Span::new(low, high);

            Some((self.lexer.string_table.insert(s), span))
        }
    }

    /// Advances the cursor one character and returns the character that was
    /// pointed to by the cursor before the advance.  Returns None if the cursor
    /// was already at the end of the stream.
    fn next(&mut self) -> Option<SourceChar> {
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

    /// Advances the cursor one character, if the next character matches the given
    /// test character.
    fn next_if(&mut self, t: char) -> bool {
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

    /// Will advance the cursor if the stream after the cursor starts with the
    /// given test string.  If the remaining stream does not start with the
    /// test string then the cursor is not advanced.
    fn next_ifn(&mut self, t: &str) -> bool {
        // Check that `t` does not contain a line break, which would violate acceptable tokens
        if t.chars().any(|c| c.is_whitespace()) {
            // Panic because this indicates that there is a bug in the compiler code itself and the
            // user cannot fix it.
            panic!("A lexical token cannot contain a whitespace character")
        }

        if self.peek_ifn(t) {
            self.index += t.len();
            true
        } else {
            false
        }
    }

    /// Returns the character pointed at by the cursor which is the next
    /// character in the stream.
    fn peek(&self) -> Option<SourceChar> {
        if self.index < self.lexer.chars.len() {
            Some(self.lexer.chars[self.index])
        } else {
            None
        }
    }

    /// Checks if the next character in the stream matches the given test character
    /// without advancing the cursor.
    fn peek_if(&self, t: char) -> bool {
        match self.peek() {
            None => false,
            Some(c) => c == t,
        }
    }

    /// Checks if the character stream from the current cursor starts with
    /// the given test string, without advancing the cursor.
    fn peek_ifn(&self, t: &str) -> bool {
        let tc: Vec<char> = t.chars().collect();
        let l = tc.len();

        // If the test string is longer than the remaining characters in the lexer
        // then the match fails
        if (self.index + (l - 1)) >= self.lexer.chars.len() {
            return false;
        }

        for i in 0..l {
            if self.lexer.chars[self.index + i] != tc[i] {
                return false;
            }
        }
        return true;
    }
}

pub struct Lexer<'a> {
    chars: Source,
    end_offset: Offset,
    index: usize,
    line: u32,
    string_table: &'a mut StringTable,
    tracing: TracingConfig,
    logger: &'a Logger<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(
        text: Source,
        string_table: &'a mut StringTable,
        logger: &'a Logger,
    ) -> Result<Lexer<'a>, LexerError> {
        let end_offset = text.high();
        Ok(Lexer {
            chars: text,
            index: 0,
            end_offset,
            line: 1,
            tracing: TracingConfig::Off,
            string_table,
            logger,
        })
    }

    pub fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }

    /// Returns the line in the source code on which the lexer cursor is currently
    /// resting.
    pub fn line(&self) -> u32 {
        self.line as u32
    }

    /// Converts the given vector of characters to a vector of tokens.
    pub fn tokenize(&mut self) -> Vec<LexerResult<Token>> {
        let mut tokens = vec![];

        while self.index < self.chars.len() {
            // Consume any whitespace before attempting to parse the next token
            self.consume_whitespace();

            // Record the current index position, so that we can see if the parser
            // has advanced
            let prev_index = self.index;
            if self.index >= self.chars.len() {
                break;
            }

            // Parse the next token
            match self.next_token() {
                Ok(Some(t)) => tokens.push(Ok(t)),
                Ok(None) => (),
                Err(msg) => tokens.push(Err(msg)),
            }

            // Can no longer consume the input text
            if prev_index == self.index {
                tokens.push(err!(
                    self.current_char_span().unwrap(), // If there is no Span then something very bad has happened
                    LexerError::Locked(self.current_char())
                ));
                break;
            }
        }

        tokens
    }

    /// Attempt to parse the token which immediately follows from where the lexer
    /// cursor is currently pointing.
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
            }
            self.index += 1;
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

        match branch.merge() {
            None => Ok(None),
            Some((id, span)) => Ok(Some(Token::new(Lex::Identifier(id), self.line, span))),
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
                        Some(c) => {
                            return err!(
                                self.span_from_index_to_char(c).unwrap(),
                                LexerError::InvalidEscapeSequence(c)
                            )
                        }

                        None => {
                            return err!(
                                self.current_char_span().unwrap(), // Need to add a span to the branch
                                LexerError::ExpectedEscapeCharacter
                            );
                        }
                    }
                }
            }
            let (s, span) = branch.merge().unwrap();

            // Remove the quotes from the string
            let mut s: String = self.string_table.get(s).unwrap().into();
            s.remove(0);
            s.pop();
            let id = self.string_table.insert(s);

            Ok(Some(Token::new(Lex::StringLiteral(id), self.line, span)))
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

        let (int_token, _) = branch.cut().unwrap();

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
            let span = self.current_char_span().unwrap();
            let err = err!(
                span, // Need to add a span to the branch
                LexerError::InvalidInteger
            );

            err
        } else {
            let (_, span) = branch.merge().unwrap();
            let int_text = self.string_table.get(int_token).unwrap();

            Self::create_int_literal(self.line, span, int_text, type_suffix)
        }
        .map(|ok| {
            ok.as_ref().map(|token| {
                self.logger.write(Event::<LexerError> {
                    span: token.span,
                    msg: Ok("Integer"),
                });
            });
            ok
        })
        .map_err(|err| {
            self.logger.write(Event {
                span: err.span(),
                msg: Err(&err),
            });
            err
        })
    }

    fn create_int_literal(
        line: u32,
        span: Span,
        int_token: &str,
        prim: Primitive,
    ) -> LexerResult<Option<Token>> {
        match prim {
            Primitive::U8 => Ok(Some(Token::new(
                U8(int_token.parse::<u8>().unwrap()),
                line,
                span,
            ))),
            Primitive::U16 => Ok(Some(Token::new(
                U16(int_token.parse::<u16>().unwrap()),
                line,
                span,
            ))),
            Primitive::U32 => Ok(Some(Token::new(
                U32(int_token.parse::<u32>().unwrap()),
                line,
                span,
            ))),
            Primitive::U64 => Ok(Some(Token::new(
                U64(int_token.parse::<u64>().unwrap()),
                line,
                span,
            ))),
            Primitive::I8 => Ok(Some(Token::new(
                I8(int_token.parse::<i8>().unwrap()),
                line,
                span,
            ))),
            Primitive::I16 => Ok(Some(Token::new(
                I16(int_token.parse::<i16>().unwrap()),
                line,
                span,
            ))),
            Primitive::I32 => Ok(Some(Token::new(
                I32(int_token.parse::<i32>().unwrap()),
                line,
                span,
            ))),
            Primitive::I64 => Ok(Some(Token::new(
                I64(int_token.parse::<i64>().unwrap()),
                line,
                span,
            ))),
            Primitive::Bool | Primitive::StringLiteral => {
                err!(span, LexerError::UnexpectedSuffixType(prim))
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
                token = Some(t);
                break;
            }
        }
        Ok(token.map(|t| {
            let (_, span) = branch.merge().unwrap();
            Token::new(t.clone(), line, span)
        }))
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
                line: l,
                sym: Identifier(id),
                span,
                ..
            } => match self.string_table.get(*id).unwrap() {
                "true" => Token::new(Bool(true), *l, *span),
                "false" => Token::new(Bool(false), *l, *span),
                _ => token,
            },
            _ => token,
        }
    }

    pub fn if_primitive_map(&self, token: Token) -> Token {
        trace!(self);
        match token {
            Token {
                line: l,
                span,
                sym: Identifier(ref id),
                ..
            } => match self.string_table.get(*id).unwrap() {
                "u8" => Token::new(Primitive(Primitive::U8), l, span),
                "u16" => Token::new(Primitive(Primitive::U16), l, span),
                "u32" => Token::new(Primitive(Primitive::U32), l, span),
                "u64" => Token::new(Primitive(Primitive::U64), l, span),
                "i8" => Token::new(Primitive(Primitive::I8), l, span),
                "i16" => Token::new(Primitive(Primitive::I16), l, span),
                "i32" => Token::new(Primitive(Primitive::I32), l, span),
                "i64" => Token::new(Primitive(Primitive::I64), l, span),
                "bool" => Token::new(Primitive(Primitive::Bool), l, span),
                "string" => Token::new(Primitive(Primitive::StringLiteral), l, span),
                _ => Token::new(Identifier(id.clone()), l, span),
            },
            _ => token,
        }
    }

    pub fn if_keyword_map(&self, token: Token) -> Token {
        trace!(self);
        match token {
            Token {
                line: l,
                span,
                sym: Identifier(ref id),
                ..
            } => match self.string_table.get(*id).unwrap() {
                "let" => Token::new(Let, l, span),
                "mut" => Token::new(Mut, l, span),
                "return" => Token::new(Return, l, span),
                "yield" => Token::new(Yield, l, span),
                "yret" => Token::new(YieldReturn, l, span),
                "fn" => Token::new(FunctionDef, l, span),
                "co" => Token::new(CoroutineDef, l, span),
                "mod" => Token::new(ModuleDef, l, span),
                "struct" => Token::new(Struct, l, span),
                "extern" => Token::new(Extern, l, span),
                "init" => Token::new(Init, l, span),
                "if" => Token::new(If, l, span),
                "else" => Token::new(Else, l, span),
                "while" => Token::new(While, l, span),
                "self" => Token::new(PathSelf, l, span),
                "super" => Token::new(PathSuper, l, span),
                "root" => Token::new(PathFileRoot, l, span),
                "project" => Token::new(PathProjectRoot, l, span),
                _ => Token::new(Identifier(id.clone()), l, span),
            },
            _ => token,
        }
    }

    /// Returns the character that the lexer cursor is currently pointing to.
    fn current_char(&self) -> Option<SourceChar> {
        if self.index < self.chars.len() {
            Some(self.chars[self.index])
        } else {
            None
        }
    }

    /// Returns the span covered by the character the lexer cursor is currently
    /// pointing at.  If the cursor is at the end of the input text, then this
    /// will return `None`.
    fn current_char_span(&self) -> Option<Span> {
        if self.index < self.chars.len() {
            let char_offset = self.chars[self.index].offset();
            let next_char_offset = if self.index + 1 < self.chars.len() {
                self.chars[self.index + 1].offset()
            } else {
                self.end_offset
            };

            Some(Span::new(char_offset, next_char_offset))
        } else {
            None
        }
    }

    /// Returns the span from the Lexer cursor to the given character.
    fn span_from_index_to_char(&self, c: SourceChar) -> Option<Span> {
        if self.index < self.chars.len() {
            let start = self.chars[self.index].offset();
            let end = c.offset(); // This will actually result in an off by one error for the span.
            Some(Span::new(start, end))
        } else {
            None
        }
    }

    /// Returns true if the given character is a Braid delimiter.  Which is
    /// punctuation or whitespace.
    fn is_delimiter(c: SourceChar) -> bool {
        c.is_ascii_punctuation() || c.is_whitespace()
    }

    /// Returns true if the character is a valid code for an escape sequence
    fn is_escape_code(c: SourceChar) -> bool {
        c == 'n' || c == 'r' || c == 't' || c == '"' || c == '0' || c == '\\'
    }
}
