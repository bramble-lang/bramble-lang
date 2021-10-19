use super::{ctx_for_tokens, ParserError};
use crate::compiler::ast::Context;
use crate::compiler::lexer::tokens::{Lex, Token};
use crate::compiler::source::Offset;
use crate::compiler::{CompilerError, Span};
use crate::StringId;
//use crate::result::Result;

pub struct TokenStream<'a> {
    tokens: &'a Vec<Token>,
    index: usize,
    span: Span,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> TokenStream {
        TokenStream {
            tokens,
            index: 0,
            span: ctx_for_tokens(tokens)
                .map_or(Span::new(Offset::new(0), Offset::new(0)), |ctx| ctx.span()),
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn set_index(&mut self, i: usize) {
        self.index = i;
    }

    pub fn next(&mut self) -> Option<Token> {
        if self.index >= self.tokens.len() {
            None
        } else {
            self.index += 1;
            Some(self.tokens[self.index - 1].clone())
        }
    }

    pub fn next_if(&mut self, test: &Lex) -> Option<Token> {
        if self.test_if(test) {
            self.next()
        } else {
            None
        }
    }

    // TODO: return the line # and the ID name
    pub fn next_if_id(&mut self) -> Option<(StringId, u32, Span)> {
        match self.next_if(&Lex::Identifier(StringId::new())) {
            Some(Token {
                line: l,
                span,
                sym: Lex::Identifier(id),
                ..
            }) => Some((id, l, span)),
            Some(_) => None,
            None => None,
        }
    }

    pub fn next_must_be(&mut self, test: &Lex) -> Result<Token, CompilerError<ParserError>> {
        let (line, span, found) = match self.peek() {
            Some(t) => (t.line, t.span, t.sym.clone()),
            None => {
                return err!(
                    0,
                    Span::new(self.span.high(), self.span.high()),
                    ParserError::ExpectedButFound(vec![test.clone()], None)
                )
            }
        };
        match self.next_if(test) {
            Some(t) => Ok(t),
            None => {
                err!(
                    line,
                    span,
                    ParserError::ExpectedButFound(vec![test.clone()], Some(found))
                )
            }
        }
    }

    pub fn next_ifn(&mut self, test: Vec<Lex>) -> Option<Vec<Token>> {
        let end = self.index + test.len();
        if self.test_ifn(test) {
            let v: Vec<Token> = self.tokens[self.index..end].into();
            self.index = end;
            Some(v)
        } else {
            None
        }
    }

    pub fn next_if_one_of(&mut self, set: Vec<Lex>) -> Option<Token> {
        if self.test_if_one_of(set) {
            self.next()
        } else {
            None
        }
    }

    pub fn peek(&self) -> Option<&Token> {
        if self.index < self.tokens.len() {
            Some(&self.tokens[self.index])
        } else {
            None
        }
    }

    pub fn peek_at(&self, i: usize) -> Option<&Token> {
        if self.index + i < self.tokens.len() {
            Some(&self.tokens[self.index + i])
        } else {
            None
        }
    }

    pub fn test_if(&self, test: &Lex) -> bool {
        match self.peek() {
            None => false,
            Some(t) => t.token_eq(test),
        }
    }

    pub fn test_ifn(&self, test: Vec<Lex>) -> bool {
        for i in 0..test.len() {
            match self.peek_at(i) {
                None => return false,
                Some(token) => {
                    if !token.token_eq(&test[i]) {
                        return false;
                    }
                }
            }
        }

        true
    }

    pub fn test_if_one_of(&self, set: Vec<Lex>) -> bool {
        match self.peek() {
            None => false,
            Some(t) => set.iter().find(|l| t.token_eq(l)).is_some(),
        }
    }
}

#[cfg(test)]
mod test_tokenstream {
    use super::TokenStream;
    use crate::compiler::lexer::tokens::{Lex, Token};
    use crate::compiler::source::Offset;
    use crate::compiler::{Lexer, SourceMap, Span};
    use crate::StringTable;

    fn new_span(l: u32, h: u32) -> Span {
        Span::new(Offset::new(l), Offset::new(h))
    }

    #[test]
    fn test_peek() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let ts = TokenStream::new(&tokens);
        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::LParen,
                span: Span::new(Offset::new(0), Offset::new(1)),
            }
        );
    }

    #[test]
    fn test_peek_empty() {
        let tokens = vec![];
        let ts = TokenStream::new(&tokens);
        let p = ts.peek();
        assert_eq!(p, None);
    }

    #[test]
    fn test_peek_at() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let ts = TokenStream::new(&tokens);
        let p = ts.peek_at(0).unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::LParen,
                span: new_span(0, 1),
            }
        );

        let p = ts.peek_at(1).unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );

        let p = ts.peek_at(8);
        assert_eq!(p, None);
    }

    #[test]
    fn test_next() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let mut ts = TokenStream::new(&tokens);
        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::LParen,
                span: new_span(0, 1),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::Add,
                span: new_span(3, 4),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::I64(4),
                span: new_span(5, 6),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::RParen,
                span: new_span(6, 7),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::Mul,
                span: new_span(8, 9),
            }
        );

        let p = ts.next().unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::I64(3),
                span: new_span(10, 11),
            }
        );

        let p = ts.next();
        assert_eq!(p, None);
    }

    #[test]
    fn test_next_if() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let mut ts = TokenStream::new(&tokens);
        let p = ts.next_if(&Lex::LParen).unwrap(); // should I really use a borrow for this?  If not then gotta do clones and BS i think.
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::LParen,
                span: new_span(0, 1),
            }
        );

        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );

        let p = ts.next_if(&Lex::LParen); // should I really use a borrow for this?  If not then gotta do clones and BS i think.
        assert_eq!(p, None);

        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );
    }

    #[test]
    fn test_next_ifn() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let mut ts = TokenStream::new(&tokens);
        let p = ts.next_ifn(vec![Lex::LParen, Lex::I64(0)]).unwrap();
        assert_eq!(
            *p,
            vec![
                Token {
                    line: 1,
                    sym: Lex::LParen,
                    span: new_span(0, 1),
                },
                Token {
                    line: 1,
                    sym: Lex::I64(2),
                    span: new_span(1, 2),
                }
            ]
        );

        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::Add,
                span: new_span(3, 4),
            }
        );
    }

    #[test]
    fn test_if_one_of() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let ts = TokenStream::new(&tokens);
        let p = ts.test_if_one_of(vec![Lex::LParen, Lex::I64(0)]);
        assert_eq!(p, true);

        let p = ts.test_if_one_of(vec![Lex::RParen, Lex::I64(0)]);
        assert_eq!(p, false);
    }

    #[test]
    fn test_next_if_one_of() {
        let text = "(2 + 4) * 3";

        let mut sm = SourceMap::new();
        sm.add_string(text, "/test".into()).unwrap();
        let src = sm.get(0).unwrap().read().unwrap();

        let mut table = StringTable::new();
        let tokens: Vec<Token> = Lexer::new(&mut table, src)
            .unwrap()
            .tokenize()
            .into_iter()
            .collect::<Result<_, _>>()
            .unwrap();

        let mut ts = TokenStream::new(&tokens);
        let p = ts.next_if_one_of(vec![Lex::LParen, Lex::I64(0)]).unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::LParen,
                span: new_span(0, 1),
            }
        );
        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );

        let p = ts.next_if_one_of(vec![Lex::LParen, Lex::I64(0)]).unwrap();
        assert_eq!(
            p,
            Token {
                line: 1,
                sym: Lex::I64(2),
                span: new_span(1, 2),
            }
        );
        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::Add,
                span: new_span(3, 4),
            }
        );

        let p = ts.next_if_one_of(vec![Lex::LParen, Lex::I64(0)]).is_none();
        assert_eq!(p, true);
        let p = ts.peek().unwrap();
        assert_eq!(
            *p,
            Token {
                line: 1,
                sym: Lex::Add,
                span: new_span(3, 4),
            }
        );
    }
}
