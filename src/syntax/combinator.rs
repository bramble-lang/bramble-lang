#[derive(Clone)]
struct TokenStream {
    v: Vec<i32>,
    index: usize,
}

struct TokenResult {
    tokens: Result<Vec<i32>, String>,
    stream: TokenStream,
}

impl TokenResult {
    pub fn if_then(&self, test: i32, f: fn (i32) -> Result<i32, String>) -> TokenResult {
        let token = self.stream.v[self.stream.index];
        if token == test {
            let result = f(token);
            match result {
                Ok(t) => {
                    TokenResult {
                        tokens: Ok(vec![t]),
                        stream: TokenStream {
                            v: self.stream.v.clone(),
                            index: self.stream.index + 1,
                        }
                    }
                }
                Err(e) => {
                    TokenResult {
                        tokens: Err(e),
                        stream: self.stream.clone(),
                    }
                }
            }
        } else {
            TokenResult {
                tokens: Err(format!("Expected {}", test)),
                stream: self.stream.clone(),
            }
        }
    }

    pub fn or_else(&self, test: i32) -> TokenResult {
        if self.stream.v[self.stream.index] == test {
            let tokens = &self.stream.v[self.stream.index..=self.stream.index];
            TokenResult {
                tokens: Ok(tokens.into()),
                stream: TokenStream {
                    v: self.stream.v.clone(),
                    index: self.stream.index + 1,
                }
            }
        } else {
            TokenResult {
                tokens: Err(format!("Expected {}", test)),
                stream: self.stream.clone(),
            }
        }
    }
}

fn and_then< F, G, T>(parser_1: &'static F, parser_2: &'static G) -> Box<dyn Fn(&TokenStream) -> Result<((T, T), TokenStream), String>>
    where F: Fn(&TokenStream) -> Result<(T, TokenStream), String>,
        G: Fn(&TokenStream) -> Result<(T, TokenStream), String> {
    Box::new(move |ts| { 
        match parser_1(ts) {
            Err(e) => Err(e),
            Ok((token1, ts)) => {
                match parser_2(&ts) {
                    Err(e) => Err(e),
                    Ok((token2, ts)) => {
                        Ok(((token1, token2), ts))
                    }
                }
            }
        }
    })
}

fn or_else< F, T>(parser_1: &'static F, parser_2: &'static F) -> Box<dyn Fn(&TokenStream) -> Result<(T, TokenStream), String>>
    where F: Fn(&TokenStream) -> Result<(T, TokenStream), String> {
    Box::new(move |ts| { 
        match parser_1(ts) {
            Err(_) => {
                parser_2(ts)
            },
            Ok((token1, ts)) => {
                Ok((token1, ts))
            }
        }
    })
}