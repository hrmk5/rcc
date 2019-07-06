use std::collections::HashMap;
use crate::token::*;
use crate::error::{CompileError, Span};

#[derive(Debug, Clone)]
enum Macro {
    ObjLike(Vec<Token>),
    FuncLike(Vec<String>, Vec<Token>),
}

struct Preprocessor {
    tokens: Vec<Token>,
    macros: HashMap<String, Macro>,
    input: Vec<Token>,
    pos: usize,
    errors: Vec<CompileError>,
}

impl Preprocessor {
    fn new(input: Vec<Token>) -> Self {
        Self {
            tokens: Vec::new(),
            macros: HashMap::new(),
            input,
            errors: Vec::new(),
            pos: 0,
        }
    }

    fn add_error(&mut self, msg: &str, token: &Token) {
        self.errors.push(CompileError {
            msg: msg.to_string(),
            span: Span::from_token(token),
        });
    }

    fn curr(&self) -> &Token {
        &self.input[self.pos]
    }

    fn next(&mut self) -> &Token {
        self.pos += 1;
        &self.input[self.pos - 1]
    }

    fn apply(&mut self, name: String) {
        match self.macros[&name].clone() {
            Macro::ObjLike(tokens) => {
                for token in tokens {
                    match token.kind {
                        TokenKind::Ident(name) if self.macros.contains_key(&name) => self.apply(name),
                        _ => self.tokens.push(token),
                    };
                }
            },
            _ => {},
        };
    }

    fn get_tokens_until_newline(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let token = self.next();
            if let TokenKind::NewLine | TokenKind::EOF = token.kind {
                break;
            }

            tokens.push(token.clone())
        }
        tokens
    }

    fn define_objlike(&mut self, name: String) {
        let tokens = self.get_tokens_until_newline();
        self.macros.insert(name, Macro::ObjLike(tokens));
    }
    
    fn get_params(&mut self) -> Vec<String> {
        let mut params = Vec::new();
        if let TokenKind::Lparen = self.curr().kind {
            self.next();
        } else {
            loop {
                let token = self.next().clone();
                match token.kind {
                    TokenKind::Ident(ident) => params.push(ident),
                    _ => {
                        self.add_error("Unexpected token", &token);
                        break;
                    },
                };

                let token = self.next().clone();
                match token.kind {
                    TokenKind::Comma => {},
                    TokenKind::Rparen => break,
                    _ => {
                        self.add_error("Unexpected token", &token);
                        break;
                    },
                }
            }
        }
        params
    }

    fn define_funclike(&mut self, name: String) {
        // Consume '('
        self.next();

        let params = self.get_params();
        let tokens = self.get_tokens_until_newline();

        self.macros.insert(name, Macro::FuncLike(params, tokens));
    }

    fn define_macro(&mut self) {
        // `#` を消費
        self.next();

        match self.next().clone() {
            Token { kind: TokenKind::Ident(name), .. } if self.curr().kind == TokenKind::Lparen => self.define_funclike(name),
            Token { kind: TokenKind::Ident(name), .. } => self.define_objlike(name),
            token => self.add_error("識別子ではありません", &token),
        };
    }

    fn preprocess(mut self) -> Result<Vec<Token>, Vec<CompileError>> {
        loop {
            let token = self.next().clone();
            match token.kind {
                TokenKind::Hash => self.define_macro(),
                TokenKind::Ident(name) if self.macros.contains_key(&name) => self.apply(name),
                TokenKind::NewLine => {},
                TokenKind::EOF => { self.tokens.push(token); break },
                _ => self.tokens.push(token),
            };
        }

        println!("{:?}", self.macros);
        
        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
        }
    }
}

pub fn preprocess(input: Vec<Token>) -> Result<Vec<Token>, Vec<CompileError>> {
    let preprossor = Preprocessor::new(input);
    preprossor.preprocess()
}
