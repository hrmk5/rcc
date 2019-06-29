use std::collections::HashMap;
use crate::token::*;
use crate::error::{CompileError, Span};

struct Preprocessor {
    tokens: Vec<Token>,
    objlike_macros: HashMap<String, Vec<Token>>,
    iter: Box<dyn Iterator<Item = Token>>,
    errors: Vec<CompileError>,
}

impl Preprocessor {
    fn new(input: impl Iterator<Item = Token> + 'static) -> Self {
        Self {
            tokens: Vec::new(),
            objlike_macros: HashMap::new(),
            iter: Box::new(input),
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, msg: &str, token: &Token) {
        self.errors.push(CompileError {
            msg: msg.to_string(),
            span: Span::from_token(token),
        });
    }

    fn define_objlike(&mut self, name: String) {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            match self.iter.next() {
                Some(token) => match token.kind {
                    TokenKind::NewLine | TokenKind::EOF => break,
                    _ => tokens.push(token),
                },
                None => panic!(),
            };
        }

        self.objlike_macros.insert(name, tokens);
    }

    fn apply_objlike(&mut self, name: String) {
        let tokens = self.objlike_macros[&name].clone();
        for token in tokens {
            match token.kind {
                TokenKind::Ident(name) if self.objlike_macros.contains_key(&name) => self.apply_objlike(name),
                _ => self.tokens.push(token),
            };
        }
    }

    fn define_macro(&mut self) {
        // `#` を消費
        self.iter.next().unwrap();

        match self.iter.next() {
            Some(Token { kind: TokenKind::Ident(name), .. }) => self.define_objlike(name),
            Some(token) => self.add_error("識別子ではありません", &token),
            _ => panic!(),
        };
    }

    fn preprocess(mut self) -> Result<Vec<Token>, Vec<CompileError>> {
        while let Some(token) = self.iter.next() {
            match token.kind {
                TokenKind::Hash => self.define_macro(),
                TokenKind::Ident(name) if self.objlike_macros.contains_key(&name) => self.apply_objlike(name),
                TokenKind::NewLine => {},
                TokenKind::EOF => { self.tokens.push(token); break },
                _ => self.tokens.push(token),
            };
        }
        
        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
        }
    }
}

pub fn preprocess(input: Vec<Token>) -> Result<Vec<Token>, Vec<CompileError>> {
    let preprossor = Preprocessor::new(input.into_iter());
    preprossor.preprocess()
}
