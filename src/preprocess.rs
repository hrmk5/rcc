use std::collections::HashMap;
use crate::token::*;
use crate::error::{CompileError, Span};

struct Preprocessor {
    tokens: Vec<Token>,
    objlike_macros: HashMap<String, Vec<Token>>,
    input: Vec<Token>,
    pos: usize,
    errors: Vec<CompileError>,
}

impl Preprocessor {
    fn new(input: Vec<Token>) -> Self {
        Self {
            tokens: Vec::new(),
            objlike_macros: HashMap::new(),
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

    // fn curr(&self) -> &Token {
    //     &self.input[self.pos]
    // }

    fn next(&mut self) -> &Token {
        self.pos += 1;
        &self.input[self.pos - 1]
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

    fn define_objlike(&mut self, name: String) {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let token = self.next();
            if let TokenKind::NewLine | TokenKind::EOF = token.kind {
                break;
            }

            tokens.push(token.clone())
        }

        self.objlike_macros.insert(name, tokens);
    }

    fn define_macro(&mut self) {
        // `#` を消費
        self.next();

        match self.next().clone() {
            Token { kind: TokenKind::Ident(name), .. } => self.define_objlike(name),
            token => self.add_error("識別子ではありません", &token),
        };
    }

    fn preprocess(mut self) -> Result<Vec<Token>, Vec<CompileError>> {
        loop {
            let token = self.next().clone();
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
    let preprossor = Preprocessor::new(input);
    preprossor.preprocess()
}
