use std::path::Path;
use std::fs;
use std::collections::HashMap;
use crate::token::*;
use crate::tokenizer::Tokenizer;
use crate::error::{CompileError, Span};
use crate::id::{Id, IdMap};

#[derive(Debug, Clone)]
enum Macro {
    ObjLike(Vec<Token>),
    FuncLike(Vec<Id>, Vec<Token>),
}

struct Preprocessor<'a> {
    tokens: Vec<Token>,
    macros: HashMap<Id, Macro>,
    input: Vec<Token>,
    pos: usize,
    errors: Vec<CompileError>,
    id_map: &'a mut IdMap,
}

impl<'a> Preprocessor<'a> {
    fn new(input: Vec<Token>, id_map: &'a mut IdMap) -> Self {
        Self {
            tokens: Vec::new(),
            macros: HashMap::new(),
            input,
            errors: Vec::new(),
            pos: 0,
            id_map,
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

    fn expect(&mut self, kind: TokenKind) {
        let token = self.next().clone();
        if token.kind != kind {
            self.add_error(&format!("Expected `{}`", kind.to_string()), &token);
        }
    }

    fn get_arguments(&mut self) -> Vec<Vec<Token>> {
        let mut arguments = Vec::new();
        if let TokenKind::Rparen = self.curr().kind {
            self.next();
        } else {
            let mut tokens = Vec::new();
            loop {
                let token = self.next().clone();
                match token.kind {
                    TokenKind::Comma => {
                        arguments.push(tokens.clone());
                        tokens.clear();
                    },
                    TokenKind::Rparen => {
                        arguments.push(tokens);
                        break;
                    },
                    TokenKind::EOF => {
                        self.add_error("Unterminated argument list", &token);
                        return Vec::new();
                    },
                    _ => tokens.push(token),
                }
            }
        }
        arguments
    }

    fn apply(&mut self, name: &Id) {
        match self.macros[name].clone() {
            Macro::ObjLike(tokens) => {
                for token in tokens {
                    match token.kind {
                        TokenKind::Ident(name) if self.macros.contains_key(&name) => self.apply(&name),
                        _ => self.tokens.push(token),
                    };
                }
            },
            Macro::FuncLike(params, tokens) => {
                self.expect(TokenKind::Lparen);
                let macro_pos = self.pos;
                let arguments = self.get_arguments();
                if params.len() != arguments.len() {
                    self.add_error(&format!("Passed {} arguments, but macro takes just {}", arguments.len(), params.len()), &self.input[macro_pos].clone());
                    return;
                }

                for token in tokens {
                    match token.kind {
                        TokenKind::Ident(name) => {
                            let index = params.iter().position(|param| param.clone() == name);
                            if let Some(index) = index {
                                self.tokens.extend_from_slice(&arguments[index]);
                            } else if self.macros.contains_key(&name) {
                                self.apply(&name);
                            }
                        },
                        _ => self.tokens.push(token),
                    };
                }
            },
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

    fn define_objlike(&mut self, name: &Id) {
        let tokens = self.get_tokens_until_newline();
        self.macros.insert(*name, Macro::ObjLike(tokens));
    }
    
    fn get_params(&mut self) -> Vec<Id> {
        let mut params = Vec::new();
        if let TokenKind::Rparen = self.curr().kind {
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

    fn define_funclike(&mut self, name: &Id) {
        // Consume '('
        self.next();

        let params = self.get_params();
        let tokens = self.get_tokens_until_newline();

        self.macros.insert(*name, Macro::FuncLike(params, tokens));
    }

    fn tokenize_file(&mut self, path: &Path) -> Option<Vec<Token>> {
        let code = match fs::read_to_string(path) {
            Ok(code) => code,
            Err(err) => {
                self.add_error(&format!("{}", err), &self.curr().clone());
                return None;
            },
        };

        let tokenizer = Tokenizer::new(&code, self.id_map);
        match tokenizer.tokenize() {
            Ok(result) => Some(result),
            Err(errors) => {
                self.errors.extend(errors.into_iter());
                None
            },
        }
    }

    fn include(&mut self) {
        match self.next().clone() {
            Token { kind: TokenKind::String(path), .. } => {
                // Convert Id to Path
                let path = self.id_map.name(&path).to_string();
                let path = Path::new(&path);

                // Tokenize and preprocess
                if let Some(tokens) = self.tokenize_file(path) {
                    let mut preprocessor = Preprocessor::new(tokens, self.id_map);
                    match preprocessor.preprocess() {
                        Ok(mut tokens) => {
                            // Remove EOF token
                            tokens.pop().unwrap();
                            self.tokens.extend(tokens.into_iter());
                            self.macros.extend(preprocessor.macros.into_iter());
                        },
                        Err(errors) => self.errors.extend(errors.into_iter()),
                    };
                }
            },
            token => { self.add_error("文字列ではありません", &token); dbg!(token); },
        };
    }

    fn define_macro(&mut self) {
        match self.next().clone() {
            Token { kind: TokenKind::Ident(name), .. } => match self.id_map.name(&name) {
                "include" => self.include(),
                "define" => match self.next().clone() {
                    Token { kind: TokenKind::Ident(name), .. } if self.curr().kind == TokenKind::Lparen => self.define_funclike(&name),
                    Token { kind: TokenKind::Ident(name), .. } => self.define_objlike(&name),
                    _ => self.add_error("識別子ではありません", &self.curr().clone()),
                },
                _ => self.add_error("\"include\", \"define\" のみ対応しています", &self.curr().clone()),
            },
            token => self.add_error("識別子ではありません", &token),
        };
    }

    fn preprocess(&mut self) -> Result<Vec<Token>, Vec<CompileError>> {
        loop {
            let token = self.next().clone();
            match token.kind {
                TokenKind::Hash => self.define_macro(),
                TokenKind::Ident(name) if self.macros.contains_key(&name) => self.apply(&name),
                TokenKind::NewLine => {},
                TokenKind::EOF => { self.tokens.push(token); break },
                _ => self.tokens.push(token),
            };
        }

        if self.errors.is_empty() {
            Ok(self.tokens.clone())
        } else {
            Err(self.errors.clone())
        }
    }
}

pub fn preprocess(input: Vec<Token>, id_map: &mut IdMap) -> Result<Vec<Token>, Vec<CompileError>> {
    let mut preprossor = Preprocessor::new(input, id_map);
    preprossor.preprocess()
}
