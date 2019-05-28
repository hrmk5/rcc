use crate::tokenizer::{Token, TokenKind};

#[derive(Debug)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Literal {
    Number(i32),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Infix(Infix, Box<Expr>,  Box<Expr>),
    Invalid,
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    input: String,
}

impl Parser {
    pub fn new(input: &str, tokens: Vec<Token>) -> Self {
        Parser {
            pos: 0,
            input: String::from(input),
            tokens,
        }
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.tokens[self.pos].kind == kind {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    fn error_at(&self, pos: usize, msg: &str) {
        println!("{}", self.input);
        println!("{}^ {}", std::iter::repeat(" ").take(pos).collect::<String>(), msg);
    }

    pub fn parse_term(&mut self) -> Expr {
        if let TokenKind::Number(num) = self.tokens[self.pos].kind {
            self.pos += 1;
            Expr::Literal(Literal::Number(num))
        } else {
            self.error_at(self.tokens[self.pos].pos, "数値ではないトークンです");
            Expr::Invalid
        }
    }

    pub fn parse(&mut self) -> Expr {
        let mut expr = self.parse_term(); 

        loop {
            if self.consume(TokenKind::Add) {
                expr = Expr::Infix(Infix::Add, Box::new(expr), Box::new(self.parse_term()));
            } else if self.consume(TokenKind::Sub) {
                expr = Expr::Infix(Infix::Sub, Box::new(expr), Box::new(self.parse_term()));
            } else {
                return expr;
            }
        }
    }
}
