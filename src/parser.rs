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

    fn parse_term(&mut self) -> Expr {
        match self.tokens[self.pos].kind {
            TokenKind::Lparen => {
                self.pos += 1;
                let expr = self.parse();
                if !self.consume(TokenKind::Rparen) {
                    self.error_at(self.tokens[self.pos].pos, "開きカッコに対応する閉じカッコがありません");
                }
                expr
            },
            TokenKind::Number(num) => {
                self.pos += 1;
                Expr::Literal(Literal::Number(num))
            },
            _ => {
                self.error_at(self.tokens[self.pos].pos, "数値でも開きカッコでもないトークンです");
                Expr::Invalid
            },
        }
    }

    fn parse_mul(&mut self) -> Expr {
        let mut expr = self.parse_unary();

        loop {
            if self.consume(TokenKind::Mul) {
                expr = Expr::Infix(Infix::Mul, Box::new(expr), Box::new(self.parse_unary()));
            } else if self.consume(TokenKind::Div) {
                expr = Expr::Infix(Infix::Div, Box::new(expr), Box::new(self.parse_unary()));
            } else {
                return expr;
            }
        }
    }

    fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_mul(); 

        loop {
            if self.consume(TokenKind::Add) {
                expr = Expr::Infix(Infix::Add, Box::new(expr), Box::new(self.parse_mul()));
            } else if self.consume(TokenKind::Sub) {
                expr = Expr::Infix(Infix::Sub, Box::new(expr), Box::new(self.parse_mul()));
            } else {
                return expr;
            }
        }
    }

    fn parse_unary(&mut self) -> Expr {
        match self.tokens[self.pos].kind {
            TokenKind::Add => {
                self.pos += 1;
                self.parse_term()
            },
            TokenKind::Sub => {
                self.pos += 1;
                Expr::Infix(Infix::Sub, Box::new(Expr::Literal(Literal::Number(0))), Box::new(self.parse_term()))
            },
            _ => {
                self.parse_term()
            }
        }
    }

    pub fn parse(&mut self) -> Expr {
        self.parse_expr()
    }
}
