use crate::tokenizer::{Token, TokenKind};

#[derive(Debug)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

#[derive(Debug)]
pub enum Literal {
    Number(i32),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Ident(String),
    Assign(Box<Expr>, Box<Expr>),
    Infix(Infix, Box<Expr>,  Box<Expr>),
    Invalid,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Return(Expr),
}

#[derive(Debug)]
pub struct Program(pub Vec<Stmt>);

#[derive(Debug)]
pub struct ParseError {
    pub pos: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct Parser {
    pub errors: Vec<ParseError>,
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            pos: 0,
            tokens,
            errors: Vec::new(),
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

    fn add_error(&mut self, msg: &str) {
        self.errors.push(ParseError {
            pos: self.tokens[self.pos].pos,
            message: String::from(msg),
        });
    }

    fn parse_term(&mut self) -> Expr {
        match self.tokens[self.pos].kind {
            TokenKind::Lparen => {
                self.pos += 1;
                let expr = self.parse_expr();
                if !self.consume(TokenKind::Rparen) {
                    self.add_error("開きカッコに対応する閉じカッコがありません");
                }
                expr
            },
            TokenKind::Number(num) => {
                self.pos += 1;
                Expr::Literal(Literal::Number(num))
            },
            TokenKind::Ident(ref ident) => {
                self.pos += 1;
                Expr::Ident(ident.clone())
            },
            _ => {
                self.add_error("数値でも開きカッコでもないトークンです");
                Expr::Invalid
            },
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

    fn parse_add(&mut self) -> Expr {
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

    fn parse_relational(&mut self) -> Expr {
        let mut expr = self.parse_add(); 

        loop {
            if self.consume(TokenKind::LessThan) {
                expr = Expr::Infix(Infix::LessThan, Box::new(expr), Box::new(self.parse_add()));
            } else if self.consume(TokenKind::LessThanOrEqual) {
                expr = Expr::Infix(Infix::LessThanOrEqual, Box::new(expr), Box::new(self.parse_add()));
            } else if self.consume(TokenKind::GreaterThan) {
                expr = Expr::Infix(Infix::LessThan, Box::new(self.parse_add()), Box::new(expr));
            } else if self.consume(TokenKind::GreaterThanOrEqual) {
                expr = Expr::Infix(Infix::LessThanOrEqual, Box::new(self.parse_add()), Box::new(expr));
            } else {
                return expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_relational(); 

        loop {
            if self.consume(TokenKind::Equal) {
                expr = Expr::Infix(Infix::Equal, Box::new(expr), Box::new(self.parse_relational()));
            } else if self.consume(TokenKind::NotEqual) {
                expr = Expr::Infix(Infix::NotEqual, Box::new(expr), Box::new(self.parse_relational()));
            } else {
                return expr;
            }
        }
    }

    fn parse_assign(&mut self) -> Expr {
        let mut expr = self.parse_equality();
        if self.consume(TokenKind::Assign) {
            expr = Expr::Assign(Box::new(expr), Box::new(self.parse_assign()));
        }

        expr
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assign()
    }

    fn parse_stmt(&mut self) -> Stmt {
        let stmt = match self.tokens[self.pos].kind {
            TokenKind::Return => {
                self.pos += 1;
                Stmt::Return(self.parse_expr())
            },
            _ => Stmt::Expr(self.parse_expr()),
        };

        if !self.consume(TokenKind::Semicolon) {
            self.add_error("';' ではないトークンです");
        }

        stmt
    }

    pub fn parse(&mut self) -> Program {
        let mut stmt_list = Vec::new();
        while self.tokens[self.pos].kind != TokenKind::EOF {
            stmt_list.push(self.parse_stmt());
        }
        Program(stmt_list)
    }
}
