use crate::tokenizer::{Token, TokenKind};
use std::collections::HashMap;

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
    Ident(usize),
    Assign(Box<Expr>, Box<Expr>),
    Infix(Infix, Box<Expr>,  Box<Expr>),
    Call(String, Vec<Expr>),
    Invalid,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Return(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Block(Vec<Stmt>),
}

#[derive(Debug)]
pub enum Declaration {
    Func(String, Vec<String>, HashMap<String, usize>, Stmt),
}

#[derive(Debug)]
pub struct Program(pub Vec<Declaration>);

#[derive(Debug)]
pub struct ParseError {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
    pub message: String,
}

#[derive(Debug)]
pub struct Parser {
    pub errors: Vec<ParseError>,
    variables: HashMap<String, usize>,
    tokens: Vec<Token>,
    pos: usize,
}

macro_rules! expect {
    ($self: ident, $e: expr) => {
        if !$self.consume($e) {
            $self.add_error(&format!("'{}' ではないトークンです", $e.to_string()));
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            pos: 0,
            tokens,
            errors: Vec::new(),
            variables: HashMap::new(),
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
        let token = &self.tokens[self.pos];
        self.errors.push(ParseError {
            start_line: token.start_line,
            start_col: token.start_col,
            end_line: token.end_line,
            end_col: token.end_col,
            message: String::from(msg),
        });
    }

    fn parse_term(&mut self) -> Expr {
        let (is_ident, ident) = match self.tokens[self.pos].kind {
            TokenKind::Ident(ref ident) => (true, ident.clone()),
            _ => (false, String::new()),
        };

        if is_ident {
            self.pos += 1;

            if self.consume(TokenKind::Lparen) {
                // 関数呼び出し
                let mut args = Vec::<Expr>::new();
                loop {
                    args.push(self.parse_expr());
                    if self.consume(TokenKind::Rparen) {
                        break;
                    } else if self.consume(TokenKind::EOF) {
                        self.add_error("開きカッコに対応する閉じカッコがありません");
                        break;
                    }

                    expect!(self, TokenKind::Comma);
                }

                return Expr::Call(ident, args);
            } else {
                // 変数
                let offset = match self.variables.get(&ident) {
                    Some(offset) => *offset,
                    None => {
                        let offset = self.variables.len() * 8;
                        self.variables.insert(ident.clone(), offset);
                        offset
                    },
                };
                return Expr::Ident(offset);
            }
        }

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
                let stmt = Stmt::Return(self.parse_expr());

                expect!(self, TokenKind::Semicolon);

                stmt
            },
            TokenKind::If => {
                self.pos += 1;

                expect!(self, TokenKind::Lparen);

                let expr = self.parse_expr();
                if !self.consume(TokenKind::Rparen) {
                    self.add_error("開きカッコに対応する閉じカッコがありません");
                }

                let if_stmt = Box::new(self.parse_stmt());
                let else_stmt = if self.consume(TokenKind::Else) { Some(Box::new(self.parse_stmt())) } else { None };
                Stmt::If(expr, if_stmt, else_stmt)
            },
            TokenKind::While => {
                self.pos += 1;

                expect!(self, TokenKind::Lparen);

                let expr = self.parse_expr();
                if !self.consume(TokenKind::Rparen) {
                    self.add_error("開きカッコに対応する閉じカッコがありません");
                }

                Stmt::While(expr, Box::new(self.parse_stmt()))
            },
            TokenKind::For => {
                self.pos += 1;
                expect!(self, TokenKind::Lparen);

                let mut parse_expr_in_for = |is_last: bool| -> Option<Expr> {
                    if self.consume(TokenKind::Semicolon) { 
                        None
                    } else {
                        let expr = self.parse_expr();

                        expect!(self, if !is_last { TokenKind::Semicolon } else { TokenKind::Rparen });

                        Some(expr)
                    }
                };

                let expr1 = parse_expr_in_for(false);
                let expr2 = parse_expr_in_for(false);
                let expr3  = parse_expr_in_for(true);

                Stmt::For(expr1, expr2, expr3, Box::new(self.parse_stmt()))
            },
            TokenKind::Lbrace => {
                self.pos += 1;
                let mut stmt_list = Vec::<Stmt>::new();
                loop {
                    if self.consume(TokenKind::Rbrace) {
                        break;
                    } else if self.consume(TokenKind::EOF) {
                        self.pos -= 1;
                        self.add_error("'{' に対応する '}' がありません");
                        break;
                    }

                    stmt_list.push(self.parse_stmt());
                }

                Stmt::Block(stmt_list)
            },
            _ => {
                let stmt = Stmt::Expr(self.parse_expr());
                expect!(self, TokenKind::Semicolon);
                stmt
            },
        };


        stmt
    }

    pub fn parse_declaration(&mut self) -> Option<Declaration> {
        let (is_ident, ident) = match self.tokens[self.pos].kind {
            TokenKind::Ident(ref ident) => (true, ident.clone()),
            _ => (false, String::new()),
        };

        if is_ident {
            self.pos += 1;
            expect!(self, TokenKind::Lparen);

            // 引数
            let mut variables = HashMap::<String, usize>::new();
            let mut args = Vec::new();
            loop {
                if let TokenKind::Ident(ref ident) = self.tokens[self.pos].kind {
                    self.pos += 1;
                    variables.insert(ident.clone(), variables.len() * 8);
                    args.push(ident.clone());
                }

                if self.consume(TokenKind::Rparen) {
                    break;
                } else if !self.consume(TokenKind::Comma) {
                    self.add_error("',' か ')' ではないトークンです");
                    break;
                }
            }

            self.variables = variables;

            let stmt = self.parse_stmt();
            match stmt {
                Stmt::Block(_) => {},
                _ => self.add_error("ブロックではありません"),
            };

            return Some(Declaration::Func(ident, args, self.variables.clone(), stmt));
        }

        match self.tokens[self.pos].kind {
            _ => {
                self.add_error("関数定義ではありません");
                None
            },
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut declarations = Vec::new();
        while self.tokens[self.pos].kind != TokenKind::EOF {
            if let Some(declaration) = self.parse_declaration() {
                declarations.push(declaration);
            }
        }
        Program(declarations)
    }
}
