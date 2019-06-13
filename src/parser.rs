use crate::tokenizer::{Token, TokenKind};
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn get_size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Pointer(_) => 8,
            Type::Array(_, _) => 8,
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    Number(i32),
}

#[derive(Debug, Clone)]
pub enum Location {
    Local(usize), // rbpからのオフセット
    Global(String), // ラベル
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub ty: Type,
    pub location: Location,
}

impl Variable {
    pub fn new(ty: Type, location: Location) -> Self {
        Self {
            ty,
            location,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Variable(Variable),
    Dereference(Box<Expr>),
    Address(Variable),
    Assign(Box<Expr>, Box<Expr>),
    Infix(Infix, Box<Expr>,  Box<Expr>),
    Call(String, Type, Vec<Expr>),
    Invalid,
}

impl Expr {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::Literal(Literal::Number(_)) => Some(Type::Int),
            Expr::Variable(variable) => Some(variable.ty.clone()),
            Expr::Dereference(expr) => match expr.get_type() {
                Some(Type::Pointer(box ty)) => Some(ty.clone()),
                Some(Type::Array(box ty, _)) => Some(ty.clone()),
                _ => panic!("ポインタではない式を参照外ししています"),
            },
            Expr::Address(varaible) => Some(Type::Pointer(Box::new(varaible.ty.clone()))),
            Expr::Assign(lhs, _) => lhs.get_type(),
            Expr::Infix(Infix::Add, lhs, rhs) | Expr::Infix(Infix::Sub, lhs, rhs) => {
                let lty = lhs.get_type()?;
                let rty = rhs.get_type()?;
                match (lty.clone(), rty.clone()) {
                    (Type::Pointer(ty), _) | (_, Type::Pointer(ty)) => Some(Type::Pointer(Box::new(*ty))),
                    (Type::Array(ty, _), _) | (_, Type::Array(ty, _)) => Some(Type::Pointer(Box::new(*ty))),
                    _ => Some(Type::Int),
                }
            },
            Expr::Infix(_, _, _) => Some(Type::Int),
            Expr::Call(_, ty, _) => Some(ty.clone()),
            Expr::Invalid => None,
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Return(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    Block(Vec<Stmt>),
    Define(Variable),
}

#[derive(Debug)]
pub struct Function {
    return_type: Type,
    args: Vec<Variable>,
}

impl Function {
    pub fn new(return_type: Type, args: Vec<Variable>) -> Self {
        Function {
            return_type,
            args,
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Func(String, Vec<Variable>, usize, Stmt), // 関数名, 引数の数, スタックのサイズ, 処理
    GlobalVariable(Variable), // 変数名, 変数
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub global_variables: HashMap<String, Variable>,
}

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
    global_variables: HashMap<String, Variable>,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, Function>,
    tokens: Vec<Token>,
    pos: usize,
    stack_size: usize,
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
            global_variables: HashMap::new(),
            variables: HashMap::new(),
            functions: HashMap::new(),
            stack_size: 0,
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

    fn add_error_token(&mut self, msg: &str, pos: usize) {
        let token = &self.tokens[pos];
        self.errors.push(ParseError {
            start_line: token.start_line,
            start_col: token.start_col,
            end_line: token.end_line,
            end_col: token.end_col,
            message: String::from(msg),
        });
    }

    fn add_error(&mut self, msg: &str) {
        self.add_error_token(msg, self.pos);
    }

    fn expect_ident(&mut self) -> Option<String> {
        match self.tokens[self.pos].kind {
            TokenKind::Ident(ref ident) => {
                self.pos += 1;
                Some(ident.clone())
            },
            _ => None,
        }
    }

    fn expect_subscript(&mut self) -> Option<usize> {
        if self.consume(TokenKind::Lbracket) {
            let result = match self.tokens[self.pos].kind {
                TokenKind::Number(num) => match num.try_into() {
                    Ok(num) => Some(num),
                    _ => {
                        self.add_error("大きすぎる値です");
                        None
                    },
                },
                TokenKind::Sub => {
                    self.add_error("負の値です");
                    self.pos += 1;
                    None
                },
                _ => {
                    self.add_error("数値ではありません");
                    None
                },
            };
            self.pos += 1;
            expect!(self, TokenKind::Rbracket);
            result
        } else {
            None
        }
    }

    fn expect_define(&mut self) -> Option<Variable> {
        let ty = self.expect_type()?;
        let ident = self.expect_ident();
        match ident {
            Some(ident) => {
                let mut ty = ty;
                if let Some(num) = self.expect_subscript() {
                    ty = Type::Array(Box::new(ty), num);
                }

                let offset = self.stack_size;
                self.define_variable(&ident, &ty);
                Some(Variable::new(ty, Location::Local(offset)))
            },
            None => {
                self.add_error("識別子ではありません");
                None
            },
        }
    }

    fn define_variable(&mut self, ident: &str, ty: &Type) {
        match ty {
            Type::Array(_, size) => {
                self.variables.insert(ident.to_string(), Variable::new(ty.clone(), Location::Local(self.stack_size + 8 * (size - 1))));
                self.stack_size += 8 * size;
            },
            _ => {
                self.variables.insert(ident.to_string(), Variable::new(ty.clone(), Location::Local(self.stack_size)));
                self.stack_size += 8;
            }
        };
    }

    fn parse_pointer(&mut self, ty: Type) -> Type {
        if self.consume(TokenKind::Asterisk) {
            self.parse_pointer(Type::Pointer(Box::new(ty)))
        } else {
            ty
        }
    }

    fn expect_type(&mut self) -> Option<Type> {
        let ty = match self.tokens[self.pos].kind {
            TokenKind::Int => Type::Int,
            _ => return None,
        };

        self.pos += 1;

        Some(self.parse_pointer(ty))
    }

    fn parse_term(&mut self) -> Expr {
        let ident = self.expect_ident();
        if let Some(ident) = ident {
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

                // TODO: 引数の型チェック
                return match self.functions.get(&ident) {
                    Some(func) => Expr::Call(ident, func.return_type.clone(), args),
                    None => {
                        self.add_error("関数が見つかりません");
                        Expr::Invalid
                    }
                };
            } else {
                // 変数
                return match (self.variables.get(&ident), self.global_variables.get(&ident)) {
                    (Some(variable), _) | (_, Some(variable)) => Expr::Variable(variable.clone()),
                    _ => {
                        self.add_error_token(&format!("変数 \"{}\" が見つかりません", ident), self.pos - 1);
                        Expr::Invalid
                    },
                };
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

    fn parse_postfix(&mut self) -> Expr {
        let expr = self.parse_term();
        if self.consume(TokenKind::Lbracket) {
            // 配列の添字
            let index = self.parse_expr();
            let expr = Expr::Dereference(Box::new(Expr::Infix(Infix::Add, Box::new(expr), Box::new(index))));
            expect!(self, TokenKind::Rbracket);
            expr
        } else {
            expr
        }
    }

    fn parse_unary(&mut self) -> Expr {
        match self.tokens[self.pos].kind {
            TokenKind::Add => {
                self.pos += 1;
                self.parse_postfix()
            },
            TokenKind::Sub => {
                self.pos += 1;
                Expr::Infix(Infix::Sub, Box::new(Expr::Literal(Literal::Number(0))), Box::new(self.parse_postfix()))
            },
            TokenKind::SizeOf => {
                self.pos += 1;
                let expr = self.parse_expr();
                let ty = expr.get_type();
                match ty {
                    Some(ty) => Expr::Literal(Literal::Number(ty.get_size() as i32)),
                    None => {
                        self.add_error("型を識別できませんでした");
                        Expr::Invalid
                    },
                }
            },
            TokenKind::Asterisk => {
                self.pos += 1;
                let expr = self.parse_unary();
                match expr.get_type() {
                    Some(Type::Pointer(_)) => Expr::Dereference(Box::new(expr)),
                    _ => {
                        self.add_error("ポインタではない値を参照外しすることはできません");
                        Expr::Invalid
                    }
                }
            },
            TokenKind::Ampersand => {
                self.pos += 1;
                let ident = self.expect_ident();
                if let Some(ident) = ident {
                    match self.variables.get(&ident) {
                        Some(variable) => Expr::Address(variable.clone()),
                        None => {
                            self.add_error_token(&format!("変数 \"{}\" が見つかりません", ident), self.pos - 1);
                            Expr::Invalid
                        },
                    }
                } else {
                    self.add_error("変数ではありません");
                    Expr::Invalid
                }
            },
            _ => {
                self.parse_postfix()
            }
        }
    }

    fn parse_mul(&mut self) -> Expr {
        let mut expr = self.parse_unary();

        loop {
            if self.consume(TokenKind::Asterisk) {
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
        // 変数定義
        let variable = self.expect_define();
        if let Some(variable) = variable {
            expect!(self, TokenKind::Semicolon);
            return Stmt::Define(variable);
        }

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
        let ty = self.expect_type();
        return if let Some(ty) = ty {
            let ident = self.expect_ident();
            if let Some(ident) = ident {
                if self.consume(TokenKind::Lparen) {
                    // 関数定義
                    self.stack_size = 0;

                    // 引数
                    self.variables = HashMap::<String, Variable>::new();
                    loop {
                        let _ = self.expect_define();

                        if self.consume(TokenKind::Rparen) {
                            break;
                        } else if !self.consume(TokenKind::Comma) {
                            self.add_error("',' か ')' ではないトークンです");
                            break;
                        }
                    }

                    let mut args: Vec<Variable> = self.variables.clone().into_iter().map(|(_, v)| v).collect();
                    args.sort_by_key(|v| {
                        match v.location {
                            Location::Local(offset) => offset,
                            _ => panic!("引数がグローバル変数です"),
                        }
                    });

                    self.functions.insert(ident.clone(), Function::new(Type::Int, self.variables.clone().into_iter().map(|(_, v)| v).collect()));

                    let stmt = self.parse_stmt();
                    match stmt {
                        Stmt::Block(_) => {},
                        _ => self.add_error("ブロックではありません"),
                    };

                    Some(Declaration::Func(ident, args, self.stack_size, stmt))
                } else {
                    // グローバル変数定義

                    // 添字演算子があったら配列型にする
                    let mut ty = ty;
                    if let Some(size) = self.expect_subscript() {
                        ty = Type::Array(Box::new(ty), size);
                    }

                    expect!(self, TokenKind::Semicolon);

                    let variable = Variable::new(ty, Location::Global(ident.clone()));
                    self.global_variables.insert(ident.clone(), variable.clone());
                    Some(Declaration::GlobalVariable(variable))
                }
            } else {
                self.add_error("識別子ではありません");
                None
            }
        } else {
            self.add_error("型ではありません");
            None
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut declarations = Vec::new();
        while self.tokens[self.pos].kind != TokenKind::EOF {
            if let Some(declaration) = self.parse_declaration() {
                declarations.push(declaration);
            }
        }

        Program{
            declarations,
            global_variables: self.global_variables.clone(),
        }
    }
}
