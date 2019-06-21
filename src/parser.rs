use crate::tokenizer::{Token, TokenKind};
use std::collections::HashMap;
use std::convert::TryInto;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Char,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn get_size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Char => 1,
            Type::Pointer(_) => 8,
            Type::Array(ty, size) => ty.get_size() * size,
        }
    }

    // selfはtyに代入できるかどうか
    pub fn can_assign_to(&self, ty: &Self) -> bool {
        match (ty, self) {
            (Type::Pointer(box ty1), Type::Array(box ty2, _)) => ty1 == ty2,
            (ty1, ty2) if ty1.is_number() && ty2.is_number() => true,
            (ty1, ty2) => ty1 == ty2,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Type::Int | Type::Char | Type::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Char => "char".to_string(),
            Type::Pointer(ty) => format!("{}*", ty.to_string()),
            Type::Array(ty, size) => format!("{}[{}]", ty.to_string(), size),
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
    String(usize),
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
    Initializer(Vec<Expr>),
    Invalid,
}

impl Expr {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::Literal(Literal::Number(_)) => Some(Type::Int),
            Expr::Literal(Literal::String(_)) => Some(Type::Pointer(Box::new(Type::Char))),
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
            Expr::Invalid | Expr::Initializer(_) => None,
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
    Define(Variable, Option<Expr>),
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
    GlobalVariable(Variable, Option<Expr>), // 変数名, 変数, 初期化式
}

#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub global_variables: Vec<Variable>,
    pub string_list: Vec<String>,
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
    string_list: Vec<String>,
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
            string_list: Vec::new(),
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

    fn add_error_range(&mut self, msg: &str, start_pos: usize, end_pos: usize) {
        let start_token = &self.tokens[start_pos];
        let end_token = &self.tokens[end_pos];
        self.errors.push(ParseError {
            start_line: start_token.start_line,
            start_col: start_token.start_col,
            end_line: end_token.end_line,
            end_col: end_token.end_col,
            message: String::from(msg),
        });
    }

    fn add_error_token(&mut self, msg: &str, pos: usize) {
        self.add_error_range(msg, pos, pos);
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

    fn expect_define(&mut self, array_as_pointer: bool) -> Option<Variable> {
        let ty = self.expect_type()?;
        let ident = self.expect_ident();
        match ident {
            Some(ident) => {
                let mut ty = ty;
                if let Some(num) = self.expect_subscript() {
                    ty = Type::Array(Box::new(ty), num);
                }

                let variable = self.define_variable(&ident, &ty, array_as_pointer);
                Some(variable)
            },
            None => {
                self.add_error("識別子ではありません");
                None
            },
        }
    }

    fn define_variable(&mut self, ident: &str, ty: &Type, array_as_pointer: bool) -> Variable {
        let ty = match ty {
            Type::Array(ty, _) if array_as_pointer => Type::Pointer(ty.clone()),
            ty => ty.clone(),
        };
        let size = ty.get_size();

        self.stack_size += size;
        // アラインメント
        let padding = size - self.stack_size % size;
        if padding != size {
            self.stack_size += padding;
        }

        let variable = Variable::new(ty.clone(), Location::Local(self.stack_size));
        self.variables.insert(ident.to_string(), variable.clone());
        variable
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
            TokenKind::Char => Type::Char,
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
                    None => Expr::Call(ident, Type::Int, args),
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
            TokenKind::String(ref s) => {
                self.pos += 1;
                self.string_list.push(s.clone());
                Expr::Literal(Literal::String(self.string_list.len() - 1))
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
                let expr = self.parse_unary();
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
                    match (self.variables.get(&ident), self.global_variables.get(&ident)) {
                        (Some(variable), _) | (_, Some(variable)) => Expr::Address(variable.clone()),
                        _ => {
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
            let expr1_ty = expr.get_type();
            let start_pos = self.pos;

            let expr2 = self.parse_assign();
            let expr2_ty = expr2.get_type();
            expr = Expr::Assign(Box::new(expr), Box::new(expr2));

            // 型チェック
            match (expr1_ty, expr2_ty) {
                (Some(ty1), Some(ty2)) => {
                    if !ty2.can_assign_to(&ty1) {
                        self.add_error_range(&format!("\"{}\" は \"{}\" に代入できません", ty1.to_string(), ty2.to_string()), start_pos, self.pos - 1);
                    }
                },
                _ => {},
            }
        }

        expr
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assign()
    }

    fn parse_initializer(&mut self) -> Expr {
        // { を消費
        self.consume(TokenKind::Lbrace);

        let expr_list: Vec<Expr> = if self.consume(TokenKind::Rbrace) {
            Vec::new()
        } else {
            let mut expr_list = Vec::new();
            loop {
                let expr = if let TokenKind::Lbrace = self.tokens[self.pos].kind {
                    self.parse_initializer()
                } else {
                    self.parse_expr()
                };

                expr_list.push(expr);

                if self.consume(TokenKind::Rbrace) {
                    break;
                } else if self.consume(TokenKind::EOF) {
                    self.add_error("{ に対応する } がありません");
                    break;
                }

                expect!(self, TokenKind::Comma);
            }
            expr_list
        };

        Expr::Initializer(expr_list)
    }

    fn parse_stmt(&mut self) -> Stmt {
        // 変数定義
        let variable = self.expect_define(false);
        if let Some(variable) = variable {
            // = があったら初期化式をパース
            let init_expr = if self.consume(TokenKind::Assign) {
                // { があったら初期化リストとしてパース
                if let TokenKind::Lbrace = self.tokens[self.pos].kind {
                    Some(self.parse_initializer())
                } else {
                    let start_pos = self.pos;
                    let expr = self.parse_expr();

                    // 型チェック
                    if let Some(ty) = expr.get_type() {
                        if !ty.can_assign_to(&variable.ty) {
                            self.add_error_range("\"{}\" は \"{}\" 型の変数を初期化できません", start_pos, self.pos - 1);
                        }
                    }

                    Some(expr)
                }
            } else {
                None
            };

            expect!(self, TokenKind::Semicolon);
            return Stmt::Define(variable, init_expr);
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

                    // 引数をパース
                    self.variables = HashMap::<String, Variable>::new();
                    loop {
                        let _ = self.expect_define(true);

                        if self.consume(TokenKind::Rparen) {
                            break;
                        } else if !self.consume(TokenKind::Comma) {
                            self.add_error("',' か ')' ではないトークンです");
                            break;
                        }
                    }

                    // 引数をVec<Variable>に変換
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

                    // = があったら初期化式をパース
                    let init_expr = if self.consume(TokenKind::Assign) {
                        let start_pos = self.pos;
                        let expr = self.parse_add();
                        match expr {
                            Expr::Infix(Infix::Add, _, _) | Expr::Infix(Infix::Sub, _, _) | Expr::Literal(_) | Expr::Address(_) => {},
                            _ => {
                                self.add_error("リテラルとポインタ演算式以外の式は使用できません");
                            },
                        };

                        // 型チェック
                        let init_expr_ty = expr.get_type();
                        if let Some(init_expr_ty) = init_expr_ty {
                            if !init_expr_ty.can_assign_to(&ty) {
                                self.add_error_range(&format!("\"{}\" は \"{}\" に代入できません", init_expr_ty.to_string(), ty.to_string()), start_pos, self.pos - 1);
                            }
                        }

                        Some(expr)
                    } else {
                        None
                    };

                    expect!(self, TokenKind::Semicolon);

                    let variable = Variable::new(ty, Location::Global(ident.clone()));
                    self.global_variables.insert(ident.clone(), variable.clone());
                    Some(Declaration::GlobalVariable(variable, init_expr))
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

        Program {
            declarations,
            global_variables: self.global_variables.clone().into_iter().map(|(_, v)| v).collect(),
            string_list: self.string_list.clone(),
        }
    }
}
