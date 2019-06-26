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
}

#[derive(Debug, Clone)]
pub enum Infix {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
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
    BitNot(Box<Expr>),
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
            Expr::BitNot(expr) => expr.get_type(),
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

    fn invalid_expr(&mut self, msg: &str, offset: i32) -> Expr {
        self.add_error_token(msg, (self.pos as i32 + offset) as usize);
        Expr::Invalid
    }

    fn get_token(&mut self) -> TokenKind {
        self.tokens[self.pos].kind.clone()
    }

    fn get_token_and_next(&mut self) -> TokenKind {
        let kind = self.get_token();
        self.pos += 1;
        kind
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
                while let Some(num) = self.expect_subscript() {
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

    fn parse_string(&mut self, s: String) -> Expr {
        self.string_list.push(s);
        Expr::Literal(Literal::String(self.string_list.len() - 1))
    }

    fn parse_call(&mut self, ident: String) -> Expr {
        // 引数をパース
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

        match self.functions.get(&ident) {
            Some(func) => Expr::Call(ident, func.return_type.clone(), args),
            // 関数が見つからなかったら戻り値の型をintとする
            None => Expr::Call(ident, Type::Int, args),
        }
    }

    fn parse_var(&mut self, ident: String) -> Expr {
        // 変数マップから探す
        match (self.variables.get(&ident), self.global_variables.get(&ident)) {
            (Some(variable), _) | (_, Some(variable)) => Expr::Variable(variable.clone()),
            _ => self.invalid_expr(&format!("変数 \"{}\" が見つかりません", ident), -1),
        }
    }

    fn parse_var_or_call(&mut self, ident: String) -> Expr {
        if self.consume(TokenKind::Lparen) {
            // 識別子の直後のトークンが開きカッコだったら関数呼び出としてパースする
            self.parse_call(ident)
        } else {
            // 開きカッコではなかったら変数
            self.parse_var(ident)
        }
    }

    fn parse_term(&mut self) -> Expr {
        // 現在のトークンが識別子だったら変数か関数呼び出しとしてパースする
        let ident = self.expect_ident();
        if let Some(ident) = ident {
            return self.parse_var_or_call(ident);
        }

        match self.get_token_and_next() {
            TokenKind::Lparen => {
                let expr = self.parse_expr();
                expect!(self, TokenKind::Rparen);
                expr
            },
            TokenKind::Number(num) => Expr::Literal(Literal::Number(num)),
            TokenKind::String(s) => self.parse_string(s),
            _ => self.invalid_expr("数値でも開きカッコでもないトークンです", 0),
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_term();

        // 添字演算子
        loop {
            if self.consume(TokenKind::Lbracket) {
                let index = self.parse_expr();
                expr = Expr::Dereference(Box::new(Expr::Infix(Infix::Add, Box::new(expr), Box::new(index))));
                expect!(self, TokenKind::Rbracket);
            } else {
                break;
            }
        }

        expr
    }

    fn parse_unary_minus(&mut self) -> Expr {
        // 0 - n にする
        Expr::Infix(Infix::Sub, Box::new(Expr::Literal(Literal::Number(0))), Box::new(self.parse_postfix()))
    }

    fn parse_unary_sizeof(&mut self) -> Expr {
        let expr = self.parse_unary();
        let ty = expr.get_type();
        match ty {
            Some(ty) => Expr::Literal(Literal::Number(ty.get_size() as i32)),
            None => self.invalid_expr("型を識別できませんでした", 0),
        }
    }

    fn parse_unary_deref(&mut self) -> Expr {
        let expr = self.parse_unary();
        match expr.get_type() {
            Some(Type::Pointer(_)) => Expr::Dereference(Box::new(expr)),
            _ => self.invalid_expr("ポインタではない値を参照外しすることはできません", 0),
        }
    }

    fn parse_unary_address(&mut self) -> Expr {
        let ident = self.expect_ident();
        if let Some(ident) = ident {
            match (self.variables.get(&ident), self.global_variables.get(&ident)) {
                (Some(variable), _) | (_, Some(variable)) => Expr::Address(variable.clone()),
                _ => self.invalid_expr(&format!("変数 \"{}\" が見つかりません", ident), -1),
            }
        } else {
            self.invalid_expr("変数ではありません", 0)
        }
    }

    fn parse_unary(&mut self) -> Expr {
        match self.get_token_and_next() {
            TokenKind::Add => self.parse_postfix(),
            TokenKind::Sub => self.parse_unary_minus(),
            TokenKind::SizeOf => self.parse_unary_sizeof(),
            TokenKind::Asterisk => self.parse_unary_deref(),
            TokenKind::Ampersand => self.parse_unary_address(),
            TokenKind::BitNot => Expr::BitNot(Box::new(self.parse_postfix())),
            _ => {
                self.pos -= 1;
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
            } else if self.consume(TokenKind::Mod) {
                expr = Expr::Infix(Infix::Mod, Box::new(expr), Box::new(self.parse_unary()));
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

    fn parse_shift(&mut self) -> Expr {
        let mut expr = self.parse_add(); 

        loop {
            if self.consume(TokenKind::Shl) {
                expr = Expr::Infix(Infix::Shl, Box::new(expr), Box::new(self.parse_add()));
            } else if self.consume(TokenKind::Shr) {
                expr = Expr::Infix(Infix::Shr, Box::new(expr), Box::new(self.parse_add()));
            } else {
                return expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expr {
        let mut expr = self.parse_shift(); 

        loop {
            if self.consume(TokenKind::LessThan) {
                expr = Expr::Infix(Infix::LessThan, Box::new(expr), Box::new(self.parse_shift()));
            } else if self.consume(TokenKind::LessThanOrEqual) {
                expr = Expr::Infix(Infix::LessThanOrEqual, Box::new(expr), Box::new(self.parse_shift()));
            } else if self.consume(TokenKind::GreaterThan) {
                expr = Expr::Infix(Infix::LessThan, Box::new(self.parse_shift()), Box::new(expr));
            } else if self.consume(TokenKind::GreaterThanOrEqual) {
                expr = Expr::Infix(Infix::LessThanOrEqual, Box::new(self.parse_shift()), Box::new(expr));
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

    fn parse_bit_and(&mut self) -> Expr {
        let mut expr = self.parse_equality();
        while self.consume(TokenKind::Ampersand) {
            expr = Expr::Infix(Infix::BitAnd, Box::new(expr), Box::new(self.parse_equality()));
        }
        expr
    }

    fn parse_bit_xor(&mut self) -> Expr {
        let mut expr = self.parse_bit_and();
        while self.consume(TokenKind::Xor) {
            expr = Expr::Infix(Infix::BitXor, Box::new(expr), Box::new(self.parse_bit_and()));
        }
        expr
    }

    fn parse_bit_or(&mut self) -> Expr {
        let mut expr = self.parse_bit_xor();
        while self.consume(TokenKind::Or) {
            expr = Expr::Infix(Infix::BitOr, Box::new(expr), Box::new(self.parse_bit_xor()));
        }
        expr
    }

    fn parse_assign(&mut self) -> Expr {
        let mut expr = self.parse_bit_or();
        if self.consume(TokenKind::Assign) {
            expr = Expr::Assign(Box::new(expr), Box::new(self.parse_assign()));
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

    fn parse_return_stmt(&mut self) -> Stmt {
        let stmt = Stmt::Return(self.parse_expr());
        expect!(self, TokenKind::Semicolon);
        stmt
    }

    fn parse_if_stmt(&mut self) -> Stmt {
        expect!(self, TokenKind::Lparen);

        let expr = self.parse_expr();
        if !self.consume(TokenKind::Rparen) {
            self.add_error("開きカッコに対応する閉じカッコがありません");
        }

        let if_stmt = Box::new(self.parse_stmt());
        let else_stmt = if self.consume(TokenKind::Else) { Some(Box::new(self.parse_stmt())) } else { None };
        Stmt::If(expr, if_stmt, else_stmt)
    }

    fn parse_while_stmt(&mut self) -> Stmt {
        expect!(self, TokenKind::Lparen);

        let expr = self.parse_expr();
        if !self.consume(TokenKind::Rparen) {
            self.add_error("開きカッコに対応する閉じカッコがありません");
        }

        Stmt::While(expr, Box::new(self.parse_stmt()))
    }

    fn parse_for_stmt(&mut self) -> Stmt {
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
    }

    fn parse_block_stmt(&mut self) -> Stmt {
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
    }

    fn parse_expr_stmt(&mut self) -> Stmt {
        let stmt = Stmt::Expr(self.parse_expr());
        expect!(self, TokenKind::Semicolon);
        stmt
    }

    fn parse_stmt(&mut self) -> Stmt {
        // 変数定義
        let variable = self.expect_define(false);
        if let Some(variable) = variable {
            // = があったら初期化式をパース
            let init_expr = if self.consume(TokenKind::Assign) {
                // { があったら初期化リストとしてパース
                match self.get_token() {
                    TokenKind::Lbrace => Some(self.parse_initializer()),
                    _ => Some(self.parse_expr()),
                }
            } else {
                None
            };

            expect!(self, TokenKind::Semicolon);
            return Stmt::Define(variable, init_expr);
        }

        match self.get_token_and_next() {
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Lbrace => self.parse_block_stmt(),
            _ => {
                self.pos -= 1;
                self.parse_expr_stmt()
            }
        }
    }

    fn parse_func_decl(&mut self, ty: Type, ident: String) -> Declaration {
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

        self.functions.insert(ident.clone(), Function::new(ty, self.variables.clone().into_iter().map(|(_, v)| v).collect()));

        let stmt = self.parse_stmt();
        match stmt {
            Stmt::Block(_) => {},
            _ => self.add_error("ブロックではありません"),
        };

        Declaration::Func(ident, args, self.stack_size, stmt)
    }

    fn parse_global_var_decl(&mut self, ty: Type, ident: String) -> Option<Declaration> {
        // 添字演算子があったら配列型にする
        let mut ty = ty;
        while let Some(size) = self.expect_subscript() {
            ty = Type::Array(Box::new(ty), size);
        }

        // = があったら初期化式をパース
        let init_expr = if self.consume(TokenKind::Assign) {
            // { があったら初期化リストとしてパース
            match self.get_token() {
                TokenKind::Lbrace => Some(self.parse_initializer()),
                _ => Some(self.parse_add()),
            }
        } else {
            None
        };

        expect!(self, TokenKind::Semicolon);

        let variable = Variable::new(ty, Location::Global(ident.clone()));
        self.global_variables.insert(ident.clone(), variable.clone());
        Some(Declaration::GlobalVariable(variable, init_expr))
    }

    pub fn parse_declaration(&mut self) -> Option<Declaration> {
        // 型
        let ty = self.expect_type();
        if let Some(ty) = ty {
            // 識別子
            let ident = self.expect_ident();
            if let Some(ident) = ident {
                if self.consume(TokenKind::Lparen) {
                    // 識別子の次のトークンが開きカッコだったら関数定義としてパースする
                    Some(self.parse_func_decl(ty, ident))
                } else {
                    // 開きカッコではなかったらグローバル変数定義としてパースする
                    self.parse_global_var_decl(ty, ident)
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
