use std::collections::HashMap;
use std::convert::TryInto;
use std::mem;

use crate::token::*;
use crate::ast::*;
use crate::error::{CompileError, Span};

#[derive(Debug)]
pub struct Parser {
    pub errors: Vec<CompileError>,

    global_variables: HashMap<String, Variable>,
    global_structures: HashMap<String, Type>,
    global_typedefs: HashMap<String, Type>,
    global_enums: HashMap<String, i64>,
    variables: HashMap<String, Variable>,
    structures: HashMap<String, Type>,
    typedefs: HashMap<String, Type>,
    enums: HashMap<String, i64>,

    string_list: Vec<String>,
    tokens: Vec<Token>,
    pos: usize,
    stack_size: usize,
    start_token_stack: Vec<Token>,
    cases: Vec<Expr>,
    switch_has_default: Vec<bool>,
}

macro_rules! expect {
    ($self: ident, $e: expr) => {
        if !$self.consume($e) {
            $self.add_error(&format!("'{}' ではないトークンです", $e.to_string()));
        }
    };
}

macro_rules! new_expr {
    ($self: ident, $kind: expr) => {
        Expr {
            kind: $kind,
            ty: None,
            span: $self.get_span(),
        }
    };
}

macro_rules! new_expr_peek {
    ($self: ident, $kind: expr) => {
        {
            let start_token = $self.start_token_stack.last().unwrap();
            let end_token = &$self.tokens[$self.pos - 1];
            let span = Span::from_two_token(&start_token, end_token);

            Expr {
                kind: $kind,
                ty: None,
                span,
            }
        }
    };
}

macro_rules! new_stmt {
    ($self: ident, $kind: expr) => {
        Stmt {
            kind: $kind,
            span: $self.get_span(),
        }
    };
}

macro_rules! new_initializer {
    ($self: ident, $kind: expr) => {
        Initializer {
            kind: $kind,
            span: $self.get_span(),
        }
    };
}

macro_rules! new_decl {
    ($self: ident, $kind: expr) => {
        Declaration {
            kind: $kind,
            span: $self.get_span(),
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
            global_structures: HashMap::new(),
            global_typedefs: HashMap::new(),
            global_enums: HashMap::new(),
            variables: HashMap::new(),
            structures: HashMap::new(),
            typedefs: HashMap::new(),
            enums: HashMap::new(),
            string_list: Vec::new(),
            stack_size: 0,
            start_token_stack: Vec::new(),
            cases: Vec::new(),
            switch_has_default: Vec::new(),
        }
    }

    fn push_start_token(&mut self) {
        self.start_token_stack.push(self.tokens[self.pos].clone());
    }

    fn push_prev_token(&mut self) {
        self.start_token_stack.push(self.tokens[self.pos - 1].clone());
    }
    
    fn pop_token(&mut self) {
        self.start_token_stack.pop().unwrap();
    }

    fn get_span(&mut self) -> Span {
        let start_token = self.start_token_stack.pop().unwrap();
        let end_token = &self.tokens[self.pos - 1];
        Span::from_two_token(&start_token, end_token)
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
        self.errors.push(CompileError {
            span: Span::new(start_token.start_line, start_token.start_col, end_token.end_line, end_token.end_col),
            msg: msg.to_string(),
        });
    }

    fn add_error_token(&mut self, msg: &str, pos: usize) {
        self.add_error_range(msg, pos, pos);
    }

    fn add_error(&mut self, msg: &str) {
        self.add_error_token(msg, self.pos);
    }

    fn invalid_expr(&mut self, msg: &str, offset: i32) -> ExprKind {
        self.add_error_token(msg, (self.pos as i32 + offset) as usize);
        ExprKind::Invalid
    }

    fn get_token(&mut self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn get_token_and_next(&mut self) -> &TokenKind {
        self.pos += 1;
        &self.tokens[self.pos - 1].kind
    }

    fn find_struct(&self, name: &str) -> Option<Type> {
        self.structures.get(name)
            .or_else(|| self.global_structures.get(name))
            .map(|ty| ty.clone())
    }

    fn find_var(&self, name: &str) -> Option<Variable> {
        self.variables.get(name)
            .or_else(|| self.global_variables.get(name))
            .map(|var| var.clone())
    }

    fn find_typedef(&self, name: &str) -> Option<&Type> {
        self.typedefs.get(name)
            .or_else(|| self.global_typedefs.get(name))
    }

    fn find_enum(&self, name: &str) -> Option<i64> {
        self.enums.get(name)
            .or_else(|| self.global_enums.get(name))
            .map(|n| n.clone())
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

    fn expect_define(&mut self, array_as_pointer: bool, allow_ident_omit: bool) -> Option<Variable> {
        let ty = self.expect_type(false, true)?;
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
            None if !allow_ident_omit => {
                self.add_error("識別子ではありません");
                None
            },
            None => Some(Variable::new(ty, Location::Global(String::new()))),
        }
    }

    fn define_variable(&mut self, ident: &str, ty: &Type, array_as_pointer: bool) -> Variable {
        let ty = match ty {
            Type::Array(ty, _) if array_as_pointer => Type::Pointer(ty.clone()),
            ty => ty.clone(),
        };

        let size = ty.get_size();
        self.stack_size += size;
        self.stack_size = align(self.stack_size, &ty);

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

    fn parse_type_struct(&mut self, is_global: bool) -> Option<Type> {
        // "struct" を消費
        self.pos += 1;

        // 名前
        let name = if let TokenKind::Ident(ref name) = self.tokens[self.pos].kind {
            self.pos += 1;
            Some(name.clone())
        } else {
            None
        };

        if self.consume(TokenKind::Lbrace) {
            // '{' だったら定義
            let mut members = Vec::new();

            loop {
                if self.consume(TokenKind::Rbrace) {
                    break;
                }

                if let Some(ty) = self.expect_type(is_global, true) {
                    if let Some(ident) = self.expect_ident() {
                        members.push((ident, ty));
                    } else {
                        self.add_error("メンバ名ではありません");
                    }
                } else {
                    self.add_error("型ではありません");
                }

                if self.consume(TokenKind::Rbrace) {
                    break;
                } else if !self.consume(TokenKind::Semicolon) {
                    self.add_error("',' ではありません");
                    break;
                }
            }

            // expect_type内でself.posが1足されてしまう
            self.pos -= 1;

            let ty = Type::new_structure(members);

            // 名前が指定されていたら構造体マップに追加する
            if let Some(name) = name {
                // 構造体マップ
                let structures = if is_global { &mut self.global_structures } else { &mut self.structures };
                structures.insert(name, ty.clone());
            }

            Some(ty)
        } else if let Some(name) = name {
            // expect_type内でself.posが1足されてしまう
            self.pos -= 1;

            // { ではなかったら構造体マップから探す
            if let Some(ty) = self.find_struct(&name) {
                Some(ty.clone())
            } else {
                self.add_error("構造体が見つかりません");
                None
            }
        } else {
            self.add_error("'{' か識別子ではありません");
            None
        }
    }

    fn expect_type(&mut self, is_global: bool, parse_pointer: bool) -> Option<Type> {
        let ty = match self.tokens[self.pos].kind {
            TokenKind::Int => Type::Int,
            TokenKind::Char => Type::Char,
            TokenKind::Short => Type::Short,
            TokenKind::Long => Type::Long,
            TokenKind::Void => Type::Void,
            TokenKind::Struct => match self.parse_type_struct(is_global) {
                Some(ty) => ty,
                None => return None,
            },
            TokenKind::Const => {
                self.pos += 1;
                match self.expect_type(is_global, false) {
                    Some(ty) => {
                        self.pos -= 1;
                        Type::Const(Box::new(ty))
                    },
                    None => {
                        self.add_error("型ではありません");
                        return None;
                    },
                }
            },
            TokenKind::Ident(ref name) => self.find_typedef(name)?.clone(),
            _ => return None,
        };

        self.pos += 1;

        if parse_pointer {
            Some(self.parse_pointer(ty))
        } else {
            Some(ty)
        }
    }

    fn parse_string(&mut self, s: String) -> ExprKind {
        let mut s = s;
        while let TokenKind::String(new_s) = self.get_token() {
            s += new_s;
            self.pos += 1;
        }

        self.string_list.push(s);
        ExprKind::Literal(Literal::String(self.string_list.len() - 1))
    }

    fn parse_call(&mut self, ident: String) -> ExprKind {
        // 引数をパース
        let mut args = Vec::<Expr>::new();
        
        if !self.consume(TokenKind::Rparen) {
            loop {
                if self.consume(TokenKind::Rparen) {
                    self.add_error_token("末尾のカンマです", self.pos - 2);
                    break;
                }

                args.push(self.parse_expr());
                if self.consume(TokenKind::Rparen) {
                    break;
                } else if self.consume(TokenKind::EOF) {
                    self.add_error_token("開きカッコに対応する閉じカッコがありません", self.pos - 1);
                    break;
                }

                expect!(self, TokenKind::Comma);
            }
        }

        ExprKind::Call(ident, args)
    }

    fn parse_var(&mut self, ident: String) -> ExprKind {
        if let Some(num) = self.find_enum(&ident) {
            return ExprKind::Literal(Literal::Number(num as i32));
        }

        // 変数マップから探す
        match self.find_var(&ident) {
            Some(var) => ExprKind::Variable(var.clone()),
            _ => self.invalid_expr("変数が見つかりません", -1),
        }
    }

    fn parse_member_access(&mut self, expr: Expr, is_arrow: bool) -> ExprKind {
        // メンバ名をパース
        if let Some(member_name) = self.expect_ident() {
            if is_arrow {
                ExprKind::MemberAccess(Box::new(new_expr_peek!(self, ExprKind::Dereference(Box::new(expr)))), member_name)
            } else {
                ExprKind::MemberAccess(Box::new(expr), member_name)
            }
        } else {
            self.invalid_expr("メンバ名ではありません", 0)
        }
    }

    fn parse_var_or_call(&mut self, ident: String) -> ExprKind {
        if self.consume(TokenKind::Lparen) {
            // 識別子の直後のトークンが開きカッコだったら関数呼び出としてパースする
            self.parse_call(ident)
        } else {
            // 開きカッコではなかったら変数
            self.parse_var(ident)
        }
    }

    fn parse_term(&mut self) -> Expr {
        self.push_start_token();

        let mut expr = match self.get_token_and_next().clone() {
            TokenKind::Lparen => {
                self.pop_token();
                let expr = self.parse_expr();
                expect!(self, TokenKind::Rparen);
                expr
            },
            TokenKind::Number(num) => new_expr!(self, ExprKind::Literal(Literal::Number(num))),
            TokenKind::String(s) => new_expr!(self, self.parse_string(s)),
            TokenKind::Ident(ident) => new_expr!(self, self.parse_var_or_call(ident)),
            _ => new_expr!(self, self.invalid_expr("数値でも開きカッコでもないトークンです", 0)),
        };

        // 現在のトークンがドットだったらメンバアクセスとしてパースする
        if self.consume(TokenKind::Dot) {
            self.push_start_token();
            expr = new_expr!(self, self.parse_member_access(expr, false));
        } else if self.consume(TokenKind::Arrow) {
            self.push_start_token();
            expr = new_expr!(self, self.parse_member_access(expr, true));
        }

        expr
    }

    fn parse_postfix(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_term();

        let two_tokens = (self.get_token().clone(), self.tokens[self.pos + 1].kind.clone());
        if let (TokenKind::Add, TokenKind::Add) = two_tokens {
            self.pos += 2;
            expr = new_expr_peek!(self, ExprKind::Increment(Box::new(expr), true));
        } else if let (TokenKind::Sub, TokenKind::Sub) = two_tokens {
            self.pos += 2;
            expr = new_expr_peek!(self, ExprKind::Decrement(Box::new(expr), true));
        }

        // 添字演算子
        loop {
            if self.consume(TokenKind::Lbracket) {
                let index = self.parse_expr();
                expr = new_expr_peek!(self, ExprKind::Dereference(Box::new(
                            new_expr_peek!(self, ExprKind::Infix(Infix::Add, Box::new(expr), Box::new(index))))));
                expect!(self, TokenKind::Rbracket);
            } else {
                break;
            }
        }

        self.pop_token();
        expr
    }

    fn parse_unary_minus(&mut self) -> Expr {
        self.push_prev_token();
        // 0 - n にする
        new_expr!(self, ExprKind::Infix(Infix::Sub,
                        Box::new(new_expr_peek!(self, ExprKind::Literal(Literal::Number(0)))),
                        Box::new(self.parse_postfix())))
    }

    fn parse_unary_sizeof(&mut self) -> Expr {
        self.push_prev_token();
        new_expr!(self, ExprKind::SizeOf(Box::new(self.parse_unary())))
    }

    fn parse_unary_deref(&mut self) -> Expr {
        self.push_prev_token();
        new_expr!(self, ExprKind::Dereference(Box::new(self.parse_unary())))
    }

    fn parse_unary_address(&mut self) -> Expr {
        self.push_prev_token();
        let ident = self.expect_ident();
        let kind = if let Some(ident) = ident {
            match self.find_var(&ident) {
                Some(var) => ExprKind::Address(var.clone()),
                _ => self.invalid_expr("変数が見つかりません", -1),
            }
        } else {
            self.invalid_expr("変数名ではありません", 0)
        };
        new_expr!(self, kind)
    }

    fn parse_increment(&mut self) -> Expr {
        self.push_prev_token();
        self.pos += 1;
        new_expr!(self, ExprKind::Increment(Box::new(self.parse_postfix()), false))
    }

    fn parse_decrement(&mut self) -> Expr {
        self.push_prev_token();
        self.pos += 1;
        new_expr!(self, ExprKind::Decrement(Box::new(self.parse_postfix()), false))
    }

    fn parse_unary(&mut self) -> Expr {
        let next_token = self.tokens[self.pos + 1].kind.clone();
        match self.get_token_and_next() {
            TokenKind::Add if next_token == TokenKind::Add => self.parse_increment(),
            TokenKind::Add => self.parse_postfix(),
            TokenKind::Sub if next_token == TokenKind::Sub => self.parse_decrement(),
            TokenKind::Sub => self.parse_unary_minus(),
            TokenKind::SizeOf => self.parse_unary_sizeof(),
            TokenKind::Asterisk => self.parse_unary_deref(),
            TokenKind::Ampersand => self.parse_unary_address(),
            TokenKind::BitNot => {
                self.push_prev_token();
                new_expr!(self, ExprKind::BitNot(Box::new(self.parse_postfix())))
            },
            _ => {
                self.pos -= 1;
                self.parse_postfix()
            }
        }
    }

    fn parse_mul(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_unary();

        loop {
            if self.consume(TokenKind::Asterisk) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Mul, Box::new(expr), Box::new(self.parse_unary())));
            } else if self.consume(TokenKind::Div) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Div, Box::new(expr), Box::new(self.parse_unary())));
            } else if self.consume(TokenKind::Mod) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Mod, Box::new(expr), Box::new(self.parse_unary())));
            } else {
                self.pop_token();
                return expr;
            }
        }
    }

    fn parse_add(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_mul(); 

        loop {
            if self.consume(TokenKind::Add) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Add, Box::new(expr), Box::new(self.parse_mul())));
            } else if self.consume(TokenKind::Sub) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Sub, Box::new(expr), Box::new(self.parse_mul())));
            } else {
                self.pop_token();
                return expr;
            }
        }
    }

    fn parse_shift(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_add(); 

        loop {
            if self.consume(TokenKind::Shl) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Shl, Box::new(expr), Box::new(self.parse_add())));
            } else if self.consume(TokenKind::Shr) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Shr, Box::new(expr), Box::new(self.parse_add())));
            } else {
                self.pop_token();
                return expr;
            }
        }
    }

    fn parse_relational(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_shift(); 

        loop {
            if self.consume(TokenKind::LessThan) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::LessThan, Box::new(expr), Box::new(self.parse_shift())));
            } else if self.consume(TokenKind::LessThanOrEqual) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::LessThanOrEqual, Box::new(expr), Box::new(self.parse_shift())));
            } else if self.consume(TokenKind::GreaterThan) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::LessThan, Box::new(self.parse_shift()), Box::new(expr)));
            } else if self.consume(TokenKind::GreaterThanOrEqual) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::LessThanOrEqual, Box::new(self.parse_shift()), Box::new(expr)));
            } else {
                self.pop_token();
                return expr;
            }
        }
    }

    fn parse_equality(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_relational(); 

        loop {
            if self.consume(TokenKind::Equal) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::Equal, Box::new(expr), Box::new(self.parse_relational())));
            } else if self.consume(TokenKind::NotEqual) {
                expr = new_expr_peek!(self, ExprKind::Infix(Infix::NotEqual, Box::new(expr), Box::new(self.parse_relational())));
            } else {
                self.pop_token();
                return expr;
            }
        }
    }

    fn parse_bit_and(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_equality();
        while self.consume(TokenKind::Ampersand) {
            expr = new_expr_peek!(self, ExprKind::Infix(Infix::BitAnd, Box::new(expr), Box::new(self.parse_equality())));
        }

        self.pop_token();
        expr
    }

    fn parse_bit_xor(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_bit_and();
        while self.consume(TokenKind::Xor) {
            expr = new_expr_peek!(self, ExprKind::Infix(Infix::BitXor, Box::new(expr), Box::new(self.parse_bit_and())));
        }

        self.pop_token();
        expr
    }

    fn parse_bit_or(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_bit_xor();
        while self.consume(TokenKind::Or) {
            expr = new_expr_peek!(self, ExprKind::Infix(Infix::BitOr, Box::new(expr), Box::new(self.parse_bit_xor())));
        }

        self.pop_token();
        expr
    }

    fn parse_assign(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_bit_or();
        if self.consume(TokenKind::Assign) {
            expr = new_expr_peek!(self, ExprKind::Assign(Box::new(expr), Box::new(self.parse_assign())));
        }

        self.pop_token();
        expr
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_assign()
    }

    fn parse_initializer(&mut self) -> Initializer {
        self.push_start_token();
        // { だったら初期化リストとしてパース
        if self.consume(TokenKind::Lbrace) {
            let initializers: Vec<Initializer> = if self.consume(TokenKind::Rbrace) {
                Vec::new()
            } else {
                let mut initializers = Vec::new();
                loop {
                    if self.consume(TokenKind::Rbrace) {
                        self.add_error_token("末尾のカンマです", self.pos - 2);
                        break;
                    }

                    initializers.push(self.parse_initializer());

                    if self.consume(TokenKind::Rbrace) {
                        break;
                    } else if self.consume(TokenKind::EOF) {
                        self.add_error("{ に対応する } がありません");
                        break;
                    }

                    expect!(self, TokenKind::Comma);
                }
                initializers
            };

            new_initializer!(self, InitializerKind::List(initializers))
        } else {
            new_initializer!(self, InitializerKind::Expr(self.parse_expr()))
        }
    }

    fn parse_return_stmt(&mut self) -> StmtKind {
        let stmt = StmtKind::Return(self.parse_expr());
        expect!(self, TokenKind::Semicolon);
        stmt
    }

    fn parse_if_stmt(&mut self) -> StmtKind {
        expect!(self, TokenKind::Lparen);

        let expr = self.parse_expr();
        if !self.consume(TokenKind::Rparen) {
            self.add_error("開きカッコに対応する閉じカッコがありません");
        }

        let if_stmt = Box::new(self.parse_stmt());
        let else_stmt = if self.consume(TokenKind::Else) { Some(Box::new(self.parse_stmt())) } else { None };
        StmtKind::If(expr, if_stmt, else_stmt)
    }

    fn parse_while_stmt(&mut self) -> StmtKind {
        expect!(self, TokenKind::Lparen);

        let expr = self.parse_expr();
        if !self.consume(TokenKind::Rparen) {
            self.add_error("開きカッコに対応する閉じカッコがありません");
        }

        StmtKind::While(expr, Box::new(self.parse_stmt()))
    }

    fn parse_for_stmt(&mut self) -> StmtKind {
        expect!(self, TokenKind::Lparen);

        // 初期化
        self.push_start_token();
        let init_stmt = if let Some(stmt) = self.expect_define_stmt() {
            Some(Box::new(new_stmt!(self, stmt)))
        } else if !self.consume(TokenKind::Semicolon) {
            let expr = self.parse_expr();
            expect!(self, TokenKind::Semicolon);
            Some(Box::new(new_stmt!(self, StmtKind::Expr(expr))))
        } else {
            self.pop_token();
            None
        };
        
        // 条件
        let cond = if !self.consume(TokenKind::Semicolon) {
            let expr = self.parse_expr();
            expect!(self, TokenKind::Semicolon);
            Some(expr)
        } else {
            None
        };

        // i++ するところ
        let expr3 = if !self.consume(TokenKind::Semicolon) {
            Some(self.parse_expr())
        } else {
            None
        };

        expect!(self, TokenKind::Rparen);

        StmtKind::For(init_stmt, cond, expr3, Box::new(self.parse_stmt()))
    }

    fn parse_block_stmt(&mut self) -> StmtKind {
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

        StmtKind::Block(stmt_list)
    }

    fn parse_expr_stmt(&mut self) -> StmtKind {
        let stmt = StmtKind::Expr(self.parse_expr());
        expect!(self, TokenKind::Semicolon);
        stmt
    }

    fn expect_define_stmt(&mut self) -> Option<StmtKind> {
        let ty = self.expect_type(false, true)?;
        let ident = self.expect_ident().or_else(|| { self.add_error("識別子ではありません"); None })?;

        // 添字演算子があったら配列型にする
        let mut ty = ty;
        // 要素数を省略した配列の要素の型へのポインタ
        let first_ty = &mut ty as *mut Type;
        // 要素数を省略した時に出すエラーの位置
        let pos = self.pos + 1;

        // 要素数を省略しているかどうか
        let is_omitted = if let (TokenKind::Lbracket, TokenKind::Rbracket) = (self.get_token().clone(), &self.tokens[self.pos + 1].kind) {
            self.pos += 2;
            true
        } else {
            false
        };

        // 添字演算子をパース
        while let Some(size) = self.expect_subscript() {
            ty = Type::Array(Box::new(ty), size);
        }

        // = があったら初期化式をパース
        let initializer = if self.consume(TokenKind::Assign) {
            Some(self.parse_initializer())
        } else {
            None
        };

        // 要素数を省略していたら初期化リスト内の式の数を要素数にする
        if is_omitted {
            if let Some(Initializer { kind: InitializerKind::List(initializers), .. }) = &initializer {
                unsafe {
                    *first_ty = Type::Array(Box::new((*first_ty).clone()), initializers.len());
                }
            } else {
                self.add_error_token("要素数は省略できません", pos);
            }
        }

        expect!(self, TokenKind::Semicolon);

        let var = self.define_variable(&ident, &ty, false);
        Some(StmtKind::Define(var, initializer))
    }

    fn parse_typedef(&mut self, is_global: bool) {
        let ty = match self.expect_type(is_global, true) {
            Some(ty) => ty,
            None => {
                self.add_error("型ではありません");
                return;
            },
        };

        if let Some(name) = self.expect_ident() {
            let typedefs = if is_global { &mut self.global_typedefs } else { &mut self.typedefs };
            typedefs.insert(name, ty);
        } else {
            self.add_error("識別子ではありません");
        }

        expect!(self, TokenKind::Semicolon);
    }

    fn parse_enum(&mut self, is_global: bool) {
        expect!(self, TokenKind::Lbrace);

        let mut n = 0i64;

        if !self.consume(TokenKind::Rbrace) {
            loop {
                if let Some(ident) = self.expect_ident() {
                    if self.consume(TokenKind::Assign) {
                        if let TokenKind::Number(num) = self.get_token_and_next() {
                            n = *num as i64;
                        } else {
                            self.add_error_token("", self.pos - 1);
                        }
                    }

                    let enums = if is_global { &mut self.global_enums } else { &mut self.enums };
                    enums.insert(ident.clone(), n);
                    n += 1;
                } else {
                    self.add_error("識別子ではありません");
                    break;
                }

                if self.consume(TokenKind::Rbrace) {
                    break;
                } else if !self.consume(TokenKind::Comma) {
                    self.add_error("',' ではありません");
                    break;
                }
            }
        }

        expect!(self, TokenKind::Semicolon);
    }

    fn parse_switch_stmt(&mut self) -> StmtKind {
        expect!(self, TokenKind::Lparen);
        let expr = self.parse_expr();
        expect!(self, TokenKind::Rparen);

        self.switch_has_default.push(false);
        let stmt = self.parse_stmt();

        let has_default = *self.switch_has_default.last().unwrap();
        self.switch_has_default.pop().unwrap();

        // self.casesの取得とリセット
        let cases = mem::replace(&mut self.cases, Vec::new());
        StmtKind::Switch(expr, cases, Box::new(stmt), has_default)
    }

    fn parse_case_stmt(&mut self) -> StmtKind {
        let expr = self.parse_expr();
        self.cases.push(expr.clone());
        expect!(self, TokenKind::Colon);
        StmtKind::Case(expr)
    }

    fn parse_default_stmt(&mut self) -> StmtKind {
        match self.switch_has_default.last_mut() {
            Some(has_default) => *has_default = true,
            None => self.add_error("switch文の中以外では使用できません"),
        };
        expect!(self, TokenKind::Colon);
        StmtKind::Default
    }

    fn parse_stmt(&mut self) -> Stmt {
        self.push_start_token();
        if let Some(kind) = self.expect_define_stmt() {
            return new_stmt!(self, kind);
        }

        let kind = match self.get_token_and_next() {
            TokenKind::Return => self.parse_return_stmt(),
            TokenKind::If => self.parse_if_stmt(),
            TokenKind::While => self.parse_while_stmt(),
            TokenKind::For => self.parse_for_stmt(),
            TokenKind::Switch => self.parse_switch_stmt(),
            TokenKind::Case => self.parse_case_stmt(),
            TokenKind::Default => self.parse_default_stmt(),
            TokenKind::Break => {
                expect!(self, TokenKind::Semicolon);
                StmtKind::Break
            },
            TokenKind::Continue => {
                expect!(self, TokenKind::Semicolon);
                StmtKind::Continue
            },
            TokenKind::Lbrace => self.parse_block_stmt(),
            TokenKind::Typedef => {
                self.parse_typedef(false);
                StmtKind::Block(Vec::new())
            },
            TokenKind::Enum => {
                self.parse_enum(false);
                StmtKind::Block(Vec::new())
            },
            _ => {
                self.pos -= 1;
                self.parse_expr_stmt()
            }
        };
        new_stmt!(self, kind)
    }

    fn parse_func_decl(&mut self, ty: Type, ident: String, is_static: bool) -> Option<DeclarationKind> {
        self.stack_size = 0;

        // 引数をパース
        self.variables = HashMap::<String, Variable>::new();
        if !self.consume(TokenKind::Rparen) {
            loop {
                let _ = self.expect_define(true, true);

                if self.consume(TokenKind::Rparen) {
                    break;
                } else if !self.consume(TokenKind::Comma) {
                    self.add_error("',' か ')' ではないトークンです");
                    break;
                }
            }
        }

        // 引数をVec<Variable>に変換
        let mut args: Vec<Variable> = self.variables.clone().into_iter().map(|(_, v)| v).collect();
        let mut is_prototype = false;
        args.sort_by_key(|v| {
            match v.location {
                Location::Local(offset) => offset,
                _ => { is_prototype = true; 0 },
            }
        });

        if self.consume(TokenKind::Semicolon) {
            // セミコロンだったらプロトタイプ宣言
            Some(DeclarationKind::Prototype(ident, ty, args.into_iter().map(|var| var.ty).collect()))
        } else if is_prototype {
            self.add_error("引数名を省略しています");
            None
        } else {
            let stmt = self.parse_stmt();
            Some(DeclarationKind::Func(ident, ty, args, self.stack_size, stmt, is_static))
        }
    }

    fn parse_global_var_decl(&mut self, ty: Type, ident: String, is_static: bool) -> Option<DeclarationKind> {
        // 添字演算子があったら配列型にする
        let mut ty = ty;
        let first_ty = &mut ty as *mut Type;
        let pos = self.pos + 1;

        // 要素数を省略しているかどうか
        let is_omitted = if let (TokenKind::Lbracket, TokenKind::Rbracket) = (self.get_token().clone(), &self.tokens[self.pos + 1].kind) {
            self.pos += 2;
            true
        } else {
            false
        };

        // 添字演算子をパース
        while let Some(size) = self.expect_subscript() {
            ty = Type::Array(Box::new(ty), size);
        }

        // = があったら初期化式をパース
        let initializer = if self.consume(TokenKind::Assign) {
            Some(self.parse_initializer())
        } else {
            None
        };

        // 要素数を省略していたら初期化リスト内の式の数を要素数にする
        if is_omitted {
            if let Some(Initializer { kind: InitializerKind::List(initializers), .. }) = &initializer {
                unsafe {
                    *first_ty = Type::Array(Box::new((*first_ty).clone()), initializers.len());
                }
            } else {
                self.add_error_token("要素数は省略できません", pos);
            }
        }

        expect!(self, TokenKind::Semicolon);

        let variable = Variable::new(ty, Location::Global(ident.clone()));
        self.global_variables.insert(ident.clone(), variable.clone());
        Some(DeclarationKind::GlobalVariable(variable, initializer, is_static))
    }

    pub fn parse_var_or_func_decl(&mut self, ty: Type, is_static: bool) -> Option<DeclarationKind> {
        // セミコロンだったら構造体などの定義
        if self.consume(TokenKind::Semicolon) {
            return None;
        }

        // 識別子
        if let TokenKind::Ident(ident) = self.get_token().clone() {
            self.pos += 1;
            if self.consume(TokenKind::Lparen) {
                // 識別子の次のトークンが開きカッコだったら関数定義としてパースする
                self.parse_func_decl(ty, ident, is_static)
            } else {
                // 開きカッコではなかったらグローバル変数定義としてパースする
                self.parse_global_var_decl(ty, ident, is_static)
            }
        } else {
            self.add_error("識別子ではありません");
            None
        }
    }

    pub fn parse_declaration(&mut self) -> Option<Declaration> {
        self.push_start_token();

        // staticがあるかどうか
        let is_static = self.consume(TokenKind::Static);

        let kind = match self.get_token() {
            TokenKind::Typedef => {
                self.pos += 1;
                self.parse_typedef(true);
                None
            },
            TokenKind::Enum => {
                self.pos += 1;
                self.parse_enum(true);
                None
            },
            TokenKind::Extern => {
                self.pos += 1;
                let decl = self.parse_declaration();
                match decl {
                    Some(decl) => Some(DeclarationKind::Extern(Box::new(decl))),
                    None => {
                        self.add_error("関数と変数にのみextern指定子を付けることができます");
                        None
                    },
                }
            },
            _ => if let Some(ty) = self.expect_type(false, true) {
                self.parse_var_or_func_decl(ty, is_static)
            } else {
                self.add_error("型ではありません");
                self.pos += 1;
                None
            },
        };

        if let Some(kind) = kind {
            Some(new_decl!(self, kind))
        } else {
            self.pop_token();
            None
        }
    }

    pub fn parse(mut self) -> Result<Program, Vec<CompileError>> {
        let mut declarations = Vec::new();
        while self.tokens[self.pos].kind != TokenKind::EOF {
            if let Some(declaration) = self.parse_declaration() {
                declarations.push(declaration);
            }
        }

        if !self.start_token_stack.is_empty() {
            for token in self.start_token_stack {
                println!("{:?}", token);
            }
            panic!("スタックにトークンが余っています");
        }

        if self.errors.is_empty() {
            Ok(Program {
                declarations,
                global_variables: self.global_variables.clone().into_iter().map(|(_, v)| v).collect(),
                string_list: self.string_list.clone(),
            })
        } else {
            Err(self.errors)
        }
    }
}
