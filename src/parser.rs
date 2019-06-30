use std::collections::HashMap;
use std::convert::TryInto;

use crate::token::*;
use crate::ast::*;
use crate::error::{CompileError, Span};

#[derive(Debug)]
pub struct Parser {
    pub errors: Vec<CompileError>,
    global_variables: HashMap<String, Variable>,
    string_list: Vec<String>,
    variables: HashMap<String, Variable>,
    tokens: Vec<Token>,
    pos: usize,
    stack_size: usize,
    start_token_stack: Vec<Token>,
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
            string_list: Vec::new(),
            variables: HashMap::new(),
            stack_size: 0,
            start_token_stack: Vec::new(),
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

    fn expect_define(&mut self, array_as_pointer: bool, allow_ident_omit: bool) -> Option<Variable> {
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
        self.stack_size = align(self.stack_size, size);

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

    fn parse_string(&mut self, s: String) -> ExprKind {
        self.string_list.push(s);
        ExprKind::Literal(Literal::String(self.string_list.len() - 1))
    }

    fn parse_call(&mut self, ident: String) -> ExprKind {
        // 引数をパース
        let mut args = Vec::<Expr>::new();
        
        if !self.consume(TokenKind::Rparen) {
            loop {
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
        // 変数マップから探す
        match (self.variables.get(&ident), self.global_variables.get(&ident)) {
            (Some(variable), _) | (_, Some(variable)) => ExprKind::Variable(variable.clone()),
            _ => self.invalid_expr(&format!("変数 \"{}\" が見つかりません", ident), -1),
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

        // 現在のトークンが識別子だったら変数か関数呼び出しとしてパースする
        let ident = self.expect_ident();
        if let Some(ident) = ident {
            return new_expr!(self, self.parse_var_or_call(ident));
        }

        match self.get_token_and_next() {
            TokenKind::Lparen => {
                self.pop_token();
                let expr = self.parse_expr();
                expect!(self, TokenKind::Rparen);
                expr
            },
            TokenKind::Number(num) => new_expr!(self, ExprKind::Literal(Literal::Number(num))),
            TokenKind::String(s) => new_expr!(self, self.parse_string(s)),
            _ => new_expr!(self, self.invalid_expr("数値でも開きカッコでもないトークンです", 0)),
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        self.push_start_token();
        let mut expr = self.parse_term();

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
            match (self.variables.get(&ident), self.global_variables.get(&ident)) {
                (Some(variable), _) | (_, Some(variable)) => ExprKind::Address(variable.clone()),
                _ => self.invalid_expr(&format!("変数 \"{}\" が見つかりません", ident), -1),
            }
        } else {
            self.invalid_expr("変数ではありません", 0)
        };
        new_expr!(self, kind)
    }

    fn parse_unary(&mut self) -> Expr {
        match self.get_token_and_next() {
            TokenKind::Add => self.parse_postfix(),
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
        // 変数定義
        let variable = self.expect_define(false, false);
        if let Some(variable) = variable {
            // = があったら初期化式をパース
            let initializer = if self.consume(TokenKind::Assign) {
                Some(self.parse_initializer())
            } else {
                None
            };

            expect!(self, TokenKind::Semicolon);
            Some(StmtKind::Define(variable, initializer))
        } else {
            None
        }
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
            TokenKind::Lbrace => self.parse_block_stmt(),
            _ => {
                self.pos -= 1;
                self.parse_expr_stmt()
            }
        };
        new_stmt!(self, kind)
    }

    fn parse_func_decl(&mut self, ty: Type, ident: String) -> Option<DeclarationKind> {
        self.stack_size = 0;

        // 引数をパース
        self.variables = HashMap::<String, Variable>::new();
        loop {
            let _ = self.expect_define(true, true);

            if self.consume(TokenKind::Rparen) {
                break;
            } else if !self.consume(TokenKind::Comma) {
                self.add_error("',' か ')' ではないトークンです");
                break;
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
            Some(DeclarationKind::Func(ident, ty, args, self.stack_size, stmt))
        }
    }

    fn parse_global_var_decl(&mut self, ty: Type, ident: String) -> Option<DeclarationKind> {
        // 添字演算子があったら配列型にする
        let mut ty = ty;
        while let Some(size) = self.expect_subscript() {
            ty = Type::Array(Box::new(ty), size);
        }

        // = があったら初期化式をパース
        let initializer = if self.consume(TokenKind::Assign) {
            Some(self.parse_initializer())
        } else {
            None
        };

        expect!(self, TokenKind::Semicolon);

        let variable = Variable::new(ty, Location::Global(ident.clone()));
        self.global_variables.insert(ident.clone(), variable.clone());
        Some(DeclarationKind::GlobalVariable(variable, initializer))
    }

    pub fn parse_declaration(&mut self) -> Option<Declaration> {
        self.push_start_token();

        // 型
        let ty = self.expect_type();
        let kind = if let Some(ty) = ty {
            // 識別子
            let ident = self.expect_ident();
            if let Some(ident) = ident {
                if self.consume(TokenKind::Lparen) {
                    // 識別子の次のトークンが開きカッコだったら関数定義としてパースする
                    self.parse_func_decl(ty, ident)
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
