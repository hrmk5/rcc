use std::collections::HashMap;
use std::mem;
use crate::ast::*;
use crate::error::{CompileError, Span};

struct Function {
    return_type: Type,
    args: Vec<Type>,
}

impl Function {
    fn new(return_type: Type, args: Vec<Type>) -> Self {
        Self {
            return_type,
            args,
        }
    }
}

struct Analyzer {
    functions: HashMap<String, Function>,
    errors: Vec<CompileError>,
}

impl Analyzer {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, msg: &str, span: &Span) {
        self.errors.push(CompileError {
            msg: msg.to_string(),
            span: span.clone(),
        });
    }

    fn get_type(&mut self, expr: &mut Expr) -> Type {
        self.walk_expr(expr);
        expr.ty()
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        // sizeofは数値リテラルに置き換える
        if let ExprKind::SizeOf(inner_expr) = &mut expr.kind.clone() {
            self.get_type(inner_expr);
            mem::replace(expr, Expr {
                kind: ExprKind::Literal(Literal::Number(inner_expr.ty().get_size() as i32)),
                ty: Some(Type::Int),
                span: inner_expr.span.clone(),
            });
            return;
        }

        expr.ty = match &mut expr.kind {
            ExprKind::Literal(Literal::Number(_)) => Some(Type::Int),
            ExprKind::Literal(Literal::String(_)) => Some(Type::Pointer(Box::new(Type::Char))),
            ExprKind::Variable(var) => Some(var.ty.clone()),
            ExprKind::Dereference(expr) => match self.get_type(expr) {
                Type::Pointer(box ty) | Type::Array(box ty, _) => Some(ty),
                _ => panic!("ポインタではない式を参照外ししています"),
            },
            ExprKind::Address(var) => Some(Type::Pointer(Box::new(var.ty.clone()))),
            ExprKind::Assign(lhs, rhs) => {
                self.walk_expr(rhs);
                Some(self.get_type(lhs))
            },
            ExprKind::Infix(infix, lhs, rhs) => {
                let lty = self.get_type(lhs);
                let rty = self.get_type(rhs);
                Some(match infix {
                    Infix::Add | Infix::Sub => {
                        match (lty, rty) {
                            (Type::Pointer(ty), _) | (_, Type::Pointer(ty)) => Type::Pointer(ty),
                            (Type::Array(ty, _), _) | (_, Type::Array(ty, _)) => Type::Pointer(ty),
                            _ => Type::Int,
                        }
                    },
                    _ => Type::Int,
                })
            },
            ExprKind::Call(name, args) => {
                for arg in args {
                    self.walk_expr(arg);
                }

                // TODO: 型チェック
                // TODO: 関数の存在チェック
                Some(self.functions.get(name).map_or(Type::Int, |func| func.return_type.clone()))
            },
            ExprKind::BitNot(expr) => Some(self.get_type(expr)),
            ExprKind::MemberAccess(expr, name) => {
                let ty = self.get_type(expr);
                match ty {
                    Type::Structure(members, _) => match members.get(name) {
                        Some(var) => Some(var.ty.clone()),
                        _ => {
                            self.add_error("メンバが見つかりません", &expr.span);
                            None
                        },
                    },
                    _ => {
                        self.add_error("構造体ではありません", &expr.span);
                        None
                    },
                }
            },
            ExprKind::Invalid => panic!("Unexpected invalid expression"),
            ExprKind::SizeOf(_) => panic!("Unexpected sizeof unary operator"),
        };
    }

    fn walk_initializer(&mut self, initializer: &mut Initializer) {
        match &mut initializer.kind {
            InitializerKind::List(initializers) => {
                for initializer in initializers {
                    self.walk_initializer(initializer);
                }
            },
            InitializerKind::Expr(expr) => self.walk_expr(expr),
        }
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Expr(expr) => self.walk_expr(expr),
            StmtKind::If(cond, stmt, else_stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt);
                if let Some(else_stmt) = else_stmt {
                    self.walk_stmt(else_stmt);
                }
            },
            StmtKind::For(init_stmt, cond, loop_expr, stmt) => {
                if let Some(stmt) = init_stmt { self.walk_stmt(stmt) };
                if let Some(cond) = cond { self.walk_expr(cond) };
                if let Some(stmt) = loop_expr { self.walk_expr(stmt) };
                self.walk_stmt(stmt);
            },
            StmtKind::While(cond, stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt);
            },
            StmtKind::Return(expr) => {
                self.walk_expr(expr);
            },
            StmtKind::Define(_, initializer) => {
                if let Some(initializer) = initializer {
                    self.walk_initializer(initializer);
                }
            },
            StmtKind::Block(stmt_list) => {
                for stmt in stmt_list {
                    self.walk_stmt(stmt);
                }
            },
        }
    }

    fn walk_declaration(&mut self, declaration: &mut Declaration) {
        match &mut declaration.kind {
            DeclarationKind::Func(name, ty, args, _, stmt) => {
                let function = Function::new(ty.clone(), args.clone().into_iter().map(|var| var.ty).collect());
                self.functions.insert(name.clone(), function);

                // 本体がブロックではない場合はエラー
                if let StmtKind::Block(_) = stmt.kind {
                    self.walk_stmt(stmt);
                } else {
                    self.add_error("関数の本体がブロックではありません", &stmt.span);
                }
            },
            DeclarationKind::GlobalVariable(_, Some(init_expr)) => self.walk_initializer(init_expr),
            DeclarationKind::GlobalVariable(_, _) => {},
            DeclarationKind::Prototype(name, return_type, args) => {
                self.functions.insert(name.clone(), Function::new(return_type.clone(), args.clone()));
            },
        };
    }

    fn walk(mut self, program: &mut Program) -> Result<(), Vec<CompileError>> {
        for declaration in &mut program.declarations {
            self.walk_declaration(declaration);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

pub fn walk(program: &mut Program) -> Result<(), Vec<CompileError>> {
    let analyzer = Analyzer::new();
    analyzer.walk(program)
}
