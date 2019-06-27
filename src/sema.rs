use std::collections::HashMap;
use std::mem;
use crate::ast::*;

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
}

impl Analyzer {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    fn get_type(&mut self, expr: &mut Expr) -> Type {
        self.walk_expr(expr);
        expr.ty()
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        // sizeofは数値リテラルに置き換える
        if let ExprKind::SizeOf(inner_expr) = &mut expr.kind.clone() {
            self.get_type(inner_expr);
            mem::replace(expr, Expr::with_type(
                ExprKind::Literal(Literal::Number(inner_expr.ty().get_size() as i32)),
                Type::Int));
            return;
        }

        expr.ty = Some(match &mut expr.kind {
            ExprKind::Literal(Literal::Number(_)) => Type::Int,
            ExprKind::Literal(Literal::String(_)) => Type::Pointer(Box::new(Type::Char)),
            ExprKind::Variable(var) => var.ty.clone(),
            ExprKind::Dereference(expr) => match self.get_type(expr) {
                Type::Pointer(box ty) | Type::Array(box ty, _) => ty,
                _ => panic!("ポインタではない式を参照外ししています"),
            },
            ExprKind::Address(var) => Type::Pointer(Box::new(var.ty.clone())),
            ExprKind::Assign(lhs, rhs) => {
                self.walk_expr(rhs);
                self.get_type(lhs)
            },
            ExprKind::Infix(infix, lhs, rhs) => {
                let lty = self.get_type(lhs);
                let rty = self.get_type(rhs);
                match infix {
                    Infix::Add | Infix::Sub => {
                        match (lty, rty) {
                            (Type::Pointer(ty), _) | (_, Type::Pointer(ty)) => Type::Pointer(ty),
                            (Type::Array(ty, _), _) | (_, Type::Array(ty, _)) => Type::Pointer(ty),
                            _ => Type::Int,
                        }
                    },
                    _ => Type::Int,
                }
            },
            ExprKind::Call(name, args) => {
                for arg in args {
                    self.walk_expr(arg);
                }

                // TODO: 型チェック
                // TODO: 関数の存在チェック
                self.functions.get(name).map_or(Type::Int, |func| func.return_type.clone())
            },
            ExprKind::BitNot(expr) => self.get_type(expr),
            ExprKind::Invalid => panic!("Unexpected invalid expression"),
            ExprKind::SizeOf(_) => panic!("Unexpected sizeof unary operator"),
        });
    }

    fn walk_initializer(&mut self, initializer: &mut Initializer) {
        match initializer {
            Initializer::List(initializers) => {
                for initializer in initializers {
                    self.walk_initializer(initializer);
                }
            },
            Initializer::Expr(expr) => self.walk_expr(expr),
        }
    }

    fn walk_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Expr(expr) => self.walk_expr(expr),
            Stmt::If(cond, stmt, else_stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt);
                if let Some(else_stmt) = else_stmt {
                    self.walk_stmt(else_stmt);
                }
            },
            Stmt::For(init_stmt, cond, loop_expr, stmt) => {
                if let Some(stmt) = init_stmt { self.walk_stmt(stmt) };
                if let Some(cond) = cond { self.walk_expr(cond) };
                if let Some(stmt) = loop_expr { self.walk_expr(stmt) };
                self.walk_stmt(stmt);
            },
            Stmt::While(cond, stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt);
            },
            Stmt::Return(expr) => {
                self.walk_expr(expr);
            },
            Stmt::Define(_, initializer) => {
                if let Some(initializer) = initializer {
                    self.walk_initializer(initializer);
                }
            },
            Stmt::Block(stmt_list) => {
                for stmt in stmt_list {
                    self.walk_stmt(stmt);
                }
            },
        }
    }

    fn walk_declaration(&mut self, declaration: &mut Declaration) {
        match declaration {
            Declaration::Func(name, ty, args, _, stmt) => {
                let function = Function::new(ty.clone(), args.clone().into_iter().map(|var| var.ty).collect());
                self.functions.insert(name.clone(), function);
                self.walk_stmt(stmt);
            },
            Declaration::GlobalVariable(_, Some(init_expr)) => self.walk_initializer(init_expr),
            _ => {},
        };
    }

    fn walk(&mut self, program: &mut Program) {
        for declaration in &mut program.declarations {
            self.walk_declaration(declaration);
        }
    }
}

pub fn walk(program: &mut Program) {
    let mut analyzer = Analyzer::new();
    analyzer.walk(program);
}
