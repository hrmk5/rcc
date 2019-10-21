use std::collections::{HashMap};
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
    labels: HashMap<String, u32>,
    errors: Vec<CompileError>,
}

impl Analyzer {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            labels: HashMap::new(),
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
        expr.ty.clone().unwrap_or(Type::Void)
    }

    fn is_lvalue(&mut self, expr: &Expr) -> bool {
        match expr.kind {
            ExprKind::Variable(_) | ExprKind::Dereference(_) | ExprKind::MemberAccess(_, _) => true,
            _ => false,
        }
    }

    fn check_infix(&mut self, kind: &Infix, lhs: &Expr, rhs: &Expr, span: &Span) -> bool {
        let lty = lhs.ty.clone().unwrap_or(Type::Void);
        let rty = rhs.ty.clone().unwrap_or(Type::Void);

        if lty.is_floating_number() || rty.is_floating_number() {
            match kind {
                Infix::Add | Infix::Sub | Infix::Mul | Infix::Div | Infix::Equal | Infix::NotEqual | Infix::LessThan | Infix::LessThanOrEqual => true,
                _ => {
                    self.add_error("小数の計算ができる演算子ではありません", span);
                    false
                },
            }
        } else {
            true
        }
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        // sizeofは数値リテラルに置き換える
        if let ExprKind::SizeOf(inner_expr) = &mut expr.kind.clone() {
            self.get_type(inner_expr);
            mem::replace(expr, Expr {
                kind: ExprKind::Literal(Literal::Number(inner_expr.ty().get_size() as i64)),
                ty: Some(Type::Int),
                span: inner_expr.span.clone(),
            });
            return;
        }

        expr.ty = match &mut expr.kind {
            ExprKind::Literal(Literal::Number(_)) => Some(Type::Int),
            ExprKind::Literal(Literal::Float(_)) => Some(Type::Float),
            ExprKind::Literal(Literal::Double(_)) => Some(Type::Double),
            ExprKind::Literal(Literal::String(_)) => Some(Type::Pointer(Box::new(Type::Char))),
            ExprKind::Variable(var) => Some(var.ty.clone()),
            ExprKind::Dereference(expr) => match self.get_type(expr) {
                Type::Pointer(box ty) | Type::Array(box ty, _) => Some(ty),
                _ => {
                    self.add_error("ポインタではない式を参照外ししています", &expr.span);
                    None
                },
            },
            ExprKind::Address(var) => Some(Type::Pointer(Box::new(var.ty.clone()))),
            ExprKind::Assign(lhs, rhs) => {
                let lty = self.get_type(lhs);
                let rty = self.get_type(rhs);
                
                if let Type::Const(_) = &lty {
                    self.add_error("const変数に代入できません", &rhs.span);
                } else if !rty.can_assign_to(&lty) {
                    self.add_error("互換性のない型です", &rhs.span);
                }

                Some(lty)
            },
            ExprKind::Infix(infix, lhs, rhs) => {
                let lty = self.get_type(lhs);
                let rty = self.get_type(rhs);

                if !self.check_infix(&infix, &lhs, &rhs, &expr.span) {
                    None
                } else if let (Type::Double, _) | (_, Type::Double) = (&lty, &rty) {
                    Some(Type::Double)
                } else if let (Type::Float, _) | (_, Type::Float) = (&lty, &rty) {
                    Some(Type::Float)
                } else {
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
                }
            },
            ExprKind::Call(name, args) => {
                for arg in args.iter_mut() {
                    self.walk_expr(arg);
                }
                let args_ = args.clone();

                let span = &expr.span;
                let get_ty = || {
                    let func = match self.functions.get(name) {
                        Some(func) => func,
                        None => {
                            self.add_error("関数が見つかりません", span);
                            return None;
                        },
                    };

                    // Check argument length
                    if func.args.len() != args_.len() {
                        self.add_error("引数の数が違います", span);
                        return None;
                    } 

                    // Check arguments
                    let ok = func.args.iter()
                        .zip(args_.into_iter().map(|arg| arg.ty))
                        .all(|a| match a {
                            (parameter, Some(argument)) => argument.can_assign_to(parameter),
                            (_, None) => false,
                        });
                    if !ok {
                        self.add_error("不正な引数です", span);
                        return None;
                    }

                    Some(func.return_type.clone())
                };
                get_ty()
            },
            ExprKind::BitNot(expr) => Some(self.get_type(expr)),
            ExprKind::MemberAccess(expr, name) => {
                let ty = self.get_type(expr);
                match ty {
                    Type::Structure(_, members, _) => match members.iter().find(|(name_, _)| name == name_) {
                        Some((_, var)) => Some(var.ty.clone()),
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
            ExprKind::Increment(expr, _) => {
                let ty = self.get_type(expr);

                if !self.is_lvalue(&expr) {
                    self.add_error("左辺値ではありません", &expr.span);
                    None
                } else {
                    Some(ty)
                }
            },
            ExprKind::Decrement(expr, _) => {
                let ty = self.get_type(expr);

                if !self.is_lvalue(&expr) {
                    self.add_error("左辺値ではありません", &expr.span);
                    None
                } else {
                    Some(ty)
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

    fn walk_stmt(&mut self, stmt: &mut Stmt, allow_case: bool, allow_break: bool, allow_continue: bool) {
        match &mut stmt.kind {
            StmtKind::Expr(expr) => self.walk_expr(expr),
            StmtKind::If(cond, stmt, else_stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt, allow_case, allow_break, allow_continue);
                if let Some(else_stmt) = else_stmt {
                    self.walk_stmt(else_stmt, allow_case, allow_break, allow_continue);
                }
            },
            StmtKind::For(init_stmt, cond, loop_expr, stmt) => {
                if let Some(stmt) = init_stmt { self.walk_stmt(stmt, allow_case, allow_break, allow_continue) };
                if let Some(cond) = cond { self.walk_expr(cond) };
                if let Some(stmt) = loop_expr { self.walk_expr(stmt) };
                self.walk_stmt(stmt, allow_case, true, true);
            },
            StmtKind::While(cond, stmt) => {
                self.walk_expr(cond);
                self.walk_stmt(stmt, allow_case, true, true);
            },
            StmtKind::Return(expr) => {
                self.walk_expr(expr);
            },
            StmtKind::Define(var, initializer) => {
                match var.ty {
                    Type::Void => self.add_error("void型の変数は定義できません", &stmt.span),
                    Type::Array(box Type::Void, _) => self.add_error("void型の配列は定義できません", &stmt.span),
                    _ => {
                        if let Some(initializer) = initializer {
                            if let InitializerKind::List(_) = initializer.kind {
                                if let Type::Array(_, _) | Type::Structure(_, _, _) = var.ty {
                                    self.walk_initializer(initializer);
                                } else {
                                    self.add_error("配列と構造体以外の変数に初期化リストを使用しています", &initializer.span);
                                }
                            } else {
                                self.walk_initializer(initializer);
                            }
                        }
                    },
                };
            },
            StmtKind::Block(stmt_list) => {
                for stmt in stmt_list {
                    self.walk_stmt(stmt, allow_case, allow_break, allow_continue);
                }
            },
            StmtKind::Switch(expr, _, stmt, _) => {
                self.walk_expr(expr);

                if !expr.ty().is_integer() {
                    self.add_error("整数ではありません", &expr.span);
                }

                self.walk_stmt(stmt, true, true, allow_continue);
            },
            StmtKind::Case(expr) => {
                if !allow_case {
                    self.add_error("switch文の中以外でcaseは使用できません", &stmt.span)
                }

                self.walk_expr(expr);

                if !expr.ty().is_integer() {
                    self.add_error("整数ではありません", &expr.span);
                    return;
                }
            }
            StmtKind::Break => {
                if !allow_break {
                    self.add_error("switch文かループの中以外でbreakは使用できません", &stmt.span);
                }
            },
            StmtKind::Continue => {
                if !allow_continue {
                    self.add_error("ループの中以外でcontinueは使用できません", &stmt.span);
                }
            },
            StmtKind::Goto(name, ref mut label_num) => {
                if let Some(n) = self.labels.get(name) {
                    *label_num = *n;
                } else {
                    self.add_error("ラベルが存在しません", &stmt.span);
                }
            },
            StmtKind::Default => {},
            StmtKind::Label(_) => {},
        }
    }

    fn walk_labels(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Label(label) => { self.labels.insert(label.clone(), self.labels.len() as u32); },
            StmtKind::Block(stmts) => {
                for stmt in stmts {
                    self.walk_labels(&stmt);
                }
            },
            _ => {},
        };
    }

    fn walk_declaration(&mut self, declaration: &mut Declaration) {
        match &mut declaration.kind {
            DeclarationKind::Func(name, ty, args, _, stmt, _) => {
                let function = Function::new(ty.clone(), args.clone().into_iter().map(|var| var.ty).collect());
                self.functions.insert(name.clone(), function);

                // 本体がブロックではない場合はエラー
                if let StmtKind::Block(_) = stmt.kind {
                    self.walk_labels(stmt);
                    self.walk_stmt(stmt, false, false, false);
                } else {
                    self.add_error("関数の本体がブロックではありません", &stmt.span);
                }
            },
            DeclarationKind::GlobalVariable(_, Some(init_expr), _) => self.walk_initializer(init_expr),
            DeclarationKind::GlobalVariable(_, _, _) => {},
            DeclarationKind::Prototype(name, return_type, args) => {
                self.functions.insert(name.clone(), Function::new(return_type.clone(), args.clone()));
            },
            DeclarationKind::Extern(decl) => {
                match decl.kind {
                    DeclarationKind::Prototype(_, _, _) | DeclarationKind::GlobalVariable(_, _, _) => self.walk_declaration(decl),
                    _ => self.add_error("関数と変数にのみextern指定子を付けることができます", &decl.span),
                }
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
