use crate::parser::{Program, Stmt, Expr, Literal, Infix, Declaration, Type, Variable};

pub struct Generator {
    pub code: String,
    label_num: u32,
}

const ARG_REGISTERS: [&str; 6] = ["r9", "r8", "rcx", "rdx", "rsi", "rdi"];

impl Generator {
    pub fn new() -> Self {
        Generator {
            code: String::new(),
            label_num: 0,
        }
    }

    fn get_size_str(&self, size: usize) -> Option<&'static str> {
        match size {
            1 => Some("BYTE PTR"),
            2 => Some("WORD PTR"),
            4 => Some("DWORD PTR"),
            8 => Some("QWORD PTR"),
            _ => None,
        }
    }

    fn get_size_register(&self, size: usize, register: &'static str) -> Option<&'static str> {
        match size {
            4 => match register {
                "rax" => Some("eax"),
                "rbx" => Some("ebx"),
                "rcx" => Some("ecx"),
                "rdx" => Some("edx"),
                "rsi" => Some("esi"),
                "rdi" => Some("edi"),
                _ => None,
            },
            8 => Some(register),
            _ => None,
        }
    }

    fn gen_lvalue(&mut self, expr: Expr) -> Option<usize> {
        match expr {
            Expr::Dereference(variable) => {
                if let Type::Pointer(_) = variable.ty {
                    self.code.push_str(&format!("  mov rax, [rbp-{}]\n", variable.offset + 8));
                    self.code.push_str("  push rax\n");
                    Some(8)
                } else {
                    println!("ポインタではない変数です");
                    None
                }
            },
            Expr::Variable(variable) => {
                self.code.push_str("  mov rax, rbp\n");
                self.code.push_str(&format!("  sub rax, {}\n", variable.offset + 8));
                self.code.push_str("  push rax\n");
                Some(variable.ty.get_size())
            },
            _ => {
                println!("代入の左辺値が変数ではありません");
                None
            },
        }
    }

    fn gen_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(Literal::Number(num)) => {
                self.code.push_str(&format!("  push {}\n", num));
            },
            Expr::Variable(ref variable) => {
                let size_str = self.get_size_str(variable.ty.get_size()).unwrap();
                let register = self.get_size_register(variable.ty.get_size(), "rax").unwrap();
                self.gen_lvalue(expr);
                self.code.push_str("  pop rax\n");
                self.code.push_str(&format!("  mov {}, {} [rax]\n", register, size_str));
                self.code.push_str("  push rax\n");
            },
            Expr::Dereference(ref variable) => {
                if let Type::Pointer(ty) = &variable.ty {
                    let size_str = self.get_size_str(ty.get_size()).unwrap();
                    let register = self.get_size_register(ty.get_size(), "rax").unwrap();
                    self.gen_lvalue(expr);
                    self.code.push_str("  pop rax\n");
                    self.code.push_str(&format!("  mov {}, {} [rax]\n", register, size_str));
                    self.code.push_str("  push rax\n");
                } else {
                    println!("ポインタではない変数です");
                }
            },
            Expr::Address(variable) => {
                self.code.push_str("  mov rax, rbp\n");
                self.code.push_str(&format!("  sub rax, {}\n", variable.offset + 8));
                self.code.push_str("  push rax\n");
            },
            Expr::Assign(lhs, rhs) => {
                let size = self.gen_lvalue(*lhs);
                if let Some(size) = size {
                    self.gen_expr(*rhs);

                    self.code.push_str("  pop rdi\n");
                    self.code.push_str("  pop rax\n");
                    self.code.push_str(&format!("  mov {} [rax], {}\n", self.get_size_str(size).unwrap(), self.get_size_register(size, "rdi").unwrap()));
                    self.code.push_str("  push rdi\n");
                }
            },
            Expr::Infix(kind, lhs, rhs) => {
                self.gen_expr(*lhs.clone());
                self.gen_expr(*rhs);

                if let Expr::Variable(variable) = *lhs {
                    if let Type::Pointer(_) = variable.ty {
                        self.code.push_str("  pop rdi\n");
                        self.code.push_str("  mov rax, 8\n");
                        self.code.push_str("  imul rdi\n");
                        self.code.push_str("  push rax\n");
                    }
                }

                self.code.push_str("  pop rdi\n");
                self.code.push_str("  pop rax\n");

                self.code.push_str(match kind {
                    Infix::Add => "  add rax, rdi\n",
                    Infix::Sub => "  sub rax, rdi\n",
                    Infix::Mul => "  imul rdi\n",
                    Infix::Div => "  cqo\n  idiv rdi\n",
                    Infix::Equal => "  cmp rax, rdi\n  sete al\n  movzb  rax, al\n",
                    Infix::NotEqual => "  cmp rax, rdi\n  setne al\n  movzb  rax, al\n",
                    Infix::LessThan => "  cmp rax, rdi\n  setl al\n  movzb  rax, al\n",
                    Infix::LessThanOrEqual => "  cmp rax, rdi\n  setle al\n  movzb  rax, al\n",
                });

                self.code.push_str("  push rax\n");
            },
            Expr::Call(name, _, args) => {
                let arg_count = args.len();
                for arg_expr in args {
                    self.gen_expr(arg_expr);
                }

                for register in ARG_REGISTERS[6 - arg_count..].iter() {
                    self.code.push_str(&format!("  pop {}\n", register));
                }
                // TODO: RSP を調整する
                // 調整してないけど動く
                self.code.push_str(&format!("  call {}\n", name));
                self.code.push_str("  push rax\n");
            },
            _ => {},
        };
    }

    pub fn gen_stmt(&mut self, stmt: Stmt) {
        #[allow(unreachable_patterns)]
        match stmt {
            Stmt::Expr(expr) => self.gen_expr(expr),
            Stmt::Return(expr) => {
                self.gen_expr(expr);
                self.code.push_str("  pop rax\n");
                self.code.push_str("  mov rsp, rbp\n");
                self.code.push_str("  pop rbp\n");
                self.code.push_str("  ret\n");
            },
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.gen_expr(cond);
                self.code.push_str("  pop rax\n");
                self.code.push_str("  cmp rax, 0\n");
                self.label_num += 1;
                let label_num = self.label_num;

                // else 節がある場合
                if let Some(else_stmt) = else_stmt {
                    self.code.push_str(&format!("  je .Lelse{}\n", label_num));
                    self.gen_stmt(*if_stmt);
                    self.code.push_str(&format!("  jmp .Lend{}\n", label_num));
                    self.code.push_str(&format!(".Lelse{}:\n", label_num));
                    self.gen_stmt(*else_stmt);
                } else {
                    self.code.push_str(&format!("  je .Lend{}\n", label_num));
                    self.gen_stmt(*if_stmt);
                }

                self.code.push_str(&format!(".Lend{}:\n", label_num));
            },
            Stmt::While(expr, stmt) => {
                self.label_num += 1;
                let label_num = self.label_num;

                self.code.push_str(&format!(".Lbegin{}:\n", label_num));

                // 条件式
                self.gen_expr(expr);
                self.code.push_str("  pop rax\n");
                self.code.push_str("  cmp rax, 0\n");
                self.code.push_str(&format!("  je .Lend{}\n", label_num));

                // 文
                self.gen_stmt(*stmt);
                self.code.push_str(&format!("  jmp .Lbegin{}\n", label_num));

                self.code.push_str(&format!(".Lend{}:\n", label_num));
            },
            Stmt::For(init, cond, loop_expr, stmt) => {
                self.label_num += 1;
                let label_num = self.label_num;

                // 初期化式
                if let Some(init) = init {
                    self.gen_expr(init);
                }

                self.code.push_str(&format!(".Lbegin{}:\n", label_num));

                // 条件式
                if let Some(cond) = cond {
                    self.gen_expr(cond);
                    self.code.push_str("  pop rax\n");
                    self.code.push_str("  cmp rax, 0\n");
                    self.code.push_str(&format!("  je .Lend{}\n", label_num));
                }

                // 文
                self.gen_stmt(*stmt);

                if let Some(loop_expr) = loop_expr {
                    self.gen_expr(loop_expr);
                }

                self.code.push_str(&format!("  jmp .Lbegin{}\n", label_num));
                self.code.push_str(&format!(".Lend{}:\n", label_num));
            },
            Stmt::Block(stmt_list) => {
                for stmt in stmt_list {
                    let pop = match stmt {
                        Stmt::Return(_) | Stmt::Define(_, _) => "",
                        _ => "  pop rax\n",
                    };
                    self.gen_stmt(stmt);
                    self.code.push_str(pop);
                }
            },
            _ => {},
        }
    }

    pub fn gen_declaration(&mut self, declaration: Declaration) {
        match declaration {
            Declaration::Func(name, args, stack_size, block) => {
                self.code.push_str(&format!("{}:\n", name));

                self.code.push_str("  push rbp\n");
                self.code.push_str("  mov rbp, rsp\n");
                self.code.push_str(&format!("  sub rsp, {}\n", stack_size));

                // スタックに引数の値をプッシュする
                for (i, arg) in args.into_iter().enumerate() {
                    let register = self.get_size_register(arg.ty.get_size(), ARG_REGISTERS[5 - i]).unwrap();
                    self.code.push_str(&format!("  mov [rbp-{}], {}\n", arg.offset + 8, register));
                }

                self.gen_stmt(block);
            },
        }
    }

    pub fn gen(&mut self, program: Program) {
        self.code.push_str(".intel_syntax noprefix\n");
        self.code.push_str(".global main\n");
        for declaration in program.0 {
            self.gen_declaration(declaration);
        }
    }
}
