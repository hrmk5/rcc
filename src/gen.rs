use crate::parser::{Program, Stmt, Expr, Literal, Infix, Declaration, Type, Variable, Location};

pub struct Generator {
    pub code: String,
    label_num: u32,
    string_count: u32,
}

const ARG_REGISTERS: [&str; 6] = ["r9", "r8", "rcx", "rdx", "rsi", "rdi"];

macro_rules! add_mnemonic {
    ($self: ident, $s: tt) => {
        $self.code.push_str(concat!("  ", $s, "\n"));
    };
    ($self: ident, $fmt: expr, $($arg: tt)*) => {
        $self.code.push_str(&format!(concat!("  ", $fmt, "\n"), $($arg)*));
    };
}

macro_rules! add_label {
    ($self: ident, $name: expr) => {
        $self.code.push_str(&format!("{}:\n", $name));
    };
    ($self: ident, $prefix: expr, $num: expr) => {
        $self.code.push_str(&format!(concat!($prefix, "{}:\n"), $num));
    };
}

macro_rules! global {
    ($variable: expr) => {
        match $variable.location {
            Location::Global(name) => name,
            _ => panic!("グローバル変数ではありません"),
        }
    };
}

impl Generator {
    pub fn new() -> Self {
        Generator {
            code: String::new(),
            label_num: 0,
            string_count: 0,
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
            1 => match register {
                "rax" => Some("al"),
                "rbx" => Some("bl"),
                "rcx" => Some("cl"),
                "rdx" => Some("dl"),
                "rsi" => Some("sil"),
                "rdi" => Some("dil"),
                "r8" => Some("r8b"),
                "r9" => Some("r9b"),
                _ => None,
            },
            4 => match register {
                "rax" => Some("eax"),
                "rbx" => Some("ebx"),
                "rcx" => Some("ecx"),
                "rdx" => Some("edx"),
                "rsi" => Some("esi"),
                "rdi" => Some("edi"),
                "r8" => Some("r8d"),
                "r9" => Some("r9d"),
                _ => None,
            },
            8 => Some(register),
            _ => None,
        }
    }

    fn gen_dereference(&mut self, expr: Expr) {
        match expr {
            Expr::Dereference(expr) => {
                self.gen_dereference(*expr);
                add_mnemonic!(self, "mov rax, [rax]");
            },
            Expr::Variable(variable) => {
                match variable.location {
                    Location::Local(offset) => add_mnemonic!(self, "mov rax, [rbp-{}]", offset),
                    Location::Global(name) => {
                        add_mnemonic!(self, "mov rax, {}[rip]", &name);
                    },
                };
            },
            _ => {
                self.gen_expr(expr);
                add_mnemonic!(self, "pop rax");
            },
        }
    }

    fn gen_lvalue(&mut self, expr: Expr) -> Option<usize> {
        match expr {
            Expr::Dereference(expr) => {
                let ty = expr.get_type();
                self.gen_dereference(*expr);
                add_mnemonic!(self, "push rax");
                Some(match ty {
                    Some(Type::Pointer(ty)) => ty.get_size(),
                    _ => panic!("ポインタではない値を参照外ししています"),
                })
            },
            Expr::Variable(variable) => {
                // 変数のアドレスをプッシュする
                match variable.location {
                    Location::Local(offset) => {
                        add_mnemonic!(self, "mov rax, rbp");
                        add_mnemonic!(self, "sub rax, {}", offset);
                        add_mnemonic!(self, "push rax");
                    },
                    Location::Global(name) => {
                        add_mnemonic!(self, "lea rax, {}[rip]", &name);
                        add_mnemonic!(self, "push rax");
                    },
                };

                // 変数のサイズを返す
                // 配列の場合はポインタのサイズを返す
                Some(match variable.ty {
                    Type::Array(_, _) => 8,
                    _ => variable.ty.get_size(),
                })
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
                add_mnemonic!(self, "push {}", num);
            },
            Expr::Literal(Literal::String) => {
                add_mnemonic!(self, "lea rax, .Ltext{}[rip]", self.string_count);
                add_mnemonic!(self, "push rax");
                self.string_count += 1;
            },
            Expr::Variable(_) | Expr::Dereference(_) => {
                let size = self.gen_lvalue(expr.clone());

                if let Some(size) = size {
                    let size_str = self.get_size_str(size).unwrap();
                    let register = self.get_size_register(if size == 1 { 4 } else { size }, "rax").unwrap();
                    let mov = match size {
                        1 => "movsx",
                        _ => "mov",
                    };

                    match expr {
                        // 配列型だったらメモリアクセスせずにアドレスを返す
                        Expr::Variable(Variable { ty: Type::Array(_, _), .. }) => {},
                        _ => {
                            add_mnemonic!(self, "pop rax");
                            add_mnemonic!(self, "{} {}, {} [rax]", mov, register, size_str);
                            add_mnemonic!(self, "push rax");
                        },
                    };
                }
            },
            Expr::Address(variable) => {
                match variable.location {
                    Location::Local(offset) => {
                        add_mnemonic!(self, "mov rax, rbp");
                        add_mnemonic!(self, "sub rax, {}", offset);
                        add_mnemonic!(self, "push rax");
                    },
                    Location::Global(name) => {
                        add_mnemonic!(self, "lea rax, {}[rip]", &name);
                        add_mnemonic!(self, "push rax");
                    },
                };
            },
            Expr::Assign(lhs, rhs) => {
                let size = self.gen_lvalue(*lhs);
                if let Some(size) = size {
                    self.gen_expr(*rhs);

                    add_mnemonic!(self, "pop rdx");
                    add_mnemonic!(self, "pop rax");
                    add_mnemonic!(self, "mov {} [rax], {}", self.get_size_str(size).unwrap(), self.get_size_register(size, "rdx").unwrap());
                    add_mnemonic!(self, "push rdx");
                }
            },
            Expr::Infix(kind, lhs, rhs) => {
                match kind.clone() {
                    Infix::Add | Infix::Sub => match (lhs.get_type(), rhs.get_type()) {
                        (Some(Type::Pointer(ty)), Some(Type::Int)) | (Some(Type::Array(ty, _)), Some(Type::Int)) => {
                            self.gen_expr(*lhs);
                            self.gen_expr(*rhs);
                            // rhsにポインタか配列の型のサイズを掛ける
                            add_mnemonic!(self, "pop rdi");
                            add_mnemonic!(self, "mov rax, {}", ty.get_size());
                            add_mnemonic!(self, "imul rdi");
                            add_mnemonic!(self, "push rax");
                        },
                        (Some(Type::Int), Some(Type::Pointer(ty))) | (Some(Type::Int), Some(Type::Array(ty, _))) => {
                            self.gen_expr(*lhs);
                            // lhsにポインタか配列の型のサイズを掛ける
                            add_mnemonic!(self, "pop rdi");
                            add_mnemonic!(self, "mov rax, {}", ty.get_size());
                            add_mnemonic!(self, "imul rdi");
                            add_mnemonic!(self, "push rax");

                            self.gen_expr(*rhs);
                        },
                        _ => {
                            self.gen_expr(*lhs);
                            self.gen_expr(*rhs);
                        },
                    },
                    _ => {
                        self.gen_expr(*lhs);
                        self.gen_expr(*rhs);
                    },
                };

                add_mnemonic!(self, "pop rdi");
                add_mnemonic!(self, "pop rax");

                match kind {
                    Infix::Add => add_mnemonic!(self, "add rax, rdi"),
                    Infix::Sub => add_mnemonic!(self, "sub rax, rdi"),
                    Infix::Mul => add_mnemonic!(self, "imul rdi"),
                    Infix::Div => {
                        add_mnemonic!(self, "cqo");
                        add_mnemonic!(self, "idiv rdi");
                    },
                    Infix::Equal => {
                        add_mnemonic!(self, "cmp rax, rdi");
                        add_mnemonic!(self, "sete al");
                        add_mnemonic!(self, "movzb  rax, al");
                    },
                    Infix::NotEqual => {
                        add_mnemonic!(self, "cmp rax, rdi");
                        add_mnemonic!(self, "setne al");
                        add_mnemonic!(self, "movzb  rax, al");
                    }
                    Infix::LessThan => {
                        add_mnemonic!(self, "cmp rax, rdi");
                        add_mnemonic!(self, "setl al");
                        add_mnemonic!(self, "movzb  rax, al");
                    },
                    Infix::LessThanOrEqual => {
                        add_mnemonic!(self, "cmp rax, rdi");
                        add_mnemonic!(self, "setle al");
                        add_mnemonic!(self, "movzb  rax, al");
                    },
                };

                add_mnemonic!(self, "push rax");
            },
            Expr::Call(name, _, args) => {
                let arg_count = args.len();
                for arg_expr in args {
                    self.gen_expr(arg_expr);
                }

                for register in ARG_REGISTERS[6 - arg_count..].iter() {
                    add_mnemonic!(self, "pop {}", register);
                }
                // 浮動小数点の引数の数
                add_mnemonic!(self, "mov al, 0");
                // TODO: RSP を調整する
                // 調整してないけど動く
                add_mnemonic!(self, "call {}", name);
                add_mnemonic!(self, "push rax");
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
                add_mnemonic!(self, "pop rax");
                add_mnemonic!(self, "mov rsp, rbp");
                add_mnemonic!(self, "pop rbp");
                add_mnemonic!(self, "ret");
            },
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.gen_expr(cond);
                add_mnemonic!(self, "pop rax");
                add_mnemonic!(self, "cmp rax, 0");
                self.label_num += 1;
                let label_num = self.label_num;

                // else 節がある場合
                if let Some(else_stmt) = else_stmt {
                    add_mnemonic!(self, "je .Lelse{}", label_num);
                    self.gen_stmt(*if_stmt);
                    add_mnemonic!(self, "jmp .Lend{}", label_num);
                    add_label!(self, ".Lelse", label_num);
                    self.gen_stmt(*else_stmt);
                } else {
                    add_mnemonic!(self, "je .Lend{}", label_num);
                    self.gen_stmt(*if_stmt);
                }

                add_label!(self, ".Lend", label_num);
            },
            Stmt::While(expr, stmt) => {
                self.label_num += 1;
                let label_num = self.label_num;

                add_label!(self, ".Lbegin", label_num);

                // 条件式
                self.gen_expr(expr);
                add_mnemonic!(self, "pop rax");
                add_mnemonic!(self, "cmp rax, 0");
                add_mnemonic!(self, "je .Lend{}", label_num);

                // 文
                self.gen_stmt(*stmt);
                add_mnemonic!(self, "jmp .Lbegin{}", label_num);

                add_label!(self, ".Lend", label_num);
            },
            Stmt::For(init, cond, loop_expr, stmt) => {
                self.label_num += 1;
                let label_num = self.label_num;

                // 初期化式
                if let Some(init) = init {
                    self.gen_expr(init);
                    add_mnemonic!(self, "pop rax");
                }

                add_label!(self, ".Lbegin", label_num);

                // 条件式
                if let Some(cond) = cond {
                    self.gen_expr(cond);
                    add_mnemonic!(self, "pop rax");
                    add_mnemonic!(self, "cmp rax, 0");
                    add_mnemonic!(self, "je .Lend{}", label_num);
                }

                // 文
                self.gen_stmt(*stmt);
                //add_mnemonic!(self, "pop rax");

                if let Some(loop_expr) = loop_expr {
                    self.gen_expr(loop_expr);
                    //add_mnemonic!(self, "pop rax");
                }

                add_mnemonic!(self, "jmp .Lbegin{}", label_num);
                add_label!(self, ".Lend", label_num);
            },
            Stmt::Block(stmt_list) => {
                for stmt in stmt_list {
                    let must_pop = match stmt {
                        Stmt::Return(_) | Stmt::Define(_) => false,
                        _ => true,
                    };

                    self.gen_stmt(stmt);

                    if must_pop {
                        add_mnemonic!(self, "pop rax");
                    }
                }
            },
            _ => {},
        }
    }

    pub fn gen_declaration(&mut self, declaration: Declaration) {
        match declaration {
            Declaration::Func(name, args, stack_size, block) => {
                add_label!(self, &name);

                add_mnemonic!(self, "push rbp");
                add_mnemonic!(self, "mov rbp, rsp");
                add_mnemonic!(self, "sub rsp, {}", stack_size);

                // スタックに引数の値を格納する
                for (i, arg) in args.into_iter().enumerate() {
                    let size = arg.ty.get_size();
                    let register = self.get_size_register(size, ARG_REGISTERS[5 - i]).unwrap();
                    match arg.location {
                        Location::Local(offset) => add_mnemonic!(self, "mov [rbp-{}], {}", offset, register),
                        _ => panic!("引数がグローバル変数です"),
                    };
                }

                self.gen_stmt(block);
            },
            _ => {},
        }
    }

    pub fn gen(&mut self, program: Program) {
        self.code.push_str(".intel_syntax noprefix\n");
        self.code.push_str(".global main\n");

        self.code.push_str(".data\n");
        // グローバル変数
        for variable in program.global_variables {
            // アラインメント
            // 配列は要素の型のサイズをアラインメントにする
            let align = match variable.ty {
                Type::Array(ref ty, _) => ty.get_size(),
                _ => variable.ty.get_size(),
            };
            add_mnemonic!(self, ".align {}", align);
            add_label!(self, global!(variable));
            // 初期値
            add_mnemonic!(self, "{}", match variable.ty {
                Type::Int => ".int 0".to_string(),
                Type::Char => ".byte 0".to_string(),
                Type::Pointer(_) => ".long 0".to_string(),
                Type::Array(ty, size) => format!(".ascii \"{}\"", "\\0".repeat(ty.get_size()).repeat(size)),
            });
        }

        // 文字列リテラル
        // .rodataに配置
        self.code.push_str(".section .rodata\n");
        for (i, string) in program.string_list.into_iter().enumerate() {
            add_label!(self, ".Ltext", i);
            add_mnemonic!(self, ".string \"{}\"", string);
        }

        self.code.push_str(".text\n");
        for declaration in program.declarations {
            self.gen_declaration(declaration);
        }
    }
}
