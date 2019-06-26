use crate::parser::{Program, Stmt, Expr, Literal, Infix, Declaration, Type, Location, Variable};

pub struct Generator {
    pub code: String,
    label_num: u32,
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
                    Some(Type::Pointer(box Type::Array(_, _))) => 8,
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

    fn gen_var_or_deref(&mut self, expr: Expr) {
        let ty = expr.get_type().unwrap();
        let size = self.gen_lvalue(expr.clone());

        if let Some(size) = size {
            let size_str = self.get_size_str(size).unwrap();
            let register = self.get_size_register(if size == 1 { 4 } else { size }, "rax").unwrap();
            let mov = match size {
                1 => "movsx",
                _ => "mov",
            };

            match ty {
                // 配列型だったらメモリアクセスせずにアドレスを返す
                Type::Array(_, _) => {},
                _ => {
                    add_mnemonic!(self, "pop rax");
                    add_mnemonic!(self, "{} {}, {} [rax]", mov, register, size_str);
                    add_mnemonic!(self, "push rax");
                },
            };
        }
    }

    fn gen_address(&mut self, variable: Variable) {
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
    }

    fn gen_assign(&mut self, lhs: Expr, rhs: Expr) {
        let size = self.gen_lvalue(lhs);
        if let Some(size) = size {
            self.gen_expr(rhs);

            add_mnemonic!(self, "pop rdx");
            add_mnemonic!(self, "pop rax");
            add_mnemonic!(self, "mov {} [rax], {}", self.get_size_str(size).unwrap(), self.get_size_register(size, "rdx").unwrap());
            add_mnemonic!(self, "push rdx");
        }
    }

    fn gen_infix(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        match kind.clone() {
            Infix::Add | Infix::Sub => match (lhs.get_type(), rhs.get_type()) {
                (Some(Type::Pointer(ty)), Some(Type::Int)) | (Some(Type::Array(ty, _)), Some(Type::Int)) => {
                    self.gen_expr(lhs);
                    self.gen_expr(rhs);
                    // rhsにポインタか配列の型のサイズを掛ける
                    add_mnemonic!(self, "pop rdi");
                    add_mnemonic!(self, "mov rax, {}", ty.get_size());
                    add_mnemonic!(self, "imul rdi");
                    add_mnemonic!(self, "push rax");
                },
                (Some(Type::Int), Some(Type::Pointer(ty))) | (Some(Type::Int), Some(Type::Array(ty, _))) => {
                    self.gen_expr(lhs);
                    // lhsにポインタか配列の型のサイズを掛ける
                    add_mnemonic!(self, "pop rdi");
                    add_mnemonic!(self, "mov rax, {}", ty.get_size());
                    add_mnemonic!(self, "imul rdi");
                    add_mnemonic!(self, "push rax");

                    self.gen_expr(rhs);
                },
                _ => {
                    self.gen_expr(lhs);
                    self.gen_expr(rhs);
                },
            },
            _ => {
                self.gen_expr(lhs);
                self.gen_expr(rhs);
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
    }

    fn gen_call(&mut self, name: String, _: Type, args: Vec<Expr>) {
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
    }

    fn gen_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(Literal::Number(num)) => {
                add_mnemonic!(self, "push {}", num);
            },
            Expr::Literal(Literal::String(num)) => {
                add_mnemonic!(self, "lea rax, .Ltext{}[rip]", num);
                add_mnemonic!(self, "push rax");
            },
            Expr::Variable(_) | Expr::Dereference(_) => self.gen_var_or_deref(expr),
            Expr::Address(variable) => self.gen_address(variable),
            Expr::Assign(lhs, rhs) => self.gen_assign(*lhs, *rhs),
            Expr::Infix(kind, lhs, rhs) => self.gen_infix(kind, *lhs, *rhs),
            Expr::Call(name, ty, args) => self.gen_call(name, ty, args),
            _ => {},
        };
    }

    fn gen_return_stmt(&mut self, expr: Expr) {
        self.gen_expr(expr);
        add_mnemonic!(self, "pop rax");
        add_mnemonic!(self, "mov rsp, rbp");
        add_mnemonic!(self, "pop rbp");
        add_mnemonic!(self, "ret");
    }

    fn gen_if_stmt(&mut self, cond: Expr, if_stmt: Stmt, else_stmt: Option<Box<Stmt>>) {
        self.gen_expr(cond);
        add_mnemonic!(self, "pop rax");
        add_mnemonic!(self, "cmp rax, 0");
        self.label_num += 1;
        let label_num = self.label_num;

        // else 節がある場合
        if let Some(else_stmt) = else_stmt {
            add_mnemonic!(self, "je .Lelse{}", label_num);
            self.gen_stmt(if_stmt);
            add_mnemonic!(self, "jmp .Lend{}", label_num);
            add_label!(self, ".Lelse", label_num);
            self.gen_stmt(*else_stmt);
        } else {
            add_mnemonic!(self, "je .Lend{}", label_num);
            self.gen_stmt(if_stmt);
        }

        add_label!(self, ".Lend", label_num);
    }

    fn gen_while_stmt(&mut self, expr: Expr, stmt: Stmt) {
        self.label_num += 1;
        let label_num = self.label_num;

        add_label!(self, ".Lbegin", label_num);

        // 条件式
        self.gen_expr(expr);
        add_mnemonic!(self, "pop rax");
        add_mnemonic!(self, "cmp rax, 0");
        add_mnemonic!(self, "je .Lend{}", label_num);

        // 文
        self.gen_stmt(stmt);
        add_mnemonic!(self, "jmp .Lbegin{}", label_num);

        add_label!(self, ".Lend", label_num);
    }

    fn gen_for_stmt(&mut self, init: Option<Expr>, cond: Option<Expr>, loop_expr: Option<Expr>, stmt: Stmt) {
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
        self.gen_stmt(stmt);

        if let Some(loop_expr) = loop_expr {
            self.gen_expr(loop_expr);
        }

        add_mnemonic!(self, "jmp .Lbegin{}", label_num);
        add_label!(self, ".Lend", label_num);
    }

    fn gen_define_stmt(&mut self, variable: Variable, init_expr: Option<Expr>) {
        let offset = match variable.location {
            Location::Local(offset) => offset,
            _ => panic!("ローカル変数ではありません"),
        };

        // 初期化式
        if let Some(init_expr) = init_expr {
            match init_expr {
                Expr::Initializer(_) => self.gen_initalizer(offset, &variable.ty, init_expr),
                expr => {
                    self.gen_expr(expr);
                    add_mnemonic!(self, "pop rax");

                    let size = match variable.ty {
                        Type::Array(_, _) => 8,
                        ty => ty.get_size(),
                    };

                    let register = self.get_size_register(size, "rax").unwrap();
                    let size_str = self.get_size_str(size).unwrap();
                    add_mnemonic!(self, "mov {} [rbp-{}], {}", size_str, offset, register);
                }
            };
        }
    }

    fn gen_block_stmt(&mut self, stmt_list: Vec<Stmt>) {
        for stmt in stmt_list {
            let must_pop = match stmt {
                Stmt::Return(_) | Stmt::Define(_, _) => false,
                _ => true,
            };

            self.gen_stmt(stmt);

            if must_pop {
                add_mnemonic!(self, "pop rax");
            }
        }
    }

    fn gen_stmt(&mut self, stmt: Stmt) {
        #[allow(unreachable_patterns)]
        match stmt {
            Stmt::Expr(expr) => self.gen_expr(expr),
            Stmt::Return(expr) => self.gen_return_stmt(expr),
            Stmt::If(cond, if_stmt, else_stmt) => self.gen_if_stmt(cond, *if_stmt, else_stmt),
            Stmt::While(expr, stmt) => self.gen_while_stmt(expr, *stmt),
            Stmt::For(init, cond, loop_expr, stmt) => self.gen_for_stmt(init, cond, loop_expr, *stmt),
            Stmt::Define(variable, init_expr) => self.gen_define_stmt(variable, init_expr),
            Stmt::Block(stmt_list) => self.gen_block_stmt(stmt_list),
            _ => {},
        }
    }

    fn gen_initalizer(&mut self, offset: usize, ty: &Type, expr: Expr) {
        let (element_type, _) = match ty {
            Type::Array(ty, size) => (ty.clone(), size),
            _ => panic!("配列ではありません"),
        };

        match expr {
            Expr::Initializer(expr_list) => {
                let mut i = 0;
                for expr in expr_list {
                    let element_offset = offset - i * element_type.get_size();

                    match expr {
                        Expr::Initializer(_) => {
                            self.gen_initalizer(element_offset, &element_type, expr);
                        },
                        _ => {
                            self.gen_expr(expr);
                            let register = self.get_size_register(element_type.get_size(), "rax").unwrap();
                            add_mnemonic!(self, "pop rax");
                            add_mnemonic!(self, "mov [rbp-{}], {}", element_offset, register);
                        },
                    };

                    i += 1;
                }
            },
            _ => panic!("gen_initializer"),
        };
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

    pub fn gen_global_var(&mut self, declarations: &Vec<Declaration>) {
        self.code.push_str(".data\n");
        // グローバル変数
        for declaration in declarations {
            match declaration {
                Declaration::GlobalVariable(variable, init_expr) => {
                    add_label!(self, global!(variable.clone()));

                    // 初期値
                    self.gen_global_init_expr(&variable.ty, init_expr.clone());
                },
                _ => {},
            };
        }
    }

    fn gen_global_init_expr(&mut self, ty: &Type, init_expr: Option<Expr>) {
        match (ty.clone(), init_expr.clone()) {
            (Type::Int, Some(Expr::Literal(Literal::Number(num)))) => add_mnemonic!(self, ".int {}", num),
            (Type::Int, None) => add_mnemonic!(self, ".int 0"),
            (Type::Char, Some(Expr::Literal(Literal::Number(num)))) => add_mnemonic!(self, ".byte {}", num),
            (Type::Char, None) => add_mnemonic!(self, ".byte 0"),
            (Type::Pointer(_), None) => add_mnemonic!(self, ".long 0"),
            (Type::Pointer(_), Some(Expr::Address(variable))) => add_mnemonic!(self, ".quad {}", global!(variable)),
            // TODO: ポインタ演算
            (Type::Array(box Type::Char, _), Some(Expr::Literal(Literal::String(num)))) |
                (Type::Pointer(box Type::Char), Some(Expr::Literal(Literal::String(num)))) => add_mnemonic!(self, ".quad .Ltext{}", num),
            (Type::Array(_, _), Some(Expr::Initializer(_))) => self.gen_global_initializer(&ty, init_expr.unwrap()),
            (Type::Array(ty, size), None) => add_mnemonic!(self, ".ascii \"{}\"", "\\0".repeat(ty.get_size()).repeat(size)),
            _ => {},
        };
    }

    fn gen_global_initializer(&mut self, ty: &Type, expr: Expr) {
        let (element_type, size) = match ty {
            Type::Array(ty, size) => (ty.clone(), size),
            _ => panic!("配列ではありません"),
        };

        match expr {
            Expr::Initializer(expr_list) => {
                let mut i = 0;
                for expr in expr_list {
                    if let Expr::Initializer(_) = expr {
                        self.gen_global_initializer(&element_type, expr);
                    } else {
                        self.gen_global_init_expr(&element_type, Some(expr));
                    }
                    i += 1;
                }

                let padding = (size - i) * element_type.get_size();
                if padding > 0 {
                    add_mnemonic!(self, ".zero {}", padding);
                }
            },
            _ => panic!("gen_global_initializer"),
        };
    }

    pub fn gen(&mut self, program: Program) {
        self.code.push_str(".intel_syntax noprefix\n");
        self.code.push_str(".global main\n");

        // 文字列リテラル
        // .rodataに配置
        self.code.push_str(".section .rodata\n");
        for (i, string) in program.string_list.into_iter().enumerate() {
            add_label!(self, ".Ltext", i);
            add_mnemonic!(self, ".string \"{}\"", string);
        }

        self.gen_global_var(&program.declarations);

        self.code.push_str(".text\n");
        for declaration in program.declarations {
            self.gen_declaration(declaration);
        }
    }
}
