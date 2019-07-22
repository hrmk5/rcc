use std::collections::HashMap;
use crate::ast::*;

pub struct Generator {
    pub code: String,
    label_num: u32,
    has_return: bool,
    case_labels_iter: Box<dyn Iterator<Item = u32>>,
    break_label_stack: Vec<u32>,
    continue_label_stack: Vec<u32>,
    default_label: Vec<u32>,
    stack_size: usize,
    functions: HashMap<String, Vec<Type>>,
}

const ARG_REGISTERS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const XMM_ARG_REGISTERS: [&str; 6] = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5"];

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
            has_return: false,
            case_labels_iter: Box::new(Vec::new().into_iter()),
            break_label_stack: Vec::new(),
            continue_label_stack: Vec::new(),
            default_label: Vec::new(),
            stack_size: 0,
            functions: HashMap::new(),
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
            2 => match register {
                "rax" => Some("ax"),
                "rbx" => Some("bx"),
                "rcx" => Some("cx"),
                "rdx" => Some("dx"),
                "rsi" => Some("si"),
                "rdi" => Some("di"),
                "r8" => Some("r8w"),
                "r9" => Some("r9w"),
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

    fn push(&mut self, reg: &'static str) {
        add_mnemonic!(self, "push {}", reg);
        self.stack_size += 8;
    }

    fn pop(&mut self, reg: &'static str) {
        add_mnemonic!(self, "pop {}", reg);
        self.stack_size -= 8;
    }

    fn push_xmm(&mut self, reg: &'static str) {
        add_mnemonic!(self, "sub rsp, 4");
        add_mnemonic!(self, "movss [rsp], {}", reg);
        self.stack_size += 4;
    }

    fn pop_xmm(&mut self, reg: &'static str) {
        add_mnemonic!(self, "movss {}, [rsp]", reg);
        add_mnemonic!(self, "add rsp, 4");
        self.stack_size -= 4;
    }

    fn gen_dereference(&mut self, expr: Expr) {
        match expr.kind {
            ExprKind::Dereference(expr) => {
                self.gen_dereference(*expr);
                add_mnemonic!(self, "mov rax, [rax]");
            },
            ExprKind::Variable(variable) => {
                match variable.location {
                    Location::Local(offset) => add_mnemonic!(self, "mov rax, [rbp-{}]", offset),
                    Location::Global(name) => {
                        add_mnemonic!(self, "mov rax, {}[rip]", &name);
                    },
                };
            },
            _ => {
                self.gen_expr(expr);
                self.pop("rax");
            },
        }
    }

    fn gen_lvalue(&mut self, expr: Expr) -> Option<usize> {
        match expr.kind {
            ExprKind::Dereference(expr) => {
                let ty = expr.ty();
                self.gen_dereference(*expr);
                self.push("rax");
                Some(match ty {
                    Type::Pointer(box Type::Array(_, _)) => 8,
                    Type::Pointer(ty) => ty.get_size(),
                    _ => panic!(),
                })
            },
            ExprKind::Variable(variable) => {
                // 変数のアドレスをプッシュする
                match variable.location {
                    Location::Local(offset) => {
                        add_mnemonic!(self, "mov rax, rbp");
                        add_mnemonic!(self, "sub rax, {}", offset);
                        self.push("rax");
                    },
                    Location::Global(name) => {
                        add_mnemonic!(self, "lea rax, {}[rip]", &name);
                        self.push("rax");
                    },
                };

                // 変数のサイズを返す
                // 配列の場合はポインタのサイズを返す
                Some(match variable.ty {
                    Type::Array(_, _) => 8,
                    _ => variable.ty.get_size(),
                })
            },
            ExprKind::MemberAccess(lhs, member) => {
                // メンバを取得
                let ty = lhs.ty();
                let member = ty.find_member(&member);

                self.gen_lvalue(*lhs);
                self.pop("rax");
                add_mnemonic!(self, "lea rax, [rax+{}]", member.offset());
                self.push("rax");

                // 変数のサイズを返す
                // 配列の場合はポインタのサイズを返す
                Some(match &member.ty {
                    Type::Array(_, _) => 8,
                    ty => ty.get_size(),
                })
            },
            _ => {
                println!("代入の左辺値が変数ではありません");
                None
            },
        }
    }

    fn pop_and_convert(&mut self, dst: &'static str, dst_ty: &Type, src_ty: &Type) {
        self.gen_load_and_convert(dst, "rsp", dst_ty, src_ty);
        if let Type::Float = src_ty {
            add_mnemonic!(self, "add rsp, 4");
            self.stack_size -= 4;
        } else {
            add_mnemonic!(self, "add rsp, 8");
            self.stack_size -= 8;
        }
    }

    fn gen_load_and_convert(&mut self, dst: &'static str, src: &'static str, dst_ty: &Type, src_ty: &Type) {
        let size_str = match src_ty {
            Type::Array(_, _) => self.get_size_str(8),
            ty => self.get_size_str(ty.get_size()),
        }.unwrap();

        match (src_ty, dst_ty) {
            // float <- float
            (Type::Float, Type::Float) => {
                add_mnemonic!(self, "movss {}, {} [{}]", dst, size_str, src);
            },
            // integer <- float
            (Type::Float, dst_ty) if dst_ty.is_integer() => {
                add_mnemonic!(self, "cvttss2si {}, {} [{}]", dst, size_str, src);
            },
            // float <- integer
            (src_ty, Type::Float) if src_ty.is_integer() => {
                if src_ty.get_size() < 4 {
                    add_mnemonic!(self, "movsx eax, {} [{}]", size_str, src);
                    add_mnemonic!(self, "cvtsi2ss {}, eax", dst);
                } else {
                    add_mnemonic!(self, "cvtsi2ss {}, {} [{}]", dst, size_str, src);
                }
            },
            // integer <- integer
            _ => self.gen_load(dst, src, dst_ty),
        };
    }

    
    fn gen_load(&mut self, dst: &'static str, src: &'static str, ty: &Type) {
        if let Type::Float = ty {
            add_mnemonic!(self, "movss {}, [{}]", dst, src);
        } else {
            let size = match ty {
                // Return address size
                Type::Array(_, _) => 8,
                ty => ty.get_size(),
            };

            let size_str = self.get_size_str(size).unwrap();
            let register = self.get_size_register(if size < 4 { 4 } else { size }, dst).unwrap();
            let mov = match size {
                1 | 2 => "movsx",
                _ => "mov",
            };

            match ty {
                // Return an address instead of accessing memory if dst type is array
                Type::Array(_, _) => {},
                _ => {
                    add_mnemonic!(self, "{} {}, {} [{}]", mov, register, size_str, src);
                },
            };
        }
    }

    fn gen_save(&mut self, dst: &str, src: &'static str, dst_ty: &Type) {
        if let Type::Float = dst_ty {
            add_mnemonic!(self, "movss [{}], {}", dst, src);
        } else {
            let size = match dst_ty {
                Type::Array(_, _) => 8,
                ty => ty.get_size(),
            };

            let src = self.get_size_register(size, src).unwrap();
            add_mnemonic!(self, "mov [{}], {}", dst, src);
        }
    }

    fn gen_var_or_deref(&mut self, expr: Expr) {
        let ty = expr.ty();

        self.gen_lvalue(expr);
        self.pop("rax");

        if let Type::Float = &ty {
            self.gen_load("xmm0", "rax", &ty);
            self.push_xmm("xmm0");
        } else {
            self.gen_load("rax", "rax", &ty);
            self.push("rax");
        }
    }

    fn gen_address(&mut self, variable: Variable) {
        match variable.location {
            Location::Local(offset) => {
                add_mnemonic!(self, "mov rax, rbp");
                add_mnemonic!(self, "sub rax, {}", offset);
                self.push("rax");
            },
            Location::Global(name) => {
                add_mnemonic!(self, "lea rax, {}[rip]", &name);
                self.push("rax");
            },
        };
    }

    fn gen_assign(&mut self, lhs: Expr, rhs: Expr) {
        let lty = lhs.ty();
        let rty = rhs.ty();

        self.gen_lvalue(lhs);
        self.gen_expr(rhs);
        
        let register = if let Type::Float = lty {
            self.pop_and_convert("xmm0", &lty, &rty);
            "xmm0"
        } else {
            self.pop_and_convert("rdx", &lty, &rty);
            "rdx"
        };
        
        self.pop("rax");
        self.gen_save("rax", register, &lty);
        
        if let Type::Float = lty {
            self.push_xmm("xmm0");
        } else {
            self.push("rdx");
        }
    }

    fn gen_infix_float(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        let lty = lhs.ty();
        let rty = rhs.ty();

        self.gen_expr(lhs);
        self.gen_expr(rhs);

        self.pop_and_convert("xmm1", &Type::Float, &rty);
        self.pop_and_convert("xmm0", &Type::Float, &lty);

        let mut label_num = None;
        match kind {
            Infix::Add => add_mnemonic!(self, "addss xmm0, xmm1"),
            Infix::Sub => add_mnemonic!(self, "subss xmm0, xmm1"),
            Infix::Mul => add_mnemonic!(self, "mulss xmm0, xmm1"),
            Infix::Div => add_mnemonic!(self, "divss xmm0, xmm1"),
            Infix::LessThan => {
                label_num = Some(self.label_num);
                add_mnemonic!(self, "comiss xmm1, xmm0");
                add_mnemonic!(self, "jbe .Lelse{}", label_num.unwrap());
            },
            Infix::LessThanOrEqual => {
                label_num = Some(self.label_num);
                add_mnemonic!(self, "comiss xmm1, xmm0");
                add_mnemonic!(self, "jb .Lelse{}", label_num.unwrap());
            },
            Infix::Equal => {
                label_num = Some(self.label_num);
                add_mnemonic!(self, "ucomiss xmm0, xmm1");
                add_mnemonic!(self, "jne .Lelse{}", label_num.unwrap());
            },
            Infix::NotEqual => {
                label_num = Some(self.label_num);
                add_mnemonic!(self, "ucomiss xmm0, xmm1");
                add_mnemonic!(self, "je .Lelse{}", label_num.unwrap());
            },
            _ => panic!(),
        };

        if let Some(label_num) = label_num {
            self.label_num += 1;
            add_mnemonic!(self, "movss xmm0, .Lfone");
            add_mnemonic!(self, "jmp .Lend{}", label_num);
            add_label!(self, ".Lelse", label_num);
            add_mnemonic!(self, "pxor xmm0, xmm0");
            add_label!(self, ".Lend", label_num);
        }

        self.push_xmm("xmm0");
    }

    fn gen_infix_integer(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        match kind.clone() {
            Infix::Add | Infix::Sub => match (lhs.ty(), rhs.ty()) {
                (Type::Pointer(ty), Type::Int) | (Type::Array(ty, _), Type::Int) => {
                    self.gen_expr(lhs);
                    self.gen_expr(rhs);
                    // rhsにポインタか配列の型のサイズを掛ける
                    self.pop("rdi");
                    add_mnemonic!(self, "mov rax, {}", ty.get_size());
                    add_mnemonic!(self, "imul rdi");
                    self.push("rax");
                },
                (Type::Int, Type::Pointer(ty)) | (Type::Int, Type::Array(ty, _)) => {
                    self.gen_expr(lhs);
                    // lhsにポインタか配列の型のサイズを掛ける
                    self.pop("rdi");
                    add_mnemonic!(self, "mov rax, {}", ty.get_size());
                    add_mnemonic!(self, "imul rdi");
                    self.push("rax");

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

        self.pop("rdi");
        self.pop("rax");

        match kind {
            Infix::Add => add_mnemonic!(self, "add rax, rdi"),
            Infix::Sub => add_mnemonic!(self, "sub rax, rdi"),
            Infix::Mul => add_mnemonic!(self, "imul rdi"),
            Infix::Div => {
                add_mnemonic!(self, "cqo");
                add_mnemonic!(self, "idiv rdi");
            },
            Infix::Mod => {
                add_mnemonic!(self, "cqo");
                add_mnemonic!(self, "idiv rdi");
                add_mnemonic!(self, "mov rax, rdx");
            },
            Infix::BitAnd => add_mnemonic!(self, "and rax, rdi"),
            Infix::BitOr => add_mnemonic!(self, "or rax, rdi"),
            Infix::BitXor => add_mnemonic!(self, "xor rax, rdi"),
            Infix::Shl => {
                add_mnemonic!(self, "mov ecx, edi");
                add_mnemonic!(self, "sal rax, cl");
            },
            Infix::Shr => {
                add_mnemonic!(self, "mov ecx, edi");
                add_mnemonic!(self, "sar rax, cl");
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

        self.push("rax");
    }

    fn gen_infix(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        if lhs.ty().is_integer() && rhs.ty().is_integer() {
            self.gen_infix_integer(kind, lhs, rhs);
        } else {
            self.gen_infix_float(kind, lhs, rhs);
        }
    }

    fn gen_call(&mut self, name: String, args: Vec<Expr>) {
        let mut types: Vec<Type> = Vec::new();
        for arg_expr in args.clone() {
            types.push(arg_expr.ty());
            self.gen_expr(arg_expr);
        }

        let mut arg_reg = 0;
        let mut xmm_arg_reg = 0;
        let params = self.functions[&name].clone();
        for (ty, param_ty) in types.into_iter().zip(params.into_iter()).rev() {
            let reg = match param_ty {
                Type::Float => {
                    xmm_arg_reg += 1;
                    XMM_ARG_REGISTERS[xmm_arg_reg - 1]
                },
                _ => {
                    arg_reg += 1;
                    ARG_REGISTERS[arg_reg - 1]
                },
            };

            if name == "printf" {
                // TODO: Remove later
                if let Type::Float = ty {
                    add_mnemonic!(self, "cvtss2sd {}, [rsp]", reg);
                    add_mnemonic!(self, "add rsp, 4");
                    self.stack_size -= 4;
                } else {
                    self.pop_and_convert(reg, &param_ty, &ty);
                }
            } else {
                self.pop_and_convert(reg, &param_ty, &ty);
            }
        }

        add_mnemonic!(self, "mov al, {}", xmm_arg_reg);

        // Align stack
        let padding = 16 - (self.stack_size - 8) % 16;
        if padding == 16 {
            add_mnemonic!(self, "call {}", name);
        } else {
            add_mnemonic!(self, "sub rsp, {}", padding);
            add_mnemonic!(self, "call {}", name);
            // Revert stack
            add_mnemonic!(self, "add rsp, {}", padding);
        }

        self.push("rax");
    }

    fn gen_bit_not(&mut self, expr: Expr) {
        self.gen_expr(expr);
        self.pop("rax");
        add_mnemonic!(self, "not rax");
        self.push("rax");
    }

    fn gen_inc_or_dec(&mut self, expr: Expr, is_post: bool, is_inc: bool) {
        let opcode = if is_inc { "inc" } else { "dec" };
        let ty = expr.ty();

        self.gen_lvalue(expr);
        self.pop("rax");

        self.gen_load("rbx", "rax", &ty);

        if is_post {
            self.push("rbx");
            add_mnemonic!(self, "{} rbx", opcode);
        } else {
            add_mnemonic!(self, "{} rbx", opcode);
            self.push("rbx");
        }

        self.gen_save("rax", "rbx", &ty);
    }

    fn gen_expr(&mut self, expr: Expr) {
        match expr.kind {
            ExprKind::Literal(Literal::Number(num)) => {
                add_mnemonic!(self, "push {}", num);
                self.stack_size += 8;
            },
            ExprKind::Literal(Literal::String(num)) => {
                add_mnemonic!(self, "lea rax, .Ltext{}[rip]", num);
                self.push("rax");
            },
            ExprKind::Literal(Literal::Float(num)) => {
                add_mnemonic!(self, "movss xmm0, .Lfloat{}[rip]", num);
                self.push_xmm("xmm0");
            },
            ExprKind::Increment(expr, is_post) => self.gen_inc_or_dec(*expr, is_post, true),
            ExprKind::Decrement(expr, is_post) => self.gen_inc_or_dec(*expr, is_post, false),
            ExprKind::Variable(_) | ExprKind::Dereference(_) | ExprKind::MemberAccess(_, _) => self.gen_var_or_deref(expr),
            ExprKind::Address(variable) => self.gen_address(variable),
            ExprKind::Assign(lhs, rhs) => self.gen_assign(*lhs, *rhs),
            ExprKind::Infix(kind, lhs, rhs) => self.gen_infix(kind, *lhs, *rhs),
            ExprKind::Call(name, args) => self.gen_call(name, args),
            ExprKind::BitNot(expr) => self.gen_bit_not(*expr),
            ExprKind::SizeOf(_) => panic!("unexpected sizeof unary operator"),
            ExprKind::Invalid => eprintln!("invalid expression"),
        };
    }

    fn gen_expr_stmt(&mut self, expr: Expr) {
        let size = match &expr.ty {
            Some(Type::Float) => 4,
            _ => 8,
        };

        self.gen_expr(expr);
        add_mnemonic!(self, "add rsp, {}", size);
        self.stack_size -= size;
    }

    fn gen_return_stmt(&mut self, expr: Expr) {
        self.has_return = true;
        self.gen_expr(expr);
        self.pop("rax");
        add_mnemonic!(self, "mov rsp, rbp");
        self.pop("rbp");
        add_mnemonic!(self, "ret");
    }

    fn gen_if_stmt(&mut self, cond: Expr, if_stmt: Stmt, else_stmt: Option<Box<Stmt>>) {
        self.gen_expr(cond);
        self.pop("rax");
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
        add_label!(self, ".Lcontinue", label_num);

        // 条件式
        self.gen_expr(expr);
        self.pop("rax");
        add_mnemonic!(self, "cmp rax, 0");
        add_mnemonic!(self, "je .Lend{}", label_num);

        // 文
        self.break_label_stack.push(label_num);
        self.continue_label_stack.push(label_num);
        self.gen_stmt(stmt);
        self.continue_label_stack.pop().unwrap();
        self.break_label_stack.pop().unwrap();

        add_mnemonic!(self, "jmp .Lbegin{}", label_num);

        add_label!(self, ".Lend", label_num);
    }

    fn gen_for_stmt(&mut self, init: Option<Box<Stmt>>, cond: Option<Expr>, loop_expr: Option<Expr>, stmt: Stmt) {
        self.label_num += 1;
        let label_num = self.label_num;

        // 初期化式
        if let Some(init) = init {
            self.gen_stmt(*init);
        }

        add_label!(self, ".Lbegin", label_num);

        // 条件式
        if let Some(cond) = cond {
            self.gen_expr(cond);
            self.pop("rax");
            add_mnemonic!(self, "cmp rax, 0");
            add_mnemonic!(self, "je .Lend{}", label_num);
        }

        // 文
        self.break_label_stack.push(label_num);
        self.continue_label_stack.push(label_num);
        self.gen_stmt(stmt);
        self.continue_label_stack.pop().unwrap();
        self.break_label_stack.pop().unwrap();

        add_label!(self, ".Lcontinue", label_num);

        if let Some(loop_expr) = loop_expr {
            self.gen_expr(loop_expr);
            self.pop("rax");
        }

        add_mnemonic!(self, "jmp .Lbegin{}", label_num);
        add_label!(self, ".Lend", label_num);
    }

    fn gen_define_stmt(&mut self, variable: Variable, initializer: Option<Initializer>) {
        let offset = match variable.location {
            Location::Local(offset) => offset,
            _ => panic!("ローカル変数ではありません"),
        };

        // 初期化式
        if let Some(initializer) = initializer {
            self.gen_initializer(offset, &variable.ty, initializer);
        }
    }

    fn gen_block_stmt(&mut self, stmt_list: Vec<Stmt>) {
        for stmt in stmt_list {
            self.gen_stmt(stmt);
        }
    }

    fn gen_switch_stmt(&mut self, expr: Expr, cases: Vec<Expr>, stmt: Stmt, has_default: bool) {
        self.label_num += 1;
        let label_num = self.label_num;

        self.gen_expr(expr);
        self.pop("rax");

        // caseにジャンプする処理
        let mut case_labels = Vec::new();
        for case in cases {
            self.label_num += 1;
            case_labels.push(self.label_num);

            self.gen_expr(case);
            self.pop("rbx");
            add_mnemonic!(self, "cmp rax, rbx");
            add_mnemonic!(self, "je .Lcase{}", self.label_num);
        }
        self.case_labels_iter = Box::new(case_labels.into_iter());

        if has_default {
            add_mnemonic!(self, "jmp .Ldefault{}", label_num);
        } else {
            add_mnemonic!(self, "jmp .Lend{}", label_num);
        }

        self.break_label_stack.push(label_num);
        self.default_label.push(label_num);
        self.gen_stmt(stmt);
        self.default_label.pop().unwrap();
        self.break_label_stack.pop().unwrap();

        add_label!(self, ".Lend", label_num)
    }

    fn gen_case_stmt(&mut self) {
        add_label!(self, ".Lcase", self.case_labels_iter.next().unwrap());
    }

    fn gen_default_stmt(&mut self) {
        add_label!(self, ".Ldefault", self.default_label.last().unwrap());
    }

    fn gen_break_stmt(&mut self) {
        add_mnemonic!(self, "jmp .Lend{}", self.break_label_stack.last().unwrap());
    }

    fn gen_continue_stmt(&mut self) {
        add_mnemonic!(self, "jmp .Lcontinue{}", self.continue_label_stack.last().unwrap());
    }

    fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt.kind {
            StmtKind::Expr(expr) => self.gen_expr_stmt(expr),
            StmtKind::Return(expr) => self.gen_return_stmt(expr),
            StmtKind::If(cond, if_stmt, else_stmt) => self.gen_if_stmt(cond, *if_stmt, else_stmt),
            StmtKind::While(expr, stmt) => self.gen_while_stmt(expr, *stmt),
            StmtKind::For(init, cond, loop_expr, stmt) => self.gen_for_stmt(init, cond, loop_expr, *stmt),
            StmtKind::Define(variable, initializer) => self.gen_define_stmt(variable, initializer),
            StmtKind::Block(stmt_list) => self.gen_block_stmt(stmt_list),
            StmtKind::Switch(expr, cases, stmt, has_default) => self.gen_switch_stmt(expr, cases, *stmt, has_default),
            StmtKind::Case(_) => self.gen_case_stmt(),
            StmtKind::Default => self.gen_default_stmt(),
            StmtKind::Break => self.gen_break_stmt(),
            StmtKind::Continue => self.gen_continue_stmt(),
        }
    }

    fn gen_initializer(&mut self, offset: usize, ty: &Type, initializer: Initializer) {
        match initializer.kind {
            InitializerKind::List(initializers) => match ty {
                Type::Array(element_type, _) => {
                    let mut i = 0;
                    for initializer in initializers {
                        let element_offset = offset - i * element_type.get_size();
                        self.gen_initializer(element_offset, &element_type, initializer);
                        i += 1;
                    }
                },
                Type::Structure(members, _) => {
                    let mut i = 0;
                    for initializer in initializers {
                        let member_offset = offset - members[i].1.offset();
                        self.gen_initializer(member_offset, &members[i].1.ty, initializer);
                        i += 1;
                    }
                },
                _ => panic!(),
            },
            InitializerKind::Expr(expr) => {
                let dst_ty = ty;
                let src_ty = expr.ty();
                self.gen_expr(expr);

                let reg = if let Type::Float = dst_ty {
                    self.pop_and_convert("xmm0", dst_ty, &src_ty);
                    "xmm0"
                } else {
                    self.pop_and_convert("rax", dst_ty, &src_ty);
                    "rax"
                };

                self.gen_save(&format!("rbp-{}", offset), reg, dst_ty);
            },
        };
    }

    pub fn gen_declaration(&mut self, declaration: Declaration) {
        match declaration.kind {
            DeclarationKind::Func(name, _, args, stack_size, block, is_static) => {
                self.functions.insert(name.clone(), args.clone().into_iter().map(|arg| arg.ty).collect());

                if !is_static {
                    self.code.push_str(&format!(".global {}\n", name));
                }

                add_label!(self, &name);

                self.stack_size = 0;

                self.push("rbp");
                add_mnemonic!(self, "mov rbp, rsp");
                add_mnemonic!(self, "sub rsp, {}", stack_size);

                self.stack_size += stack_size;

                // Store argument values to stack
                let mut reg = 0;
                let mut xmm_reg = 0;
                for arg in args {
                    let dst = format!("rbp-{}", arg.offset());
                    match arg.ty {
                        Type::Float => {
                            self.gen_save(&dst, XMM_ARG_REGISTERS[xmm_reg], &Type::Float);
                            xmm_reg += 1;
                        },
                        ty => {
                            self.gen_save(&dst, ARG_REGISTERS[reg], &ty);
                            reg += 1;
                        },
                    };
                }

                self.has_return = false;
                self.gen_stmt(block);

                self.stack_size -= stack_size;

                // return文がなかったら0を返す
                if !self.has_return {
                    add_mnemonic!(self, "mov rax, 0");
                    add_mnemonic!(self, "mov rsp, rbp");
                    self.pop("rbp");
                    add_mnemonic!(self, "ret");
                }
            },
            DeclarationKind::Prototype(name, _, args) => { self.functions.insert(name.clone(), args.clone()); },
            DeclarationKind::Extern(box Declaration { kind: DeclarationKind::Prototype(name, _, args), .. }) => { self.functions.insert(name.clone(), args.clone()); },
            _ => {},
        }
    }

    pub fn gen_global_var(&mut self, declarations: &Vec<Declaration>) {
        self.code.push_str(".data\n");
        // グローバル変数
        for declaration in declarations {
            match &declaration.kind {
                DeclarationKind::GlobalVariable(variable, initializer, is_static) => {
                    if !is_static {
                        self.code.push_str(&format!(".global {}\n", global!(variable.clone())));
                    }

                    add_label!(self, global!(variable.clone()));

                    // 初期値
                    if let Some(initializer) = initializer {
                        self.gen_global_initializer(&variable.ty, initializer.clone());
                    } else {
                        // 初期化式がない場合は0で初期化
                        add_mnemonic!(self, ".zero {}", variable.ty.get_size());
                    }
                },
                _ => {},
            };
        }
    }

    fn gen_global_init_expr(&mut self, ty: &Type, init_expr: Expr) {
        match (ty.clone(), init_expr.kind.clone()) {
            (Type::Int, ExprKind::Literal(Literal::Number(num))) => add_mnemonic!(self, ".int {}", num),
            (Type::Char, ExprKind::Literal(Literal::Number(num))) => add_mnemonic!(self, ".byte {}", num),
            (Type::Short, ExprKind::Literal(Literal::Number(num))) => add_mnemonic!(self, ".value {}", num),
            (Type::Long, ExprKind::Literal(Literal::Number(num))) => add_mnemonic!(self, ".quad {}", num),
            (Type::Pointer(_), ExprKind::Address(variable)) => add_mnemonic!(self, ".quad {}", global!(variable)),
            // TODO: ポインタ演算
            (Type::Array(box Type::Char, _), ExprKind::Literal(Literal::String(num))) |
            (Type::Pointer(box Type::Char), ExprKind::Literal(Literal::String(num))) |
            (Type::Pointer(box Type::Const(box Type::Char)), ExprKind::Literal(Literal::String(num))) => add_mnemonic!(self, ".quad .Ltext{}", num),
            (Type::Const(ty), _) => self.gen_global_init_expr(&ty, init_expr),
            _ => panic!("サポートしていない初期化式です"),
        };
    }

    fn gen_global_initializer(&mut self, ty: &Type, initializer: Initializer) {
        match initializer.kind {
            InitializerKind::List(initializers) => match ty {
                Type::Array(element_type, size) => {
                    let mut i = 0;
                    for initializer in initializers {
                        self.gen_global_initializer(&element_type, initializer);
                        i += 1;
                    }

                    let padding = (size - i) * element_type.get_size();
                    if padding > 0 {
                        add_mnemonic!(self, ".zero {}", padding);
                    }
                },
                Type::Structure(members, _) => {
                    let mut size = 0;

                    let mut get_padding = |ty: &Type| -> usize {
                        size += ty.get_size();
                        let align = ty.align();
                        let padding = align - size % align;
                        if padding != align {
                            size += padding;
                            padding
                        } else {
                            0
                        }
                    };

                    let mut i = 0;
                    for initializer in initializers {
                        let member_ty = &members[i].1.ty;

                        let padding = get_padding(member_ty);
                        if padding > 0 {
                            add_mnemonic!(self, ".zero {}", padding);
                        }

                        self.gen_global_initializer(member_ty, initializer);

                        i += 1;
                    }

                    for (_, member) in members.iter().skip(i) {
                        let padding = get_padding(&member.ty);
                        if padding > 0 {
                            add_mnemonic!(self, ".zero {}", padding);
                        }

                        add_mnemonic!(self, ".zero {}", member.ty.get_size());
                    }
                }
                _ => panic!(),
            },
            InitializerKind::Expr(expr) => {
                self.gen_global_init_expr(ty, expr);
            },
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

        // Floating point number literals
        self.code.push_str(".data\n");
        add_label!(self, ".Lfone");
        add_mnemonic!(self, ".long 1065353216");
        for (i, float_num) in program.float_list.into_iter().enumerate() {
            add_label!(self, ".Lfloat", i);
            add_mnemonic!(self, ".long {}", float_num.to_bits());
        }



        self.gen_global_var(&program.declarations);

        self.code.push_str(".text\n");
        for declaration in program.declarations {
            self.gen_declaration(declaration);
        }
    }
}
