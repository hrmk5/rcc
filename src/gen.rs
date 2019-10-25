use std::collections::HashMap;
use crate::ast::*;
use crate::id::{Id, IdMap};

#[derive(Clone, Debug)]
struct Function {
    return_type: Type,
    params: Vec<Type>,
}

pub struct Generator {
    pub code: String,
    label_num: u32,
    label_count: u32,
    has_return: bool,
    case_labels_iter: Box<dyn Iterator<Item = u32>>,
    break_label_stack: Vec<u32>,
    continue_label_stack: Vec<u32>,
    default_label: Vec<u32>,
    stack_size: usize,
    functions: HashMap<Id, Function>,
    curr_func: Option<Id>,
    id_map: IdMap,
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
    pub fn new(id_map: IdMap) -> Self {
        Generator {
            code: String::new(),
            label_num: 0,
            label_count: 0,
            has_return: false,
            case_labels_iter: Box::new(Vec::new().into_iter()),
            break_label_stack: Vec::new(),
            continue_label_stack: Vec::new(),
            default_label: Vec::new(),
            stack_size: 0,
            functions: HashMap::new(),
            curr_func: None,
            id_map,
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

    fn get_label_num(&mut self) -> u32 {
        let label_num = self.label_num;
        self.label_num += 1;
        label_num
    }

    fn push(&mut self, reg: &'static str) {
        add_mnemonic!(self, "push {}", reg);
        self.stack_size += 8;
    }

    fn pop(&mut self, reg: &'static str) {
        add_mnemonic!(self, "pop {}", reg);
        self.stack_size -= 8;
    }

    fn push_xmm(&mut self, reg: &'static str, ty: &Type) {
        add_mnemonic!(self, "sub rsp, 8");

        match ty {
            Type::Float => add_mnemonic!(self, "movss [rsp], {}", reg),
            Type::Double => add_mnemonic!(self, "movsd [rsp], {}", reg),
            _ => panic!(),
        };

        self.stack_size += 8;
    }

    fn pop_xmm(&mut self, reg: &'static str) {
        add_mnemonic!(self, "movss {}, [rsp]", reg);
        add_mnemonic!(self, "add rsp, 8");
        self.stack_size -= 8;
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
                        add_mnemonic!(self, "mov rax, {}", self.id_map.name(&name));
                    },
                };
            },
            _ => {
                self.gen_expr(expr);
                self.pop("rax");
            },
        }
    }

    fn gen_lvalue(&mut self, expr: Expr) {
        match expr.kind {
            ExprKind::Dereference(expr) => {
                self.gen_dereference(*expr);
                self.push("rax");
            },
            ExprKind::Variable(variable) => {
                match variable.location {
                    Location::Local(offset) => add_mnemonic!(self, "lea rax, [rbp-{}]", offset),
                    Location::Global(name) => add_mnemonic!(self, "lea rax, {}", self.id_map.name(&name)),
                };

                self.push("rax");
            },
            ExprKind::MemberAccess(lhs, member) => {
                let ty = lhs.ty();
                let member = ty.find_member(&member);

                self.gen_lvalue(*lhs);
                self.pop("rax");
                add_mnemonic!(self, "lea rax, [rax+{}]", member.offset());
                self.push("rax");
            },
            _ => {
                panic!("Not lvalue");
            },
        }
    }

    fn pop_and_convert(&mut self, dst: &'static str, dst_ty: &Type, src_ty: &Type) {
        self.gen_load_and_convert(dst, "rsp", dst_ty, src_ty);
        add_mnemonic!(self, "add rsp, 8");
        self.stack_size -= 8;
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
            // double <- double
            (Type::Double, Type::Double) => {
                add_mnemonic!(self, "movsd {}, {} [{}]", dst, size_str, src);
            },
            // integer <- double
            (Type::Double, dst_ty) if dst_ty.is_integer() => {
                add_mnemonic!(self, "cvttsd2si {}, {} [{}]", dst, size_str, src);
            },
            // double <- integer
            (src_ty, Type::Double) if src_ty.is_integer() => {
                if src_ty.get_size() < 4 {
                    add_mnemonic!(self, "movsx eax, {} [{}]", size_str, src);
                    add_mnemonic!(self, "cvtsi2sd {}, eax", dst);
                } else {
                    add_mnemonic!(self, "cvtsi2sd {}, {} [{}]", dst, size_str, src);
                }
            },
            // float <- double
            (Type::Double, Type::Float) => {
                add_mnemonic!(self, "cvtsd2ss {}, {} [{}]", dst, size_str, src);
            },
            // double <- float
            (Type::Float, Type::Double) => {
                add_mnemonic!(self, "cvtss2sd {}, {} [{}]", dst, size_str, src);
            },
            // integer <- integer
            _ => self.gen_load(dst, src, dst_ty),
        };
    }

    
    fn gen_load(&mut self, dst: &'static str, src: &str, ty: &Type) {
        match ty {
            Type::Float => {
                add_mnemonic!(self, "movss {}, [{}]", dst, src);
            },
            Type::Double => {
                add_mnemonic!(self, "movsd {}, [{}]", dst, src);
            },
            ty => {
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
    }

    fn gen_save(&mut self, dst: &str, src: &'static str, dst_ty: &Type) {
        match dst_ty {
            Type::Float => {
                add_mnemonic!(self, "movss [{}], {}", dst, src);
            },
            Type::Double => {
                add_mnemonic!(self, "movsd [{}], {}", dst, src);
            },
            _ => {
                let size = match dst_ty {
                    Type::Array(_, _) => 8,
                    ty => ty.get_size(),
                };

                let src = self.get_size_register(size, src).unwrap();
                add_mnemonic!(self, "mov [{}], {}", dst, src);
            },
        }
    }

    fn gen_var_or_deref(&mut self, expr: Expr) {
        let ty = expr.ty();

        self.gen_lvalue(expr);
        self.pop("rax");

        if ty.is_floating_number() {
            self.gen_load("xmm0", "rax", &ty);
            self.push_xmm("xmm0", &ty);
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
                add_mnemonic!(self, "lea rax, {}", self.id_map.name(&name));
                self.push("rax");
            },
        };
    }

    fn gen_copy_structure(&mut self, dst: &str, src: &str, members: Vec<(Id, Variable)>) {
        for (_, member) in members {
            let reg = if member.ty.is_floating_number() {
                "xmm0"
            } else {
                "rcx"
            };

            let dst = format!("{}+{}", dst, member.offset());
            let src = format!("{}+{}", src, member.offset());
            self.gen_load(reg, &src, &member.ty);
            self.gen_save(&dst, reg, &member.ty);
        }
    }

    fn gen_assign(&mut self, lhs: Expr, rhs: Expr) {
        let lty = lhs.ty();
        let rty = rhs.ty();

        if let (Type::Structure(_, members, _), Type::Structure(_, _, _)) = (lty.clone(), &rty) {
            self.gen_lvalue(lhs);
            self.gen_lvalue(rhs);
            self.pop("rbx");
            self.pop("rax");

            self.gen_copy_structure("rax", "rbx", members);

            self.push("rax");
        } else {
            self.gen_lvalue(lhs);
            self.gen_expr(rhs);
            
            let register = if lty.is_floating_number() {
                self.pop_and_convert("xmm0", &lty, &rty);
                "xmm0"
            } else {
                self.pop_and_convert("rdx", &lty, &rty);
                "rdx"
            };
            
            self.pop("rax");
            self.gen_save("rax", register, &lty);
            
            if lty.is_floating_number() {
                self.push_xmm("xmm0", &lty);
            } else {
                self.push("rdx");
            }
        }
    }

    fn gen_infix_double(&mut self, kind: Infix, lty: &Type, rty: &Type) {
        self.pop_and_convert("xmm1", &Type::Double, rty);
        self.pop_and_convert("xmm0", &Type::Double, lty);

        let mut label_num = None;
        match kind {
            Infix::Add => add_mnemonic!(self, "addsd xmm0, xmm1"),
            Infix::Sub => add_mnemonic!(self, "subsd xmm0, xmm1"),
            Infix::Mul => add_mnemonic!(self, "mulsd xmm0, xmm1"),
            Infix::Div => add_mnemonic!(self, "divsd xmm0, xmm1"),
            Infix::LessThan => {
                label_num = Some(self.get_label_num());
                add_mnemonic!(self, "comisd xmm1, xmm0");
                add_mnemonic!(self, "jbe .Lelse{}", label_num.unwrap());
            },
            Infix::LessThanOrEqual => {
                label_num = Some(self.get_label_num());
                add_mnemonic!(self, "comisd xmm1, xmm0");
                add_mnemonic!(self, "jb .Lelse{}", label_num.unwrap());
            },
            Infix::Equal => {
                label_num = Some(self.get_label_num());
                add_mnemonic!(self, "ucomisd xmm0, xmm1");
                add_mnemonic!(self, "jne .Lelse{}", label_num.unwrap());
            },
            Infix::NotEqual => {
                label_num = Some(self.get_label_num());
                add_mnemonic!(self, "ucomisd xmm0, xmm1");
                add_mnemonic!(self, "je .Lelse{}", label_num.unwrap());
            },
            _ => panic!(),
        };

        if let Some(label_num) = label_num {
            add_mnemonic!(self, "movsd xmm0, .Ldone");
            add_mnemonic!(self, "jmp .Lend{}", label_num);
            add_label!(self, ".Lelse", label_num);
            add_mnemonic!(self, "pxor xmm0, xmm0");
            add_label!(self, ".Lend", label_num);
        }
    }

    fn gen_infix_float(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        let lty = lhs.ty();
        let rty = rhs.ty();

        self.gen_expr(lhs);
        self.gen_expr(rhs);

        let ty = if let (Type::Double, _) | (_, Type::Double) = (&lty, &rty) {
            self.gen_infix_double(kind, &lty, &rty);
            Type::Double
        } else {
            self.pop_and_convert("xmm1", &Type::Float, &rty);
            self.pop_and_convert("xmm0", &Type::Float, &lty);

            let mut label_num = None;
            match kind {
                Infix::Add => add_mnemonic!(self, "addss xmm0, xmm1"),
                Infix::Sub => add_mnemonic!(self, "subss xmm0, xmm1"),
                Infix::Mul => add_mnemonic!(self, "mulss xmm0, xmm1"),
                Infix::Div => add_mnemonic!(self, "divss xmm0, xmm1"),
                Infix::LessThan => {
                    label_num = Some(self.get_label_num());
                    add_mnemonic!(self, "comiss xmm1, xmm0");
                    add_mnemonic!(self, "jbe .Lelse{}", label_num.unwrap());
                },
                Infix::LessThanOrEqual => {
                    label_num = Some(self.get_label_num());
                    add_mnemonic!(self, "comiss xmm1, xmm0");
                    add_mnemonic!(self, "jb .Lelse{}", label_num.unwrap());
                },
                Infix::Equal => {
                    label_num = Some(self.get_label_num());
                    add_mnemonic!(self, "ucomiss xmm0, xmm1");
                    add_mnemonic!(self, "jne .Lelse{}", label_num.unwrap());
                },
                Infix::NotEqual => {
                    label_num = Some(self.get_label_num());
                    add_mnemonic!(self, "ucomiss xmm0, xmm1");
                    add_mnemonic!(self, "je .Lelse{}", label_num.unwrap());
                },
                _ => panic!(),
            };

            if let Some(label_num) = label_num {
                add_mnemonic!(self, "movss xmm0, .Lfone");
                add_mnemonic!(self, "jmp .Lend{}", label_num);
                add_label!(self, ".Lelse", label_num);
                add_mnemonic!(self, "pxor xmm0, xmm0");
                add_label!(self, ".Lend", label_num);
            }
            Type::Float
        };

        self.push_xmm("xmm0", &ty);
    }

    fn gen_infix_integer(&mut self, kind: Infix, lhs: Expr, rhs: Expr) {
        match kind.clone() {
            Infix::Add | Infix::Sub => match (lhs.ty(), rhs.ty()) {
                (Type::Pointer(ty), Type::Int) | (Type::Array(ty, _), Type::Int) => {
                    self.gen_expr(lhs);
                    self.gen_expr(rhs);
                    // rhs multiply by pointer or array type size
                    self.pop("rdi");
                    add_mnemonic!(self, "mov rax, {}", ty.get_size());
                    add_mnemonic!(self, "imul rdi");
                    self.push("rax");
                },
                (Type::Int, Type::Pointer(ty)) | (Type::Int, Type::Array(ty, _)) => {
                    self.gen_expr(lhs);
                    // lhs multiply by pointer or array type size
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
        if lhs.ty().is_floating_number() || rhs.ty().is_floating_number() {
            self.gen_infix_float(kind, lhs, rhs);
        } else {
            self.gen_infix_integer(kind, lhs, rhs);
        }
    }

    fn gen_call(&mut self, name: &Id, args: Vec<Expr>) {
        let Function { params, return_type, .. } = self.functions[name].clone();

        let mut arg_reg = 0;
        let mut xmm_arg_reg = 0;

        let mut types: Vec<(Type, &'static str)> = Vec::new();
        for (arg_expr, param_ty) in args.clone().into_iter().zip(params.iter()) {
            // Determine a register
            let reg = match param_ty {
                ty if ty.is_floating_number() => {
                    xmm_arg_reg += 1;
                    XMM_ARG_REGISTERS[xmm_arg_reg - 1]
                },
                _ => {
                    arg_reg += 1;
                    ARG_REGISTERS[arg_reg - 1]
                },
            };

            types.push((arg_expr.ty(), reg));

            // Push the argument value
            self.gen_expr(arg_expr);
        }

        // Store arguments into register
        for ((ty, reg), param_ty) in types.into_iter().zip(params.into_iter()).rev() {
            self.pop_and_convert(reg, &param_ty, &ty);
        }

        add_mnemonic!(self, "mov al, {}", xmm_arg_reg);

        // Align the stack
        let padding = 16 - (self.stack_size - 8) % 16;
        if padding == 16 {
            add_mnemonic!(self, "call {}", self.id_map.name(&name));
        } else {
            add_mnemonic!(self, "sub rsp, {}", padding);
            add_mnemonic!(self, "call {}", self.id_map.name(&name));
            // Revert the stack
            add_mnemonic!(self, "add rsp, {}", padding);
        }

        // Push the return value
        if return_type.is_floating_number() {
            self.push_xmm("xmm0", &return_type);
        } else {
            self.push("rax");
        }
    }

    fn gen_bit_not(&mut self, expr: Expr) {
        self.gen_expr(expr);
        self.pop("rax");
        add_mnemonic!(self, "not rax");
        self.push("rax");
    }

    fn gen_inc_or_dec(&mut self, expr: Expr, is_post: bool, is_inc: bool) {
        let ty = expr.ty();

        self.gen_lvalue(expr);
        self.pop("rax");

        if ty.is_floating_number() {
            let (opcode, one) = match &ty {
                Type::Float => {
                    (if is_inc { "addss" } else { "subss" }, ".Lfone")
                },
                Type::Double => {
                    (if is_inc { "addsd" } else { "subsd" }, ".Ldone")
                },
                _ => panic!(),
            };

            self.gen_load("xmm0", "rax", &ty);

            if is_post {
                self.push_xmm("xmm0", &ty);
                add_mnemonic!(self, "{} xmm0, {}", opcode, one);
            } else {
                add_mnemonic!(self, "{} xmm0, {}", opcode, one);
                self.push_xmm("xmm0", &ty);
            }

            self.gen_save("rax", "xmm0", &ty);
        } else {
            let opcode = if is_inc { "inc" } else { "dec" };

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
    }

    fn gen_expr(&mut self, expr: Expr) {
        match expr.kind {
            ExprKind::Literal(Literal::Number(num)) => {
                add_mnemonic!(self, "push {}", num);
                self.stack_size += 8;
            },
            ExprKind::Literal(Literal::String(num)) => {
                add_mnemonic!(self, "lea rax, .Ltext{}", num);
                self.push("rax");
            },
            ExprKind::Literal(Literal::Float(num)) => {
                add_mnemonic!(self, "movss xmm0, .Lfloat{}", num);
                self.push_xmm("xmm0", &Type::Float);
            },
            ExprKind::Literal(Literal::Double(num)) => {
                add_mnemonic!(self, "movsd xmm0, .Ldouble{}", num);
                self.push_xmm("xmm0", &Type::Double);
            },
            ExprKind::Increment(expr, is_post) => self.gen_inc_or_dec(*expr, is_post, true),
            ExprKind::Decrement(expr, is_post) => self.gen_inc_or_dec(*expr, is_post, false),
            ExprKind::Variable(_) | ExprKind::Dereference(_) | ExprKind::MemberAccess(_, _) => self.gen_var_or_deref(expr),
            ExprKind::Address(variable) => self.gen_address(variable),
            ExprKind::Assign(lhs, rhs) => self.gen_assign(*lhs, *rhs),
            ExprKind::Infix(kind, lhs, rhs) => self.gen_infix(kind, *lhs, *rhs),
            ExprKind::Call(name, args) => self.gen_call(&name, args),
            ExprKind::BitNot(expr) => self.gen_bit_not(*expr),
            ExprKind::SizeOf(_) => panic!("unexpected sizeof unary operator"),
            ExprKind::Invalid => eprintln!("invalid expression"),
        };
    }

    fn gen_cmp(&mut self, cond: Expr) {
        let ty = cond.ty();

        self.gen_expr(cond);

        match ty {
            Type::Float => {
                self.pop_and_convert("xmm0", &Type::Float, &ty);
                add_mnemonic!(self, "pxor xmm1, xmm1");
                add_mnemonic!(self, "ucomiss xmm0, xmm1");
            },
            Type::Double => {
                self.pop_and_convert("xmm0", &Type::Double, &ty);
                add_mnemonic!(self, "pxor xmm1, xmm1");
                add_mnemonic!(self, "ucomisd xmm0, xmm1");
            },
            _ => {
                self.pop("rax");
                add_mnemonic!(self, "cmp rax, 0");
            }
        };
    }

    fn gen_expr_stmt(&mut self, expr: Expr) {
        self.gen_expr(expr);

        // Have to pop because gen_expr always push a result
        add_mnemonic!(self, "add rsp, 8");
        self.stack_size -= 8;
    }

    fn gen_return_stmt(&mut self, expr: Expr) {
        let return_type = self.functions[&self.curr_func.unwrap()].return_type.clone();
        let ty = expr.ty();

        self.has_return = true;
        self.gen_expr(expr);

        let reg = if return_type.is_floating_number() {
            "xmm0"
        } else {
            "rax"
        };

        self.pop_and_convert(reg, &return_type, &ty);

        // Epilogue
        add_mnemonic!(self, "mov rsp, rbp");
        self.pop("rbp");
        add_mnemonic!(self, "ret");

        // Add size of rbp for return statements below
        self.stack_size += 8;
    }

    fn gen_if_stmt(&mut self, cond: Expr, if_stmt: Stmt, else_stmt: Option<Box<Stmt>>) {
        let label_num = self.get_label_num();

        self.gen_cmp(cond);

        if let Some(else_stmt) = else_stmt {
            add_mnemonic!(self, "je .Lelse{}", label_num);

            // Generate the statement in if
            self.gen_stmt(if_stmt);
            add_mnemonic!(self, "jmp .Lend{}", label_num);

            // Generate the statement in else
            add_label!(self, ".Lelse", label_num);
            self.gen_stmt(*else_stmt);
        } else {
            // Generate the statement in if
            add_mnemonic!(self, "je .Lend{}", label_num);
            self.gen_stmt(if_stmt);
        }

        add_label!(self, ".Lend", label_num);
    }

    fn gen_while_stmt(&mut self, expr: Expr, stmt: Stmt) {
        let label_num = self.get_label_num();

        add_label!(self, ".Lbegin", label_num);
        add_label!(self, ".Lcontinue", label_num);

        // Generate the condition
        self.gen_cmp(expr);
        add_mnemonic!(self, "je .Lend{}", label_num);

        // Generate the statement
        self.break_label_stack.push(label_num);
        self.continue_label_stack.push(label_num);
        self.gen_stmt(stmt);
        self.continue_label_stack.pop().unwrap();
        self.break_label_stack.pop().unwrap();

        add_mnemonic!(self, "jmp .Lbegin{}", label_num);

        add_label!(self, ".Lend", label_num);
    }

    fn gen_for_stmt(&mut self, init: Option<Box<Stmt>>, cond: Option<Expr>, loop_expr: Option<Expr>, stmt: Stmt) {
        let label_num = self.get_label_num();

        // Generate the initialization
        if let Some(init) = init {
            self.gen_stmt(*init);
        }

        add_label!(self, ".Lbegin", label_num);

        // Generate the condition
        if let Some(cond) = cond {
            self.gen_cmp(cond);
            add_mnemonic!(self, "je .Lend{}", label_num);
        }

        // Generate the statement
        self.break_label_stack.push(label_num);
        self.continue_label_stack.push(label_num);
        self.gen_stmt(stmt);
        self.continue_label_stack.pop().unwrap();
        self.break_label_stack.pop().unwrap();

        add_label!(self, ".Lcontinue", label_num);

        // Generate the loop expression
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
        let label_num = self.get_label_num();

        self.gen_expr(expr);
        self.pop("rax");

        // Jump to case label
        let mut case_labels = Vec::new();
        for case in cases {
            let label_num = self.get_label_num();
            case_labels.push(label_num);

            self.gen_expr(case);
            self.pop("rbx");
            add_mnemonic!(self, "cmp rax, rbx");
            add_mnemonic!(self, "je .Lcase{}", label_num);
        }

        // Save case label numbers. this will use in "gen_case_stmt"
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

    fn gen_goto_stmt(&mut self, _: &Id, label_num: u32) {
        add_mnemonic!(self, "jmp .Llabel{}", label_num);
    }

    fn gen_label(&mut self, _: &Id) {
        add_label!(self, ".Llabel", self.label_count);
        self.label_count += 1;
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
            StmtKind::Goto(name, label_num) => self.gen_goto_stmt(&name, label_num),
            StmtKind::Label(name) => self.gen_label(&name),
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
                Type::Structure(_, members, _) => {
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

                if let Type::Structure(_, members, _) = src_ty {
                    self.gen_lvalue(expr);
                    self.pop("rax");

                    for (_, member) in members {
                        let reg = if member.ty.is_floating_number() {
                            "xmm0"
                        } else {
                            "rbx"
                        };

                        let dst = format!("rbp-{}+{}", offset, member.offset());
                        let src = format!("rax+{}", member.offset());

                        self.gen_load(reg, &src, &member.ty);
                        self.gen_save(&dst, reg, &member.ty);
                    }
                } else {
                    self.gen_expr(expr);

                    let reg = if dst_ty.is_floating_number() {
                        self.pop_and_convert("xmm0", dst_ty, &src_ty);
                        "xmm0"
                    } else {
                        self.pop_and_convert("rax", dst_ty, &src_ty);
                        "rax"
                    };

                    self.gen_save(&format!("rbp-{}", offset), reg, dst_ty);
                }
            },
        };
    }

    pub fn gen_declaration(&mut self, declaration: Declaration) {
        match declaration.kind {
            DeclarationKind::Func(name, return_type, args, stack_size, block, is_static) => {
                self.curr_func = Some(name);
                self.functions.insert(name, Function {
                    return_type,
                    params: args.clone().into_iter().map(|arg| arg.ty).collect(),
                });

                if !is_static {
                    self.code.push_str(&format!(".global {}\n", self.id_map.name(&name)));
                }

                add_label!(self, self.id_map.name(&name));

                self.stack_size = 0;

                // Prologue
                self.push("rbp");
                add_mnemonic!(self, "mov rbp, rsp");
                add_mnemonic!(self, "sub rsp, {}", stack_size);

                self.stack_size += stack_size;

                // Store argument values into the stack
                let mut reg = 0;
                let mut xmm_reg = 0;
                for arg in args {
                    let dst = format!("rbp-{}", arg.offset());
                    match arg.ty {
                        ty if ty.is_floating_number() => {
                            self.gen_save(&dst, XMM_ARG_REGISTERS[xmm_reg], &ty);
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

                // If doesn't have return statements, return 0
                if !self.has_return {
                    add_mnemonic!(self, "mov rax, 0");
                    add_mnemonic!(self, "mov rsp, rbp");
                    self.pop("rbp");
                    add_mnemonic!(self, "ret");
                }
            },
            DeclarationKind::Prototype(name, return_type, args) => { self.functions.insert(name.clone(), Function {
                return_type,
                params: args,
            } ); },
            DeclarationKind::Extern(box Declaration { kind: DeclarationKind::Prototype(name, return_type, args), .. }) => { self.functions.insert(name.clone(), Function {
                return_type,
                params: args,
            } ); },
            _ => {},
        }
    }

    pub fn gen_global_var(&mut self, declarations: &Vec<Declaration>) {
        self.code.push_str(".data\n");
        for declaration in declarations {
            match &declaration.kind {
                DeclarationKind::GlobalVariable(variable, initializer, is_static) => {
                    if !is_static {
                        self.code.push_str(&format!(".global {}\n", self.id_map.name(&global!(variable.clone()))));
                    }

                    add_label!(self, self.id_map.name(&global!(variable.clone())));

                    // Initializer
                    if let Some(initializer) = initializer {
                        self.gen_global_initializer(&variable.ty, initializer.clone());
                    } else {
                        // Initialize to 0 if there is no initializer
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
            (Type::Pointer(_), ExprKind::Address(variable)) => add_mnemonic!(self, ".quad {}", self.id_map.name(&global!(variable))),
            // TODO: Pointer arithmetic
            (Type::Array(box Type::Char, _), ExprKind::Literal(Literal::String(num))) |
            (Type::Pointer(box Type::Char), ExprKind::Literal(Literal::String(num))) |
            (Type::Pointer(box Type::Const(box Type::Char)), ExprKind::Literal(Literal::String(num))) => add_mnemonic!(self, ".quad .Ltext{}", num),
            (Type::Const(ty), _) => self.gen_global_init_expr(&ty, init_expr),
            _ => panic!(),
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
                Type::Structure(_, members, _) => {
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

    pub fn gen_double_literal(&mut self, literals: Vec<f64>) {
        for (i, num) in literals.into_iter().enumerate() {
            let bits = num.to_bits();
            let high = (bits & 0b1111111111111111111111111111111100000000000000000000000000000000) / 2u64.pow(32);
            let low = bits & 0b0000000000000000000000000000000011111111111111111111111111111111;
            add_label!(self, ".Ldouble", i);
            add_mnemonic!(self, ".long {}", low);
            add_mnemonic!(self, ".long {}", high);
        }
    }

    pub fn gen(&mut self, program: Program) {
        self.code.push_str(".intel_syntax noprefix\n");
        self.code.push_str(".global main\n");

        // String literals
        self.code.push_str(".section .rodata\n");
        for (i, string) in program.string_list.into_iter().enumerate() {
            add_label!(self, ".Ltext", i);
            add_mnemonic!(self, ".string \"{}\"", string);
        }

        self.code.push_str(".data\n");

        // 1 and 0
        add_label!(self, ".Lfone");
        add_mnemonic!(self, ".long 1065353216");
        add_label!(self, ".Ldone");
        add_mnemonic!(self, ".long 0");
        add_mnemonic!(self, ".long 1072693248");

        // Floating point number literals
        for (i, float_num) in program.float_list.into_iter().enumerate() {
            add_label!(self, ".Lfloat", i);
            add_mnemonic!(self, ".long {}", float_num.to_bits());
        }

        self.gen_double_literal(program.double_list);

        self.gen_global_var(&program.declarations);

        self.code.push_str(".text\n");
        for declaration in program.declarations {
            self.gen_declaration(declaration);
        }
    }
}
