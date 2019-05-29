use std::collections::HashMap;
use crate::parser::{Program, Stmt, Expr, Literal, Infix};

fn gen_lvalue(expr: &Expr, asm: &mut String) {
    match expr {
        Expr::Ident(offset) => {
            asm.push_str("  mov rax, rbp\n");
            asm.push_str(&format!("  sub rax, {}\n", offset));
            asm.push_str("  push rax\n");
        },
        _ => {
            println!("代入の左辺値が変数ではありません");
        },
    };
}

fn gen_expr(expr: &Expr, asm: &mut String) {
    match expr {
        Expr::Literal(Literal::Number(num)) => {
            asm.push_str(&format!("  push {}\n", num));
        },
        Expr::Ident(_) => {
            gen_lvalue(expr, asm);
            asm.push_str("  pop rax\n");
            asm.push_str("  mov rax, [rax]\n");
            asm.push_str("  push rax\n");
        },
        Expr::Assign(lhs, rhs) => {
            gen_lvalue(lhs, asm);
            gen_expr(rhs, asm);

            asm.push_str("  pop rdi\n");
            asm.push_str("  pop rax\n");
            asm.push_str("  mov [rax], rdi\n");
            asm.push_str("  push rdi\n");
        },
        Expr::Infix(kind, lhs, rhs) => {
            gen_expr(lhs, asm);
            gen_expr(rhs, asm);

            asm.push_str("  pop rdi\n");
            asm.push_str("  pop rax\n");

            asm.push_str(match kind {
                Infix::Add => "  add rax, rdi\n",
                Infix::Sub => "  sub rax, rdi\n",
                Infix::Mul => "  imul rdi\n",
                Infix::Div => "  cqo\n  idiv rdi\n",
                Infix::Equal => "  cmp rax, rdi\n  sete al\n  movzb  rax, al\n",
                Infix::NotEqual => "  cmp rax, rdi\n  setne al\n  movzb  rax, al\n",
                Infix::LessThan => "  cmp rax, rdi\n  setl al\n  movzb  rax, al\n",
                Infix::LessThanOrEqual => "  cmp rax, rdi\n  setle al\n  movzb  rax, al\n",
            });

            asm.push_str("  push rax\n");
        }
        _ => {},
    };
}

pub fn gen(program: &Program, variables: &HashMap<String, usize>, asm: &mut String) {
    asm.push_str(".intel_syntax noprefix\n");
    asm.push_str(".global main\n");
    asm.push_str("main:\n");

    asm.push_str("  push rbp\n");
    asm.push_str("  mov rbp, rsp\n");
    asm.push_str(&format!("  sub rsp, {}\n", variables.len() * 8));

    for stmt in &program.0 {
        match stmt {
            Stmt::Expr(expr) => gen_expr(&expr, asm),
            Stmt::Return(expr) => {
                gen_expr(&expr, asm);
                asm.push_str("  pop rax\n");
                asm.push_str("  mov rsp, rbp\n");
                asm.push_str("  pop rbp\n");
                asm.push_str("  ret\n");
            },
        }
        asm.push_str("  pop rax\n");
    }
}
