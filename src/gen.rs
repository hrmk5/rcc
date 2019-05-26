use crate::parser::{Expr, Literal, Infix};

pub fn gen(expr: &Expr, asm: &mut String) {
    fn gen(expr: &Expr, asm: &mut String) {
        if let Expr::Literal(Literal::Number(num)) = expr {
            asm.push_str(&format!("  push {}\n", num));
            return;
        }

        if let Expr::Infix(kind, lhs, rhs) = expr {
            gen(lhs, asm);
            gen(rhs, asm);

            asm.push_str("  pop rdi\n");
            asm.push_str("  pop rax\n");

            asm.push_str(match kind {
                Infix::Add => "  add rax, rdi\n",
                Infix::Sub => "  sub rax, rdi\n",
                Infix::Mul => "  imul rdi\n",
                Infix::Div => "  cqo\n  idiv rdi\n",
            });

            asm.push_str("  push rax\n");
        }
    }

    asm.push_str(".intel_syntax noprefix\n");
    asm.push_str(".global main\n");
    asm.push_str("main:\n");

    gen(expr, asm);

    asm.push_str("  pop rax\n");
    asm.push_str("  ret\n");
}
