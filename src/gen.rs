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
                Infix::Equal => "  cmp rax, rdi\n  sete al\n  movzb  rax, al\n",
                Infix::NotEqual => "  cmp rax, rdi\n  setne al\n  movzb  rax, al\n",
                Infix::LessThan => "  cmp rax, rdi\n  setl al\n  movzb  rax, al\n",
                Infix::LessThanOrEqual => "  cmp rax, rdi\n  setle al\n  movzb  rax, al\n",
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
