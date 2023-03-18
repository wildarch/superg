use crate::ast;
use crate::compiled_expr::{cap, Comb, CompiledExpr, ExprCompiler};

pub struct BracketCompiler;

impl ExprCompiler for BracketCompiler {
    fn compile(&mut self, expr: &crate::ast::Expr) -> CompiledExpr {
        compile(expr)
    }
}

pub fn compile(e: &ast::Expr) -> CompiledExpr {
    match e {
        ast::Expr::Int(i) => CompiledExpr::Int(*i),
        ast::Expr::Var(s) => match s.as_str() {
            "if" => CompiledExpr::Comb(Comb::Cond),
            s => CompiledExpr::Var(s.to_owned()),
        },
        ast::Expr::BinOp(l, o, r) => {
            let l = compile(l);
            let r = compile(r);
            cap(cap(Comb::from(*o), l), r)
        }
        ast::Expr::Not(e) => cap(Comb::Not, compile(e)),
        ast::Expr::Ap(l, r) => {
            let l = compile(l);
            let r = compile(r);
            cap(l, r)
        }
        ast::Expr::Lam(x, e) => abstract_var(compile(e), x),
    }
}

fn abstract_var(e: CompiledExpr, n: &str) -> CompiledExpr {
    match e {
        CompiledExpr::Comb(c) => cap(Comb::K, c),
        CompiledExpr::Ap(l, r) => cap(cap(Comb::S, abstract_var(*l, n)), abstract_var(*r, n)),
        CompiledExpr::Var(s) => {
            if s == n {
                CompiledExpr::Comb(Comb::I)
            } else {
                cap(CompiledExpr::Comb(Comb::K), CompiledExpr::Var(s))
            }
        }
        i @ CompiledExpr::Int(_) => cap(CompiledExpr::Comb(Comb::K), i),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse_expr;

    #[test]
    fn compile_increment() {
        let expr = "lam (x) (+ x 1)";
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let compiled = compile(&parsed_expr);
        use Comb::{Plus, I, K, S};
        assert_eq!(
            compiled,
            cap(
                cap(S, cap(cap(S, cap(K, Plus)), I)),
                cap(K, CompiledExpr::Int(1))
            )
        );
    }
}
