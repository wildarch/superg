use crate::ast::Expr;
use crate::compiled_expr::{CompiledExpr, ExprCompiler};

pub mod lazy;
pub mod lazy_opt;
pub mod linear;
pub mod strict;

pub struct StrictCompiler;

impl ExprCompiler for StrictCompiler {
    fn compile(&mut self, expr: &crate::ast::Expr) -> CompiledExpr {
        strict::compile_strict(&to_debruijn(expr, &mut Vec::new()))
    }
}

pub struct LazyCompiler;

impl ExprCompiler for LazyCompiler {
    fn compile(&mut self, expr: &crate::ast::Expr) -> CompiledExpr {
        lazy::compile_lazy(&to_debruijn(expr, &mut Vec::new()))
    }
}

pub struct LazyOptCompiler;

impl ExprCompiler for LazyOptCompiler {
    fn compile(&mut self, expr: &crate::ast::Expr) -> CompiledExpr {
        lazy_opt::compile_lazy_opt(&to_debruijn(expr, &mut Vec::new()))
    }
}

pub struct LinearCompiler;

impl ExprCompiler for LinearCompiler {
    fn compile(&mut self, expr: &crate::ast::Expr) -> CompiledExpr {
        linear::compile_linear(&to_debruijn(expr, &mut Vec::new()))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Fun(Box<Type>, Box<Type>),
}

// Same as ast::Expr but using de Bruijn indices
#[derive(Debug, PartialEq, Clone)]
pub enum BExpr {
    Int(i32),
    Var(usize),
    SVar(String),
    BinOp(Box<BExpr>, crate::ast::BinOp, Box<BExpr>),
    Not(Box<BExpr>),
    Ap(Box<BExpr>, Box<BExpr>),
    Lam(Box<BExpr>),
}

pub fn to_debruijn(e: &Expr, vars: &mut Vec<String>) -> BExpr {
    match e {
        Expr::Int(i) => BExpr::Int(*i),
        Expr::Var(v) => {
            if let Some(i) = vars.iter().position(|x| x == v) {
                BExpr::Var(vars.len() - 1 - i)
            } else {
                BExpr::SVar(v.clone())
            }
        }
        Expr::BinOp(l, o, r) => BExpr::BinOp(
            Box::new(to_debruijn(l, vars)),
            *o,
            Box::new(to_debruijn(r, vars)),
        ),
        Expr::Not(e) => BExpr::Not(Box::new(to_debruijn(e, vars))),
        Expr::Ap(f, a) => BExpr::Ap(
            Box::new(to_debruijn(f, vars)),
            Box::new(to_debruijn(a, vars)),
        ),
        Expr::Lam(v, e) => {
            vars.push(v.clone());
            let e = to_debruijn(e, vars);
            vars.pop().unwrap();
            BExpr::Lam(Box::new(e))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex, parser::parse_expr};

    use super::BExpr;
    #[test]
    fn test_to_debruijn() {
        assert_de_bruijn_equals("lam (x y) (y x)", lam(lam(ap(z(), s(z())))));
    }

    #[test]
    fn test_to_debruijn_k() {
        assert_de_bruijn_equals("lam (x y) x", lam(lam(s(z()))));
    }

    #[test]
    fn test_to_debruijn_s() {
        assert_de_bruijn_equals(
            "lam (x y z) ((x z) (y z))",
            lam(lam(lam(ap(ap(s(s(z())), z()), ap(s(z()), z()))))),
        );
    }

    fn assert_de_bruijn_equals(expr: &str, expected_bexpr: BExpr) {
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let bexpr = to_debruijn(&parsed_expr, &mut vec![]);
        assert_eq!(bexpr, expected_bexpr);
    }

    fn z() -> BExpr {
        BExpr::Var(0)
    }
    fn s(e: BExpr) -> BExpr {
        if let BExpr::Var(i) = e {
            BExpr::Var(i + 1)
        } else {
            panic!()
        }
    }

    fn lam(e: BExpr) -> BExpr {
        BExpr::Lam(Box::new(e))
    }

    fn ap(f: BExpr, a: BExpr) -> BExpr {
        BExpr::Ap(Box::new(f), Box::new(a))
    }
}
