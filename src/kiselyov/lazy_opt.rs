/// The lazy compilation algorithm, as given in fig.8
use crate::compiled_expr::{cap, Comb, CompiledExpr};

use super::BExpr;

pub fn compile_lazy_opt(e: &BExpr) -> CompiledExpr {
    match compile(e) {
        CExpr::Closed(e) => e,
        _ => unreachable!("expect to reach a closed expression"),
    }
}

// Internal type to store context together with a compiled expression.
#[derive(Debug, Clone)]
enum CExpr {
    Closed(CompiledExpr),
    Value,
    Next(Box<CExpr>),
    Weak(Box<CExpr>),
}

fn compile(e: &BExpr) -> CExpr {
    match e {
        BExpr::Var(i) => {
            let mut e = CExpr::Value;
            for _ in 0..*i {
                e = CExpr::Weak(Box::new(e));
            }
            e
        }
        BExpr::Lam(e) => match compile(e) {
            CExpr::Closed(d) => CExpr::Closed(cap(Comb::K, d)),
            CExpr::Value => CExpr::Closed(CompiledExpr::Comb(Comb::I)),
            CExpr::Next(e) => *e,
            CExpr::Weak(e) => semantic(CExpr::Closed(CompiledExpr::Comb(Comb::K)), *e),
        },
        BExpr::Ap(a, b) => semantic(compile(a), compile(b)),
        BExpr::Int(i) => CExpr::Closed(CompiledExpr::Int(*i)),
        BExpr::SVar(s) => CExpr::Closed(match s.as_str() {
            "if" => CompiledExpr::Comb(Comb::Cond),
            _ => CompiledExpr::Var(s.clone()),
        }),
        BExpr::BinOp(l, o, r) => semantic(
            semantic(
                CExpr::Closed(CompiledExpr::Comb(Comb::from(*o))),
                compile(l),
            ),
            compile(r),
        ),
        BExpr::Not(_) => todo!(),
    }
}

fn semantic(e1: CExpr, e2: CExpr) -> CExpr {
    use CExpr::*;
    use Comb::*;
    fn weak<C: Into<Box<CExpr>>>(e: C) -> CExpr {
        Weak(e.into())
    }
    fn next<C: Into<Box<CExpr>>>(e: C) -> CExpr {
        Next(e.into())
    }
    fn closed<C: Into<CompiledExpr>>(e: C) -> CExpr {
        Closed(e.into())
    }
    fn semantic3(e1: CExpr, e2: CExpr, e3: CExpr) -> CExpr {
        semantic(semantic(e1, e2), e3)
    }
    match (e1, e2) {
        (Weak(e1), Weak(e2)) => weak(semantic(*e1, *e2)),
        (Weak(e), Closed(d)) => weak(semantic(*e, closed(d))),
        (Closed(d), Weak(e)) => weak(semantic(closed(d), *e)),
        (Weak(e), Value) => next(e),
        (Value, Weak(e)) => next(semantic(closed(cap(C, I)), *e)),
        (Weak(e1), Next(e2)) => next(semantic3(closed(B), *e1, *e2)),
        (Next(e1), Weak(e2)) => next(semantic3(closed(C), *e1, *e2)),
        (Next(e1), Next(e2)) => next(semantic3(closed(S), *e1, *e2)),
        (Next(e), Value) => next(semantic3(closed(S), *e, closed(I))),
        (Value, Next(e)) => next(semantic(closed(cap(S, I)), *e)),
        (Closed(d), Next(e)) => next(semantic(closed(cap(B, d)), *e)),
        (Closed(d), Value) => next(closed(d)),
        (Value, Closed(d)) => next(closed(cap(cap(C, I), d))),
        (Value, Value) => unreachable!("can't happen for simple types"),
        (Next(e), Closed(d)) => next(semantic(closed(cap(cap(C, C), d)), *e)),
        (Closed(d1), Closed(d2)) => closed(cap(d1, d2)),
    }
}

#[cfg(test)]
mod tests {
    use super::super::to_debruijn;
    use super::*;
    use crate::{
        lex,
        parser::{parse_compiled_expr, parse_expr},
    };

    // Testcases taken from Kiselyov's paper, Table 1.
    #[test]
    fn kiselyov_examples() {
        assert_compiles_to("lam (x) (lam (y) y)", "KI");
        assert_compiles_to("lam (x) (lam (y) x)", "K");
        assert_compiles_to("lam (x) (lam (y) (x y))", "I");
        assert_compiles_to("lam (x) (lam (y) (y x))", "CI");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z x)))", "BK(CI)");
        // Note: BK(BKI) in Kiselyov
        assert_compiles_to("lam (x) (lam (y) (lam (z) ((lam (w) w) x)))", "BK(BKI)");
        assert_compiles_to("lam (x) (lam (y) (lam (z) ((x z) (y z))))", "S");
    }

    #[test]
    fn kiselyov_worsecase() {
        assert_compiles_to("lam (x) (lam (y) (y x))", "CI");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z y x)))", "C(BC(CI))");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (lam (a) (a z y x))))",
            "C(BC(B(BC) (C(BC(CI)))))",
        );
    }

    fn assert_compiles_to(expr: &str, compiled_expr: &str) {
        // Compile expr
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let bexpr = to_debruijn(&parsed_expr, &mut vec![]);
        let actual_compiled_expr = compile_lazy_opt(&bexpr);
        // Parse expected expr
        let expected_compiled_expr = parse_compiled_expr(lex(compiled_expr));

        assert_eq!(actual_compiled_expr, expected_compiled_expr);
    }
}
