/// The linear compilation algorithm, as given in fig.10
use super::BExpr;

use crate::compiled_expr::*;

fn infer_n(e: &BExpr) -> usize {
    match e {
        BExpr::Var(i) => i + 1,
        BExpr::Lam(e) => {
            let n = infer_n(e);
            if n == 0 {
                0
            } else {
                n - 1
            }
        }
        BExpr::Ap(f, a) => {
            let n_f = infer_n(f);
            let n_a = infer_n(a);
            std::cmp::max(n_f, n_a)
        }
        BExpr::Int(_) => 0,
        BExpr::SVar(_) => 0,
        BExpr::BinOp(l, _, r) => std::cmp::max(infer_n(l), infer_n(r)),
        BExpr::Not(e) => infer_n(e),
    }
}

pub fn compile_linear(e: &BExpr) -> CompiledExpr {
    match e {
        BExpr::Var(i) => {
            let n = infer_n(e);
            if n == 1 {
                CompiledExpr::Comb(Comb::I)
            } else {
                let comp_inner = compile_linear(&BExpr::Var(i - 1));
                semantic(0, CompiledExpr::Comb(Comb::K), n - 1, comp_inner)
            }
        }
        BExpr::Lam(e) => {
            let n = infer_n(e);
            if n == 0 {
                CompiledExpr::Ap(
                    Box::new(CompiledExpr::Comb(Comb::K)),
                    Box::new(compile_linear(e)),
                )
            } else {
                compile_linear(e)
            }
        }
        BExpr::Ap(e1, e2) => semantic(
            infer_n(e1),
            compile_linear(e1),
            infer_n(e2),
            compile_linear(e2),
        ),
        BExpr::Int(i) => CompiledExpr::Int(*i),
        BExpr::SVar(s) => match s.as_str() {
            "if" => CompiledExpr::Comb(Comb::Cond),
            _ => CompiledExpr::Var(s.clone()),
        },
        BExpr::BinOp(l, o, r) => {
            let nl = infer_n(l);
            // l <op> r => ((<op> l) r)
            semantic(
                nl,
                semantic(0, CompiledExpr::Comb(Comb::from(*o)), nl, compile_linear(l)),
                infer_n(r),
                compile_linear(r),
            )
        }
        BExpr::Not(_) => todo!(),
    }
}

fn semantic(n1: usize, e1: CompiledExpr, n2: usize, e2: CompiledExpr) -> CompiledExpr {
    #[allow(non_snake_case)]
    fn Sn(i: usize) -> Comb {
        if i == 1 {
            Comb::S
        } else {
            Comb::Sn(i)
        }
    }
    #[allow(non_snake_case)]
    fn Bn(i: usize) -> Comb {
        if i == 1 {
            Comb::B
        } else {
            Comb::Bn(i)
        }
    }
    #[allow(non_snake_case)]
    fn Cn(i: usize) -> Comb {
        if i == 1 {
            Comb::C
        } else {
            Comb::Cn(i)
        }
    }
    match (n1, n2) {
        (0, 0) => cap(e1, e2),
        (0, n) => cap(cap(Bn(n), e1), e2),
        (n, 0) => cap(cap(Cn(n), e1), e2),
        (n, m) => {
            if n == m {
                cap(cap(Sn(n), e1), e2)
            } else if n < m {
                cap(cap(Bn(m - n), cap(Sn(n), e1)), e2)
            } else {
                cap(cap(Cn(n - m), cap(cap(Bn(n - m), Sn(m)), e1)), e2)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::to_debruijn;
    use super::*;
    use crate::lex;
    use crate::parser::{parse_compiled_expr, parse_expr};

    // Testcases taken from Kiselyov's paper, Table 1.
    #[test]
    fn kiselyov_examples() {
        assert_compiles_to("lam (x) (lam (y) y)", "KI");
        assert_compiles_to("lam (x) (lam (y) x)", "BKI");
        // Kiselyov' paper contains an error, it states the output is: C(BS(BKI))I
        assert_compiles_to("lam (x) (lam (y) (x y))", "C(BS(BKI))I");
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(SI)(BKI)");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z x)))", "B2(SI) (B2 K(BKI))");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) ((lam (w) w) x)))",
            "B3 I(B2 K(BKI))",
        );
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) ((x z) (y z))))",
            "C(B S2(C2 (B2 S(B2 K (BKI)))I)) (C(BS(BKI))I)",
        );
    }

    #[test]
    fn kiselyov_worsecase() {
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(SI)(BKI)");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (z y x)))",
            "B(S2 (B(SI)(BKI))) (B2 K(BKI))",
        );
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (lam (a) (a z y x))))",
            "B(S3 (B(S2 (B(SI)(BKI))) (B2 K(BKI)))) (B3 K(B2 K (BKI)))",
        );
    }

    fn assert_compiles_to(expr: &str, compiled_expr: &str) {
        // Compile expr
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let bexpr = to_debruijn(&parsed_expr, &mut vec![]);
        let actual_compiled_expr = compile_linear(&bexpr);
        // Parse expected expr
        let expected_compiled_expr = parse_compiled_expr(lex(compiled_expr));

        assert_eq!(actual_compiled_expr, expected_compiled_expr);
    }
}
