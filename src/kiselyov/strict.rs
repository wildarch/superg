use super::BExpr;
/// The strict compilation algorithm, as given in fig.6
use crate::compiled_expr::{cap, Comb, CompiledExpr};

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

pub fn compile_strict(e: &BExpr) -> CompiledExpr {
    match e {
        BExpr::Var(i) => {
            let n = infer_n(e);
            if n == 1 {
                CompiledExpr::Comb(Comb::I)
            } else {
                let comp_inner = compile_strict(&BExpr::Var(i - 1));
                semantic(0, CompiledExpr::Comb(Comb::K), n - 1, comp_inner)
            }
        }
        BExpr::Lam(e) => {
            let n = infer_n(e);
            if n == 0 {
                CompiledExpr::Ap(
                    Box::new(CompiledExpr::Comb(Comb::K)),
                    Box::new(compile_strict(e)),
                )
            } else {
                compile_strict(e)
            }
        }
        BExpr::Ap(e1, e2) => semantic(
            infer_n(e1),
            compile_strict(e1),
            infer_n(e2),
            compile_strict(e2),
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
                semantic(0, CompiledExpr::Comb(Comb::from(*o)), nl, compile_strict(l)),
                infer_n(r),
                compile_strict(r),
            )
        }
        BExpr::Not(_) => todo!(),
    }
}

fn semantic(n1: usize, e1: CompiledExpr, n2: usize, e2: CompiledExpr) -> CompiledExpr {
    match (n1, n2) {
        (0, 0) => cap(e1, e2),
        (0, n2) => semantic(0, cap(Comb::B, e1), n2 - 1, e2),
        (n1, 0) => semantic(0, cap(cap(Comb::C, Comb::C), e2), n1 - 1, e1),
        (n1, n2) => semantic(
            n1 - 1,
            semantic(0, CompiledExpr::Comb(Comb::S), n1 - 1, e1),
            n2 - 1,
            e2,
        ),
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
        assert_compiles_to("lam (x) (lam (y) x)", "BKI");
        assert_compiles_to("lam (x) (lam (y) (x y))", "CCI (BS(BKI))");
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(SI)(BKI)");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z x)))", "B(B(SI)) (B(BK)(BKI))");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) ((lam (w) w) x)))",
            "B(B(BI)) (B(BK)(BKI))",
        );
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) ((x z) (y z))))",
            "CC(CCI(BS(BKI))) (BS(B(BS)(B(CCI)(B(BS) (B(BK)(BKI))))))",
        );
    }

    #[test]
    fn kiselyov_worsecase() {
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(SI)(BKI)");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (z y x)))",
            "B(S(BS (B(SI)(BKI)))) (B(BK)(BKI))",
        );
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (lam (a) (a z y x))))",
            "B(S(BS(B(BS) (B(S(BS(B(SI)(BKI)))) (B(BK)(BKI)))))) (B(B(BK)) (B(BK)(BKI)))",
        );
    }

    fn assert_compiles_to(expr: &str, compiled_expr: &str) {
        // Compile expr
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let bexpr = to_debruijn(&parsed_expr, &mut vec![]);
        let actual_compiled_expr = compile_strict(&bexpr);
        // Parse expected expr
        let expected_compiled_expr = parse_compiled_expr(lex(compiled_expr));

        assert_eq!(actual_compiled_expr, expected_compiled_expr);
    }
}
