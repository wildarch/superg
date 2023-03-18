/// The lazy compilation algorithm, as given in fig.8
use crate::compiled_expr::{cap, Comb, CompiledExpr};

use super::BExpr;

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContextElem {
    Ignored,
    Used,
}
type Context = Vec<ContextElem>;

fn infer_context(e: &BExpr) -> Context {
    use ContextElem::*;
    match e {
        BExpr::Var(i) => {
            let mut v = vec![Used];
            for _ in 0..*i {
                v.push(Ignored);
            }
            v
        }
        BExpr::Lam(e) => {
            let mut ctx = infer_context(e);
            ctx.pop();
            ctx
        }
        BExpr::Ap(f, a) => unify_contexts(infer_context(f), infer_context(a)),
        BExpr::BinOp(l, _, r) => unify_contexts(infer_context(l), infer_context(r)),
        BExpr::SVar(_) | BExpr::Int(_) => Context::new(),
        BExpr::Not(e) => infer_context(e),
    }
}

fn unify_contexts(mut a: Context, mut b: Context) -> Context {
    use ContextElem::*;
    let mut ctx = Context::new();
    loop {
        let e = match (a.pop(), b.pop()) {
            (Some(Used), _) | (_, Some(Used)) => Used,
            (None, Some(Ignored)) | (Some(Ignored), None) | (Some(Ignored), Some(Ignored)) => {
                Ignored
            }
            (None, None) => {
                // We are building the context in reverse order
                ctx.reverse();
                return ctx;
            }
        };
        ctx.push(e);
    }
}

pub fn compile_lazy(e: &BExpr) -> CompiledExpr {
    use ContextElem::*;
    match e {
        BExpr::Var(_) => {
            // Lazy weakening allows us to just reduce to I in all cases.
            // K is inserted in Abs1 case for Lam below.
            CompiledExpr::Comb(Comb::I)
        }
        BExpr::Lam(e) => {
            // Context for the inner expression
            let mut ctx = infer_context(e);
            match ctx.last() {
                // Abs0
                None => cap(Comb::K, compile_lazy(e)),
                // Abs1
                Some(Ignored) => {
                    ctx.pop().unwrap();
                    semantic(Context::new(), Comb::K, ctx, compile_lazy(e))
                }
                // Abs
                Some(Used) => compile_lazy(e),
            }
        }
        BExpr::Ap(e1, e2) => semantic(
            infer_context(e1),
            compile_lazy(e1),
            infer_context(e2),
            compile_lazy(e2),
        ),
        BExpr::Int(i) => CompiledExpr::Int(*i),
        BExpr::SVar(s) => match s.as_str() {
            "if" => CompiledExpr::Comb(Comb::Cond),
            _ => CompiledExpr::Var(s.clone()),
        },
        BExpr::BinOp(l, o, r) => {
            let cl = infer_context(l);
            // l <op> r => ((<op> l) r)
            semantic(
                cl.clone(),
                semantic(
                    Context::new(),
                    CompiledExpr::Comb(Comb::from(*o)),
                    cl,
                    compile_lazy(l),
                ),
                infer_context(r),
                compile_lazy(r),
            )
        }
        BExpr::Not(_) => todo!(),
    }
}

fn semantic<E1: Into<CompiledExpr>, E2: Into<CompiledExpr>>(
    mut c1: Context,
    e1: E1,
    mut c2: Context,
    e2: E2,
) -> CompiledExpr {
    use ContextElem::*;
    let e1 = e1.into();
    let e2 = e2.into();

    match (c1.pop(), c2.pop()) {
        (None, None) => cap(e1, e2),
        (None, Some(Used)) => semantic(Context::new(), cap(Comb::B, e1), c2, e2),
        (Some(Used), None) => semantic(Context::new(), cap(cap(Comb::C, Comb::C), e2), c1, e1),
        (Some(Used), Some(Used)) => semantic(
            c1.clone(),
            semantic(Context::new(), Comb::S, c1, e1),
            c2,
            e2,
        ),
        // From fig.8.
        (Some(Ignored), Some(Ignored)) => semantic(c1, e1, c2, e2),
        (Some(Ignored), Some(Used)) => semantic(
            // Need to check, maybe used Context::new() instead
            c1.clone(),
            semantic(Context::new(), Comb::B, c1, e1),
            c2,
            e2,
        ),
        (Some(Used), Some(Ignored)) => semantic(
            // Need to check, maybe used Context::new() instead
            c1.clone(),
            semantic(Context::new(), Comb::C, c1, e1),
            c2,
            e2,
        ),
        // Not clearly described in the paper, but necessary for matching results.
        // This is correct, because we can model a shorter context as one with implicit 'unused' elements.
        (None, Some(Ignored)) => semantic(c1, e1, c2, e2),
        (Some(Ignored), None) => semantic(c1, e1, c2, e2),
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
        assert_compiles_to("lam (x) (lam (y) (x y))", "CCI(BBI)");
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(CI)I");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z x)))", "BK(B(CI)I)");
        assert_compiles_to("lam (x) (lam (y) (lam (z) ((lam (w) w) x)))", "BK(BK(BII))");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) ((x z) (y z))))",
            "CC(CCI(BBI)) (BB(BS (CCI(BBI))))",
        );
    }

    #[test]
    fn kiselyov_worsecase() {
        assert_compiles_to("lam (x) (lam (y) (y x))", "B(CI)I");
        assert_compiles_to("lam (x) (lam (y) (lam (z) (z y x)))", "B(C(BC (B(CI)I)))I");
        assert_compiles_to(
            "lam (x) (lam (y) (lam (z) (lam (a) (a z y x))))",
            "B(C(BC(B(BC) (B(C(BC (B(CI)I)))I))))I",
        );
    }

    fn assert_compiles_to(expr: &str, compiled_expr: &str) {
        // Compile expr
        let mut tokens = lex(expr);
        let parsed_expr = parse_expr(&mut tokens);
        let bexpr = to_debruijn(&parsed_expr, &mut vec![]);
        let actual_compiled_expr = compile_lazy(&bexpr);
        // Parse expected expr
        let expected_compiled_expr = parse_compiled_expr(lex(compiled_expr));

        assert_eq!(actual_compiled_expr, expected_compiled_expr);
    }
}
