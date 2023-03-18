use superg::{
    ast::Expr,
    bracket::BracketCompiler,
    compiled_expr::{CompiledExpr, ExprCompiler},
    kiselyov::{LazyCompiler, LazyOptCompiler, LinearCompiler, StrictCompiler},
    lex, parse,
};

const ACK: &str = r#"
(defun ack (x z) (if (= x 0)
                     (+ z 1)
                     (if (= z 0)
                         (ack (- x 1) 1)
                         (ack (- x 1) (ack x (- z 1))))))
    "#;

fn main() {
    measure_compiled_size("Bracket", BracketCompiler);
    measure_compiled_size("$\\strict$", StrictCompiler);
    measure_compiled_size("$\\lazy$", LazyCompiler);
    measure_compiled_size("$\\lazyeta$", LazyOptCompiler);
    measure_compiled_size("$\\linear$", LinearCompiler);
}

fn measure_compiled_size<C: ExprCompiler>(name: &str, mut c: C) {
    let compiled = c.compile(&ack_expr());
    let size = compiled_expr_size(&compiled);
    println!("({size},{name})");
}

fn ack_expr() -> Expr {
    let program = parse(lex(ACK));
    program.defs[0].as_lam()
}

fn compiled_expr_size(e: &CompiledExpr) -> usize {
    match e {
        CompiledExpr::Comb(_) => 1,
        CompiledExpr::Ap(a, b) => 1 + compiled_expr_size(a) + compiled_expr_size(b),
        CompiledExpr::Var(_) => 1,
        CompiledExpr::Int(_) => 1,
    }
}
