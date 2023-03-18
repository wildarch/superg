use superg::bracket::BracketCompiler;
use superg::compiled_expr::ExprCompiler;
use superg::kiselyov::{LazyCompiler, LazyOptCompiler, LinearCompiler, StrictCompiler};
use superg::lexer::lex;
use superg::parser::parse;
use superg::tigre::TigreEngine;
use superg::turner::TurnerEngine;
use superg::Engine;

// Our set of test programs should run with all different combinators of engines and compilers.
// Given a set of test cases (name, program, expected output), this generates unique tests for every possible combination.
macro_rules! test_suite {
    // entrypoint to the macro.
    ($( ($test_name:ident, $test_program:literal, $expected_output:literal) ),+) => {
        mod turner {
            use super::*;
            test_suite!(TurnerEngine, $( ($test_name, $test_program, $expected_output) ),+);
        }
        mod tigre {
            use super::*;
            test_suite!(TigreEngine, $( ($test_name, $test_program, $expected_output) ),+);
        }
    };

    // with engine but no compiler.
    ($engine:ty, $( ($test_name:ident, $test_program:literal, $expected_output:literal) ),+) => {
        mod bracket {
            use super::*;
            test_suite!($engine, BracketCompiler, $( ($test_name, $test_program, $expected_output) ),+);
        }
        mod strict {
            use super::*;
            test_suite!($engine, StrictCompiler, $( ($test_name, $test_program, $expected_output) ),+);
        }
        mod lazy {
            use super::*;
            test_suite!($engine, LazyCompiler, $( ($test_name, $test_program, $expected_output) ),+);
        }
        mod lazy_opt {
            use super::*;
            test_suite!($engine, LazyOptCompiler, $( ($test_name, $test_program, $expected_output) ),+);
        }
        mod linear {
            use super::*;
            test_suite!($engine, LinearCompiler, $( ($test_name, $test_program, $expected_output) ),+);
        }
    };

    // with engine and compiler fixed, we can generate a unique test.
    ($engine:ty, $compiler:expr, $( ($test_name:ident, $test_program:literal, $expected_output:literal) ),+) => {
        $(
            #[test]
            fn $test_name() {
                assert_runs_to_int_gen::<_, $engine>($compiler, $test_program, $expected_output);
            }
        )+
    };
}

test_suite!(
    (test_lit, "(defun main () 42)", 42),
    (
        test_id,
        r#"
(defun id (x) x)
(defun main () (id 42))
        "#,
        42
    ),
    (
        test_k,
        r#"
(defun k (x y) x)
(defun main () (k 42 84))
        "#,
        42
    ),
    (
        test_s,
        r#"
(defun s (f g x) (f x (g x)))
(defun k (x y) x)
(defun main () (s k k 42))
        "#,
        42
    ),
    (
        test_b,
        r#"
(defun b (f g x) (f (g x)))
(defun k (x y) x)
(defun i (x) x)
(defun main () (b i i 42))
    "#,
        42
    ),
    (
        test_add,
        r#"
(defun main () (+ 2 40))
        "#,
        42
    ),
    (
        test_add_indirect,
        r#"
(defun id (x) x)
(defun main () (+ (id 2) (id 40)))
        "#,
        42
    ),
    (
        test_cond,
        r#"
(defun main () (+ 2 (if 0 30 40)))
        "#,
        42
    ),
    (
        test_cond_add,
        r#"
(defun main () (+ 2 (if 0 30 40)))
        "#,
        42
    ),
    (
        test_eq,
        r#"
    (defun id (x) x)
    (defun k (x y) x)
    (defun main () (= (k 1 1000) 0))
            "#,
        0
    ),
    (
        test_cond_eq,
        r#"
    (defun main () (if (= 2 2) 42 43))
            "#,
        42
    ),
    (
        test_factorial,
        r#"
    (defun fac (n)
      (if (= n 1)
          1
          (* n (fac (- n 1)))))
    (defun main () (fac 5))
            "#,
        120
    ),
    (
        test_fibonacci,
        r#"
    (defun fib (n)
      (if (< n 2) 
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (defun main () (fib 5))
            "#,
        5
    ),
    (
        test_ackermann,
        r#"
(defun ack (x z) (if (= x 0)
                     (+ z 1)
                     (if (= z 0)
                         (ack (- x 1) 1)
                         (ack (- x 1) (ack x (- z 1))))))
(defun main () (ack 3 4))
    "#,
        125
    )
);

fn assert_runs_to_int_gen<C: ExprCompiler, E: Engine>(mut compiler: C, program: &str, v: i32) {
    let parsed = parse(lex(program));
    let mut engine = E::compile(&mut compiler, &parsed);
    let res = engine.run();
    assert_eq!(res, v);
}
