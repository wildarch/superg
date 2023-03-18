use criterion::measurement::WallTime;
use criterion::{criterion_group, criterion_main, Bencher, BenchmarkGroup, BenchmarkId, Criterion};
use superg::compiled_expr::ExprCompiler;
use superg::kiselyov::{LazyCompiler, LazyOptCompiler, LinearCompiler, StrictCompiler};
use superg::tigre::TigreEngine;

use std::fmt::Display;
use std::io::Write;
use std::time::{Duration, Instant};
use superg::bracket::BracketCompiler;
use superg::turner::TurnerEngine;
use superg::{lex, parse, Engine};

mod miranda;

criterion_group!(turner_vs_miranda, bench_fib_turner_vs_miranda);
criterion_group!(turner_vs_tigre, bench_fib_turner_vs_tigre);
criterion_group!(compilers, bench_compilers_fib, bench_compilers_ack);
criterion_main!(turner_vs_miranda, turner_vs_tigre, compilers);

fn fib_program(n: u16) -> String {
    format!(
        r#"
    (defun fib (n)
      (if (< n 2) 
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (defun main () (fib {n}))
    "#
    )
}

fn fib(n: u16) -> i32 {
    if n < 2 {
        n as i32
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn ack_program(m: u16, n: u16) -> String {
    format!(
        r#"
(defun ack (x z) (if (= x 0)
                     (+ z 1)
                     (if (= z 0)
                         (ack (- x 1) 1)
                         (ack (- x 1) (ack x (- z 1))))))
(defun main () (ack {m} {n}))
    "#
    )
}

fn ack(m: i32, n: i32) -> i32 {
    match (m, n) {
        (0, n) => n + 1,
        (m, 0) => ack(m - 1, 1),
        (m, n) => ack(m - 1, ack(m, n - 1)),
    }
}

fn bench_instance<C: ExprCompiler, E: Engine>(
    b: &mut Bencher,
    program: &str,
    expected_res: i32,
    mut compiler: C,
) {
    let parsed = parse(lex(program));
    b.iter_custom(|iters| {
        let mut elapsed = Duration::ZERO;
        for _ in 0..iters {
            let mut engine = E::compile(&mut compiler, &parsed);
            // Measure only the time spent reducing the graph
            let start = Instant::now();
            let res = engine.run();
            elapsed += start.elapsed();
            assert_eq!(res, expected_res);
        }
        elapsed
    })
}

fn bench_fib_turner_vs_miranda(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci (Turner vs. Miranda)");
    for n in (10..=20).step_by(5) {
        let expected_res = fib(n);
        // Turner
        group.bench_with_input(BenchmarkId::new("TurnerEngine (Bracket)", n), &n, |b, n| {
            bench_instance::<_, TurnerEngine>(b, &fib_program(*n), expected_res, BracketCompiler);
        });

        // Miranda
        group.bench_with_input(BenchmarkId::new("Miranda", n), &n, |b, n| {
            // Create the temp file with the module contents
            let mut temp_file = tempfile::Builder::new().suffix(".m").tempfile().unwrap();
            write!(
                temp_file,
                r#"
fib n = n,                              if n < 2
      = fib(n - 1) + fib(n - 2),        otherwise
    "#
            )
            .unwrap();
            temp_file.flush().unwrap();
            b.iter_custom(|iters| {
                let mut elapsed = Duration::ZERO;
                for _ in 0..iters {
                    let stats = miranda::run_miranda(temp_file.path(), &format!("fib {n}"));
                    elapsed += stats.runtime;
                    assert_eq!(stats.result, expected_res);
                }
                elapsed
            });
        });
    }
}

fn bench_fib_turner_vs_tigre(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci (Turner vs. TIGRE)");
    for n in (10..=20).step_by(5) {
        let expected_res = fib(n);
        // Turner
        group.bench_with_input(BenchmarkId::new("TurnerEngine (Bracket)", n), &n, |b, n| {
            bench_instance::<_, TurnerEngine>(b, &fib_program(*n), expected_res, BracketCompiler);
        });
        group.bench_with_input(BenchmarkId::new("TigreEngine (Bracket)", n), &n, |b, n| {
            bench_instance::<_, TigreEngine>(b, &fib_program(*n), expected_res, BracketCompiler);
        });
    }
}

fn bench_compilers_fib(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci n (All compilers)");
    for n in (10..=20).step_by(5) {
        add_all_compilers(&mut group, n, &fib_program(n), fib(n));
    }
}

fn bench_compilers_ack(c: &mut Criterion) {
    let mut group = c.benchmark_group("Ackermann 3 n (All compilers)");
    let m = 3;
    for n in 0..=4 {
        add_all_compilers(&mut group, n, &ack_program(m, n), ack(m as i32, n as i32));
    }
}

fn add_all_compilers<P: Display + Clone + Copy>(
    group: &mut BenchmarkGroup<WallTime>,
    n: P,
    program: &str,
    expected_res: i32,
) {
    // Bracket
    group.bench_function(BenchmarkId::new("Bracket", n), |b| {
        bench_instance::<_, TurnerEngine>(b, program, expected_res, BracketCompiler);
    });
    // Strict
    group.bench_function(BenchmarkId::new("Kiselyov - Strict", n), |b| {
        bench_instance::<_, TurnerEngine>(b, program, expected_res, StrictCompiler);
    });
    // Lazy
    group.bench_function(BenchmarkId::new("Kiselyov - Lazy", n), |b| {
        bench_instance::<_, TurnerEngine>(b, program, expected_res, LazyCompiler);
    });
    // LazyOpt
    group.bench_function(BenchmarkId::new("Kiselyov - LazyOpt", n), |b| {
        bench_instance::<_, TurnerEngine>(b, program, expected_res, LazyOptCompiler);
    });
    // Linear
    group.bench_function(BenchmarkId::new("Kiselyov - Linear", n), |b| {
        bench_instance::<_, TurnerEngine>(b, program, expected_res, LinearCompiler);
    });
}
