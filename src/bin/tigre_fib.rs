use superg::{bracket::BracketCompiler, lex, parse, tigre::TigreEngine, Engine};

const FIB_20: &str = r#"
    (defun fib (n)
      (if (< n 2) 
          n
          (+ (fib (- n 1)) (fib (- n 2)))))
    (defun main () (fib 20))
    "#;

fn main() {
    let program = parse(lex(FIB_20));
    let mut engine = TigreEngine::compile(&mut BracketCompiler, &program);
    let res = engine.run();
    println!("fib 20 = {res}");
}
