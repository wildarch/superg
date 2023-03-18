pub mod ast;
pub mod lexer;
pub mod parser;
pub mod turner;

pub mod bracket;
pub mod compiled_expr;
pub mod kiselyov;

pub use lexer::lex;
pub use parser::parse;

pub mod tigre;

pub trait Engine {
    fn compile<C: compiled_expr::ExprCompiler>(compiler: &mut C, program: &ast::Program) -> Self;
    fn run(&mut self) -> i32;
}
