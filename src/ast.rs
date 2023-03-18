#[derive(Debug, PartialEq)]
pub struct Program {
    pub defs: Vec<Def>,
}

#[derive(Debug, PartialEq)]
pub struct Def {
    pub name: String,
    pub params: Vec<String>,
    pub expr: Expr,
}

impl Def {
    /// Convert this definition into a lambda expression,
    /// discarding the name.
    pub fn as_lam(&self) -> Expr {
        let mut expr = self.expr.clone();
        for arg in self.params.iter().rev() {
            expr = Expr::Lam(arg.clone(), Box::new(expr));
        }
        expr
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Int(i32),
    Var(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Not(Box<Expr>),
    Ap(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    Cons,
    // Arithmetic
    Plus,
    Minus,
    Times,
    Divide,
    // Comparison
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}
