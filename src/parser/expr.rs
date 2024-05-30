use crate::lexer::Token;
use crate::trans::Transpilable;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(usize),
    Iden(String),
    Bin(Box<Expr>, Token, Box<Expr>),
}

impl Transpilable for Expr {
    fn transpile(self) -> String {
        match self {
            Expr::Int(int) => int.to_string(),
            Expr::Iden(iden) => iden,
            Expr::Bin(left, op, right) => format!(
                "{} {} {}",
                left.transpile(),
                op.to_string(),
                right.transpile()
            ),
        }
    }
}

impl Expr {
    pub fn bin(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }
}
