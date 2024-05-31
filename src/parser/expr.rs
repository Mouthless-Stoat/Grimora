use std::fmt::Display;

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(usize),
    Iden(String),
    Bin(Box<Expr>, Token, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(int) => write!(f, "{}", int),
            Expr::Iden(iden) => write!(f, "{}", iden),
            Expr::Bin(left, op, right) => write!(
                f,
                "{} {} {}",
                left.to_string(),
                op.to_string(),
                right.to_string()
            ),
        }
    }
}

impl Expr {
    pub fn bin(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }
}
