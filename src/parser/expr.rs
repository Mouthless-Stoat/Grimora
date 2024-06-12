use std::fmt::Display;

use crate::lexer::{Iden, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(f32),
    Iden(String),
    Un(Token, Box<Expr>),
    Bin(Box<Expr>, Token, Box<Expr>),
    ReserveIden(Iden),
    Paren(Box<Expr>),
}

impl Expr {
    pub fn bin(left: Expr, op: Token, right: Expr) -> Self {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Paren(expr) => write!(f, "({expr})"),
            Expr::Num(int) => write!(f, "{int}"),
            Expr::Iden(iden) => write!(f, "{iden}"),
            Expr::Bin(left, op, right) => write!(f, "{left} {op} {right}",),
            Expr::ReserveIden(iden) => write!(f, "{iden}"),
            Expr::Un(op, expr) => write!(f, "{op}{expr}"),
        }
    }
}
