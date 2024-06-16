use std::fmt::Display;

use crate::lexer::{Iden, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Num(f32),
    String(String),
    Iden(String),
    Card(String),
    Un(Token, Box<Expr>),
    Bin(Box<Expr>, Token, Box<Expr>),
    ReserveIden(Iden),
    Paren(Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Paren(expr) => write!(f, "({expr})"),
            Expr::Num(int) => write!(f, "{int}"),
            Expr::String(str) => write!(f, "{str:?}"),
            Expr::Iden(iden) => write!(f, "{iden}"),
            Expr::Card(name) => write!(f, "CardData.from_name({name:?})"),
            Expr::Bin(left, op, right) => write!(f, "{left} {op} {right}",),
            Expr::ReserveIden(iden) => write!(f, "{iden}"),
            Expr::Un(op, expr) => write!(f, "{op}{expr}"),
        }
    }
}
