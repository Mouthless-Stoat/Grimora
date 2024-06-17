use std::fmt::Display;

use crate::lexer::{Iden, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(f32),
    String(String),
    Iden(String),
    Bool(bool),
    Card(String),
    Un(Token, Box<Expr>),
    Bin(Box<Expr>, Token, Box<Expr>),
    ReserveIden(Iden),
    Paren(Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Paren(expr) => write!(f, "({expr})"),
            Expr::Num(int) => write!(f, "{int}"),
            Expr::String(str) => write!(f, "{str:?}"),
            Expr::Iden(iden) => write!(f, "{iden}_0"),
            Expr::Bool(bool) => write!(f, "{bool}",),
            Expr::Card(name) => write!(f, "CardData.from_name({name:?})"),
            Expr::Bin(left, op, right) => write!(f, "{left} {op} {right}",),
            Expr::ReserveIden(iden) => write!(f, "{iden}"),
            Expr::Un(op, expr) => write!(f, "{op}{expr}"),
            Expr::Call(caller, args) => write!(
                f,
                "{caller}({})",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
