use std::fmt::{Debug, Display};

use crate::lexer::{Iden, Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Num(f32),
    String(String),
    Iden(String),
    Bool(bool),
    Card(String),

    ResIden(Iden),

    Paren(Box<Expr>),

    Un(Token, Box<Expr>),
    Bin(Box<Expr>, Token, Box<Expr>),

    Call(Box<Expr>, Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Attr(Box<Expr>, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Num(int) => write!(f, "{int}"),
            Expr::String(str) => write!(f, "{str:?}"),
            Expr::Iden(iden) => write!(f, "{iden}",),
            Expr::Bool(bool) => write!(f, "{bool}",),
            Expr::Card(name) => write!(f, "CardData.from_name({name:?})"),

            Expr::ResIden(iden) => write!(f, "{iden}"),

            Expr::Paren(expr) => write!(f, "({expr})"),

            Expr::Un(op, expr) => write!(f, "{op}{expr}"),
            Expr::Bin(left, op, right) => write!(f, "{left} {op} {right}",),

            Expr::Call(caller, args) => write!(
                f,
                "{caller}({})",
                args.iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expr::Sub(expr, sub) => write!(f, "{expr}[{sub}]"),
            Expr::Attr(expr, attr) => write!(f, "{expr}.{attr}"),
        }
    }
}
