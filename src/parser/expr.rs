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
            Expr::Int(_) => todo!(),
            Expr::Iden(_) => todo!(),
            Expr::Bin(_, _, _) => todo!(),
        }
    }
}

impl Expr {
    pub fn bin(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }
}
