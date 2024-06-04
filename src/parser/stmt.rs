use std::fmt::Display;

use super::expr::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(String, Box<Expr>),
}

impl Stmt {
    pub fn var_decl(name: String, value: Expr) -> Stmt {
        Stmt::VarDecl(name, Box::new(value))
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::VarDecl(name, val) => write!(f, "var {}_0 = {}", name, val),
        }
    }
}
