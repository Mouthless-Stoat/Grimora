use std::fmt::Display;

use super::expr::Expr;
use super::Node;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(String, Box<Expr>),
    If(Expr, Box<Stmt>),
    Body(Vec<Node>),
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
            Stmt::If(cond, body) => write!(f, "if {cond}:\n{body}"),
            Stmt::Body(nodes) => write!(
                f,
                "{}",
                nodes
                    .iter()
                    .map(|n| format!("\t{}\n", n.to_string()))
                    .collect::<String>()
                    .trim()
            ),
        }
    }
}
