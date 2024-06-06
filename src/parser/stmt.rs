use std::fmt::Display;

use super::expr::Expr;
use super::Node;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(String, Expr),
    If(Expr, Box<Stmt>),
    Block(Vec<Node>),
}

impl Stmt {
    pub fn if_stmt(cond: Expr, body: Stmt) -> Stmt {
        Stmt::If(cond, Box::new(body))
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::VarDecl(name, val) => write!(f, "var {name}_0 = {val}"),
            Stmt::If(cond, body) => {
                write!(f, "if {cond}:\n{body}")
            }
            Stmt::Block(nodes) => {
                write!(
                    f,
                    "{}",
                    nodes.iter().map(|n| format!("\t{n}\n")).collect::<String>()
                )
            }
        }
    }
}
