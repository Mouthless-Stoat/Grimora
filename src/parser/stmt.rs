use std::fmt::Display;

use super::expr::Expr;
use super::Node;
use crate::trans::TABCHAR;

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(String, Expr),
    If(Expr, Box<Stmt>),
    Block(Vec<Node>),
}

impl Stmt {
    pub fn if_stmt(cond: Expr, body: Stmt) -> Self {
        Stmt::If(cond, Box::new(body))
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::VarDecl(name, val) => write!(f, "var {name}_0 = {val}"),
            Stmt::If(cond, body) => {
                write!(f, "if {cond}:\n{body}",)
            }
            Stmt::Block(nodes) => {
                write!(
                    f,
                    "{}",
                    nodes
                        .iter()
                        .map(|n| n
                            .to_string()
                            .lines()
                            .map(|l| format!("{TABCHAR}{l}\n"))
                            .collect::<String>())
                        .collect::<String>()
                )
            }
        }
    }
}
