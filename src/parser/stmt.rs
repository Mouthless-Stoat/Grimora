use std::fmt::Display;

use super::expr::Expr;
use super::Node;
use crate::trans::TABCHAR;

#[derive(Debug, PartialEq)]
pub enum EventIden {
    This,
    Other,
    Any,
}

#[derive(Debug, PartialEq)]
pub enum EventType {
    Die,
    Hit,
    Summon,
    Move,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(String, Expr),
    If(Expr, Box<Stmt>),
    Block(Vec<Node>),
    Event(EventIden, EventType, Option<Expr>, Box<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::VarDecl(name, val) => write!(f, "var {name}_0 = {val}"),
            Stmt::If(cond, body) => {
                write!(f, "if {cond}:\n{body}",)
            }
            Stmt::Block(nodes) => write!(
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
            ),
            Stmt::Event(iden, event, cond, body) => write!(
                f,
                "if event == {event:?}{what}{cond}:\n{body}",
                event = match event {
                    EventType::Die => "card_perished",
                    EventType::Hit => "card_hit",
                    EventType::Summon => "card_summoned",
                    EventType::Move => "card_moved",
                },
                what = match iden {
                    EventIden::This => " and params[0] == card",
                    EventIden::Other => " and params[0] != card",
                    EventIden::Any => "",
                },
                cond = match cond {
                    Some(expr) => format!(" and {expr}"),
                    None => "".to_string(),
                }
            ),
        }
    }
}
