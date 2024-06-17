use std::fmt::Display;

use super::expr::Expr;
use super::Node;
use crate::trans::TABCHAR;

#[derive(Debug, PartialEq, Clone)]
pub enum EventIden {
    This,
    Other,
    Any,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EventType {
    Die,
    Hit,
    Summon,
    Move,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Block(Vec<Node>),

    VarDecl(String, Expr),
    Assign(Expr, Expr),

    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Event(EventIden, EventType, Option<Expr>, Box<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            Stmt::VarDecl(name, val) => write!(f, "var {name} = {val}"),
            Stmt::Assign(iden, value) => write!(f, "{iden} = {value}"),
            Stmt::If(cond, body, other) => {
                write!(
                    f,
                    "if {cond}:\n{body}{}",
                    match other {
                        Some(block) => match **block {
                            Stmt::If(_, _, _) => format!("el{block}"),
                            Stmt::Block(_) => format!("else:\n{block}"),
                            _ => unreachable!(),
                        },
                        None => "".to_string(),
                    }
                )
            }
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
