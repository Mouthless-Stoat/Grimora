use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Stmt {}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
