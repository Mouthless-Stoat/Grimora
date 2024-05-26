use std::collections::VecDeque;

use super::lexer::Token;

pub trait Transpilable {
    fn transpile(&self) -> String;
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(usize),
    Iden(String),
    Bin(Box<Expr>, Token, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(),
}

pub struct Parser {
    pub tokens: VecDeque<Token>,
}

impl Parser {
    pub fn gen_ast(&mut self) -> Vec<Node> {
        let mut ast: Vec<Node> = Vec::new();

        while !self.tokens.is_empty() {
            ast.push(self.parse_stmt())
        }

        return ast;
    }

    fn peek(&self) -> &Token {
        self.tokens.get(0).unwrap()
    }

    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap()
    }

    fn parse_stmt(&mut self) -> Node {
        match self.peek() {
            _ => self.parse_unit(),
        }
    }

    fn parse_unit(&mut self) -> Node {
        match self.next() {
            Token::Int(int) => Node::Expr(Expr::Int(int)),
            Token::Iden(iden) => Node::Expr(Expr::Iden(iden)),
            _ => panic!("Wahoo you encounter a bug :D"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::tokenize;
    use crate::parser::{Expr, Node, Parser};

    macro_rules! test {
        ($name:ident, $source:literal => $output:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    (Parser {
                        tokens: tokenize($source.to_string()).unwrap()
                    })
                    .gen_ast(),
                    $output
                )
            }
        };
    }

    test!(simple, "1" => [Node::Expr(Expr::Int(1))]);
}
