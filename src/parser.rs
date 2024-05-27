mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use stmt::Stmt;

use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
}

impl Node {
    fn unwrap_expr(self) -> Expr {
        match self {
            Node::Expr(it) => it,
            _ => unreachable!(),
        }
    }
    fn unwrap_stmt(self) -> Stmt {
        match self {
            Node::Stmt(it) => it,
            _ => unreachable!(),
        }
    }
}

pub struct Parser {
    pub tokens: VecDeque<Token>,
}

impl Parser {
    pub fn gen_ast(&mut self) -> Vec<Node> {
        let mut ast: Vec<Node> = Vec::new();

        while !self.tokens.is_empty() && !matches!(self.curr(), Token::EOF) {
            ast.push(self.parse_stmt())
        }

        return ast;
    }

    fn peek(&self) -> &Token {
        self.tokens.get(1).unwrap()
    }

    fn curr(&self) -> &Token {
        self.tokens.get(0).unwrap()
    }

    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap()
    }

    fn parse_stmt(&mut self) -> Node {
        match self.curr() {
            _ => Node::Expr(self.parse_bin()),
        }
    }

    /// Parse a binary expression
    fn parse_bin(&mut self) -> Expr {
        let mut left = self.parse_unit();
        while matches!(
            self.curr(),
            Token::Plus | Token::Minus | Token::Star | Token::Slash
        ) {
            let op = self.next();
            let right = self.parse_unit();
            left = Expr::bin(left, op, right);
        }
        return left;
    }

    /// Parse a unit or literal
    fn parse_unit(&mut self) -> Expr {
        match self.next() {
            Token::Int(it) => Expr::Int(it),
            Token::Iden(it) => Expr::Iden(it),
            _ => panic!("Wahoo you encounter a bug :D"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{tokenize, Token};
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
    test!(bin, "1 + 1" => [Node::Expr(Expr::bin(Expr::Int(1), Token::Plus, Expr::Int(1)))]);
}
