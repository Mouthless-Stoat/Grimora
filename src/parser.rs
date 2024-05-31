mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use std::fmt::Display;
use stmt::Stmt;

use crate::lexer::Token;

#[derive(PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr(expr) => write!(f, "{}", expr),
            Node::Stmt(stmt) => write!(f, "{}", stmt),
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

    fn curr(&self) -> &Token {
        self.tokens.get(0).unwrap()
    }

    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap()
    }

    fn parse_stmt(&mut self) -> Node {
        match self.curr() {
            _ => self.parse_expr(),
        }
    }

    fn parse_expr(&mut self) -> Node {
        Node::Expr(self.parse_add_bin())
    }

    /// Parse a binary expression
    fn parse_add_bin(&mut self) -> Expr {
        let mut left = self.parse_mul_bin();
        while matches!(self.curr(), Token::Plus | Token::Minus) {
            let op = self.next();
            let right = self.parse_mul_bin();

            // constant collapsing time
            if let (Expr::Int(l), Expr::Int(r)) = (&left, &right) {
                left = Expr::Int(match op {
                    Token::Plus => l + r,
                    Token::Minus => l - r,
                    _ => unreachable!(),
                })
            } else {
                left = Expr::bin(left, op, right);
            };
        }
        return left;
    }

    fn parse_mul_bin(&mut self) -> Expr {
        let mut left = self.parse_unit();
        while matches!(self.curr(), Token::Star | Token::Slash) {
            let op = self.next();
            let right = self.parse_unit();

            if let (Expr::Int(l), Expr::Int(r)) = (&left, &right) {
                left = Expr::Int(match op {
                    Token::Star => l * r,
                    Token::Slash => l / r,
                    _ => unreachable!(),
                })
            } else {
                left = Expr::bin(left, op, right);
            };
        }
        return left;
    }

    /// Parse a unit or literal
    fn parse_unit(&mut self) -> Expr {
        match self.next() {
            Token::Int(it) => Expr::Int(it),
            Token::Iden(it) => Expr::Iden(it),
            _ => todo!("Return a parser error instead"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{tokenize, Token};
    use crate::parser::{Expr, Node, Parser};

    // Helper to make typing ast less annoying
    macro_rules! ast {
        ($($node:expr),*) => {
            {
                let mut t = Vec::new();
                $(
                    t.push(Node::Expr($node));
                )*
                t
            }
        };
    }

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

    test!(simple, "1" => ast![Expr::Int(1)]);
    test!(bin, "1 + 1" => ast![Expr::bin(Expr::Int(1), Token::Plus, Expr::Int(1))]);
}
