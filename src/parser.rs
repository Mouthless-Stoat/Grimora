mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use std::fmt::Display;
use stmt::Stmt;

use crate::lexer::{Token, TokenLoc};

#[derive(Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
}

// impl Display for node for transpiler later
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr(expr) => write!(f, "{}", expr),
            Node::Stmt(stmt) => write!(f, "{}", stmt),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    UnexpectedToken(Token, (usize, usize)),
    ExpectToken(Token, (usize, usize)),
}

pub struct Parser {
    pub tokens: VecDeque<TokenLoc>,
}

type Maybe<T> = Result<T, ParserError>;

impl Parser {
    pub fn gen_ast(&mut self) -> Maybe<Vec<Node>> {
        let mut ast: Vec<Node> = Vec::new();

        while !self.tokens.is_empty() && !matches!(self.curr(), Token::EOF) {
            ast.push(self.parse_stmt()?);
            self.expect(Token::EOL)?
        }

        return Ok(ast);
    }

    fn expect(&mut self, token: Token) -> Maybe<()> {
        let next = self.next_loc();
        match next.0 == token {
            true => Ok(()),
            false => Err(ParserError::ExpectToken(token, (next.1 .0, next.1 .1))),
        }
    }

    fn curr(&self) -> &Token {
        &self.tokens.get(0).unwrap().0
    }

    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap().0
    }

    fn next_loc(&mut self) -> TokenLoc {
        self.tokens.pop_front().unwrap()
    }

    fn parse_stmt(&mut self) -> Maybe<Node> {
        match self.curr() {
            _ => self.parse_expr(),
        }
    }

    fn parse_expr(&mut self) -> Maybe<Node> {
        Ok(Node::Expr(self.parse_add_bin()?))
    }

    /// Parse a binary expression
    fn parse_add_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_mul_bin()?;
        while matches!(self.curr(), Token::Plus | Token::Minus) {
            let op = self.next();
            let right = self.parse_mul_bin()?;

            // constant collapsing time
            if let (Expr::Num(l), Expr::Num(r)) = (&left, &right) {
                left = Expr::Num(match op {
                    Token::Plus => l + r,
                    Token::Minus => l - r,
                    _ => unreachable!(),
                })
            } else {
                left = Expr::bin(left, op, right);
            };
        }
        return Ok(left);
    }

    fn parse_mul_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_unit()?;
        while matches!(self.curr(), Token::Star | Token::Slash) {
            let op = self.next();
            let right = self.parse_unit()?;

            if let (Expr::Num(l), Expr::Num(r)) = (&left, &right) {
                left = Expr::Num(match op {
                    Token::Star => l * r,
                    Token::Slash => l / r,
                    _ => unreachable!(),
                })
            } else {
                left = Expr::bin(left, op, right);
            };
        }
        return Ok(left);
    }

    /// Parse a unit or literal
    fn parse_unit(&mut self) -> Maybe<Expr> {
        let curr = self.next_loc();
        let expr = match curr.0 {
            Token::Num(it) => Expr::Num(it),
            Token::Iden(it) => Expr::Iden(it),
            _ => return Err(ParserError::UnexpectedToken(curr.0, curr.1)),
        };
        Ok(expr)
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{tokenize, Token};
    use crate::parser::{Expr, Node, Parser, ParserError};

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
                    .gen_ast()
                    .unwrap(),
                    $output
                )
            }
        };
    }

    macro_rules! testError {
        ($name:ident, $source:literal => $output:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    (Parser {
                        tokens: tokenize($source.to_string()).unwrap()
                    })
                    .gen_ast()
                    .unwrap_err(),
                    $output
                )
            }
        };
    }

    test!(simple, "1" => ast![Expr::num(1)]);
    test!(bin, "1 + 1" => ast![Expr::num(2)]);

    test!(multiline, "hello\n12"=>ast![Expr::iden("hello"), Expr::num(12)]);
    testError!(line_break, "1\n+\n1" => ParserError::UnexpectedToken(Token::Plus, (1, 0)));
}
