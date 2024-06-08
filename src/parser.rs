mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use std::fmt::Display;
use stmt::Stmt;

use crate::lexer::{Loc, Token, TokenLoc};

#[derive(Debug, PartialEq)]
pub enum Node {
    Expr(Expr),
    Stmt(Stmt),
}

// impl Display for node for transpiler later
impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Expr(expr) => write!(f, "{expr}"),
            Node::Stmt(stmt) => write!(f, "{stmt}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        get: Token,
        loc: Loc,
        len: usize,
    },
    ExpectToken {
        get: Token,
        loc: Loc,
        want: Vec<Token>,
        len: usize,
    },
}

pub struct Parser {
    pub tokens: VecDeque<TokenLoc>,
}

type Maybe<T> = Result<T, ParseError>;

impl Parser {
    pub fn gen_ast(&mut self) -> Maybe<Vec<Node>> {
        let mut ast: Vec<Node> = Vec::new();

        while !self.tokens.is_empty() && self.not_eof() {
            ast.push(self.parse_stmt()?);
        }

        return Ok(ast);
    }

    fn not_eof(&self) -> bool {
        !matches!(self.curr(), Token::EOF)
    }

    fn expect(&mut self, what: &[Token]) -> Maybe<Token> {
        let next = self.next_loc();
        match what.iter().any(|tk| next.0 == *tk) {
            true => Ok(next.0),
            false => Err(ParseError::ExpectToken {
                len: next.0.get_len(),
                get: next.0,
                loc: (next.1 .0, next.1 .1),
                want: what.to_vec(),
            }),
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

    fn parse_block(&mut self) -> Maybe<Stmt> {
        self.expect(&[Token::Colon])?;
        if matches!(self.curr(), Token::EOL) {
            self.next();
            let mut body = Vec::new();
            self.expect(&[Token::IND])?;
            while self.not_eof() && !matches!(self.curr(), Token::DED) {
                body.push(self.parse_stmt()?);
            }
            if self.not_eof() {
                self.expect(&[Token::DED])?;
            }
            Ok(Stmt::Block(body))
        } else {
            Ok(Stmt::Block(vec![self.parse_stmt()?]))
        }
    }

    fn parse_stmt(&mut self) -> Maybe<Node> {
        // if it is a expression skip EOL check else check them for stmt
        let stmt = 'o: {
            Node::Stmt(match self.curr() {
                Token::Var => self.parse_var_decl()?,
                Token::If => self.parse_if()?,
                _ => break 'o Node::Expr(self.parse_expr()?),
            })
        };

        // if not eof expect a eol
        if !matches!(self.curr(), Token::EOF | Token::DED) {
            self.expect(&[Token::EOL])?;
        }

        Ok(stmt)
    }

    // STMT PARSE
    fn parse_var_decl(&mut self) -> Maybe<Stmt> {
        self.next();
        let name = self.next_loc();
        if matches!(name.0, Token::Iden(_)) {
            let name = name.0;
            self.expect(&[Token::Equal])?;
            let val = self.parse_expr()?;
            Ok(Stmt::VarDecl(
                match name {
                    Token::Iden(n) => n,
                    _ => unreachable!(),
                },
                val,
            ))
        } else {
            Err(ParseError::ExpectToken {
                len: name.0.get_len(),
                get: name.0,
                loc: name.1,
                want: vec![Token::Iden("identifier".to_string())],
            })
        }
    }

    fn parse_if(&mut self) -> Maybe<Stmt> {
        self.next();

        let cond = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(Stmt::if_stmt(cond, body))
    }

    // EXPR PARSE
    fn parse_expr(&mut self) -> Maybe<Expr> {
        Ok(self.parse_add_bin()?)
    }

    /// Parse add/sub binary expression
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

    /// Parse mul/div binary expression
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
        Ok(match curr.0 {
            Token::Num(it) => Expr::Num(it),
            Token::Iden(it) => Expr::Iden(it),
            Token::True => Expr::Bool(true),
            Token::False => Expr::Bool(false),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    len: curr.0.get_len(),
                    get: curr.0,
                    loc: curr.1,
                });
            }
        })
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{lex, Loc, Token};
    use crate::parser::{
        Expr,
        Node::{Expr as ExprN, Stmt as StmtN},
        ParseError, Parser, Stmt,
    };

    // Helper to make typing ast less annoying
    macro_rules! expr_ast {
        ($($node:expr),*) => {
            {
                let mut t = Vec::new();
                $(
                    t.push(ExprN($node));
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
                    Parser {
                        tokens: lex($source.to_string()).ok().unwrap()
                    }
                    .gen_ast()
                    .unwrap(),
                    $output
                )
            }
        };
    }

    macro_rules! should_error {
        ($name:ident, $source:literal => $error:expr) => {
            #[test]
            fn $name() {
                assert_eq!(
                    Parser {
                        tokens: lex($source.to_string()).ok().unwrap()
                    }
                    .gen_ast()
                    .unwrap_err(),
                    $error
                )
            }
        };
    }

    fn iden(name: &str) -> Expr {
        Expr::Iden(name.to_string())
    }

    fn num(value: usize) -> Expr {
        Expr::Num(value as f32)
    }

    fn expect(get: Token, loc: Loc, want: Vec<Token>, len: usize) -> ParseError {
        ParseError::ExpectToken {
            get,
            loc,
            want,
            len,
        }
    }

    test!(simple, "1" => expr_ast![num(1)]);
    test!(empty, "" => []);

    test!(bin, "1 + 1" => expr_ast![num(2)]);
    test!(bin_oop, "1 + 1 * 9" => expr_ast![num(10)]);

    test!(multiline, "hello\n12"=>expr_ast![iden("hello"), num(12)]);

    test!(var, "var x = 1" => vec![StmtN(Stmt::VarDecl("x".to_string(), num(1)))]);
    should_error!(var_where_iden, "var 1 = 1" => expect(Token::Num(1.0), (0, 4), vec![Token::Iden("identifier".to_string())], 1));
    should_error!(var_where_equal, "var e" => expect(Token::EOF, (0, 5), vec![Token::Equal], 1));

    test!(if_stmt, "if 1 + 1:\n\thelo" => [StmtN(Stmt::if_stmt(num(2), Stmt::Block(vec![ExprN(iden("helo"))])))]);
    test!(if_stmt_nest, "if true: if true: hello" => [
        StmtN(
            Stmt::if_stmt(
                Expr::Bool(true),
                Stmt::Block(
                    vec![
                        StmtN(
                            Stmt::if_stmt(
                                Expr::Bool(true),
                                Stmt::Block(vec![ExprN(iden("hello"))])
                            )
                        )
                    ]
                )
            )
        )
    ]);
}
