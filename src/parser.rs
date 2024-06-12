mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use std::fmt::Display;
use stmt::Stmt;

use crate::lexer::{Loc, Token, TokenLoc};

use self::stmt::{EventIden, EventType};

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
    InvalidEventIden(Loc, usize),
    InvalidEventType(Loc, usize),
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

    fn parse_block(&mut self) -> Maybe<Box<Stmt>> {
        self.expect(&[Token::Colon])?;
        Ok(match self.curr() {
            Token::EOL => {
                self.next();
                let mut body = Vec::new();
                self.expect(&[Token::IND])?;
                while self.not_eof() && !matches!(self.curr(), Token::DED) {
                    body.push(self.parse_stmt()?);
                }
                if self.not_eof() {
                    self.expect(&[Token::DED])?;
                }
                Box::new(Stmt::Block(body))
            }
            _ => Box::new(Stmt::Block(vec![self.parse_stmt()?])),
        })
    }

    fn parse_stmt(&mut self) -> Maybe<Node> {
        // if it is a expression skip EOL check else check them for stmt
        let stmt = 'o: {
            Node::Stmt(match self.curr() {
                Token::Var => self.parse_var_decl()?,
                Token::If => self.parse_if()?,
                Token::When => self.parse_event()?,
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
        match self.next_loc() {
            (Token::Iden(name), _) => {
                self.expect(&[Token::Equal])?;
                let val = self.parse_expr()?;
                Ok(Stmt::VarDecl(name, val))
            }
            (get, loc) => Err(ParseError::ExpectToken {
                len: get.get_len(),
                get,
                loc,
                want: vec![Token::Iden("identifier".to_string())],
            }),
        }
    }

    fn parse_if(&mut self) -> Maybe<Stmt> {
        self.next();

        let cond = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(Stmt::If(cond, body))
    }

    fn parse_event(&mut self) -> Maybe<Stmt> {
        self.next();
        let iden: EventIden;
        let event: EventType;

        match self.next_loc() {
            (Token::Iden(it), loc) if matches!(it.as_str(), "die" | "hit" | "summon" | "move") => {
                iden = EventIden::This;
                event = match it.as_str() {
                    "die" => EventType::Die,
                    "hit" => EventType::Hit,
                    "summon" => EventType::Summon,
                    "move" => EventType::Move,
                    _ => return Err(ParseError::InvalidEventType(loc, it.len())),
                };
            }
            (Token::Iden(it), loc) => {
                iden = match it.as_str() {
                    "this" => EventIden::This,
                    "other" => EventIden::Other,
                    "any" => EventIden::Any,
                    _ => return Err(ParseError::InvalidEventIden(loc, it.len())),
                };
                event = match self.next_loc() {
                    (Token::Iden(it), loc) => match it.as_str() {
                        "die" => EventType::Die,
                        "hit" => EventType::Hit,
                        "summon" => EventType::Summon,
                        "move" => EventType::Move,
                        _ => return Err(ParseError::InvalidEventType(loc, it.len())),
                    },
                    (get, loc) => {
                        return Err(ParseError::ExpectToken {
                            len: get.get_len(),
                            get,
                            loc,
                            want: vec![Token::Iden("identifier".to_string())],
                        })
                    }
                };
            }
            (get, loc) => {
                return Err(ParseError::ExpectToken {
                    len: get.get_len(),
                    get,
                    loc,
                    want: vec![Token::Iden("identifier".to_string())],
                })
            }
        }

        let cond = match self.curr() {
            Token::And => {
                self.next();
                Some(self.parse_expr()?)
            }
            _ => None,
        };

        let body = self.parse_block()?;

        Ok(Stmt::Event(iden, event, cond, body))
    }

    // EXPR PARSE
    fn parse_expr(&mut self) -> Maybe<Expr> {
        Ok(match self.parse_add_bin()? {
            Expr::Paren(expr) => *expr,
            expr => expr,
        })
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
                left = Expr::Bin(Box::new(left), op, Box::new(right));
            };
        }
        return Ok(left);
    }

    /// Parse mul/div binary expression
    fn parse_mul_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_un()?;
        while matches!(self.curr(), Token::Star | Token::Slash) {
            let op = self.next();
            let right = self.parse_un()?;

            if let (Expr::Num(l), Expr::Num(r)) = (&left, &right) {
                left = Expr::Num(match op {
                    Token::Star => l * r,
                    Token::Slash => l / r,
                    _ => unreachable!(),
                })
            } else {
                left = Expr::Bin(Box::new(left), op, Box::new(right));
            };
        }
        return Ok(left);
    }

    fn parse_un(&mut self) -> Maybe<Expr> {
        Ok(match self.curr() {
            Token::Minus => match self.parse_expr()? {
                Expr::Num(num) => Expr::Num(-num),
                expr => Expr::Un(Token::Minus, Box::new(expr)),
            },
            _ => self.parse_unit()?,
        })
    }

    /// Parse a unit or literal
    fn parse_unit(&mut self) -> Maybe<Expr> {
        let curr = self.next_loc();
        Ok(match curr.0 {
            Token::Num(it) => Expr::Num(it),
            Token::Iden(it) => Expr::Iden(it),
            Token::ReserveIden(it) => Expr::ReserveIden(it),
            Token::OpenParen => {
                let t = self.parse_expr()?;
                self.expect(&[Token::CloseParen])?;
                match t {
                    Expr::Num(_) | Expr::Iden(_) | Expr::ReserveIden(_) => t,
                    _ => Expr::Paren(Box::new(t)),
                }
            }
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
    use crate::lexer::{lex, Iden, Loc, Token};
    use crate::parser::{
        stmt::{EventIden::*, EventType::*},
        EventIden, EventType, Expr,
        Node::{Expr as ExprN, Stmt as StmtN},
        ParseError,
        ParseError::*,
        Parser, Stmt,
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

    fn new_iden(name: &str) -> Expr {
        Expr::Iden(name.to_string())
    }

    fn new_num(value: usize) -> Expr {
        Expr::Num(value as f32)
    }

    fn new_if_stmt(cond: Expr, body: Stmt) -> Stmt {
        Stmt::If(cond, Box::new(body))
    }

    fn new_event(iden: EventIden, event: EventType, expr: Option<Expr>, body: Stmt) -> Stmt {
        Stmt::Event(iden, event, expr, Box::new(body))
    }

    fn new_bin(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }

    fn new_paren(expr: Expr) -> Expr {
        Expr::Paren(Box::new(expr))
    }

    fn expect(get: Token, loc: Loc, want: Vec<Token>, len: usize) -> ParseError {
        ExpectToken {
            get,
            loc,
            want,
            len,
        }
    }

    test!(simple, "1" => expr_ast![new_num(1)]);
    test!(empty, "" => []);

    test!(bin, "1 + 1" => expr_ast![new_num(2)]);
    test!(bin_oop, "1 + 1 * 9" => expr_ast![new_num(10)]);

    test!(paren, "(1 + 1) * 8" => expr_ast![new_num(16)]);
    test!(paren_iden, "1 * (1 + a)" => expr_ast![new_bin(new_num(1), Token::Star, new_paren(new_bin(new_num(1), Token::Plus, new_iden("a"))))]);

    test!(multiline, "hello\n12"=>expr_ast![new_iden("hello"), new_num(12)]);

    test!(var, "var x = 1" => vec![StmtN(Stmt::VarDecl("x".to_string(), new_num(1)))]);
    should_error!(var_where_iden, "var 1 = 1" => expect(Token::Num(1.0), (0, 4), vec![Token::Iden("identifier".to_string())], 1));
    should_error!(var_where_equal, "var e" => expect(Token::EOF, (0, 5), vec![Token::Equal], 1));

    test!(if_stmt, "if 1 + 1:\n\thelo" => [StmtN(new_if_stmt(new_num(2), Stmt::Block(vec![ExprN(new_iden("helo"))])))]);
    test!(if_stmt_nest, "if true: if true: hello" => [
        StmtN(
            new_if_stmt(
                Expr::ReserveIden(Iden::True),
                Stmt::Block(
                    vec![
                        StmtN(
                            new_if_stmt(
                                Expr::ReserveIden(Iden::True),
                                Stmt::Block(vec![ExprN(new_iden("hello"))])
                            )
                        )
                    ]
                )
            )
        )
    ]);

    test!(event, "when summon: 1" => [StmtN(new_event(This, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_explicit, "when this summon: 1" => [StmtN(new_event(This, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_other_iden, "when other summon: 1" => [StmtN(new_event(Other, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_condition, "when summon and true: 1" => [StmtN(new_event(This, Summon, Some(Expr::ReserveIden(Iden::True)), Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_other_iden_condition, "when other summon and true: 1" => [StmtN(new_event(Other, Summon, Some(Expr::ReserveIden(Iden::True)), Stmt::Block(vec![ExprN(new_num(1))])))]);

    should_error!(event_invalid_iden, "when what summon: 1" => InvalidEventIden((0, 5), 4));
    should_error!(event_invalid_event, "when this what: 1" => InvalidEventType((0, 10), 4));
}
