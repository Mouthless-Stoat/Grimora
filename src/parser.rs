mod expr;
mod stmt;

use expr::Expr;
use std::collections::VecDeque;
use std::fmt::Display;
use stmt::Stmt;

use crate::lexer::{Loc, Token, TokenLoc};

use self::stmt::{EventIden, EventType};

#[derive(Debug, PartialEq, Clone)]
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
    UnexpectedToken(Loc, usize, Token),
    ExpectToken(Loc, usize, Token, Vec<Token>),
    InvalidEventIden(Loc, usize),
    InvalidEventType(Loc, usize),
    AttrIden(Loc, usize, Expr, Expr),
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
        !self.is_curr(Token::EOF)
    }

    fn expect(&mut self, what: Token) -> Maybe<Token> {
        let next = self.next_loc();
        match next.0 == what {
            true => Ok(next.0),
            false => Err(ParseError::ExpectToken(
                (next.1 .0, next.1 .1),
                next.0.get_len(),
                next.0,
                [what].to_vec(),
            )),
        }
    }

    fn curr(&self) -> &Token {
        &self.tokens.get(0).unwrap().0
    }

    fn curr_loc(&self) -> &TokenLoc {
        &self.tokens.get(0).unwrap()
    }

    fn peek(&self) -> &Token {
        &self.tokens.get(1).unwrap_or(&(Token::EOF, (0, 0))).0
    }

    fn next(&mut self) -> Token {
        self.tokens.pop_front().unwrap().0
    }

    fn next_loc(&mut self) -> TokenLoc {
        self.tokens.pop_front().unwrap()
    }

    fn is_curr(&self, tk: Token) -> bool {
        *self.curr() == tk
    }

    fn is_currs(&self, tks: &[Token]) -> bool {
        tks.iter().any(|tk| *self.curr() == *tk)
    }

    fn is_peek(&self, tk: Token) -> bool {
        *self.peek() == tk
    }

    #[allow(dead_code)]
    fn is_peeks(&self, tks: &[Token]) -> bool {
        tks.iter().any(|tk| *self.peek() == *tk)
    }

    fn parse_args(&mut self) -> Maybe<Vec<Expr>> {
        let mut args = Vec::new();
        self.expect(Token::OpenParen)?;

        while !matches!(self.curr(), Token::CloseParen) {
            args.push(self.parse_expr()?);
            if !matches!(self.curr(), Token::CloseParen) {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::CloseParen)?;

        Ok(args)
    }

    fn parse_block(&mut self) -> Maybe<Box<Stmt>> {
        self.expect(Token::Colon)?;
        Ok(match self.curr() {
            Token::EOL => {
                self.next();
                let mut body = Vec::new();
                self.expect(Token::IND)?;
                while self.not_eof() && !self.is_curr(Token::DED) {
                    body.push(self.parse_stmt()?);
                }
                if self.not_eof() {
                    self.expect(Token::DED)?;
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
                Token::Iden(_) => break 'o self.parse_assign()?,
                _ => break 'o Node::Expr(self.parse_expr()?),
            })
        };

        // if not eof expect a eol
        if !matches!(self.curr(), Token::EOF | Token::DED) {
            self.expect(Token::EOL)?;
        }

        Ok(stmt)
    }

    //== STMT PARSE ==//

    fn parse_var_decl(&mut self) -> Maybe<Stmt> {
        self.next();
        match self.next_loc() {
            (Token::Iden(name), _) => {
                self.expect(Token::Equal)?;
                let val = self.parse_expr()?;
                Ok(Stmt::VarDecl(name, val))
            }
            (get, loc) => Err(ParseError::ExpectToken(
                loc,
                get.get_len(),
                get,
                vec![Token::Iden("identifier".to_string())],
            )),
        }
    }

    fn parse_if(&mut self) -> Maybe<Stmt> {
        self.next();

        let cond = self.parse_expr()?;
        let body = self.parse_block()?;

        match self.peek() {
            Token::Elif => Ok(Stmt::If(cond, body, {
                self.next();
                Some(Box::new(self.parse_if()?))
            })),
            Token::Else => Ok(Stmt::If(cond, body, {
                self.next();
                self.next();
                Some(self.parse_block()?)
            })),
            _ => Ok(Stmt::If(cond, body, None)),
        }
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
                        return Err(ParseError::ExpectToken(
                            loc,
                            get.get_len(),
                            get,
                            vec![Token::Iden("identifier".to_string())],
                        ))
                    }
                };
            }
            (get, loc) => {
                return Err(ParseError::ExpectToken(
                    loc,
                    get.get_len(),
                    get,
                    vec![Token::Iden("identifier".to_string())],
                ))
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

    fn parse_assign(&mut self) -> Maybe<Node> {
        let iden = self.parse_expr()?;
        if !((self.is_currs(&[Token::Plus, Token::Minus, Token::Star, Token::Slash])
            && self.is_peek(Token::Equal))
            || self.is_curr(Token::Equal))
        {
            return Ok(Node::Expr(iden));
        }

        let mut op = None;
        if !self.is_curr(Token::Equal) {
            op = Some(self.next());
        }
        self.next();

        let mut value = self.parse_expr()?;

        if let Some(op) = op {
            value = Expr::Bin(Box::new(iden.clone()), op, Box::new(value))
        }

        Ok(Node::Stmt(Stmt::Assign(iden, value)))
    }

    //== EXPR PARSE ==//

    fn parse_expr(&mut self) -> Maybe<Expr> {
        Ok(match self.parse_logic_bin()? {
            Expr::Paren(expr) => *expr,
            expr => expr,
        })
    }

    fn parse_logic_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_comp_bin()?;
        while matches!(self.curr(), Token::And | Token::Or) && !self.is_peek(Token::Equal) {
            let op = self.next();
            let right = self.parse_comp_bin()?;

            // constant collapsing time
            left = match (&left, &right) {
                (Expr::Bool(l), Expr::Bool(r)) => Expr::Bool(match op {
                    Token::And => *l && *r,
                    Token::Or => *l || *r,
                    _ => unreachable!(),
                }),
                _ => Expr::Bin(Box::new(left), op, Box::new(right)),
            };
        }
        return Ok(left);
    }

    fn parse_comp_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_add_bin()?;
        while matches!(
            self.curr(),
            Token::Greater | Token::GreaterEq | Token::Lesser | Token::LesserEq | Token::Equality
        ) && !self.is_peek(Token::Equal)
        {
            let op = self.next();
            let right = self.parse_add_bin()?;

            // constant collapsing time
            left = match (&left, &right) {
                (Expr::Num(l), Expr::Num(r)) => Expr::Bool(match op {
                    Token::Lesser => l < r,
                    Token::LesserEq => l <= r,
                    Token::Greater => l > r,
                    Token::GreaterEq => l >= r,
                    _ => unreachable!(),
                }),
                _ => Expr::Bin(Box::new(left), op, Box::new(right)),
            }
        }
        return Ok(left);
    }

    /// Parse add/sub binary expression
    fn parse_add_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_mul_bin()?;
        while matches!(self.curr(), Token::Plus | Token::Minus) && !self.is_peek(Token::Equal) {
            let op = self.next();
            let right = self.parse_mul_bin()?;

            // constant collapsing time
            left = match (&left, &right) {
                (Expr::Num(l), Expr::Num(r)) => Expr::Num(match op {
                    Token::Plus => l + r,
                    Token::Minus => l - r,
                    _ => unreachable!(),
                }),
                _ => Expr::Bin(Box::new(left), op, Box::new(right)),
            };
        }
        return Ok(left);
    }

    /// Parse mul/div binary expression
    fn parse_mul_bin(&mut self) -> Maybe<Expr> {
        let mut left = self.parse_un()?;
        while matches!(self.curr(), Token::Star | Token::Slash) && !self.is_peek(Token::Equal) {
            let op = self.next();
            let right = self.parse_un()?;

            left = match (&left, &right) {
                (Expr::Num(l), Expr::Num(r)) => Expr::Num(match op {
                    Token::Star => l * r,
                    Token::Slash => l / r,
                    _ => unreachable!(),
                }),
                _ => Expr::Bin(Box::new(left), op, Box::new(right)),
            };
        }
        return Ok(left);
    }

    fn parse_un(&mut self) -> Maybe<Expr> {
        Ok(match self.curr() {
            Token::Minus => match self.parse_un()? {
                Expr::Num(num) => Expr::Num(-num),
                expr => Expr::Un(Token::Minus, Box::new(expr)),
            },
            _ => self.parse_call()?,
        })
    }

    fn parse_call(&mut self) -> Maybe<Expr> {
        let mut caller = self.parse_attr()?;
        while self.is_curr(Token::OpenParen) {
            let args = self.parse_args()?;
            caller = Expr::Call(Box::new(caller), args);
        }
        Ok(caller)
    }

    fn parse_attr(&mut self) -> Maybe<Expr> {
        let start = self.curr_loc().1;
        let mut expr = self.parse_sub()?;
        while self.is_curr(Token::Dot) {
            self.next();
            let attr = self.parse_unit()?;
            if !matches!(attr, Expr::Iden(_)) {
                return Err(ParseError::AttrIden(
                    start,
                    format!("{expr}.{attr}").len(),
                    expr,
                    attr,
                ));
            }
            expr = Expr::Attr(Box::new(expr), Box::new(attr));
        }

        Ok(expr)
    }

    fn parse_sub(&mut self) -> Maybe<Expr> {
        let mut expr = self.parse_unit()?;
        while self.is_curr(Token::OpenBracket) {
            self.next();
            let index = self.parse_expr()?;
            self.expect(Token::CloseBracket)?;

            expr = Expr::Sub(Box::new(expr), Box::new(index))
        }
        Ok(expr)
    }

    /// Parse a unit or literal
    fn parse_unit(&mut self) -> Maybe<Expr> {
        let curr = self.next_loc();
        Ok(match curr.0 {
            Token::Num(num) => Expr::Num(num),
            Token::String(str) => Expr::String(str),
            Token::Iden(name) => Expr::Iden(name),
            Token::Card(name) => Expr::Card(name),
            Token::Bool(bool) => Expr::Bool(bool),
            Token::ResIden(name) => Expr::ResIden(name),
            Token::OpenParen => {
                let t = self.parse_expr()?;
                self.expect(Token::CloseParen)?;
                match t {
                    Expr::Num(_) | Expr::Iden(_) | Expr::ResIden(_) => t,
                    _ => Expr::Paren(Box::new(t)),
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    curr.1,
                    curr.0.get_len(),
                    curr.0,
                ));
            }
        })
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::{lex, Loc, Token};
    use crate::parser::{
        stmt::{EventIden::*, EventType::*},
        EventIden, EventType, Expr,
        Node::{Expr as ExprN, Stmt as StmtN},
        ParseError,
        ParseError::*,
        Parser, Stmt,
    };

    use super::Node;

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

    fn new_str(value: &str) -> Expr {
        Expr::String(value.to_string())
    }

    fn new_block(block: &[Node]) -> Stmt {
        Stmt::Block(block.to_vec())
    }

    fn new_if_stmt(cond: Expr, body: Stmt) -> Stmt {
        Stmt::If(cond, Box::new(body), None)
    }

    fn new_if_else_stmt(cond: Expr, body: Stmt, else_body: Stmt) -> Stmt {
        Stmt::If(cond, Box::new(body), Some(Box::new(else_body)))
    }

    fn new_if_elif_stmt(cond: Expr, body: Stmt, elif_cond: Expr, elif_body: Stmt) -> Stmt {
        Stmt::If(
            cond,
            Box::new(body),
            Some(Box::new(new_if_stmt(elif_cond, elif_body))),
        )
    }

    fn new_event(iden: EventIden, event: EventType, expr: Option<Expr>, body: Stmt) -> Stmt {
        Stmt::Event(iden, event, expr, Box::new(body))
    }

    fn new_bin(left: Expr, op: Token, right: Expr) -> Expr {
        Expr::Bin(Box::new(left), op, Box::new(right))
    }

    fn new_assign(name: Expr, value: Expr) -> Stmt {
        Stmt::Assign(name, value)
    }

    fn new_paren(expr: Expr) -> Expr {
        Expr::Paren(Box::new(expr))
    }

    fn new_card(name: &str) -> Expr {
        Expr::Card(name.to_string())
    }

    fn new_call(caller: Expr, args: &[Expr]) -> Expr {
        Expr::Call(Box::new(caller), args.to_vec())
    }

    fn new_attr(expr: Expr, attr: Expr) -> Expr {
        Expr::Attr(Box::new(expr), Box::new(attr))
    }

    fn new_sub(expr: Expr, index: Expr) -> Expr {
        Expr::Sub(Box::new(expr), Box::new(index))
    }

    fn expect(get: Token, loc: Loc, want: Vec<Token>, len: usize) -> ParseError {
        ExpectToken(loc, len, get, want)
    }

    test!(simple, "1" => expr_ast![new_num(1)]);
    test!(string, "\"a\"" => expr_ast![new_str("a")]);
    test!(card, "Wolf" => expr_ast![new_card("Wolf")]);
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
                Expr::Bool(true),
                Stmt::Block(
                    vec![
                        StmtN(
                            new_if_stmt(
                                Expr::Bool(true),
                                Stmt::Block(vec![ExprN(new_iden("hello"))])
                            )
                        )
                    ]
                )
            )
        )
    ]);
    test!(if_stmt_else, "if 1:\n\t1\nelse:\n\t1" => [StmtN(new_if_else_stmt(new_num(1), new_block(&[ExprN(new_num(1))]), new_block(&[ExprN(new_num(1))])))]);
    test!(if_stmt_elif, "if 1:\n\t1\nelif 1:\n\t1" => [StmtN(new_if_elif_stmt(new_num(1), new_block(&[ExprN(new_num(1))]), new_num(1), new_block(&[ExprN(new_num(1))])))]);

    test!(event, "when summon: 1" => [StmtN(new_event(This, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_explicit, "when this summon: 1" => [StmtN(new_event(This, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_other_iden, "when other summon: 1" => [StmtN(new_event(Other, Summon, None, Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_condition, "when summon and true: 1" => [StmtN(new_event(This, Summon, Some(Expr::Bool(true)), Stmt::Block(vec![ExprN(new_num(1))])))]);
    test!(event_other_iden_condition, "when other summon and true: 1" => [StmtN(new_event(Other, Summon, Some(Expr::Bool(true)), Stmt::Block(vec![ExprN(new_num(1))])))]);

    should_error!(event_invalid_iden, "when what summon: 1" => InvalidEventIden((0, 5), 4));
    should_error!(event_invalid_event, "when this what: 1" => InvalidEventType((0, 10), 4));

    test!(assign, "a = 1" => [StmtN(new_assign(new_iden("a"), new_num(1)))]);

    test!(call, "a()" => [ExprN(new_call(new_iden("a"), &[]))]);
    test!(call_arg, "a(1)" => [ExprN(new_call(new_iden("a"), &[new_num(1)]))]);
    test!(call_args, "a(1, 2, 3, 4)" => [ExprN(new_call(new_iden("a"), &[new_num(1), new_num(2), new_num(3), new_num(4)]))]);
    test!(call_chain, "a()()" => [ExprN(new_call(new_call(new_iden("a"), &[]), &[]))]);

    test!(attr, "a.b" => [ExprN(new_attr(new_iden("a"), new_iden("b")))]);
    test!(attr_chain, "a.b.c" => [ExprN(new_attr(new_attr(new_iden("a"), new_iden("b")), new_iden("c")))]);
    test!(attr_call, "a.b()" => [ExprN(new_call(new_attr(new_iden("a"), new_iden("b")), &[]))]);

    test!(sub, "a[1]" => [ExprN(new_sub(new_iden("a"), new_num(1)))]);
    test!(sub_chain, "a[1][2]" => [ExprN(new_sub(new_sub(new_iden("a"), new_num(1)), new_num(2)))]);
    test!(sub_call, "a[1]()" => [ExprN(new_call(new_sub(new_iden("a"), new_num(1)), &[]))]);
    test!(sub_attr, "a[1].a" => [ExprN(new_attr(new_sub(new_iden("a"), new_num(1)), new_iden("a")))]);
}
