use core::f64;
use std::collections::VecDeque;
use std::fmt::{Debug, Display};

#[derive(Debug, PartialEq)]
pub struct TokenLoc {
    pub token: Token,
    pub loc: (usize, usize),
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Num(f32),
    Iden(String),
    Plus,
    Minus,
    Star,
    Slash,

    When,
    Attack,
    Summon,
    Hit,
    Death,
    Move,

    EOF,
}

// impl Display for to string conversion
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            _ => write!(f, "idk"),
        }
    }
}

impl Token {
    pub fn iden(name: &str) -> Token {
        Token::Iden(name.to_string())
    }

    pub fn num(value: usize) -> Token {
        Token::Num(value as f32)
    }
}

pub fn tokenize(source: String) -> Result<VecDeque<TokenLoc>, String> {
    let mut tokens = Vec::<TokenLoc>::new();

    for (num, line) in source.lines().enumerate() {
        let mut src: VecDeque<(usize, char)> = line.char_indices().collect();

        while src.len() > 0 {
            let idx = src[0].0;
            let c = src[0].1;

            if c.is_whitespace() {
                src.pop_front();
                continue;
            }

            if let Some(tk) = {
                match c {
                    '+' => Some(Token::Plus),
                    '-' => Some(Token::Minus),
                    '*' => Some(Token::Star),
                    '/' => Some(Token::Slash),
                    _ => None,
                }
            } {
                tokens.push(TokenLoc {
                    token: tk,
                    loc: (num, idx),
                });
                src.pop_front();
                continue;
            };

            if !c.is_alphanumeric() {
                todo!("Return a lexer error instead of string");
            }

            let is_alpha = c.is_alphabetic();
            let cond: fn(&(usize, char)) -> bool = match is_alpha {
                true => |c| c.1.is_alphabetic(),
                false => |c| c.1.is_numeric(),
            };

            let acc = {
                let mut t = String::new();
                loop {
                    match src.get(0).is_some_and(cond) {
                        true => t.push(src.pop_front().unwrap().1),
                        false => break t,
                    }
                }
            };

            match is_alpha {
                true => tokens.push(TokenLoc {
                    token: match acc.as_str() {
                        "when" => Token::When,
                        "attack" => Token::Attack,
                        "summon" => Token::Summon,
                        "hit" => Token::Hit,
                        "death" => Token::Death,
                        "move" => Token::Move,
                        _ => Token::Iden(acc),
                    },
                    loc: (num, idx),
                }),
                false => tokens.push(TokenLoc {
                    token: Token::Num(acc.parse().unwrap()),
                    loc: (num, idx),
                }),
            }
        }
    }

    tokens.push(TokenLoc {
        token: Token::EOF,
        loc: (usize::MAX, usize::MAX),
    });

    return Ok(VecDeque::from(tokens));
}

#[cfg(test)]
mod test {
    use crate::lexer::{tokenize, Token, TokenLoc};

    // macro to help with writing test
    macro_rules! test {
        ($name:ident, $source:literal => [$($tk:expr , $line:literal:$col:literal);*]) => {
            #[test]
            fn $name() {
                assert_eq!(tokenize($source.to_string()).unwrap(), [$(TokenLoc {token: $tk, loc: ($line, $col)},)* TokenLoc {token: Token::EOF, loc: (usize::MAX, usize::MAX)}])
            }
        };
    }

    test!(simple, "1 + 1" => [Token::num(1),0:0; Token::Plus,0:2; Token::num(1),0:4]);
    test!(identifier, "thisIsAIdentifier" => [Token::iden("thisIsAIdentifier"),0:0]);
    test!(keyword, "when attack" => [Token::When,0:0; Token::Attack,0:5]);

    test!(multiline, "hello\n12" => [Token::iden("hello"),0:0; Token::num(12),1:0]);
}
