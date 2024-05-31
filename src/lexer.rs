use std::collections::VecDeque;
use std::fmt::{Debug, Display};

macro_rules! char_token {
    ($src:ident, $tokens:ident, $($char:literal: $token:expr),*) => {
        if let Some(tk) = {
            match $src[0] {
                $(
                    $char => Some($token),
                 )*
                _ => None
            }
        } {
            $tokens.push(tk);
            $src.pop_front();
            continue;
        }
    };
}

#[allow(unused_macros)]
macro_rules! multichar_token {
    ($src:ident, $tokens:ident, $($multi:literal: $token:expr),*) => {{
        let t = $src.make_contiguous();
        $(
            if t.len() >= $multi.len()
                && t.get(..$multi.len()).unwrap().iter().collect() == $multi.to_string()
            {
                $tokens.push($token);
                $multi.chars().for_each(|_| { $src.pop_front(); });
                continue;
            }
        )*
    }};
}

#[derive(PartialEq)]
pub enum Token {
    Int(f64),
    Iden(String),
    Plus,
    Minus,
    Star,
    Slash,

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
            _ => unimplemented!(),
        }
    }
}

// impl Debug for printing using the same impl for Display
impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn tokenize(source: String) -> Result<VecDeque<Token>, String> {
    let mut tokens = Vec::<Token>::new();
    let mut src: VecDeque<char> = source.chars().collect();

    while src.len() > 0 {
        let c = src[0];

        if c.is_whitespace() {
            src.pop_front();
            continue;
        }

        char_token!(
            src, tokens,
            '+': Token::Plus,
            '-': Token::Minus,
            '*': Token::Star,
            '/': Token::Slash
        );

        if !c.is_alphanumeric() {
            todo!("Return a lexer error instead of string");
        }

        let is_alpha = c.is_alphabetic();
        let cond: fn(&char) -> bool = match is_alpha {
            true => |c| c.is_alphabetic(),
            false => |c| c.is_numeric(),
        };

        let acc = {
            let mut t = String::new();
            loop {
                match src.get(0).is_some_and(cond) {
                    true => t.push(src.pop_front().unwrap()),
                    false => break t,
                }
            }
        };

        match is_alpha {
            true => tokens.push(Token::Iden(acc)),
            false => tokens.push(Token::Int(acc.parse().unwrap())),
        }
    }

    tokens.push(Token::EOF);

    return Ok(VecDeque::from(tokens));
}

#[cfg(test)]
mod test {
    use crate::lexer::{tokenize, Token};

    macro_rules! test {
        ($name:ident, $source:literal => $output:expr) => {
            #[test]
            fn $name() {
                assert_eq!(tokenize($source.to_string()).unwrap(), $output)
            }
        };
    }

    test!(simple, "1 + 1" => [Token::Int(1.0), Token::Plus, Token::Int(1.0),Token::EOF]);
    test!(identifier, "thisIsAIdentifier" => [Token::Iden("thisIsAIdentifier".to_string()), Token::EOF]);
}
