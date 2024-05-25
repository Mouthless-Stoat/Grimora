use std::collections::VecDeque;
use std::process;

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
                && t.get(..$multi.len()).unwrap().iter().collect::<String>() == $multi.to_string()
            {
                $tokens.push($token);
                $multi.chars().for_each(|_| { $src.pop_front(); });
                continue;
            }
        )*
    }};
}

#[derive(Debug)]
pub enum Token {
    Int(usize),
    Iden(String),
    Plus,
    Minus,
    Star,
    Slash,
    Arrow,
}

pub fn tokenize(source: String) -> Vec<Token> {
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
            println!("I DON'T KNOW WHAT THIS TOKEN IS: {:?}", c);
            process::exit(1)
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

    return tokens;
}
