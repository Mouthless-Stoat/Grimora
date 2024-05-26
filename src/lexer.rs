use std::collections::VecDeque;

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

#[derive(Debug, PartialEq)]
pub enum Token {
    Int(usize),
    Iden(String),
    Plus,
    Minus,
    Star,
    Slash,
}

pub fn tokenize(source: String) -> Result<Vec<Token>, String> {
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
            return Err("Idk what this token is".to_string());
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

    return Ok(tokens);
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

    test!(simple, "1 + 1" => [Token::Int(1), Token::Plus, Token::Int(1)]);
    test!(identifier, "thisIsAIdentifier" => [Token::Iden("thisIsAIdentifier".to_string())]);
}
