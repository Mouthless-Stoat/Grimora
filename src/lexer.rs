use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::{char, usize};

pub type TokenLoc = (Token, (usize, usize));

#[derive(Debug, PartialEq)]
pub enum Token {
    // literal
    Num(f32),
    Iden(String),

    // single char
    Plus,
    Minus,
    Star,
    Slash,
    Equal,

    // multi char
    Arrow,
    Equality,
    Greater,
    Lesser,
    GreaterEq,
    LesserEq,

    // keyword
    Var,
    When,
    Attack,
    Summon,
    Hit,
    Death,
    Move,

    EOL,
    EOF,
}

// impl Display for to string conversion
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Num(num) => return write!(f, "{}", num.to_string()),
                Token::Iden(iden) => return write!(f, "{}", iden),
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Star => "*",
                Token::Slash => "/",
                Token::Equal => "=",
                Token::Arrow => "=>",

                Token::Equality => "==",
                Token::Greater => ">",
                Token::Lesser => "<",
                Token::GreaterEq => ">=",
                Token::LesserEq => "<=",

                _ => return write!(f, "{:?}", self),
            }
        )
    }
}

impl Token {
    pub fn get_len(&self) -> usize {
        match self {
            Token::EOL => 1,
            _ => format!("{}", self).len(),
        }
    }
}

macro_rules! multi_token {
    ($src:ident, $tokens:ident, $loc:ident, $($multi:literal => $tk:expr),*) => {
        {
            let t = $src.make_contiguous();
            $(
                {
                    let len = $multi.len();
                    if t.len() >= len && t[..len].iter().map(|(_, c)| c).collect::<String>() == $multi
                    {
                        $tokens.push(($tk, $loc));
                        $src.drain(..len);
                        continue;
                    }
                }
            )*
        }
    };
}

pub type Loc = (usize, usize);
pub type LexError = (char, Loc);

pub fn lex(source: String) -> Result<VecDeque<TokenLoc>, LexError> {
    if source.len() <= 0 {
        return Ok(VecDeque::new());
    }

    let mut tokens = Vec::<TokenLoc>::new();

    for (num, line) in source.lines().enumerate() {
        let mut src: VecDeque<(usize, char)> = line.char_indices().collect();

        while src.len() > 0 {
            // get some important stuff
            let loc = (num, src[0].0);
            let c = src[0].1;

            // skipping time
            if c.is_whitespace() {
                src.pop_front();
                continue;
            }

            // check for longer token
            multi_token!(
                src, tokens, loc,


                "==" =>Token::Equality,
                ">=" => Token::GreaterEq,
                "<=" => Token::LesserEq,
                ">" => Token::Greater,
                "<" => Token::Lesser,

                "=>" => Token::Arrow
            );

            // check for sing char token
            if let Some(tk) = 'o: {
                // wakcy ik but it reduce typing Some on every line
                Some(match c {
                    '+' => Token::Plus,
                    '-' => Token::Minus,
                    '*' => Token::Star,
                    '/' => Token::Slash,
                    '=' => Token::Equal,
                    _ => break 'o None,
                })
            } {
                tokens.push((tk, loc));
                src.pop_front();
                continue;
            };

            // if it not a special character then i have to be number or alpha
            if !c.is_alphanumeric() {
                return Err((c, loc)); // lexer don't know what this is
            }

            // constructing number and alphabet time
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
                true => tokens.push((
                    match acc.as_str() {
                        "var" => Token::Var,
                        "when" => Token::When,
                        "attack" => Token::Attack,
                        "summon" => Token::Summon,
                        "hit" => Token::Hit,
                        "death" => Token::Death,
                        "move" => Token::Move,
                        _ => Token::Iden(acc),
                    },
                    loc,
                )),
                false => tokens.push((Token::Num(acc.parse().unwrap()), loc)),
            }
        }

        tokens.push((Token::EOL, (num, line.len())))
    }

    tokens.push((Token::EOF, (usize::MAX, usize::MAX)));

    return Ok(VecDeque::from(tokens));
}

#[cfg(test)]
mod test {

    use crate::lexer::{lex, Token};

    fn num(num: usize) -> Token {
        Token::Num(num as f32)
    }

    fn iden(name: &str) -> Token {
        Token::Iden(name.to_string())
    }

    // macro to help with writing test
    macro_rules! test {
        ($name:ident, $source:literal => [$($tk:expr , $line:literal:$col:literal);*]) => {
            #[test]
            fn $name() {
                assert_eq!(lex($source.to_string()).unwrap(), [$(($tk, ($line, $col)),)* (Token::EOF, (usize::MAX, usize::MAX))])
            }
        };
    }

    test!(simple, "1 + 1" => [num(1),0:0; Token::Plus,0:2; num(1),0:4; Token::EOL,0:5]);
    test!(identifier, "thisIsAIdentifier" => [iden("thisIsAIdentifier"),0:0; Token::EOL,0:17]);
    test!(keyword, "when attack" => [Token::When,0:0; Token::Attack,0:5; Token::EOL,0:11]);

    test!(multiline, "hello\n12" => [iden("hello"),0:0; Token::EOL,0:5; num(12),1:0; Token::EOL,1:2]);

    test!(multi_token, "== >= <= =>" => [Token::Equality,0:0; Token::GreaterEq,0:3; Token::LesserEq,0:6; Token::Arrow,0:9]);
}
