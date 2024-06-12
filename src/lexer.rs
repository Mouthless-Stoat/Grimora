use std::collections::VecDeque;
use std::fmt::{Debug, Display};
use std::{char, usize};

#[derive(Debug, PartialEq, Clone)]
pub enum Iden {
    Card,
    Friendly,
    Fight,
    Slots,
    True,
    False,
}

impl Display for Iden {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Iden::Card => "card",
                Iden::Friendly => "isFriendly",
                Iden::Fight => "fightManager",
                Iden::Slots => "slotManager",
                Iden::True => "true",
                Iden::False => "false",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // literal
    Num(f32),
    Iden(String),

    OpenParen,
    CloseParen,

    // single char
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    Percent,
    Colon,
    Dot,

    // multi char
    Arrow,
    Equality,
    Greater,
    Lesser,
    GreaterEq,
    LesserEq,
    DoubleStar,

    // keyword
    Var,
    If,
    When,

    And,
    Or,
    Not,

    ReserveIden(Iden),

    EOL,
    EOF,
    IND,
    DED,
}

// impl Display for to string conversion
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Num(num) => return write!(f, "{num}"),
                Token::Iden(iden) => return write!(f, "{iden}"),
                Token::Plus => "+",
                Token::Minus => "-",
                Token::Star => "*",
                Token::Slash => "/",
                Token::Equal => "=",
                Token::Colon => ":",
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
            Token::EOL | Token::EOF => 1,
            _ => format!("{self}").len(),
        }
    }
}

macro_rules! symbol_token {
    ($src:ident, $c:ident, $tokens:ident, $loc:ident, $($multi:literal => $tk:expr),* ;-----; $($single:literal => $single_tk:expr),*) => {
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
        if let Some(tk) = 'o: {
            Some(match $c {
                $($single => $single_tk,)*
                _ => break 'o None,
            })
        } {
            $tokens.push((tk, $loc));
            $src.pop_front();
            continue;
        };
    };
}

pub type Loc = (usize, usize);
#[derive(Debug, PartialEq)]
pub enum LexError {
    FirstLineIndent,
    InvalidToken(char, Loc),
    InconsitentIndent(usize, usize),
    CharAfterContinuation(usize),
}

pub type TokenLoc = (Token, (usize, usize));

pub fn lex(source: String) -> Result<VecDeque<TokenLoc>, LexError> {
    let source = source.trim_end();
    if source.len() <= 0 {
        return Ok(VecDeque::new());
    }

    let mut tokens = Vec::<TokenLoc>::new();

    let mut indent_stack = vec![0];
    let mut indent_char = String::new();

    'lines: for (num, line) in source.lines().enumerate() {
        let mut src: VecDeque<(usize, char)> = line.char_indices().collect();

        // detech indent before lexing line
        if src.len() > 0 {
            let c = src[0].1;

            let mut curr_indent = String::new();
            if c.is_whitespace() {
                curr_indent.push(src.pop_front().unwrap().1);
                while src.len() > 0 && src[0].1 == c {
                    curr_indent.push(src.pop_front().unwrap().1)
                }
            }
            // first time detection set the indent for the rest of the file
            if indent_char.is_empty() {
                // idk how to fix this imma be honest
                if !curr_indent.is_empty() {
                    indent_char = curr_indent.clone();
                    indent_stack.push(1); // assume that whatever the first indent is, is 1 level in
                    tokens.push((Token::IND, (num, 0)))
                }
            } else {
                let mut indent_count = 0;
                let indent_len = curr_indent.len();

                // fast check for error
                if indent_len % indent_char.len() != 0 {
                    return Err(LexError::InconsitentIndent(num, indent_len));
                }

                // drain them to count the token
                while curr_indent.starts_with(&indent_char) {
                    indent_count += 1;
                    curr_indent.drain(..indent_char.len());
                }

                // extra check if it not the same type
                if !curr_indent.is_empty() {
                    return Err(LexError::InconsitentIndent(num, indent_len));
                }

                match indent_count.cmp(indent_stack.last().unwrap()) {
                    std::cmp::Ordering::Less => {
                        // remove the EOL token, put in DED token then add EOL back
                        let loc = tokens.pop().unwrap().1;
                        for _ in 0..(indent_stack.pop().unwrap() - indent_count) {
                            tokens.push((Token::DED, loc))
                        }
                        tokens.push((Token::EOL, loc))
                    }

                    std::cmp::Ordering::Equal => (),
                    std::cmp::Ordering::Greater => {
                        if indent_count != indent_stack.last().unwrap() + 1 {
                            return Err(LexError::InconsitentIndent(num, indent_len));
                        }
                        tokens.push((Token::IND, (num, 0)));
                        indent_stack.push(indent_count);
                    }
                }
            }
        }

        while src.len() > 0 {
            // get some important stuff
            let loc = (num, src[0].0);
            let c = src[0].1;

            if c == '\\' {
                match src.len() > 1 {
                    true => return Err(LexError::CharAfterContinuation(num)),
                    false => continue 'lines,
                }
            }

            // skipping time
            if c.is_whitespace() {
                src.pop_front();
                continue;
            }

            // check for symbol token
            symbol_token!(
                src, c, tokens, loc,

                "==" =>Token::Equality,
                ">=" => Token::GreaterEq,
                "<=" => Token::LesserEq,
                "**" => Token::DoubleStar,

                "=>" => Token::Arrow

                ;-----;

                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '%' => Token::Percent,
                '=' => Token::Equal,
                ':' => Token::Colon,
                '>' => Token::Greater,
                '<' => Token::Lesser,
                '(' => Token::OpenParen,
                ')' => Token::CloseParen,
                '.' => Token::Dot
            );

            // if it not a special character then i have to be number or alpha
            if !c.is_alphanumeric() {
                return Err(LexError::InvalidToken(c, loc)); // lexer don't know what this is
            }

            // constructing number and alphabet time
            let is_alpha = c.is_alphabetic();
            let cond: fn(&(usize, char)) -> bool = match is_alpha {
                true => |c| c.1.is_alphabetic(),
                false => |c| c.1.is_numeric() || c.1 == '_' || c.1 == '.',
            };

            let acc = {
                let mut t = String::new();
                let mut decimal_counter = 0;
                loop {
                    match src.get(0).is_some_and(cond) {
                        true => {
                            let c = src[0];
                            if !is_alpha && c.1 == '.' {
                                decimal_counter += 1
                            }
                            if decimal_counter > 1 {
                                break t;
                            }
                            t.push(src.pop_front().unwrap().1);
                        }
                        false => break t,
                    }
                }
            };

            println!("{acc:?}");
            match is_alpha {
                true => tokens.push((
                    match acc.as_str() {
                        "var" => Token::Var,
                        "if" => Token::If,
                        "when" => Token::When,

                        "and" => Token::And,
                        "or" => Token::Or,
                        "not" => Token::Not,

                        "card" => Token::ReserveIden(Iden::Card),
                        "friendly" => Token::ReserveIden(Iden::Friendly),
                        "fight" => Token::ReserveIden(Iden::Fight),
                        "slots" => Token::ReserveIden(Iden::Slots),
                        "true" => Token::ReserveIden(Iden::True),
                        "false" => Token::ReserveIden(Iden::False),

                        _ => Token::Iden(acc),
                    },
                    loc,
                )),
                false => tokens.push((Token::Num(acc.replace('_', "").parse().unwrap()), loc)),
            }
        }

        tokens.push((Token::EOL, (num, line.len())))
    }

    let t = tokens.pop().unwrap().1;
    tokens.push((Token::EOF, t));

    if matches!(tokens[0].0, Token::IND) {
        return Err(LexError::FirstLineIndent);
    }

    return Ok(VecDeque::from(tokens));
}

#[cfg(test)]
mod test {
    use crate::lexer::{lex, LexError::*, Token::*};

    fn num(num: usize) -> crate::lexer::Token {
        Num(num as f32)
    }

    fn iden(name: &str) -> crate::lexer::Token {
        Iden(name.to_string())
    }

    // macro to help with writing test
    macro_rules! test {
        ($name:ident, $source:literal => [$($tk:expr , $line:literal:$col:literal);*]) => {
            #[test]
            fn $name() {
                assert_eq!(lex($source.to_string()).ok().unwrap(), [$(($tk, ($line, $col)),)*])
            }
        };
    }

    macro_rules! should_error {
        ($name:ident, $source:literal => $error:expr) => {
            #[test]
            fn $name() {
                assert_eq!(lex($source.to_string()).unwrap_err(), $error)
            }
        };
    }

    test!(simple, "1 + 1" => [num(1),0:0; Plus,0:2; num(1),0:4; EOF,0:5]);
    test!(identifier, "thisIsAIdentifier" => [iden("thisIsAIdentifier"),0:0; EOF,0:17]);

    test!(float, "1.9" => [Num(1.9),0:0; EOF,0:3]);
    test!(float_dot, "1.9." => [Num(1.9),0:0; Dot,0:3; EOF,0:4]);

    test!(keyword, "when" => [When,0:0; EOF,0:4]);
    test!(trailing_space, "1   " => [num(1),0:0; EOF,0:1]);

    test!(line_continuation, "1\\\n+1" => [num(1),0:0; Plus,1:0; num(1),1:1; EOF,1:2]);
    should_error!(error_line_continuation, "1\\ \n+1" => CharAfterContinuation(0));

    test!(multiline, "hello\n12" => [iden("hello"),0:0; EOL,0:5; num(12),1:0; EOF,1:2]);

    test!(multi_token, "== >= <= =>" => [Equality,0:0; GreaterEq,0:3; LesserEq,0:6; Arrow,0:9; EOF,0:11]);

    test!(indent_space, "1\n 1" => [num(1),0:0; EOL,0:1; IND,1:0; num(1),1:1; EOF,1:2]);
    test!(inden_tab, "1\n\t1" => [num(1),0:0; EOL,0:1; IND,1:0; num(1),1:1; EOF,1:2]);
    test!(indent_multispace, "1\n   1" => [num(1),0:0; EOL,0:1; IND,1:0; num(1),1:3; EOF,1:4]);
    test!(inden_multitab, "1\n\t\t\t1" => [num(1),0:0; EOL,0:1; IND,1:0; num(1),1:3; EOF,1:4]);

    test!(indent_multiline_space, "1\n 1\n  1" => [
                 num(1),0:0; EOL,0:1;
        IND,1:0; num(1),1:1; EOL,1:2;
        IND,2:0; num(1),2:2; EOF,2:3
    ]);
    test!(indent_multiline_tab, "1\n\t1\n\t\t1" => [
                 num(1),0:0; EOL,0:1;
        IND,1:0; num(1),1:1; EOL,1:2;
        IND,2:0; num(1),2:2; EOF,2:3
    ]);
    test!(indent_multiline_multispace, "1\n  1\n    1" => [
                 num(1),0:0; EOL,0:1;
        IND,1:0; num(1),1:2; EOL,1:3;
        IND,2:0; num(1),2:4; EOF,2:5
    ]);
    test!(indent_multiline_multitab, "1\n\t\t1\n\t\t\t\t1" => [
                 num(1),0:0; EOL,0:1;
        IND,1:0; num(1),1:2; EOL,1:3;
        IND,2:0; num(1),2:4; EOF,2:5
    ]);

    test!(indent_cascade_space, "1\n 1\n  1\n 1\n1" => [
                 num(1),0:0;          EOL,0:1;
        IND,1:0; num(1),1:1;          EOL,1:2;
        IND,2:0; num(1),2:2; DED,2:3; EOL,2:3;
                 num(1),3:1; DED,3:2; EOL,3:2;
                 num(1),4:0;          EOF,4:1
    ]);
    test!(indent_cascade_multispace, "1\n  1\n    1\n  1\n1" => [
                 num(1),0:0;          EOL,0:1;
        IND,1:0; num(1),1:2;          EOL,1:3;
        IND,2:0; num(1),2:4; DED,2:5; EOL,2:5;
                 num(1),3:2; DED,3:3; EOL,3:3;
                 num(1),4:0;          EOF,4:1
    ]);
    test!(indent_cascade_tab, "1\n\t1\n\t\t1\n\t1\n1" => [
                 num(1),0:0;          EOL,0:1;
        IND,1:0; num(1),1:1;          EOL,1:2;
        IND,2:0; num(1),2:2; DED,2:3; EOL,2:3;
                 num(1),3:1; DED,3:2; EOL,3:2;
                 num(1),4:0;          EOF,4:1
    ]);
    test!(indent_cascade_multitab, "1\n\t\t1\n\t\t\t\t1\n\t\t1\n1" => [
                 num(1),0:0;          EOL,0:1;
        IND,1:0; num(1),1:2;          EOL,1:3;
        IND,2:0; num(1),2:4; DED,2:5; EOL,2:5;
                 num(1),3:2; DED,3:3; EOL,3:3;
                 num(1),4:0;          EOF,4:1
    ]);

    test!(indent_space_jump, "1\n 1\n  1\n1" => [
                 num(1),0:0;                   EOL,0:1;
        IND,1:0; num(1),1:1;                   EOL,1:2;
        IND,2:0; num(1),2:2; DED,2:3; DED,2:3; EOL,2:3;
                 num(1),3:0;                   EOF,3:1
    ]);
    test!(indent_multispace_jump, "1\n  1\n    1\n1" => [
                 num(1),0:0;                   EOL,0:1;
        IND,1:0; num(1),1:2;                   EOL,1:3;
        IND,2:0; num(1),2:4; DED,2:5; DED,2:5; EOL,2:5;
                 num(1),3:0;                   EOF,3:1
    ]);

    should_error!(error_indent_wrong_type, "  1\n\t1" => InconsitentIndent(1, 1));
    should_error!(error_indent_inconsitent, "  1\n   1" => InconsitentIndent(1, 3));
    should_error!(error_indent_first, "  1" => FirstLineIndent);

    test!(space, "   " => []);
    test!(empty, "" => []);
}
