mod lexer;
mod parser;
mod trans;

use std::fmt::Display;

use lexer::lex;

use self::lexer::LexError;
use self::parser::{ParseError, Parser};
use self::trans::trans;

fn snippet(source: &String, loc: (usize, usize)) -> String {
    format!(
        "{line} | {content}\n{loc}",
        line = loc.0 + 1,
        content = source.lines().collect::<Vec<&str>>()[loc.0],
        loc = " ".repeat((loc.0 + 1).checked_ilog10().unwrap_or(0) as usize + loc.1 + 4)
            + "\x1b[1;31m─\x1b[0m"
    )
}

fn long_snippet(source: &String, loc: (usize, usize), len: usize) -> String {
    format!(
        "{line} | {content}\n{loc}",
        line = loc.0 + 1,
        content = source.lines().collect::<Vec<&str>>()[loc.0],
        loc = " ".repeat((loc.0 + 1).checked_ilog10().unwrap_or(0) as usize + loc.1 + 4)
            + "\x1b[1;31m"
            + &"─".repeat(len)
            + "\x1b[0m"
    )
}

pub enum TranspileError {
    LexError { source: String, err: LexError },
    ParseError { source: String, err: ParseError },
}

impl Display for TranspileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranspileError::LexError {
                source,
                err: (token, loc),
            } => write!(
                f,
                "\x1b[1;31mError\x1b[0m: Unknown Token `{}` at {}:{}\n{}",
                token,
                loc.0 + 1,
                loc.1 + 1,
                snippet(source, *loc)
            ),
            TranspileError::ParseError { source, err } => match err {
                ParseError::UnexpectedToken {
                    get: token,
                    loc,
                    len,
                } => {
                    write!(
                        f,
                        "\x1b[1;31mError\x1b[0m: Unexpected Token `{}` at {}:{}\n{}",
                        token,
                        loc.0 + 1,
                        loc.1 + 1,
                        long_snippet(source, *loc, *len)
                    )
                }
                ParseError::ExpectToken {
                    get,
                    loc,
                    want,
                    len,
                } => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Expect {}, found `{}` at {}:{}\n{}",
                    if want.len() == 1 {
                        format!("`{}`", want[0].to_string())
                    } else {
                        format!(
                            "one of {}",
                            want.iter()
                                .map(|tk| format!("`{}`", tk.to_string()))
                                .collect::<Vec<String>>()
                                .join(",")
                        )
                    },
                    get,
                    loc.0 + 1,
                    loc.1 + 1,
                    long_snippet(source, *loc, *len)
                ),
            },
        }
    }
}
pub fn transpile(source: String) -> Result<String, TranspileError> {
    let tokens = match lex(source.clone()) {
        Ok(token) => token,
        Err(err) => {
            return Err(TranspileError::LexError { source, err });
        }
    };

    let ast = match (Parser { tokens }.gen_ast()) {
        Ok(ast) => ast,
        Err(err) => {
            return Err(TranspileError::ParseError { source, err });
        }
    };

    Ok(trans(ast))
}
