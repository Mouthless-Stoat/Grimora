mod lexer;
mod parser;
mod trans;

use std::fmt::Display;

use lexer::lex;

use self::lexer::LexError;
use self::parser::{ParseError, Parser};
use self::trans::{trans, TABCHAR};

fn snippet(source: &String, loc: (usize, usize)) -> String {
    long_snippet(source, loc, 1)
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
            TranspileError::LexError { source, err } => match err {
                LexError::InvalidToken(token, loc) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Unknown Token `{token}` at {line}:{col}\n{snippet}",
                    line = loc.0 + 1,
                    col = loc.1 + 1,
                    snippet = snippet(source, *loc)
                ),
                LexError::InconsitentIndent(line, len) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Inconsitent identation on line {line}\n{snippet}",
                    line = line + 1,
                    snippet = long_snippet(source, (*line, 0), *len)
                ),
                LexError::FirstLineIndent => {
                    write!(
                        f,
                        "\x1b[1;31mError\x1b[0m: Indent on the first line\n{snippet}",
                        snippet = snippet(source, (0, 0))
                    )
                }
            },
            TranspileError::ParseError { source, err } => match err {
                ParseError::UnexpectedToken { get, loc, len } => {
                    write!(
                        f,
                        "\x1b[1;31mError\x1b[0m: Unexpected Token `{get}` at {line}:{col}\n{snippet}",
                        line = loc.0 + 1,
                        col = loc.1 + 1,
                        snippet = long_snippet(source, *loc, *len)
                    )
                }
                ParseError::ExpectToken {
                    get,
                    loc,
                    want,
                    len,
                } => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Expect {}, found `{get}` at {}:{}\n{}",
                    if want.len() == 1 {
                        format!("`{}`", want[0].to_string())
                    } else {
                        format!(
                            "one of {}",
                            want.iter()
                                .map(|tk| format!("`{tk}`"))
                                .collect::<Vec<String>>()
                                .join(",")
                        )
                    },
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
