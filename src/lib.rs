mod lexer;
mod parser;
mod trans;

use std::fmt::Display;
use std::usize;

use lexer::lex;

use self::lexer::LexError;
use self::parser::{ParseError, Parser};
use self::trans::trans;

fn snippet(source: &String, loc: (usize, usize)) -> String {
    long_snippet(source, loc, 1, None)
}

fn long_snippet(source: &String, loc: (usize, usize), len: usize, prefix: Option<&str>) -> String {
    format!(
        "{line} | {content}\n{loc}",
        line = loc.0 + 1,
        content = source.lines().collect::<Vec<&str>>()[loc.0],
        loc = " ".repeat((loc.0 + 1).checked_ilog10().unwrap_or(0) as usize + loc.1 + 4)
            + prefix.unwrap_or("\x1b[1;31m")
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
                    snippet = long_snippet(source, (*line, 0), *len, None)
                ),
                LexError::FirstLineIndent => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Indent on the first line\n{snippet}",
                    snippet = snippet(source, (0, 0))
                ),
                LexError::CharAfterContinuation(line) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Character after line continuation on {line}\n{snippet}",
                    line = line +1,
                    snippet = snippet(source, (*line, 0))

                ),
                LexError::UnterminatedString(line) => write!( f, "\x1b[1;31mError\x1b[0m: Unterminated string on line {line}", line = line + 1),
                LexError::InvalidEscape(line, char) => write!(f, "\x1b[1;31mError\x1b[0m: Invalid escape character '{char}' on line {line}", line = line + 1)
            },
            TranspileError::ParseError { source, err } => match err {
                ParseError::UnexpectedToken(loc, len, get)=> {
                    write!(
                        f,
                        "\x1b[1;31mError\x1b[0m: Unexpected Token `{get}` at {line}:{col}\n{snippet}",
                        line = loc.0 + 1,
                        col = loc.1 + 1,
                        snippet = long_snippet(source, *loc, *len, None)
                    )
                }
                ParseError::ExpectToken(loc, len, get, want) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Expect {}, found `{get}` at {line}:{col}\n{snippet}",
                    if want.len() == 1 {
                        format!("`{}`", want[0])
                    } else {
                        format!(
                            "one of {}",
                            want.iter()
                                .map(|tk| format!("`{tk}`"))
                                .collect::<Vec<String>>()
                                .join(",")
                        )
                    },
                    line = loc.0 + 1,
                    col = loc.1 + 1,
                    snippet = long_snippet(source, *loc, *len, None)
                ),
                ParseError::InvalidEventIden(loc, len) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Invalid event target at {line}:{col}\n{snippet}",
                    line = loc.0 + 1,
                    col = loc.1 + 1,
                    snippet = long_snippet(source, *loc, *len, None)
                ),
                ParseError::InvalidEventType(loc, len) => write!(
                    f,
                    "\x1b[1;31mError\x1b[0m: Invalid event type at {line}:{col}\n{snippet}",
                    line = loc.0 + 1,
                    col = loc.1 + 1,
                    snippet = long_snippet(source, *loc, *len, None)
                ),
                ParseError::AttrIden(loc, len, expr, attr) => write!(f,
                    "\x1b[1;31mError\x1b[0m: Attribute expression can only use identifier at {line}:{col}\n{snippet}",
                    line = loc.0 + 1,
                    col = loc.1 + 1,
                    snippet = long_snippet(source, *loc, *len, Some("\x1b[1;33m")) + &format!("\x1b[1;33m-> Did you mean to use subscript instead? `{expr}[{attr}]`\x1b[0m")
                ),
                ParseError::InvalidNew(loc, len) => write!(f,
                    "\x1b[1;31mError\x1b[0m: Invalid usage of `new` at {line}:{col}\n{snippet}",
                    line = loc.0 + 1,
                    col = loc.0 + 1,
                    snippet = long_snippet(source, *loc, *len, None) + &format!("\x1b[1;31m Remove this `new`.\x1b[0m")

                )

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
