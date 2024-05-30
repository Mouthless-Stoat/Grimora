mod lexer;
mod parser;
mod trans;

use lexer::tokenize;
use parser::Parser;

use crate::trans::transpile;

fn main() {
    println!(
        "{}",
        transpile(
            (Parser {
                tokens: tokenize("1+110 + 102 + 19".to_string()).unwrap()
            })
            .gen_ast()
        ),
    );
}
