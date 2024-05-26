mod lexer;
mod parser;

use lexer::tokenize;
use parser::Parser;

fn main() {
    println!(
        "{:?}",
        (Parser {
            tokens: tokenize("1".to_string()).unwrap()
        })
        .gen_ast(),
    );
}
