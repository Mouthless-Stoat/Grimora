mod lexer;
mod parser;
mod trans;

use lexer::tokenize;
use parser::Parser;

fn main() {
    let ast = (Parser {
        tokens: tokenize("1+110 * 102 + 19".to_string()).unwrap(),
    })
    .gen_ast();

    println!("{}", ast[0]);
}
