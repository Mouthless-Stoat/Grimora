mod lexer;
mod parser;
mod trans;

use lexer::tokenize;
use parser::Parser;

fn main() {
    let input = "1*4-9";
    let ast = (Parser {
        tokens: tokenize(input.to_string()).unwrap(),
    })
    .gen_ast();

    println!("{}", input);
    println!("{}", ast[0]);
}
