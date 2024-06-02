mod lexer;
mod parser;
mod trans;

use lexer::tokenize;
use parser::Parser;

use crate::trans::transpile;

fn main() {
    let input = "1*8".to_string();

    println!("Input:\n{}", input);

    let tokens = tokenize(input).unwrap();
    let ast = (Parser { tokens }).gen_ast();

    println!("Raw Call:\n{}", ast[0]);
    println!("Transpile:\n{}", transpile(ast))

    // println!("{:?}", tokenize("1 + 1".to_string()).unwrap())
}
