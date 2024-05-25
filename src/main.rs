#![feature(char_indices_offset)]
mod lexer;

fn main() {
    println!("{:?}", lexer::tokenize(String::from("==> ++ 112 ausyhds")));
}
