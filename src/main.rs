mod lexer;

fn main() {
    println!("{:?}", lexer::tokenize(String::from("1 + 1")));
}
