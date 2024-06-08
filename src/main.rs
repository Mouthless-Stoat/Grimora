use lang::transpile;

fn main() {
    println!("extends SigilEffect\nfunc handle_event(event, params):\n\tif true:\n\t\tif true:\n\t\thello");
    loop {
        // let stdin = std::io::stdin();
        // let mut stdout = std::io::stdout();

        // let mut buf = String::new();
        // print!("> ");
        // stdout.flush().expect("Cannot flush output");
        // stdin.read_line(&mut buf).expect("Cannot get input");
        // buf = buf.trim().to_owned();

        let buf = "1\\\n+1".to_string();

        println!("{}", "=".repeat(40));
        println!(
            "{}",
            match transpile(buf) {
                Ok(res) => res,
                Err(err) => err.to_string(),
            }
        );
        println!("{}", "=".repeat(40));

        break;
    }
}
