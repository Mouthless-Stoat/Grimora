use lang::transpile;

fn main() {
    loop {
        // let stdin = std::io::stdin();
        // let mut stdout = std::io::stdout();

        // let mut buf = String::new();
        // print!("> ");
        // stdout.flush().expect("Cannot flush output");
        // stdin.read_line(&mut buf).expect("Cannot get input");
        // buf = buf.trim().to_owned();

        let buf = "when summon: var a = 1 + 1".to_string();

        println!("{t} INPUT {t}", t = "=".repeat(20));
        println!("{buf}");
        println!("{t} OUTPUT {r}", t = "=".repeat(20), r = "=".repeat(19));
        println!(
            "{}",
            match transpile(buf) {
                Ok(res) => res,
                Err(err) => err.to_string(),
            }
        );
        println!("{}", "=".repeat(47));

        break;
    }
}
