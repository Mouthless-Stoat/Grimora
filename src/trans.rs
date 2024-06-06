use crate::parser::Node;

pub const TABCHAR: &str = "\t";

pub fn trans(program: Vec<Node>) -> String {
    let mut out = String::from("extends SigilEffect\nfunc handle_event(event, params):\n");
    if program.len() <= 0 {
        return out + "\tpass";
    }
    for node in program {
        out.push_str(
            &node
                .to_string()
                .lines()
                .map(|l| format!("{TABCHAR}{l}\n"))
                .collect::<String>(),
        )
    }
    return out;
}

#[cfg(test)]
mod test {
    use crate::lexer::lex;
    use crate::parser::Parser;
    use crate::trans::{trans, TABCHAR};

    macro_rules! test {
        ($name:ident, $source:literal => $output:literal) => {
            #[test]
            fn $name() {
                assert_eq!(
                    trans(
                        Parser {
                            tokens: lex($source.to_string()).ok().unwrap()
                        }
                        .gen_ast()
                        .unwrap()
                    ),
                    format!(
                        "extends SigilEffect\nfunc handle_event(event, params):\n{}",
                        $output
                            .lines()
                            .map(|l| format!("{TABCHAR}{l}\n"))
                            .collect::<String>()
                    )
                )
            }
        };
    }

    test!(simple, "1" => "1");
    test!(bin, "1+1" => "2");

    test!(var, "var a = 10" => "var a_0 = 10");

    test!(multiline, "1\n2" => "1\n2");
    test!(if_stmt, "if 1 + 1:\n\thello" => "if 2:\n\thello");
}
