use crate::parser::Node;

pub const TABCHAR: &str = "\t";

pub fn trans(program: Vec<Node>) -> String {
    let mut out = String::from("extends SigilEffect\nfunc handle_event(event, params):\n");
    if program.len() <= 0 {
        return out + TABCHAR + "pass";
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
    return out.trim().to_string();
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
                    .trim()
                )
            }
        };
    }

    test!(simple, "1" => "1");
    test!(string, "\"hello\"" => "\"hello\"");
    test!(bin, "1+1" => "2");
    test!(multiline, "1\n2" => "1\n2");
    test!(paren, "1*(1+a)" => "1 * (1 + a)");
    test!(paren_top, "(1 + a)" => "1 + a");

    test!(empty, "" => "pass");

    test!(indent_multi_dedent, "if true:\n\tif true:\n\t\thello\nhello" => "if true:\n\tif true:\n\t\thello\nhello");

    test!(var, "var a = 10" => "var a_0 = 10");

    test!(if_stmt, "if 1 + 1:\n\thello" => "if 2:\n\thello");
    test!(if_stmt_single, "if true: hello" => "if true:\n\thello");
    test!(if_stmt_else, "if 1:\n\t1\nelse:\n\t1" => "if 1:\n\t1\nelse:\n\t1");
    test!(if_stmt_nest, "if true: if true: hello" => "if true:\n\tif true:\n\t\thello");
    test!(if_stmt_elif, "if 1:\n\t1\nelif 1:\n\t1" => "if 1:\n\t1\nelif 1:\n\t1");
    test!(if_stmt_elif_else, "if 1:\n\t1\nelif 1:\n\t1\nelse:\n\t1" => "if 1:\n\t1\nelif 1:\n\t1\nelse:\n\t1");
    test!(if_stmt_elif_chain, "if 1:\n\t1\nelif 1:\n\t1\nelif 1:\n\t1" => "if 1:\n\t1\nelif 1:\n\t1\nelif 1:\n\t1");
    test!(if_stmt_elif_chain_else, "if 1:\n\t1\nelif 1:\n\t1\nelif 1:\n\t1\nelse:\n\t1" => "if 1:\n\t1\nelif 1:\n\t1\nelif 1:\n\t1\nelse:\n\t1");

    test!(event, "when summon: 1" => "if event == \"card_summoned\" and params[0] == card:\n\t1");

    test!(assign, "a = 1" => "a_0 = 1");

    test!(call, "a()" => "a()");
    test!(call_arg, "a(1)" => "a(1)");
    test!(call_args, "a(1, 2)" => "a(1, 2)");
    test!(call_chain, "a()()" => "a()()");
    test!(call_chain_args, "a(1, 2)(1, 2)" => "a(1, 2)(1, 2)");
}
