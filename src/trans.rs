use crate::parser::Node;

pub fn trans(program: Vec<Node>) -> String {
    let mut out = String::from("extends SigilEffect\nfunc handle_event(event, params:\n");
    if program.len() <= 0 {
        return out + "\tpass";
    }
    for node in program {
        out.push_str(
            &(format!("{}", node)
                .lines()
                .map(|l| format!("\t{}", l))
                .collect::<String>()),
        )
    }
    return out;
}

#[cfg(test)]
mod test {}
