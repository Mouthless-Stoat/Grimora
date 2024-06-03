use crate::parser::Node;

pub fn trans(program: Vec<Node>) -> String {
    let mut out = String::new();
    for node in program {
        out.push_str(&format!("{}", node))
    }
    return out;
}

#[cfg(test)]
mod test {}
