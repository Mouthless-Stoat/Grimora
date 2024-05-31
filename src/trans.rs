use crate::parser::Node;

pub fn transpile(program: Vec<Node>) -> String {
    let mut out = String::new();
    for node in program {
        out.push_str(&format!("{}", node))
    }
    return out;
}
