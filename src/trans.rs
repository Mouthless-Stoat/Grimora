use crate::parser::Node;

pub trait Transpilable {
    fn transpile(self) -> String;
}

pub fn transpile(program: Vec<Node>) -> String {
    let mut out = String::new();
    for node in program {
        out.push_str(&match node {
            Node::Expr(expr) => expr.transpile(),
            Node::Stmt(stmt) => stmt.transpile(),
        })
    }
    return out;
}
