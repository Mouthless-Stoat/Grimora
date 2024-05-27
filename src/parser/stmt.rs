use crate::trans::Transpilable;

#[derive(Debug, PartialEq)]
pub enum Stmt {}
impl Transpilable for Stmt {
    fn transpile(self) -> String {
        todo!()
    }
}
