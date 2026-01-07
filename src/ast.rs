pub mod decl;
pub mod types;

pub trait ASTNode {
    fn children(&self) -> Vec<&dyn ASTNode>;
}
