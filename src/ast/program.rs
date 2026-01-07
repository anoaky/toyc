use crate::ast::{ASTNode, decl::Decl};

pub struct Program {
    decls: Vec<Box<dyn Decl>>,
}

impl ASTNode for Program {
    fn type_name(&self) -> &'static str {
        "Program"
    }
    fn children(&self) -> Vec<&dyn ASTNode> {
        let mut children = vec![];
        for decl in &self.decls {
            children.push(decl.as_ref() as &dyn ASTNode);
        }
        children
    }
}

impl Program {
    pub fn new(decls: Vec<Box<dyn Decl>>) -> Self {
        Self { decls }
    }
}
