use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use serde::Serialize;

use crate::ast::{ASTNode, decl::Decl};

#[derive(Serialize)]
pub struct Program {
    decls: Vec<Rc<RefCell<dyn Decl>>>,
}

impl ASTNode for Program {
    fn type_name(&self) -> &'static str {
        "Program"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        let mut children: Vec<Ref<dyn ASTNode>> = vec![];
        for decl in &self.decls {
            children.push(decl.borrow());
        }
        children
    }
}

impl Program {
    pub fn new(decls: Vec<Rc<RefCell<dyn Decl>>>) -> Self {
        Self { decls }
    }
}
