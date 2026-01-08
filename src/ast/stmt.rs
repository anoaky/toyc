use std::cell::Ref;
use std::{cell::RefCell, rc::Rc};

use downcast_rs::impl_downcast;
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::ast::{ASTNode, decl::VarDecl};

pub trait Statement: ASTNode {}
impl_downcast!(Statement);
serialize_trait_object!(Statement);

#[derive(Serialize)]
pub struct Block {
    pub vds: Vec<Rc<RefCell<VarDecl>>>,
    pub stmts: Vec<Rc<RefCell<dyn Statement>>>,
}

impl ASTNode for Block {
    fn type_name(&self) -> &'static str {
        "Block"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        let mut children: Vec<Ref<dyn ASTNode>> = vec![];
        for vd in &self.vds {
            children.push(vd.borrow());
        }
        for stmt in &self.stmts {
            children.push(stmt.borrow());
        }

        children
    }
}

impl Block {
    pub fn new(vds: Vec<Rc<RefCell<VarDecl>>>, stmts: Vec<Rc<RefCell<dyn Statement>>>) -> Self {
        Self { vds, stmts }
    }
}
