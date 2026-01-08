use std::{
    cell::{Ref, RefCell},
    ops::Deref,
    rc::Rc,
};

use downcast_rs::Downcast;
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::{
    ast::{ASTNode, stmt::Block, types::EType},
    rc, rc_ref,
};

pub trait Decl: ASTNode {}
serialize_trait_object!(Decl);

#[derive(Serialize)]
pub struct VarDecl {
    /*
    TODO: stack information needs to go here
    */
    pub ty: Rc<RefCell<EType>>,
    pub name: Rc<String>,
}

#[derive(Serialize)]
pub struct StructTypeDecl {
    pub ty: Rc<RefCell<EType>>,
    fields: Vec<Rc<RefCell<VarDecl>>>,
}

#[derive(Serialize)]
pub struct FunDecl {
    pub ty: Rc<RefCell<EType>>,
    pub name: Rc<String>,
    params: Vec<Rc<RefCell<VarDecl>>>,
}

#[derive(Serialize)]
pub struct FunDefn {
    decl: FunDecl,
    block: Rc<RefCell<Block>>,
}

impl ASTNode for VarDecl {
    fn type_name(&self) -> &'static str {
        "VarDecl"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        vec![self.ty.borrow()]
    }
}

impl Decl for VarDecl {}

impl VarDecl {
    pub fn new(ty: EType, name: String) -> Self {
        Self {
            ty: rc_ref!(ty),
            name: rc!(name),
        }
    }
}

impl ASTNode for StructTypeDecl {
    fn type_name(&self) -> &'static str {
        "StructTypeDecl"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        let mut v: Vec<Ref<dyn ASTNode>> = vec![];
        for field in &self.fields {
            v.push(field.borrow());
        }
        v
    }
}

impl Decl for StructTypeDecl {}

impl StructTypeDecl {
    pub fn new(ty: EType) -> Self {
        Self {
            ty: rc_ref!(ty),
            fields: vec![],
        }
    }

    pub fn get_field(&self, name: String) -> Option<Ref<VarDecl>> {
        self.fields
            .iter()
            .find(|&field| field.borrow().name.as_str() == name)
            .map(|f| f.borrow())
    }

    pub fn add_var_decl(&mut self, vd: VarDecl) {
        self.fields.push(rc_ref!(vd));
    }
}

impl ASTNode for FunDecl {
    fn type_name(&self) -> &'static str {
        "FunDecl"
    }

    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        let mut children: Vec<Ref<dyn ASTNode>> = vec![];
        children.push(self.ty.borrow());
        for param in &self.params {
            children.push(param.borrow());
        }
        children
    }
}

impl Decl for FunDecl {}

impl FunDecl {
    pub fn new(ty: EType, name: String, params: Vec<Rc<RefCell<VarDecl>>>) -> Self {
        Self {
            ty: rc_ref!(ty),
            name: rc!(name),
            params,
        }
    }
}

impl ASTNode for FunDefn {
    fn type_name(&self) -> &'static str {
        "FunDefn"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        let mut children: Vec<Ref<dyn ASTNode>> = vec![];
        for child in self.decl.children() {
            children.push(child);
        }
        children.push(self.block.borrow());
        children
    }
}

impl Decl for FunDefn {}

impl FunDefn {
    pub fn new(decl: FunDecl, block: Block) -> Self {
        Self {
            decl,
            block: rc_ref!(block),
        }
    }
}
