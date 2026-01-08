use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::ast::{ASTNode, types::Type};

pub trait Decl: ASTNode {
    fn ty(&self) -> &dyn Type;
    fn name(&self) -> &str;
}
serialize_trait_object!(Decl);

#[derive(Serialize)]
pub struct VarDecl {
    /*
    TODO: stack information needs to go here
    */
    var_type: Box<dyn Type>,
    name: String,
}

impl ASTNode for VarDecl {
    fn type_name(&self) -> &'static str {
        "VarDecl"
    }
    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![self.var_type.as_ref()]
    }
}

impl Decl for VarDecl {
    fn ty(&self) -> &dyn Type {
        self.var_type.as_ref()
    }

    fn name(&self) -> &str {
        &self.name
    }
}

impl VarDecl {
    pub fn new(ty: Box<dyn Type>, name: String) -> Self {
        Self { var_type: ty, name }
    }
}
