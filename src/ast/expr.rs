use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

use downcast_rs::impl_downcast;
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::{
    ast::{ASTNode, decl::VarDecl, types::EType},
    rc_ref,
};

pub trait Expr: ASTNode {}
impl_downcast!(Expr);
serialize_trait_object!(Expr);

#[derive(Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Op {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    GT,
    GE,
    LT,
    LE,
    EQ,
    NE,
    AND,
    OR,
}

#[derive(Serialize)]
pub struct BinOp {
    pub ty: EType,
    lhs: Rc<RefCell<dyn Expr>>,
    op: Rc<RefCell<Op>>,
    rhs: Rc<RefCell<dyn Expr>>,
}

#[derive(Serialize)]
pub struct VarExpr {
    name: String,
    decl: Rc<VarDecl>,
}

impl ASTNode for Op {
    fn type_name(&self) -> &'static str {
        "Op"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        vec![]
    }
}

impl ASTNode for BinOp {
    fn type_name(&self) -> &'static str {
        "BinOp"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        vec![self.lhs.borrow(), self.op.borrow(), self.rhs.borrow()]
    }
}

impl ASTNode for VarExpr {
    fn type_name(&self) -> &'static str {
        "VarExpr"
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        vec![]
    }
}
