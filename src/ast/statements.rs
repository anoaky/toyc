//! Constructs for encoding statements.
use serde::Serialize;

use crate::{
    ast::{
        exprs::Expr,
        types::{Ident, Ty},
    },
    util::NodeId,
};

/// Encodes a block.
#[derive(Clone, Serialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

/// Encodes a stack-allocated variable declaration.
#[derive(Clone, Serialize)]
pub struct Local {
    pub name: Ident,
    pub ty: Ty,
    pub value: Option<Expr>,
}

/// Encodes information for each [`Stmt`]
#[derive(Clone, Serialize)]
pub enum StmtKind {
    Block(Block),
    Local(Local),
    While(Expr, Box<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Expr(Expr),
    Continue,
    Break,
}

/// Encodes a single statement.
#[derive(Clone, Serialize)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
}

impl From<StmtKind> for Stmt {
    fn from(value: StmtKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: value,
        }
    }
}

impl From<Vec<Stmt>> for Block {
    fn from(value: Vec<Stmt>) -> Self {
        Self { stmts: value }
    }
}

impl From<Stmt> for Block {
    fn from(value: Stmt) -> Self {
        Self { stmts: vec![value] }
    }
}

impl From<(Ident, Ty, Option<Expr>)> for Local {
    fn from((name, ty, value): (Ident, Ty, Option<Expr>)) -> Self {
        Self { name, ty, value }
    }
}
