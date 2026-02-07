use internment::Intern;
use serde::Serialize;

use crate::{
    ast::{
        patterns::{Ident, Pattern},
        types::{Ty, TyKind},
    },
    util::NodeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub enum Literal {
    Int(u32),
    Char(char),
    String(Intern<String>),
}

#[derive(Debug, Clone, Serialize)]
pub struct FnParam {
    name: Ident,
    ty: Ty,
}

#[derive(Debug, Clone, Serialize)]
pub struct FnSig {
    name: Ident,
    params: Vec<FnParam>,
    ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Div,
    Rem,
    LogAnd,
    LogOr,
}

#[derive(Debug, Clone, Serialize)]
pub enum ExprKind {
    Literal(Literal),
    Let(Pattern, Box<Expr>),
    Block(Vec<Expr>),
    Pattern(Pattern),
    Fn(FnSig, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    While(Box<Expr>, Box<Expr>),
    Call(Ident, Vec<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    BinOp(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, Clone, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub ty: Ty,
}

impl From<ExprKind> for Expr {
    fn from(value: ExprKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: value,
            ty: TyKind::Infer.into(),
        }
    }
}

impl From<Literal> for Expr {
    fn from(value: Literal) -> Self {
        ExprKind::Literal(value).into()
    }
}
