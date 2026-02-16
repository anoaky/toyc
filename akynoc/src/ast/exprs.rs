use std::fmt::Display;

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
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Clone, Serialize)]
pub struct FnSig {
    pub name: Ident,
    pub params: Vec<FnParam>,
    pub ty: Ty,
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
    Tuple(Vec<Expr>),
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExprKind::Literal(l) => write!(f, "{}", l),
            ExprKind::Let(pat, expr) => write!(f, "let {} = {}", pat, *expr),
            ExprKind::Block(exprs) => write!(f, "{{{}}}", exprs.iter().map(Expr::to_string).collect::<Vec<String>>().join("\n")),
            ExprKind::Pattern(pat) => write!(f, "{}", pat),
            ExprKind::Tuple(exprs) => write!(f, "({})", exprs.iter().map(Expr::to_string).collect::<Vec<String>>().join(", ")),
            _ => unimplemented!(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::String(s) => write!(f, "\"{}\"", s),
        }
    }
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

impl From<char> for Expr {
    fn from(value: char) -> Self {
        ExprKind::Literal(Literal::Char(value)).into()
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::LogAnd => "&&",
            Self::LogOr => "||",
        };
        write!(f, "{}", s)
    }
}
