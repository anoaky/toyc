use std::fmt::Display;

use internment::Intern;
use serde::Serialize;

use crate::{
    ast::types::{Ident, Ty},
    util::NodeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub enum Value {
    Invalid,
    Int(u32),
    Char(char),
    Str(Intern<String>),
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Self::Int(value)
    }
}

impl From<char> for Value {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::Str(Intern::new(value))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::Str(Intern::from_ref(value))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct Literal {
    pub value: Intern<Value>,
}

impl From<u32> for Literal {
    fn from(value: u32) -> Self {
        Self {
            value: Into::<Value>::into(value).into(),
        }
    }
}

impl From<char> for Literal {
    fn from(value: char) -> Self {
        Self {
            value: Into::<Value>::into(value).into(),
        }
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Self {
            value: Into::<Value>::into(value).into(),
        }
    }
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Self {
            value: Into::<Value>::into(value).into(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Operator {
    Add,
    Minus,
    Times,
    Div,
    Mod,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne,
    Or,
    And,
}

#[derive(Clone, Serialize)]
pub struct CallFn {
    pub name: Ident,
    pub args: Vec<Expr>,
}

#[derive(Clone, Serialize)]
pub enum ExprKind {
    Invalid,
    Literal(Literal),
    Ident(Ident),
    Assign(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    CallFn(CallFn),
    Typecast(Ty, Box<Expr>),
    Deref(Box<Expr>),
    Ref(Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, Ident),
}

#[derive(Clone, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
}

impl From<ExprKind> for Expr {
    fn from(value: ExprKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: value,
        }
    }
}

impl From<Literal> for Expr {
    fn from(value: Literal) -> Self {
        ExprKind::Literal(value).into()
    }
}

impl From<Ident> for Expr {
    fn from(value: Ident) -> Self {
        ExprKind::Ident(value).into()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Invalid => write!(f, "INVALID"),
            Self::Literal(i) => write!(f, "{i}"),
            Self::Ident(id) => write!(f, "{id}"),
            Self::Assign(lhs, rhs) => write!(f, "({lhs} = {rhs})"),
            Self::BinOp(lhs, op, rhs) => write!(f, "({lhs} {op} {rhs})"),
            _ => todo!(),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Add => "+",
            Self::Minus => "-",
            Self::Times => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Gt => ">",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Ge => ">=",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Or => "||",
            Self::And => "&&",
        };
        write!(f, "{s}")
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Invalid => write!(f, "INVALID"),
            &Value::Int(i) => write!(f, "{i}"),
            &Value::Char(c) => write!(f, "'{c}'"),
            &Value::Str(s) => write!(f, "\"{s}\""),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}
