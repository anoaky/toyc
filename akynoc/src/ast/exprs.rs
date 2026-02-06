//! Constructs for encoding expressions
use std::fmt::Display;

use internment::Intern;
use serde::Serialize;

use crate::{
    ast::types::{Ident, Ty, TyKind},
    lexer::Token,
    util::NodeId,
};

/// Encodes a single value.
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

/// Encodes an interned [`Value`].
///
/// This construct exists to simplify the construction of interned literals.
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

/// Encodes the valid unary and binary operators.
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

/// Encodes a function call.
#[derive(Clone, Serialize)]
pub struct CallFn {
    /// This must always have kind [`ExprKind::Ident`].
    /// Implemented as an Expr to allow for easy Pratt parsing of function calls
    /// as a postfix operator.
    pub name: Box<Expr>,
    pub args: Vec<Expr>,
}

/// Encodes the kind of an [`Expr`].
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

/// Encodes a single expression in the language.
#[derive(Clone, Serialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    /// This field will only have meaningful value after type analysis.
    /// It will be [`TyKind::Infer`] for all expressions produced by the parser.
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
            Self::CallFn(call) => write!(f, "({call})"),
            Self::Typecast(cast_to, expr) => write!(f, "(({cast_to}) {expr})"),
            Self::Ref(expr) => write!(f, "(&{expr})"),
            Self::Deref(expr) => write!(f, "(*{expr})"),
            Self::Index(arr, ind) => write!(f, "({arr}[{ind}])"),
            Self::FieldAccess(str, field) => write!(f, "({str}.{field})"),
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

impl<'a> From<Token<'a>> for Operator {
    fn from(value: Token) -> Self {
        match value {
            Token::LogOr => Self::Or,
            Token::LogAnd => Self::And,
            Token::Eq => Self::Eq,
            Token::Ne => Self::Ne,
            Token::Lt => Self::Lt,
            Token::Gt => Self::Gt,
            Token::Le => Self::Le,
            Token::Plus => Self::Add,
            Token::Minus => Self::Minus,
            Token::Asterisk => Self::Times,
            Token::Div => Self::Div,
            Token::Rem => Self::Mod,
            _ => panic!("Attempt to convert invalid token {value} to Operator"),
        }
    }
}

impl Display for CallFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.name,
            self.args
                .iter()
                .map(Expr::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl From<(Expr, Vec<Expr>)> for CallFn {
    fn from((name, args): (Expr, Vec<Expr>)) -> Self {
        Self {
            name: Box::new(name),
            args,
        }
    }
}
