use std::io::Write;

use serde::Serialize;

use crate::{
    ast::{Literal, Type},
    util::{Writable, Writer},
};

#[derive(Serialize, Clone)]
pub enum ExprKind {
    InvalidExpr,
    Literal(Literal),
    VarExpr(String),
    BinOp(Box<ExprKind>, OpKind, Box<ExprKind>),
    Assign(Box<ExprKind>, Box<ExprKind>),
    FunCallExpr(Box<ExprKind>, Vec<ExprKind>),
    TypecastExpr(Type, Box<ExprKind>),
}

#[derive(Serialize, Clone, Copy)]
pub enum OpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    And,
    Or,
}

impl Writable for OpKind {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<'_, T>) -> anyhow::Result<()> {
        use OpKind::*;
        let s = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Gt => ">",
            Ge => ">=",
            Lt => "<",
            Le => "<=",
            Eq => "==",
            Ne => "!=",
            And => "&&",
            Or => "||",
        };
        write!(writer, "{}", s)?;
        Ok(())
    }
}

impl Writable for ExprKind {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<'_, T>) -> anyhow::Result<()> {
        match self {
            Self::InvalidExpr => write!(writer, "Invalid expression")?,
            Self::Literal(l) => l.write(writer)?,
            Self::VarExpr(s) => write!(writer, "{}", s)?,
            Self::BinOp(lhs, op, rhs) => {
                write!(writer, "(")?;
                lhs.write(writer)?;
                write!(writer, " ")?;
                op.write(writer)?;
                write!(writer, " ")?;
                rhs.write(writer)?;
                write!(writer, ")")?;
            }
            Self::Assign(lhs, rhs) => {
                lhs.write(writer)?;
                write!(writer, " = ")?;
                rhs.write(writer)?;
            }
            Self::FunCallExpr(name, args) => {
                name.write(writer)?;
                write!(writer, "(")?;
                let mut delim = "";
                for arg in args {
                    write!(writer, "{}", delim)?;
                    delim = ", ";
                    arg.write(writer)?;
                }
                write!(writer, ")")?;
            }
            Self::TypecastExpr(t, expr) => {
                write!(writer, "(")?;
                t.write(writer)?;
                write!(writer, ")")?;
                expr.write(writer)?;
            }
        };
        Ok(())
    }
}
