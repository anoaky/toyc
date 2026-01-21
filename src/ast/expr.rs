use std::{fmt::Display, io::Write, rc::Rc};

use serde::Serialize;

use crate::{
    ast::{Literal, Type},
    util::{Writable, Writer},
};

use super::BoundDecl;

#[derive(Serialize, Clone)]
pub enum ExprKind {
    InvalidExpr,
    Literal(Literal),
    VarExpr(String),
    BinOp(Box<ExprKind>, Operator, Box<ExprKind>),
    Assign(Box<ExprKind>, Box<ExprKind>),
    FunCallExpr(Box<ExprKind>, Vec<ExprKind>),
    TypecastExpr(Type, Box<ExprKind>),
    RefExpr(Box<ExprKind>),
    DerefExpr(Box<ExprKind>),
    FieldAccessExpr(Box<ExprKind>, String),
    ArrayAccessExpr(Box<ExprKind>, Box<ExprKind>),
}

#[derive(Serialize, Clone)]
pub enum BoundExpr {
    InvalidExpr,
    Literal(Literal),
    VarExpr(Rc<BoundDecl>),
    BinOp {
        lhs: Box<BoundExpr>,
        op: Operator,
        rhs: Box<BoundExpr>,
    },
    Assign {
        lhs: Box<BoundExpr>,
        rhs: Box<BoundExpr>,
    },
    FunCallExpr {
        defn: Rc<BoundDecl>,
        args: Vec<BoundExpr>,
    },
    TypecastExpr {
        to: Type,
        expr: Box<BoundExpr>,
    },
    RefExpr(Box<BoundExpr>),
    DerefExpr(Box<BoundExpr>),
    FieldAccessExpr(Box<BoundExpr>, String),
    ArrayAccessExpr(Box<BoundExpr>, Box<BoundExpr>),
}

#[derive(Serialize, Clone, Copy)]
pub enum Operator {
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

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operator::*;
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
        write!(f, "{}", s)
    }
}

impl Writable for Operator {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<'_, T>, _: bool) -> anyhow::Result<()> {
        write!(writer, "{}", self)?;
        Ok(())
    }
}

impl Writable for ExprKind {
    fn write<T: std::io::Write>(
        &self,
        writer: &mut Writer<'_, T>,
        eol: bool,
    ) -> anyhow::Result<()> {
        match self {
            Self::InvalidExpr => write!(writer, "Invalid expression")?,
            Self::Literal(l) => l.write(writer, false)?,
            Self::VarExpr(s) => write!(writer, "{}", s)?,
            Self::BinOp(lhs, op, rhs) => {
                write!(writer, "(")?;
                lhs.write(writer, false)?;
                write!(writer, " ")?;
                op.write(writer, false)?;
                write!(writer, " ")?;
                rhs.write(writer, false)?;
                write!(writer, ")")?;
            }
            Self::Assign(lhs, rhs) => {
                lhs.write(writer, false)?;
                write!(writer, " = ")?;
                rhs.write(writer, false)?;
            }
            Self::FunCallExpr(name, args) => {
                name.write(writer, false)?;
                write!(writer, "(")?;
                let mut delim = "";
                for arg in args {
                    write!(writer, "{}", delim)?;
                    delim = ", ";
                    arg.write(writer, false)?;
                }
                write!(writer, ")")?;
            }
            Self::TypecastExpr(t, expr) => {
                write!(writer, "(")?;
                t.write(writer, false)?;
                write!(writer, ")")?;
                expr.write(writer, false)?;
            }
            Self::RefExpr(expr) => {
                write!(writer, "&")?;
                expr.write(writer, false)?;
            }
            Self::DerefExpr(expr) => {
                write!(writer, "*")?;
                expr.write(writer, false)?;
            }
            Self::FieldAccessExpr(expr, field) => {
                expr.write(writer, false)?;
                write!(writer, ".{}", field)?;
            }
            Self::ArrayAccessExpr(array, index) => {
                array.write(writer, false)?;
                write!(writer, "[")?;
                index.write(writer, false)?;
                write!(writer, "]")?;
            }
        };
        if eol {
            writeln!(writer, ";")?;
        }
        Ok(())
    }
}
