use std::io::Write;

use serde::Serialize;

use crate::{
    ast::Ast,
    util::{Writable, Writer},
};

#[derive(Serialize, Clone)]
pub enum StmtKind {
    Block {
        decls: Vec<Ast>,
        stmts: Vec<Ast>,
    },
    While {
        expr: Box<Ast>,
        stmt: Box<Ast>,
    },
    If {
        expr: Box<Ast>,
        then: Box<Ast>,
        els: Option<Box<Ast>>,
    },
    Return(Option<Box<Ast>>),
    ExprStmt(Box<Ast>),
    Break,
    Continue,
}

impl Writable for StmtKind {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<'_, T>) -> anyhow::Result<()> {
        match self {
            Self::Block { decls, stmts } => {
                writeln!(writer, "{{")?;
                writer.inctabs();
                for decl in decls {
                    writer.tabs()?;
                    decl.write(writer)?;
                }
                for stmt in stmts {
                    writer.tabs()?;
                    stmt.write(writer)?;
                }
                writer.dectabs();
                writer.tabs()?;
                writeln!(writer, "}}")?;
            }
            Self::While { expr, stmt } => {
                write!(writer, "while (")?;
                expr.write(writer)?;
                write!(writer, ") ")?;
                stmt.write(writer)?;
            }
            Self::If { expr, then, els } => {
                write!(writer, "if (")?;
                expr.write(writer)?;
                write!(writer, ") ")?;
                then.write(writer)?;
                if let Some(els) = els {
                    write!(writer, "\n else ")?;
                    els.write(writer)?;
                }
            }
            Self::Return(rv) => {
                write!(writer, "return")?;
                if let Some(rv) = rv {
                    write!(writer, " ")?;
                    rv.write(writer)?;
                }
            }
            Self::ExprStmt(expr) => {
                expr.write(writer)?;
                writeln!(writer, ";")?;
            }
            Self::Break => write!(writer, "break")?,
            Self::Continue => write!(writer, "continue")?,
        }
        Ok(())
    }
}
