mod decl;
mod expr;
mod literal;
mod stmt;
mod types;
use std::io::Write;

use anyhow::Result;
pub use decl::DeclKind;
pub use expr::{ExprKind, OpKind};
pub use literal::Literal;
use serde::Serialize;
pub use stmt::StmtKind;
pub use types::Type;

use crate::util::{Writable, Writer};

#[derive(Serialize, Clone)]
pub enum Ast {
    Program(Vec<Ast>),
    Decl(DeclKind),
    Stmt(StmtKind),
    Expr(ExprKind),
}

impl Writable for Ast {
    fn write<T: Write>(&self, writer: &mut Writer<'_, T>) -> Result<()> {
        match self {
            Ast::Program(decls) => {
                for decl in decls {
                    decl.write(writer)?;
                }
            }
            Ast::Decl(kind) => {
                kind.write(writer)?;
            }
            Ast::Stmt(kind) => {
                kind.write(writer)?;
            }
            Ast::Expr(kind) => {
                kind.write(writer)?;
            }
        };
        Ok(())
    }
}
