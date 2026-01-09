use std::io::Write;

use serde::Serialize;

use super::types::Type;
use crate::{
    ast::Ast,
    util::{Writable, Writer},
};

#[derive(Serialize, Clone)]
pub enum DeclKind {
    VarDecl(Type, String),
    StructTypeDecl(Type, String, Vec<Ast>),
    FunDecl(Type, String, Vec<Ast>),
    FunDefn { decl: Box<Ast>, block: Box<Ast> },
}

impl Writable for DeclKind {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<'_, T>) -> anyhow::Result<()> {
        match self {
            DeclKind::VarDecl(t, s) => {
                t.write(writer)?;
                writeln!(writer, " {};", s)?;
            }
            DeclKind::StructTypeDecl(_, name, fields) => {
                writeln!(writer, "struct {} {{", name)?;
                for field in fields {
                    write!(writer, "\t")?;
                    field.write(writer)?;
                    writeln!(writer, ";")?;
                }
                write!(writer, "}}")?;
            }
            DeclKind::FunDecl(t, name, params) => {
                t.write(writer)?;
                write!(writer, " {}(", name)?;
                for param in params {
                    param.write(writer)?;
                    write!(writer, ",")?;
                }
                write!(writer, ")")?;
            }
            DeclKind::FunDefn { decl, block } => {
                decl.write(writer)?;
                write!(writer, " ")?;
                block.write(writer)?;
            }
        }
        Ok(())
    }
}
