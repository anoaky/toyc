use std::io::Write;

use anyhow::Result;
use serde::Serialize;

use crate::util::Writable;

use super::decl::DeclKind;

#[derive(Serialize, Clone)]
pub enum UnboundAst {
    Program(Vec<UnboundAst>),
    Literal(Literal),
    Decl(DeclKind),
}

#[derive(Serialize, Clone)]
pub enum Literal {
    Int(i32),
    Char(char),
    Str(String),
}

impl Writable for Literal {
    fn write<T: Write>(&self, writer: &mut T) -> Result<()> {
        match self {
            Literal::Int(i) => write!(writer, "Int({})", i),
            Literal::Char(c) => write!(writer, "Char({})", c),
            Literal::Str(s) => write!(writer, "Str({})", s),
        }?;
        Ok(())
    }
}

impl Writable for UnboundAst {
    fn write<T: Write>(&self, writer: &mut T) -> Result<()> {
        match self {
            UnboundAst::Program(decls) => {
                for decl in decls {
                    decl.write(writer)?;
                }
            }
            UnboundAst::Literal(l) => l.write(writer)?,
            UnboundAst::Decl(kind) => {
                kind.write(writer)?;
                writeln!(writer, ";")?;
            }
        };
        Ok(())
    }
}
