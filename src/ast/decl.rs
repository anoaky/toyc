use serde::Serialize;

use crate::util::Writable;

use super::{types::Type, unbound::UnboundAst};

#[derive(Serialize, Clone)]
pub enum DeclKind {
    VarDecl(Type, String),
    StructTypeDecl(Type, String, Vec<UnboundAst>),
    FunDecl(Type, String, Vec<UnboundAst>),
}

impl Writable for DeclKind {
    fn write<T: std::io::Write>(&self, writer: &mut T) -> anyhow::Result<()> {
        match self {
            DeclKind::VarDecl(t, s) => {
                t.write(writer)?;
                write!(writer, " {}", s)?;
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
        }
        Ok(())
    }
}
