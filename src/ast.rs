use std::{
    any::{Any, TypeId},
    io::Write,
};

use crate::ast::{decl::VarDecl, program::Program, types::BaseType};

pub mod decl;
pub mod program;
pub mod types;

pub trait ASTNode {
    fn type_name(&self) -> &'static str;
    fn children(&self) -> Vec<&dyn ASTNode>;
}

pub struct ASTPrinter {
    writer: Box<dyn Write>,
    tabc: u32,
}

impl Default for ASTPrinter {
    fn default() -> Self {
        Self {
            writer: Box::new(std::io::stdout()),
            tabc: 0,
        }
    }
}

impl ASTPrinter {
    pub fn new(writer: Box<dyn Write>) -> Self {
        Self { writer, tabc: 0 }
    }

    pub fn visit(&mut self, node: &dyn Any) -> Result<(), std::io::Error> {
        if node.type_id() != TypeId::of::<BaseType>() {
            write!(
                self.writer,
                "{} (",
                node.downcast_ref::<&dyn ASTNode>().unwrap().type_name()
            )?;
        }

        Ok(())
    }
}
