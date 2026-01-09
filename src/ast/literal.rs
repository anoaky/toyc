use std::io::Write;

use anyhow::Result;
use serde::Serialize;

use crate::util::{Writable, Writer};

#[derive(Serialize, Clone)]
pub enum Literal {
    Int(i32),
    Char(char),
    Str(String),
}

impl Writable for Literal {
    fn write<T: Write>(&self, writer: &mut Writer<'_, T>) -> Result<()> {
        match self {
            Literal::Int(i) => write!(writer, "{}", i),
            Literal::Char(c) => write!(writer, "'{}'", c),
            Literal::Str(s) => write!(writer, "\"{}\"", s),
        }?;
        Ok(())
    }
}
