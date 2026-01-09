use std::{fmt::Display, io::Write};

use anyhow::Result;
use serde::Serialize;

use crate::util::{Writable, Writer};

#[derive(Serialize, Clone, Debug)]
pub enum Type {
    Int,
    Char,
    Void,
    Unknown,
    None,
    Pointer(Box<Type>),
    Array(usize, Box<Type>),
    Struct(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Writable for Type {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<T>) -> Result<()> {
        use Type::*;
        match self {
            Int | Char | Void | Unknown | None => {
                write!(writer, "{}", self.to_string().to_lowercase())?
            }
            Pointer(t) => {
                t.write(writer)?;
                write!(writer, "*")?;
            }
            Struct(s) => write!(writer, "struct {}", s)?,
            Array(s, t) => {
                let mut out = Vec::new();
                let mut new_writer = Writer::new(&mut out);
                t.write(&mut new_writer)?;
                let inner_type = String::from_utf8(out)?;
                let split_point = inner_type.find("[");
                match split_point {
                    Option::None => write!(writer, "{}[{}]", inner_type, s)?,
                    Some(i) => {
                        let (l, r) = inner_type.split_at(i);
                        write!(writer, "{}[{}]{}", l, s, r)?;
                    }
                };
            }
        };
        Ok(())
    }
}
