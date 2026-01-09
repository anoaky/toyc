use anyhow::Result;
use serde::Serialize;

use crate::util::Writable;

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

impl Writable for Type {
    fn write<T: std::io::Write>(&self, writer: &mut T) -> anyhow::Result<()> {
        use Type::*;
        match self {
            Int | Char | Void | Unknown | None => write!(writer, "{:?}", self)?,
            Pointer(t) => {
                t.write(writer)?;
                write!(writer, "*")?;
            }
            Struct(s) => write!(writer, "struct {}", s)?,
            Array(s, t) => {
                let mut out = Vec::new();
                t.write(&mut out)?;
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
