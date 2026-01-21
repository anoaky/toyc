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

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        use Type::*;
        match (self, other) {
            (Int, Int) | (Char, Char) | (Void, Void) | (Unknown, Unknown) | (None, None) => true,
            (Pointer(t1), Pointer(t2)) => (**t1).eq(&**t2),
            (Array(s1, t1), Array(s2, t2)) => *s1 == *s2 && (**t1).eq(&**t2),
            (Struct(s1), Struct(s2)) => *s1 == *s2,
            (_, _) => false,
        }
    }
}

impl Eq for Type {}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Char => write!(f, "char"),
            Void => write!(f, "void"),
            Unknown => write!(f, "UNKNOWN"),
            None => write!(f, "NONE"),
            Pointer(t) => {
                write!(f, "{}*", **t)
            }
            Struct(s) => write!(f, "struct {}", s),
            Array(s, t) => {
                let mut out = Vec::new();
                write!(&mut out, "{}", **t).ok().unwrap();
                let inner_type = String::from_utf8(out).ok().unwrap();
                let split_point = inner_type.find("[");
                match split_point {
                    Option::None => write!(f, "{}[{}]", inner_type, s),
                    Some(i) => {
                        let (l, r) = inner_type.split_at(i);
                        write!(f, "{}[{}]{}", l, s, r)
                    }
                }
            }
        }
    }
}

// TODO: rewrite this using Display
impl Writable for Type {
    fn write<T: std::io::Write>(&self, writer: &mut Writer<T>, _: bool) -> Result<()> {
        use Type::*;
        match self {
            Int | Char | Void | Unknown | None => {
                write!(writer, "{}", self.to_string().to_lowercase())?
            }
            Pointer(t) => {
                t.write(writer, false)?;
                write!(writer, "*")?;
            }
            Struct(s) => write!(writer, "struct {}", s)?,
            Array(s, t) => {
                let mut out = Vec::new();
                let mut new_writer = Writer::new(&mut out);
                t.write(&mut new_writer, false)?;
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
