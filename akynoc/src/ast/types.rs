use std::{fmt::Display, hash::Hash};

use internment::Intern;
use serde::Serialize;

use crate::{ast::patterns::Ident, util::NodeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub enum Primitive {
    Unit,
    Int,
    Char,
}

#[derive(Debug, Clone, Eq, Serialize)]
pub enum TyKind {
    Primitive(Primitive),
    Struct(Ident),
    Pointer(Ty),
    Array(usize, Ty),
    Tuple(Vec<Ty>),
    Infer,
}

#[derive(Debug, Clone, Eq, Serialize)]
pub struct Ty {
    pub id: NodeId,
    pub kind: Intern<TyKind>,
}

impl PartialEq for TyKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Primitive(p1), Self::Primitive(p2)) => *p1 == *p2,
            (Self::Struct(id1), Self::Struct(id2)) => *id1 == *id2,
            (Self::Pointer(t1), Self::Pointer(t2)) => *t1 == *t2,
            (Self::Array(s1, t1), Self::Array(s2, t2)) => *s1 == *s2 && *t1 == *t2,
            (Self::Tuple(t1), Self::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    false
                } else {
                    t1.iter().zip(t2).all(|(t1, t2)| *t1 == *t2)
                }
            }
            _ => false,
        }
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Hash for TyKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Primitive(p) => p.hash(state),
            Self::Struct(id) => id.hash(state),
            Self::Pointer(ty) => {
                "*".hash(state);
                ty.hash(state);
            }
            Self::Array(size, ty) => {
                size.hash(state);
                ty.hash(state);
            }
            Self::Tuple(tys) => {
                tys.hash(state);
            }
            Self::Infer => "infer".hash(state),
        }
    }
}

impl Hash for Ty {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.kind.hash(state);
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Unit => write!(f, "()"),
            Primitive::Int => write!(f, "int"),
            Primitive::Char => write!(f, "char"),
        }
    }
}
impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::io::Write;
        match self {
            Self::Primitive(p) => write!(f, "{p}"),
            Self::Struct(id) => write!(f, "struct {id}"),
            Self::Pointer(ty) => write!(f, "(&{})", ty),
            Self::Array(size, ty) => {
                let mut out = vec![];
                write!(&mut out, "{}", ty).unwrap();
                let inner_type = String::from_utf8(out).unwrap();
                let split_point = inner_type.find("[");
                match split_point {
                    None => write!(f, "({inner_type}[{size}])"),
                    Some(i) => {
                        let (l, r) = inner_type.split_at(i);
                        write!(f, "{l}[{size}]{r}")
                    }
                }
            }
            Self::Tuple(tys) => {
                write!(f, "(")?;
                let mut delim = "";
                for ty in tys {
                    write!(f, "{}{}", delim, ty)?;
                    delim = ", ";
                }
                write!(f, ")")
            }
            Self::Infer => write!(f, "_"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
