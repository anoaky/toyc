//! Constructs for encoding types.
use std::fmt::Display;

use internment::Intern;
use serde::Serialize;

use crate::util::NodeId;

/// Encodes an identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct Ident {
    pub name: Intern<String>,
}

/// Encodes the two primitive types: `int` and `char`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub enum Primitive {
    Int,
    Char,
}

/// Encodes information for [`Ty`].
///
/// Since all [`TyKind`] are interned, two different [`Items`](super::Item) with equivalent type
/// will have the exact same [`TyKind`].
#[derive(Debug, Clone, Copy, Eq, Serialize, Hash)]
pub enum TyKind {
    Primitive(Primitive),
    Void,
    Struct(Ident),
    Pointer(Ty),
    Array(usize, Ty),
    Infer,
}

/// Encodes a single type.
#[derive(Debug, Clone, Copy, Eq, Serialize, Hash)]
pub struct Ty {
    pub id: NodeId,
    pub kind: Intern<TyKind>,
}

impl From<TyKind> for Ty {
    fn from(value: TyKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: Intern::new(value),
        }
    }
}

impl PartialEq for TyKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TyKind::Primitive(p1), TyKind::Primitive(p2)) => *p1 == *p2,
            (TyKind::Void, TyKind::Void) => true,
            (TyKind::Struct(s1), TyKind::Struct(s2)) => *s1 == *s2,
            (TyKind::Pointer(t1), TyKind::Pointer(t2)) => *t1 == *t2,
            (TyKind::Array(s1, t1), TyKind::Array(s2, t2)) => *s1 == *s2 && *t1 == *t2,
            #[cfg(test)]
            (TyKind::Infer, TyKind::Infer) => true,
            (_, _) => false,
        }
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Int => write!(f, "int"),
            Primitive::Char => write!(f, "char"),
        }
    }
}
use std::io::Write;

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &Self::Primitive(p) => write!(f, "{p}"),
            &Self::Void => write!(f, "void"),
            &Self::Struct(id) => write!(f, "struct {id}"),
            &Self::Pointer(ty) => write!(f, "{}*", ty),
            &Self::Array(size, ty) => {
                let mut out = vec![];
                write!(&mut out, "{}", ty).unwrap();
                let inner_type = String::from_utf8(out).unwrap();
                let split_point = inner_type.find("[");
                match split_point {
                    None => write!(f, "{inner_type}[{size}]"),
                    Some(i) => {
                        let (l, r) = inner_type.split_at(i);
                        write!(f, "{l}[{size}]{r}")
                    }
                }
            }
            &Self::Infer => write!(f, "_"),
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self {
            name: Intern::from_ref(value),
        }
    }
}

impl From<String> for Ident {
    fn from(value: String) -> Self {
        Self {
            name: Intern::new(value),
        }
    }
}
