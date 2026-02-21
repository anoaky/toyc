use std::{fmt::Display, hash::Hash};

use akyno_util::NodeId;
use internment::Intern;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub enum Primitive {
    Unit,
    Int,
    Char,
}

#[derive(Debug, Clone, Eq, Serialize)]
pub enum TyKind {
    Primitive(Primitive),
    Pointer(Ty),
    Tuple(Vec<Ty>),
    Fn(Vec<Ty>, Ty),
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
            (Self::Pointer(t1), Self::Pointer(t2)) => *t1 == *t2,
            (Self::Tuple(t1), Self::Tuple(t2)) => {
                if t1.len() != t2.len() {
                    false
                } else {
                    t1.iter().zip(t2).all(|(t1, t2)| *t1 == *t2)
                }
            }
            (Self::Fn(p1, r1), Self::Fn(p2, r2)) => Self::Tuple(p1.clone()) == Self::Tuple(p2.clone()) && *r1 == *r2,
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
            Self::Pointer(ty) => {
                "*".hash(state);
                ty.hash(state);
            }
            Self::Tuple(tys) => {
                tys.hash(state);
            }
            Self::Fn(params, ret) => {
                params.hash(state);
                ret.hash(state);
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
        match self {
            Self::Primitive(p) => write!(f, "{p}"),
            Self::Pointer(ty) => write!(f, "(&{})", ty),
            Self::Tuple(tys) => {
                write!(f, "(")?;
                let mut delim = "";
                for ty in tys {
                    write!(f, "{}{}", delim, ty)?;
                    delim = ", ";
                }
                write!(f, ")")
            }
            Self::Fn(params, ret) => {
                write!(f, "{} -> {}", Self::Tuple(params.clone()), ret)
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

impl From<TyKind> for Ty {
    fn from(value: TyKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: Intern::new(value),
        }
    }
}
