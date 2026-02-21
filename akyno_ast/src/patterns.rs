use std::{fmt::Display, hash::Hash};

use akyno_util::NodeId;
use internment::Intern;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Hash)]
pub struct Ident {
    pub name: Intern<String>,
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, Eq, Serialize)]
pub enum PatternKind {
    Single(Ident),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone, Eq, Serialize, Hash)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: Intern<PatternKind>,
}

impl Display for PatternKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single(id) => write!(f, "{}", id),
            Self::Tuple(ids) => write!(f, "({})", ids.iter().map(Pattern::to_string).collect::<Vec<String>>().join(", ")),
        }
    }
}

impl PartialEq for PatternKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Single(id1), Self::Single(id2)) => *id1 == *id2,
            (Self::Tuple(v1), Self::Tuple(v2)) => v1.len() == v2.len() && v1.iter().zip(v2).all(|(id1, id2)| *id1 == *id2),
            _ => false,
        }
    }
}

impl Hash for PatternKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Single(id) => id.hash(state),
            Self::Tuple(v) => v.hash(state),
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self {
            name: Intern::from_ref(value),
        }
    }
}

impl From<PatternKind> for Pattern {
    fn from(value: PatternKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: Intern::new(value),
        }
    }
}
