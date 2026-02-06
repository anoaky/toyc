use serde::Serialize;

use crate::{
    ast::{exprs::Expr, types::Ident},
    util::NodeId,
};

#[derive(Clone, Serialize)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
}

#[derive(Clone, Serialize)]
pub enum PatternKind {
    RangePattern(Ident, Range),
}

#[derive(Clone, Serialize)]
pub enum Range {
    Exclusive(Expr, Expr),
    Inclusive(Expr, Expr),
    ExclusiveInclusive(Expr, Expr),
    InclusiveExclusive(Expr, Expr),
}

impl From<(Ident, Range)> for Pattern {
    fn from((id, range): (Ident, Range)) -> Self {
        Self {
            id: NodeId::next(),
            kind: PatternKind::RangePattern(id, range),
        }
    }
}
