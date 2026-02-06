//! Abstract Syntax Tree (AST), produced by the [Parser](super::parser).
//!
//! The AST is the first intermediate representation (IR) produced by the compiler from the raw, tokenized input.
use serde::Serialize;

pub mod exprs;
pub mod functions;
pub mod pattern;
pub mod statements;
pub mod structs;
pub mod types;
use crate::{
    ast::{
        exprs::Literal,
        functions::{FnDecl, FnDefn},
        structs::StructDecl,
        types::{Ident, Ty},
    },
    util::NodeId,
};

/// Encodes the kind of an [`Item`].
#[derive(Clone, Serialize)]
pub enum ItemKind {
    Static(StaticDecl),
    StructDecl(StructDecl),
    FnDecl(FnDecl),
    FnDefn(FnDefn),
}

/// Any top-level declaration.
///
/// An Akyno program consists of a series of [`Items`](`Item`).
#[derive(Clone, Serialize)]
pub struct Item {
    pub id: NodeId,
    pub kind: ItemKind,
}

/// A statically-allocated variable.
#[derive(Clone, Eq, Serialize)]
pub struct StaticDecl {
    pub ident: Ident,
    pub ty: Ty,
    pub value: Option<Literal>,
}

/// Equal if [`Ident`] and [`Ty`] are equal
impl PartialEq for StaticDecl {
    fn eq(&self, other: &Self) -> bool {
        self.ident == other.ident && self.ty == other.ty
    }
}

impl From<ItemKind> for Item {
    fn from(value: ItemKind) -> Self {
        Self {
            id: NodeId::next(),
            kind: value,
        }
    }
}

impl From<(Ident, Ty, Option<Literal>)> for StaticDecl {
    fn from((ident, ty, value): (Ident, Ty, Option<Literal>)) -> Self {
        Self { ident, ty, value }
    }
}
