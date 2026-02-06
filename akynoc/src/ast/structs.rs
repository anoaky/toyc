//! Constructs for encoding structs.
use serde::Serialize;

use crate::ast::types::{Ident, Ty, TyKind};

/// Encodes a single field in a [`StructDecl`].
#[derive(Clone, Serialize)]
pub struct Field {
    pub name: Ident,
    pub ty: Ty,
}

impl Field {
    pub fn new(name: String, ty_kind: TyKind) -> Self {
        Self {
            name: name.into(),
            ty: ty_kind.into(),
        }
    }
}

/// Encodes a struct.
#[derive(Clone, Serialize)]
pub struct StructDecl {
    pub name: Ident,
    pub fields: Vec<Field>,
}

impl StructDecl {
    pub fn new(name: String, fields: Vec<Field>) -> Self {
        Self {
            name: name.into(),
            fields,
        }
    }
}

impl PartialEq for StructDecl {
    fn eq(&self, other: &Self) -> bool {
        if (self.name != other.name) || (self.fields.len() != other.fields.len()) {
            return false;
        }
        for i in 0..self.fields.len() {
            let f1 = self.fields.get(i).unwrap();
            let f2 = other.fields.get(i).unwrap();
            if (f1.name != f2.name) || (f1.ty != f2.ty) {
                return false;
            }
        }
        true
    }
}
