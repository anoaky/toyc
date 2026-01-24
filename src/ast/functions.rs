//! Constructs for encoding functions.
use serde::Serialize;

use crate::ast::{
    statements::Stmt,
    types::{Ident, Ty, TyKind},
};

/// Encodes a single parameter in a [`FnSig`].
///
/// Note that [`PartialEq`] is implemented for [`FnSig`] but not [`Param`].
/// This is because two parameters from two different functions are not equal, but two parameters
/// from the same function are. That is, we require the additional context in [`FnSig`] -- the name and
/// return type -- in order to do an equality comparison.
#[derive(Clone, Serialize)]
pub struct Param {
    pub name: Ident,
    pub ty: Ty,
}

/// Encodes the signature of a function.
#[derive(Clone, Serialize)]
pub struct FnSig {
    pub name: Ident,
    pub ty: Ty,
    pub params: Vec<Param>,
}

impl PartialEq for FnSig {
    fn eq(&self, other: &Self) -> bool {
        if (self.name != other.name)
            || (self.ty != other.ty)
            || (self.params.len() != other.params.len())
        {
            return false;
        }
        for (p1, p2) in self.params.iter().zip(other.params.iter()) {}
        true
    }
}

impl FnSig {
    pub fn new(name: String, ty: TyKind) -> Self {
        Self {
            name: name.into(),
            ty: ty.into(),
            params: vec![],
        }
    }

    pub fn with_parameters(&mut self, params: &mut Vec<Param>) -> &mut Self {
        self.params.append(params);
        self
    }
}

/// Encodes a function declaration.
#[derive(Clone, Serialize)]
pub struct FnDecl {
    pub sig: FnSig,
}

impl From<FnSig> for FnDecl {
    fn from(value: FnSig) -> Self {
        Self { sig: value }
    }
}

impl FnDecl {
    pub fn new(name: String, ty: TyKind) -> Self {
        FnSig::new(name, ty).into()
    }

    pub fn with_parameters(&mut self, params: &mut Vec<Param>) -> &mut Self {
        self.sig.params.append(params);
        self
    }
}

/// Encodes a function definition.
#[derive(Clone, Serialize)]
pub struct FnDefn {
    pub sig: FnSig,
    pub decl: Option<FnDecl>,
    pub block: Stmt,
}
