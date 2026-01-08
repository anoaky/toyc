use std::any::Any;
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use std::vec::Vec;

use downcast_rs::impl_downcast;
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::ast::ASTNode;

#[derive(Clone, PartialEq, Eq, Debug, Serialize)]
pub enum EType {
    Int,
    Char,
    Void,
    Array(usize, Rc<RefCell<EType>>),
    Pointer(Rc<RefCell<EType>>),
    Struct(RefCell<usize>, String),
    Unknown,
    None,
}

impl ASTNode for EType {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Int | Self::Char | Self::Void | Self::Unknown | Self::None => "BaseType",
            Self::Array(_, _) => "ArrayType",
            Self::Pointer(_) => "PointerType",
            Self::Struct(_, _) => "StructType",
        }
    }
    fn children(&self) -> Vec<Ref<dyn ASTNode>> {
        match self {
            Self::Int
            | Self::Char
            | Self::Void
            | Self::Struct(_, _)
            | Self::Unknown
            | Self::None => vec![],
            Self::Array(_, inner) | Self::Pointer(inner) => vec![inner.borrow()],
        }
    }
}

impl EType {
    pub fn equals(&self, other: EType) -> bool {
        use EType::*;
        match (self, other) {
            (Int, Int) | (Char, Char) | (Void, Void) | (Unknown, Unknown) | (None, None) => true,
            (Array(s1, i1), Array(s2, i2)) => *s1 == s2 && i1.borrow().equals(i2.borrow().clone()),
            (Pointer(t1), Pointer(t2)) => t1.borrow().equals(t2.borrow().clone()),
            (Struct(s1, n1), Struct(s2, n2)) => *s1.borrow() == *s2.borrow() && *n1 == n2,
            _ => false,
        }
    }
}
