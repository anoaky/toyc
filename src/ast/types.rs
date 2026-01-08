use std::any::Any;

use downcast_rs::impl_downcast;
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::ast::ASTNode;

pub trait Type: ASTNode {
    fn equals(&self, other: Box<dyn Any>) -> bool;
}
impl_downcast!(Type);
serialize_trait_object!(Type);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize)]
pub enum BaseType {
    INT,
    CHAR,
    VOID,
    UNKNOWN,
    NONE,
}

impl ASTNode for BaseType {
    fn type_name(&self) -> &'static str {
        "BaseType"
    }
    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl Type for BaseType {
    fn equals(&self, other: Box<dyn Any>) -> bool {
        if let Ok(other) = other.downcast::<BaseType>() {
            *self == *other.as_ref()
        } else {
            false
        }
    }
}

#[derive(Serialize)]
pub struct ArrayType {
    pub array_type: Box<dyn Type>,
    pub len: usize,
}

impl ASTNode for ArrayType {
    fn type_name(&self) -> &'static str {
        "ArrayType"
    }
    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![self.array_type.as_ref()]
    }
}

impl Type for ArrayType {
    fn equals(&self, other: Box<dyn Any>) -> bool {
        if let Ok(other) = other.downcast::<ArrayType>() {
            self.len == other.len && self.array_type.equals(other.array_type)
        } else {
            false
        }
    }
}

impl ArrayType {
    pub fn new(ty: Box<dyn Type>, len: usize) -> Self {
        Self {
            array_type: ty,
            len,
        }
    }
}

#[derive(Serialize)]
pub struct PointerType {
    pub points_to: Box<dyn Type>,
}

impl ASTNode for PointerType {
    fn type_name(&self) -> &'static str {
        "PointerType"
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![self.points_to.as_ref()]
    }
}

impl Type for PointerType {
    fn equals(&self, other: Box<dyn Any>) -> bool {
        if let Ok(other) = other.downcast::<PointerType>() {
            self.points_to.equals(other.points_to)
        } else {
            false
        }
    }
}

impl PointerType {
    pub fn new(points_to: Box<dyn Type>) -> Self {
        Self { points_to }
    }
}

#[derive(Serialize)]
pub struct StructType {
    pub name: String,
    pub size: usize,
}

impl ASTNode for StructType {
    fn type_name(&self) -> &'static str {
        "StructType"
    }

    fn children(&self) -> Vec<&dyn ASTNode> {
        vec![]
    }
}

impl Type for StructType {
    fn equals(&self, other: Box<dyn Any>) -> bool {
        if let Ok(other) = other.downcast::<StructType>() {
            self.name == other.name
        } else {
            false
        }
    }
}

impl StructType {
    pub fn new(name: String) -> Self {
        Self { name, size: 0 }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_base_type_equals() {
        let t1 = Box::new(BaseType::INT);
        let t2 = Box::new(BaseType::INT);

        assert!(t1.equals(t2));

        let t3 = Box::new(BaseType::CHAR);
        assert!(!t1.equals(t3));
    }

    #[test]
    fn check_array_type_equals() {
        let t1 = Box::new(BaseType::INT);
        let t2 = Box::new(BaseType::INT);
        let at1 = Box::new(ArrayType::new(t1, 2));
        let at2 = Box::new(ArrayType::new(t2, 2));

        assert!(at1.equals(at2));
    }
}
