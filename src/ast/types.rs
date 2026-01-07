use std::any::Any;

use crate::ast::ASTNode;

pub trait Type: ASTNode {
    fn equals(&self, other: Box<dyn Any>) -> bool;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BaseType {
    INT,
    CHAR,
    VOID,
    UNKNOWN,
    NONE,
}

impl ASTNode for BaseType {
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
}
