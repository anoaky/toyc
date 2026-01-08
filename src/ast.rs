use std::{any::TypeId, cell::Ref, io::Write};

use downcast_rs::{Downcast, impl_downcast};
use erased_serde::serialize_trait_object;
use serde::Serialize;

use crate::ast::{
    decl::{Decl, VarDecl},
    program::Program,
    types::EType,
};

pub mod decl;
pub mod expr;
pub mod program;
pub mod stmt;
pub mod types;

pub trait ASTNode: Downcast + erased_serde::Serialize {
    fn type_name(&self) -> &'static str;
    fn children(&self) -> Vec<Ref<dyn ASTNode>>;
}
impl_downcast!(ASTNode);
serialize_trait_object!(ASTNode);

pub struct ASTPrinter<'a> {
    writer: &'a mut dyn Write,
    tabc: u32,
}

impl<'a> ASTPrinter<'a> {
    pub fn new(writer: &'a mut dyn Write) -> Self {
        Self { writer, tabc: 0 }
    }

    fn format_type(&self, t: &EType) -> String {
        use EType::*;
        match t {
            Int | Char | Void | Unknown | None => format!("{:?}", *t),
            Pointer(inner) => self.format_type(&*inner.borrow()) + "*",
            Struct(_, name) => format!("struct {}", name),
            Array(size, inner) => {
                let inner_type = self.format_type(&*inner.borrow());
                let split_point = inner_type.find("[");
                match split_point {
                    Option::None => format!("{}[{}]", inner_type, size),
                    Some(i) => {
                        let (l, r) = inner_type.split_at(i);
                        format!("{}[{}]{}", l, size, r)
                    }
                }
            }
        }
    }

    // fn format_type(&self, t: &dyn Type) -> String {
    //     if let Some(pt) = t.downcast_ref::<PointerType>() {
    //         let inner_type = self.format_type(pt.points_to.as_ref());
    //         inner_type + "*"
    //     } else if let Some(st) = t.downcast_ref::<StructType>() {
    //         format!("struct {}", st.name)
    //     } else if let Some(at) = t.downcast_ref::<ArrayType>() {
    //         let inner_type = self.format_type(at.ty.as_ref());
    //         let split_point = inner_type.find("[");
    //         match split_point {
    //             None => format!("{}[{}]", inner_type, at.len),
    //             Some(i) => {
    //                 let (l, r) = inner_type.split_at(i);
    //                 format!("{}[{}]{}", l, at.len, r)
    //             }
    //         }
    //     } else if let Some(bt) = t.downcast_ref::<BaseType>() {
    //         format!("{:?}", bt)
    //     } else {
    //         "".to_string()
    //     }
    // }

    pub fn visit(&mut self, node: &dyn ASTNode) -> Result<(), std::io::Error> {
        let node_name = node.type_name();
        let parenthesized_node = node_name == "BaseType"
            || node_name == "PointerType"
            || node_name == "ArrayType"
            || node_name == "StructType";
        if !parenthesized_node {
            write!(self.writer, "{}(", node_name)?;
        }

        match node_name {
            "BaseType" => {
                let bt = node.downcast_ref::<EType>().unwrap();
                write!(self.writer, "{}", self.format_type(bt))?;
            }
            "PointerType" => {
                let pt = node.downcast_ref::<EType>().unwrap();
                write!(self.writer, "{}", self.format_type(pt))?;
            }
            "ArrayType" => {
                let at = node.downcast_ref::<EType>().unwrap();
                write!(self.writer, "{}", self.format_type(at))?;
            }
            "VarDecl" => {
                let vd = node.downcast_ref::<VarDecl>().unwrap();
                self.visit(&*vd.ty.borrow())?;
                write!(self.writer, ", {}", vd.name)?;
            }
            "Program" => {
                let prog = node.downcast_ref::<Program>().unwrap();
                self.tabc += 1;
                let mut delim = "";
                for child in prog.children() {
                    write!(self.writer, "{}", delim)?;
                    self.lftabs()?;
                    delim = ",";
                    self.visit(&*child)?;
                }
                self.tabc -= 1;
                self.lftabs()?;
            }
            _ => {
                let mut delim = "";
                for child in node.children() {
                    write!(self.writer, "{}", delim)?;
                    delim = ",";
                    self.visit(&*child)?;
                }
            }
        }

        if !parenthesized_node {
            write!(self.writer, ")")?;
        }

        Ok(())
    }

    fn lftabs(&mut self) -> std::io::Result<()> {
        writeln!(self.writer)?;
        self.tabs()?;
        Ok(())
    }

    fn tabs(&mut self) -> std::io::Result<()> {
        for _ in 0..self.tabc {
            write!(self.writer, "\t")?;
        }
        Ok(())
    }
}

pub fn format_program(node: &dyn ASTNode) -> Result<String, std::io::Error> {
    let mut out = Vec::new();
    let mut printer = ASTPrinter::new(&mut out);
    printer.visit(node)?;

    let s = match String::from_utf8(out) {
        Ok(s) => s,
        Err(_) => panic!("Failed to convert output"),
    };
    Ok(s)
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;

    use crate::rc_ref;

    use super::*;

    #[test]
    fn test_base_type_print() -> std::io::Result<()> {
        let t1 = EType::Int;

        let s = format_program(&t1)?;
        assert_eq!(s, "INT");

        Ok(())
    }

    #[test]
    fn test_var_decl_print() -> std::io::Result<()> {
        let t1 = EType::Int;
        let vd = VarDecl::new(t1, "x".to_string());

        let s = format_program(&vd)?;

        assert_snapshot!(s, @"VarDecl(INT, x)");
        Ok(())
    }

    #[test]
    fn test_program_print() -> std::io::Result<()> {
        let t1 = EType::Int;
        let t2 = EType::Char;
        let t3 = EType::Int;
        let vd1 = VarDecl::new(t1, "x".to_string());
        let vd2 = VarDecl::new(t2, "y".to_string());
        let vd3 = VarDecl::new(t3, "z".to_string());
        let prog = Program::new(vec![rc_ref!(vd1), rc_ref!(vd2), rc_ref!(vd3)]);

        let s = format_program(&prog)?;

        assert_snapshot!(s, @r"
        Program(
        	VarDecl(INT, x),
        	VarDecl(CHAR, y),
        	VarDecl(INT, z)
        )
        ");
        Ok(())
    }
}
