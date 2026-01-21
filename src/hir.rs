mod bind;
mod scope;
mod typer;

pub use scope::Scope;

use std::{path::Path, rc::Rc};

use anyhow::Result;
pub use bind::Binder;
use serde::Serialize;
pub use typer::Typer;

use crate::{
    ast::{Literal, Operator, Type},
    parser::Parser,
    util::{CompilerPass, NodeId, NodeRef},
};

pub struct SemanticAnalysis {
    num_errors: u32,
    parser: Parser,
}

impl CompilerPass for SemanticAnalysis {
    fn num_errors(&self) -> u32 {
        self.num_errors
    }
    fn inc_error(&mut self) {
        self.num_errors += 1;
    }
}

impl SemanticAnalysis {
    pub fn from_path(path: &Path) -> Result<Self> {
        let parser = Parser::from_path(path)?;
        Ok(Self {
            num_errors: 0,
            parser,
        })
    }

    pub fn analyse(&mut self) -> Result<HirPool<TypedHir>> {
        let mut binder = Binder::new();
        let ast = self.parser.parse()?;
        let untyped_hir = binder.bind(ast)?;
        if binder.has_error() {
            self.num_errors += binder.num_errors();
        }
        let mut typer = Typer::new(untyped_hir);
        let typed_hir = typer.type_all()?;
        if typer.has_error() {
            self.num_errors += typer.num_errors();
        }
        Ok(typed_hir)
    }
}

#[derive(Serialize, Clone)]
pub enum HIRKind {
    Invalid,
    VarDecl(Type, String, Option<NodeRef>),
    StructTypeDecl(Type, String, Vec<NodeRef>),
    FunDecl(Type, String, Vec<NodeRef>),
    FunDefn(NodeRef, NodeRef),

    Literal(Literal),
    VarExpr(NodeRef),
    BinOp(NodeRef, Operator, NodeRef),
    Assign(NodeRef, NodeRef),
    FunCallExpr(NodeRef, Vec<NodeRef>),
    TypecastExpr(Type, NodeRef),
    RefExpr(NodeRef),
    DerefExpr(NodeRef),
    FieldAccessExpr(NodeRef, String),
    ArrayAccessExpr(NodeRef, NodeRef),

    Block(Vec<NodeRef>),
    While(NodeRef, NodeRef),
    If(NodeRef, NodeRef, Option<NodeRef>),
    Return(Option<NodeRef>),
    Break,
    Continue,
}

#[derive(Serialize, Clone)]
pub struct UntypedHir {
    id: NodeId,
    kind: HIRKind,
}

impl UntypedHir {
    fn new(kind: HIRKind) -> Self {
        Self {
            kind,
            ..Self::default()
        }
    }
}

impl Default for UntypedHir {
    fn default() -> Self {
        Self {
            id: NodeId::next(),
            kind: HIRKind::Invalid,
        }
    }
}

#[derive(Serialize, Clone)]
pub struct TypedHir {
    id: NodeId,
    ty: Type,
    kind: HIRKind,
}

impl TypedHir {
    fn new(ty: Type, kind: HIRKind) -> Self {
        Self {
            ty,
            kind,
            ..Self::default()
        }
    }
}

impl Default for TypedHir {
    fn default() -> Self {
        Self {
            id: NodeId::next(),
            ty: Type::Unknown,
            kind: HIRKind::Invalid,
        }
    }
}

#[derive(Serialize, Clone)]
pub struct HirPool<T>(Vec<T>, Vec<NodeRef>);

impl<T> HirPool<T> {
    pub fn new() -> Self {
        Self(vec![], vec![])
    }

    pub fn add(&mut self, node: T) -> NodeRef {
        let new_ref = self.0.len();
        self.0.push(node);
        NodeRef(new_ref)
    }

    pub fn get(&self, node_ref: NodeRef) -> Option<&T> {
        self.0.get(node_ref.0)
    }

    pub fn get_tops(&self) -> Vec<NodeRef> {
        self.1.clone()
    }

    pub fn set_top(&mut self, node_ref: NodeRef) {
        self.1.push(node_ref);
    }
}

#[derive(Serialize, Clone)]
pub enum HIRDeclKind {
    VarDecl(Type, String, Option<HIRExprKind>),
    StructTypeDecl(Type, String, Vec<Rc<HIRDeclKind>>),
    FunDecl(Type, String, Vec<Rc<HIRDeclKind>>),
    FunDefn(Rc<HIRDeclKind>, Box<HIRStmtKind>),
}

#[derive(Serialize, Clone)]
pub enum HIRExprKind {
    Invalid,
    Literal(Type, Literal), // potentially redundant ?
    VarExpr(NodeRef),
    BinOp(NodeRef, Operator, NodeRef),
    Assign(NodeRef, NodeRef),
    FunCallExpr(NodeRef, Vec<NodeRef>),
    TypecastExpr(Type, NodeRef),
    RefExpr(NodeRef),
    DerefExpr(NodeRef),
    FieldAccessExpr(NodeRef, String),
    ArrayAccessExpr(NodeRef, NodeRef),
}

#[derive(Serialize, Clone)]
pub enum HIRStmtKind {
    Block(Vec<HIRStmtKind>),
    While(Box<HIRExprKind>, Box<HIRStmtKind>),
    If(Box<HIRExprKind>, Box<HIRStmtKind>, Option<Box<HIRStmtKind>>),
    Decl(Rc<HIRDeclKind>),
    Return(Option<Box<HIRExprKind>>),
    ExprStmt(HIRExprKind),
    Break,
    Continue,
}

#[derive(Serialize, Clone)]
pub struct HIRDecl {
    id: NodeId,
    kind: HIRDeclKind,
}

#[derive(Serialize, Clone)]
pub struct UntypedExpr {
    id: NodeId,
    kind: HIRExprKind,
}

#[derive(Serialize, Clone)]
pub struct TypedExpr {
    id: NodeId,
    ty: Type,
    kind: HIRExprKind,
}

#[derive(Serialize, Clone)]
pub struct HIRStmt {
    id: NodeId,
    kind: HIRStmtKind,
}
