use std::{
    collections::{HashMap, VecDeque},
    default::Default,
};

use anyhow::{bail, Result};

use crate::{
    ast::{DeclKind, ExprKind, StmtKind, Type},
    util::{CompilerPass, NodeId, NodeRef},
};

use super::{HIRKind, HirPool, Scope, UntypedHir};

pub struct Binder {
    unbound_functions: HashMap<String, NodeRef>,
    errors: u32,
}

impl CompilerPass for Binder {
    fn inc_error(&mut self) {
        self.errors += 1;
    }
    fn num_errors(&self) -> u32 {
        self.errors
    }
}

impl Binder {
    pub fn new() -> Self {
        Self {
            unbound_functions: HashMap::from([]),
            errors: 0,
        }
    }

    pub fn declare_stdlib(ast: Vec<DeclKind>) -> Vec<DeclKind> {
        let mut deque_ast: VecDeque<DeclKind> = ast.into();
        // print_i
        deque_ast.push_front(DeclKind::FunDecl(
            Type::Void,
            "print_i".to_string(),
            vec![DeclKind::VarDecl(Type::Int, "i".to_string(), None)],
        ));
        // print_c
        deque_ast.push_front(DeclKind::FunDecl(
            Type::Void,
            "print_c".to_string(),
            vec![DeclKind::VarDecl(Type::Char, "c".to_string(), None)],
        ));
        // print_s
        deque_ast.push_front(DeclKind::FunDecl(
            Type::Void,
            "print_s".to_string(),
            vec![DeclKind::VarDecl(
                Type::Pointer(Box::new(Type::Char)),
                "s".to_string(),
                None,
            )],
        ));
        deque_ast.into()
    }

    pub fn bind(&mut self, ast: Vec<DeclKind>) -> Result<HirPool<UntypedHir>> {
        let mut pool = HirPool::new();
        let mut scope = Scope::new();
        let ast = Self::declare_stdlib(ast);
        for decl in ast {
            let bound_decls = self.bind_decl(decl, &mut scope, &mut pool)?;
            for bd in bound_decls {
                pool.set_top(bd);
            }
        }
        Ok(pool)
    }

    fn bind_decl(
        &mut self,
        decl: DeclKind,
        scope: &mut Scope<String, NodeRef>,
        pool: &mut HirPool<UntypedHir>,
    ) -> Result<Vec<NodeRef>> {
        Ok(match decl {
            DeclKind::MultiVarDecl(ty, names, exprs) => {
                let mut refs = vec![];
                for i in 0..names.len() {
                    let name = names.get(i).unwrap().clone();
                    let expr = exprs.get(i).unwrap();
                    let bound_expr = if let Some(expr) = expr {
                        Some(self.bind_expr(expr.clone(), scope, pool)?)
                    } else {
                        None
                    };
                    let decl_kind = HIRKind::VarDecl(ty.clone(), name.clone(), bound_expr);
                    let hir_decl = UntypedHir {
                        id: NodeId::next(),
                        kind: decl_kind,
                    };
                    if let Some(_) = scope.lookup_local(&name) {
                        self.error(format!(
                            "Variable {} has already been declared in this scope.",
                            name.clone()
                        ))?;
                    }
                    let node_ref = pool.add(hir_decl);
                    scope.put(name, node_ref);
                    refs.push(node_ref);
                }
                refs
            }
            DeclKind::VarDecl(ty, name, expr) => {
                let bound_expr = match expr.map(|e| self.bind_expr(e, scope, pool)) {
                    Some(res) => Some(res?),
                    None => None,
                };
                let decl_kind = HIRKind::VarDecl(ty, name.clone(), bound_expr);
                let hir_decl = UntypedHir {
                    id: NodeId::next(),
                    kind: decl_kind,
                };
                if let Some(_) = scope.lookup_local(&name) {
                    self.error(format!(
                        "Variable {} has already been declared in this scope.",
                        name.clone()
                    ))?;
                }
                let node_ref = pool.add(hir_decl);
                scope.put(name, node_ref);
                vec![node_ref]
            }
            DeclKind::StructTypeDecl(ty, name, fields) => {
                let mut struct_scope = Scope::new();
                let mut hir_fields: Vec<NodeRef> = vec![];
                for field in fields {
                    let bound_field = self
                        .bind_decl(field, &mut struct_scope, pool)?
                        .get(0)
                        .unwrap()
                        .clone();
                    hir_fields.push(bound_field);
                }
                let decl_kind = HIRKind::StructTypeDecl(ty, name.clone(), hir_fields);
                let hir_decl = UntypedHir {
                    id: NodeId::next(),
                    kind: decl_kind,
                };
                if let Some(_) = scope.lookup_local(&format!("struct {}", name.clone())) {
                    self.error(format!("Repeat definition of struct {}", name.clone()))?;
                }
                let node_ref = pool.add(hir_decl);
                scope.put(format!("struct {}", name), node_ref);
                vec![node_ref]
            }
            DeclKind::FunDecl(ty, name, params) => {
                let mut param_scope = Scope::new();
                let mut bound_params: Vec<NodeRef> = vec![];
                for param in params {
                    let bound_param = self
                        .bind_decl(param, &mut param_scope, pool)?
                        .get(0)
                        .unwrap()
                        .clone();
                    bound_params.push(bound_param);
                }
                if self.unbound_functions.get(&name).is_some() {
                    self.error(format!(
                        "Duplicate declaration of function {}",
                        name.clone()
                    ))?;
                } else if scope.lookup(&name).is_some() {
                    self.error(format!("Duplicate usage of identifier {}", name.clone()))?;
                }
                let decl_kind = HIRKind::FunDecl(ty, name.clone(), bound_params);
                let hir_decl = UntypedHir {
                    id: NodeId::next(),
                    kind: decl_kind,
                };
                let node_ref = pool.add(hir_decl);
                self.unbound_functions.insert(name.clone(), node_ref);
                scope.put(name.clone(), node_ref);
                vec![node_ref]
            }
            DeclKind::FunDefn {
                decl: unbound_fun_decl,
                block,
            } => {
                let fun_name = match *unbound_fun_decl.clone() {
                    DeclKind::FunDecl(_, name, _) => name,
                    _ => {
                        self.error(format!("Function declaration is not a declaration"))?;
                        "".to_owned()
                    }
                };

                let fun_decl = match self.unbound_functions.get(&fun_name) {
                    Some(fd) => *fd,
                    None => *self
                        .bind_decl(*unbound_fun_decl.clone(), scope, pool)?
                        .get(0)
                        .unwrap(),
                };
                // self.match_decl_defn(fun_decl.clone(), *unbound_fun_decl.clone())?;
                let fun_decl_kind = pool.get(fun_decl).unwrap().kind.clone();
                self.match_decl_defn(fun_decl_kind.clone(), *unbound_fun_decl.clone())?;
                let mut fun_scope = Scope::with_parent(&scope);
                match fun_decl_kind {
                    HIRKind::FunDecl(_, name, params) => {
                        self.unbound_functions.remove(&name);
                        for param in params {
                            let param_kind = pool.get(param).unwrap().kind.clone();
                            match param_kind {
                                HIRKind::VarDecl(_, name, _) => {
                                    fun_scope.put(name.clone(), param.clone());
                                }
                                _ => self.error(format!("Function parameter is not a variable"))?,
                            }
                        }
                    }
                    _ => self.error(format!("Function declaration is not a declaration"))?,
                };
                let stmt_ref = self.bind_stmt(*block, &mut fun_scope, pool)?;
                let decl_kind = HIRKind::FunDefn(fun_decl, *stmt_ref.get(0).unwrap());
                let hir_decl = UntypedHir {
                    id: NodeId::next(),
                    kind: decl_kind,
                };
                let node_ref = pool.add(hir_decl);
                vec![node_ref]
            }
        })
    }

    fn bind_expr(
        &mut self,
        expr: ExprKind,
        scope: &mut Scope<String, NodeRef>,
        pool: &mut HirPool<UntypedHir>,
    ) -> Result<NodeRef> {
        Ok(match expr {
            ExprKind::InvalidExpr => pool.add(UntypedHir {
                id: NodeId::next(),
                kind: HIRKind::Invalid,
            }),
            ExprKind::Literal(l) => pool.add(UntypedHir {
                id: NodeId::next(),
                kind: HIRKind::Literal(l),
            }),
            ExprKind::VarExpr(name) => {
                if let Some(decl) = scope.lookup(&name) {
                    let decl_kind = pool.get(*decl).unwrap().kind.clone();
                    let expr_kind = match decl_kind {
                        HIRKind::VarDecl(_, _, _) => HIRKind::VarExpr(*decl),
                        _ => {
                            self.error(format!(
                                "Identifier {} is not a variable in this scope",
                                name
                            ))?;
                            HIRKind::Invalid
                        }
                    };
                    let hir_expr = UntypedHir {
                        id: NodeId::next(),
                        kind: expr_kind,
                    };
                    pool.add(hir_expr)
                } else {
                    self.error(format!("Variable {} does not exist in this scope", name))?;
                    pool.add(UntypedHir::default())
                }
            }
            ExprKind::BinOp(lhs, op, rhs) => {
                let expr_kind = HIRKind::BinOp(
                    self.bind_expr(*lhs, scope, pool)?,
                    op,
                    self.bind_expr(*rhs, scope, pool)?,
                );
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
            ExprKind::Assign(lhs, rhs) => {
                let expr_kind = HIRKind::Assign(
                    self.bind_expr(*lhs, scope, pool)?,
                    self.bind_expr(*rhs, scope, pool)?,
                );
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
            ExprKind::FunCallExpr(fun_expr, args) => match *fun_expr {
                ExprKind::VarExpr(n) => {
                    let lookup_ref = scope.lookup(&n).copied();
                    if let Some(decl) = lookup_ref {
                        let fun_decl_kind = pool.get(decl).unwrap().kind.clone();
                        match fun_decl_kind {
                            HIRKind::FunDecl(_, _, _) => (),
                            _ => self.error(format!(
                                "Identifier {} not bound to a function in this scope",
                                n
                            ))?,
                        };
                        let mut bound_args: Vec<NodeRef> = vec![];
                        for arg in args {
                            bound_args.push(self.bind_expr(arg, scope, pool)?);
                        }
                        let expr_kind = HIRKind::FunCallExpr(decl, bound_args);
                        let hir_expr = UntypedHir {
                            kind: expr_kind,
                            ..Default::default()
                        };
                        pool.add(hir_expr)
                    } else {
                        self.error(format!("Function {} does not exist in this scope", n))?;
                        pool.add(UntypedHir::default())
                    }
                }
                _ => {
                    self.error(format!("Function pointers are not supported at this time"))?;
                    pool.add(UntypedHir::default())
                }
            },
            ExprKind::TypecastExpr(ty, expr) => {
                let expr_kind = HIRKind::TypecastExpr(ty, self.bind_expr(*expr, scope, pool)?);
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }

            ExprKind::RefExpr(expr) => {
                let expr_kind = HIRKind::RefExpr(self.bind_expr(*expr, scope, pool)?);
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
            ExprKind::DerefExpr(expr) => {
                let expr_kind = HIRKind::DerefExpr(self.bind_expr(*expr, scope, pool)?);
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
            ExprKind::FieldAccessExpr(struct_expr, field) => {
                let expr_kind =
                    HIRKind::FieldAccessExpr(self.bind_expr(*struct_expr, scope, pool)?, field);
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
            ExprKind::ArrayAccessExpr(array_expr, i_expr) => {
                let expr_kind = HIRKind::ArrayAccessExpr(
                    self.bind_expr(*array_expr, scope, pool)?,
                    self.bind_expr(*i_expr, scope, pool)?,
                );
                let hir_expr = UntypedHir {
                    kind: expr_kind,
                    ..Default::default()
                };
                pool.add(hir_expr)
            }
        })
    }

    fn bind_stmt(
        &mut self,
        stmt: StmtKind,
        scope: &mut Scope<String, NodeRef>,
        pool: &mut HirPool<UntypedHir>,
    ) -> Result<Vec<NodeRef>> {
        Ok(match stmt {
            StmtKind::Block { stmts } => {
                let mut bound_stmts = vec![];
                let mut block_scope = Scope::with_parent(&scope);
                for stmt in stmts {
                    bound_stmts.append(&mut self.bind_stmt(stmt, &mut block_scope, pool)?);
                }
                let stmt_kind = HIRKind::Block(bound_stmts);
                let hir_stmt = UntypedHir {
                    kind: stmt_kind,
                    ..Default::default()
                };
                vec![pool.add(hir_stmt)]
            }
            StmtKind::While { expr, stmt } => {
                let bound_expr = self.bind_expr(*expr, scope, pool)?;
                // wrap while stmt in a block
                let block_stmt = StmtKind::Block { stmts: vec![*stmt] };
                // now we can safely assume bound_stmt has length 1
                let bound_stmt = self.bind_stmt(block_stmt, scope, pool)?;
                let stmt_kind = HIRKind::While(bound_expr, *bound_stmt.get(0).unwrap());
                let hir_stmt = UntypedHir {
                    kind: stmt_kind,
                    ..Default::default()
                };
                vec![pool.add(hir_stmt)]
            }
            StmtKind::If { expr, then, els } => {
                let bound_expr = self.bind_expr(*expr, scope, pool)?;
                let block_then = StmtKind::Block { stmts: vec![*then] };
                let bound_then = *self.bind_stmt(block_then, scope, pool)?.get(0).unwrap();
                let bound_els = match els {
                    Some(els) => Some(
                        *self
                            .bind_stmt(StmtKind::Block { stmts: vec![*els] }, scope, pool)?
                            .get(0)
                            .unwrap(),
                    ),
                    None => None,
                };
                let stmt_kind = HIRKind::If(bound_expr, bound_then, bound_els);
                let hir_stmt = UntypedHir {
                    kind: stmt_kind,
                    ..Default::default()
                };
                vec![pool.add(hir_stmt)]
            }
            StmtKind::Decl(decl) => self.bind_decl(decl, scope, pool)?,
            StmtKind::Return(expr) => {
                let bound_expr = match expr {
                    Some(expr) => Some(self.bind_expr(*expr, scope, pool)?),
                    None => None,
                };
                let stmt_kind = HIRKind::Return(bound_expr);
                vec![pool.add(UntypedHir::new(stmt_kind))]
            }
            StmtKind::ExprStmt(expr) => vec![self.bind_expr(*expr, scope, pool)?],
            StmtKind::Break => vec![pool.add(UntypedHir::new(HIRKind::Break))],
            StmtKind::Continue => vec![pool.add(UntypedHir::new(HIRKind::Continue))],
        })
    }

    fn error(&mut self, err: String) -> Result<()> {
        self.inc_error();
        bail!("{}", err);
    }

    fn match_decl_defn(&mut self, fun_decl: HIRKind, fun_defn: DeclKind) -> Result<()> {
        // TODO: match return type and parametres between decl and defn
        Ok(())
    }
}
