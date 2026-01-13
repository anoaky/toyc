use std::{
    collections::{HashMap, VecDeque},
    rc::Rc,
};

use anyhow::{Result, bail};

use crate::{
    ast::{BoundDecl, BoundExpr, BoundStmt, DeclKind, ExprKind, StmtKind, Type},
    util::CompilerPass,
};

use super::Scope;

pub struct Binder {
    unbound_functions: HashMap<String, Rc<BoundDecl>>,
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

    pub fn bind(
        &mut self,
        ast: Vec<DeclKind>,
        scope: &mut Scope<Rc<BoundDecl>>,
    ) -> Result<Vec<Rc<BoundDecl>>> {
        let mut bound_decls = vec![];
        let ast = Self::declare_stdlib(ast);
        for decl in ast {
            let mut bound_decl = self.bind_decl(decl, scope)?;
            bound_decls.append(&mut bound_decl);
        }
        Ok(bound_decls)
    }

    fn bind_decl(
        &mut self,
        decl: DeclKind,
        scope: &mut Scope<Rc<BoundDecl>>,
    ) -> Result<Vec<Rc<BoundDecl>>> {
        Ok(match decl {
            DeclKind::MultiVarDecl(ty, names, exprs) => {
                let mut decls = vec![];
                for i in 0..names.len() {
                    let name = names.get(i).unwrap().clone();
                    let expr = exprs.get(i).unwrap();
                    let bound_expr = if let Some(expr) = expr {
                        Some(self.bind_expr(expr.clone(), scope)?)
                    } else {
                        None
                    };
                    let bound_decl = Rc::new(BoundDecl::VarDecl {
                        ty: ty.clone(),
                        name: name.clone(),
                        expr: bound_expr,
                    });
                    if let Some(_) = scope.put(name.clone(), bound_decl.clone()) {
                        self.error(format!(
                            "Variable {} has already been declared in this scope.",
                            name.clone()
                        ))?;
                    }
                    decls.push(bound_decl);
                }
                decls
            }
            DeclKind::VarDecl(ty, name, expr) => {
                let bound_expr = match expr.map(|e| self.bind_expr(e, scope)) {
                    Some(res) => Some(res?),
                    None => None,
                };
                let bound_decl = Rc::new(BoundDecl::VarDecl {
                    ty,
                    name: name.clone(),
                    expr: bound_expr,
                });
                if let Some(_) = scope.put(name.clone(), bound_decl.clone()) {
                    self.error(format!(
                        "Variable {} has already been declared in this scope.",
                        name.clone()
                    ))?;
                }
                vec![bound_decl]
            }
            DeclKind::StructTypeDecl(ty, name, fields) => {
                let mut struct_scope = Scope::new();
                let mut bound_fields: Vec<Rc<BoundDecl>> = vec![];
                for field in fields {
                    let bound_field = self
                        .bind_decl(field, &mut struct_scope)?
                        .get(0)
                        .unwrap()
                        .clone();
                    bound_fields.push(bound_field);
                }
                let bound_decl = Rc::new(BoundDecl::StructTypeDecl(ty, name.clone(), bound_fields));
                if let Some(_) = scope.put(format!("struct {}", name.clone()), bound_decl.clone()) {
                    self.error(format!("Repeat definition of struct {}", name.clone()))?;
                }
                vec![bound_decl]
            }
            DeclKind::FunDecl(ty, name, params) => {
                let mut param_scope = Scope::new();
                let mut bound_params: Vec<Rc<BoundDecl>> = vec![];
                for param in params {
                    let bound_param = self
                        .bind_decl(param, &mut param_scope)?
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
                let bound_decl = Rc::new(BoundDecl::FunDecl {
                    ty,
                    name: name.clone(),
                    params: bound_params,
                });
                self.unbound_functions
                    .insert(name.clone(), bound_decl.clone());
                scope.put(name.clone(), bound_decl.clone());
                vec![bound_decl]
            }
            DeclKind::FunDefn {
                decl: unbound_fun_decl,
                block,
            } => {
                let mut fun_scope = Scope::with_parent(&scope);
                let fun_name = match *unbound_fun_decl.clone() {
                    DeclKind::FunDecl(_, name, _) => name,
                    _ => {
                        self.error(format!("Function declaration is not a declaration"))?;
                        "".to_owned()
                    }
                };

                let fun_decl = match self.unbound_functions.get(&fun_name) {
                    Some(fd) => fd.clone(),
                    None => self
                        .bind_decl(*unbound_fun_decl.clone(), &mut scope.clone())?
                        .get(0)
                        .unwrap()
                        .clone(),
                };
                self.match_decl_defn(fun_decl.clone(), *unbound_fun_decl.clone())?;
                match &*fun_decl {
                    BoundDecl::FunDecl {
                        ty: _,
                        name,
                        params,
                    } => {
                        self.unbound_functions.remove(name);
                        for param in params {
                            match &**param {
                                BoundDecl::VarDecl {
                                    ty: _,
                                    name,
                                    expr: _,
                                } => {
                                    fun_scope.put(name.clone(), param.clone());
                                }
                                _ => self.error(format!("Function parameter is not a variable"))?,
                            }
                        }
                    }
                    _ => self.error(format!("Function declaration is not a declaration"))?,
                };
                let bound_stmt = self.bind_stmt(*block, &mut fun_scope)?;
                let bound_fun_defn = Rc::new(BoundDecl::FunDefn {
                    decl: fun_decl,
                    block: Box::new(bound_stmt),
                });
                vec![bound_fun_defn]
            }
        })
    }

    fn bind_expr(&mut self, expr: ExprKind, scope: &mut Scope<Rc<BoundDecl>>) -> Result<BoundExpr> {
        Ok(match expr {
            ExprKind::InvalidExpr => BoundExpr::InvalidExpr,
            ExprKind::Literal(l) => BoundExpr::Literal(l),
            ExprKind::VarExpr(name) => {
                if let Some(decl) = scope.lookup(&name) {
                    BoundExpr::VarExpr(decl.clone())
                } else {
                    self.error(format!("Variable {} does not exist in this scope", name))?;
                    BoundExpr::InvalidExpr
                }
            }
            ExprKind::BinOp(lhs, op, rhs) => BoundExpr::BinOp {
                lhs: Box::new(self.bind_expr(*lhs, scope)?),
                op,
                rhs: Box::new(self.bind_expr(*rhs, scope)?),
            },
            ExprKind::Assign(lhs, rhs) => BoundExpr::Assign {
                lhs: Box::new(self.bind_expr(*lhs, scope)?),
                rhs: Box::new(self.bind_expr(*rhs, scope)?),
            },
            ExprKind::FunCallExpr(fun_expr, args) => match *fun_expr {
                ExprKind::VarExpr(n) => {
                    if let Some(decl) = scope.clone().lookup(&n) {
                        let mut bound_args: Vec<BoundExpr> = vec![];
                        for arg in args {
                            bound_args.push(self.bind_expr(arg, scope)?);
                        }
                        BoundExpr::FunCallExpr {
                            defn: decl.clone(),
                            args: bound_args,
                        }
                    } else {
                        self.error(format!("Function {} does not exist in this scope", n))?;
                        BoundExpr::InvalidExpr
                    }
                }
                _ => {
                    self.error(format!("Function pointers are not supported at this time"))?;
                    BoundExpr::InvalidExpr
                }
            },
            ExprKind::TypecastExpr(ty, expr) => BoundExpr::TypecastExpr {
                to: ty,
                expr: Box::new(self.bind_expr(*expr, scope)?),
            },
            ExprKind::RefExpr(expr) => BoundExpr::RefExpr(Box::new(self.bind_expr(*expr, scope)?)),
            ExprKind::DerefExpr(expr) => {
                BoundExpr::DerefExpr(Box::new(self.bind_expr(*expr, scope)?))
            }
            ExprKind::FieldAccessExpr(struct_expr, field) => {
                BoundExpr::FieldAccessExpr(Box::new(self.bind_expr(*struct_expr, scope)?), field)
            }
            ExprKind::ArrayAccessExpr(array_expr, i_expr) => BoundExpr::ArrayAccessExpr(
                Box::new(self.bind_expr(*array_expr, scope)?),
                Box::new(self.bind_expr(*i_expr, scope)?),
            ),
        })
    }

    fn bind_stmt(&mut self, stmt: StmtKind, scope: &mut Scope<Rc<BoundDecl>>) -> Result<BoundStmt> {
        Ok(match stmt {
            StmtKind::Block { stmts } => {
                let mut bound_stmts = vec![];
                for stmt in stmts {
                    bound_stmts.push(self.bind_stmt(stmt, scope)?);
                }
                BoundStmt::Block(bound_stmts)
            }
            StmtKind::While { expr, stmt } => {
                let bound_expr = self.bind_expr(*expr, scope)?;
                let bound_stmt = self.bind_stmt(*stmt, scope)?;
                BoundStmt::While(Box::new(bound_expr), Box::new(bound_stmt))
            }
            StmtKind::If { expr, then, els } => {
                let bound_expr = self.bind_expr(*expr, scope)?;
                let bound_then = self.bind_stmt(*then, scope)?;
                let bound_els = match els {
                    Some(els) => Some(Box::new(self.bind_stmt(*els, scope)?)),
                    None => None,
                };
                BoundStmt::If(Box::new(bound_expr), Box::new(bound_then), bound_els)
            }
            StmtKind::Decl(decl) => {
                let bound_decl = self.bind_decl(decl, scope)?.get(0).unwrap().clone();
                BoundStmt::Decl(bound_decl)
            }
            StmtKind::Return(expr) => {
                let bound_expr = match expr {
                    Some(expr) => Some(Box::new(self.bind_expr(*expr, scope)?)),
                    None => None,
                };
                BoundStmt::Return(bound_expr)
            }
            StmtKind::ExprStmt(expr) => {
                BoundStmt::ExprStmt(Box::new(self.bind_expr(*expr, scope)?))
            }
            StmtKind::Break => BoundStmt::Break,
            StmtKind::Continue => BoundStmt::Continue,
        })
    }

    fn error(&mut self, err: String) -> Result<()> {
        self.inc_error();
        bail!("{}", err);
    }

    fn match_decl_defn(&mut self, fun_decl: Rc<BoundDecl>, fun_defn: DeclKind) -> Result<()> {
        // TODO
        Ok(())
    }
}
