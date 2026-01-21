use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    ast::{Literal, Operator, Type},
    util::{CompilerPass, NodeRef},
};

use super::{HIRKind, HirPool, Scope, TypedHir, UntypedHir};

pub struct Typer {
    untyped_pool: HirPool<UntypedHir>,
    function_declarations: HashMap<String, NodeRef>,
    struct_declarations: HashMap<String, NodeRef>,
    reference_map: HashMap<NodeRef, NodeRef>,
    return_type: Type,
    errors: u32,
}

impl CompilerPass for Typer {
    fn inc_error(&mut self) {
        self.errors += 1;
    }
    fn num_errors(&self) -> u32 {
        self.errors
    }
}

impl Typer {
    pub fn new(untyped_pool: HirPool<UntypedHir>) -> Self {
        Self {
            untyped_pool,
            function_declarations: HashMap::new(),
            struct_declarations: HashMap::new(),
            reference_map: HashMap::new(),
            return_type: Type::None,
            errors: 0,
        }
    }

    pub fn type_all(&mut self) -> Result<HirPool<TypedHir>> {
        let mut pool = HirPool::new();
        let mut scope = Scope::new();
        for top_level_decl_ref in self.untyped_pool.get_tops() {
            let typed_tld_ref = self.type_node(top_level_decl_ref, &mut scope, &mut pool)?;
            pool.set_top(typed_tld_ref);
        }
        Ok(pool)
    }

    fn error(&mut self, err: String) -> Result<()> {
        self.inc_error();
        bail!("{}", err);
    }

    fn type_node(
        &mut self,
        untyped_node_ref: NodeRef,
        scope: &mut Scope<NodeRef, Type>,
        pool: &mut HirPool<TypedHir>,
    ) -> Result<NodeRef> {
        let node = self
            .untyped_pool
            .get(untyped_node_ref)
            .expect("Invalid NodeRef passed to type_node");
        let typed_node_ref = match node.kind.clone() {
            HIRKind::Invalid => pool.add(TypedHir::default()),
            HIRKind::VarDecl(ty, name, value) => {
                let typed_value = if let Some(value) = value {
                    let typed_value = self.type_node(value, scope, pool)?;
                    let value_type = pool.get(typed_value).unwrap().ty.clone();
                    if value_type != ty {
                        self.error(format!(
                            "Value for variable {} has type {} instead of defined type {}",
                            name, value_type, ty
                        ))?;
                    }
                    Some(typed_value)
                } else {
                    None
                };
                let nr = pool.add(TypedHir::new(
                    ty.clone(),
                    HIRKind::VarDecl(ty.clone(), name.clone(), typed_value),
                ));
                scope.put(nr, ty.clone());
                nr
            }
            HIRKind::StructTypeDecl(ty, name, fields) => {
                let mut typed_fields = vec![];
                for field_ref in fields {
                    typed_fields.push(self.type_node(field_ref, scope, pool)?);
                }
                let nr = pool.add(TypedHir::new(
                    ty.clone(),
                    HIRKind::StructTypeDecl(ty.clone(), name.clone(), typed_fields),
                ));
                scope.put(nr, ty.clone());
                self.struct_declarations.insert(name, nr);
                nr
            }
            HIRKind::FunDecl(ty, name, params) => {
                let mut typed_params = vec![];
                for param in params {
                    typed_params.push(self.type_node(param, scope, pool)?);
                }
                let nr = pool.add(TypedHir::new(
                    ty.clone(),
                    HIRKind::FunDecl(ty.clone(), name.clone(), typed_params),
                ));
                scope.put(nr, ty.clone());
                self.function_declarations.insert(name, nr);
                nr
            }
            HIRKind::FunDefn(untyped_decl_ref, block) => {
                let untyped_decl = self.untyped_pool.get(untyped_decl_ref).unwrap();
                let fun_name = match untyped_decl.kind.clone().clone() {
                    HIRKind::FunDecl(_, name, _) => name.clone(),
                    _ => unreachable!(),
                };
                let typed_decl_ref = if let Some(decl) = self.function_declarations.get(&fun_name) {
                    *decl
                } else {
                    self.type_node(untyped_decl_ref, scope, pool)?
                };
                let typed_decl = pool.get(typed_decl_ref).unwrap();
                let typed_decl_ty = typed_decl.ty.clone();
                self.return_type = typed_decl_ty.clone();
                let typed_block_ref = self.type_node(block, scope, pool)?;
                self.return_type = Type::None;
                let nr = pool.add(TypedHir::new(
                    typed_decl_ty.clone(),
                    HIRKind::FunDefn(typed_decl_ref, typed_block_ref),
                ));
                scope.put(nr, typed_decl_ty.clone());
                nr
            }
            HIRKind::Literal(l) => {
                let ty = match l {
                    Literal::Int(_) | Literal::Sizeof(_) => Type::Int,
                    Literal::Char(_) => Type::Char,
                    Literal::Str(_) => Type::Pointer(Box::new(Type::Char)),
                };
                let nr = pool.add(TypedHir::new(ty.clone(), HIRKind::Literal(l)));
                scope.put(nr, ty.clone());
                nr
            }
            HIRKind::VarExpr(untyped_decl_ref) => {
                let typed_decl_ref = self.reference_map.get(&untyped_decl_ref).unwrap();
                let var_type = scope.lookup(typed_decl_ref).unwrap().clone();
                let nr = pool.add(TypedHir::new(
                    var_type.clone(),
                    HIRKind::VarExpr(*typed_decl_ref),
                ));
                scope.put(nr, var_type.clone());
                nr
            }
            HIRKind::BinOp(lhs, op, rhs) => {
                let typed_lhs_ref = self.type_node(lhs, scope, pool)?;
                let typed_rhs_ref = self.type_node(rhs, scope, pool)?;
                let lhs_type = scope.lookup(&typed_lhs_ref).unwrap().clone();
                let rhs_type = scope.lookup(&typed_rhs_ref).unwrap().clone();
                if lhs_type != rhs_type {
                    self.error(format!(
                        "LHS of binary operator has type {} and RHS has type {}",
                        lhs_type, rhs_type
                    ))?;
                    let nr = pool.add(TypedHir::default());
                    scope.put(nr, Type::Unknown);
                    nr
                } else {
                    use Operator::*;
                    use Type::*;
                    match (lhs_type.clone(), op) {
                        (Void | Struct(_) | Array(_, _), Eq | Ne)
                        | (
                            Char | Void | Struct(_) | Pointer(_) | Array(_, _) | Unknown | None,
                            Add | Sub | Mul | Div | Mod | Gt | Ge | Lt | Le | And | Or,
                        ) => {
                            self.error(format!(
                                "Operator {} not compatible with type {}",
                                op,
                                lhs_type.clone()
                            ))?;
                            let nr = pool.add(TypedHir::default());
                            scope.put(nr, Type::Unknown);
                            nr
                        }
                        _ => {
                            let nr = pool.add(TypedHir::new(
                                Type::Int,
                                HIRKind::BinOp(typed_lhs_ref, op, typed_rhs_ref),
                            ));
                            scope.put(nr, Type::Int);
                            nr
                        }
                    }
                }
            }
            HIRKind::Assign(lhs, rhs) => {
                let typed_lhs_ref = self.type_node(lhs, scope, pool)?;
                let typed_rhs_ref = self.type_node(rhs, scope, pool)?;
                let lhs_type = scope.lookup(&typed_lhs_ref).unwrap().clone();
                let rhs_type = scope.lookup(&typed_rhs_ref).unwrap().clone();
                if lhs_type != rhs_type {
                    self.error(format!(
                        "Mismatched assign: LHS has type {} and RHS has type {}",
                        lhs_type, rhs_type
                    ))?;
                }
                match lhs_type.clone() {
                    Type::Void | Type::Array(_, _) => {
                        self.error(format!("Cannot assign value of type {}", lhs_type.clone()))?;
                        let nr = pool.add(TypedHir::default());
                        scope.put(nr, Type::Unknown);
                        nr
                    }
                    _ => {
                        let nr = pool.add(TypedHir::new(
                            lhs_type.clone(),
                            HIRKind::Assign(typed_lhs_ref, typed_rhs_ref),
                        ));
                        scope.put(nr, lhs_type.clone());
                        nr
                    }
                }
            }
            HIRKind::FunCallExpr(untyped_decl_ref, untyped_args_refs) => {
                let typed_decl_ref = *self.reference_map.get(&untyped_decl_ref).unwrap();
                let fun_type = scope.lookup(&typed_decl_ref).unwrap().clone();
                let typed_params_refs = match pool.get(typed_decl_ref).unwrap().kind.clone() {
                    HIRKind::FunDecl(_, _, params) => params.clone(),
                    _ => unreachable!(),
                };
                let mut typed_args_refs = vec![];
                for i in 0..untyped_args_refs.len() {
                    let typed_arg_ref =
                        self.type_node(*untyped_args_refs.get(i).unwrap(), scope, pool)?;
                    let arg_type = scope.lookup(&typed_arg_ref).unwrap().clone();
                    let param_type = scope
                        .lookup(typed_params_refs.get(i).unwrap())
                        .unwrap()
                        .clone();
                    if arg_type != param_type {
                        self.error(format!(
                            "Argument at position {} has type {}, expected {}",
                            i, arg_type, param_type
                        ))?;
                    }
                    typed_args_refs.push(typed_arg_ref);
                }
                let nr = pool.add(TypedHir::new(
                    fun_type.clone(),
                    HIRKind::FunCallExpr(typed_decl_ref, typed_args_refs),
                ));
                scope.put(nr, fun_type.clone());
                nr
            }
            HIRKind::TypecastExpr(cast_to, untyped_expr_ref) => {
                let typed_expr_ref = self.type_node(untyped_expr_ref, scope, pool)?;
                let src_type = scope.lookup(&typed_expr_ref).unwrap();
                let ty = match (src_type.clone(), cast_to.clone()) {
                    (Type::Char, Type::Int) => Type::Int,
                    (Type::Array(_, t1) | Type::Pointer(t1), Type::Pointer(t2)) if *t1 == *t2 => {
                        Type::Pointer(t2)
                    }
                    _ => {
                        self.error(format!(
                            "Invalid cast from {} to {}",
                            src_type.clone(),
                            cast_to.clone()
                        ))?;
                        Type::Unknown
                    }
                };
                let nr = pool.add(TypedHir::new(
                    ty.clone(),
                    HIRKind::TypecastExpr(ty.clone(), typed_expr_ref),
                ));
                scope.put(nr, ty.clone());
                nr
            }
            HIRKind::RefExpr(untyped_expr_ref) => {
                let typed_expr_ref = self.type_node(untyped_expr_ref, scope, pool)?;
                let src_type = scope.lookup(&typed_expr_ref).unwrap().clone();
                let ty = Type::Pointer(Box::new(src_type));
                let nr = pool.add(TypedHir::new(ty.clone(), HIRKind::RefExpr(typed_expr_ref)));
                scope.put(nr, ty.clone());
                nr
            }
            HIRKind::DerefExpr(untyped_expr_ref) => {
                let typed_expr_ref = self.type_node(untyped_expr_ref, scope, pool)?;
                let src_type = scope.lookup(&typed_expr_ref).unwrap().clone();
                let ty = match src_type.clone() {
                    Type::Pointer(t) => *t,
                    _ => {
                        self.error(format!(
                            "Attempt to dereference non-pointer type {}",
                            src_type
                        ))?;
                        Type::Unknown
                    }
                };
                let nr = pool.add(TypedHir::new(
                    ty.clone(),
                    HIRKind::DerefExpr(typed_expr_ref),
                ));
                scope.put(nr, ty.clone());
                nr
            }
            HIRKind::FieldAccessExpr(untyped_expr_ref, field) => {
                let typed_expr_ref = self.type_node(untyped_expr_ref, scope, pool)?;
                let expr_type = scope.lookup(&typed_expr_ref).unwrap().clone();
                let name = match expr_type.clone() {
                    Type::Struct(n) => n.clone(),
                    _ => {
                        self.error(format!(
                            "Attempt to access field for non-struct type {}",
                            expr_type
                        ))?;
                        unreachable!()
                    }
                };
                let struct_decl_ref = *self.struct_declarations.get(&name).unwrap();
                let fields = match pool.get(struct_decl_ref).unwrap().kind.clone() {
                    HIRKind::StructTypeDecl(_, _, fields) => fields.clone(),
                    _ => unreachable!(),
                };
                let field_type = fields.iter().find_map(|field_ref| {
                    let field_kind = pool.get(*field_ref).unwrap().kind.clone();
                    match field_kind {
                        HIRKind::VarDecl(ty, name, _) if name == field => Some(ty),
                        _ => None,
                    }
                });
                match field_type {
                    Some(t) => {
                        let nr = pool.add(TypedHir::new(
                            t.clone(),
                            HIRKind::FieldAccessExpr(typed_expr_ref, field.clone()),
                        ));
                        scope.put(nr, t.clone());
                        nr
                    }
                    None => {
                        self.error(format!(
                            "No field {} found in struct {}",
                            field.clone(),
                            name.clone()
                        ))?;
                        unreachable!()
                    }
                }
            }
            HIRKind::ArrayAccessExpr(untyped_arr_ref, untyped_ind_ref) => {
                let typed_arr_ref = self.type_node(untyped_arr_ref, scope, pool)?;
                let typed_ind_ref = self.type_node(untyped_ind_ref, scope, pool)?;
                let arr_type = scope.lookup(&typed_arr_ref).unwrap().clone();
                let ind_type = scope.lookup(&typed_ind_ref).unwrap().clone();
                let element_type = match arr_type {
                    Type::Array(_, t) => *t,
                    _ => {
                        self.error(format!("Attempt to index non-array type {}", arr_type))?;
                        Type::Unknown
                    }
                };
                if ind_type != Type::Int {
                    self.error(format!(
                        "Attempt to index with non-integer type {}",
                        ind_type
                    ))?;
                }
                let nr = pool.add(TypedHir::new(
                    element_type.clone(),
                    HIRKind::ArrayAccessExpr(typed_arr_ref, typed_ind_ref),
                ));
                scope.put(nr, element_type.clone());
                nr
            }
            HIRKind::Block(untyped_stmts_refs) => {
                let mut typed_stmts_refs = vec![];
                for stmt_ref in untyped_stmts_refs {
                    typed_stmts_refs.push(self.type_node(stmt_ref, scope, pool)?);
                }
                let nr = pool.add(TypedHir::new(Type::None, HIRKind::Block(typed_stmts_refs)));
                scope.put(nr, Type::None);
                nr
            }
            HIRKind::While(untyped_expr_ref, untyped_stmt_ref) => {
                let typed_expr_ref = self.type_node(untyped_expr_ref, scope, pool)?;
                let expr_type = scope.lookup(&typed_expr_ref).unwrap().clone();
                if expr_type != Type::Int {
                    self.error(format!(
                        "While loop expects expression of type int, got {}",
                        expr_type
                    ))?;
                }
                let typed_stmt_ref = self.type_node(untyped_stmt_ref, scope, pool)?;
                let nr = pool.add(TypedHir::new(
                    Type::None,
                    HIRKind::While(typed_expr_ref, typed_stmt_ref),
                ));
                scope.put(nr, Type::None);
                nr
            }
            HIRKind::If(expr_ref, then_ref, els_ref) => {
                let typed_expr_ref = self.type_node(expr_ref, scope, pool)?;
                let expr_type = scope.lookup(&typed_expr_ref).unwrap().clone();
                if expr_type != Type::Int {
                    self.error(format!(
                        "If statement expects expression of type int, got {}",
                        expr_type
                    ))?;
                }
                let typed_then_ref = self.type_node(then_ref, scope, pool)?;
                let typed_els_ref = if let Some(els_ref) = els_ref {
                    Some(self.type_node(els_ref, scope, pool)?)
                } else {
                    None
                };
                let nr = pool.add(TypedHir::new(
                    Type::None,
                    HIRKind::If(typed_expr_ref, typed_then_ref, typed_els_ref),
                ));
                scope.put(nr, Type::None);
                nr
            }
            HIRKind::Return(ret_ref) => {
                let typed_ret_ref = match (ret_ref, self.return_type.clone()) {
                    (None, Type::Void) => None,
                    (Some(e), _) => {
                        let typed_ret_ref = self.type_node(e, scope, pool)?;
                        let ret_expr_type = scope.lookup(&typed_ret_ref).unwrap().clone();
                        if ret_expr_type != self.return_type {
                            self.error(format!(
                                "Function should return type {}, got {}",
                                self.return_type, ret_expr_type
                            ))?;
                        }
                        Some(typed_ret_ref)
                    }
                    (None, _) => {
                        self.error(format!(
                            "Function should return type {}, got void",
                            self.return_type
                        ))?;
                        None
                    }
                };
                pool.add(TypedHir::new(Type::None, HIRKind::Return(typed_ret_ref)))
            }
            HIRKind::Break => pool.add(TypedHir::new(Type::None, HIRKind::Break)),
            HIRKind::Continue => pool.add(TypedHir::new(Type::None, HIRKind::Continue)),
        };
        self.reference_map.insert(untyped_node_ref, typed_node_ref);
        Ok(typed_node_ref)
    }
}
