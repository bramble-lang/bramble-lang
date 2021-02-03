use std::collections::HashMap;

use crate::syntax::{
    path::Path,
    statement::{Bind, Mutate, Yield, YieldReturn},
};
use crate::syntax::{
    statement::{Return, Statement},
    ty::Type,
};
use crate::{
    diagnostics::config::{Tracing, TracingConfig},
    expression,
};
use crate::{
    expression::{BinaryOperator, Expression, UnaryOperator},
    syntax::{
        module::{self, Item},
        routinedef,
    },
};
use crate::{parser::parser::ParserInfo, semantics::symbol_table::*, syntax::structdef};
use crate::{
    semantics::semanticnode::{SemanticAst, SemanticNode},
    syntax::module::Module,
    syntax::statement,
};
use braid_lang::result::Result;
use Type::*;

use super::{semanticnode::SemanticAnnotations, stack::SymbolTableScopeStack};

pub fn resolve_types(
    ast: &module::Module<ParserInfo>,
    trace: TracingConfig,
    trace_path: TracingConfig,
) -> Result<module::Module<SemanticAnnotations>> {
    resolve_types_with_imports(ast, &vec![], trace, trace_path)
}

pub fn resolve_types_with_imports(
    ast: &module::Module<ParserInfo>,
    imported_functions: &Vec<(Path, Vec<Type>, Type)>,
    trace: TracingConfig,
    trace_path: TracingConfig,
) -> Result<module::Module<SemanticAnnotations>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(&ast)?;
    SymbolTable::add_item_defs_to_table(&mut sm_ast)?;

    let mut semantic = TypeResolver::new(&sm_ast);

    for (name, params, ret_ty) in imported_functions.into_iter() {
        semantic.import_function(name.clone(), params.clone(), ret_ty.clone());
    }

    semantic.set_tracing(trace);
    semantic.path_tracing = trace_path;
    semantic.resolve_types()
}

pub struct TypeResolver<'a> {
    root: &'a Module<SemanticAnnotations>,
    stack: SymbolTableScopeStack, // I think I can move this into a Cell<> and then make `resolve_types` into &self instead of &mut self
    tracing: TracingConfig,
    path_tracing: TracingConfig,
    imported_symbols: HashMap<String, Symbol>,
}

impl<'a> Tracing for TypeResolver<'a> {
    fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }
}

impl<'a> TypeResolver<'a> {
    pub fn new(root: &'a Module<SemanticAnnotations>) -> TypeResolver {
        TypeResolver {
            root,
            stack: SymbolTableScopeStack::new(),
            tracing: TracingConfig::Off,
            path_tracing: TracingConfig::Off,
            imported_symbols: HashMap::new(),
        }
    }

    pub fn import_function(
        &mut self,
        canonical_name: Path,
        params: Vec<Type>,
        return_ty: Type,
    ) -> Option<Symbol> {
        match canonical_name.item() {
            Some(item) => self.imported_symbols.insert(
                canonical_name.to_string(),
                Symbol {
                    name: item.into(),
                    ty: Type::FunctionDef(params, Box::new(return_ty)),
                    mutable: false,
                },
            ),
            None => None,
        }
    }

    pub fn resolve_types(&mut self) -> Result<module::Module<SemanticAnnotations>> {
        let mut empty_table = SymbolTable::new();
        self.analyze_module(self.root, &mut empty_table)
            .map_err(|e| format!("Semantic: {}", e))
    }

    fn get_imported_symbol(&self, canonical_name: &Path) -> Option<&Symbol> {
        self.imported_symbols.get(&canonical_name.to_string())
    }

    fn analyze_module(
        &mut self,
        m: &module::Module<SemanticAnnotations>,
        sym: &mut SymbolTable,
    ) -> Result<module::Module<SemanticAnnotations>> {
        let mut nmodule = module::Module::new(m.get_name(), m.get_annotations().clone());
        let mut meta = nmodule.get_annotations_mut().clone();

        let tmp_sym = sym.clone();
        self.stack.push(tmp_sym);

        *nmodule.get_modules_mut() = m
            .get_modules()
            .iter()
            .map(|m| self.analyze_module(m, &mut meta.sym))
            .collect::<Result<Vec<module::Module<SemanticAnnotations>>>>()?;
        *nmodule.get_functions_mut() = m
            .get_functions()
            .iter()
            .map(|f| self.analyze_item(f, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticAnnotations>>>>()?;
        *nmodule.get_coroutines_mut() = m
            .get_coroutines()
            .iter()
            .map(|c| self.analyze_item(c, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticAnnotations>>>>()?;
        *nmodule.get_structs_mut() = m
            .get_structs()
            .iter()
            .map(|s| self.analyze_item(s, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticAnnotations>>>>()?;

        self.stack.pop();

        meta.ty = Unit;
        *nmodule.get_annotations_mut() = meta;
        Ok(nmodule)
    }

    fn analyze_item(
        &mut self,
        i: &module::Item<SemanticAnnotations>,
        sym: &mut SymbolTable,
    ) -> Result<module::Item<SemanticAnnotations>> {
        match i {
            Item::Struct(s) => self.analyze_structdef(s, sym).map(|s2| Item::Struct(s2)),
            Item::Routine(r) => self.analyze_routine(r, sym).map(|r2| Item::Routine(r2)),
        }
    }

    fn analyze_routine(
        &mut self,
        routine: &routinedef::RoutineDef<SemanticAnnotations>,
        sym: &mut SymbolTable,
    ) -> Result<routinedef::RoutineDef<SemanticAnnotations>> {
        let routinedef::RoutineDef {
            annotations,
            name,
            def,
            params,
            body,
            ty: p,
            ..
        } = routine;
        let mut meta = annotations.clone();
        let canonical_params = self.params_to_canonical(sym, &params)?;
        for (pname, pty) in canonical_params.iter() {
            meta.sym.add(pname, pty.clone(), false)?;
        }
        let tmp_sym = sym.clone();
        self.stack.push(tmp_sym);
        let mut resolved_body = vec![];
        for stmt in body.iter() {
            let exp = self.analyze_statement(stmt, &Some(name.clone()), &mut meta.sym)?;
            resolved_body.push(exp);
        }
        self.stack.pop();
        meta.ty = self.type_to_canonical(sym, p)?;

        let canonical_ret_ty = self.type_to_canonical(sym, &meta.ty)?;

        let canon_path = self
            .stack
            .to_path(sym)
            .map(|mut p| {
                p.push(name);
                p
            })
            .expect("Failed to create canonical path for function");
        meta.set_canonical_path(canon_path);
        Ok(routinedef::RoutineDef {
            annotations: meta.clone(),
            def: def.clone(),
            name: name.clone(),
            params: canonical_params,
            ty: canonical_ret_ty,
            body: resolved_body,
        })
    }

    fn analyze_structdef(
        &mut self,
        struct_def: &structdef::StructDef<SemanticAnnotations>,
        sym: &mut SymbolTable,
    ) -> Result<structdef::StructDef<SemanticAnnotations>> {
        // Check the type of each member
        let fields = struct_def.get_fields();
        for (field_name, field_type) in fields.iter() {
            if let Custom(ty_name) = field_type {
                self.lookup_symbol_by_path(sym, ty_name).map_err(|e| {
                    format!(
                        "member {}.{} invalid: {}",
                        struct_def.get_name(),
                        field_name,
                        e
                    )
                })?;
            }
        }

        // Update all fields so that their types use the full canonical path of the type
        let canonical_fields = self.params_to_canonical(sym, &fields)?;

        // Update the annotations with canonical path information and set the type to Unit
        let mut meta = struct_def.get_annotations().clone();
        meta.ty = Unit;
        meta.set_canonical_path(
            self.to_canonical(sym, &vec![struct_def.get_name().clone()].into())?,
        );

        Ok(structdef::StructDef::new(
            struct_def.get_name().clone(),
            meta.clone(),
            canonical_fields,
        ))
    }

    fn analyze_statement(
        &mut self,
        stmt: &Statement<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<Statement<SemanticAnnotations>> {
        use statement::Statement::*;
        let inner = match stmt {
            Bind(box b) => Bind(Box::new(self.analyze_bind(b, current_func, sym)?)),
            Mutate(box b) => Mutate(Box::new(self.analyze_mutate(b, current_func, sym)?)),
            Return(box x) => Return(Box::new(self.analyze_return(x, current_func, sym)?)),
            YieldReturn(box x) => {
                YieldReturn(Box::new(self.analyze_yieldreturn(x, current_func, sym)?))
            }
            Expression(box e) => Expression(Box::new(self.traverse(e, current_func, sym)?)),
        };

        Ok(inner)
    }

    fn analyze_bind(
        &mut self,
        bind: &Bind<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<Bind<SemanticAnnotations>> {
        let meta = bind.get_annotations();
        let rhs = bind.get_rhs();
        let result = match current_func {
            Some(_) => {
                let mut meta = meta.clone();
                meta.ty = self.type_to_canonical(sym, bind.get_type())?;
                let rhs = self.traverse(rhs, current_func, sym)?;
                if meta.ty == rhs.get_type() {
                    match sym.add(bind.get_id(), meta.ty.clone(), bind.is_mutable()) {
                        Ok(()) => Ok(Bind::new(
                            meta,
                            bind.get_id(),
                            bind.get_type().clone(),
                            bind.is_mutable(),
                            rhs,
                        )),
                        Err(e) => Err(e),
                    }
                } else {
                    Err(format!(
                        "Bind expected {} but got {}",
                        meta.ty,
                        rhs.get_type()
                    ))
                }
            }
            None => Err(format!(
                "Attempting to bind variable {} outside of function",
                bind.get_id()
            )),
        };
        result.map_err(|e| format!("L{}: {}", bind.get_annotations().ln, e))
    }

    fn analyze_mutate(
        &mut self,
        mutate: &Mutate<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<Mutate<SemanticAnnotations>> {
        let result = match current_func {
            Some(_) => {
                let mut meta = mutate.get_annotations().clone();
                let rhs = self.traverse(mutate.get_rhs(), current_func, sym)?;
                match self.lookup_var(sym, mutate.get_id()) {
                    Ok(symbol) => {
                        if symbol.mutable {
                            if symbol.ty == rhs.get_type() {
                                meta.ty = rhs.get_type().clone();
                                Ok(Mutate::new(meta, mutate.get_id(), rhs))
                            } else {
                                Err(format!(
                                    "{} is of type {} but is assigned {}",
                                    mutate.get_id(),
                                    symbol.ty,
                                    rhs.get_type()
                                ))
                            }
                        } else {
                            Err(format!("Variable {} is not mutable", mutate.get_id()))
                        }
                    }
                    Err(e) => Err(e),
                }
            }
            None => Err(format!(
                "Attempting to mutate a variable {} outside of function",
                mutate.get_id()
            )),
        };
        result.map_err(|e| format!("L{}: {}", mutate.get_annotations().ln, e))
    }

    fn analyze_yieldreturn(
        &mut self,
        yr: &YieldReturn<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<YieldReturn<SemanticAnnotations>> {
        let result = match current_func {
            None => Err(format!("yret appears outside of function")),
            Some(cf) => {
                let mut meta = yr.get_annotations().clone();
                match yr.get_value() {
                    None => {
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        if *ret_ty == Unit {
                            meta.ty = Unit;
                            Ok(YieldReturn::new(meta, None))
                        } else {
                            Err(format!("Yield return expected {} but got unit", ret_ty))
                        }
                    }
                    Some(val) => {
                        let exp = self.traverse(val, current_func, sym)?;
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        let ret_ty = self.type_to_canonical(sym, ret_ty)?;
                        if ret_ty == exp.get_type() {
                            meta.ty = ret_ty;
                            Ok(YieldReturn::new(meta, Some(exp)))
                        } else {
                            Err(format!(
                                "Yield return expected {} but got {}",
                                ret_ty,
                                exp.get_type()
                            ))
                        }
                    }
                }
            }
        };
        result.map_err(|e| format!("L{}: {}", yr.get_annotations().ln, e))
    }

    fn analyze_return(
        &mut self,
        r: &Return<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<Return<SemanticAnnotations>> {
        let result = match current_func {
            None => Err(format!("return appears outside of function")),
            Some(cf) => {
                let mut meta = r.get_annotations().clone();
                match r.get_value() {
                    None => {
                        let (_, ret_ty) = self.lookup_func_or_cor(sym, cf)?;
                        if *ret_ty == Unit {
                            meta.ty = Unit;
                            Ok(Return::new(meta, None))
                        } else {
                            Err(format!("Return expected {} but got unit", ret_ty))
                        }
                    }
                    Some(val) => {
                        let exp = self.traverse(val, current_func, sym)?;
                        let (_, ret_ty) = self.lookup_func_or_cor(sym, cf)?;
                        let ret_ty = self.type_to_canonical(sym, ret_ty)?;
                        if ret_ty == exp.get_type() {
                            meta.ty = ret_ty;
                            Ok(Return::new(meta, Some(exp)))
                        } else {
                            Err(format!(
                                "Return expected {} but got {}",
                                ret_ty,
                                exp.get_type()
                            ))
                        }
                    }
                }
            }
        };
        result.map_err(|e| format!("L{}: {}", r.get_annotations().ln, e))
    }

    fn traverse(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        self.trace(ast, current_func, sym);
        self.analyze_expression(ast, current_func, sym)
            .map_err(|e| {
                if !e.starts_with("L") {
                    format!("L{}: {}", ast.get_annotations().ln, e)
                } else {
                    e
                }
            })
    }

    fn analyze_expression(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        match &ast {
            &Expression::Integer64(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = I64;
                Ok(Expression::Integer64(meta, *v))
            }
            Expression::Boolean(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Bool;
                Ok(Expression::Boolean(meta.clone(), *v))
            }
            Expression::StringLiteral(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::StringLiteral;
                Ok(Expression::StringLiteral(meta.clone(), v.clone()))
            }
            Expression::CustomType(meta, name) => {
                let mut meta = meta.clone();
                meta.ty = Custom(name.clone());
                Ok(Expression::CustomType(meta.clone(), name.clone()))
            }
            Expression::IdentifierDeclare(meta, name, p) => {
                let mut meta = meta.clone();
                meta.ty = p.clone();
                Ok(Expression::IdentifierDeclare(
                    meta.clone(),
                    name.clone(),
                    p.clone(),
                ))
            }
            Expression::Identifier(meta, id) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(_) => {
                    let mut meta = meta.clone();
                    match self.lookup_var(sym, &id)? {
                        Symbol { ty: p, .. } => meta.ty = self.type_to_canonical(sym, p)?,
                    };
                    Ok(Expression::Identifier(meta.clone(), id.clone()))
                }
            },
            Expression::Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            Expression::MemberAccess(meta, src, member) => {
                let mut meta = meta.clone();
                // Get the type of src and look up its struct definition
                // Check the struct definition for the type of `member`
                // if it exists, if it does not exist then return an error
                let src = self.traverse(&src, current_func, sym)?;
                match src.get_type() {
                    Custom(struct_name) => {
                        let (struct_def, canonical_path) =
                            self.lookup_symbol_by_path(sym, &struct_name)?;
                        let member_ty = struct_def
                            .ty
                            .get_member(&member)
                            .ok_or(format!("{} does not have member {}", struct_name, member))?;
                        meta.ty =
                            Self::type_to_canonical_with_path(&canonical_path.parent(), member_ty)?;
                        meta.set_canonical_path(canonical_path);
                        Ok(Expression::MemberAccess(
                            meta,
                            Box::new(src),
                            member.clone(),
                        ))
                    }
                    _ => Err(format!("Type {} does not have members", src.get_type())),
                }
            }
            Expression::BinaryOp(meta, op, l, r) => {
                let mut meta = meta.clone();
                let (ty, l, r) = self.binary_op(*op, &l, &r, current_func, sym)?;
                meta.ty = ty;
                Ok(Expression::BinaryOp(
                    meta.clone(),
                    *op,
                    Box::new(l),
                    Box::new(r),
                ))
            }
            Expression::UnaryOp(meta, op, operand) => {
                let mut meta = meta.clone();
                let (ty, operand) = self.unary_op(*op, &operand, current_func, sym)?;
                meta.ty = ty;
                Ok(Expression::UnaryOp(meta.clone(), *op, Box::new(operand)))
            }
            Expression::If {
                annotation: meta,
                cond,
                if_arm,
                else_arm,
            } => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond, current_func, sym)?;
                if cond.get_type() == Bool {
                    let if_arm = self.traverse(&if_arm, current_func, sym)?;

                    let else_arm = else_arm
                        .as_ref()
                        .map(|e| self.traverse(&e, current_func, sym))
                        .map_or(Ok(None), |r| r.map(|x| Some(box x)))?;

                    let else_arm_ty = else_arm
                        .as_ref()
                        .map(|e| e.get_type().clone())
                        .unwrap_or(Type::Unit);

                    if if_arm.get_type() == else_arm_ty {
                        meta.ty = if_arm.get_type().clone();
                        Ok(Expression::If {
                            annotation: meta.clone(),
                            cond: box cond,
                            if_arm: box if_arm,
                            else_arm: else_arm,
                        })
                    } else {
                        Err(format!(
                            "If expression has mismatching arms: expected {} got {}",
                            if_arm.get_type(),
                            else_arm_ty
                        ))
                    }
                } else {
                    Err(format!(
                        "Expected boolean expression in if conditional, got: {}",
                        cond.get_type()
                    ))
                }
            }
            Expression::Yield(meta, exp) => match current_func {
                None => Err(format!("Yield appears outside of function")),
                Some(_) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    meta.ty = match exp.get_type() {
                        Coroutine(ret_ty) => self.type_to_canonical(sym, ret_ty)?,
                        _ => return Err(format!("Yield expects co<_> but got {}", exp.get_type())),
                    };
                    Ok(Expression::Yield(meta, Box::new(exp)))
                }
            },
            Expression::RoutineCall(meta, call, routine_path, params) => {
                let mut meta = meta.clone();
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut resolved_params = vec![];
                for param in params.iter() {
                    let ty = self.traverse(param, current_func, sym)?;

                    resolved_params.push(ty);
                }

                // Check that the function being called exists
                let (symbol, routine_canon_path) = self.lookup_symbol_by_path(sym, routine_path)?;

                let (expected_param_tys, ret_ty) =
                    Self::extract_routine_type_info(symbol, call, &routine_path)?;
                let expected_param_tys = expected_param_tys
                    .iter()
                    .map(|pty| Self::type_to_canonical_with_path(&routine_canon_path.parent(), pty))
                    .collect::<Result<Vec<Type>>>()?;

                // Check that parameters are correct and if so, return the node annotated with
                // semantic information
                if resolved_params.len() != expected_param_tys.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                        routine_path,
                        expected_param_tys.len(),
                        resolved_params.len(),
                    ))
                } else {
                    match Self::check_for_invalid_routine_parameters(
                        &routine_path,
                        &resolved_params,
                        &expected_param_tys,
                    ) {
                        Err(msg) => Err(msg),
                        Ok(()) => {
                            meta.ty = self.type_to_canonical(sym, &ret_ty)?;
                            Ok(Expression::RoutineCall(
                                meta.clone(),
                                *call,
                                routine_canon_path,
                                resolved_params,
                            ))
                        }
                    }
                }
            }
            Expression::ExpressionBlock(meta, body, final_exp) => {
                let mut meta = meta.clone();
                let mut resolved_body = vec![];
                let tmp_sym = sym.clone();
                self.stack.push(tmp_sym);
                for stmt in body.iter() {
                    let exp = self.analyze_statement(stmt, current_func, &mut meta.sym)?;
                    resolved_body.push(exp);
                }

                let (final_exp, block_ty) = match final_exp {
                    None => (None, Unit),
                    Some(fe) => {
                        let fe = self.traverse(fe, current_func, &mut meta.sym)?;
                        let ty = fe.get_type().clone();
                        (Some(Box::new(fe)), ty)
                    }
                };
                self.stack.pop();
                meta.ty = block_ty;
                Ok(Expression::ExpressionBlock(
                    meta.clone(),
                    resolved_body,
                    final_exp,
                ))
            }
            Expression::StructExpression(meta, struct_name, params) => {
                let mut meta = meta.clone();
                // Validate the types in the initialization parameters
                // match their respective members in the struct
                let (struct_def, canonical_path) = self.lookup_symbol_by_path(sym, &struct_name)?;
                let struct_def_ty = struct_def.ty.clone();
                let expected_num_params = struct_def_ty
                    .get_members()
                    .ok_or("Invalid structure")?
                    .len();
                if params.len() != expected_num_params {
                    return Err(format!(
                        "expected {} parameters but found {}",
                        expected_num_params,
                        params.len()
                    ));
                }

                let mut resolved_params = vec![];
                for (pn, pv) in params.iter() {
                    let member_ty = struct_def_ty
                        .get_member(pn)
                        .ok_or(format!("member {} not found on {}", pn, canonical_path))?;
                    let member_ty_canon =
                        Self::type_to_canonical_with_path(&canonical_path.parent(), member_ty)?;
                    let param = self.traverse(pv, current_func, sym)?;
                    if param.get_type() != member_ty_canon {
                        return Err(format!(
                            "{}.{} expects {} but got {}",
                            canonical_path,
                            pn,
                            member_ty_canon,
                            param.get_type()
                        ));
                    }
                    resolved_params.push((pn.clone(), param));
                }

                let anonymouse_name = format!("!{}_{}", canonical_path, meta.id);
                meta.ty = Custom(canonical_path.clone());
                sym.add(&anonymouse_name, meta.ty.clone(), false)?;
                Ok(Expression::StructExpression(
                    meta.clone(),
                    canonical_path,
                    resolved_params,
                ))
            }
        }
    }

    fn analyze_yield(
        &mut self,
        y: &Yield<SemanticAnnotations>,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<Yield<SemanticAnnotations>> {
        match current_func {
            None => Err(format!("yield appears outside of function")),
            Some(_) => {
                let mut meta = y.get_annotations().clone();
                let exp = self.traverse(y.get_value(), current_func, sym)?;
                meta.ty = match exp.get_type() {
                    Coroutine(ret_ty) => self.type_to_canonical(sym, ret_ty)?,
                    _ => return Err(format!("Yield expects co<_> but got {}", exp.get_type())),
                };
                Ok(Yield::new(meta, exp))
            }
        }
    }

    fn unary_op(
        &mut self,
        op: UnaryOperator,
        operand: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<(Type, SemanticNode)> {
        use UnaryOperator::*;

        let operand = self.traverse(operand, current_func, sym)?;

        match op {
            Minus => {
                if operand.get_type() == I64 {
                    Ok((I64, operand))
                } else {
                    Err(format!(
                        "{} expected i64 but found {}",
                        op,
                        operand.get_type()
                    ))
                }
            }
            Not => {
                if operand.get_type() == Bool {
                    Ok((Bool, operand))
                } else {
                    Err(format!(
                        "{} expected bool but found {}",
                        op,
                        operand.get_type()
                    ))
                }
            }
        }
    }

    fn binary_op(
        &mut self,
        op: BinaryOperator,
        l: &SemanticNode,
        r: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<(Type, SemanticNode, SemanticNode)> {
        use BinaryOperator::*;

        let l = self.traverse(l, current_func, sym)?;
        let r = self.traverse(r, current_func, sym)?;

        match op {
            Add | Sub | Mul | Div => {
                if l.get_type() == I64 && r.get_type() == I64 {
                    Ok((I64, l, r))
                } else {
                    Err(format!(
                        "{} expected i64 but found {} and {}",
                        op,
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
            BAnd | BOr => {
                if l.get_type() == Bool && r.get_type() == Bool {
                    Ok((Bool, l, r))
                } else {
                    Err(format!(
                        "{} expected bool but found {} and {}",
                        op,
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
            Eq | NEq | Ls | LsEq | Gr | GrEq => {
                if l.get_type() == r.get_type() {
                    Ok((Bool, l, r))
                } else {
                    Err(format!(
                        "{} expected {} but found {} and {}",
                        op,
                        l.get_type(),
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
        }
    }

    fn get_current_path(&self, sym: &'a SymbolTable) -> Result<Path> {
        self.stack
            .to_path(sym)
            .ok_or("A valid path is expected".into())
    }

    /// Convert a path to its canonical form by merging with the ancestors in the AST.
    fn to_canonical(&self, sym: &'a SymbolTable, path: &Path) -> Result<Path> {
        let current_path = self.stack.to_path(sym).ok_or("A valid path is expected")?;
        path.to_canonical(&current_path)
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical(&self, sym: &'a SymbolTable, ty: &Type) -> Result<Type> {
        match ty {
            Custom(path) => Ok(Custom(path.to_canonical(&self.get_current_path(sym)?)?)),
            Coroutine(ty) => Ok(Coroutine(Box::new(self.type_to_canonical(sym, &ty)?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical_with_path(parent_path: &Path, ty: &Type) -> Result<Type> {
        match ty {
            Custom(path) => Ok(Custom(path.to_canonical(parent_path)?)),
            Coroutine(ty) => Ok(Coroutine(Box::new(Self::type_to_canonical_with_path(
                parent_path,
                &ty,
            )?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Convert any parameter that is a custom type, to its canonical form.
    fn params_to_canonical(
        &self,
        sym: &'a SymbolTable,
        params: &Vec<(String, Type)>,
    ) -> Result<Vec<(String, Type)>> {
        let mut canonical_params = vec![];
        for (name, ty) in params.iter() {
            canonical_params.push((name.clone(), self.type_to_canonical(sym, ty)?));
        }
        Ok(canonical_params)
    }

    fn lookup_symbol_by_path(
        &'a self,
        sym: &'a SymbolTable,
        path: &Path,
    ) -> Result<(&'a Symbol, Path)> {
        let canon_path = self.to_canonical(sym, path)?;

        if path.len() > 1 {
            // If the path contains more than just the item's name then
            // traverse the parent path to find the specified item
            let item = canon_path
                .item()
                .expect("Expected a canonical path with at least one step in it");

            // Look in the project being compiled
            let project_symbol = self
                .root
                .go_to_module(&canon_path.parent())
                .map(|module| module.get_annotations().sym.get(&item))
                .flatten();

            // look in any imported symbols
            let imported_symbol = self.get_imported_symbol(&canon_path);

            // Make sure that there is no ambiguity about what is being referenced
            match (project_symbol, imported_symbol) {
                (Some(ps), None) => Ok((ps, canon_path)),
                (None, Some(is)) => Ok((is, canon_path)),
                (Some(_), Some(_)) => Err(format!("Found multiple definitions of {}", path)),
                (None, None) => Err(format!("Could not find item with the given path: {}", path)),
            }
        } else if path.len() == 1 {
            // If the path has just the item name, then check the local scope and
            // the parent scopes for the given symbol
            let item = &path[0];
            sym.get(item)
                .or_else(|| self.stack.get(item))
                .map(|i| (i, canon_path))
                .ok_or(format!("{} is not defined", item))
        } else {
            Err("empty path passed to lookup_path".into())
        }
    }

    fn lookup_func_or_cor(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(sym, &vec![id].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            }
            | Symbol {
                ty: Type::FunctionDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine or function", id)),
        }
    }

    fn lookup_coroutine(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup_symbol_by_path(sym, &vec![id].into())?.0 {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    fn lookup_var(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&'a Symbol> {
        let (symbol, _) = &self.lookup_symbol_by_path(sym, &vec![id].into())?;
        match symbol.ty {
            FunctionDef(..) | CoroutineDef(..) | StructDef { .. } | Unknown => {
                return Err(format!("{} is not a variable", id))
            }
            Custom(..) | Coroutine(_) | I32 | I64 | Bool | StringLiteral | Unit => Ok(symbol),
        }
    }

    fn extract_routine_type_info<'b>(
        symbol: &'b Symbol,
        call: &expression::RoutineCall,
        routine_path: &Path,
    ) -> Result<(&'b Vec<Type>, Type)> {
        let routine_path_parent = routine_path.parent();
        let (expected_param_tys, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, rty),
                ..
            } if *call == crate::syntax::expression::RoutineCall::Function => (
                pty,
                Self::type_to_canonical_with_path(&routine_path_parent, rty)?,
            ),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == crate::syntax::expression::RoutineCall::CoroutineInit => (
                pty,
                Type::Coroutine(Box::new(Self::type_to_canonical_with_path(
                    &routine_path_parent,
                    rty,
                )?)),
            ),
            _ => {
                let expected = match call {
                    expression::RoutineCall::Function => "function",
                    expression::RoutineCall::CoroutineInit => "coroutine",
                };
                return Err(format!(
                    "Expected {0} but {1} is a {2}",
                    expected, routine_path, symbol.ty
                ));
            }
        };

        Ok((expected_param_tys, ret_ty))
    }

    fn check_for_invalid_routine_parameters<'b>(
        routine_path: &Path,
        given: &'b Vec<SemanticNode>,
        expected_types: &'b Vec<Type>,
    ) -> Result<()> {
        let mut mismatches = vec![];
        let mut idx = 0;
        for (user, expected) in given.iter().zip(expected_types.iter()) {
            idx += 1;
            let user_ty = user.get_type();
            if user_ty != expected {
                mismatches.push((idx, user_ty, expected));
            }
        }
        if mismatches.len() > 0 || given.len() != expected_types.len() {
            let errors: Vec<String> = mismatches
                .iter()
                .map(|(idx, got, expected)| {
                    format!("parameter {} expected {} but got {}", idx, expected, got)
                })
                .collect();
            Err(format!(
                "One or more parameters have mismatching types for function {}: {}",
                routine_path,
                errors.join(", ")
            ))
        } else {
            Ok(())
        }
    }

    fn trace(
        &self,
        node: &SemanticNode,
        current_func: &Option<String>,
        current_scope: &SymbolTable,
    ) {
        let md = node.get_annotations();
        let line = md.ln as usize;
        let print_trace = match self.tracing {
            TracingConfig::All => true,
            TracingConfig::After(start) if start <= line => true,
            TracingConfig::Before(end) if line <= end => true,
            TracingConfig::Between(start, end) if start <= line && line <= end => true,
            TracingConfig::Only(only) if line == only => true,
            _ => false,
        };
        let print_path = match self.path_tracing {
            TracingConfig::All => true,
            TracingConfig::After(start) if start <= line => true,
            TracingConfig::Before(end) if line <= end => true,
            TracingConfig::Between(start, end) if start <= line && line <= end => true,
            TracingConfig::Only(only) if line == only => true,
            _ => false,
        };

        if print_trace {
            let func = match current_func {
                Some(f) => format!("{}: ", f),
                None => "".into(),
            };
            println!("L{}: {}{}\n{}", line, func, node, self.stack);
        }

        if print_path {
            let func = match current_func {
                Some(f) => format!("{}: ", f),
                None => "".into(),
            };
            let path = self
                .stack
                .to_path(current_scope)
                .map_or("[]".into(), |p| format!("{}", p));
            println!("L{}: {}{} <- {}", line, func, node, path);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
    use crate::parser::parser;
    use crate::syntax::{module::Item, routinedef};
    use crate::{expression::Expression, syntax::statement::Statement};

    #[test]
    pub fn test_identifiers() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return k;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> string {
                    let k: string := \"hello\";
                    return k;
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := false;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i64 but got bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_path_to_function() {
        for (text, expected) in vec![
            (
                "mod my_mod{ 
                    fn test() -> i64{ 
                        return 0;
                    } 
                    fn main() {
                        let k: i64 := test();
                        let i: i64 := self::test(); 
                        let j: i64 := root::my_mod::test();
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                "mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                    fn main() {
                        let i: i64 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test]
    pub fn test_path_to_function_in_different_module() {
        for (text,) in vec![
            ("mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                }
                mod main_mod{
                    fn main() {
                        let j: i64 := root::my_mod::test();
                        return;
                    }
                }",),
            ("mod my_mod{ 
                    mod inner {
                        fn test() -> i64{ return 0;} 
                    }
                }
                mod main_mod{
                    fn main() {
                        let j: i64 := root::my_mod::inner::test();
                        return;
                    }
                }",),
            ("
                mod main_mod{
                    fn main() {
                        let j: i64 := root::main_mod::inner::test();
                        let k: i64 := inner::test();
                        let l: i64 := self::inner::test();
                        return;
                    }

                    mod inner {
                        fn test() -> i64{ return 0;} 
                    }
                }",),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            assert!(result.is_ok());
        }
    }

    #[test]
    pub fn test_path_to_struct() {
        for (text, expected) in vec![
            (
                "mod my_mod{ 
                    struct test{i: i64}

                    fn main() {
                        let k: test := test{i: 5};
                        let i: self::test := self::test{i: 5}; 
                        let j: root::my_mod::test := root::my_mod::test{i: 5};
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                "mod my_mod{ 
                    fn test() -> i64{ return 0;} 
                    fn main() {
                        let i: i64 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_expression_renamed_with_canonical_path() {
        for text in vec![
            "
                struct test{i: i64}

                fn main() {
                    let k: test := test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i64}

                fn main() {
                    let k: test := root::test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i64}

                fn main() {
                    let k: test := self::test{i: 5};
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { body, .. }) = &result.get_functions()[0] {
                if let Statement::Bind(box b) = &body[0] {
                    if let Expression::StructExpression(_, struct_name, ..) = b.get_rhs() {
                        let expected: Path = vec!["root", "test"].into();
                        assert_eq!(struct_name, &expected)
                    } else {
                        panic!("Not a struct expression")
                    }
                } else {
                    panic!("Not a bind")
                }
            } else {
                panic!("Not a function")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_function_params_renamed_with_canonical_path() {
        for text in vec![
            "
                struct test{i: i64}

                fn main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { params, .. }) = &result.get_functions()[0]
            {
                if let (_, Custom(ty_path)) = &params[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a function")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_coroutine_params_renamed_with_canonical_path() {
        for text in vec![
            "
                struct test{i: i64}

                co main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { params, .. }) =
                &result.get_coroutines()[0]
            {
                if let (_, Custom(ty_path)) = &params[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a coroutine")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_def_fields_are_converted_to_canonical_paths() {
        for text in vec![
            "
                struct test{i: i64}

                struct test2{t: test}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Struct(s) = &result.get_structs()[1] {
                let fields = s.get_fields();
                if let (_, Custom(ty_path)) = &fields[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a structure")
            }
        }
    }

    #[test]
    pub fn test_unary_ops() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return -k;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return -k;
                }",
                Err("Semantic: L3: - expected i64 but found bool"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return !k;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return !k;
                }",
                Err("Semantic: L3: ! expected bool but found i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_add_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 1 + 5;
                    return k + 3;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 1 + false;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i64 but found i64 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := \"hello\" + 5;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i64 but found string and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k + 3;
                }",
                Err("Semantic: L3: + expected i64 but found bool and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(bind_stm.get_annotations().ty, I64);
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mul_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 1 * 5;
                    return k * 3;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 1 * false;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i64 but found i64 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := \"hello\" * 5;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i64 but found string and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k * 3;
                }",
                Err("Semantic: L3: * expected i64 but found bool and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_annotations().ty, I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_and_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true && false;
                    return k && true;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true && 1;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found bool and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" && false;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k && true;
                }",
                Err("Semantic: L3: && expected bool but found i64 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_annotations().ty, Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_or_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true || false;
                    return k || true;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true || 1;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found bool and i64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" || false;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k || true;
                }",
                Err("Semantic: L3: || expected bool but found i64 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_annotations().ty, Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_annotations().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_comparison_op() {
        for op in vec!["<", ">", "<=", ">=", "==", "!="] {
            for (text, expected) in vec![
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Ok(Bool),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} true;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected i64 but found i64 and bool",
                        op
                    )),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := false {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected bool but found bool and i64",
                        op
                    )),
                ),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
                match expected {
                    Ok(expected_ty) => {
                        let module = module.unwrap();
                        let fn_main = module.get_functions()[0].to_routine().unwrap();

                        let bind_stm = &fn_main.get_body()[0];
                        assert_eq!(bind_stm.get_type(), Bool);

                        // validate that the RHS of the bind is the correct type
                        if let Statement::Bind(box b) = bind_stm {
                            assert_eq!(b.get_rhs().get_type(), expected_ty);
                        } else {
                            panic!("Expected a bind statement");
                        }

                        // Validate that the return statement is the correct type
                        let ret_stm = &fn_main.get_body()[1];
                        assert_eq!(ret_stm.get_type(), expected_ty);
                        if let Statement::Return(box r) = ret_stm {
                            assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                    Err(msg) => {
                        assert_eq!(module.unwrap_err(), msg);
                    }
                }
            }
        }
    }

    #[test]
    pub fn test_bind_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> i64 {
                    let k: bool := 5;
                    return k;
                }",
                Err("Semantic: L2: Bind expected bool but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i64 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i64"),
            ),
            (
                // Test recursive definition
                "fn main() -> bool {
                    let k: bool := k;
                    return k;
                }",
                Err("Semantic: L2: k is not defined"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := x;
                    return k;
                }",
                Err("Semantic: L2: x is not defined"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), I64);
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the return statement is typed correctly
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(r.get_value().clone().unwrap().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mutate_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let mut k: i64 := 5;
                    mut k := 3;
                    return k;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> i64 {
                    let mut k: i64 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: k is of type i64 but is assigned bool"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut k := 3;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    mut x := false;
                    return k;
                }",
                Err("Semantic: L3: x is not defined"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the mutate statement is typed correctly
                    let mut_stm = &fn_main.get_body()[1];
                    assert_eq!(mut_stm.get_type(), I64);
                    if let Statement::Mutate(box m) = mut_stm {
                        assert_eq!(m.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_return_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    return 5;
                }",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    return false;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> string {
                    return \"hello\";
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() {
                    return;
                }",
                Ok(Type::Unit),
            ),
            (
                "fn main() -> bool {
                    return 5;
                }",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
            (
                "fn main() {
                    return 5;
                }",
                Err("Semantic: L2: Return expected unit but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_calls() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    return number();
                }
                fn number() -> i64 {return 5;}
                ",
                Ok(I64),
            ),
            (
                // test recursion
                "fn main() -> i64 {
                    return number();
                }
                fn number() -> i64 {return number();}
                ",
                Ok(I64),
            ),
            (
                "fn main() -> bool {
                    return number();
                }
                fn number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
            (
                "fn main() -> bool {
                    return bad_fun();
                }
                fn number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: bad_fun is not defined"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, 2);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Ok(I64),
            ),
            (
                "fn main() -> i64 {
                    return add(false, 2);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function add: parameter 1 expected i64 but got bool"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, true);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function add: parameter 2 expected i64 but got bool"),
            ),
            (
                "fn main() -> i64 {
                    return add(1);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 1"),
            ),
            (
                "fn main() -> i64 {
                    return add(1, 2, 3);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 3"),
            ),
            (
                "fn main() -> i64 {
                    return add(false);
                }
                fn add(a: i64, b: i64) -> i64 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 1"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r.get_value().clone().map(|v| v.get_type().clone()).unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_init() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {return 5;}
                ",
                Ok(Coroutine(Box::new(I64))),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(3);
                    return;
                }
                co number() -> i64 {return 5;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 0 but got 1"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5);
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Ok(Coroutine(Box::new(I64))),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 1 but got 0"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5, 3);
                    return;
                }
                co number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 1 but got 2"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number(5);
                    return;
                }
                fn number(i: i64) -> i64 {return i;}
                ",
                Err("Semantic: L2: Expected coroutine but number is a fn (i64) -> i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Coroutine(Box::new(I64)));

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement, got {:?}", bind_stm);
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_return_statement() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(I64),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret false;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i64 but got bool"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    return;
                }
                co number() -> i64 {
                    yret;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i64 but got unit"),
            ),
            /*
                Need to add a symbol for the unit type
            (
                "fn main() {
                    let c: co := init number();
                    return;
                }
                co number() {
                    yret 5;
                    return;
                }
                ",
                Err("Semantic: L6: Yield return expected unit but got i64"),
            ),*/
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_coroutines()[0].to_routine().unwrap();

                    let yret_stm = &co_number.get_body()[0];
                    assert_eq!(yret_stm.get_type(), I64);

                    // validate that the RHS of the yield return is the correct type
                    if let Statement::YieldReturn(box yr) = yret_stm {
                        match yr.get_value() {
                            None => panic!("Expected a value"),
                            Some(v) => assert_eq!(v.get_type(), expected_ty),
                        }
                    } else {
                        panic!("Expected a bind statement");
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_statement() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i64 := init number();
                    let i: i64 := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(I64),
            ),
            (
                "fn main() {
                    let c: bool := false;
                    let i: i64 := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Yield expects co<_> but got bool"),
            ),
            (
                "fn main() {
                    let c: co i64 := init number();
                    let i: bool := yield c;
                    return;
                }
                co number() -> i64 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Bind expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &co_number.get_body()[1];
                    assert_eq!(bind_stm.get_type(), I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement, got {:?}", bind_stm);
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_definition() {
        for (text, expected) in vec![
            (
                "fn main(i: i64) -> i64 {
                    return i;
                }
                ",
                Ok(I64),
            ),
            (
                "fn main(b: bool) -> i64 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got bool"),
            ),
            (
                "fn main(b: bool) -> i64 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got unit"),
            ),
            (
                "fn main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_definition() {
        for (text, expected) in vec![
            (
                "co main(i: i64) -> i64 {
                    return i;
                }
                ",
                Ok(I64),
            ),
            (
                "co main(b: bool) -> i64 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got bool"),
            ),
            (
                "co main(b: bool) -> i64 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i64 but got unit"),
            ),
            (
                "co main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_main = module.get_coroutines()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &co_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r
                            .get_value()
                            .clone()
                            .map(|v| v.get_type().clone())
                            .unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_if_expressions() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let x: i64 := if (true) {1} else {2};
                    return;
                }
                ",
                Ok(I64),
            ),
            (
                "fn main() {
                    let x: i64 := if (4) {1} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: Expected boolean expression in if conditional, got: i64"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {true} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected bool got i64"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {5} else {\"hello\"};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected i64 got string"),
            ),
            (
                "fn main() {
                    let x: i64 := if (false) {\"true\"} else {\"false\"};
                    return;
                }
                ",
                Err("Semantic: L2: Bind expected i64 but got string"),
            ),
            (
                "fn main() {
                    if (false) {1};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected i64 got unit"),
            ),
            (
                "fn main() {
                    if (false) {};
                    return;
                }
                ",
                Ok(Unit),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // Check the return value
                    if expected_ty != Unit {
                        if let Statement::Bind(box b) = bind_stm {
                            let rhs_ty = b.get_rhs().get_type();
                            assert_eq!(rhs_ty, expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_struct_expression() {
        for (line, text, expected) in vec![
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> root::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> self::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return self::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return root::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := {self::MyStruct{x: 1}};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i64}}
                fn test() -> my_mod::MyStruct 
                {
                    let x: root::my_mod::MyStruct := self::my_mod::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i64}}
                mod fn_mod {
                    fn test() -> self::super::my_mod::MyStruct 
                    {
                        let x: root::my_mod::MyStruct := super::my_mod::MyStruct{x: 1};
                        return x;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{ms: MyStruct}
                fn test() -> MyStruct2
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: root::MyStruct2 := self::MyStruct2{ ms: x};
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test2(ms: MyStruct) -> i64 {return ms.x;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i64 := test2(x);
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                fn test2(ms: MyStruct) -> MyStruct {return ms;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: MyStruct := test2(x);
                    return y.x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                co test2(ms: MyStruct) -> MyStruct { 
                    yret ms; 
                    return ms;
                }
                fn test() -> root::MyStruct
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: co self::MyStruct := init test2(x);
                    let z: MyStruct := yield (y);
                    return z;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{ms: MyStruct}
                fn test2(ms2: MyStruct2) -> i64 {return ms2.ms.x;}
                fn test() -> i64
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i64 := test2(x);
                    return y;
                }",
                Err("Semantic: L7: One or more parameters have mismatching types for function test2: parameter 1 expected root::MyStruct2 but got root::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64}
                struct MyStruct2{x:i64}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct2 := self::MyStruct{x: 1};
                    return x;
                }",
                Err("Semantic: L5: Bind expected root::MyStruct2 but got root::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{x:false};}",
                Err("Semantic: L1: root::MyStruct.x expects i64 but got bool"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> MyStruct {return MyStruct{};}",
                Err("Semantic: L1: expected 1 parameters but found 0"),
            ),
            (
                line!(),
                "struct MyStruct{x:i64} fn test() -> i64 {return MyStruct{x:5};}",
                Err("Semantic: L1: Return expected i64 but got root::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:co i64} fn test(c: co i64) -> MyStruct {return MyStruct{x: c};}",
                Ok(()),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(_) => {assert!(result.is_ok(), "\nL{}: {} => {:?}", line, text, result)},
                Err(msg) => assert_eq!(result.err().unwrap(), msg),
            }
        }
    }

    #[test]
    pub fn test_member_access() {
        for (text, expected) in vec![
                ("struct MyStruct{x:i64}
                fn test(ms:MyStruct) -> i64 {
                    return ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i64} fn test(ms:MyStruct) -> i64 {return ms.y;}",
                Err("Semantic: L1: root::MyStruct does not have member y")),
                ("struct MyStruct{x:i64} fn test(ms:MyStruct) -> bool{return ms.x;}",
                Err("Semantic: L1: Return expected bool but got i64")),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i64 {return ms.ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> MyStruct {return ms.ms;}",
                Ok(())),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i64 {return ms.ms.y;}",
                Err("Semantic: L1: root::MyStruct does not have member y")),
                ("struct MyStruct{x:i64} struct MS2{ms:MyStruct} fn test(ms:MS2) -> bool {return ms.ms.x;}",
                Err("Semantic: L1: Return expected bool but got i64")),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off);
                match expected {
                    Ok(_) => assert!(result.is_ok(), "{} -> {:?}", text, result),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
    }

    #[test]
    pub fn test_imported_functions() {
        for (text, import_func, expected) in vec![
            (
                " 
                fn main() {
                    let k: i64 := root::std::test();
                    return;
                }
                ",
                (vec![],(Type::I64)),
                Ok(()),
            ),
            (
                " 
                fn main() {
                    return root::std::test();
                }
                ",
                (vec![],(Type::Unit)),
                Ok(()),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test(5);
                    return;
                }
                ",
                (vec![Type::I64], (Type::I64)),
                Ok(()),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test(5, true);
                    return;
                }
                ",
                (vec![Type::I64, Type::Bool], (Type::I64)),
                Ok(()),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test2();
                    return;
                }
                ",
                (vec![], (Type::I64)),
                Err("Semantic: L3: Could not find item with the given path: root::std::test2"),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test(5);
                    return;
                }
                ",
                (vec![], (Type::I64)),
                Err("Semantic: L3: Incorrect number of parameters passed to routine: root::std::test. Expected 0 but got 1"),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test(5, 2);
                    return;
                }
                ",
                (vec![Type::I64, Type::Bool], (Type::I64)),
                Err("Semantic: L3: One or more parameters have mismatching types for function root::std::test: parameter 2 expected bool but got i64"),
            ),
            (
                " 
                fn main() {
                    let k: i64 := root::std::test(5);
                    return;
                }
                mod std {
                    fn test(x: i64) -> i64 {
                        return x;
                    }
                }
                ",
                (vec![Type::I64], (Type::I64)),
                Err("Semantic: L3: Found multiple definitions of root::std::test"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let mut sa = SemanticAst::new();
            let mut sm_ast = sa.from_module(&ast).unwrap();
            SymbolTable::add_item_defs_to_table(&mut sm_ast).unwrap();

            let mut semantic = TypeResolver::new(&sm_ast);

            semantic.import_function(
                vec!["root", "std", "test"].into(),
                import_func.0, import_func.1,
            );

            let result = semantic
                .resolve_types();
            match expected {
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }
}
