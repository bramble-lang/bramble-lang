use crate::compiler::{
    ast::*,
    parser::parser::ParserContext,
    semantics::semanticnode::{SemanticAst, SemanticNode},
    semantics::symbol_table::*,
};
use crate::diagnostics::config::{Tracing, TracingConfig};
use crate::manifest::Manifest;
use braid_lang::result::Result;
use std::collections::HashMap;

use super::{
    canonize::canonize_paths, semanticnode::SemanticContext, stack::SymbolTableScopeStack,
};

pub fn resolve_types(
    ast: &Module<ParserContext>,
    main_fn: &str,
    trace: TracingConfig,
    trace_semantic_node: TracingConfig,
    trace_canonization: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticContext>> {
    resolve_types_with_imports(
        ast,
        main_fn,
        &vec![],
        trace_semantic_node,
        trace_canonization,
        trace,
        trace_path,
    )
}

pub fn resolve_types_with_imports(
    ast: &Module<ParserContext>,
    main_fn: &str,
    imports: &[Manifest],
    trace_semantic_node: TracingConfig,
    trace_canonization: TracingConfig,
    trace_type_resolver: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticContext>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(ast, trace_semantic_node);
    SymbolTable::add_item_defs_to_table(&mut sm_ast)?;
    canonize_paths(&mut sm_ast, imports, trace_canonization)?; //TODO: Add a trace for this step

    let mut semantic = TypeResolver::new(&sm_ast, imports, main_fn);

    semantic.set_tracing(trace_type_resolver);
    semantic.resolve_types()
}

pub struct TypeResolver {
    symbols: SymbolTableScopeStack,
    tracing: TracingConfig,
    imported_symbols: HashMap<String, Symbol>,
    main_fn: Path,
}

impl Tracing for TypeResolver {
    fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }
}

impl TypeResolver {
    pub fn new(
        root: &Module<SemanticContext>,
        imports: &[Manifest],
        main_fn: &str,
    ) -> TypeResolver {
        TypeResolver {
            symbols: SymbolTableScopeStack::new(root, imports),
            tracing: TracingConfig::Off,
            imported_symbols: HashMap::new(),
            main_fn: vec![CANONICAL_ROOT, MAIN_MODULE, main_fn.into()].into(), // TODO: should get rid of this
        }
    }

    pub fn resolve_types(&mut self) -> Result<Module<SemanticContext>> {
        // TODO: I think that this is the problem, perhaps I should get rid of the concept
        // of the stack root?  I need root to be able to find items using the stack.
        self.analyze_module(self.symbols.get_root())
            .map_err(|e| format!("Semantic: {}", e))
    }

    fn analyze_module(&mut self, m: &Module<SemanticContext>) -> Result<Module<SemanticContext>> {
        let mut nmodule = Module::new(m.get_name(), m.get_context().clone());

        self.symbols.enter_scope(&nmodule.get_context().sym);

        *nmodule.get_modules_mut() = m
            .get_modules()
            .iter()
            .map(|m| self.analyze_module(m))
            .collect::<Result<Vec<Module<SemanticContext>>>>()?;
        *nmodule.get_functions_mut() = m
            .get_functions()
            .iter()
            .map(|f| self.analyze_item(f))
            .collect::<Result<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_coroutines_mut() = m
            .get_coroutines()
            .iter()
            .map(|c| self.analyze_item(c))
            .collect::<Result<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_structs_mut() = m
            .get_structs()
            .iter()
            .map(|s| self.analyze_item(s))
            .collect::<Result<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_externs_mut() = m
            .get_externs()
            .iter()
            .map(|e| self.analyze_item(e))
            .collect::<Result<Vec<Item<SemanticContext>>>>()?;

        let mut meta = nmodule.get_context_mut();
        meta.ty = Type::Unit;
        meta.sym = self.symbols.leave_scope();

        Ok(nmodule)
    }

    fn analyze_item(&mut self, i: &Item<SemanticContext>) -> Result<Item<SemanticContext>> {
        match i {
            Item::Struct(s) => self.analyze_structdef(s).map(|s2| Item::Struct(s2)),
            Item::Routine(r) => self.analyze_routine(r).map(|r2| Item::Routine(r2)),
            Item::Extern(ex) => self.analyze_extern(ex).map(|e2| Item::Extern(e2)),
        }
    }

    fn analyze_routine(
        &mut self,
        routine: &RoutineDef<SemanticContext>,
    ) -> Result<RoutineDef<SemanticContext>> {
        let RoutineDef {
            context,
            name,
            def,
            params,
            body,
            ret_ty,
            ..
        } = routine;

        // If routine is root::my_main it must be a function type and have type () -> i64
        if context.get_canonical_path() == &self.main_fn {
            Self::validate_main_fn(routine).map_err(|e| format!("L{}: {}", context.line(), e))?;
        }

        let mut meta = context.clone();

        // canonize routine parameter types
        meta.ty = ret_ty.clone();

        // Add parameters to symbol table
        for p in params.iter() {
            meta.sym.add(&p.name, p.ty.clone(), false, false)?;
        }

        self.symbols.enter_scope(&meta.sym);

        let mut resolved_body = vec![];
        for stmt in body.iter() {
            let exp = self.analyze_statement(stmt)?;
            resolved_body.push(exp);
        }

        meta.sym = self.symbols.leave_scope();

        let canonical_ret_ty = meta.ty.clone();
        Ok(RoutineDef {
            context: meta,
            def: def.clone(),
            name: name.clone(),
            params: params.clone(),
            ret_ty: canonical_ret_ty,
            body: resolved_body,
        })
    }

    fn analyze_structdef(
        &mut self,
        struct_def: &StructDef<SemanticContext>,
    ) -> Result<StructDef<SemanticContext>> {
        // Check the type of each member
        let fields = struct_def.get_fields();
        for Parameter {
            name: field_name,
            ty: field_type,
            ..
        } in fields.iter()
        {
            match field_type {
                Type::Custom(ty_name) => {
                    self.symbols.lookup_symbol_by_path(ty_name).map_err(|e| {
                        format!(
                            "member {}.{} invalid: {}",
                            struct_def.get_name(),
                            field_name,
                            e
                        )
                    })?;
                }
                _ => (),
            }
        }

        // Update the context with canonical path information and set the type to Type::Unit
        let mut meta = struct_def.get_context().clone();
        meta.ty = Type::Unit;

        Ok(StructDef::new(
            struct_def.get_name().clone(),
            meta.clone(),
            fields.clone(),
        ))
    }

    fn analyze_extern(&mut self, ex: &Extern<SemanticContext>) -> Result<Extern<SemanticContext>> {
        // Check the type of each member
        let params = ex.get_params();
        for Parameter { ty: field_type, .. } in params.iter() {
            if let Type::Custom(_) = field_type {
                panic!("Custom types are not supported for extern function declarations")
            }
        }

        // Update the context with canonical path information and set the type to Type::Unit
        let name = ex.name().expect("Externs must have a name");
        let mut meta = ex.get_context().clone();
        meta.ty = ex.get_return_type().clone();

        Ok(Extern::new(
            name,
            meta.clone(),
            params.clone(),
            ex.has_varargs,
            meta.ty.clone(),
        ))
    }

    fn analyze_statement(
        &mut self,
        stmt: &Statement<SemanticContext>,
    ) -> Result<Statement<SemanticContext>> {
        use Statement::*;
        let inner = match stmt {
            Bind(box b) => Bind(Box::new(self.analyze_bind(b)?)),
            Mutate(box b) => Mutate(Box::new(self.analyze_mutate(b)?)),
            Return(box x) => Return(Box::new(self.analyze_return(x)?)),
            YieldReturn(box x) => YieldReturn(Box::new(self.analyze_yieldreturn(x)?)),
            Expression(box e) => Expression(Box::new(self.traverse(e)?)),
        };

        Ok(inner)
    }

    fn analyze_bind(&mut self, bind: &Bind<SemanticContext>) -> Result<Bind<SemanticContext>> {
        let meta = bind.get_context();
        let rhs = bind.get_rhs();
        let result = {
            let mut meta = meta.clone();
            meta.ty = bind.get_type().clone();
            let rhs = self.traverse(rhs)?;
            if meta.ty == rhs.get_type() {
                match self
                    .symbols
                    .add(bind.get_id(), meta.ty.clone(), bind.is_mutable(), false)
                {
                    Ok(()) => {
                        let ty = meta.ty().clone();
                        Ok(Bind::new(meta, bind.get_id(), ty, bind.is_mutable(), rhs))
                    }
                    Err(e) => Err(e),
                }
            } else {
                Err(format!(
                    "Bind expected {} but got {}",
                    meta.ty,
                    rhs.get_type()
                ))
            }
        };
        result.map_err(|e| format!("L{}: {}", bind.get_context().line(), e))
    }

    fn analyze_mutate(
        &mut self,
        mutate: &Mutate<SemanticContext>,
    ) -> Result<Mutate<SemanticContext>> {
        let mut meta = mutate.get_context().clone();
        let rhs = self.traverse(mutate.get_rhs())?;
        let result = match self.symbols.lookup_var(mutate.get_id()) {
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
        };
        result.map_err(|e| format!("L{}: {}", mutate.get_context().line(), e))
    }

    fn analyze_yieldreturn(
        &mut self,
        yr: &YieldReturn<SemanticContext>,
    ) -> Result<YieldReturn<SemanticContext>> {
        // Get the actual expression and its type as it comes from the
        // source code written by the user.
        let (actual_ret_exp, actual_ret_ty) = match yr.get_value() {
            None => (None, Type::Unit),
            Some(exp) => {
                let exp = self.traverse(exp)?;
                let exp_ty = exp.get_type().clone();
                (Some(exp), exp_ty)
            }
        };

        // Get the expected yield return type of the coroutine that the yield return
        // occurs within.
        let current_func = self
            .symbols
            .get_current_fn()
            .ok_or(format!("Yield return must occur inside of a function"))?;
        let (_, expected_ret_ty) = self.symbols.lookup_coroutine(current_func)?;

        if actual_ret_ty == expected_ret_ty {
            let mut meta = yr.get_context().clone();
            meta.ty = actual_ret_ty;
            Ok(YieldReturn::new(meta, actual_ret_exp))
        } else {
            Err(format!(
                "Yield return expected {} but got {}",
                expected_ret_ty, actual_ret_ty,
            ))
        }
        .map_err(|e| format!("L{}: {}", yr.get_context().line(), e))
    }

    fn analyze_return(&mut self, r: &Return<SemanticContext>) -> Result<Return<SemanticContext>> {
        // Get the actual expression and its type as it comes from the
        // source code written by the user.
        let (actual_ret_exp, actual_ret_ty) = match r.get_value() {
            None => (None, Type::Unit),
            Some(exp) => {
                let exp = self.traverse(exp)?;
                let exp_ty = exp.get_type().clone();
                (Some(exp), exp_ty)
            }
        };

        // Get the expected return type of the function that the return
        // occurs within.
        let current_func = self
            .symbols
            .get_current_fn()
            .ok_or(format!("Return must occur inside of a function"))?;
        let (_, expected_ret_ty) = self.symbols.lookup_func_or_cor(current_func)?;

        // Check that the actual expression matches the expected return type
        // of the function
        if actual_ret_ty == expected_ret_ty {
            let mut meta = r.get_context().clone();
            meta.ty = actual_ret_ty;
            Ok(Return::new(meta, actual_ret_exp))
        } else {
            Err(format!(
                "Return expected {} but got {}",
                expected_ret_ty, actual_ret_ty,
            ))
        }
        .map_err(|e| format!("L{}: {}", r.get_context().line(), e))
    }

    fn traverse(&mut self, ast: &SemanticNode) -> Result<SemanticNode> {
        self.analyze_expression(ast).map_err(|e| {
            if !e.starts_with("L") {
                format!("L{}: {}", ast.get_context().line(), e)
            } else {
                e
            }
        })
    }

    fn analyze_expression(&mut self, ast: &SemanticNode) -> Result<SemanticNode> {
        match &ast {
            &Expression::U8(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::U8;
                Ok(Expression::U8(meta, *v))
            }
            &Expression::U16(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::U16;
                Ok(Expression::U16(meta, *v))
            }
            &Expression::U32(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::U32;
                Ok(Expression::U32(meta, *v))
            }
            &Expression::U64(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::U64;
                Ok(Expression::U64(meta, *v))
            }
            &Expression::I8(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I8;
                Ok(Expression::I8(meta, *v))
            }
            &Expression::I16(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I16;
                Ok(Expression::I16(meta, *v))
            }
            &Expression::I32(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I32;
                Ok(Expression::I32(meta, *v))
            }
            &Expression::I64(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I64;
                Ok(Expression::I64(meta, *v))
            }
            Expression::Boolean(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::Bool;
                Ok(Expression::Boolean(meta.clone(), *v))
            }
            Expression::StringLiteral(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::StringLiteral;
                Ok(Expression::StringLiteral(meta.clone(), v.clone()))
            }
            Expression::ArrayExpression(meta, elements, len) => {
                // Resolve the types for each element in the array value
                let nelements: Result<Vec<Expression<SemanticContext>>> =
                    elements.iter().map(|e| self.traverse(e)).collect();
                let nelements = nelements?;

                // Check that they are homogenous
                let el_ty;
                if nelements.len() == 0 {
                    return Err("Arrays with 0 length are not allowed".into());
                } else {
                    el_ty = nelements[0].get_context().ty.clone();
                    for e in &nelements {
                        if e.get_context().ty != el_ty {
                            return Err("Inconsistent types in array value".into());
                        }
                    }
                }

                // Use the size of the array and the type to define the array type
                let mut meta = meta.clone();
                meta.ty = Type::Array(Box::new(el_ty), *len);
                Ok(Expression::ArrayExpression(meta, nelements, *len))
            }
            Expression::ArrayAt {
                context: meta,
                array,
                index,
            } => {
                //  Check that the array value is an array type
                let n_array = self.traverse(array)?;
                let el_ty = match n_array.get_context().ty() {
                    Type::Array(box el_ty, _) => Ok(el_ty),
                    ty => Err(format!("Expected array type on LHS of [] but found {}", ty)),
                }?;

                // Check that the index is an i64 type
                let n_index = self.traverse(index)?;
                if !n_index.get_context().ty().is_integral() {
                    return Err(format!(
                        "Expected integral type for index but found {}",
                        n_index.get_context().ty()
                    ));
                }

                let mut meta = meta.clone();
                meta.ty = el_ty.clone();

                Ok(Expression::ArrayAt {
                    context: meta,
                    array: box n_array,
                    index: box n_index,
                })
            }
            Expression::CustomType(meta, name) => {
                let mut meta = meta.clone();
                meta.ty = Type::Custom(name.clone());
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
            Expression::Identifier(meta, id) => {
                let mut meta = meta.clone();
                match self.symbols.lookup_var(&id)? {
                    Symbol { ty: p, .. } => meta.ty = p.clone(),
                };
                Ok(Expression::Identifier(meta.clone(), id.clone()))
            }
            Expression::Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            Expression::MemberAccess(meta, src, member) => {
                let mut meta = meta.clone();
                // Get the type of src and look up its struct definition
                // Check the struct definition for the type of `member`
                // if it exists, if it does not exist then return an error
                let src = self.traverse(&src)?;
                match src.get_type() {
                    Type::Custom(struct_name) => {
                        let (struct_def, _) = self.symbols.lookup_symbol_by_path(&struct_name)?;
                        let member_ty = struct_def
                            .ty
                            .get_member(&member)
                            .ok_or(format!("{} does not have member {}", struct_name, member))?;
                        meta.ty = member_ty.clone();

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
                let (ty, l, r) = self.binary_op(*op, &l, &r)?;
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
                let (ty, operand) = self.unary_op(*op, &operand)?;
                meta.ty = ty;
                Ok(Expression::UnaryOp(meta.clone(), *op, Box::new(operand)))
            }
            Expression::If {
                context: meta,
                cond,
                if_arm,
                else_arm,
            } => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond)?;
                if cond.get_type() == Type::Bool {
                    let if_arm = self.traverse(&if_arm)?;

                    let else_arm = else_arm
                        .as_ref()
                        .map(|e| self.traverse(&e))
                        .map_or(Ok(None), |r| r.map(|x| Some(box x)))?;

                    let else_arm_ty = else_arm
                        .as_ref()
                        .map(|e| e.get_type().clone())
                        .unwrap_or(Type::Unit);

                    if if_arm.get_type() == else_arm_ty {
                        meta.ty = if_arm.get_type().clone();
                        Ok(Expression::If {
                            context: meta.clone(),
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
            Expression::While {
                context: meta,
                cond,
                body,
                ..
            } => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond)?;
                if cond.get_type() == Type::Bool {
                    let body = self.traverse(&body)?;

                    if body.get_type() == Type::Unit {
                        meta.ty = Type::Unit;
                        Ok(Expression::While {
                            context: meta.clone(),
                            cond: box cond,
                            body: box body,
                        })
                    } else {
                        Err(format!(
                            "The body of a while expression must resolve to a unit type, but got: {}",
                            body.get_type()
                        ))
                    }
                } else {
                    Err(format!(
                        "The condition of a while expression must resolve to a unit type, but got: {}",
                        cond.get_type()
                    ))
                }
            }
            Expression::Yield(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp)?;
                meta.ty = match exp.get_type() {
                    Type::Coroutine(box ret_ty) => ret_ty.clone(),
                    _ => return Err(format!("Yield expects co<_> but got {}", exp.get_type())),
                };
                Ok(Expression::Yield(meta, Box::new(exp)))
            }
            Expression::RoutineCall(meta, call, routine_path, params) => {
                let mut meta = meta.clone();
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut resolved_params = vec![];
                for param in params.iter() {
                    let ty = self.traverse(param)?;

                    resolved_params.push(ty);
                }

                // Check that the function being called exists
                let (symbol, routine_canon_path) =
                    self.symbols.lookup_symbol_by_path(routine_path)?;

                let (expected_param_tys, has_varargs, ret_ty) =
                    self.extract_routine_type_info(symbol, call, &routine_canon_path)?;

                // Check that parameters are correct and if so, return the node annotated with
                // semantic information
                if !has_varargs && (resolved_params.len() != expected_param_tys.len()) {
                    Err(format!(
                        "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                        routine_path,
                        expected_param_tys.len(),
                        resolved_params.len(),
                    ))
                } else if has_varargs && (resolved_params.len() < expected_param_tys.len()) {
                    Err(format!(
                        "Function {} expects at least {} parameters, but got {}",
                        routine_canon_path,
                        expected_param_tys.len(),
                        resolved_params.len(),
                    ))
                } else {
                    match Self::check_for_invalid_routine_parameters(
                        &routine_path,
                        &resolved_params,
                        &expected_param_tys,
                        has_varargs,
                    ) {
                        Err(msg) => Err(msg),
                        Ok(()) => {
                            meta.ty = ret_ty.clone();
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
                let mut resolved_body = vec![];

                self.symbols.enter_scope(&meta.sym);

                for stmt in body.iter() {
                    let exp = self.analyze_statement(stmt)?;
                    resolved_body.push(exp);
                }

                let (final_exp, block_ty) = match final_exp {
                    None => (None, Type::Unit),
                    Some(fe) => {
                        let fe = self.traverse(fe)?;
                        let ty = fe.get_type().clone();
                        (Some(Box::new(fe)), ty)
                    }
                };

                let mut meta = meta.clone();
                meta.sym = self.symbols.leave_scope();

                meta.ty = block_ty;
                Ok(Expression::ExpressionBlock(
                    meta.clone(),
                    resolved_body,
                    final_exp,
                ))
            }
            Expression::StructExpression(meta, struct_name, params) => {
                // Validate the types in the initialization parameters
                // match their respective members in the struct
                let (struct_def, canonical_path) =
                    self.symbols.lookup_symbol_by_path(&struct_name)?;
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
                    let param = self.traverse(pv)?;
                    if param.get_type() != member_ty {
                        return Err(format!(
                            "{}.{} expects {} but got {}",
                            canonical_path,
                            pn,
                            member_ty,
                            param.get_type()
                        ));
                    }
                    resolved_params.push((pn.clone(), param));
                }

                let mut meta = meta.clone();
                meta.ty = Type::Custom(struct_name.clone());
                Ok(Expression::StructExpression(
                    meta.clone(),
                    canonical_path,
                    resolved_params,
                ))
            }
        }
    }

    fn unary_op(
        &mut self,
        op: UnaryOperator,
        operand: &SemanticNode,
    ) -> Result<(Type, SemanticNode)> {
        use UnaryOperator::*;

        let operand = self.traverse(operand)?;

        match op {
            Negate => {
                if operand.get_type().is_signed_int() {
                    Ok((operand.get_type().clone(), operand))
                } else {
                    Err(format!(
                        "{} expected i32 or i64 but found {}",
                        op,
                        operand.get_type()
                    ))
                }
            }
            Not => {
                if operand.get_type() == Type::Bool {
                    Ok((Type::Bool, operand))
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
    ) -> Result<(Type, SemanticNode, SemanticNode)> {
        use BinaryOperator::*;

        let l = self.traverse(l)?;
        let r = self.traverse(r)?;

        match op {
            Add | Sub | Mul | Div => {
                if l.get_type().is_integral()
                    && r.get_type().is_integral()
                    && l.get_type() == r.get_type()
                {
                    Ok((l.get_type().clone(), l, r))
                } else {
                    let expected = if l.get_type().is_integral() {
                        format!("{}", l.get_type())
                    } else {
                        "i64".into()
                    };
                    Err(format!(
                        "{} expected {} but found {} and {}",
                        op,
                        expected,
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
            BAnd | BOr => {
                if l.get_type() == Type::Bool && r.get_type() == Type::Bool {
                    Ok((Type::Bool, l, r))
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
                    Ok((Type::Bool, l, r))
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

    fn get_current_path(&self) -> Result<Path> {
        self.symbols
            .to_path()
            .ok_or("A valid path is expected".into())
    }

    fn extract_routine_type_info<'b>(
        &self,
        symbol: &'b Symbol,
        call: &RoutineCall,
        routine_path: &Path,
    ) -> Result<(&'b Vec<Type>, HasVarArgs, Type)> {
        let (expected_param_tys, has_varargs, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, box rty),
                ..
            } if *call == RoutineCall::Function => (pty, false, rty.clone()),
            Symbol {
                ty: Type::ExternDecl(pty, has_varargs, box rty),
                ..
            } if *call == RoutineCall::Extern => (pty, *has_varargs, rty.clone()),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == RoutineCall::CoroutineInit => (pty, false, Type::Coroutine(rty.clone())),
            _ => {
                let expected = match call {
                    RoutineCall::Function => "function",
                    RoutineCall::CoroutineInit => "coroutine",
                    RoutineCall::Extern => "extern",
                };
                return Err(format!(
                    "Expected {0} but {1} is a {2}",
                    expected, routine_path, symbol.ty
                ));
            }
        };

        Ok((expected_param_tys, has_varargs, ret_ty))
    }

    fn check_for_invalid_routine_parameters<'b>(
        routine_path: &Path,
        given: &'b Vec<SemanticNode>,
        expected_types: &'b Vec<Type>,
        has_varargs: HasVarArgs,
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

        if !has_varargs && mismatches.len() == 0 && given.len() == expected_types.len() {
            Ok(())
        } else if has_varargs && mismatches.len() == 0 && given.len() >= expected_types.len() {
            Ok(())
        } else {
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
        }
    }

    fn validate_main_fn(routine: &RoutineDef<SemanticContext>) -> Result<()> {
        let RoutineDef {
            def,
            params,
            ret_ty: p,
            ..
        } = routine;

        // If routine is root::my_main it must be a function type and have type () -> i64
        if def != &RoutineDefType::Function {
            return Err(format!("my_main must be a function of type () -> i64",));
        }

        if params.len() > 0 {
            return Err(format!(
                "my_main must take no parameters. It must be of type () -> i64",
            ));
        }

        if p != Type::I64 {
            return Err(format!(
                "my_main must return an i64. It must be of type () -> i64",
            ));
        }

        Ok(())
    }
}
