use crate::compiler::{
    ast::*,
    parser::parser::ParserInfo,
    semantics::semanticnode::{SemanticAst, SemanticNode},
    semantics::symbol_table::*,
};
use crate::diagnostics::config::{Tracing, TracingConfig};
use crate::manifest::Manifest;
use braid_lang::result::Result;
use std::collections::HashMap;

use super::{
    canonize::canonize_paths, semanticnode::SemanticAnnotations, stack::SymbolTableScopeStack,
};

pub fn resolve_types(
    ast: &Module<ParserInfo>,
    main_fn: &str,
    trace: TracingConfig,
    trace_semantic_node: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticAnnotations>> {
    resolve_types_with_imports(
        ast,
        main_fn,
        &vec![],
        trace_semantic_node,
        trace,
        trace_path,
    )
}

pub fn resolve_types_with_imports(
    ast: &Module<ParserInfo>,
    main_fn: &str,
    imports: &[Manifest],
    trace_semantic_node: TracingConfig,
    trace_type_resolver: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticAnnotations>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(ast, trace_semantic_node);
    SymbolTable::add_item_defs_to_table(&mut sm_ast)?;
    canonize_paths(&mut sm_ast, trace_type_resolver)?; //TODO: Add a trace for this step

    let mut semantic = TypeResolver::new(&sm_ast, main_fn);
    semantic.add_imports(imports);

    semantic.set_tracing(trace_type_resolver);
    semantic.path_tracing = trace_path;
    semantic.resolve_types()
}

pub struct TypeResolver {
    symbols: SymbolTableScopeStack, // I think I can move this into a Cell<> and then make `resolve_types` into &self instead of &mut self
    tracing: TracingConfig,
    path_tracing: TracingConfig,
    imported_symbols: HashMap<String, Symbol>,
    main_fn: Path,
}

impl Tracing for TypeResolver {
    fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }
}

impl TypeResolver {
    pub fn new(root: &Module<SemanticAnnotations>, main_fn: &str) -> TypeResolver {
        TypeResolver {
            symbols: SymbolTableScopeStack::new(root),
            tracing: TracingConfig::Off,
            path_tracing: TracingConfig::Off,
            imported_symbols: HashMap::new(),
            main_fn: vec![CANONICAL_ROOT, MAIN_MODULE, main_fn.into()].into(), // TODO: should get rid of this
        }
    }

    pub fn add_imports(&mut self, manifests: &[Manifest]) {
        // Load all struct imports first because imported functions may depend upon
        // imported structures (If any semantic analysis is done on functions)
        for manifest in manifests.into_iter() {
            for sd in manifest.get_structs().iter() {
                self.import_structdef(sd);
            }
        }

        for manifest in manifests.into_iter() {
            for (path, params, ret_ty) in manifest.get_functions().iter() {
                self.import_function(path.clone(), params.clone(), ret_ty.clone());
            }
        }
    }

    pub fn import_function(
        &mut self,
        canonical_name: Path,
        params: Vec<Type>,
        return_ty: Type,
    ) -> Option<Symbol> {
        self.symbols
            .import_function(canonical_name, params, return_ty)
    }

    pub fn import_structdef(&mut self, sd: &StructDef<SemanticAnnotations>) -> Option<Symbol> {
        self.symbols.import_structdef(sd)
    }

    pub fn resolve_types(&mut self) -> Result<Module<SemanticAnnotations>> {
        // TODO: I think that this is the problem, perhaps I should get rid of the concept
        // of the stack root?  I need root to be able to find items using the stack.
        self.analyze_module(self.symbols.get_root())
            .map_err(|e| format!("Semantic: {}", e))
    }

    fn analyze_module(
        &mut self,
        m: &Module<SemanticAnnotations>,
    ) -> Result<Module<SemanticAnnotations>> {
        let mut nmodule = Module::new(m.get_name(), m.annotation().clone());

        self.symbols.enter_scope(&nmodule.annotation().sym);

        *nmodule.get_modules_mut() = m
            .get_modules()
            .iter()
            .map(|m| self.analyze_module(m))
            .collect::<Result<Vec<Module<SemanticAnnotations>>>>()?;
        *nmodule.get_functions_mut() = m
            .get_functions()
            .iter()
            .map(|f| self.analyze_item(f))
            .collect::<Result<Vec<Item<SemanticAnnotations>>>>()?;
        *nmodule.get_coroutines_mut() = m
            .get_coroutines()
            .iter()
            .map(|c| self.analyze_item(c))
            .collect::<Result<Vec<Item<SemanticAnnotations>>>>()?;
        *nmodule.get_structs_mut() = m
            .get_structs()
            .iter()
            .map(|s| self.analyze_item(s))
            .collect::<Result<Vec<Item<SemanticAnnotations>>>>()?;
        *nmodule.get_externs_mut() = m
            .get_externs()
            .iter()
            .map(|e| self.analyze_item(e))
            .collect::<Result<Vec<Item<SemanticAnnotations>>>>()?;

        let mut meta = nmodule.annotation_mut();
        meta.ty = Type::Unit;
        meta.sym = self.symbols.leave_scope();

        Ok(nmodule)
    }

    fn analyze_item(&mut self, i: &Item<SemanticAnnotations>) -> Result<Item<SemanticAnnotations>> {
        match i {
            Item::Struct(s) => self.analyze_structdef(s).map(|s2| Item::Struct(s2)),
            Item::Routine(r) => self.analyze_routine(r).map(|r2| Item::Routine(r2)),
            Item::Extern(ex) => self.analyze_extern(ex).map(|e2| Item::Extern(e2)),
        }
    }

    fn analyze_routine(
        &mut self,
        routine: &RoutineDef<SemanticAnnotations>,
    ) -> Result<RoutineDef<SemanticAnnotations>> {
        let RoutineDef {
            annotations,
            name,
            def,
            params,
            body,
            ..
        } = routine;

        // If routine is root::my_main it must be a function type and have type () -> i64
        if annotations.get_canonical_path() == &self.main_fn {
            Self::validate_main_fn(routine).map_err(|e| format!("L{}: {}", annotations.ln, e))?;
        }

        let mut meta = annotations.clone();

        // canonize routine parameter types
        let canonical_params = self.params_to_canonical(&params)?;
        //meta.ty = self.symbols.canonize_local_type_ref(ret_ty)?;

        // Add parameters to symbol table
        for p in canonical_params.iter() {
            meta.sym.add(&p.name, p.ty.clone(), false, false)?;
        }

        self.symbols.enter_scope(&meta.sym);

        let mut resolved_body = vec![];
        for stmt in body.iter() {
            let exp = self.analyze_statement(stmt, &name)?;
            resolved_body.push(exp);
        }

        meta.sym = self.symbols.leave_scope();

        let canonical_ret_ty = meta.ty.clone();
        Ok(RoutineDef {
            annotations: meta,
            def: def.clone(),
            name: name.clone(),
            params: canonical_params,
            ty: canonical_ret_ty,
            body: resolved_body,
        })
    }

    fn analyze_structdef(
        &mut self,
        struct_def: &StructDef<SemanticAnnotations>,
    ) -> Result<StructDef<SemanticAnnotations>> {
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

        // Update all fields so that their types use the full canonical path of the type
        let canonical_fields = self.params_to_canonical(fields)?;

        // Update the annotations with canonical path information and set the type to Type::Unit
        let mut meta = struct_def.annotation().clone();
        meta.ty = Type::Unit;

        Ok(StructDef::new(
            struct_def.get_name().clone(),
            meta.clone(),
            canonical_fields,
        ))
    }

    fn analyze_extern(
        &mut self,
        ex: &Extern<SemanticAnnotations>,
    ) -> Result<Extern<SemanticAnnotations>> {
        // Check the type of each member
        let params = ex.get_params();
        for Parameter { ty: field_type, .. } in params.iter() {
            if let Type::Custom(_) = field_type {
                panic!("Custom types are not supported for extern function declarations")
            }
        }

        // Update all fields so that their types use the full canonical path of the type
        let canonical_params = self.params_to_canonical(params)?;

        // Update the annotations with canonical path information and set the type to Type::Unit
        let name = ex.name().expect("Externs must have a name");
        let meta = ex.annotation().clone();

        Ok(Extern::new(
            name,
            meta.clone(),
            canonical_params,
            ex.has_varargs,
            meta.ty.clone(),
        ))
    }

    fn analyze_statement(
        &mut self,
        stmt: &Statement<SemanticAnnotations>,
        current_func: &str,
    ) -> Result<Statement<SemanticAnnotations>> {
        use Statement::*;
        let inner = match stmt {
            Bind(box b) => Bind(Box::new(self.analyze_bind(b, current_func)?)),
            Mutate(box b) => Mutate(Box::new(self.analyze_mutate(b, current_func)?)),
            Return(box x) => Return(Box::new(self.analyze_return(x, current_func)?)),
            YieldReturn(box x) => YieldReturn(Box::new(self.analyze_yieldreturn(x, current_func)?)),
            Expression(box e) => Expression(Box::new(self.traverse(e, current_func)?)),
        };

        Ok(inner)
    }

    fn analyze_bind(
        &mut self,
        bind: &Bind<SemanticAnnotations>,
        current_func: &str,
    ) -> Result<Bind<SemanticAnnotations>> {
        let meta = bind.annotation();
        let rhs = bind.get_rhs();
        let result = {
            let meta = meta.clone();
            //meta.ty = self.symbols.canonize_local_type_ref(bind.get_type())?;
            let rhs = self.traverse(rhs, current_func)?;
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
        result.map_err(|e| format!("L{}: {}", bind.annotation().ln, e))
    }

    fn analyze_mutate(
        &mut self,
        mutate: &Mutate<SemanticAnnotations>,
        current_func: &str,
    ) -> Result<Mutate<SemanticAnnotations>> {
        let mut meta = mutate.annotation().clone();
        let rhs = self.traverse(mutate.get_rhs(), current_func)?;
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
        result.map_err(|e| format!("L{}: {}", mutate.annotation().ln, e))
    }

    fn analyze_yieldreturn(
        &mut self,
        yr: &YieldReturn<SemanticAnnotations>,
        current_func: &str,
    ) -> Result<YieldReturn<SemanticAnnotations>> {
        let result = {
            let mut meta = yr.annotation().clone();
            match yr.get_value() {
                None => {
                    let (_, ret_ty) = self.symbols.lookup_coroutine(current_func)?;
                    if *ret_ty == Type::Unit {
                        meta.ty = Type::Unit;
                        Ok(YieldReturn::new(meta, None))
                    } else {
                        Err(format!("Yield return expected {} but got unit", ret_ty))
                    }
                }
                Some(val) => {
                    let expr = self.traverse(val, current_func)?;
                    let (_, ret_ty) = self.symbols.lookup_coroutine(current_func)?;
                    //println!("ret_ty: {:?}", ret_ty);
                    //let ret_ty = self.symbols.canonize_local_type_ref(ret_ty)?;
                    if ret_ty == expr.get_type() {
                        Ok(YieldReturn::new(meta, Some(expr)))
                    } else {
                        Err(format!(
                            "Yield return expected {} but got {}",
                            ret_ty,
                            expr.get_type(),
                        ))
                    }
                }
            }
        };
        result.map_err(|e| format!("L{}: {}", yr.annotation().ln, e))
    }

    fn analyze_return(
        &mut self,
        r: &Return<SemanticAnnotations>,
        current_func: &str,
    ) -> Result<Return<SemanticAnnotations>> {
        let result = {
            let mut meta = r.annotation().clone();
            match r.get_value() {
                None => {
                    let (_, ret_ty) = self.symbols.lookup_func_or_cor(current_func)?;
                    if *ret_ty == Type::Unit {
                        meta.ty = Type::Unit;
                        Ok(Return::new(meta, None))
                    } else {
                        Err(format!("Return expected {} but got unit", ret_ty))
                    }
                }
                Some(val) => {
                    let exp = self.traverse(val, current_func)?;
                    let (_, ret_ty) = self.symbols.lookup_func_or_cor(current_func)?;
                    let ret_ty = self.symbols.canonize_local_type_ref(ret_ty)?;
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
        };
        result.map_err(|e| format!("L{}: {}", r.annotation().ln, e))
    }

    fn traverse(&mut self, ast: &SemanticNode, current_func: &str) -> Result<SemanticNode> {
        self.analyze_expression(ast, current_func).map_err(|e| {
            if !e.starts_with("L") {
                format!("L{}: {}", ast.annotation().ln, e)
            } else {
                e
            }
        })
    }

    fn analyze_expression(
        &mut self,
        ast: &SemanticNode,
        current_func: &str,
    ) -> Result<SemanticNode> {
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
            Expression::ArrayValue(meta, elements, len) => {
                // Resolve the types for each element in the array value
                let nelements: Result<Vec<Expression<SemanticAnnotations>>> = elements
                    .iter()
                    .map(|e| self.traverse(e, current_func))
                    .collect();
                let nelements = nelements?;

                // Check that they are homogenous
                let el_ty;
                if nelements.len() == 0 {
                    return Err("Arrays with 0 length are not allowed".into());
                } else {
                    el_ty = nelements[0].annotation().ty.clone();
                    for e in &nelements {
                        if e.annotation().ty != el_ty {
                            return Err("Inconsistent types in array value".into());
                        }
                    }
                }

                let mut meta = meta.clone();
                /*meta.ty = self
                .symbols
                .canonize_local_type_ref(&Type::Array(Box::new(el_ty), *len))?;*/

                // Use the size of the array and the type to define the array type
                Ok(Expression::ArrayValue(meta, nelements, *len))
            }
            Expression::ArrayAt {
                annotation: meta,
                array,
                index,
            } => {
                //  Check that the array value is an array type
                let n_array = self.traverse(array, current_func)?;
                let el_ty = match n_array.annotation().ty() {
                    Type::Array(box el_ty, _) => Ok(el_ty),
                    ty => Err(format!("Expected array type on LHS of [] but found {}", ty)),
                }?;

                // Check that the index is an i64 type
                let n_index = self.traverse(index, current_func)?;
                if !n_index.annotation().ty().is_integral() {
                    return Err(format!(
                        "Expected integral type for index but found {}",
                        n_index.annotation().ty()
                    ));
                }

                let mut meta = meta.clone();
                meta.ty = el_ty.clone();

                Ok(Expression::ArrayAt {
                    annotation: meta,
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
                    Symbol { ty: p, .. } => meta.ty = self.symbols.canonize_local_type_ref(p)?,
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
                let src = self.traverse(&src, current_func)?;
                match src.get_type() {
                    Type::Custom(struct_name) => {
                        let (struct_def, canonical_path) =
                            self.symbols.lookup_symbol_by_path(&struct_name)?;
                        let member_ty = struct_def
                            .ty
                            .get_member(&member)
                            .ok_or(format!("{} does not have member {}", struct_name, member))?;
                        meta.ty = self
                            .symbols
                            .canonize_nonlocal_type_ref(&canonical_path.parent(), member_ty)?;

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
                let (ty, l, r) = self.binary_op(*op, &l, &r, current_func)?;
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
                let (ty, operand) = self.unary_op(*op, &operand, current_func)?;
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
                let cond = self.traverse(&cond, current_func)?;
                if cond.get_type() == Type::Bool {
                    let if_arm = self.traverse(&if_arm, current_func)?;

                    let else_arm = else_arm
                        .as_ref()
                        .map(|e| self.traverse(&e, current_func))
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
            Expression::While {
                annotation: meta,
                cond,
                body,
                ..
            } => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond, current_func)?;
                if cond.get_type() == Type::Bool {
                    let body = self.traverse(&body, current_func)?;

                    if body.get_type() == Type::Unit {
                        meta.ty = Type::Unit;
                        Ok(Expression::While {
                            annotation: meta.clone(),
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
                let exp = self.traverse(&exp, current_func)?;
                meta.ty = match exp.get_type() {
                    Type::Coroutine(ret_ty) => self.symbols.canonize_local_type_ref(ret_ty)?,
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
                    let ty = self.traverse(param, current_func)?;

                    resolved_params.push(ty);
                }

                // Check that the function being called exists
                //println!("L{}: Call {}", meta.line(), routine_path);
                let (symbol, routine_canon_path) =
                    self.symbols.lookup_symbol_by_path(routine_path)?;

                let (expected_param_tys, has_varargs, ret_ty) =
                    self.extract_routine_type_info(symbol, call, &routine_canon_path)?;
                let expected_param_tys = expected_param_tys
                    .iter()
                    .map(|pty| {
                        self.symbols
                            .canonize_nonlocal_type_ref(&routine_canon_path.parent(), pty)
                    })
                    .collect::<Result<Vec<Type>>>()?;

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
                            meta.ty = self.symbols.canonize_local_type_ref(&ret_ty)?;
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
                    let exp = self.analyze_statement(stmt, current_func)?;
                    resolved_body.push(exp);
                }

                let (final_exp, block_ty) = match final_exp {
                    None => (None, Type::Unit),
                    Some(fe) => {
                        let fe = self.traverse(fe, current_func)?;
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
                let meta = meta.clone();
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
                    let member_ty_canon = self
                        .symbols
                        .canonize_nonlocal_type_ref(&canonical_path.parent(), member_ty)?;
                    let param = self.traverse(pv, current_func)?;
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

                /*println!(
                    "L{} ForEach: {}, TR: {}",
                    meta.line(),
                    meta.ty,
                    canonical_path
                );*/
                //meta.ty = Type::Custom(canonical_path.clone());
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
        current_func: &str,
    ) -> Result<(Type, SemanticNode)> {
        use UnaryOperator::*;

        let operand = self.traverse(operand, current_func)?;

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
        current_func: &str,
    ) -> Result<(Type, SemanticNode, SemanticNode)> {
        use BinaryOperator::*;

        let l = self.traverse(l, current_func)?;
        let r = self.traverse(r, current_func)?;

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

    /// Convert any parameter that is a custom type, to its canonical form.
    fn params_to_canonical(
        &self,
        params: &Vec<Parameter<SemanticAnnotations>>,
    ) -> Result<Vec<Parameter<SemanticAnnotations>>> {
        let mut canonical_params = vec![];
        for p in params.iter() {
            let mut p2 = p.clone();
            p2.ty = self.symbols.canonize_local_type_ref(&p.ty)?;
            p2.annotation_mut().ty = p2.ty.clone();
            canonical_params.push(p2);
        }
        Ok(canonical_params)
    }

    fn extract_routine_type_info<'b>(
        &self,
        symbol: &'b Symbol,
        call: &RoutineCall,
        routine_path: &Path,
    ) -> Result<(&'b Vec<Type>, HasVarArgs, Type)> {
        let routine_path_parent = routine_path.parent();
        let (expected_param_tys, has_varargs, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, rty),
                ..
            } if *call == RoutineCall::Function => (
                pty,
                false,
                self.symbols
                    .canonize_nonlocal_type_ref(&routine_path_parent, rty)?,
            ),
            Symbol {
                ty: Type::ExternDecl(pty, has_varargs, rty),
                ..
            } if *call == RoutineCall::Extern => (
                pty,
                *has_varargs,
                self.symbols
                    .canonize_nonlocal_type_ref(&routine_path_parent, rty)?,
            ),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == RoutineCall::CoroutineInit => (
                pty,
                false,
                Type::Coroutine(Box::new(
                    self.symbols
                        .canonize_nonlocal_type_ref(&routine_path_parent, rty)?,
                )),
            ),
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

    fn validate_main_fn(routine: &RoutineDef<SemanticAnnotations>) -> Result<()> {
        let RoutineDef {
            def, params, ty: p, ..
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
