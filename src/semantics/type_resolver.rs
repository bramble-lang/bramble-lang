use std::collections::HashMap;

use crate::ast::*;
use crate::diagnostics::config::{Tracing, TracingConfig};
use crate::semantics::semanticnode::{SemanticAst, SemanticNode};
use crate::{parser::parser::ParserInfo, semantics::symbol_table::*};
use braid_lang::result::Result;

use super::{semanticnode::SemanticAnnotations, stack::SymbolTableScopeStack};

pub fn resolve_types(
    ast: &Module<ParserInfo>,
    trace: TracingConfig,
    trace_semantic_node: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticAnnotations>> {
    resolve_types_with_imports(ast, &vec![], trace_semantic_node, trace, trace_path)
}

pub fn resolve_types_with_imports(
    ast: &Module<ParserInfo>,
    imported_functions: &Vec<(Path, Vec<Type>, Type)>,
    trace_semantic_node: TracingConfig,
    trace_type_resolver: TracingConfig,
    trace_path: TracingConfig,
) -> Result<Module<SemanticAnnotations>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(&ast, trace_semantic_node);
    SymbolTable::add_item_defs_to_table(&mut sm_ast)?;

    let mut semantic = TypeResolver::new(&sm_ast);

    for (name, params, ret_ty) in imported_functions.into_iter() {
        semantic.import_function(name.clone(), params.clone(), ret_ty.clone());
    }

    semantic.set_tracing(trace_type_resolver);
    semantic.path_tracing = trace_path;
    semantic.resolve_types()
}

pub struct TypeResolver<'a> {
    symbols: SymbolTableScopeStack<'a>, // I think I can move this into a Cell<> and then make `resolve_types` into &self instead of &mut self
    tracing: TracingConfig,
    path_tracing: TracingConfig,
    imported_symbols: HashMap<String, Symbol>,
    main_fn: Path,
}

impl<'a> Tracing for TypeResolver<'a> {
    fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }
}

impl<'a> TypeResolver<'a> {
    pub fn new(root: &'a Module<SemanticAnnotations>) -> TypeResolver {
        TypeResolver {
            symbols: SymbolTableScopeStack::new(root),
            tracing: TracingConfig::Off,
            path_tracing: TracingConfig::Off,
            imported_symbols: HashMap::new(),
            main_fn: vec!["root", "my_main"].into(),
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

    pub fn resolve_types(&mut self) -> Result<Module<SemanticAnnotations>> {
        self.analyze_module(self.symbols.root)
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
            ty: p,
            ..
        } = routine;
        let canon_path = self
            .symbols
            .to_path()
            .map(|mut p| {
                p.push(name);
                p
            })
            .expect("Failed to create canonical path for function");

        // If routine is root::my_main it must be a function type and have type () -> i64
        if canon_path == self.main_fn {
            Self::validate_main_fn(routine).map_err(|e| format!("L{}: {}", annotations.ln, e))?;
        }

        let mut meta = annotations.clone();

        // canonize routine parameter types
        let canonical_params = self.params_to_canonical(&params)?;
        meta.ty = self.symbols.canonize_local_type_ref(p)?;
        meta.set_canonical_path(canon_path);

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
        meta.set_canonical_path(
            self.symbols
                .to_canonical(&vec![struct_def.get_name().clone()].into())?,
        );

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
        let mut meta = ex.annotation().clone();
        meta.ty = self.symbols.canonize_local_type_ref(ex.get_return_type())?;
        meta.set_canonical_path(vec![ex.get_name().clone()].into());

        Ok(Extern::new(
            ex.get_name().clone(),
            meta.clone(),
            canonical_params,
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
            let mut meta = meta.clone();
            meta.ty = self.symbols.canonize_local_type_ref(bind.get_type())?;
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
                    let exp = self.traverse(val, current_func)?;
                    let (_, ret_ty) = self.symbols.lookup_coroutine(current_func)?;
                    let ret_ty = self.symbols.canonize_local_type_ref(ret_ty)?;
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
            &Expression::Integer8(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I8;
                Ok(Expression::Integer8(meta, *v))
            }
            &Expression::Integer16(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I16;
                Ok(Expression::Integer16(meta, *v))
            }
            &Expression::Integer32(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I32;
                Ok(Expression::Integer32(meta, *v))
            }
            &Expression::Integer64(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::I64;
                Ok(Expression::Integer64(meta, *v))
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
                meta.ty = self
                    .symbols
                    .canonize_local_type_ref(&Type::Array(Box::new(el_ty), *len))?;

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
                if n_index.annotation().ty() != Type::I64 {
                    return Err(format!(
                        "Expected i64 for index but found {}",
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
                let (symbol, routine_canon_path) =
                    self.symbols.lookup_symbol_by_path(routine_path)?;

                let (expected_param_tys, ret_ty) =
                    self.extract_routine_type_info(symbol, call, &routine_path)?;
                let expected_param_tys = expected_param_tys
                    .iter()
                    .map(|pty| {
                        self.symbols
                            .canonize_nonlocal_type_ref(&routine_canon_path.parent(), pty)
                    })
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
                let mut meta = meta.clone();
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

                meta.ty = Type::Custom(canonical_path.clone());
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
            Minus => {
                if operand.get_type() == Type::I8
                    || operand.get_type() == Type::I16
                    || operand.get_type() == Type::I32
                    || operand.get_type() == Type::I64
                {
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
                if l.get_type().is_int() && r.get_type().is_int() && l.get_type() == r.get_type() {
                    Ok((l.get_type().clone(), l, r))
                } else {
                    let expected = if l.get_type().is_int() {
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
    ) -> Result<(&'b Vec<Type>, Type)> {
        let routine_path_parent = routine_path.parent();
        let (expected_param_tys, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, rty),
                ..
            } if *call == RoutineCall::Function => (
                pty,
                self.symbols
                    .canonize_nonlocal_type_ref(&routine_path_parent, rty)?,
            ),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == RoutineCall::CoroutineInit => (
                pty,
                Type::Coroutine(Box::new(
                    self.symbols
                        .canonize_nonlocal_type_ref(&routine_path_parent, rty)?,
                )),
            ),
            _ => {
                let expected = match call {
                    RoutineCall::Function => "function",
                    RoutineCall::CoroutineInit => "coroutine",
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

    fn validate_main_fn(routine: &RoutineDef<SemanticAnnotations>) -> Result<()> {
        let RoutineDef {
            def, params, ty: p, ..
        } = routine;

        // If routine is root::my_main it must be a function type and have type () -> i64
        if def != &RoutineDefType::Function {
            return Err(format!(
                "root::my_main must be a function of type () -> i64"
            ));
        }

        if params.len() > 0 {
            return Err(format!(
                "root::my_main must take no parameters. It must be of type () -> i64"
            ));
        }

        if p != Type::I64 {
            return Err(format!(
                "root::my_main must return an i64. It must be of type () -> i64"
            ));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
    use crate::parser::parser;

    #[test]
    pub fn test_identifiers() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> u8 {
                    let k: u8 := 5u8;
                    return k;
                }",
                Ok(Type::U8),
            ),
            (
                "fn main() -> u16 {
                    let k: u16 := 5u16;
                    return k;
                }",
                Ok(Type::U16),
            ),
            (
                "fn main() -> u32 {
                    let k: u32 := 5u32;
                    return k;
                }",
                Ok(Type::U32),
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5u64;
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i16 {
                    let k: i16 := 5i16;
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                "fn main() -> i8 {
                    let k: i8 := 5i8;
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return k;
                }",
                Ok(Type::Bool),
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
                    let k: i32 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i32 but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i16 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i16 but got i64"),
            ),
            (
                "fn main() -> bool {
                    let k: i8 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i8 but got i64"),
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5i64;
                    return k;
                }",
                Err("Semantic: L2: Bind expected u64 but got i64"),
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
            let ast = parser::parse(tokens).expect(&format!("{}", text)).unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { body, .. }) = &result.get_functions()[0] {
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

    #[test]
    pub fn test_my_main_signature() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn my_main() -> i64 {
                    return 0;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn my_main() -> i32 {
                    return 0i32;
                }",
                Err("Semantic: L1: root::my_main must return an i64. It must be of type () -> i64"),
            ),
            (
                line!(),
                "fn my_main(i: i32) -> i64 {
                    return 0;
                }",
                Err("Semantic: L1: root::my_main must take no parameters. It must be of type () -> i64"),
            ),
            (
                line!(),
                "co my_main() -> i64 {
                    return 0;
                }",
                Err("Semantic: L1: root::my_main must be a function of type () -> i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    assert_eq!(
                        fn_main.annotation().ty,
                        expected_ty,
                        "Test Case at L:{}",
                        line
                    );
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg, "Test Case at L:{}", line);
                }
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { params, .. }) = &result.get_functions()[0] {
                if let Parameter {
                    ty: Type::Custom(ty_path),
                    ..
                } = &params[0]
                {
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

    #[test]
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Routine(RoutineDef { params, .. }) = &result.get_coroutines()[0] {
                if let Type::Custom(ty_path) = &params[0].ty {
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
            let result = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            )
            .unwrap();
            if let Item::Struct(s) = &result.get_structs()[1] {
                let fields = s.get_fields();
                if let Type::Custom(ty_path) = &fields[0].ty {
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
    pub fn test_integer_arithmetic_type_checking() {
        for (line, text, expected) in vec![
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := 1 + 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i64 {
                    let k: i64 := (1 + 5i64) * (3 - 4/(2 + 3));
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := 1i32 + 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2i32 + 3i32));
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                line!(),
                "fn main() -> i16 {
                    let k: i16 := 1i16 + 5i16;
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                line!(),
                "fn main() -> i16 {
                    let k: i16 := (1i16 + 5i16) * (3i16 - 4i16/(2i16 + 3i16));
                    return k;
                }",
                Ok(Type::I16),
            ),
            (
                line!(),
                "fn main() -> i8 {
                    let k: i8 := 1i8 + 5i8;
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                line!(),
                "fn main() -> i8 {
                    let k: i8 := (1i8 + 5i8) * (3i8 - 4i8/(2i8 + 3i8));
                    return k;
                }",
                Ok(Type::I8),
            ),
            (
                line!(),
                "fn main() -> u64 {
                    let k: u64 := 1u64 + 5u64;
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                line!(),
                "fn main() -> u8 {
                    let k: u8 := (1u8 + 5u8) * (3u8 - 4u8/(2u8 + 3u8));
                    return k;
                }",
                Ok(Type::U8),
            ),
            (
                line!(),
                "fn main() -> u16 {
                    let k: u16 := (1u16 + 5u16) * (3u16 - 4u16/(2u16 + 3u16));
                    return k;
                }",
                Ok(Type::U16),
            ),
            (
                line!(),
                "fn main() -> u32 {
                    let k: u32 := (1u32 + 5u32) * (3u32 - 4u32/(2u32 + 3u32));
                    return k;
                }",
                Ok(Type::U32),
            ),
            (
                line!(),
                "fn main() -> u64 {
                    let k: u64 := (1u64 + 5u64) * (3u64 - 4u64/(2u64 + 3u64));
                    return k;
                }",
                Ok(Type::U64),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2i32 + 3));
                    return k;
                }",
                Err("Semantic: L2: + expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i32 := (1i32 + 5i32) * (3i32 - 4i32/(2 + 3));
                    return k;
                }",
                Err("Semantic: L2: / expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1 + 5i32;
                    return k;
                }",
                Err("Semantic: L2: + expected i64 but found i64 and i32"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i32 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i32 but found i32 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i8 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i8 but found i8 and i64"),
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: u64 := 1u64 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected u64 but found u64 and i64"), // TODO: Change this error message to specify the right operand is wrong
            ),
            (
                line!(),
                "fn main() -> i32 {
                    let k: i64 := 1i16 + 5i64;
                    return k;
                }",
                Err("Semantic: L2: + expected i16 but found i16 and i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    assert!(module.is_ok(), "Test Case at L:{}", line);
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(
                            bind_stm.annotation().ty,
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                        assert_eq!(
                            b.get_rhs().get_type(),
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        assert_eq!(
                            r.get_value().clone().unwrap().get_type(),
                            expected_ty,
                            "Test Case at L:{}",
                            line
                        );
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg, "Test Case at L:{}", line);
                }
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
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return -k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i16 {
                    let k: i16 := 5i16;
                    return -k;
                }",
                Ok(Type::I16),
            ),
            (
                "fn main() -> i8 {
                    let k: i8 := 5i8;
                    return -k;
                }",
                Ok(Type::I8),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return -k;
                }",
                Err("Semantic: L3: - expected i32 or i64 but found bool"), // TODO: Change this error message to include i8 and i16
            ),
            (
                "fn main() -> u64 {
                    let k: u64 := 5u64;
                    return -k;
                }",
                Err("Semantic: L3: - expected i32 or i64 but found u64"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return !k;
                }",
                Ok(Type::Bool),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(bind_stm.annotation().ty, Type::I64);
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
                Ok(Type::Bool),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
                Ok(Type::Bool),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.annotation().ty, Type::Bool);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
                    Ok(Type::Bool),
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
                let module = resolve_types(
                    &ast,
                    TracingConfig::Off,
                    TracingConfig::Off,
                    TracingConfig::Off,
                );
                match expected {
                    Ok(expected_ty) => {
                        let module = module.unwrap();
                        let fn_main = module.get_functions()[0].to_routine().unwrap();

                        let bind_stm = &fn_main.get_body()[0];
                        assert_eq!(bind_stm.get_type(), Type::Bool);

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
    pub fn test_array_at_index() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let a: [i64; 5] := [1, 2, 3, 4, 5];
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i64 {
                    let a: [i64; 5] := [[1, 2, 3, 4, 5]][0];
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i64 {
                    let a: [[i64; 1]; 1] := [[1]];
                    let k: i64 := a[0][0];
                    return k * 3;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> bool {
                    let a: [i64; 5] := [1, 2, 3, 4, 5];
                    let k: i64 := a[false];
                    return k * 3;
                }",
                Err("Semantic: L3: Expected i64 for index but found bool"),
            ),
            (
                "fn main() -> bool {
                    let a: i64 := 1;
                    let k: i64 := a[0];
                    return k * 3;
                }",
                Err("Semantic: L3: Expected array type on LHS of [] but found i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[1];
                    assert_eq!(bind_stm.annotation().ty, Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[2];
                    assert_eq!(ret_stm.annotation().ty, expected_ty);
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
    pub fn test_bind_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i64 {
                    let k: i64 := 5;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5i64;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5i64;
                    return k;
                }",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5i32;
                    return k;
                }",
                Ok(Type::I32),
            ),
            (
                "fn main() -> [i64;5] {
                    let k: [i64;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Ok(Type::Array(Box::new(Type::I64), 5)),
            ),
            (
                "fn main() -> [i32;5] {
                    let k: [i64;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L3: Return expected [i32; 5] but got [i64; 5]"),
            ),
            (
                "fn main() -> [i32;0] {
                    let k: [i64;0] := [];
                    return k;
                }",
                Err("Semantic: Expected length > 0 for array, but found 0"),
            ),
            (
                "fn main() -> [i32;1] {
                    [];
                    let k: [i64;1] := [1];
                    return k;
                }",
                Err("Semantic: L2: Arrays with 0 length are not allowed"),
            ),
            (
                "fn main() -> [i32;5] {
                    let k: [i32;5] := [1, 2, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L2: Bind expected [i32; 5] but got [i64; 5]"),
            ),
            (
                "fn main() -> [i64;5] {
                    let k: [i64;5] := [1, 2i32, 3, 4, 5];
                    return k;
                }",
                Err("Semantic: L2: Inconsistent types in array value"),
            ),
            (
                "fn main() -> i64 {
                    let k: i32 := 5;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i32 but got i64"),
            ),
            (
                "fn main() -> i64 {
                    let k: i64 := 5i32;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i64 but got i32"),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);
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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Type::I64);

                    // validate that the RHS of the bind is the correct type
                    if let Statement::Bind(box b) = bind_stm {
                        assert_eq!(b.get_rhs().get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the mutate statement is typed correctly
                    let mut_stm = &fn_main.get_body()[1];
                    assert_eq!(mut_stm.get_type(), Type::I64);
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
                Ok(Type::I64),
            ),
            (
                "fn main() -> bool {
                    return false;
                }",
                Ok(Type::Bool),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
                            .unwrap_or(Type::Unit);
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
    pub fn test_extern_calls() {
        for (text, expected) in vec![
            (
                "
                extern fn number() -> i64;
                fn main() -> i64 {
                    return number();
                }
                ",
                Ok(Type::I64),
            ),
            (
                "
                extern fn number() -> i32;
                fn main() -> i32 {
                    return number();
                }
                ",
                Ok(Type::I32),
            ),
            (
                "fn main() -> bool {
                    return number();
                }
                extern fn number() -> i64;
                ",
                Err("Semantic: L2: Return expected bool but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
                            .unwrap_or(Type::Unit);
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
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return 5i32;}
                ",
                Ok(Type::I32),
            ),
            (
                // test recursion
                "fn main() -> i64 {
                    return number();
                }
                fn number() -> i64 {return number();}
                ",
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return 5;}
                ",
                Err("Semantic: L4: Return expected i32 but got i64"),
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
                Ok(Type::I64),
            ),
            (
                "fn main() -> i32 {
                    return add(1i32, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Ok(Type::I32),
            ),
            (
                "fn main() -> i32 {
                    return add(1, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function add: parameter 1 expected i32 but got i64"),
            ),
            (
                "fn main() -> i64 {
                    return add(1i32, 2i32);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: Return expected i64 but got i32"),
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
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Statement::Return(box r) = ret_stm {
                        let value_ty = r.get_value().clone().map(|v| v.get_type().clone()).unwrap_or(Type::Unit);
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
                Ok(Type::Coroutine(Box::new(Type::I64))),
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
                Ok(Type::Coroutine(Box::new(Type::I64))),
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
            (
                "fn main() {
                    let c: co i32 := init number(5i32);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Ok(Type::Coroutine(Box::new(Type::I32))),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function number: parameter 1 expected i32 but got i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_coroutines()[0].to_routine().unwrap();

                    let yret_stm = &co_number.get_body()[0];
                    assert_eq!(yret_stm.get_type(), Type::I64);

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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &co_number.get_body()[1];
                    assert_eq!(bind_stm.get_type(), Type::I64);

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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
                            .unwrap_or(Type::Unit);
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
                Ok(Type::I64),
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
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
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
                            .unwrap_or(Type::Unit);
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
                Ok(Type::I64),
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
                Ok(Type::Unit),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // Check the return value
                    if expected_ty != Type::Unit {
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
    pub fn test_while_expressions() {
        for (text, expected) in vec![
            (
                "fn main() {
                    while (true) {1;};
                    return;
                }
                ",
                Ok(Type::Unit),
            ),
            (
                "fn main() {
                    return while (true) {1;};
                }
                ",
                Ok(Type::Unit),
            ),
            (
                "fn main() {
                    while (5) {1;};
                    return;
                }
                ",
                Err("Semantic: L2: The condition of a while expression must resolve to a unit type, but got: i64"),
            ),
            (
                "fn main() {
                    while (true) {1};
                    return;
                }
                ",
                Err("Semantic: L2: The body of a while expression must resolve to a unit type, but got: i64"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(
                &ast,
                TracingConfig::Off,
                TracingConfig::Off,
                TracingConfig::Off,
            );
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), expected_ty);

                    // Check the return value
                    if expected_ty != Type::Unit {
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
            let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off);
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
                let result = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off, TracingConfig::Off);
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
            let mut sm_ast = sa.from_module(&ast, TracingConfig::Off);
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
