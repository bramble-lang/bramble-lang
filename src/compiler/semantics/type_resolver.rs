use crate::compiler::diagnostics::{Event, EventStack, Logger, View2};
use crate::compiler::source::SourceIr;
use crate::compiler::Span;
use crate::{
    compiler::{
        ast::*,
        import::Import,
        parser::ParserContext,
        semantics::semanticnode::{SemanticAst, SemanticNode},
        semantics::symbol_table::*,
        CompilerError,
    },
    StringId,
};
use std::collections::HashMap;

use super::semanticnode::Addressability;
use super::TypeOk;
use super::{
    canonize::canonize_paths, semanticnode::SemanticContext, stack::SymbolTableScopeStack,
    SemanticError, SemanticResult,
};

pub fn resolve_types(
    ast: &Module<ParserContext>,
    main_mod: StringId,
    main_fn: StringId,
    logger: &Logger,
) -> SemanticResult<Module<SemanticContext>> {
    resolve_types_with_imports(ast, main_mod, main_fn, &vec![], logger)
}

pub fn resolve_types_with_imports(
    ast: &Module<ParserContext>,
    main_mod: StringId,
    main_fn: StringId,
    imports: &[Import],
    logger: &Logger,
) -> SemanticResult<Module<SemanticContext>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(ast);
    canonize_paths(&mut sm_ast, imports, logger)?; //TODO: Add a trace for this step
    SymbolTable::add_item_defs_to_table(&mut sm_ast)
        .map_err(|e| CompilerError::new(Span::zero(), e))?;

    let mut semantic = TypeResolver::new(&sm_ast, imports, main_mod, main_fn, logger);

    semantic.resolve_types()
}

pub struct TypeResolver<'a> {
    symbols: SymbolTableScopeStack,
    imported_symbols: HashMap<String, Symbol>,
    main_fn: Path,
    logger: &'a Logger<'a>,
    event_stack: EventStack,
}

impl<'a> TypeResolver<'a> {
    pub fn new(
        root: &Module<SemanticContext>,
        imports: &[Import],
        main_mod: StringId,
        main_fn: StringId,
        logger: &'a Logger,
    ) -> TypeResolver<'a> {
        TypeResolver {
            symbols: SymbolTableScopeStack::new(root, imports),
            imported_symbols: HashMap::new(),
            main_fn: vec![
                Element::CanonicalRoot,
                Element::Id(main_mod),
                Element::Id(main_fn),
            ]
            .into(), // TODO: should get rid of this,
            logger,
            event_stack: EventStack::new(),
        }
    }

    pub fn resolve_types(&mut self) -> SemanticResult<Module<SemanticContext>> {
        // TODO: I think that this is the problem, perhaps I should get rid of the concept
        // of the stack root?  I need root to be able to find items using the stack.
        self.analyze_module(self.symbols.get_root())
    }

    fn analyze_module(
        &mut self,
        m: &Module<SemanticContext>,
    ) -> SemanticResult<Module<SemanticContext>> {
        let mut nmodule = Module::new(m.get_name(), m.context().clone());

        self.symbols.enter_scope(nmodule.context().sym().clone());

        *nmodule.get_modules_mut() = m
            .get_modules()
            .iter()
            .map(|m| self.analyze_module(m))
            .collect::<SemanticResult<Vec<Module<SemanticContext>>>>()?;
        *nmodule.get_functions_mut() = m
            .get_functions()
            .iter()
            .map(|f| self.analyze_item(f))
            .collect::<SemanticResult<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_coroutines_mut() = m
            .get_coroutines()
            .iter()
            .map(|c| self.analyze_item(c))
            .collect::<SemanticResult<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_structs_mut() = m
            .get_structs()
            .iter()
            .map(|s| self.analyze_item(s))
            .collect::<SemanticResult<Vec<Item<SemanticContext>>>>()?;
        *nmodule.get_externs_mut() = m
            .get_externs()
            .iter()
            .map(|e| self.analyze_item(e))
            .collect::<SemanticResult<Vec<Item<SemanticContext>>>>()?;

        // We can ignore the returned symbol table because currently, the type
        // resolver will not modify the symbol table of a module. As only routine
        // and expression block symbol tables can be modified (through binds).
        self.symbols.leave_scope();

        Ok(nmodule)
    }

    fn analyze_item(&mut self, i: &Item<SemanticContext>) -> SemanticResult<Item<SemanticContext>> {
        match i {
            Item::Struct(s) => self.analyze_structdef(s).map(|s2| Item::Struct(s2)),
            Item::Routine(r) => self.analyze_routine(r).map(|r2| Item::Routine(r2)),
            Item::Extern(ex) => self.analyze_extern(ex).map(|e2| Item::Extern(e2)),
        }
    }

    fn analyze_routine(
        &mut self,
        routine: &RoutineDef<SemanticContext>,
    ) -> SemanticResult<RoutineDef<SemanticContext>> {
        let (event, result) = self.new_event().and_then(|| {
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
            if context.canonical_path() == &self.main_fn {
                Self::validate_main_fn(routine)?;
            }

            let mut ctx = context.with_type(ret_ty.clone());

            // Add parameters to symbol table
            let mut resolved_params = vec![];
            for p in params.iter() {
                // Check that the type exists
                self.valid_type(&p.ty, ctx.span())?;

                // Set the resolved type of the parameter node
                let mut resolved_param = p.clone();
                let param_ctx = p.context().with_type(p.ty.clone());
                resolved_param.context = param_ctx;

                // Add parameter to the routines symbol table
                ctx.add_symbol(
                    resolved_param.name,
                    resolved_param.ty.clone(),
                    false,
                    false,
                    resolved_param.span(),
                )
                .map_err(|e| CompilerError::new(p.span(), e))?;

                self.record(&resolved_param, vec![]);
                resolved_params.push(resolved_param);
            }

            self.symbols.enter_scope(ctx.sym().clone());

            let mut resolved_body = vec![];
            for stmt in body.iter() {
                let exp = self.analyze_statement(stmt)?;
                resolved_body.push(exp);
            }

            let sym = self.symbols.leave_scope();

            Ok(RoutineDef {
                context: ctx.with_sym(sym),
                def: def.clone(),
                name: *name,
                params: resolved_params,
                ret_ty: ret_ty.clone(),
                body: resolved_body,
            })
        });

        result.view(|v| self.record2(event, v, vec![]))
    }

    fn analyze_structdef(
        &mut self,
        struct_def: &StructDef<SemanticContext>,
    ) -> SemanticResult<StructDef<SemanticContext>> {
        let (event, result) = self.new_event().and_then(|| {
            // Check the type of each member
            let mut resolved_fields = vec![];
            let fields = struct_def.get_fields();
            for f in fields.iter() {
                self.valid_type(&f.ty, f.context().span())?;

                let ctx = f.context().with_type(f.ty.clone());
                let mut resolved_field = f.clone();
                resolved_field.context = ctx;

                self.record(&resolved_field, vec![]);
                resolved_fields.push(resolved_field);
            }

            // Update the context with canonical path information and set the type to Type::Unit
            let ctx = struct_def.context().with_type(Type::Unit);

            Ok(StructDef::new(
                struct_def.get_name(),
                ctx,
                resolved_fields,
            ))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    fn analyze_extern(
        &mut self,
        ex: &Extern<SemanticContext>,
    ) -> SemanticResult<Extern<SemanticContext>> {
        // Check the type of each member
        let (event, result) = self.new_event().and_then(|| {
            let params = ex.get_params();
            for Parameter { ty: field_type, .. } in params.iter() {
                if let Type::Custom(_) = field_type {
                    panic!("Custom types are not supported for extern function declarations")
                }
            }

            // Update the context with canonical path information and set the type to Type::Unit
            let name = ex.name().expect("Externs must have a name");
            let ctx = ex.context().with_type(ex.get_return_type().clone());
            let ret_ty = ctx.ty().clone();

            Ok(Extern::new(
                name,
                ctx,
                params.clone(),
                ex.has_varargs,
                ret_ty,
            ))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    fn analyze_statement(
        &mut self,
        stmt: &Statement<SemanticContext>,
    ) -> SemanticResult<Statement<SemanticContext>> {
        use Statement::*;
        let inner = match stmt {
            Bind(b) => Bind(Box::new(self.analyze_bind(b)?)),
            Mutate(b) => Mutate(Box::new(self.analyze_mutate(b)?)),
            Return(x) => Return(Box::new(self.analyze_return(x)?)),
            YieldReturn(x) => YieldReturn(Box::new(self.analyze_yieldreturn(x)?)),
            Expression(e) => Expression(Box::new(self.analyze_expression(e)?)),
        };

        Ok(inner)
    }

    fn analyze_bind(
        &mut self,
        bind: &Bind<SemanticContext>,
    ) -> SemanticResult<Bind<SemanticContext>> {
        let ctx = bind.context();
        let rhs = bind.get_rhs();
        let (event, result) = self.new_event().and_then(|| {
            {
                // Check that the type from the type annotation exists
                self.valid_type(bind.get_type(), ctx.span())?;
                let ctx = ctx.with_type(bind.get_type().clone());
                let rhs = self.analyze_expression(rhs)?;
                if ctx.ty().can_be_assigned(rhs.get_type()) {
                    match self.symbols.add(
                        bind.get_id(),
                        ctx.ty().clone(),
                        bind.is_mutable(),
                        false,
                        bind.span(),
                    ) {
                        Ok(()) => {
                            let ty = ctx.ty().clone();
                            Ok(Bind::new(ctx, bind.get_id(), ty, bind.is_mutable(), rhs))
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    Err(SemanticError::BindExpected(
                        ctx.ty().clone(),
                        rhs.get_type().clone(),
                    ))
                }
            }
            .map_err(|e| CompilerError::new(ctx.span(), e))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    fn analyze_mutate(
        &mut self,
        mutate: &Mutate<SemanticContext>,
    ) -> SemanticResult<Mutate<SemanticContext>> {
        let (event, result) = self.new_event().and_then(|| {
            let lhs = self.analyze_expression(mutate.get_lhs())?;
            let rhs = self.analyze_expression(mutate.get_rhs())?;
            if lhs.context().is_mutable() {
                if lhs.get_type().can_be_assigned(rhs.get_type()) {
                    let ctx = mutate.context().with_type(rhs.get_type().clone());
                    Ok(Mutate::new(ctx, lhs, rhs))
                } else {
                    Err(SemanticError::BindMismatch(
                        lhs.span(),
                        lhs.get_type().clone(),
                        rhs.get_type().clone(),
                    ))
                }
            } else {
                Err(SemanticError::ExpressionNotMutable(lhs.span()))
            }
            .map_err(|e| CompilerError::new(mutate.span(), e))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    fn analyze_yieldreturn(
        &mut self,
        yr: &YieldReturn<SemanticContext>,
    ) -> SemanticResult<YieldReturn<SemanticContext>> {
        let (event, result) = self.new_event().and_then(|| {
            // Get the actual expression and its type as it comes from the
            // source code written by the user.
            let (actual_ret_exp, actual_ret_ty) = match yr.get_value() {
                None => (None, Type::Unit),
                Some(exp) => {
                    let exp = self.analyze_expression(exp)?;
                    let exp_ty = exp.get_type().clone();
                    (Some(exp), exp_ty)
                }
            };

            // Get the expected yield return type of the coroutine that the yield return
            // occurs within.
            let current_func = self.symbols.get_current_fn().ok_or_else(|| {
                CompilerError::new(yr.span(), SemanticError::YieldInvalidLocation)
            })?;
            let (_, expected_ret_ty) = self
                .symbols
                .lookup_coroutine(current_func)
                .map_err(|e| CompilerError::new(yr.span(), e))?;

            if actual_ret_ty == expected_ret_ty {
                let ctx = yr.context().with_type(actual_ret_ty);
                Ok(YieldReturn::new(ctx, actual_ret_exp))
            } else {
                Err(SemanticError::YieldExpected(
                    expected_ret_ty.clone(),
                    actual_ret_ty,
                ))
            }
            .map_err(|e| CompilerError::new(yr.span(), e))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    fn analyze_return(
        &mut self,
        r: &Return<SemanticContext>,
    ) -> SemanticResult<Return<SemanticContext>> {
        let (event, result) = self.new_event().and_then(|| {
            // Get the actual expression and its type as it comes from the
            // source code written by the user.
            let (actual_ret_exp, actual_ret_ty) = match r.get_value() {
                None => (None, Type::Unit),
                Some(exp) => {
                    let exp = self.analyze_expression(exp)?;
                    let exp_ty = exp.get_type().clone();
                    (Some(exp), exp_ty)
                }
            };

            // Get the expected return type of the function that the return
            // occurs within.
            let current_func = self
                .symbols
                .get_current_fn()
                .ok_or_else(|| SemanticError::ReturnInvalidLocation)
                .map_err(|e| CompilerError::new(r.span(), e))?;
            let (_, expected_ret_ty) = self
                .symbols
                .lookup_func_or_cor(current_func)
                .map_err(|e| CompilerError::new(r.span(), e))?;

            // Check that the actual expression matches the expected return type
            // of the function
            if actual_ret_ty == expected_ret_ty {
                let ctx = r.context().with_type(actual_ret_ty);
                Ok(Return::new(ctx, actual_ret_exp))
            } else {
                Err(SemanticError::ReturnExpected(
                    expected_ret_ty.clone(),
                    actual_ret_ty,
                ))
            }
            .map_err(|e| CompilerError::new(r.span(), e))
        });
        result.view(|e| self.record2(event, e, vec![]))
    }

    /// Recursively resolve every child of the given expression and check that every
    /// operator and function is given operands with the correct types.  Finally,
    /// return the type that the expression ultimately resolves to or return an
    /// error if there is a mismatch between the required type of an operand and
    /// the given type of an operand.
    fn analyze_expression(&mut self, ast: &SemanticNode) -> SemanticResult<SemanticNode> {
        let mut refs = vec![];
        let (event, result) = self.new_event().and_then(|| {
        match &ast {
            Expression::Null(ctx) => {
                let ctx = ctx.with_type(Type::Null);
                Ok(Expression::Null(ctx))
            }
            Expression::U8(ctx, v) => {
                let ctx = ctx.with_type(Type::U8);
                Ok(Expression::U8(ctx, *v))
            }
            Expression::U16(ctx, v) => {
                let ctx = ctx.with_type(Type::U16);
                Ok(Expression::U16(ctx, *v))
            }
            Expression::U32(ctx, v) => {
                let ctx = ctx.with_type(Type::U32);
                Ok(Expression::U32(ctx, *v))
            }
            Expression::U64(ctx, v) => {
                let ctx = ctx.with_type(Type::U64);
                Ok(Expression::U64(ctx, *v))
            }
            Expression::I8(ctx, v) => {
                let ctx = ctx.with_type(Type::I8);
                Ok(Expression::I8(ctx, *v))
            }
            Expression::I16(ctx, v) => {
                let ctx = ctx.with_type(Type::I16);
                Ok(Expression::I16(ctx, *v))
            }
            Expression::I32(ctx, v) => {
                let ctx = ctx.with_type(Type::I32);
                Ok(Expression::I32(ctx, *v))
            }
            Expression::I64(ctx, v) => {
                let ctx = ctx.with_type(Type::I64);
                Ok(Expression::I64(ctx, *v))
            }
            Expression::F64(ctx, v) => {
                let ctx = ctx.with_type(Type::F64);
                Ok(Expression::F64(ctx, *v))
            }
            Expression::Boolean(ctx, v) => {
                let ctx = ctx.with_type(Type::Bool);
                Ok(Expression::Boolean(ctx, *v))
            }
            Expression::StringLiteral(ctx, v) => {
                let ctx = ctx.with_type(Type::StringLiteral);
                Ok(Expression::StringLiteral(ctx, *v))
            }
            Expression::ArrayExpression(ctx, elements, len) => {
                // Resolve the types for each element in the array value
                let nelements: SemanticResult<Vec<Expression<SemanticContext>>> = elements
                    .iter()
                    .map(|e| self.analyze_expression(e))
                    .collect();
                let nelements = nelements?;

                // Check that they are homogenous
                let el_ty;
                if nelements.is_empty() {
                    return Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::ArrayInvalidSize(nelements.len()),
                    ));
                } else {
                    el_ty = nelements[0].context().ty().clone();
                    for e in &nelements {
                        if !e.context().ty().can_be_assigned(&el_ty) {
                            return Err(CompilerError::new(
                                ctx.span(),
                                SemanticError::ArrayInconsistentElementTypes,
                            ));
                        }
                    }
                }

                // Use the size of the array and the type to define the array type
                let ctx = ctx.with_type(Type::Array(Box::new(el_ty), *len));
                Ok(Expression::ArrayExpression(ctx, nelements, *len))
            }
            Expression::ArrayAt {
                context: ctx,
                array,
                index,
            } => {
                //  Check that the array value is an array type
                let array = self.analyze_expression(array)?;

                refs.push(array.span());

                let el_ty = match array.context().ty() {
                    Type::Array(el_ty, _) => Ok(*el_ty.clone()),
                    ty => Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::ArrayIndexingInvalidType(ty.clone()),
                    )),
                }?;

                // Check that the index is an i64 type
                let n_index = self.analyze_expression(index)?;
                if !n_index.context().ty().is_integral() {
                    return Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::ArrayIndexingInvalidIndexType(
                            n_index.context().ty().clone(),
                        ),
                    ));
                }

                // If the source expression is an addressable location or is mutable then copy that
                // property
                let ctx = if array.context().is_mutable() {
                    ctx.with_type(el_ty)
                        .with_addressable(true)
                } else if array.context().is_addressable() {
                    ctx.with_type(el_ty)
                        .with_addressable(false)
                } else {
                    ctx.with_type(el_ty)
                };

                Ok(Expression::ArrayAt {
                    context: ctx,
                    array: Box::new(array),
                    index: Box::new(n_index),
                })
            }
            Expression::SizeOf(ctx, ty) => {
                let ctx = ctx.with_type(Type::U64);
                self.valid_type(ty.as_ref(), ctx.span())?;
                Ok(Expression::SizeOf(ctx, ty.clone()))
            }
            Expression::CustomType(ctx, name) => {
                let ctx = ctx.with_type(Type::Custom(name.clone()));
                Ok(Expression::CustomType(ctx, name.clone()))
            }
            Expression::IdentifierDeclare(ctx, name, p) => {
                let ctx = ctx.with_type(p.clone());
                Ok(Expression::IdentifierDeclare(ctx, *name, p.clone()))
            }
            Expression::Identifier(ctx, id) => {
                let ctx = match self
                    .symbols
                    .lookup_var(*id)
                    .map_err(|e| CompilerError::new(ctx.span(), e))?
                {
                    Symbol { ty: p, span, is_mutable, .. } => {
                        span.and_then(|s| Some(refs.push(s)));
                        ctx.with_type(p.clone()).with_addressable(*is_mutable)
                    }
                };
                Ok(Expression::Identifier(ctx, *id))
            }
            Expression::Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            Expression::MemberAccess(ctx, src, member) => {
                // Get the type of src and look up its struct definition
                // Check the struct definition for the type of `member`
                // if it exists, if it does not exist then return an error
                let src = self.analyze_expression(src)?;
                match src.get_type() {
                    Type::Custom(struct_name) => {
                        let (struct_def, _) = self
                            .symbols
                            .lookup_symbol_by_path(struct_name)
                            .map_err(|e| CompilerError::new(ctx.span(), e))?;

                        // Record the span of the struct definition as a reference for resolving the type of the member access
                        if let Some(s) = struct_def.span{ refs.push(s)};

                        let member_ty = struct_def
                            .ty
                            .get_member(*member)
                            .ok_or_else(||SemanticError::MemberAccessMemberNotFound(
                                struct_name.clone(),
                                *member,
                            ))
                            .map_err(|e| CompilerError::new(ctx.span(), e))?;

                        // If the source expression is an addressable location or is mutable then copy that
                        // property
                        let ctx = if src.context().is_mutable() {
                            ctx.with_type(member_ty.clone()).with_addressable(true)
                        } else if src.context().is_addressable() {
                            ctx.with_type(member_ty.clone()).with_addressable(false)
                        } else {
                            ctx.with_type(member_ty.clone())
                        };

                        Ok(Expression::MemberAccess(ctx, Box::new(src), *member))
                    }
                    _ => Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::MemberAccessInvalidRootType(src.get_type().clone()),
                    )),
                }
            }
            Expression::BinaryOp(ctx, op, l, r) => {
                let (ty, l, r) = self.binary_op(*op, l, r)?;
                let ctx = ctx.with_type(ty);
                Ok(Expression::BinaryOp(ctx, *op, Box::new(l), Box::new(r)))
            }
            Expression::UnaryOp(ctx, op, operand) => {
                let (ty, addry, operand) = self.unary_op(*op, operand)?;
                let ctx = ctx.with_type(ty);
                let ctx = match addry {
                    Addressability::Addressable => ctx.with_addressable(false),
                    Addressability::AddressableMutable => ctx.with_addressable(true),
                    _ => ctx,
                };
                Ok(Expression::UnaryOp(ctx, *op, Box::new(operand)))
            }
            Expression::If {
                context: ctx,
                cond,
                if_arm,
                else_arm,
            } => {
                let cond = self.analyze_expression(cond)?;
                if cond.get_type() == Type::Bool {
                    let if_arm = self.analyze_expression(if_arm)?;

                    let else_arm = else_arm
                        .as_ref()
                        .map(|e| self.analyze_expression(e))
                        .map_or(Ok(None), |r| r.map(|x| Some(Box::new(x))))?;

                    let else_arm_ty = else_arm
                        .as_ref()
                        .map(|e| e.get_type().clone())
                        .unwrap_or(Type::Unit);

                    if if_arm.get_type() == else_arm_ty {
                        let ctx = ctx.with_type(if_arm.get_type().clone());
                        Ok(Expression::If {
                            context: ctx,
                            cond: Box::new(cond),
                            if_arm: Box::new(if_arm),
                            else_arm: else_arm,
                        })
                    } else {
                        Err(CompilerError::new(
                            ctx.span(),
                            SemanticError::IfExprMismatchArms(
                                if_arm.get_type().clone(),
                                else_arm_ty,
                            ),
                        ))
                    }
                } else {
                    Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::CondExpectedBool(cond.get_type().clone()),
                    ))
                }
            }
            Expression::While {
                context: ctx,
                cond,
                body,
                ..
            } => {
                let cond = self.analyze_expression(cond)?;
                if cond.get_type() == Type::Bool {
                    let body = self.analyze_expression(body)?;

                    if body.get_type() == Type::Unit {
                        let ctx = ctx.with_type(Type::Unit);
                        Ok(Expression::While {
                            context: ctx,
                            cond: Box::new( cond),
                            body: Box::new(body),
                        })
                    } else {
                        Err(CompilerError::new(
                            ctx.span(),
                            SemanticError::WhileInvalidType(body.get_type().clone()),
                        ))
                    }
                } else {
                    Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::WhileCondInvalidType(cond.get_type().clone()),
                    ))
                }
            }
            Expression::Yield(ctx, exp) => {
                let exp = self.analyze_expression(exp)?;
                let ctx = match exp.get_type() {
                    Type::Coroutine(ret_ty) => ctx.with_type(*ret_ty.clone()),
                    _ => {
                        return Err(CompilerError::new(
                            ctx.span(),
                            SemanticError::YieldInvalidType(exp.get_type().clone()),
                        ))
                    }
                };
                Ok(Expression::Yield(ctx, Box::new(exp)))
            }
            Expression::RoutineCall(ctx, call, routine_path, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut resolved_params = vec![];
                for param in params.iter() {
                    let ty = self.analyze_expression(param)?;

                    resolved_params.push(ty);
                }

                // Check that the function being called exists
                let (symbol, routine_canon_path) = self
                    .symbols
                    .lookup_symbol_by_path(routine_path)
                    .map_err(|e| CompilerError::new(ctx.span(), e))?;

                // record the reference span for this routine definition as a source for type resolution
                if let Some(s) = symbol.span{ refs.push(s)};

                // if the routine is external, then change the call type to extern
                let call = if symbol.is_extern {
                    RoutineCall::Extern
                } else {
                    *call
                };

                let (expected_param_tys, has_varargs, ret_ty) = self
                    .extract_routine_type_info(symbol, &call, &routine_canon_path)
                    .map_err(|e| CompilerError::new(ctx.span(), e))?;

                // Check that parameters are correct and if so, return the node annotated with
                // semantic information
                if !has_varargs && (resolved_params.len() != expected_param_tys.len()) {
                    Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::RoutineCallWrongNumParams(
                            routine_path.clone(),
                            expected_param_tys.len(),
                            resolved_params.len(),
                        ),
                    ))
                } else if has_varargs && (resolved_params.len() < expected_param_tys.len()) {
                    Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::FunctionParamsNotEnough(
                            routine_path.clone(),
                            expected_param_tys.len(),
                            resolved_params.len(),
                        ),
                    ))
                } else {
                    match Self::check_for_invalid_routine_parameters(
                        routine_path,
                        &resolved_params,
                        expected_param_tys,
                        has_varargs,
                    ) {
                        Err(msg) => Err(CompilerError::new(ctx.span(), msg)),
                        Ok(()) => {
                            let ctx = ctx.with_type(ret_ty.clone());
                            Ok(Expression::RoutineCall(
                                ctx,
                                call,
                                routine_canon_path,
                                resolved_params,
                            ))
                        }
                    }
                }
            }
            Expression::ExpressionBlock(ctx, body, final_exp) => {
                let mut resolved_body = vec![];

                self.symbols.enter_scope(ctx.sym().clone());

                for stmt in body.iter() {
                    let exp = self.analyze_statement(stmt)?;
                    resolved_body.push(exp);
                }

                let (final_exp, block_ty) = match final_exp {
                    None => (None, Type::Unit),
                    Some(fe) => {
                        let fe = self.analyze_expression(fe)?;
                        let ty = fe.get_type().clone();
                        (Some(Box::new(fe)), ty)
                    }
                };

                let sym = self.symbols.leave_scope();
                let ctx = ctx.with_type(block_ty).with_sym(sym);

                Ok(Expression::ExpressionBlock(ctx, resolved_body, final_exp))
            }
            Expression::StructExpression(ctx, struct_name, params) => {
                // Validate the types in the initialization parameters
                // match their respective members in the struct
                let (struct_def, canonical_path) = self
                    .symbols
                    .lookup_symbol_by_path(struct_name)
                    .map_err(|e| CompilerError::new(ctx.span(), e))?;

                // Record the span of the struct definition as a reference for resolving the type of the member access
                if let Some(s) = struct_def.span{ refs.push(s)};

                let struct_def_ty = struct_def.ty.clone();
                let expected_num_params = struct_def_ty
                    .get_members()
                    .ok_or_else(||CompilerError::new(
                        ctx.span(),
                        SemanticError::InvalidStructure,
                    ))?
                    .len();
                if params.len() != expected_num_params {
                    return Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::StructExprWrongNumParams(expected_num_params, params.len()),
                    ));
                }

                let mut resolved_params = vec![];
                for (pn, pv) in params.iter() {
                    let member_ty = struct_def_ty.get_member(*pn).ok_or_else(||CompilerError::new(
                        ctx.span(),
                        SemanticError::StructExprMemberNotFound(canonical_path.clone(), *pn),
                    ))?;
                    let param = self.analyze_expression(pv)?;
                    if !member_ty.can_be_assigned(param.get_type()) {
                        return Err(CompilerError::new(
                            ctx.span(),
                            SemanticError::StructExprFieldTypeMismatch(
                                canonical_path,
                                *pn,
                                member_ty.clone(),
                                param.get_type().clone(),
                            ),
                        ));
                    }
                    resolved_params.push((*pn, param));
                }

                let ctx = ctx.with_type(Type::Custom(struct_name.clone()));
                Ok(Expression::StructExpression(
                    ctx,
                    canonical_path,
                    resolved_params,
                ))
            }
            Expression::TypeCast(ctx, exp, target) => {
                // 1. Analyze exp and get it's type
                let exp2 = self.analyze_expression(exp)?;
                // 2. Make sure that exp.ty can be cast to ty  have a method on Type for testing casts.  This keeps the data about
                // casting within the type definitions.
                if exp2.context().ty().can_cast_to(target) {
                    // 4. If it can, then update the type information for this node in the tree
                    let ctx2 = ctx.with_type(target.clone());
                    Ok(Expression::TypeCast(ctx2, Box::new(exp2), target.clone()))
                } else {
                // 3. If not, then return an error
                    return Err(CompilerError::new(
                        ctx.span(),
                        SemanticError::InvalidTypeCast,
                    ));
                }
            },
        }
        });
        result.view(|e| self.record2(event, e, refs))
    }

    /// Check that the operand has the correct type for the given unary
    /// operator and return the type that the unary operation will resolve
    /// to.
    fn unary_op(
        &mut self,
        op: UnaryOperator,
        operand: &SemanticNode,
    ) -> SemanticResult<(Type, Addressability, SemanticNode)> {
        use UnaryOperator::*;

        let operand = self.analyze_expression(operand)?;

        match op {
            Negate => {
                if operand.get_type().is_signed_int() || operand.get_type().is_float() {
                    Ok((operand.get_type().clone(), Addressability::Value, operand))
                } else {
                    Err(CompilerError::new(
                        operand.span(),
                        SemanticError::ExpectedSignedInteger(op, operand.get_type().clone()),
                    ))
                }
            }
            Not => {
                if operand.get_type() == Type::Bool {
                    Ok((Type::Bool, Addressability::Value, operand))
                } else {
                    Err(CompilerError::new(
                        operand.span(),
                        SemanticError::ExpectedBool(op, operand.get_type().clone()),
                    ))
                }
            }
            AddressConst => {
                // The operand must be a location in memory
                if !operand.context().is_addressable() {
                    return Err(CompilerError::new(
                        operand.span(),
                        SemanticError::ExpectedAddressable(op),
                    ));
                }

                // Type is a raw pointer to the type of the operand
                Ok((
                    Type::RawPointer(PointerMut::Const, Box::new(operand.get_type().clone())),
                    Addressability::Value,
                    operand,
                ))
            }
            AddressMut => {
                // The operand must be a mutable location in memory
                if !operand.context().is_mutable() {
                    return Err(CompilerError::new(
                        operand.span(),
                        SemanticError::MutablePointerToImmutable,
                    ));
                }
                Ok((
                    Type::RawPointer(PointerMut::Mut, Box::new(operand.get_type().clone())),
                    Addressability::Value,
                    operand,
                ))
            }
            DerefRawPointer => {
                // Operand must be a *const or a *mut
                match operand.get_type() {
                    Type::RawPointer(mutability, target_ty) => {
                        let addressability = match mutability {
                            PointerMut::Mut => Addressability::AddressableMutable,
                            PointerMut::Const => Addressability::Addressable,
                        };
                        Ok((*target_ty.clone(), addressability, operand))
                    }
                    ty => Err(CompilerError::new(
                        operand.span(),
                        SemanticError::ExpectedRawPointer(op, ty.clone()),
                    )),
                }
            }
        }
    }

    /// Check that the two operands have the same type as the the given binary
    /// operator requires, then, if they do, return the type that the given
    /// binary operator resolves to.
    fn binary_op(
        &mut self,
        op: BinaryOperator,
        l: &SemanticNode,
        r: &SemanticNode,
    ) -> SemanticResult<(Type, SemanticNode, SemanticNode)> {
        use BinaryOperator::*;

        let l = self.analyze_expression(l)?;
        let r = self.analyze_expression(r)?;

        match op {
            RawPointerOffset => {
                // The type of the lhs must be a raw pointer (const or mut)
                if l.get_type().is_raw_pointer() {
                    // The type of the rhs must be an integral type
                    if r.get_type().is_integral() {
                        Ok((l.get_type().clone(), l, r))
                    } else {
                        // The RHS must be an integer
                        Err(CompilerError::new(
                            l.span(),
                            SemanticError::OffsetOperatorRequiresInteger(r.get_type().clone()),
                        ))
                    }
                } else {
                    // The LHS must be a raw pointer
                    Err(CompilerError::new(
                        l.span(),
                        SemanticError::OffsetOperatorRequiresPointer(l.get_type().clone()),
                    ))
                }
            }
            Add | Sub | Mul | Div => {
                if l.get_type().is_number()
                    && r.get_type().is_number()
                    && l.get_type() == r.get_type()
                {
                    Ok((l.get_type().clone(), l, r))
                } else {
                    let expected = if l.get_type().is_integral() {
                        l.get_type().clone()
                    } else {
                        Type::I64
                    };
                    Err(CompilerError::new(
                        l.span(),
                        SemanticError::OpExpected(
                            op,
                            expected,
                            l.get_type().clone(),
                            r.get_type().clone(),
                        ),
                    ))
                }
            }
            BAnd | BOr => {
                if l.get_type() == Type::Bool && r.get_type() == Type::Bool {
                    Ok((Type::Bool, l, r))
                } else {
                    Err(CompilerError::new(
                        l.span(),
                        SemanticError::OpExpected(
                            op,
                            Type::Bool,
                            l.get_type().clone(),
                            r.get_type().clone(),
                        ),
                    ))
                }
            }
            Eq | NEq | Ls | LsEq | Gr | GrEq => {
                if l.get_type().can_be_compared(r.get_type()) {
                    Ok((Type::Bool, l, r))
                } else {
                    Err(CompilerError::new(
                        l.span(),
                        SemanticError::OpExpected(
                            op,
                            l.get_type().clone(),
                            l.get_type().clone(),
                            r.get_type().clone(),
                        ),
                    ))
                }
            }
        }
    }

    fn get_current_path(&self) -> Result<Path, SemanticError> {
        self.symbols.to_path().ok_or(SemanticError::PathNotValid)
    }

    fn extract_routine_type_info<'b>(
        &self,
        symbol: &'b Symbol,
        call: &RoutineCall,
        routine_path: &Path,
    ) -> Result<(&'b Vec<Type>, HasVarArgs, Type), SemanticError> {
        let (expected_param_tys, has_varargs, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, rty),
                ..
            } if *call == RoutineCall::Function => (pty, false, *rty.clone()),
            Symbol {
                ty: Type::ExternDecl(pty, has_varargs, rty),
                ..
            } if *call == RoutineCall::Extern => (pty, *has_varargs, *rty.clone()),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == RoutineCall::CoroutineInit => (pty, false, Type::Coroutine(rty.clone())),
            _ => {
                return Err(SemanticError::RoutineCallInvalidTarget(
                    *call,
                    routine_path.clone(),
                    symbol.ty.clone(),
                ));
            }
        };

        Ok((expected_param_tys, has_varargs, ret_ty))
    }

    fn check_for_invalid_routine_parameters<'b>(
        routine_path: &Path,
        given: &'b [SemanticNode],
        expected_types: &'b [Type],
        has_varargs: HasVarArgs,
    ) -> Result<(), SemanticError> {
        let mut mismatches = vec![];
        let mut idx = 0;
        for (user, expected) in given.iter().zip(expected_types.iter()) {
            idx += 1;
            let user_ty = user.get_type();
            if user_ty != expected {
                mismatches.push((idx, user_ty, expected));
            }
        }

        if (!has_varargs && mismatches.is_empty() && given.len() == expected_types.len())
            || (has_varargs && mismatches.is_empty() && given.len() >= expected_types.len())
        {
            Ok(())
        } else {
            let errors: Vec<_> = mismatches
                .iter()
                .map(|(idx, got, expected)| (*idx, (*expected).clone(), (*got).clone()))
                .collect();
            Err(SemanticError::RoutineParamTypeMismatch(
                routine_path.clone(),
                errors,
            ))
        }
    }

    fn validate_main_fn(routine: &RoutineDef<SemanticContext>) -> SemanticResult<()> {
        let RoutineDef {
            def,
            params,
            ret_ty,
            ..
        } = routine;

        // If routine is root::my_main it must be a function type and have type () -> i64
        if def != &RoutineDefType::Function {
            return Err(CompilerError::new(
                routine.span(),
                SemanticError::MainFnInvalidType,
            ));
        }

        if !params.is_empty() {
            return Err(CompilerError::new(
                routine.span(),
                SemanticError::MainFnInvalidParams,
            ));
        }

        if ret_ty != Type::I64 {
            return Err(CompilerError::new(
                routine.span(),
                SemanticError::MainFnInvalidType,
            ));
        }

        Ok(())
    }

    fn new_event<'e>(&self) -> Event<'e, TypeOk<'e>, SemanticError> {
        Event::new("type-resolver", Span::zero(), self.event_stack.clone())
    }

    fn record2<'e, N: Node<SemanticContext>>(
        &self,
        event: Event<'e, TypeOk, SemanticError>,
        r: Result<&N, &CompilerError<SemanticError>>,
        refs: Vec<Span>,
    ) {
        let msg = r.map(|v| {
            let ctx = v.context();
            TypeOk {
                ty: ctx.ty(),
                addressable: ctx.addressability(),
                refs,
            }
        });
        let span = match r {
            Ok(ok) => ok.span(),
            Err(err) => err.span(),
        };
        self.logger.write(event.with_span(span).with_msg(msg))
    }

    fn record<N: Node<SemanticContext>>(&self, n: &N, refs: Vec<Span>) {
        let ctx = n.context();
        self.logger
            .write(Event::<_, SemanticError>::new_without_parent(
                "type-resolver",
                ctx.span(),
                Ok(TypeOk {
                    ty: ctx.ty(),
                    addressable: ctx.addressability(),
                    refs,
                }),
            ));
    }

    fn record_err(&self, e: &CompilerError<SemanticError>) {
        self.logger
            .write(Event::<TypeOk, SemanticError>::new_without_parent(
                "type-resolver",
                e.span(),
                Err(e),
            ));
    }

    /// Check that the given [`Type`] is valid. If it is a custom type, such as
    /// a structure, this will make sure that the path points to a valid item.
    fn valid_type(&self, ty: &Type, span: Span) -> SemanticResult<()> {
        match ty {
            Type::Custom(type_name) => {
                // Find item that the path points to
                let (item, _) = self
                    .symbols
                    .lookup_symbol_by_path(type_name)
                    .map_err(|e| CompilerError::new(span, e))?;

                // Make sure the item is a structure
                match item.ty {
                    Type::StructDef(_) => Ok(()),
                    _ => err!(span, SemanticError::InvalidIdentifierType(item.ty.clone())),
                }
            }
            _ => Ok(()),
        }
    }
}
