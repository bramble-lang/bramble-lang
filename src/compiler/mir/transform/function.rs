//! Transforms the AST of a single function into its MIR control flow
//! graph representation. All types, functions, and static variables that
//! are referred to into the body of the input function must have already
//! had their delcarations added to the [`MirProject`] or the transformation
//!  will fail.

use log::debug;

use crate::{
    compiler::{
        ast::{self, *},
        semantics::semanticnode::SemanticContext,
        source::Offset,
        Span,
    },
    StringId,
};

use super::super::{builder::MirProcedureBuilder, ir::*, project::MirProject, typetable::*};

/// Transform a single function to the MIR form
pub(super) struct FuncTransformer<'a> {
    project: &'a mut MirProject,
    mir: MirProcedureBuilder,
}

impl<'a> FuncTransformer<'a> {
    pub fn new(path: &Path, project: &'a mut MirProject) -> FuncTransformer<'a> {
        let unit = project
            .find_type(&Type::Unit)
            .expect("Cannot find Unit type");
        FuncTransformer {
            project,
            mir: MirProcedureBuilder::new(path, unit),
        }
    }

    pub fn transform(mut self, func: &RoutineDef<SemanticContext>) -> Procedure {
        self.mir.set_span(func.context.span());

        // Set the function return type
        let ret_ty = self.find_type(func.get_return_type());
        self.mir.set_ret_ty(ret_ty);

        // Add the parameters of the function to the set of variables
        func.params.iter().for_each(|p| {
            let ty = self.find_type(p.context().ty());
            self.mir.arg(p.name, ty, p.context().span());
        });

        // Create a new MIR Procedure
        // Create a BasicBlock for the function
        let bb = self.mir.new_bb();
        self.mir.set_bb(bb);

        // Iterate over every statement and add it to the basic block
        func.body.iter().for_each(|stm| self.statement(stm));

        // Add the return from function as the terminator for the final basic block of the function
        self.mir.term_return(span_end(func.context.span()));
        self.mir.complete()
    }

    fn statement(&mut self, stm: &ast::Statement<SemanticContext>) {
        debug!("Transform statement");
        match stm {
            ast::Statement::Bind(bind) => self.bind(bind),
            ast::Statement::Expression(expr) => {
                self.expression(expr);
            }
            ast::Statement::Mutate(mutate) => self.mutate(mutate),
            ast::Statement::YieldReturn(_) => todo!(),
            ast::Statement::Return(ret) => self.ret(ret),
        }
    }

    fn bind(&mut self, bind: &Bind<SemanticContext>) {
        debug!("Binding statement");
        let var = bind.get_id();
        let mutable = bind.is_mutable();
        let ty = self.find_type(bind.context().ty());
        let vid = self.mir.var(var, mutable, ty, bind.context().span());

        let expr = self.expression(bind.get_rhs());

        self.mir
            .store(LValue::Var(vid), RValue::Use(expr), bind.context().span())
    }

    fn mutate(&mut self, mutate: &Mutate<SemanticContext>) {
        debug!("Mutate statement");
        let lhs = self
            .expression(mutate.get_lhs())
            .into_lvalue()
            .expect("LHS of a mutate must be an addressable expression");
        let rhs = self.expression(mutate.get_rhs());
        self.mir
            .store(lhs, RValue::Use(rhs), mutate.context().span());
    }

    fn ret(&mut self, ret: &Return<SemanticContext>) {
        match ret.get_value() {
            Some(val) => {
                let v = self.expression(val);
                self.mir
                    .store(LValue::ReturnPointer, RValue::Use(v), val.context().span());
            }
            None => (),
        };
        self.mir.term_return(ret.context().span());
    }

    /// This can return either an Operand or an RValue, if this is evaluating a constant or an identifier
    /// then this returns an operand.  If this is evaluating an operation then it returns an RValue.
    fn expression(&mut self, expr: &Expression<SemanticContext>) -> Operand {
        match expr {
            // Literals
            Expression::I8(_, i) => self.mir.const_i8(*i),
            Expression::I16(_, i) => self.mir.const_i16(*i),
            Expression::I32(_, i) => self.mir.const_i32(*i),
            Expression::I64(_, i) => self.mir.const_i64(*i),
            Expression::U8(_, u) => self.mir.const_u8(*u),
            Expression::U16(_, u) => self.mir.const_u16(*u),
            Expression::U32(_, u) => self.mir.const_u32(*u),
            Expression::U64(_, u) => self.mir.const_u64(*u),
            Expression::F64(_, f) => self.mir.const_f64(*f),
            Expression::Null(_) => self.mir.const_null(),
            Expression::Boolean(_, b) => self.mir.const_bool(*b),
            Expression::StringLiteral(_, sid) => {
                // If it exists Get static definition of the string literal
                let def_id = self.project.add_string_literal(*sid).unwrap();
                // Otherwise, add the string literal to hte static definition table
                self.mir.const_stringliteral(def_id)
            }

            // Operations
            Expression::BinaryOp(ctx, op, left, right) => {
                let rv = self.binary_op(ctx, *op, left, right);
                let ty = self.find_type(ctx.ty());
                self.mir.temp_store(rv, ty, ctx.span())
            }
            Expression::UnaryOp(ctx, op, right) => self.unary_op(ctx, *op, right),
            Expression::TypeCast(ctx, expr, target) => self.cast(ctx, expr, target),
            Expression::SizeOf(ctx, ty) => self.size_of(ctx, ty.as_ref()),
            Expression::MemberAccess(_, base, field) => self.member_access(base, *field),
            Expression::ArrayExpression(ctx, els, sz) => {
                self.array_expr(ctx.ty(), els, *sz, ctx.span())
            }
            Expression::ArrayAt {
                context,
                array,
                index,
            } => self.array_at(array, index),
            Expression::Identifier(_, id) => {
                // Look up Var ID using the Identifier String ID
                let vid = self.mir.find_var(*id).unwrap();

                // Return a LValue::Var(VarId) as the result of this expression
                Operand::LValue(LValue::Var(vid))
            }
            Expression::CustomType(_, _) => todo!(),
            Expression::Path(_, _) => todo!(),
            Expression::IdentifierDeclare(_, _, _) => todo!(),
            Expression::RoutineCall(ctx, call, target, args) => {
                self.fn_call(ctx, *call, target, args)
            }
            Expression::StructExpression(ctx, ty, fields) => self.stuct_expr(ctx, ty, fields),
            Expression::If {
                context,
                cond,
                if_arm,
                else_arm,
            } => self.if_expr(cond, if_arm, else_arm),
            Expression::While {
                context,
                cond,
                body,
            } => self.while_expr(cond, body, context.span()),
            Expression::ExpressionBlock(_, block, expr) => {
                self.mir.start_scope();
                for stm in block {
                    self.statement(stm);
                }
                let result = if let Some(expr) = expr {
                    self.expression(expr)
                } else {
                    Operand::Constant(Constant::Unit)
                };
                self.mir.close_scope();
                result
            }
            Expression::Yield(_, _) => todo!(),
        }
    }

    fn size_of(&mut self, ctx: &SemanticContext, ty: &Type) -> Operand {
        let ty = self.project.find_type(ty).expect("Could not find type");
        self.mir.size_of(ty)
    }

    fn stuct_expr(
        &mut self,
        ctx: &SemanticContext,
        def: &Path,
        expr: &[(StringId, Expression<SemanticContext>)],
    ) -> Operand {
        // Get the type for this structure
        let ty = self.project.find_type(&Type::Custom(def.clone())).unwrap();

        // Create a temporary location on the stack to store the struct expression
        let temp = self.mir.temp(ty, ctx.span());

        // Get the structure definition
        let def = self
            .project
            .get_type(ty)
            .get_struct_def()
            .expect("Trying to access a field on a non-structure type")
            .clone();

        // Evaluate each expression and store into the associated temp field
        for (field, val) in expr {
            let result = self.expression(val);
            let field_loc = self.mir.member_access(LValue::Temp(temp), &def, *field);
            self.mir
                .store(field_loc, RValue::Use(result), val.context().span());
        }
        Operand::LValue(LValue::Temp(temp))
    }

    fn cast(
        &mut self,
        ctx: &SemanticContext,
        expr: &Expression<SemanticContext>,
        target: &Type,
    ) -> Operand {
        let expr_ty = self
            .project
            .find_type(expr.context().ty())
            .expect("Cannot find the given type");
        let expr = self.expression(expr);
        let target = self
            .project
            .find_type(target)
            .expect("Cannot find the given type");
        let result = self.mir.cast(expr, expr_ty, target);
        self.mir.temp_store(result, target, ctx.span())
    }

    /// [Terminates](Terminator) the current [`BasicBlock`] with a function call and starts a new basic block
    fn fn_call(
        &mut self,
        ctx: &SemanticContext,
        call: RoutineCall,
        target: &Path,
        args: &[Expression<SemanticContext>],
    ) -> Operand {
        let fn_id = if call == RoutineCall::Extern {
            let extern_path: Path = vec![Element::Id(target.item().unwrap())].into();
            self.project.find_def(&extern_path)
        } else {
            self.project.find_def(target)
        }
        .unwrap_or_else(|| panic!("Target function not found: {}", target));

        // Compute the value of each argument
        let args: Vec<_> = args.iter().map(|a| self.expression(a)).collect();

        // Create a basic block that the function will return into
        let reentry_bb = self.mir.new_bb();

        // Look up the declaration of the target function
        let func = self
            .project
            .get_def_fn(fn_id)
            .expect("No function bound to given DefId");

        // Create a temp location for the result value of the function call
        let result = self.mir.temp(func.ret_ty(), ctx.span());

        // Create the call Terminator
        self.mir.term_call(
            Operand::LValue(LValue::Static(fn_id)),
            &args,
            (LValue::Temp(result), reentry_bb),
            ctx.span(),
        );

        // Change the current basic block to continue adding statements after the function call returns
        self.mir.set_bb(reentry_bb);

        // return an operand that has the result of the function call (if any)
        Operand::LValue(LValue::Temp(result))
    }

    /// Creates a member access operand which can be used in a statement or terminator
    fn member_access(&mut self, base: &Expression<SemanticContext>, field: StringId) -> Operand {
        // Get the Index of the Field and convert to a `FieldId`
        let ty = base.context().ty();
        let mir_ty = self
            .project
            .find_type(ty)
            .expect("Could not find given type in the type table");

        let base_mir = self
            .expression(base)
            .into_lvalue()
            .expect("The LHS of a '.' operator must be an addressable expression");

        // Extract the Structure Definition from the type
        let mir_ty = self.project.get_type(mir_ty);
        let def = mir_ty
            .get_struct_def()
            .expect("Trying to access a field on a non-structure type");

        let access = self.mir.member_access(base_mir, def, field);
        Operand::LValue(access)
    }

    /// Transform an Array At operation to its MIR form and return the Location Expression as
    /// an [`Operand::LValue`]. This operand can then be used in other MIR operations.
    fn array_at(
        &mut self,
        array: &Expression<SemanticContext>,
        index: &Expression<SemanticContext>,
    ) -> Operand {
        // resolve the array expression to find out where in memory to begin the indexing operation
        if let Operand::LValue(array_mir) = self.expression(array) {
            // Compute the index expression to find out the position to read from
            let index_mir = self.expression(index);
            // Return the array at memory location
            Operand::LValue(self.mir.array_at(array_mir, index_mir))
        } else {
            // The type resolver stage will make sure that the left operand of the index operation
            // must be a location (LValue) expression. Therefore, if this branch is ever reached then
            // there is a critical bug in the Type Resolver logic and we should panic and not allow
            // the compiler to go any further.
            panic!("Array expressions must resolve to Location Expressions")
        }
    }

    fn array_expr(
        &mut self,
        ty: &Type,
        elements: &[Expression<SemanticContext>],
        sz: usize,
        span: Span,
    ) -> Operand {
        // Create a temporary place on the stack for the array expression
        let ty = self.find_type(ty);
        let temp = LValue::Temp(self.mir.temp(ty, span));

        // Compute the value of each element expression and store in the stack variable
        for idx in 0..sz {
            let idx_mir = self.mir.const_i64(idx as i64);
            let array_el_loc = self.mir.array_at(temp.clone(), idx_mir);

            let el = self.expression(&elements[idx]);
            let el_span = elements[idx].context().span();

            self.mir.store(array_el_loc, RValue::Use(el), el_span);
        }

        // Return the temporary variable as the value of the array expression
        Operand::LValue(temp)
    }

    fn while_expr(
        &mut self,
        cond: &Expression<SemanticContext>,
        body: &Expression<SemanticContext>,
        span: Span,
    ) -> Operand {
        // Create a block that will evaluate the conditional
        // the conditional has to be reevaluated on every iteration of the loop, therefore it has its own BB
        let cond_bb = self.mir.new_bb();
        // Create a block for the while body
        let body_bb = self.mir.new_bb();
        // Create a block for exiting the while loop
        let exit_bb = self.mir.new_bb();

        // Have the BB prior to the while loop enter into the Conditional BB
        self.mir.term_goto(cond_bb, span_begin(span));

        // Construct the condition evaluation BB
        self.mir.set_bb(cond_bb);
        let cond_val = self.expression(cond);
        self.mir
            .term_cond_goto(cond_val, body_bb, exit_bb, cond.context().span());

        // Construct the while loop body BB
        self.mir.set_bb(body_bb);
        self.expression(body); // While loops always resolve to Unit value, so ignore the result of this expression
        self.mir.term_goto(cond_bb, span_end(body.context().span()));

        // Set the exit_bb as the current BB to continue constructing the MIR after the while loop
        self.mir.set_bb(exit_bb);

        Operand::Constant(Constant::Unit) // All while loops resolve to Unit value
    }

    fn if_expr(
        &mut self,
        cond: &Expression<SemanticContext>,
        then_block: &Expression<SemanticContext>,
        else_block: &Option<Box<Expression<SemanticContext>>>,
    ) -> Operand {
        let then_bb = self.mir.new_bb();
        let else_bb = else_block.as_ref().map(|block| (block, self.mir.new_bb()));
        let merge_bb = self.mir.new_bb();

        // Setup the conditional
        let cond_val = self.expression(cond);

        // If there is an else block then jump to the else block on false
        // otherwise jump to the merge block
        if let Some(else_bb) = &else_bb {
            self.mir
                .term_cond_goto(cond_val, then_bb, else_bb.1, cond.context().span());
        } else {
            self.mir
                .term_cond_goto(cond_val, then_bb, merge_bb, cond.context().span());
        }

        // Only create a temp location if this If Expression can resolve to a
        // value
        let result = if else_block.is_some() && then_block.get_type() != Type::Unit {
            let then_ty = self.find_type(then_block.get_type());
            Some(self.mir.temp(then_ty, then_block.context().span()))
        } else {
            None
        };

        self.mir.set_bb(then_bb);
        let val = self.expression(then_block);
        if let Some(t) = result {
            self.mir.store(
                LValue::Temp(t),
                RValue::Use(val),
                then_block.context().span(),
            )
        }

        self.mir
            .term_goto(merge_bb, span_end(then_block.context().span()));

        // If there is an else block, then construct it
        if let Some((else_block, else_bb)) = else_bb {
            self.mir.set_bb(else_bb);
            let val = self.expression(else_block);

            if let Some(t) = result {
                self.mir.store(
                    LValue::Temp(t),
                    RValue::Use(val),
                    else_block.context().span(),
                )
            }

            self.mir
                .term_goto(merge_bb, span_end(else_block.context().span()));
        }

        self.mir.set_bb(merge_bb);
        match result {
            Some(r) => Operand::LValue(LValue::Temp(r)),
            None => Operand::Constant(Constant::Unit),
        }
    }

    fn unary_op(
        &mut self,
        ctx: &SemanticContext,
        op: UnaryOperator,
        right: &Expression<SemanticContext>,
    ) -> Operand {
        let is_float = right.context().ty().is_float();
        let right = self.expression(right);
        match op {
            UnaryOperator::Negate => {
                let rv = if is_float {
                    self.mir.fnegate(right)
                } else {
                    self.mir.negate(right)
                };
                let ty = self.find_type(ctx.ty());
                self.mir.temp_store(rv, ty, ctx.span())
            }
            UnaryOperator::Not => {
                let rv = self.mir.not(right);
                let ty = self.find_type(ctx.ty());
                self.mir.temp_store(rv, ty, ctx.span())
            }
            UnaryOperator::AddressConst | UnaryOperator::AddressMut => {
                if let Operand::LValue(lv) = right {
                    let rv = RValue::AddressOf(lv);
                    let ty = self.find_type(ctx.ty());
                    self.mir.temp_store(rv, ty, ctx.span())
                } else {
                    // Type checking should ensure that this branch never happens
                    // so if it does, then there is a bug in the compiler.
                    panic!("AddressOf can only be applied to LValues")
                }
            }
            UnaryOperator::DerefRawPointer => {
                if let Operand::LValue(lv) = right {
                    Operand::LValue(self.mir.deref_rawpointer(lv))
                } else {
                    panic!("Deref can only be applied to LValues")
                }
            }
        }
    }

    fn binary_op(
        &mut self,
        ctx: &SemanticContext,
        op: BinaryOperator,
        left: &Expression<SemanticContext>,
        right: &Expression<SemanticContext>,
    ) -> RValue {
        let is_float = left.context().ty().is_float();

        match op {
            BinaryOperator::Add => {
                let left = self.expression(left);
                let right = self.expression(right);
                if is_float {
                    self.mir.fadd(left, right)
                } else {
                    self.mir.add(left, right)
                }
            }
            BinaryOperator::Sub => {
                let left = self.expression(left);
                let right = self.expression(right);

                if is_float {
                    self.mir.fsub(left, right)
                } else {
                    self.mir.sub(left, right)
                }
            }
            BinaryOperator::Mul => {
                let left = self.expression(left);
                let right = self.expression(right);
                if is_float {
                    self.mir.fmul(left, right)
                } else {
                    self.mir.mul(left, right)
                }
            }
            BinaryOperator::Div => {
                let left = self.expression(left);
                let right = self.expression(right);
                if is_float {
                    self.mir.fdiv(left, right)
                } else if ctx.ty().is_unsigned_int() {
                    self.mir.ui_div(left, right)
                } else {
                    self.mir.div(left, right)
                }
            }
            BinaryOperator::BAnd => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.bitwise_and(left, right)
            }
            BinaryOperator::BOr => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.bitwise_or(left, right)
            }
            BinaryOperator::Eq => {
                let left = self.expression(left);
                let right = self.expression(right);
                if is_float {
                    self.mir.f_eq(left, right)
                } else {
                    self.mir.eq(left, right)
                }
            }
            BinaryOperator::NEq => {
                let left = self.expression(left);
                let right = self.expression(right);
                if is_float {
                    self.mir.f_neq(left, right)
                } else {
                    self.mir.neq(left, right)
                }
            }
            BinaryOperator::Ls => {
                let l = self.expression(left);
                let r = self.expression(right);
                if left.context().ty().is_unsigned_int() {
                    self.mir.ui_lt(l, r)
                } else if is_float {
                    self.mir.f_lt(l, r)
                } else {
                    self.mir.lt(l, r)
                }
            }
            BinaryOperator::LsEq => {
                let l = self.expression(left);
                let r = self.expression(right);
                if left.context().ty().is_unsigned_int() {
                    self.mir.ui_le(l, r)
                } else if is_float {
                    self.mir.f_le(l, r)
                } else {
                    self.mir.le(l, r)
                }
            }
            BinaryOperator::Gr => {
                let l = self.expression(left);
                let r = self.expression(right);
                if left.context().ty().is_unsigned_int() {
                    self.mir.ui_gt(l, r)
                } else if is_float {
                    self.mir.f_gt(l, r)
                } else {
                    self.mir.gt(l, r)
                }
            }
            BinaryOperator::GrEq => {
                let l = self.expression(left);
                let r = self.expression(right);
                if left.context().ty().is_unsigned_int() {
                    self.mir.ui_ge(l, r)
                } else if is_float {
                    self.mir.f_ge(l, r)
                } else {
                    self.mir.ge(l, r)
                }
            }
            BinaryOperator::RawPointerOffset => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.offset(left, right)
            }
        }
    }

    fn find_type(&self, ty: &Type) -> TypeId {
        self.project
            .find_type(ty)
            .unwrap_or_else(|| panic!("Cannot find type: {}", ty))
    }
}

/// Returns a new span that represents the 0-width point immediately
/// preceeding the given span.
///
/// This is used to represent MIR instructions that are inserted
/// before the an expression block has started and which don't
/// correspond to any code written by the User.  For example, the
/// GoTo that transitions from the code before a while loop into the
/// BasicBlock of the while loop's condition expression.
fn span_begin(span: Span) -> Span {
    let low = span.low().as_u32();
    if low == 0 {
        Span::zero()
    } else {
        Span::new(Offset::new(low), Offset::new(low))
    }
}

/// Returns a new span that represents the 0-width point immediately
/// following the givne span.
///
/// This is used to represent MIR instructions that are inserted
/// after the an expression block has ended and which don't
/// correspond to any code written by the User.  For example, the
/// GoTo inserted at the end of a While loop to return to the top
/// of the loop.
fn span_end(span: Span) -> Span {
    let high = span.high().as_u32();
    if high == 0 {
        Span::zero()
    } else {
        Span::new(Offset::new(high), Offset::new(high))
    }
}
