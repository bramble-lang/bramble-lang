//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

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

use super::{builder::MirProcedureBuilder, ir::*, project::MirProject, typetable::*};

/// Transform a [`Module`] into its MIR representation and add all items to the
/// given [`MirProject`].
pub fn module_transform(
    module: &Module<SemanticContext>,
    project: &mut MirProject,
) -> Result<(), ()> {
    // Add all the types in this module
    module.get_structs().iter().for_each(|sd| {
        if let Item::Struct(sd) = sd {
            project.add_struct_def(sd).unwrap();
        }
    });

    let funcs = module.get_functions();

    for f in funcs {
        match f {
            crate::compiler::ast::Item::Routine(r) => {
                let ft = FuncTransformer::new(r.context().canonical_path(), project);
                let p = ft.transform(r);
                project.add_func(p)?;
            }
            crate::compiler::ast::Item::Struct(_) => todo!(),
            crate::compiler::ast::Item::Extern(_) => todo!(),
        }
    }

    Ok(())
}

/// Transform a single function to the MIR form
struct FuncTransformer<'a> {
    project: &'a MirProject,
    mir: MirProcedureBuilder,
}

impl<'a> FuncTransformer<'a> {
    pub fn new(path: &Path, project: &'a MirProject) -> FuncTransformer<'a> {
        FuncTransformer {
            project,
            mir: MirProcedureBuilder::new(path),
        }
    }

    pub fn transform(mut self, func: &RoutineDef<SemanticContext>) -> Procedure {
        self.mir.set_span(func.context.span());

        // Add the parameters of the function to the set of variables
        func.params.iter().for_each(|p| {
            self.mir.arg(p.name, p.context().ty(), p.context().span());
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
            ast::Statement::Mutate(_) => todo!(),
            ast::Statement::YieldReturn(_) => todo!(),
            ast::Statement::Return(ret) => self.ret(ret),
        }
    }

    fn bind(&mut self, bind: &Bind<SemanticContext>) {
        debug!("Binding statement");
        let var = bind.get_id();
        let mutable = bind.is_mutable();
        let ty = bind.get_type();
        let vid = self.mir.var(var, mutable, ty, bind.context().span());

        let expr = self.expression(bind.get_rhs());

        self.mir
            .store(LValue::Var(vid), RValue::Use(expr), bind.context().span())
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
            Expression::StringLiteral(_, sid) => self.mir.const_stringliteral(*sid),

            // Operations
            Expression::BinaryOp(ctx, op, left, right) => {
                let rv = self.binary_op(*op, left, right);
                self.mir.temp_store(rv, ctx.ty(), ctx.span())
            }
            Expression::UnaryOp(ctx, op, right) => {
                let rv = self.unary_op(*op, right);
                self.mir.temp_store(rv, ctx.ty(), ctx.span())
            }
            Expression::TypeCast(_, _, _) => todo!(),
            Expression::SizeOf(_, _) => todo!(),
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
            Expression::RoutineCall(ctx, call, path, args) => self.fn_call(ctx, *call, path, args),
            Expression::StructExpression(_, _, _) => todo!(),
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
                for stm in block {
                    self.statement(stm);
                }
                if let Some(expr) = expr {
                    self.expression(expr)
                } else {
                    Operand::Constant(Constant::Unit)
                }
            }
            Expression::Yield(_, _) => todo!(),
        }
    }

    /// [Terminates](Terminator) the current [`BasicBlock`] with a function call and starts a new basic block
    fn fn_call(
        &mut self,
        ctx: &SemanticContext,
        call: RoutineCall,
        path: &Path,
        args: &[Expression<SemanticContext>],
    ) -> Operand {
        // Look up the MIR Function ID for the target
        // Get the TypeID for the return type of the called function
        // Compute the value of each argument
        // Create a basic block that the function will return into
        // Create the call Terminator
        // Change the current basic block to continue adding statements after the function call returns
        todo!()
    }

    /// Creates a member access operand which can be used in a statement or terminator
    fn member_access(&mut self, base: &Expression<SemanticContext>, field: StringId) -> Operand {
        // Get the Index of the Field and convert to a `FieldId`
        let ty = base.context().ty();
        let mir_ty = self
            .project
            .find_type(ty)
            .expect("Could not find given type in the type table");

        // Extract the Structure Definition from the type
        let mir_ty = self.project.get_type(mir_ty);
        let def = if let MirTypeDef::Structure { def, .. } = mir_ty {
            def
        } else {
            // Type checking should guarantee that if this method is called then the type of the
            // AST node is a structure. If it is not, then a critical bug has been encountered.
            panic!("Trying to access a field on a non-structure type")
        };

        if let Operand::LValue(base_mir) = self.expression(base) {
            let access = self.mir.member_access(base_mir, def, field);
            Operand::LValue(access)
        } else {
            // Base expression must be a location expression
            panic!("Base expression must be a location expression")
        }
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
            Some(
                self.mir
                    .temp(then_block.get_type(), then_block.context().span()),
            )
        } else {
            None
        };

        self.mir.set_bb(then_bb);
        let val = self.expression(then_block);
        result.map(|t| {
            self.mir.store(
                LValue::Temp(t),
                RValue::Use(val),
                then_block.context().span(),
            )
        });
        self.mir
            .term_goto(merge_bb, span_end(then_block.context().span()));

        // If there is an else block, then construct it
        if let Some((else_block, else_bb)) = else_bb {
            self.mir.set_bb(else_bb);
            let val = self.expression(else_block);
            result.map(|t| {
                self.mir.store(
                    LValue::Temp(t),
                    RValue::Use(val),
                    else_block.context().span(),
                )
            });
            self.mir
                .term_goto(merge_bb, span_end(else_block.context().span()));
        }

        self.mir.set_bb(merge_bb);
        match result {
            Some(r) => Operand::LValue(LValue::Temp(r)),
            None => Operand::Constant(Constant::Unit),
        }
    }

    fn unary_op(&mut self, op: UnaryOperator, right: &Expression<SemanticContext>) -> RValue {
        match op {
            UnaryOperator::Negate => {
                let right = self.expression(right);
                self.mir.negate(right)
            }
            UnaryOperator::Not => {
                let right = self.expression(right);
                self.mir.not(right)
            }
            UnaryOperator::AddressConst | UnaryOperator::AddressMut => {
                let right = self.expression(right);
                if let Operand::LValue(lv) = right {
                    RValue::AddressOf(lv)
                } else {
                    // Type checking should ensure that this branch never happens
                    // so if it does, then there is a bug in the compiler.
                    panic!("AddressOf can only be applied to LValues")
                }
            }
            UnaryOperator::DerefRawPointer => todo!(),
        }
    }

    fn binary_op(
        &mut self,
        op: BinaryOperator,
        left: &Expression<SemanticContext>,
        right: &Expression<SemanticContext>,
    ) -> RValue {
        match op {
            BinaryOperator::Add => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.add(left, right)
            }
            BinaryOperator::Sub => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.sub(left, right)
            }
            BinaryOperator::Mul => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.mul(left, right)
            }
            BinaryOperator::Div => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.div(left, right)
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
                self.mir.eq(left, right)
            }
            BinaryOperator::NEq => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.neq(left, right)
            }
            BinaryOperator::Ls => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.lt(left, right)
            }
            BinaryOperator::LsEq => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.le(left, right)
            }
            BinaryOperator::Gr => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.gt(left, right)
            }
            BinaryOperator::GrEq => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.ge(left, right)
            }
            BinaryOperator::RawPointerOffset => {
                let left = self.expression(left);
                let right = self.expression(right);
                self.mir.offset(left, right)
            }
        }
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
