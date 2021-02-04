use crate::compiler::arch::registers::RegSize;
use crate::compiler::memory::scope::SymbolOffsetTable;
use crate::compiler::memory::struct_table::ResolvedStructTable;
use crate::expression::Expression;
use crate::syntax::module::*;
use crate::syntax::routinedef::*;
use crate::syntax::statement::*;
use crate::syntax::structdef::*;
use crate::syntax::ty::Type;
use crate::TracingConfig;
use stdext::function_name;

macro_rules! trace {
    ($self:expr, $ts:expr, $sz:expr) => {
        let line = $ts.get_annotations().line() as usize;
        let print_trace = match &$self.tracing {
            &TracingConfig::Only(ln) => (line) == ln,
            &TracingConfig::Before(ln) => (line) <= ln,
            &TracingConfig::After(ln) => (line) >= ln,
            &TracingConfig::Between(start, end) => (line) >= start && (line) <= end,
            &TracingConfig::All => true,
            &TracingConfig::Off => false,
        };
        if print_trace {
            println!(
                "{} [{}]{} -> {:?} {:?}",
                function_name!(),
                $ts.get_annotations().id(),
                $ts.get_name(),
                $sz,
                RegSize::assign($sz.unwrap_or(0) as usize)
            )
        }
    };
}

/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */

pub struct RegisterAssigner {
    tracing: TracingConfig,
}

impl RegisterAssigner {
    pub fn new(tracing: TracingConfig) -> RegisterAssigner {
        RegisterAssigner { tracing }
    }

    fn register_for_type(ty: &Type, struct_table: &ResolvedStructTable) -> Option<RegSize> {
        let sz = struct_table.size_of(ty);
        sz.and_then(|sz| RegSize::assign(sz as usize))
    }

    fn assign_register(a: &mut SymbolOffsetTable, struct_table: &ResolvedStructTable) {
        let reg = Self::register_for_type(a.ty(), struct_table);
        a.set_reg_size(reg);
    }

    pub fn for_module(
        &self,
        m: &mut Module<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, m, struct_table.size_of(m.get_annotations().ty()));
        Self::assign_register(m.get_annotations_mut(), struct_table);

        for child_module in m.get_modules_mut().iter_mut() {
            self.for_module(child_module, struct_table);
        }
        self.for_items(m.get_functions_mut(), struct_table);

        self.for_items(m.get_coroutines_mut(), struct_table);

        self.for_items(m.get_structs_mut(), struct_table);
    }

    fn for_items(
        &self,
        items: &mut Vec<Item<SymbolOffsetTable>>,
        struct_table: &ResolvedStructTable,
    ) {
        for i in items.iter_mut() {
            trace!(self, i, struct_table.size_of(i.get_annotations().ty()));
            Self::assign_register(i.get_annotations_mut(), struct_table);
            match i {
                Item::Struct(sd) => {
                    self.for_structdef(sd, struct_table);
                }
                Item::Routine(rd) => {
                    self.for_routine(rd, struct_table);
                }
            };
        }
    }

    fn for_structdef(
        &self,
        sd: &mut StructDef<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, sd, struct_table.size_of(sd.get_annotations().ty()));
        Self::assign_register(sd.get_annotations_mut(), struct_table);
    }

    fn for_routine(
        &self,
        rd: &mut RoutineDef<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, rd, struct_table.size_of(rd.get_annotations().ty()));
        Self::assign_register(rd.get_annotations_mut(), struct_table);

        // loop through all the params
        let mut param_annotations = vec![];
        for p in rd.get_params() {
            let mut param_annotation = rd.get_annotations().clone();
            param_annotation.ty = p.1.clone();
            Self::assign_register(&mut param_annotation, struct_table);
            param_annotations.push(param_annotation);
        }

        rd.get_param_annotations_mut()
            .append(&mut param_annotations);

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body_mut().iter_mut() {
            self.for_statement(e, struct_table);
        }
    }

    fn for_statement(
        &self,
        statement: &mut Statement<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            statement,
            struct_table.size_of(statement.get_annotations().ty())
        );
        Self::assign_register(statement.get_annotations_mut(), struct_table);

        match statement {
            Statement::Bind(b) => {
                self.for_bind(b, struct_table);
            }
            Statement::Mutate(m) => {
                self.for_mutate(m, struct_table);
            }
            Statement::Return(r) => {
                self.for_return(r, struct_table);
            }
            Statement::YieldReturn(yr) => {
                self.for_yieldreturn(yr, struct_table);
            }
            Statement::Expression(e) => {
                self.for_expression(e, struct_table);
            }
        };
    }

    fn for_bind(&self, bind: &mut Bind<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(
            self,
            bind,
            struct_table.size_of(bind.get_annotations().ty())
        );
        Self::assign_register(bind.get_annotations_mut(), struct_table);
        self.for_expression(bind.get_rhs_mut(), struct_table)
    }

    fn for_mutate(
        &self,
        mutate: &mut Mutate<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            mutate,
            struct_table.size_of(mutate.get_annotations().ty())
        );
        Self::assign_register(mutate.get_annotations_mut(), struct_table);

        self.for_expression(mutate.get_rhs_mut(), struct_table);
    }

    fn for_yieldreturn(
        &self,
        yr: &mut YieldReturn<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, yr, struct_table.size_of(yr.get_annotations().ty()));

        Self::assign_register(yr.get_annotations_mut(), struct_table);
        yr.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, struct_table));
    }

    fn for_return(&self, r: &mut Return<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(self, r, struct_table.size_of(r.get_annotations().ty()));

        Self::assign_register(r.get_annotations_mut(), struct_table);
        r.get_value_mut()
            .as_mut()
            .map(|rv| self.for_expression(rv, struct_table));
    }

    fn for_expression(
        &self,
        exp: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        use Expression::*;

        trace!(self, exp, struct_table.size_of(exp.get_annotations().ty()));

        Self::assign_register(exp.get_annotations_mut(), struct_table);
        match exp {
            ExpressionBlock(..) => self.for_expression_block(exp, struct_table),
            Expression::Integer64(_m, _i) => {}
            Expression::Boolean(_m, _b) => {}
            Expression::StringLiteral(_m, _s) => {}
            Expression::CustomType(_m, _name) => {}
            Expression::Identifier(_m, _id) => {}
            Path(_m, _path) => {}
            Expression::IdentifierDeclare(_m, _id, _p) => {}
            MemberAccess(..) => self.for_member_access(exp, struct_table),
            UnaryOp(..) => self.for_unary_op(exp, struct_table),
            BinaryOp(..) => self.for_binary_op(exp, struct_table),
            If { .. } => self.for_if(exp, struct_table),
            Yield(..) => self.for_yield(exp, struct_table),
            RoutineCall(..) => self.for_routine_call(exp, struct_table),
            StructExpression(..) => self.for_struct_expression(exp, struct_table),
        }
    }

    fn for_expression_block(
        &self,
        block: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            block,
            struct_table.size_of(block.get_annotations().ty())
        );

        Self::assign_register(block.get_annotations_mut(), struct_table);
        if let Expression::ExpressionBlock(_m, ref mut body, ref mut final_exp) = block {
            for e in body.iter_mut() {
                self.for_statement(e, struct_table);
            }

            final_exp
                .as_mut()
                .map(|fe| self.for_expression(fe, struct_table));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access(
        &self,
        access: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            access,
            struct_table.size_of(access.get_annotations().ty())
        );

        Self::assign_register(access.get_annotations_mut(), struct_table);
        if let Expression::MemberAccess(_m, src, _member) = access {
            self.for_expression(src, struct_table);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op(
        &self,
        un_op: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            un_op,
            struct_table.size_of(un_op.get_annotations().ty())
        );

        Self::assign_register(un_op.get_annotations_mut(), struct_table);
        if let Expression::UnaryOp(_m, _op, operand) = un_op {
            self.for_expression(operand, struct_table);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op(
        &self,
        bin_op: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            bin_op,
            struct_table.size_of(bin_op.get_annotations().ty())
        );

        Self::assign_register(bin_op.get_annotations_mut(), struct_table);
        if let Expression::BinaryOp(_m, _op, l, r) = bin_op {
            self.for_expression(l, struct_table);
            self.for_expression(r, struct_table);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if(
        &self,
        if_exp: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            if_exp,
            struct_table.size_of(if_exp.get_annotations().ty())
        );

        Self::assign_register(if_exp.get_annotations_mut(), struct_table);
        if let Expression::If {
            annotation: _m,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            self.for_expression(cond, struct_table);
            self.for_expression(if_arm, struct_table);
            else_arm
                .as_mut()
                .map(|ea| self.for_expression(ea, struct_table));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call(
        &self,
        rc: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, rc, struct_table.size_of(rc.get_annotations().ty()));

        Self::assign_register(rc.get_annotations_mut(), struct_table);
        if let Expression::RoutineCall(_m, _call, _name, params) = rc {
            for p in params {
                self.for_expression(p, struct_table);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield(
        &self,
        yield_exp: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(
            self,
            yield_exp,
            struct_table.size_of(yield_exp.get_annotations().ty())
        );

        Self::assign_register(yield_exp.get_annotations_mut(), struct_table);
        if let Expression::Yield(_m, e) = yield_exp {
            self.for_expression(e, struct_table);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression(
        &self,
        se: &mut Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(self, se, struct_table.size_of(se.get_annotations().ty()));

        Self::assign_register(se.get_annotations_mut(), struct_table);
        if let Expression::StructExpression(_annotations, _struct_name, fields) = se {
            for (_, fe) in fields {
                self.for_expression(fe, struct_table);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}
