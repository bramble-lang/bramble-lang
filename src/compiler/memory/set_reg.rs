/**
 * This traverses the AST and determines what size register to
 * assign to each node in the AST: if it makes sense to assign
 * it to a register.
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegSize {
    R8,
    R16,
    R32,
    R64,
}

impl RegSize {
    pub fn assign(nbytes: usize) -> Option<RegSize> {
        if nbytes == 0 {
            None
        } else if nbytes <= 1 {
            Some(RegSize::R8)
        } else if nbytes <= 2 {
            Some(RegSize::R16)
        } else if nbytes <= 4 {
            Some(RegSize::R32)
        } else if nbytes <= 8 {
            Some(RegSize::R64)
        } else {
            None
        }
    }
}

impl std::fmt::Display for RegSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            RegSize::R8 => f.write_str("R8"),
            RegSize::R16 => f.write_str("R16"),
            RegSize::R32 => f.write_str("R32"),
            RegSize::R64 => f.write_str("R64"),
        }
    }
}

pub mod assign {
    use super::*;
    use crate::compiler::memory::scope::SymbolOffsetTable;
    use crate::compiler::memory::struct_table::ResolvedStructTable;
    use crate::expression::Expression;
    use crate::syntax::module::*;
    use crate::syntax::routinedef::*;
    use crate::syntax::statement::*;
    use crate::syntax::structdef::*;
    use crate::syntax::ty::Type;
    use stdext::function_name;

    macro_rules! trace {
        ($ts:expr, $sz:expr) => {
            println!(
                "{} [{}]{} -> {:?} {:?}",
                function_name!(),
                $ts.get_annotations().id(),
                $ts.get_name(),
                $sz,
                RegSize::assign($sz.unwrap_or(0) as usize)
            )
        };
    }

    fn assign_register(ty: &Type, struct_table: &ResolvedStructTable) -> Option<RegSize> {
        let sz = struct_table.size_of(ty);
        sz.and_then(|sz| RegSize::assign(sz as usize))
    }

    pub fn for_module(m: &Module<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(m, struct_table.size_of(m.get_annotations().ty()));

        for child_module in m.get_modules().iter() {
            for_module(child_module, struct_table);
        }
        for_items(m.get_functions(), struct_table);

        for_items(m.get_coroutines(), struct_table);

        for_items(m.get_structs(), struct_table);
    }

    fn for_items(items: &Vec<Item<SymbolOffsetTable>>, struct_table: &ResolvedStructTable) {
        for i in items {
            trace!(i, struct_table.size_of(i.get_annotations().ty()));
            match i {
                Item::Struct(sd) => {
                    for_structdef(sd, struct_table);
                }
                Item::Routine(rd) => {
                    for_routine(rd, struct_table);
                }
            };
        }
    }

    fn for_structdef(sd: &StructDef<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(sd, struct_table.size_of(sd.get_annotations().ty()));

        for (fname, fty) in sd.get_fields() {
            let reg = assign_register(fty, struct_table);
            println!(
                "{}:({},{:?}) -> {:?}",
                fname,
                fty,
                struct_table.size_of(fty),
                reg
            );
        }
    }

    fn for_routine(rd: &RoutineDef<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(rd, struct_table.size_of(rd.get_annotations().ty()));

        // loop through all the params
        for p in rd.get_params() {
            println!(
                "{} {} -> {:?}",
                function_name!(),
                p.0,
                struct_table.size_of(&p.1)
            );
        }

        // loop through every statement and analyze the child nodes of the routine definition
        for e in rd.get_body().iter() {
            for_statement(e, struct_table);
        }
    }

    fn for_statement(statement: &Statement<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(
            statement,
            struct_table.size_of(statement.get_annotations().ty())
        );
        match statement {
            Statement::Bind(b) => {
                for_bind(b, struct_table);
            }
            Statement::Mutate(m) => {
                for_mutate(m, struct_table);
            }
            Statement::Return(r) => {
                for_return(r, struct_table);
            }
            Statement::YieldReturn(yr) => {
                for_yieldreturn(yr, struct_table);
            }
            Statement::Expression(e) => {
                for_expression(e, struct_table);
            }
        };
    }

    fn for_bind(bind: &Bind<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(bind, struct_table.size_of(bind.get_annotations().ty()));
        for_expression(bind.get_rhs(), struct_table)
    }

    fn for_mutate(mutate: &Mutate<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(mutate, struct_table.size_of(mutate.get_annotations().ty()));

        for_expression(mutate.get_rhs(), struct_table);
    }

    fn for_yieldreturn(yr: &YieldReturn<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(yr, struct_table.size_of(yr.get_annotations().ty()));

        yr.get_value()
            .as_ref()
            .map(|rv| for_expression(&rv, struct_table));
    }

    fn for_return(r: &Return<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(r, struct_table.size_of(r.get_annotations().ty()));

        r.get_value()
            .as_ref()
            .map(|rv| for_expression(&rv, struct_table));
    }

    fn for_expression(exp: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        use Expression::*;

        trace!(exp, struct_table.size_of(exp.get_annotations().ty()));

        match exp {
            ExpressionBlock(..) => for_expression_block(exp, struct_table),
            Expression::Integer64(_m, _i) => {}
            Expression::Boolean(_m, _b) => {}
            Expression::StringLiteral(_m, _s) => {}
            Expression::CustomType(_m, _name) => {}
            Expression::Identifier(_m, _id) => {}
            Path(_m, _path) => {}
            Expression::IdentifierDeclare(_m, _id, _p) => {}
            MemberAccess(..) => for_member_access(exp, struct_table),
            UnaryOp(..) => for_unary_op(exp, struct_table),
            BinaryOp(..) => for_binary_op(exp, struct_table),
            If { .. } => for_if(exp, struct_table),
            Yield(..) => for_yield(exp, struct_table),
            RoutineCall(..) => for_routine_call(exp, struct_table),
            StructExpression(..) => for_struct_expression(exp, struct_table),
        }
    }

    fn for_expression_block(
        block: &Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(block, struct_table.size_of(block.get_annotations().ty()));

        if let Expression::ExpressionBlock(_m, body, final_exp) = block {
            for e in body.iter() {
                for_statement(e, struct_table);
            }

            final_exp
                .as_ref()
                .map(|fe| for_expression(&fe, struct_table));
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn for_member_access(
        access: &Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(access, struct_table.size_of(access.get_annotations().ty()));

        if let Expression::MemberAccess(_m, src, _member) = access {
            for_expression(src, struct_table);
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn for_unary_op(un_op: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(un_op, struct_table.size_of(un_op.get_annotations().ty()));

        if let Expression::UnaryOp(_m, _op, operand) = un_op {
            for_expression(operand, struct_table);
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn for_binary_op(bin_op: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(bin_op, struct_table.size_of(bin_op.get_annotations().ty()));

        if let Expression::BinaryOp(_m, _op, l, r) = bin_op {
            for_expression(l, struct_table);
            for_expression(r, struct_table);
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn for_if(if_exp: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(if_exp, struct_table.size_of(if_exp.get_annotations().ty()));

        if let Expression::If {
            annotation: _m,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            for_expression(cond, struct_table);
            for_expression(if_arm, struct_table);
            else_arm
                .as_ref()
                .map(|ea| for_expression(&ea, struct_table));
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn for_routine_call(rc: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(rc, struct_table.size_of(rc.get_annotations().ty()));

        if let Expression::RoutineCall(_m, _call, _name, params) = rc {
            for p in params {
                for_expression(p, struct_table);
            }
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn for_yield(yield_exp: &Expression<SymbolOffsetTable>, struct_table: &ResolvedStructTable) {
        trace!(
            yield_exp,
            struct_table.size_of(yield_exp.get_annotations().ty())
        );

        if let Expression::Yield(_m, e) = yield_exp {
            for_expression(e, struct_table);
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn for_struct_expression(
        se: &Expression<SymbolOffsetTable>,
        struct_table: &ResolvedStructTable,
    ) {
        trace!(se, struct_table.size_of(se.get_annotations().ty()));

        if let Expression::StructExpression(_annotations, _struct_name, fields) = se {
            for (_, fe) in fields {
                for_expression(fe, struct_table);
            }
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }
}
