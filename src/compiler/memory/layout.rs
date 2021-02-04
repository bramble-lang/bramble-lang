use super::{scope::Level, struct_table};
use struct_table::ResolvedStructTable;

use crate::{
    compiler::memory::scope::{LayoutData, SymbolOffsetTable},
    semantics::semanticnode::SemanticAnnotations,
    syntax::{
        module::{self, Item, Module},
        routinedef::{RoutineDef, RoutineDefType},
        statement::{Bind, Mutate, Return, Statement, YieldReturn},
        structdef::StructDef,
    },
};
use crate::{semantics::semanticnode::SemanticNode, syntax::expression::Expression};
use braid_lang::result::Result;

/**
 * Compute the how every function and struct will be laid out in memory: for functions
 * this computes the size of the stack frame and the offset within the stack frame for
 * every function parameter and variable that is local to the function, for structures
 * it computes how large the structure is and what the relative offset of every field
 * in the structure is.
 */

pub fn compute_layout_for_program(
    ast: &Module<SemanticAnnotations>,
) -> Result<(Module<SymbolOffsetTable>, ResolvedStructTable)> {
    let unresolved_struct_table = struct_table::UnresolvedStructTable::from_module(ast)?;
    let struct_table = unresolved_struct_table.resolve()?;
    let (compiler_ast, _) = compute::layouts_for_module(ast, LayoutData::new(0), &struct_table);
    Ok((compiler_ast, struct_table))
}

mod compute {
    use super::*;

    pub(super) fn layouts_for_module(
        m: &module::Module<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (module::Module<SymbolOffsetTable>, LayoutData) {
        let (annotations, mut layout) =
            SymbolOffsetTable::module_from(m.get_annotations(), m.get_name(), struct_table, layout);

        let mut nmodule = module::Module::new(m.get_name(), annotations);
        for child_module in m.get_modules().iter() {
            let (nchild_module, nlayout) = layouts_for_module(child_module, layout, struct_table);
            layout = nlayout;
            nmodule.add_module(nchild_module);
        }
        let (functions, layout) = layouts_for_items(m.get_functions(), layout, struct_table);
        *nmodule.get_functions_mut() = functions;

        let (coroutines, layout) = layouts_for_items(m.get_coroutines(), layout, struct_table);
        *nmodule.get_coroutines_mut() = coroutines;

        let (structs, layout) = layouts_for_items(m.get_structs(), layout, struct_table);
        *nmodule.get_structs_mut() = structs;

        (nmodule, layout)
    }

    fn layouts_for_items(
        items: &Vec<Item<SemanticAnnotations>>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Vec<Item<SymbolOffsetTable>>, LayoutData) {
        let mut compiler_ast_items = vec![];
        let mut layout = layout;
        for item in items.iter() {
            let (c_ast_item, no) = match item {
                Item::Struct(sd) => {
                    let (sd2, ld) = layout_for_structdef(sd);
                    (Item::Struct(sd2), ld)
                }
                Item::Routine(rd) => {
                    let (rd2, ld) = layout_for_routine(&rd, layout, struct_table);
                    (Item::Routine(rd2), ld)
                }
            };
            layout = no;
            compiler_ast_items.push(c_ast_item);
        }

        (compiler_ast_items, layout)
    }

    fn layout_for_structdef(
        sd: &StructDef<SemanticAnnotations>,
    ) -> (StructDef<SymbolOffsetTable>, LayoutData) {
        let (scope, layout) = SymbolOffsetTable::structdef_from(sd.get_annotations());
        (
            StructDef::new(sd.get_name(), scope, sd.get_fields().clone()),
            layout,
        )
    }

    fn layout_for_routine(
        rd: &RoutineDef<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (RoutineDef<SymbolOffsetTable>, LayoutData) {
        let RoutineDef {
            annotations,
            def,
            name,
            body,
            params,
            ty,
            ..
        } = rd;
        let initial_frame_size = match def {
            RoutineDefType::Function => 0,
            RoutineDefType::Coroutine => 40,
        };
        let (mut annotations, offset) =
            SymbolOffsetTable::routine_from(annotations, def, struct_table, initial_frame_size);
        let mut nbody = vec![];
        let mut nlayout = LayoutData::new(offset);
        for e in body.iter() {
            let (e, layout) = compute::layout_for_statement(e, nlayout, struct_table);
            nlayout = layout;
            nbody.push(e);
        }
        annotations.level = Level::Routine {
            allocation: nlayout.offset,
            routine_type: *def,
        };
        (
            RoutineDef {
                annotations,
                def: *def,
                name: name.clone(),
                params: params.clone(),
                ty: ty.clone(),
                body: nbody,
            },
            layout,
        )
    }

    fn layout_for_statement(
        statement: &Statement<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Statement<SymbolOffsetTable>, LayoutData) {
        let (e, l) = match statement {
            Statement::Bind(b) => {
                let (e, l) = layout_for_bind(b, layout, struct_table);
                (Statement::Bind(Box::new(e)), l)
            }
            Statement::Mutate(m) => {
                let (e, l) = layout_for_mutate(m, layout, struct_table);
                (Statement::Mutate(Box::new(e)), l)
            }
            Statement::Return(r) => {
                let (e, l) = layout_for_return(r, layout, struct_table);
                (Statement::Return(Box::new(e)), l)
            }
            Statement::YieldReturn(yr) => {
                let (e, l) = layout_for_yieldreturn(yr, layout, struct_table);
                (Statement::YieldReturn(Box::new(e)), l)
            }
            Statement::Expression(e) => {
                let (e, l) = layout_for_expression(e, layout, struct_table);
                (Statement::Expression(Box::new(e)), l)
            }
        };
        (e, l)
    }

    fn layout_for_bind(
        bind: &Bind<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Bind<SymbolOffsetTable>, LayoutData) {
        let (annotations, layout) =
            SymbolOffsetTable::local_from(bind.get_annotations(), struct_table, layout);
        let (rhs, layout) = compute::layout_for_expression(bind.get_rhs(), layout, struct_table);
        (
            Bind::new(
                annotations,
                bind.get_id(),
                bind.get_type().clone(),
                bind.is_mutable(),
                rhs,
            ),
            layout,
        )
    }

    fn layout_for_mutate(
        mutate: &Mutate<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Mutate<SymbolOffsetTable>, LayoutData) {
        let (annotations, layout) =
            SymbolOffsetTable::local_from(mutate.get_annotations(), struct_table, layout);
        let (rhs, layout) = compute::layout_for_expression(mutate.get_rhs(), layout, struct_table);
        (Mutate::new(annotations, mutate.get_id(), rhs), layout)
    }

    fn layout_for_yieldreturn(
        yr: &YieldReturn<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (YieldReturn<SymbolOffsetTable>, LayoutData) {
        let (annotations, layout) =
            SymbolOffsetTable::local_from(yr.get_annotations(), struct_table, layout);
        match yr.get_value() {
            None => (YieldReturn::new(annotations, None), layout),
            Some(val) => {
                let (value, layout) = compute::layout_for_expression(val, layout, struct_table);
                (YieldReturn::new(annotations, Some(value)), layout)
            }
        }
    }

    fn layout_for_return(
        r: &Return<SemanticAnnotations>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Return<SymbolOffsetTable>, LayoutData) {
        let (annotations, layout) =
            SymbolOffsetTable::local_from(r.get_annotations(), struct_table, layout);
        match r.get_value() {
            None => (Return::new(annotations, None), layout),
            Some(val) => {
                let (value, layout) = compute::layout_for_expression(val, layout, struct_table);
                (Return::new(annotations, Some(value)), layout)
            }
        }
    }

    fn layout_for_expression(
        ast: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        use Expression::*;
        match ast {
            ExpressionBlock(..) => layout_for_expression_block(ast, layout, struct_table),
            Expression::Integer64(m, i) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::Integer64(annotations, *i), layout)
            }
            Expression::Boolean(m, b) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::Boolean(annotations, *b), layout)
            }
            Expression::StringLiteral(m, s) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::StringLiteral(annotations, s.clone()), layout)
            }
            Expression::CustomType(m, name) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::CustomType(annotations, name.clone()), layout)
            }
            Expression::Identifier(m, id) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::Identifier(annotations, id.clone()), layout)
            }
            Path(m, path) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (Expression::Path(annotations, path.clone()), layout)
            }
            Expression::IdentifierDeclare(m, id, p) => {
                let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
                (
                    Expression::IdentifierDeclare(annotations, id.clone(), p.clone()),
                    layout,
                )
            }
            MemberAccess(..) => layout_for_member_access(ast, layout, struct_table),
            UnaryOp(..) => layout_for_unary_op(ast, layout, struct_table),
            BinaryOp(..) => layout_for_binary_op(ast, layout, struct_table),
            If { .. } => layout_for_if(ast, layout, struct_table),
            Yield(..) => layout_for_yield(ast, layout, struct_table),
            RoutineCall(..) => layout_for_routine_call(ast, layout, struct_table),
            StructExpression(..) => layout_for_struct_expression(ast, layout, struct_table),
        }
    }

    fn layout_for_expression_block(
        block: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::ExpressionBlock(m, body, final_exp) = block {
            let (annotations, mut nlayout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            let mut nbody = vec![];
            for e in body.iter() {
                let (e, layout) = layout_for_statement(e, nlayout, struct_table);
                nlayout = layout;
                nbody.push(e);
            }
            let (final_exp, nlayout) = match final_exp {
                None => (None, nlayout),
                Some(fe) => {
                    let (fe, ld) = layout_for_expression(fe, nlayout, struct_table);
                    (Some(Box::new(fe)), ld)
                }
            };
            (
                Expression::ExpressionBlock(annotations, nbody, final_exp),
                nlayout,
            )
        } else {
            panic!("Expected ExpressionBlock, but got {:?}", block)
        }
    }

    fn layout_for_member_access(
        access: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::MemberAccess(m, src, member) = access {
            let (src, layout) = layout_for_expression(src, layout, struct_table);
            let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            (
                Expression::MemberAccess(annotations, Box::new(src), member.clone()),
                layout,
            )
        } else {
            panic!("Expected MemberAccess, but got {:?}", access)
        }
    }

    fn layout_for_unary_op(
        un_op: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::UnaryOp(m, op, operand) = un_op {
            let (operand, layout) = layout_for_expression(operand, layout, struct_table);
            let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            (
                Expression::UnaryOp(annotations, *op, Box::new(operand)),
                layout,
            )
        } else {
            panic!("Expected UnaryOp, but got {:?}", un_op)
        }
    }

    fn layout_for_binary_op(
        bin_op: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::BinaryOp(m, op, l, r) = bin_op {
            let (l, layout) = compute::layout_for_expression(l, layout, struct_table);
            let (r, layout) = compute::layout_for_expression(r, layout, struct_table);
            let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            (
                Expression::BinaryOp(annotations, *op, Box::new(l), Box::new(r)),
                layout,
            )
        } else {
            panic!("Expected BinaryOp, but got {:?}", bin_op)
        }
    }

    fn layout_for_if(
        if_exp: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let SemanticNode::If {
            annotation: m,
            cond,
            if_arm,
            else_arm,
        } = if_exp
        {
            let (annotation, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            let (cond, layout) = compute::layout_for_expression(cond, layout, struct_table);
            let (if_arm, layout) = compute::layout_for_expression(if_arm, layout, struct_table);
            let (else_arm, layout) = match else_arm {
                Some(fb) => {
                    let (else_arm, layout) =
                        compute::layout_for_expression(&fb, layout, struct_table);
                    (Some(box else_arm), layout)
                }
                None => (None, layout),
            };
            (
                Expression::If {
                    annotation,
                    cond: box cond,
                    if_arm: box if_arm,
                    else_arm,
                },
                layout,
            )
        } else {
            panic!("Expected IfExpression, but got {:?}", if_exp)
        }
    }

    fn layout_for_routine_call(
        rc: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::RoutineCall(m, call, name, params) = rc {
            let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            let mut nlayout = layout;
            let mut nparams = vec![];
            for p in params.iter() {
                let (np, playout) = layout_for_expression(p, nlayout, struct_table);
                nlayout = playout;
                nparams.push(np);
            }
            (
                Expression::RoutineCall(annotations, *call, name.clone(), nparams),
                nlayout,
            )
        } else {
            panic!("Expected RoutineCall, but got {:?}", rc)
        }
    }

    fn layout_for_yield(
        yield_exp: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let Expression::Yield(m, e) = yield_exp {
            let (annotations, layout) = SymbolOffsetTable::local_from(m, struct_table, layout);
            let (e, layout) = layout_for_expression(e, layout, struct_table);
            (Expression::Yield(annotations, Box::new(e)), layout)
        } else {
            panic!("Expected Yield, but got {:?}", yield_exp)
        }
    }

    fn layout_for_struct_expression(
        se: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Expression<SymbolOffsetTable>, LayoutData) {
        if let SemanticNode::StructExpression(annotations, struct_name, fields) = se {
            let (annotations, mut nlayout) =
                SymbolOffsetTable::local_from(annotations, struct_table, layout);
            let mut nfields = vec![];
            for (fname, fvalue) in fields.iter() {
                let (nfv, no) = layout_for_expression(fvalue, nlayout, struct_table);
                nlayout = no;
                nfields.push((fname.clone(), nfv));
            }
            (
                Expression::StructExpression(annotations, struct_name.clone(), nfields),
                nlayout,
            )
        } else {
            panic!("Expected StructExpression, but got {:?}", se)
        }
    }

    impl<SymbolOffsetTable> RoutineDef<SymbolOffsetTable> {
        pub fn validate_parameters(
            &self,
            params: &Vec<Expression<SymbolOffsetTable>>,
        ) -> Result<()> {
            let expected_params = self.get_params();
            if params.len() == expected_params.len() {
                Ok(())
            } else {
                Err(format!(
                    "Critical: expected {} but got {} parameters for {}",
                    expected_params.len(),
                    params.len(),
                    self.root_str()
                ))
            }
        }
    }

    #[cfg(test)]
    mod ast_tests {
        use module::Module;

        use super::*;
        use crate::syntax::routinedef::RoutineDefType;
        use crate::syntax::ty::Type;
        use crate::{compiler::memory::scope, syntax::path::Path};
        use crate::{
            compiler::memory::scope::Level,
            syntax::{expression::BinaryOperator, routinedef::RoutineDef},
        };
        use crate::{
            compiler::memory::struct_table::UnresolvedStructTable, semantics::symbol_table,
        };
        use crate::{
            semantics::semanticnode::SemanticAnnotations, semantics::semanticnode::SemanticNode,
        };

        #[test]
        pub fn test_node_id_is_copied() {
            let sn = SemanticNode::Integer64(
                SemanticAnnotations {
                    id: 3,
                    ln: 0,
                    ty: Type::I64,
                    sym: symbol_table::SymbolTable::new(),
                    canonical_path: vec!["root"].into(),
                },
                0,
            );
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layout_for_expression(&sn, LayoutData::new(8), &empty_struct_table);
            match cn.0 {
                Expression::Integer64(m, _) => {
                    assert_eq!(
                        m,
                        SymbolOffsetTable::new(
                            3,
                            scope::Level::Local,
                            vec!["root"].into(),
                            m.ty.clone()
                        )
                    );
                }
                _ => assert_eq!(true, false),
            }
        }

        #[test]
        pub fn test_integer() {
            let sn = SemanticNode::Integer64(
                SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: symbol_table::SymbolTable::new(),
                    canonical_path: vec!["root"].into(),
                },
                0,
            );
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layout_for_expression(&sn, LayoutData::new(8), &empty_struct_table);
            assert_eq!(cn.1.offset, 8);
            match cn.0 {
                Expression::Integer64(m, v) => {
                    assert_eq!(v, 0);
                    assert_eq!(
                        m,
                        SymbolOffsetTable::new(
                            0,
                            scope::Level::Local,
                            vec!["root"].into(),
                            m.ty.clone()
                        )
                    );
                }
                _ => assert_eq!(true, false),
            }
        }

        #[test]
        pub fn test_operator() {
            let sn1 = SemanticNode::Integer64(
                SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: symbol_table::SymbolTable::new(),
                    canonical_path: vec!["root"].into(),
                },
                1,
            );
            let sn2 = SemanticNode::Integer64(
                SemanticAnnotations {
                    id: 1,
                    ln: 0,
                    ty: Type::I64,
                    sym: symbol_table::SymbolTable::new(),
                    canonical_path: vec!["root"].into(),
                },
                2,
            );
            let snmul = SemanticNode::BinaryOp(
                SemanticAnnotations {
                    id: 2,
                    ln: 0,
                    ty: Type::I64,
                    sym: symbol_table::SymbolTable::new(),
                    canonical_path: vec!["root"].into(),
                },
                BinaryOperator::Mul,
                Box::new(sn1),
                Box::new(sn2),
            );
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn =
                compute::layout_for_expression(&snmul, LayoutData::new(8), &empty_struct_table);
            assert_eq!(cn.1.offset, 8);
            match cn.0 {
                Expression::BinaryOp(m, BinaryOperator::Mul, l, r) => {
                    assert_eq!(
                        m,
                        SymbolOffsetTable::new(2, Level::Local, vec!["root"].into(), m.ty.clone()),
                    );

                    match *l {
                        Expression::Integer64(m, v) => {
                            assert_eq!(m.id, 0);
                            assert_eq!(v, 1);
                        }
                        _ => assert!(false),
                    }
                    match *r {
                        Expression::Integer64(m, v) => {
                            assert_eq!(m.id, 1);
                            assert_eq!(v, 2);
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        }

        #[test]
        pub fn test_expression_block() {
            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I64, false).unwrap();
            semantic_table.add("y", Type::I64, false).unwrap();
            let sn = SemanticNode::ExpressionBlock(
                SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                vec![],
                None,
            );
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layout_for_expression(&sn, LayoutData::new(0), &empty_struct_table);
            assert_eq!(cn.1.offset, 16);
            match cn.0 {
                Expression::ExpressionBlock(m, _, _) => {
                    assert_eq!(m.symbols.table.len(), 2);
                    assert_eq!(m.symbols.table["x"].size, 8);
                    assert_eq!(m.symbols.table["x"].offset, 8);
                    assert_eq!(m.symbols.table["y"].size, 8);
                    assert_eq!(m.symbols.table["y"].offset, 16);
                }
                _ => assert!(false),
            }
        }

        #[test]
        pub fn test_nested_expression_block() {
            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I64, false).unwrap();
            semantic_table.add("y", Type::I64, false).unwrap();
            let sn = SemanticNode::ExpressionBlock(
                SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                vec![],
                None,
            );

            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I64, false).unwrap();
            semantic_table.add("y", Type::I64, false).unwrap();
            let sn = SemanticNode::ExpressionBlock(
                SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                vec![],
                Some(Box::new(sn)),
            );
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layout_for_expression(&sn, LayoutData::new(0), &empty_struct_table);
            assert_eq!(cn.1.offset, 32);
            match cn.0 {
                Expression::ExpressionBlock(m, _b, fe) => {
                    assert_eq!(m.symbols.table.len(), 2);
                    assert_eq!(m.symbols.table["x"].size, 8);
                    assert_eq!(m.symbols.table["x"].offset, 8);
                    assert_eq!(m.symbols.table["y"].size, 8);
                    assert_eq!(m.symbols.table["y"].offset, 16);
                    match fe {
                        Some(box Expression::ExpressionBlock(m, _, _)) => {
                            assert_eq!(m.symbols.table.len(), 2);
                            assert_eq!(m.symbols.table["x"].size, 8);
                            assert_eq!(m.symbols.table["x"].offset, 24);
                            assert_eq!(m.symbols.table["y"].size, 8);
                            assert_eq!(m.symbols.table["y"].offset, 32);
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        }

        #[test]
        pub fn test_function() {
            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I64, false).unwrap();
            semantic_table.add("y", Type::I64, false).unwrap();
            let sn = RoutineDef {
                annotations: SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                def: RoutineDefType::Function,
                name: "func".into(),
                params: vec![],
                ty: Type::I64,
                body: vec![],
            };
            let mut module = Module::new("root", SemanticAnnotations::new(1, 1, Type::Unit));
            module.add_function(sn).unwrap();
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layouts_for_module(&module, LayoutData::new(0), &empty_struct_table);
            assert_eq!(cn.1.offset, 0);
            let module = cn.0;
            match module.get_item("func") {
                Some(Item::Routine(RoutineDef {
                    annotations,
                    def: RoutineDefType::Function,
                    name,
                    ..
                })) => {
                    assert_eq!(name, "func");
                    assert_eq!(annotations.symbols.table.len(), 2);
                    assert_eq!(annotations.symbols.table["x"].size, 8);
                    assert_eq!(annotations.symbols.table["x"].offset, 8);
                    assert_eq!(annotations.symbols.table["y"].size, 8);
                    assert_eq!(annotations.symbols.table["y"].offset, 16);

                    match annotations.level {
                        scope::Level::Routine {
                            allocation,
                            routine_type,
                        } => {
                            assert_eq!(allocation, 16);
                            assert_eq!(routine_type, RoutineDefType::Function);
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        }

        /*
        Current design does not support functions defined in functions

        #[test]
        pub fn test_nested_function() {
            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I32, false).unwrap();
            semantic_table.add("y", Type::I32, false).unwrap();
            let sn = RoutineDef {
                annotations: SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I32,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                def: RoutineDefType::Function,
                name: "func".into(),
                params: vec![],
                ty: Type::I32,
                body: vec![],
            };

            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I32, false).unwrap();
            semantic_table.add("y", Type::I32, false).unwrap();
            let sn = RoutineDef {
                annotations: SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I32,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                def: RoutineDefType::Function,
                name: "outer_func".into(),
                params: vec![],
                ty: Type::I32,
                body: vec![sn],
            };

            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::compute_offsets(&sn, LayoutData::new(0), &empty_struct_table);
            assert_eq!(cn.1.offset, 0);
            match cn.0 {
                compute::RoutineDef {
                    annotations,
                    def: RoutineDefType::Function,
                    body,
                    ..
                } => {
                    assert_eq!(annotations.symbols.table.len(), 2);
                    assert_eq!(annotations.symbols.table["x"].size, 4);
                    assert_eq!(annotations.symbols.table["x"].offset, 4);
                    assert_eq!(annotations.symbols.table["y"].size, 4);
                    assert_eq!(annotations.symbols.table["y"].offset, 8);

                    match body.iter().nth(0) {
                        Some(compute::RoutineDef {
                            annotations,
                            def: RoutineDefType::Function,
                            ..
                        }) => {
                            assert_eq!(annotations.symbols.table.len(), 2);
                            assert_eq!(annotations.symbols.table["x"].size, 4);
                            assert_eq!(annotations.symbols.table["x"].offset, 4);
                            assert_eq!(annotations.symbols.table["y"].size, 4);
                            assert_eq!(annotations.symbols.table["y"].offset, 8);
                        }
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        }*/

        #[test]
        pub fn test_coroutine() {
            let mut semantic_table = symbol_table::SymbolTable::new();
            semantic_table.add("x", Type::I64, false).unwrap();
            semantic_table.add("y", Type::I64, false).unwrap();
            let sn = RoutineDef {
                annotations: SemanticAnnotations {
                    id: 0,
                    ln: 0,
                    ty: Type::I64,
                    sym: semantic_table,
                    canonical_path: Path::new(),
                },
                def: RoutineDefType::Coroutine,
                name: "coroutine".into(),
                params: vec![],
                ty: Type::I64,
                body: vec![],
            };
            let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
            let cn = compute::layout_for_routine(&sn, LayoutData::new(0), &empty_struct_table);
            assert_eq!(cn.1.offset, 0);
            match cn.0 {
                RoutineDef {
                    annotations,
                    def: RoutineDefType::Coroutine,
                    name,
                    ..
                } => {
                    assert_eq!(name, "coroutine");
                    assert_eq!(annotations.symbols.table.len(), 2);
                    assert_eq!(annotations.symbols.table["x"].size, 8);
                    assert_eq!(annotations.symbols.table["x"].offset, 48);
                    assert_eq!(annotations.symbols.table["y"].size, 8);
                    assert_eq!(annotations.symbols.table["y"].offset, 56);

                    match annotations.level {
                        scope::Level::Routine { allocation, .. } => assert_eq!(allocation, 56),
                        _ => assert!(false),
                    }
                }
                _ => assert!(false),
            }
        }
    }
}
