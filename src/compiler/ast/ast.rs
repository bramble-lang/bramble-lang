use super::{scope::Level, struct_table};
use struct_table::ResolvedStructTable;

use crate::{compiler::ast::scope::{LayoutData, Scope}, semantics::semanticnode::SemanticMetadata, syntax::{module::{self, Item, Module}, routinedef::{RoutineDef, RoutineDefType}, statement::{Bind, Mutate, Printbln, Printi, Printiln, Prints, Return, Statement, Yield, YieldReturn}, structdef::StructDef}};
use crate::{semantics::semanticnode::SemanticNode, syntax::ast::Ast};
use braid_lang::result::Result;

pub type CompilerNode = Ast<Scope>;

impl CompilerNode {
    pub fn from(ast: &Module<SemanticMetadata>) -> Result<(Module<Scope>, ResolvedStructTable)> {
        let unresolved_struct_table = struct_table::UnresolvedStructTable::from_module(ast)?;
        let struct_table = unresolved_struct_table.resolve()?;
        let (compiler_ast, _) =
            CompilerNode::compute_layouts_for_module(ast, LayoutData::new(0), &struct_table);
        Ok((compiler_ast, struct_table))
    }

    fn compute_offsets(
        ast: &SemanticNode,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (CompilerNode, LayoutData) {
        use Ast::*;
        match ast {
            ExpressionBlock(m, body, final_exp) => {
                let (meta, mut nlayout) = Scope::local_from(m, struct_table, layout);
                let mut nbody = vec![];
                for e in body.iter() {
                    let (e, layout) = CompilerNode::compute_layouts_for_statement(e, nlayout, struct_table);
                    nlayout = layout;
                    nbody.push(e);
                }
                let (final_exp, nlayout) = match final_exp {
                    None => (None, nlayout),
                    Some(fe) => {
                        let (fe, ld) = CompilerNode::compute_offsets(fe, nlayout, struct_table);
                        (Some(Box::new(fe)), ld)
                    }
                };
                (ExpressionBlock(meta, nbody, final_exp), nlayout)
            }
            Ast::Integer(m, i) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::Integer(meta, *i), layout)
            }
            Ast::Boolean(m, b) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::Boolean(meta, *b), layout)
            }
            Ast::StringLiteral(m, s) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::StringLiteral(meta, s.clone()), layout)
            }
            Ast::CustomType(m, name) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::CustomType(meta, name.clone()), layout)
            }
            Ast::Identifier(m, id) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::Identifier(meta, id.clone()), layout)
            }
            Path(m, path) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::Path(meta, path.clone()), layout)
            }
            Ast::IdentifierDeclare(m, id, p) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::IdentifierDeclare(meta, id.clone(), p.clone()), layout)
            }
            MemberAccess(m, src, member) => {
                let (src, layout) = CompilerNode::compute_offsets(src, layout, struct_table);
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (MemberAccess(meta, Box::new(src), member.clone()), layout)
            }
            UnaryOp(m, op, ref operand) => {
                let (operand, layout) =
                    CompilerNode::compute_offsets(operand, layout, struct_table);
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::UnaryOp(meta, *op, Box::new(operand)), layout)
            }
            BinaryOp(m, op, ref l, ref r) => {
                let (l, layout) = CompilerNode::compute_offsets(l, layout, struct_table);
                let (r, layout) = CompilerNode::compute_offsets(r, layout, struct_table);
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                (Ast::BinaryOp(meta, *op, Box::new(l), Box::new(r)), layout)
            }
            If(m, ref cond, ref tb, ref fb) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                let (cond, layout) = CompilerNode::compute_offsets(cond, layout, struct_table);
                let (tb, layout) = CompilerNode::compute_offsets(tb, layout, struct_table);
                let (fb, layout) = CompilerNode::compute_offsets(fb, layout, struct_table);
                (If(meta, Box::new(cond), Box::new(tb), Box::new(fb)), layout)
            }
            Yield(m, e) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                let (e, layout) = CompilerNode::compute_offsets(e, layout, struct_table);
                (Yield(meta, Box::new(e)), layout)
            }
            RoutineCall(m, call, name, params) => {
                let (meta, layout) = Scope::local_from(m, struct_table, layout);
                let mut nlayout = layout;
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, playout) = CompilerNode::compute_offsets(p, nlayout, struct_table);
                    nlayout = playout;
                    nparams.push(np);
                }
                (RoutineCall(meta, *call, name.clone(), nparams), nlayout)
            }
            StructExpression(meta, struct_name, fields) => {
                let (meta, mut nlayout) = Scope::local_from(meta, struct_table, layout);
                let mut nfields = vec![];
                for (fname, fvalue) in fields.iter() {
                    let (nfv, no) = CompilerNode::compute_offsets(fvalue, nlayout, struct_table);
                    nlayout = no;
                    nfields.push((fname.clone(), nfv));
                }
                (
                    StructExpression(meta, struct_name.clone(), nfields),
                    nlayout,
                )
            }
        }
    }

    fn compute_layouts_for_module(
        m: &module::Module<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (module::Module<Scope>, LayoutData) {
        let (meta, mut layout) =
            Scope::module_from(m.get_metadata(), m.get_name(), struct_table, layout);

        let mut nmodule = module::Module::new(m.get_name(), meta);
        for child_module in m.get_modules().iter() {
            let (nchild_module, nlayout) =
                Self::compute_layouts_for_module(child_module, layout, struct_table);
            layout = nlayout;
            nmodule.add_module(nchild_module);
        }
        let (functions, layout) =
            Self::compute_layouts_for_items(m.get_functions(), layout, struct_table);
        *nmodule.get_functions_mut() = functions;

        let (coroutines, layout) =
            Self::compute_layouts_for_items(m.get_coroutines(), layout, struct_table);
        *nmodule.get_coroutines_mut() = coroutines;

        let (structs, layout) =
            Self::compute_layouts_for_items(m.get_structs(), layout, struct_table);
        *nmodule.get_structs_mut() = structs;

        (nmodule, layout)
    }

    fn compute_layouts_for_items(
        items: &Vec<Item<SemanticMetadata>>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Vec<Item<Scope>>, LayoutData) {
        let mut compiler_ast_items = vec![];
        let mut layout = layout;
        for item in items.iter() {
            let (c_ast_item, no) = match item {
                Item::Struct(sd) => {
                    let (sd2, ld) = Self::compute_layout_for_structdef(sd);
                    (Item::Struct(sd2), ld)
                }
                Item::Routine(rd) => {
                    let (rd2, ld) = Self::compute_layouts_for_routine(&rd, layout, struct_table);
                    (Item::Routine(rd2), ld)
                }
            };
            layout = no;
            compiler_ast_items.push(c_ast_item);
        }

        (compiler_ast_items, layout)
    }

    fn compute_layout_for_structdef(
        sd: &StructDef<SemanticMetadata>,
    ) -> (StructDef<Scope>, LayoutData) {
        let (scope, layout) = Scope::structdef_from(sd.get_metadata());
        (
            StructDef::new(sd.get_name(), scope, sd.get_fields().clone()),
            layout,
        )
    }

    fn compute_layouts_for_routine(
        rd: &RoutineDef<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (RoutineDef<Scope>, LayoutData) {
        let RoutineDef {
            meta,
            def,
            name,
            body,
            params,
            ty,
            ..
        } = rd;
        let initial_frame_size = match def {
            RoutineDefType::Function => 0,
            RoutineDefType::Coroutine => 20,
        };
        let (mut meta, offset) = Scope::routine_from(meta, def, struct_table, initial_frame_size);
        let mut nbody = vec![];
        let mut nlayout = LayoutData::new(offset);
        for e in body.iter() {
            let (e, layout) = CompilerNode::compute_layouts_for_statement(e, nlayout, struct_table);
            nlayout = layout;
            nbody.push(e);
        }
        meta.level = Level::Routine {
            next_label: 0,
            allocation: nlayout.offset,
            routine_type: *def,
        };
        (
            RoutineDef {
                meta,
                def: *def,
                name: name.clone(),
                params: params.clone(),
                ty: ty.clone(),
                body: nbody,
            },
            layout,
        )
    }

    fn compute_layouts_for_statement(
        statement: &Statement<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Statement<Scope>, LayoutData) {
        let (e, l) = match statement {
            Statement::Bind(b) => {
                let (e, l) = Self::compute_layouts_for_bind(b, layout, struct_table);
                (Statement::Bind(Box::new(e)), l)
            }
            Statement::Mutate(m) => {
                let (e, l) = Self::compute_layouts_for_mutate(m, layout, struct_table);
                (Statement::Mutate(Box::new(e)), l)
            }
            Statement::Return(r) => {
                let (e, l) = Self::compute_layouts_for_return(r, layout, struct_table);
                (Statement::Return(Box::new(e)), l)
            }
            Statement::YieldReturn(yr) => {
                let (e, l) = Self::compute_layouts_for_yieldreturn(yr, layout, struct_table);
                (Statement::YieldReturn(Box::new(e)), l)
            }
            Statement::Printi(pi) => {
                let (e, l) = Self::compute_layouts_for_printi(pi, layout, struct_table);
                (Statement::Printi(Box::new(e)), l)
            }
            Statement::Printiln(pi) => {
                let (e, l) = Self::compute_layouts_for_printiln(pi, layout, struct_table);
                (Statement::Printiln(Box::new(e)), l)
            }
            Statement::Printbln(pb) => {
                let (e, l) = Self::compute_layouts_for_printbln(pb, layout, struct_table);
                (Statement::Printbln(Box::new(e)), l)
            }
            Statement::Prints(ps) => {
                let (e, l) = Self::compute_layouts_for_prints(ps, layout, struct_table);
                (Statement::Prints(Box::new(e)), l)
            }
            Statement::Expression(e) => {
                let (e, l) = Self::compute_offsets(e, layout, struct_table);
                (Statement::Expression(Box::new(e)), l)
            }
        };
        (e, l)
        /*if let Ast::Statement(m, e) = statement {
            let (meta, layout) = Scope::local_from(m, struct_table, layout);
            let (e, layout) = CompilerNode::compute_offsets(e, layout, struct_table);
            (Ast::Statement(meta, Box::new(e)), layout)
        } else {
            panic!("Expected a statement but got {}", statement.root_str())
        }*/
    }

    fn compute_layouts_for_bind(
        bind: &Bind<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Bind<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(bind.get_metadata(), struct_table, layout);
        let (rhs, layout) = CompilerNode::compute_offsets(bind.get_rhs(), layout, struct_table);
        (
            Bind::new(meta, bind.get_id(), bind.get_type().clone(), bind.is_mutable(), rhs),
            layout,
        )
    }

    fn compute_layouts_for_mutate(
        mutate: &Mutate<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Mutate<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(mutate.get_metadata(), struct_table, layout);
        let (rhs, layout) = CompilerNode::compute_offsets(mutate.get_rhs(), layout, struct_table);
        (Mutate::new(meta, mutate.get_id(), rhs), layout)
    }

    fn compute_layouts_for_printi(
        p: &Printi<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Printi<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(p.get_metadata(), struct_table, layout);
        let (value, layout) = CompilerNode::compute_offsets(p.get_value(), layout, struct_table);
        (Printi::new(meta, value), layout)
    }

    fn compute_layouts_for_printiln(
        p: &Printiln<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Printiln<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(p.get_metadata(), struct_table, layout);
        let (value, layout) = CompilerNode::compute_offsets(p.get_value(), layout, struct_table);
        (Printiln::new(meta, value), layout)
    }

    fn compute_layouts_for_printbln(
        p: &Printbln<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Printbln<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(p.get_metadata(), struct_table, layout);
        let (value, layout) = CompilerNode::compute_offsets(p.get_value(), layout, struct_table);
        (Printbln::new(meta, value), layout)
    }

    fn compute_layouts_for_prints(
        p: &Prints<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Prints<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(p.get_metadata(), struct_table, layout);
        let (value, layout) = CompilerNode::compute_offsets(p.get_value(), layout, struct_table);
        (Prints::new(meta, value), layout)
    }

    fn compute_layouts_for_yield(
        y: &Yield<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Yield<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(y.get_metadata(), struct_table, layout);
        let (value, layout) = CompilerNode::compute_offsets(y.get_value(), layout, struct_table);
        (Yield::new(meta, value), layout)
    }

    fn compute_layouts_for_yieldreturn(
        yr: &YieldReturn<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (YieldReturn<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(yr.get_metadata(), struct_table, layout);
        match yr.get_value() {
            None => {
                (YieldReturn::new(meta, None), layout)
            }
            Some(val) => {
                let (value, layout) = CompilerNode::compute_offsets(val, layout, struct_table);
                (YieldReturn::new(meta, Some(value)), layout)
            }
        }
    }

    fn compute_layouts_for_return(
        r: &Return<SemanticMetadata>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Return<Scope>, LayoutData) {
        let (meta, layout) = Scope::local_from(r.get_metadata(), struct_table, layout);
        match r.get_value() {
            None => {
                (Return::new(meta, None), layout)
            }
            Some(val) => {
                let (value, layout) = CompilerNode::compute_offsets(val, layout, struct_table);
                (Return::new(meta, Some(value)), layout)
            }
        }
    }

    fn compute_layouts_for(
        items: &Vec<SemanticNode>,
        layout: LayoutData,
        struct_table: &ResolvedStructTable,
    ) -> (Vec<CompilerNode>, LayoutData) {
        let mut compiler_ast_items = vec![];
        let mut layout = layout;
        for item in items.iter() {
            let (c_ast_item, no) = CompilerNode::compute_offsets(item, layout, struct_table);
            layout = no;
            compiler_ast_items.push(c_ast_item);
        }

        (compiler_ast_items, layout)
    }
}

impl<Scope> RoutineDef<Scope> {
    pub fn validate_parameters(&self, params: &Vec<CompilerNode>) -> Result<()> {
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
    use crate::semantics::type_checker::type_check;
    use crate::syntax::routinedef::RoutineDefType;
    use crate::syntax::ty::Type;
    use crate::{compiler::ast::scope, syntax::path::Path};
    use crate::{
        compiler::ast::scope::Level,
        diagnostics::config::TracingConfig,
        lexer::{lexer::Lexer, tokens::Token},
        syntax::{ast::BinaryOperator, routinedef::RoutineDef},
    };
    use crate::{compiler::ast::struct_table::UnresolvedStructTable, semantics::symbol_table};
    use crate::{semantics::semanticnode::SemanticMetadata, semantics::semanticnode::SemanticNode};

    #[test]
    pub fn test_node_id_is_copied() {
        let sn = SemanticNode::Integer(
            SemanticMetadata {
                id: 3,
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
                canonical_path: vec!["root"].into(),
            },
            0,
        );
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_offsets(&sn, LayoutData::new(8), &empty_struct_table);
        match cn.0 {
            CompilerNode::Integer(m, _) => {
                assert_eq!(
                    m,
                    Scope::new(3, scope::Level::Local, vec!["root"].into(), m.ty.clone())
                );
            }
            _ => assert_eq!(true, false),
        }
    }

    #[test]
    pub fn test_integer() {
        let sn = SemanticNode::Integer(
            SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
                canonical_path: vec!["root"].into(),
            },
            0,
        );
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_offsets(&sn, LayoutData::new(8), &empty_struct_table);
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::Integer(m, v) => {
                assert_eq!(v, 0);
                assert_eq!(
                    m,
                    Scope::new(0, scope::Level::Local, vec!["root"].into(), m.ty.clone())
                );
            }
            _ => assert_eq!(true, false),
        }
    }

    #[test]
    pub fn test_operator() {
        let sn1 = SemanticNode::Integer(
            SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
                canonical_path: vec!["root"].into(),
            },
            1,
        );
        let sn2 = SemanticNode::Integer(
            SemanticMetadata {
                id: 1,
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
                canonical_path: vec!["root"].into(),
            },
            2,
        );
        let snmul = SemanticNode::BinaryOp(
            SemanticMetadata {
                id: 2,
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
                canonical_path: vec!["root"].into(),
            },
            BinaryOperator::Mul,
            Box::new(sn1),
            Box::new(sn2),
        );
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_offsets(&snmul, LayoutData::new(8), &empty_struct_table);
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::BinaryOp(m, BinaryOperator::Mul, l, r) => {
                assert_eq!(
                    m,
                    Scope::new(2, Level::Local, vec!["root"].into(), m.ty.clone()),
                );

                match *l {
                    CompilerNode::Integer(m, v) => {
                        assert_eq!(m.id, 0);
                        assert_eq!(v, 1);
                    }
                    _ => assert!(false),
                }
                match *r {
                    CompilerNode::Integer(m, v) => {
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
        semantic_table.add("x", Type::I32, false).unwrap();
        semantic_table.add("y", Type::I32, false).unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
                canonical_path: Path::new(),
            },
            vec![],
            None,
        );
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_offsets(&sn, LayoutData::new(0), &empty_struct_table);
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::ExpressionBlock(m, _, _) => {
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_nested_expression_block() {
        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table.add("x", Type::I32, false).unwrap();
        semantic_table.add("y", Type::I32, false).unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
                canonical_path: Path::new(),
            },
            vec![],
            None,
        );

        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table.add("x", Type::I32, false).unwrap();
        semantic_table.add("y", Type::I32, false).unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
                canonical_path: Path::new(),
            },
            vec![],
            Some(Box::new(sn))
        );
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_offsets(&sn, LayoutData::new(0), &empty_struct_table);
        assert_eq!(cn.1.offset, 16);
        match cn.0 {
            CompilerNode::ExpressionBlock(m, _b, fe) => {
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);
                match fe {
                    Some(box CompilerNode::ExpressionBlock(m, _, _)) => {
                        assert_eq!(m.symbols.table.len(), 2);
                        assert_eq!(m.symbols.table["x"].size, 4);
                        assert_eq!(m.symbols.table["x"].offset, 12);
                        assert_eq!(m.symbols.table["y"].size, 4);
                        assert_eq!(m.symbols.table["y"].offset, 16);
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
        semantic_table.add("x", Type::I32, false).unwrap();
        semantic_table.add("y", Type::I32, false).unwrap();
        let sn = RoutineDef {
            meta: SemanticMetadata {
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
        let mut module = Module::new("root", SemanticMetadata::new(1, 1, Type::Unit));
        module.add_function(sn).unwrap();
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn = CompilerNode::compute_layouts_for_module(
            &module,
            LayoutData::new(0),
            &empty_struct_table,
        );
        assert_eq!(cn.1.offset, 0);
        let module = cn.0;
        match module.get_item("func") {
            Some(Item::Routine(RoutineDef {
                meta,
                def: RoutineDefType::Function,
                name,
                ..
            })) => {
                assert_eq!(name, "func");
                assert_eq!(meta.symbols.table.len(), 2);
                assert_eq!(meta.symbols.table["x"].size, 4);
                assert_eq!(meta.symbols.table["x"].offset, 4);
                assert_eq!(meta.symbols.table["y"].size, 4);
                assert_eq!(meta.symbols.table["y"].offset, 8);

                match meta.level {
                    scope::Level::Routine {
                        next_label,
                        allocation,
                        routine_type,
                    } => {
                        assert_eq!(next_label, 0);
                        assert_eq!(allocation, 8);
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
            meta: SemanticMetadata {
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
            meta: SemanticMetadata {
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
        let cn = CompilerNode::compute_offsets(&sn, LayoutData::new(0), &empty_struct_table);
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::RoutineDef {
                meta,
                def: RoutineDefType::Function,
                body,
                ..
            } => {
                assert_eq!(meta.symbols.table.len(), 2);
                assert_eq!(meta.symbols.table["x"].size, 4);
                assert_eq!(meta.symbols.table["x"].offset, 4);
                assert_eq!(meta.symbols.table["y"].size, 4);
                assert_eq!(meta.symbols.table["y"].offset, 8);

                match body.iter().nth(0) {
                    Some(CompilerNode::RoutineDef {
                        meta,
                        def: RoutineDefType::Function,
                        ..
                    }) => {
                        assert_eq!(meta.symbols.table.len(), 2);
                        assert_eq!(meta.symbols.table["x"].size, 4);
                        assert_eq!(meta.symbols.table["x"].offset, 4);
                        assert_eq!(meta.symbols.table["y"].size, 4);
                        assert_eq!(meta.symbols.table["y"].offset, 8);
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
        semantic_table.add("x", Type::I32, false).unwrap();
        semantic_table.add("y", Type::I32, false).unwrap();
        let sn = RoutineDef {
            meta: SemanticMetadata {
                id: 0,
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
                canonical_path: Path::new(),
            },
            def: RoutineDefType::Coroutine,
            name: "coroutine".into(),
            params: vec![],
            ty: Type::I32,
            body: vec![],
        };
        let empty_struct_table = UnresolvedStructTable::new().resolve().unwrap();
        let cn =
            CompilerNode::compute_layouts_for_routine(&sn, LayoutData::new(0), &empty_struct_table);
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            RoutineDef {
                meta,
                def: RoutineDefType::Coroutine,
                name,
                ..
            } => {
                assert_eq!(name, "coroutine");
                assert_eq!(meta.symbols.table.len(), 2);
                assert_eq!(meta.symbols.table["x"].size, 4);
                assert_eq!(meta.symbols.table["x"].offset, 24);
                assert_eq!(meta.symbols.table["y"].size, 4);
                assert_eq!(meta.symbols.table["y"].offset, 28);

                match meta.level {
                    scope::Level::Routine { allocation, .. } => assert_eq!(allocation, 28),
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_bug() {
        use crate::parser::parser;
        for text in vec![
            "
            fn my_main() {
                let x:i32 := 5;
                printiln x;

                let b:bool := my_bool();
                printbln b;
                return;
            }

            fn my_bool() -> bool {
                let b:bool := false;
                return b;
            }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let semantic_module = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            let unrealized_st = UnresolvedStructTable::from_module(&semantic_module).unwrap();
            let resolved = unrealized_st.resolve();

            assert_eq!(resolved.err(), None);
        }
    }
}
