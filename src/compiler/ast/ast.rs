use crate::compiler::ast::scope::{LayoutData, Scope, Level};

use crate::{semantics::semanticnode::SemanticNode, syntax, syntax::ast::Ast};

pub type CompilerNode = Ast<Scope>;

impl CompilerNode {
    pub fn from(ast: &SemanticNode, layout: LayoutData) -> (CompilerNode, LayoutData) {
        use Ast::*;
        match ast {
            ExpressionBlock(m, body) => {
                let (meta, mut nlayout) = Scope::block_from(m, layout);
                let mut nbody = vec![];
                for e in body.iter() {
                    let (e, layout) = CompilerNode::from(e, nlayout);
                    nlayout = layout;
                    nbody.push(e);
                }
                (ExpressionBlock(meta, nbody), nlayout)
            }
            RoutineDef(m, def, name, params, ret_ty, body) => {
                let initial_frame_size = match def {
                    syntax::ast::RoutineDef::Function => 0,
                    syntax::ast::RoutineDef::Coroutine => 20,
                };
                let (mut meta, offset2) = Scope::routine_from(m, initial_frame_size);
                let mut nbody = vec![];
                let mut nlayout = LayoutData::new(offset2);
                for e in body.iter() {
                    let (e, layout) = CompilerNode::from(e, nlayout);
                    nlayout = layout;
                    nbody.push(e);
                }
                meta.ty = Level::Routine {
                    next_label: 0,
                    allocation: nlayout.offset,
                };
                (
                    RoutineDef(
                        meta,
                        *def,
                        name.clone(),
                        params.clone(),
                        ret_ty.clone(),
                        nbody,
                    ),
                    layout,
                )
            }
            Ast::Integer(m, i) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::Integer(meta, *i), layout)
            }
            Ast::Boolean(m, b) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::Boolean(meta, *b), layout)
            }
            Ast::CustomType(m, name) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::CustomType(meta, name.clone()), layout)
            }
            Ast::Identifier(m, id) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::Identifier(meta, id.clone()), layout)
            }
            Ast::IdentifierDeclare(m, id, p) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::IdentifierDeclare(meta, id.clone(), p.clone()), layout)
            }
            UnaryOp(m, op, ref operand) => {
                let (operand, layout) = CompilerNode::from(operand, layout);
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::UnaryOp(meta, *op, Box::new(operand)), layout)
            }
            BinaryOp(m, op, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, layout) = Scope::block_from(m, layout);
                (Ast::BinaryOp(meta, *op, Box::new(l), Box::new(r)), layout)
            }
            Printi(m, ref e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Printi(meta, Box::new(e)), layout)
            }
            Printiln(m, ref e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Printiln(meta, Box::new(e)), layout)
            }
            Printbln(m, ref e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Printbln(meta, Box::new(e)), layout)
            }
            If(m, ref cond, ref tb, ref fb) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (cond, layout) = CompilerNode::from(cond, layout);
                let (tb, layout) = CompilerNode::from(tb, layout);
                let (fb, layout) = CompilerNode::from(fb, layout);
                (If(meta, Box::new(cond), Box::new(tb), Box::new(fb)), layout)
            }
            Mutate(m, id, e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Mutate(meta, id.clone(), Box::new(e)), layout)
            }
            Bind(m, id, mutable, p, e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (
                    Bind(meta, id.clone(), *mutable, p.clone(), Box::new(e)),
                    layout,
                )
            }
            Yield(m, e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Yield(meta, Box::new(e)), layout)
            }
            Return(m, None) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (Return(meta, None), layout)
            }
            Return(m, Some(e)) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Return(meta, Some(Box::new(e))), layout)
            }
            YieldReturn(m, None) => {
                let (meta, layout) = Scope::block_from(m, layout);
                (YieldReturn(meta, None), layout)
            }
            YieldReturn(m, Some(e)) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (YieldReturn(meta, Some(Box::new(e))), layout)
            }
            Statement(m, e) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let (e, layout) = CompilerNode::from(e, layout);
                (Statement(meta, Box::new(e)), layout)
            }
            RoutineCall(m, call, name, params) => {
                let (meta, layout) = Scope::block_from(m, layout);
                let mut nlayout = layout;
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, playout) = CompilerNode::from(p, nlayout);
                    nlayout = playout;
                    nparams.push(np);
                }
                (RoutineCall(meta, *call, name.clone(), nparams), nlayout)
            }
            Module {
                meta,
                functions,
                coroutines,
                structs,
            } => {
                let (meta, layout) = Scope::block_from(meta, layout);
                let mut nlayout = layout;
                let mut nfuncs = vec![];
                for f in functions.iter() {
                    let (nf, no) = CompilerNode::from(f, nlayout);
                    nlayout = no;
                    nfuncs.push(nf);
                }

                let mut ncors = vec![];
                for co in coroutines.iter() {
                    let (nco, no) = CompilerNode::from(co, nlayout);
                    nlayout = no;
                    ncors.push(nco);
                }

                let mut nstructs = vec![];
                for st in structs.iter() {
                    let (nst, no) = CompilerNode::from(st, nlayout);
                    nlayout = no;
                    nstructs.push(nst);
                }

                (
                    Module {
                        meta,
                        functions: nfuncs,
                        coroutines: ncors,
                        structs: nstructs,
                    },
                    nlayout,
                )
            }
            StructDef(..) => panic!("StructDef Unimplemented"),
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use crate::compiler::ast::scope;
    use crate::compiler::ast::scope::SymbolTable;
    use crate::semantics::symbol_table;
    use crate::syntax::ast::Type;
    use crate::syntax::ast::BinaryOperator;
    use crate::syntax::ast::RoutineDef;
    use crate::{semantics::semanticnode::SemanticMetadata, semantics::semanticnode::SemanticNode};

    use super::*;

    #[test]
    pub fn test_integer() {
        let sn = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: Type::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            0,
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(8));
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::Integer(m, v) => {
                assert_eq!(v, 0);
                assert_eq!(m, Scope::new(scope::Level::Block));
            }
            _ => assert_eq!(true, false),
        }
    }

    #[test]
    pub fn test_operator() {
        let sn1 = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            1,
        );
        let sn2 = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            2,
        );
        let snmul = SemanticNode::BinaryOp(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            BinaryOperator::Mul,
            Box::new(sn1),
            Box::new(sn2),
        );
        let cn = CompilerNode::from(&snmul, LayoutData::new(8));
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::BinaryOp(m, BinaryOperator::Mul, l, r) => {
                assert_eq!(
                    m,
                    Scope {
                        ty: scope::Level::Block,
                        symbols: SymbolTable::new(),
                        label: 2
                    }
                );

                match *l {
                    CompilerNode::Integer(m, v) => {
                        assert_eq!(m.label, 0);
                        assert_eq!(v, 1);
                    }
                    _ => assert!(false),
                }
                match *r {
                    CompilerNode::Integer(m, v) => {
                        assert_eq!(m.label, 1);
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
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: semantic_table,
            },
            vec![],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::ExpressionBlock(m, _) => {
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
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: semantic_table,
            },
            vec![],
        );

        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: semantic_table,
            },
            vec![sn],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 16);
        match cn.0 {
            CompilerNode::ExpressionBlock(m, b) => {
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);
                match b.iter().nth(0) {
                    Some(CompilerNode::ExpressionBlock(m, _)) => {
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
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::RoutineDef(
            SemanticMetadata {
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
            },
            RoutineDef::Function,
            "func".into(),
            vec![],
            Type::I32,
            vec![],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::RoutineDef(m, RoutineDef::Function, name, ..) => {
                assert_eq!(name, "func");
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);

                match m.ty {
                    scope::Level::Routine {
                        next_label,
                        allocation,
                    } => {
                        assert_eq!(next_label, 0);
                        assert_eq!(allocation, 8);
                    }
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_nested_function() {
        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::RoutineDef(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: semantic_table,
            },
            RoutineDef::Function,
            "func".into(),
            vec![],
            crate::syntax::ast::Type::I32,
            vec![],
        );

        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::RoutineDef(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Type::I32,
                sym: semantic_table,
            },
            RoutineDef::Function,
            "outer_func".into(),
            vec![],
            crate::syntax::ast::Type::I32,
            vec![sn],
        );

        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::RoutineDef(m, RoutineDef::Function, .., body) => {
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);

                match body.iter().nth(0) {
                    Some(CompilerNode::RoutineDef(m, RoutineDef::Function, ..)) => {
                        assert_eq!(m.symbols.table.len(), 2);
                        assert_eq!(m.symbols.table["x"].size, 4);
                        assert_eq!(m.symbols.table["x"].offset, 4);
                        assert_eq!(m.symbols.table["y"].size, 4);
                        assert_eq!(m.symbols.table["y"].offset, 8);
                    }
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_coroutine() {
        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", Type::I32, false)
            .unwrap();
        semantic_table
            .add("y", Type::I32, false)
            .unwrap();
        let sn = SemanticNode::RoutineDef(
            SemanticMetadata {
                ln: 0,
                ty: Type::I32,
                sym: semantic_table,
            },
            RoutineDef::Coroutine,
            "coroutine".into(),
            vec![],
            Type::I32,
            vec![],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::RoutineDef(m, RoutineDef::Coroutine, name, _, _, _) => {
                assert_eq!(name, "coroutine");
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 24);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 28);

                match m.ty {
                    scope::Level::Routine { allocation, .. } => assert_eq!(allocation, 28),
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }
}
