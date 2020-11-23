use crate::compiler::ast::scope::{Scope, Type};

use crate::{
    semantics::semanticnode::SemanticNode,
    syntax::ast::Ast,
};

pub type CompilerNode = Ast<Scope>;

#[derive(Debug, PartialEq)]
pub struct LayoutData {
    offset: i32,
}

impl LayoutData {
    pub fn new(offset:i32) -> LayoutData {
        LayoutData{
            offset,
        }
    }
}

impl CompilerNode {
    pub fn from(ast: &SemanticNode, layout: LayoutData) -> (CompilerNode, LayoutData) {
        use Ast::*;
        match ast {
            ExpressionBlock(m, body) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let mut nbody = vec![];
                let mut nlayout = LayoutData{offset};
                for e in body.iter() {
                    let (e, layout) = CompilerNode::from(e, nlayout);
                    nlayout = layout;
                    nbody.push(e);
                }
                (ExpressionBlock(meta, nbody), nlayout)
            }
            FunctionDef(m, name, params, ret_ty, body) => {
                let (mut meta, offset2) = Scope::routine_from(m, 0);
                let mut nbody = vec![];
                let mut nlayout = LayoutData::new(offset2);
                for e in body.iter() {
                    let (e, layout) = CompilerNode::from(e, nlayout);
                    nlayout = layout;
                    nbody.push(e);
                }
                meta.ty = Type::Routine{next_label: 0, allocation: nlayout.offset};
                (
                    FunctionDef(meta, name.clone(), params.clone(), *ret_ty, nbody),
                    layout,
                )
            }
            CoroutineDef(m, name, params, ret_ty, body) => {
                let coroutine_metadata_size = 20;
                let (mut meta, offset2) = Scope::routine_from(m, coroutine_metadata_size);
                let mut nbody = vec![];
                let mut nlayout = LayoutData::new(offset2);
                for e in body.iter() {
                    let (e, layout) = CompilerNode::from(e, nlayout);
                    nlayout = layout;
                    nbody.push(e);
                }
                meta.ty = Type::Routine{next_label: 0, allocation: nlayout.offset};
                (
                    CoroutineDef(meta, name.clone(), params.clone(), *ret_ty, nbody),
                    layout,
                )
            }
            Ast::Integer(m, i) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Integer(meta, *i), LayoutData::new(offset))
            }
            Ast::Boolean(m, b) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Boolean(meta, *b), LayoutData::new(offset))
            }
            Ast::Identifier(m, id) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Identifier(meta, id.clone()), LayoutData::new(offset))
            }
            Ast::IdentifierDeclare(m, id, p) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::IdentifierDeclare(meta, id.clone(), *p), LayoutData::new(offset))
            }
            Mul(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Mul(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            Add(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Add(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            BAnd(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::BAnd(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            BOr(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::BOr(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            Eq(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ast::Eq(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            NEq(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (NEq(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            Ls(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Ls(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            LsEq(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (LsEq(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            Gr(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Gr(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            GrEq(m, ref l, ref r) => {
                let (l, layout) = CompilerNode::from(l, layout);
                let (r, layout) = CompilerNode::from(r, layout);
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (GrEq(meta, Box::new(l), Box::new(r)), LayoutData::new(offset))
            }
            Printi(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Printi(meta, Box::new(e)), layout)
            }
            Printiln(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Printiln(meta, Box::new(e)), layout)
            }
            Printbln(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Printbln(meta, Box::new(e)), layout)
            }
            If(m, ref cond, ref tb, ref fb) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (cond, layout) = CompilerNode::from(cond, LayoutData::new(offset));
                let (tb, layout) = CompilerNode::from(tb, layout);
                let (fb, layout) = CompilerNode::from(fb, layout);
                (If(meta, Box::new(cond), Box::new(tb), Box::new(fb)), layout)
            }
            Bind(m, id, p, e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Bind(meta, id.clone(), *p, Box::new(e)), layout)
            }
            Yield(m, e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Yield(meta, Box::new(e)), layout)
            }
            Return(m, None) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (Return(meta, None), LayoutData::new(offset))
            }
            Return(m, Some(e)) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Return(meta, Some(Box::new(e))), layout)
            }
            YieldReturn(m, None) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                (YieldReturn(meta, None), LayoutData::new(offset))
            }
            YieldReturn(m, Some(e)) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (YieldReturn(meta, Some(Box::new(e))), layout)
            }
            Statement(m, e) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let (e, layout) = CompilerNode::from(e, LayoutData::new(offset));
                (Statement(meta, Box::new(e)), layout)
            }
            FunctionCall(m, name, params) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let mut nlayout = LayoutData::new(offset);
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, playout) = CompilerNode::from(p, nlayout);
                    nlayout = playout;
                    nparams.push(np);
                }
                (FunctionCall(meta, name.clone(), nparams), nlayout)
            }
            CoroutineInit(m, name, params) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let mut nlayout = LayoutData::new(offset);
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, playout) = CompilerNode::from(p, nlayout);
                    nlayout = playout;
                    nparams.push(np);
                }
                (CoroutineInit(meta, name.clone(), nparams), nlayout)
            }
            Module(m, funcs, cors) => {
                let (meta, offset) = Scope::block_from(m, layout.offset);
                let mut nlayout = LayoutData::new(offset);
                let mut nfuncs = vec![];
                for f in funcs.iter() {
                    let (nf, no) = CompilerNode::from(f, nlayout);
                    nlayout = no;
                    nfuncs.push(nf);
                }

                let mut ncors = vec![];
                for co in cors.iter() {
                    let (nco, no) = CompilerNode::from(co, nlayout);
                    nlayout = no;
                    ncors.push(nco);
                }

                (Module(meta, nfuncs, ncors), nlayout)
            }
        }
    }
}

#[cfg(test)]
mod ast_tests {
    use crate::{
        semantics::semanticnode::SemanticMetadata, semantics::semanticnode::SemanticNode,
    };
    use crate::semantics::symbol_table;
    use crate::syntax::ast::Primitive;

    use super::*;

    #[test]
    pub fn test_integer() {
        let sn = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: Primitive::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            0,
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(8));
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::Integer(m, v) => {
                assert_eq!(v, 0);
                assert_eq!(m, Scope::new(Type::Block));
            }
            _ => assert_eq!(true, false),
        }
    }

    #[test]
    pub fn test_operator() {
        let sn1 = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            1,
        );
        let sn2 = SemanticNode::Integer(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            2,
        );
        let snmul = SemanticNode::Mul(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: symbol_table::SymbolTable::new(),
            },
            Box::new(sn1),
            Box::new(sn2),
        );
        let cn = CompilerNode::from(&snmul, LayoutData::new(8));
        assert_eq!(cn.1.offset, 8);
        match cn.0 {
            CompilerNode::Mul(m, l, r) => {
                assert_eq!(m, Scope::new(Type::Block));

                match *l {
                    CompilerNode::Integer(_, v) => assert_eq!(v, 1),
                    _ => assert!(false),
                }
                match *r {
                    CompilerNode::Integer(_, v) => assert_eq!(v, 2),
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
            .add(
                "x",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        semantic_table
            .add(
                "y",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
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
            .add(
                "x",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        semantic_table
            .add(
                "y",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: semantic_table,
            },
            vec![],
        );

        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add(
                "x",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        semantic_table
            .add(
                "y",
                symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32),
            )
            .unwrap();
        let sn = SemanticNode::ExpressionBlock(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
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
            .add("x", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        semantic_table
            .add("y", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        let sn = SemanticNode::FunctionDef(
            SemanticMetadata {
                ln: 0,
                ty: Primitive::I32,
                sym: semantic_table,
            },
            "func".into(),
            vec![],
            crate::syntax::ast::Primitive::I32,
            vec![],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::FunctionDef(m, name, ..) => {
                assert_eq!(name, "func");
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);

                match m.ty {
                    Type::Routine{next_label, allocation} => {
                        assert_eq!(next_label, 0);
                        assert_eq!(allocation, 8);
                    }
                    _ => assert!(false)
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    pub fn test_nested_function() {
        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        semantic_table
            .add("y", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        let sn = SemanticNode::FunctionDef(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: semantic_table,
            },
            "func".into(),
            vec![],
            crate::syntax::ast::Primitive::I32,
            vec![],
        );

        let mut semantic_table = symbol_table::SymbolTable::new();
        semantic_table
            .add("x", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        semantic_table
            .add("y", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        let sn = SemanticNode::FunctionDef(
            SemanticMetadata {
                ln: 0,
                ty: crate::syntax::ast::Primitive::I32,
                sym: semantic_table,
            },
            "outer_func".into(),
            vec![],
            crate::syntax::ast::Primitive::I32,
            vec![sn],
        );

        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::FunctionDef(m, .., body) => {
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);

                match body.iter().nth(0) {
                    Some(CompilerNode::FunctionDef(m, ..)) => {
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
            .add("x", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        semantic_table
            .add("y", symbol_table::Type::Primitive(Primitive::I32))
            .unwrap();
        let sn = SemanticNode::CoroutineDef(
            SemanticMetadata {
                ln: 0,
                ty: Primitive::I32,
                sym: semantic_table,
            },
            "coroutine".into(),
            vec![],
            Primitive::I32,
            vec![],
        );
        let cn = CompilerNode::from(&sn, LayoutData::new(0));
        assert_eq!(cn.1.offset, 0);
        match cn.0 {
            CompilerNode::CoroutineDef(m, name, _, _, _) => {
                assert_eq!(name, "coroutine");
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 24);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 28);

                match m.ty {
                    Type::Routine{allocation,..} => {
                        assert_eq!(allocation, 28)
                    }
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }
}