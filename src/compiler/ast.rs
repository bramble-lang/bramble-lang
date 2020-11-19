use std::collections::HashMap;

use crate::{
    semantics, semantics::semanticnode::SemanticMetadata, semantics::semanticnode::SemanticNode,
    syntax::ast::Ast,
};

pub type CompilerNode = Ast<Scope>;

impl CompilerNode {
    pub fn from(ast: &SemanticNode, offset: i32) -> (CompilerNode, i32) {
        use Ast::*;
        match ast {
            ExpressionBlock(m, body) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let mut nbody = vec![];
                let mut noffset = offset;
                for e in body.iter() {
                    let (e, offset) = CompilerNode::from(e, noffset);
                    noffset = offset;
                    nbody.push(e);
                }
                (ExpressionBlock(meta, nbody), noffset)
            }
            FunctionDef(m, name, params, ret_ty, body) => {
                let (mut meta, offset2) = Scope::routine_from(m, 0);
                let mut nbody = vec![];
                let mut noffset = offset2;
                for e in body.iter() {
                    let (e, offset) = CompilerNode::from(e, noffset);
                    noffset = offset;
                    nbody.push(e);
                }
                meta.ty = Type::Routine{next_label: 0, allocation: noffset};
                (
                    FunctionDef(meta, name.clone(), params.clone(), *ret_ty, nbody),
                    offset,
                )
            }
            CoroutineDef(m, name, params, ret_ty, body) => {
                let (mut meta, offset2) = Scope::routine_from(m, 0);
                let mut nbody = vec![];
                let mut noffset = offset2;
                for e in body.iter() {
                    let (e, offset) = CompilerNode::from(e, noffset);
                    noffset = offset;
                    nbody.push(e);
                }
                meta.ty = Type::Routine{next_label: 0, allocation: noffset};
                (
                    CoroutineDef(meta, name.clone(), params.clone(), *ret_ty, nbody),
                    offset,
                )
            }
            Ast::Integer(m, i) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Integer(meta, *i), offset)
            }
            Ast::Boolean(m, b) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Boolean(meta, *b), offset)
            }
            Ast::Identifier(m, id) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Identifier(meta, id.clone()), offset)
            }
            Ast::IdentifierDeclare(m, id, p) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::IdentifierDeclare(meta, id.clone(), *p), offset)
            }
            Mul(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Mul(meta, Box::new(l), Box::new(r)), offset)
            }
            Add(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Add(meta, Box::new(l), Box::new(r)), offset)
            }
            BAnd(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::BAnd(meta, Box::new(l), Box::new(r)), offset)
            }
            BOr(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::BOr(meta, Box::new(l), Box::new(r)), offset)
            }
            Eq(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ast::Eq(meta, Box::new(l), Box::new(r)), offset)
            }
            NEq(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (NEq(meta, Box::new(l), Box::new(r)), offset)
            }
            Ls(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Ls(meta, Box::new(l), Box::new(r)), offset)
            }
            LsEq(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (LsEq(meta, Box::new(l), Box::new(r)), offset)
            }
            Gr(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (Gr(meta, Box::new(l), Box::new(r)), offset)
            }
            GrEq(m, ref l, ref r) => {
                let (l, offset) = CompilerNode::from(l, offset);
                let (r, offset) = CompilerNode::from(r, offset);
                let (meta, offset) = Scope::block_from(m, offset);
                (GrEq(meta, Box::new(l), Box::new(r)), offset)
            }
            Printi(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Printi(meta, Box::new(e)), offset)
            }
            Printiln(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Printiln(meta, Box::new(e)), offset)
            }
            Printbln(m, ref e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Printbln(meta, Box::new(e)), offset)
            }
            If(m, ref cond, ref tb, ref fb) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (cond, offset) = CompilerNode::from(cond, offset);
                let (tb, offset) = CompilerNode::from(tb, offset);
                let (fb, offset) = CompilerNode::from(fb, offset);
                (If(meta, Box::new(cond), Box::new(tb), Box::new(fb)), offset)
            }
            Bind(m, id, p, e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Bind(meta, id.clone(), *p, Box::new(e)), offset)
            }
            Yield(m, e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Yield(meta, Box::new(e)), offset)
            }
            Return(m, None) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (Return(meta, None), offset)
            }
            Return(m, Some(e)) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Return(meta, Some(Box::new(e))), offset)
            }
            YieldReturn(m, None) => {
                let (meta, offset) = Scope::block_from(m, offset);
                (YieldReturn(meta, None), offset)
            }
            YieldReturn(m, Some(e)) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (YieldReturn(meta, Some(Box::new(e))), offset)
            }
            Statement(m, e) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let (e, offset) = CompilerNode::from(e, offset);
                (Statement(meta, Box::new(e)), offset)
            }
            FunctionCall(m, name, params) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let mut noffset = offset;
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, poffset) = CompilerNode::from(p, noffset);
                    noffset = poffset;
                    nparams.push(np);
                }
                (FunctionCall(meta, name.clone(), nparams), noffset)
            }
            CoroutineInit(m, name, params) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let mut noffset = offset;
                let mut nparams = vec![];
                for p in params.iter() {
                    let (np, poffset) = CompilerNode::from(p, noffset);
                    noffset = poffset;
                    nparams.push(np);
                }
                (CoroutineInit(meta, name.clone(), nparams), noffset)
            }
            Module(m, funcs, cors) => {
                let (meta, offset) = Scope::block_from(m, offset);
                let mut offset = offset;
                let mut nfuncs = vec![];
                for f in funcs.iter() {
                    let (nf, no) = CompilerNode::from(f, offset);
                    offset = no;
                    nfuncs.push(nf);
                }

                let mut ncors = vec![];
                for co in cors.iter() {
                    let (nco, no) = CompilerNode::from(co, offset);
                    offset = no;
                    ncors.push(nco);
                }

                (Module(meta, nfuncs, ncors), offset)
            }
        }
    }
}

#[cfg(test)]
mod ast_tests {
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
        let cn = CompilerNode::from(&sn, 8);
        assert_eq!(cn.1, 8);
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
        let cn = CompilerNode::from(&snmul, 8);
        assert_eq!(cn.1, 8);
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
        let cn = CompilerNode::from(&sn, 0);
        assert_eq!(cn.1, 8);
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
        let cn = CompilerNode::from(&sn, 0);
        assert_eq!(cn.1, 16);
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
        let cn = CompilerNode::from(&sn, 0);
        assert_eq!(cn.1, 0);
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

        let cn = CompilerNode::from(&sn, 0);
        assert_eq!(cn.1, 0);
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
        let cn = CompilerNode::from(&sn, 0);
        assert_eq!(cn.1, 0);
        match cn.0 {
            CompilerNode::CoroutineDef(m, name, _, _, _) => {
                assert_eq!(name, "coroutine");
                assert_eq!(m.symbols.table.len(), 2);
                assert_eq!(m.symbols.table["x"].size, 4);
                assert_eq!(m.symbols.table["x"].offset, 4);
                assert_eq!(m.symbols.table["y"].size, 4);
                assert_eq!(m.symbols.table["y"].offset, 8);
            }
            _ => assert!(false),
        }
    }
}

pub struct ScopeStack<'a> {
    stack: Vec<&'a Scope>,
}

impl<'a> ScopeStack<'a> {
    pub fn new() -> ScopeStack<'a> {
        ScopeStack { stack: vec![] }
    }

    /// Push a new scope onto the stack.
    pub fn push(&mut self, scope: &'a Scope) {
        self.stack.push(scope);
    }

    /// Pop the current scope off of the stack
    pub fn pop(&mut self) -> Option<&'a Scope> {
        self.stack.pop()
    }

    /// Searches through the stack, starting at the top and going to the bottom, for a
    /// variable with the given name.  This will not search past the root scope of the
    /// current function.
    pub fn find(&self, name: &str) -> Option<&'a Symbol> {
        for scope in self.stack.iter().rev() {
            let t = scope.get(name);
            if t.is_some() {
                return t;
            }
            match scope.ty {
                Type::Block => (),
                Type::Routine { .. } => return None,
            }
        }

        None
    }
}

#[derive(Debug, PartialEq)]
pub struct Scope {
    ty: Type,
    symbols: SymbolTable,
}

impl Scope {
    pub fn new(ty: Type) -> Scope {
        Scope {
            ty,
            symbols: SymbolTable::new(),
        }
    }

    pub fn insert(&mut self, name: &str, size: i32, offset: i32) -> i32 {
        self.symbols
            .table
            .insert(name.into(), Symbol::new(name, size, offset + size));
        offset + size
    }

    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.table.get(name)
    }

    pub fn block_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Type::Block);
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        (scope, current_offset)
    }

    pub fn routine_from(m: &SemanticMetadata, current_offset: i32) -> (Scope, i32) {
        let mut scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 0,
        });
        let mut current_offset = current_offset;
        for s in m.sym.table().iter() {
            current_offset = scope.insert(&s.name, s.ty.size(), current_offset);
        }
        match scope.ty {
            Type::Routine {
                ref mut allocation, ..
            } => *allocation = current_offset,
            _ => (),
        };
        (scope, current_offset)
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Block,
    Routine { next_label: i32, allocation: i32 },
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub size: i32,
    pub offset: i32,
}

impl Symbol {
    pub fn new(name: &str, size: i32, offset: i32) -> Symbol {
        Symbol {
            name: name.into(),
            size,
            offset,
        }
    }
}

impl semantics::symbol_table::Type {
    pub fn size(&self) -> i32 {
        match self {
            semantics::symbol_table::Type::Primitive(crate::syntax::ast::Primitive::I32) => 4,
            semantics::symbol_table::Type::Primitive(crate::syntax::ast::Primitive::Bool) => 4,
            _ => 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_symbol_in_current_scope() {
        let mut scope = Scope::new(Type::Block);
        scope.insert("x", 4, 4);
        let mut stack = ScopeStack::new();
        stack.push(&scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_in_outer_scope() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        let inner_scope = Scope::new(Type::Block);
        stack.push(&inner_scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 8);
    }

    #[test]
    fn test_find_symbol_defined_in_both_scopes() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 16);
        stack.push(&inner_scope);

        let sym = stack.find("x").unwrap();
        assert_eq!(sym.name, "x");
        assert_eq!(sym.size, 4);
        assert_eq!(sym.offset, 20);
    }

    #[test]
    fn test_find_symbol_does_not_exist() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("x", 4, 4);
        stack.push(&outer_scope);
        let inner_scope = Scope::new(Type::Block);
        stack.push(&inner_scope);

        assert_eq!(stack.find("y").is_none(), true);
    }

    #[test]
    fn test_find_symbol_does_not_pass_function() {
        let mut stack = ScopeStack::new();

        let mut outer_scope = Scope::new(Type::Block);
        outer_scope.insert("nope", 4, 4);
        stack.push(&outer_scope);

        let mut fun_scope = Scope::new(Type::Routine {
            next_label: 0,
            allocation: 8,
        });
        fun_scope.insert("y", 4, 4);
        fun_scope.insert("z", 4, 8);
        stack.push(&fun_scope);

        let mut inner_scope = Scope::new(Type::Block);
        inner_scope.insert("x", 4, 4);
        stack.push(&inner_scope);

        assert_eq!(stack.find("x").is_some(), true);
        assert_eq!(stack.find("y").is_some(), true);
        assert_eq!(stack.find("nope").is_some(), false);
    }
}
