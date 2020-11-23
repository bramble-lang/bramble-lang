pub mod checker {
    use crate::ast::*;
    use crate::parser::PNode;
    use crate::semantics::semanticnode::SemanticNode;
    use crate::semantics::symbol_table::*;
    use Primitive::*;

    pub fn type_check(ast: &PNode) -> Result<Box<SemanticNode>, String> {
        let mut sm_ast = SemanticNode::from_parser_ast(&ast)?;
        SymbolTable::generate(&mut sm_ast)?;

        let mut root_table = SymbolTable::new();
        let mut semantic = SemanticAnalyzer::new();
        semantic
            .traverse(&mut sm_ast, &None, &mut root_table)
            .map_err(|e| format!("Semantic: {}", e))?;
        Ok(sm_ast)
    }

    pub struct SemanticAnalyzer {
        stack: ScopeStack,
    }

    impl SemanticAnalyzer {
        pub fn new() -> SemanticAnalyzer {
            SemanticAnalyzer {
                stack: ScopeStack::new(),
            }
        }

        fn binary_op(
            &mut self,
            op: String,
            ln: u32,
            l: &mut SemanticNode,
            r: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
            expected: Option<Primitive>,
        ) -> Result<Primitive, String> {
            let lty = self.traverse(l, current_func, sym)?;
            let rty = self.traverse(r, current_func, sym)?;

            match expected {
                None => {
                    if lty == rty {
                        Ok(lty)
                    } else {
                        Err(format!(
                            "L{}: {} expected {} but found {}",
                            ln, op, lty, rty
                        ))
                    }
                }
                Some(expected) => {
                    if lty == expected && rty == expected {
                        Ok(lty)
                    } else {
                        Err(format!("L{}: {} expected operands of {}", ln, op, expected))
                    }
                }
            }
        }

        pub fn traverse(
            &mut self,
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<Primitive, String> {
            self.analyize_node(ast, current_func, sym)
        }

        fn lookup<'a>(&'a self, sym: &'a SymbolTable, id: &str) -> Option<&'a Symbol> {
            sym.get(id).or(self.stack.get(id))
        }

        fn lookup_func_or_cor<'a>(
            &'a self,
            sym: &'a SymbolTable,
            id: &str,
        ) -> Result<(&Vec<Primitive>, &Primitive), String> {
            match self.lookup(sym, id) {
                Some(Symbol {
                    ty: Type::Coroutine(params, p),
                    ..
                })
                | Some(Symbol {
                    ty: Type::Function(params, p),
                    ..
                }) => Ok((params, p)),
                Some(_) => return Err(format!("{} is not a function or coroutine", id)),
                None => return Err(format!("label {} not found in symbol table", id)),
            }
        }

        fn lookup_coroutine<'a>(
            &'a self,
            sym: &'a SymbolTable,
            id: &str,
        ) -> Result<(&Vec<Primitive>, &Primitive), String> {
            match self.lookup(sym, id) {
                Some(Symbol {
                    ty: Type::Coroutine(params, p),
                    ..
                }) => Ok((params, p)),
                Some(_) => return Err(format!("{} is not a coroutine", id)),
                None => return Err(format!("label {} not found in symbol table", id)),
            }
        }

        fn lookup_var<'a>(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&Primitive, String> {
            match self.lookup(sym, id) {
                Some(Symbol {
                    ty: Type::Primitive(p),
                    ..
                }) => Ok(p),
                Some(_) => return Err(format!("{} is not a variable", id)),
                None => return Err(format!("label {} not found in symbol table", id)),
            }
        }

        fn analyize_node(
            &mut self,
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<Primitive, String> {
            use Ast::*;
            let root_str = ast.root_str();
            match ast {
                Integer(meta, _) => {
                    meta.ty = I32;
                    Ok(I32)
                }
                Boolean(meta, _) => {
                    meta.ty = Bool;
                    Ok(Bool)
                }
                IdentifierDeclare(meta, _, p) => {
                    meta.ty = *p;
                    Ok(*p)
                }
                Identifier(meta, id) => match current_func {
                    None => Err(format!(
                        "L{}: Variable {} appears outside of function",
                        meta.ln, id
                    )),
                    Some(_) => {
                        match self.lookup(sym, id) {
                            Some(Symbol {
                                ty: Type::Primitive(p),
                                ..
                            }) => meta.ty = *p,
                            _ => return Err(format!("L{}: Variable {} not declared", meta.ln, id)),
                        };
                        Ok(meta.ty)
                    }
                },
                BinaryOp(meta, op, l, r) => {
                    let expected_ty = match op {
                        BinaryOperator::Mul | BinaryOperator::Add => Some(I32),
                        BinaryOperator::BAnd | BinaryOperator::BOr => Some(Bool),
                        BinaryOperator::Eq | BinaryOperator::NEq | BinaryOperator::Ls | BinaryOperator::LsEq 
                        | BinaryOperator::Gr | BinaryOperator::GrEq => None,
                    };
                    let ty =
                        self.binary_op(root_str, meta.ln, l, r, current_func, sym, expected_ty)?;
                    meta.ty = ty;
                    Ok(ty)
                }
                Mul(meta, l, r) | Add(meta, l, r) => {
                    let ty =
                        self.binary_op(root_str, meta.ln, l, r, current_func, sym, Some(I32))?;
                    meta.ty = ty;
                    Ok(ty)
                }
                BAnd(meta, l, r) | BOr(meta, l, r) => {
                    let ty =
                        self.binary_op(root_str, meta.ln, l, r, current_func, sym, Some(Bool))?;
                    meta.ty = ty;
                    Ok(ty)
                }
                Eq(meta, l, r)
                | NEq(meta, l, r)
                | Gr(meta, l, r)
                | GrEq(meta, l, r)
                | Ls(meta, l, r)
                | LsEq(meta, l, r) => {
                    self.binary_op(root_str, meta.ln, l, r, current_func, sym, None)?;
                    meta.ty = Bool;
                    Ok(meta.ty)
                }
                If(meta, cond, true_arm, false_arm) => {
                    let cond_ty = self.traverse(cond, current_func, sym)?;
                    if cond_ty == Bool {
                        let true_arm = self.traverse(true_arm, current_func, sym)?;
                        let false_arm = self.traverse(false_arm, current_func, sym)?;
                        if true_arm == false_arm {
                            meta.ty = true_arm;
                            Ok(true_arm)
                        } else {
                            Err(format!(
                                "L{}: If expression has mismatching arms: expected {} got {}",
                                meta.ln, true_arm, false_arm
                            ))
                        }
                    } else {
                        Err(format!(
                            "L{}: Expected boolean expression in if conditional, got: {}",
                            meta.ln, cond_ty
                        ))
                    }
                }
                Bind(meta, name, p, exp) => match current_func {
                    Some(_) => {
                        let rhs = self.traverse(exp, current_func, sym)?;
                        if *p == rhs {
                            /*ftable
                            .add_var(cf, name, *p)
                            .map_err(|e| format!("L{}: {}", meta.ln, e))?;*/

                            sym.add(name, Type::Primitive(*p))
                                .map_err(|e| format!("L{}: {}", meta.ln, e))?;
                            meta.ty = *p;
                            Ok(rhs)
                        } else {
                            Err(format!("L{}: Bind expected {} but got {}", meta.ln, p, rhs))
                        }
                    }
                    None => Err(format!(
                        "L{}: Attempting to bind variable {} outside of function",
                        meta.ln, name
                    )),
                },
                Return(meta, None) => match current_func {
                    None => Err(format!("L{}: Return called outside of a function", meta.ln)),
                    Some(cf) => {
                        let (_, fty) = self
                            .lookup_func_or_cor(sym, cf)
                            .map_err(|e| format!("L{}: {}", meta.ln, e))?;
                        if *fty == Unit {
                            meta.ty = Unit;
                            Ok(Unit)
                        } else {
                            Err(format!(
                                "L{}: Return expected {} type and got unit",
                                meta.ln, fty
                            ))
                        }
                    }
                },
                Return(meta, Some(exp)) => match current_func {
                    None => Err(format!(
                        "L{}: Return appears outside of a function",
                        meta.ln
                    )),
                    Some(cf) => {
                        let val = self.traverse(exp, current_func, sym)?;
                        let (_, fty) = self
                            .lookup_func_or_cor(sym, cf)
                            .map_err(|e| format!("L{}: {}", meta.ln, e))?;
                        if *fty == val {
                            meta.ty = *fty;
                            Ok(meta.ty)
                        } else {
                            Err(format!(
                                "L{}: Return expected {} but got {}",
                                meta.ln, fty, val
                            ))
                        }
                    }
                },
                Yield(meta, exp) => match current_func {
                    None => Err(format!("L{}: Yield appears outside of function", meta.ln)),
                    Some(_) => match exp.as_ref() {
                        Ast::Identifier(id_meta, coname) => {
                            let ty = self
                                .lookup_var(sym, coname)
                                .map_err(|e| format!("L{}: {}", id_meta.ln, e))?;
                            meta.ty = *ty;
                            Ok(meta.ty)
                        }
                        _ => Err(format!(
                            "L{}: Expected name of coroutine after yield",
                            meta.ln
                        )),
                    },
                },
                YieldReturn(meta, None) => match current_func {
                    None => Err(format!("L{}: YRet appears outside of function", meta.ln)),
                    Some(cf) => {
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        if *ret_ty == Unit {
                            meta.ty = Unit;
                            Ok(Unit)
                        } else {
                            Err(format!(
                                "L{}: Yield return expected {} but got unit",
                                meta.ln, ret_ty
                            ))
                        }
                    }
                },
                YieldReturn(meta, Some(exp)) => match current_func {
                    None => Err(format!("L{}: YRet appears outside of function", meta.ln)),
                    Some(cf) => {
                        let val = self.traverse(exp, current_func, sym)?;
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        if *ret_ty == val {
                            meta.ty = *ret_ty;
                            Ok(meta.ty)
                        } else {
                            Err(format!(
                                "L{}: Yield return expected {} but got {}",
                                meta.ln, ret_ty, val
                            ))
                        }
                    }
                },
                Statement(meta, stmt) => {
                    self.traverse(stmt, current_func, sym)?;
                    meta.ty = Unit;
                    Ok(Unit)
                }
                FunctionCall(meta, fname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut pty = vec![];
                    for param in params.iter_mut() {
                        let ty = self.traverse(param, current_func, sym)?;
                        pty.push(ty);
                    }
                    let (expected_tys, ret_ty) = match sym.get(fname).or(self.stack.get(fname)) {
                        Some(Symbol {
                            ty: Type::Function(pty, rty),
                            ..
                        }) => (pty, rty),
                        Some(_) => {
                            return Err(format!(
                                "L{}: {} found but was not a function",
                                meta.ln, fname
                            ))
                        }
                        None => {
                            return Err(format!("L{}: function {} not declared", meta.ln, fname))
                        }
                    };

                    if pty.len() != expected_tys.len() {
                        Err(format!(
                            "L{}: Incorrect number of parameters passed to function: {}",
                            meta.ln, fname
                        ))
                    } else {
                        let z = pty.iter().zip(expected_tys.iter());
                        let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                        if all_params_match {
                            //let fty = ftable.funcs[fname].ty;
                            meta.ty = *ret_ty;
                            Ok(*ret_ty)
                        } else {
                            Err(format!(
                                "L{}: One or more parameters had mismatching types for function {}",
                                meta.ln, fname
                            ))
                        }
                    }
                }
                CoroutineInit(meta, coname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut pty = vec![];
                    for param in params.iter_mut() {
                        let ty = self.traverse(param, current_func, sym)?;
                        pty.push(ty);
                    }
                    let (expected_tys, ret_ty) = match sym.get(coname).or(self.stack.get(coname)) {
                        Some(Symbol {
                            ty: Type::Coroutine(pty, rty),
                            ..
                        }) => (pty, rty),
                        Some(_) => {
                            return Err(format!(
                                "L{}: {} found but was not a function",
                                meta.ln, coname
                            ))
                        }
                        None => {
                            return Err(format!("L{}: function {} not declared", meta.ln, coname))
                        }
                    };

                    if pty.len() != expected_tys.len() {
                        Err(format!(
                            "L{}: Incorrect number of parameters passed to coroutine: {}",
                            meta.ln, coname
                        ))
                    } else {
                        let z = pty.iter().zip(expected_tys.iter());
                        let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                        if all_params_match {
                            meta.ty = *ret_ty;
                            Ok(*ret_ty)
                        } else {
                            Err(format!(
                                "L{}: Mismatching parameter types in init for coroutine {}",
                                meta.ln, coname
                            ))
                        }
                    }
                }
                Printi(meta, exp) => {
                    let ty = self.traverse(exp, current_func, sym)?;
                    if ty == I32 {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!("L{}: Expected i32 for printi got {}", meta.ln, ty))
                    }
                }
                Printiln(meta, exp) => {
                    let ty = self.traverse(exp, current_func, sym)?;
                    if ty == I32 {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!(
                            "L{}: Expected i32 for printiln got {}",
                            meta.ln, ty
                        ))
                    }
                }
                Printbln(meta, exp) => {
                    let ty = self.traverse(exp, current_func, sym)?;
                    if ty == Bool {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!(
                            "L{}: Expected i32 for printbln got {}",
                            meta.ln, ty
                        ))
                    }
                }

                ExpressionBlock(meta, body) => {
                    let mut ty = Unit;
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for stmt in body.iter_mut() {
                        ty = self.traverse(stmt, current_func, &mut meta.sym)?;
                    }
                    self.stack.pop();
                    meta.ty = ty;
                    Ok(ty)
                }
                FunctionDef(meta, name, params, p, body)
                | CoroutineDef(meta, name, params, p, body) => {
                    for (pname, pty) in params.iter() {
                        meta.sym.add(pname, Type::Primitive(*pty))?;
                    }
                    //meta.sym.add(name, Type::Function(params.iter().map(|(_, ty)| *ty).collect::<Vec<Primitive>>(), *p))?;
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for stmt in body.iter_mut() {
                        self.traverse(stmt, &Some(name.clone()), &mut meta.sym)?;
                    }
                    self.stack.pop();
                    Ok(*p)
                }
                Module(meta, funcs, cors) => {
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for func in funcs.iter_mut() {
                        self.traverse(func, &None, &mut meta.sym)?;
                    }
                    for cor in cors.iter_mut() {
                        self.traverse(cor, &None, &mut meta.sym)?;
                    }
                    self.stack.pop();
                    meta.ty = Unit;
                    Ok(Unit)
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::ast::{Ast, Primitive};
        use std::collections::HashMap;

        /*
         * map: function name -> (params, ret, vars)
         */
        struct FunInfo {
            params: Vec<(&'static str, Primitive)>,
            ret: Primitive,
            vars: Vec<(&'static str, Primitive)>,
        }

        impl FunInfo {
            pub fn new(
                params: Vec<(&'static str, Primitive)>,
                ret: Primitive,
                vars: Vec<(&'static str, Primitive)>,
            ) -> FunInfo {
                FunInfo {
                    params: params,
                    ret: ret,
                    vars: vars,
                }
            }
        }

        struct Scope {
            func: HashMap<String, FunInfo>,
        }

        impl Scope {
            pub fn new() -> Scope {
                Scope {
                    func: HashMap::new(),
                }
            }

            pub fn add(
                &mut self,
                name: &'static str,
                params: Vec<(&'static str, Primitive)>,
                ret: Primitive,
                vars: Vec<(&'static str, Primitive)>,
            ) {
                self.func
                    .insert(name.into(), FunInfo::new(params, ret, vars));
            }
        }

        fn start(
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            scope: &Scope,
        ) -> Result<Primitive, String> {
            let mut sym = SymbolTable::new();
            match current_func {
                Some(cf) => {
                    for (vname, vty) in scope
                        .func
                        .get(cf)
                        .ok_or(format!("could not find: {}", cf))?
                        .vars
                        .iter()
                    {
                        sym.add(&vname, Type::Primitive(*vty))?;
                    }
                }
                None => {}
            };

            // add functions to the symbol table: this is just a test
            for (f, fi) in scope.func.iter() {
                let params = fi
                    .params
                    .iter()
                    .map(|(_, pty)| *pty)
                    .collect::<Vec<Primitive>>();

                if f.starts_with("my_co") {
                    sym.add(f, Type::Coroutine(params, fi.ret))?;
                } else {
                    sym.add(f, Type::Function(params, fi.ret))?;
                }
            }

            let mut semantic = SemanticAnalyzer::new();
            semantic.traverse(ast, current_func, &mut sym)
        }

        #[test]
        pub fn test_integer() {
            let node = Ast::Integer(1, 5);
            let scope = Scope::new();

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &scope,
            );
            assert_eq!(ty, Ok(Primitive::I32));
        }

        #[test]
        pub fn test_identifier() {
            let mut scope = Scope::new();
            scope.add("my_main", vec![], Unit, vec![("x", Bool)]);

            let node = Ast::Identifier(1, "x".into());

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &scope,
            );
            assert_eq!(ty, Ok(Primitive::Bool));
        }

        #[test]
        pub fn test_add() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], Unit, vec![("x", I32), ("b", Bool)]);
            // both operands are i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
        }

        #[test]
        pub fn test_mul() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], Unit, vec![("x", I32), ("b", Bool)]);
            // both operands are i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
        }

        #[test]
        pub fn test_boolean_ops() {
            let scope = Scope::new();

            let tests: Vec<(PNode, Result<Primitive, String>)> = vec![(
                Ast::BAnd(
                    1,
                    Box::new(Ast::Boolean(1, true)),
                    Box::new(Ast::Integer(1, 5)),
                ),
                Err("L1: && expected operands of bool".into()),
            )];

            for (test, expected) in tests.iter() {
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&test).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_comparison_ops() {
            let scope = Scope::new();
            let tests: Vec<(PNode, Result<Primitive, String>)> = vec![
                (
                    Ast::Eq(
                        1,
                        Box::new(Ast::Integer(1, 3)),
                        Box::new(Ast::Integer(1, 5)),
                    ),
                    Ok(Bool),
                ),
                (
                    Ast::Eq(
                        1,
                        Box::new(Ast::Boolean(1, true)),
                        Box::new(Ast::Boolean(1, false)),
                    ),
                    Ok(Bool),
                ),
                (
                    Ast::Eq(
                        1,
                        Box::new(Ast::Integer(1, 3)),
                        Box::new(Ast::Boolean(1, true)),
                    ),
                    Err("L1: == expected i32 but found bool".into()),
                ),
            ];

            for (test, expected) in tests.iter() {
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&test).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_bind() {
            // RHS type matches the LHS type
            {
                let mut scope = Scope::new();
                scope.add("my_func", vec![], Unit, vec![]);

                let node = Ast::Bind(1, "x".into(), Primitive::I32, Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // RHS type does not match LHS type
            {
                let mut scope = Scope::new();
                scope.add("my_func", vec![], Unit, vec![]);

                let node = Ast::Bind(1, "x".into(), Primitive::Bool, Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: Bind expected bool but got i32".into()));
            }

            // recursive definition
            {
                let mut scope = Scope::new();
                scope.add("my_func", vec![], Unit, vec![]);

                let node = Ast::Bind(
                    1,
                    "x".into(),
                    Primitive::I32,
                    Box::new(Ast::Identifier(1, "x".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: Variable x not declared".into()));
            }

            // use an unbound variable
            {
                let mut scope = Scope::new();
                scope.add("my_func", vec![], Unit, vec![]);

                let node = Ast::Identifier(1, "x".into());
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: Variable x not declared".into()));
            }
        }

        #[test]
        pub fn test_return_unit() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], Unit, vec![]);

            let node = Ast::Return(1, None);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            );
            assert_eq!(ty, Ok(Unit));
        }

        #[test]
        pub fn test_return_i32() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], I32, vec![]);

            let node = Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))));
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));
        }

        #[test]
        pub fn test_fn_call() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], I32, vec![]);

            let node = Ast::FunctionCall(1, "my_func".into(), vec![]);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            scope.add("my_func2", vec![("x", I32)], I32, vec![]);
            // test correct parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![Ast::Integer(1, 5)]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &scope,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to function: my_func2".into())
            );
        }

        #[test]
        pub fn test_co_init() {
            let mut scope = Scope::new();
            scope.add("my_co", vec![], I32, vec![]);
            scope.add("my_co2", vec![("x", I32)], I32, vec![]);

            let node = Ast::CoroutineInit(1, "my_co".into(), vec![]);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            // test correct parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![Ast::Integer(1, 5)]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to coroutine: my_co2".into())
            );
        }

        #[test]
        pub fn test_yield_return() {
            let mut scope = Scope::new();
            scope.add("my_co", vec![], Unit, vec![]);
            scope.add("my_co2", vec![], I32, vec![]);

            let node = Ast::YieldReturn(1, None);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co".into()),
                &scope,
            );
            assert_eq!(ty, Ok(Unit));

            // test correct type for yield return
            let node = Ast::YieldReturn(1, Some(Box::new(Ast::Integer(1, 5))));
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Ast::YieldReturn(1, None);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(ty, Err("L1: Yield return expected i32 but got unit".into()));
        }

        #[test]
        fn test_yield() {
            let mut scope = Scope::new();
            scope.add("my_main", vec![], Unit, vec![("c", I32)]);
            scope.add("my_co2", vec![], I32, vec![]);

            let node = Ast::Yield(1, Box::new(Ast::Identifier(1, "c".into())));
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));
        }

        #[test]
        fn test_func_def() {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], I32, vec![]);

            let node = Ast::FunctionDef(
                1,
                "my_func".into(),
                vec![],
                I32,
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
            );

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::FunctionDef(1, "my_func".into(), vec![], I32, vec![Ast::Return(1, None)]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &scope,
            );
            assert_eq!(ty, Err("L1: Return expected i32 type and got unit".into()));
        }

        #[test]
        fn test_coroutine_def() {
            let mut scope = Scope::new();
            scope.add("my_co", vec![], I32, vec![]);

            let node = Ast::CoroutineDef(
                1,
                "my_co".into(),
                vec![],
                I32,
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
            );

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::CoroutineDef(1, "my_co".into(), vec![], I32, vec![Ast::Return(1, None)]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &scope,
            );
            assert_eq!(ty, Err("L1: Return expected i32 type and got unit".into()));
        }

        #[test]
        fn test_if_expression() {
            let mut scope = Scope::new();
            scope.add("my_main", vec![], I32, vec![("c", I32)]);

            for (c, t, f, ex) in vec![
                (
                    Ast::Boolean(1, true),
                    Ast::Integer(1, 5),
                    Ast::Integer(1, 7),
                    Ok(I32),
                ),
                (
                    Ast::Boolean(1, true),
                    Ast::Boolean(1, true),
                    Ast::Boolean(1, true),
                    Ok(Bool),
                ),
                (
                    Ast::Integer(1, 13),
                    Ast::Integer(1, 5),
                    Ast::Integer(1, 7),
                    Err("L1: Expected boolean expression in if conditional, got: i32".into()),
                ),
                (
                    Ast::Boolean(1, true),
                    Ast::Integer(1, 5),
                    Ast::Boolean(1, true),
                    Err("L1: If expression has mismatching arms: expected i32 got bool".into()),
                ),
                (
                    Ast::Boolean(1, true),
                    Ast::Boolean(1, true),
                    Ast::Integer(1, 5),
                    Err("L1: If expression has mismatching arms: expected bool got i32".into()),
                ),
            ] {
                let node = Ast::If(1, Box::new(c), Box::new(t), Box::new(f));

                let result = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_main".into()),
                    &scope,
                );
                assert_eq!(result, ex);
            }
        }
    }
}
