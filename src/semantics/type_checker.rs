pub mod checker {
    use crate::parser::{Ast, PNode, Primitive};
    use crate::semantics::symbol_table::*;
    use crate::semantics::vartable::*;
    use Primitive::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct SemanticMetadata {
        pub ln: u32,
        pub ty: Primitive,
        pub sym: SymbolTable,
    }

    pub type SemanticNode = Ast<SemanticMetadata>;

    impl SemanticNode {
        fn from_parser_ast(ast: &PNode) -> Result<Box<SemanticNode>, String> {
            use Ast::*;
            match ast {
                Integer(ln, val) => Ok(Box::new(Integer(sm_from(*ln), *val))),
                Boolean(ln, val) => Ok(Box::new(Boolean(sm_from(*ln), *val))),
                IdentifierDeclare(ln, name, p) => {
                    Ok(Box::new(IdentifierDeclare(sm_from(*ln), name.clone(), *p)))
                }
                Identifier(l, id) => Ok(Box::new(Identifier(sm_from(*l), id.clone()))),
                Mul(ln, ref l, ref r) => Ok(Box::new(Mul(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                Add(ln, ref l, ref r) => Ok(Box::new(Add(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                BAnd(ln, ref l, ref r) => Ok(Box::new(BAnd(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                BOr(ln, ref l, ref r) => Ok(Box::new(BOr(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                Eq(ln, ref l, ref r) => Ok(Box::new(Eq(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                NEq(ln, ref l, ref r) => Ok(Box::new(NEq(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                Gr(ln, ref l, ref r) => Ok(Box::new(Gr(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                GrEq(ln, ref l, ref r) => Ok(Box::new(GrEq(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                Ls(ln, ref l, ref r) => Ok(Box::new(Ls(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                LsEq(ln, ref l, ref r) => Ok(Box::new(LsEq(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(l)?,
                    SemanticNode::from_parser_ast(r)?,
                ))),
                If(ln, cond, true_arm, false_arm) => Ok(Box::new(If(
                    sm_from(*ln),
                    SemanticNode::from_parser_ast(cond)?,
                    SemanticNode::from_parser_ast(true_arm)?,
                    SemanticNode::from_parser_ast(false_arm)?,
                ))),
                Bind(ln, name, p, ref exp) => Ok(Box::new(Bind(
                    sm_from(*ln),
                    name.clone(),
                    *p,
                    SemanticNode::from_parser_ast(exp)?,
                ))),
                Return(l, None) => Ok(Box::new(Return(sm_from(*l), None))),
                Return(l, Some(exp)) => Ok(Box::new(Return(
                    sm_from(*l),
                    Some(SemanticNode::from_parser_ast(exp)?),
                ))),
                Yield(l, box exp) => Ok(Box::new(Yield(
                    sm_from(*l),
                    SemanticNode::from_parser_ast(exp)?,
                ))),
                YieldReturn(l, None) => Ok(Box::new(YieldReturn(sm_from(*l), None))),
                YieldReturn(l, Some(exp)) => Ok(Box::new(YieldReturn(
                    sm_from(*l),
                    Some(SemanticNode::from_parser_ast(exp)?),
                ))),
                ExpressionBlock(ln, body) => {
                    let mut nbody = vec![];
                    for stmt in body.iter() {
                        let r = SemanticNode::from_parser_ast(stmt)?;
                        nbody.push(*r);
                    }
                    Ok(Box::new(ExpressionBlock(sm_from(*ln), nbody)))
                }
                Statement(_, stmt) => Ok(SemanticNode::from_parser_ast(stmt)?),
                FunctionDef(ln, fname, params, p, body) => {
                    let mut nbody = vec![];
                    for stmt in body.iter() {
                        let r = SemanticNode::from_parser_ast(stmt)?;
                        nbody.push(*r);
                    }
                    Ok(Box::new(FunctionDef(
                        sm_from(*ln),
                        fname.clone(),
                        params.clone(),
                        *p,
                        nbody,
                    )))
                }
                CoroutineDef(ln, coname, params, p, body) => {
                    let mut nbody = vec![];
                    for stmt in body.iter() {
                        let r = SemanticNode::from_parser_ast(stmt)?;
                        nbody.push(*r);
                    }
                    Ok(Box::new(CoroutineDef(
                        sm_from(*ln),
                        coname.clone(),
                        params.clone(),
                        *p,
                        nbody,
                    )))
                }
                FunctionCall(l, fname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut nparams = vec![];
                    for param in params.iter() {
                        let np = SemanticNode::from_parser_ast(param)?;
                        nparams.push(*np);
                    }
                    Ok(Box::new(FunctionCall(sm_from(*l), fname.clone(), nparams)))
                }
                CoroutineInit(l, coname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut nparams = vec![];
                    for param in params.iter() {
                        let np = SemanticNode::from_parser_ast(param)?;
                        nparams.push(*np);
                    }
                    Ok(Box::new(CoroutineInit(
                        sm_from(*l),
                        coname.clone(),
                        nparams,
                    )))
                }
                Printi(l, exp) => Ok(Box::new(Printi(
                    sm_from(*l),
                    SemanticNode::from_parser_ast(exp)?,
                ))),
                Printiln(l, exp) => Ok(Box::new(Printiln(
                    sm_from(*l),
                    SemanticNode::from_parser_ast(exp)?,
                ))),
                Printbln(l, exp) => Ok(Box::new(Printbln(
                    sm_from(*l),
                    SemanticNode::from_parser_ast(exp)?,
                ))),
                Module(ln, funcs, cors) => {
                    let mut nfuncs = vec![];
                    for func in funcs.iter() {
                        nfuncs.push(*SemanticNode::from_parser_ast(func)?);
                    }
                    let mut ncors = vec![];
                    for cor in cors.iter() {
                        ncors.push(*SemanticNode::from_parser_ast(cor)?);
                    }
                    Ok(Box::new(Module(sm_from(*ln), nfuncs, ncors)))
                }
            }
        }

        /// Analyze the AST and add functions to symbol tables
        pub fn extract_routines(&mut self) {

        }
    }

    fn sm(ln: u32, ty: Primitive) -> SemanticMetadata {
        SemanticMetadata {
            ln,
            ty,
            sym: SymbolTable::new(),
        }
    }

    fn sm_from(l: u32) -> SemanticMetadata {
        sm(l, Primitive::Unknown)
    }

    pub fn type_check(
        ast: &PNode,
        ftable: &mut FunctionTable,
    ) -> Result<Box<SemanticNode>, String> {
        let mut sym = SymbolTable::new();

        // add functions to the symbol table: this is just a test
        /*for (f, fi) in ftable.funcs.iter() {
            let params = fi.params.iter().map(|(_, pty)| *pty).collect::<Vec<Primitive>>();
            sym.add(f, Type::Function(params, fi.ty))?;
        }*/

        let mut sm_ast = SemanticNode::from_parser_ast(&ast)?;
        SymbolTable::generate(&mut sm_ast)?;
        let mut semantic = SemanticAnalyzer::new();
        semantic.traverse(&mut sm_ast, &None, ftable, &mut sym)?;
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
            ftable: &mut FunctionTable,
            sym: &mut SymbolTable,
            expected: Option<Primitive>,
        ) -> Result<Primitive, String> {
            let lty = self.traverse(l, current_func, ftable, sym)?;
            let rty = self.traverse(r, current_func, ftable, sym)?;

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
            ftable: &mut FunctionTable,
            sym: &mut SymbolTable,
        ) -> Result<Primitive, String> {
            self.analyize_node(ast, current_func, ftable, sym)
        }

        fn analyize_node(
            &mut self,
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            ftable: &mut FunctionTable,
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
                        match sym.get(id).or(self.stack.get(id)) {
                            Some(Symbol {
                                ty: Type::Primitive(p),
                                ..
                            }) => meta.ty = *p,
                            _ => return Err(format!("L{}: Variable {} not declared", meta.ln, id)),
                        };
                        Ok(meta.ty)
                    }
                },
                Mul(meta, l, r) | Add(meta, l, r) => {
                    let ty = self.binary_op(
                        root_str,
                        meta.ln,
                        l,
                        r,
                        current_func,
                        ftable,
                        sym,
                        Some(I32),
                    )?;
                    meta.ty = ty;
                    Ok(ty)
                }
                BAnd(meta, l, r) | BOr(meta, l, r) => {
                    let ty = self.binary_op(
                        root_str,
                        meta.ln,
                        l,
                        r,
                        current_func,
                        ftable,
                        sym,
                        Some(Bool),
                    )?;
                    meta.ty = ty;
                    Ok(ty)
                }
                Eq(meta, l, r)
                | NEq(meta, l, r)
                | Gr(meta, l, r)
                | GrEq(meta, l, r)
                | Ls(meta, l, r)
                | LsEq(meta, l, r) => {
                    self.binary_op(root_str, meta.ln, l, r, current_func, ftable, sym, None)?;
                    meta.ty = Bool;
                    Ok(meta.ty)
                }
                If(meta, cond, true_arm, false_arm) => {
                    let cond_ty = self.traverse(cond, current_func, ftable, sym)?;
                    if cond_ty == Bool {
                        let true_arm = self.traverse(true_arm, current_func, ftable, sym)?;
                        let false_arm = self.traverse(false_arm, current_func, ftable, sym)?;
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
                    Some(cf) => {
                        let rhs = self.traverse(exp, current_func, ftable, sym)?;
                        if *p == rhs {
                            ftable
                                .add_var(cf, name, *p)
                                .map_err(|e| format!("L{}: {}", meta.ln, e))?;

                            sym.add(name, Type::Primitive(*p))?;
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
                        let fty = ftable.funcs[cf].ty;
                        if fty == Unit {
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
                        let fty = ftable.funcs[cf].ty;
                        let val = self.traverse(exp, current_func, ftable, sym)?;
                        if fty == val {
                            meta.ty = fty;
                            Ok(fty)
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
                    Some(cf) => match exp.as_ref() {
                        Ast::Identifier(id_meta, coname) => {
                            let coty = ftable
                                .get_var(cf, coname)
                                .map_err(|e| format!("L{}: {}", id_meta.ln, e))?
                                .ty;
                            meta.ty = coty;
                            Ok(coty)
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
                        let fty = ftable.funcs[cf].ty;
                        if fty == Unit {
                            meta.ty = Unit;
                            Ok(Unit)
                        } else {
                            Err(format!(
                                "L{}: Yield return expected {} but got unit",
                                meta.ln, fty
                            ))
                        }
                    }
                },
                YieldReturn(meta, Some(exp)) => match current_func {
                    None => Err(format!("L{}: YRet appears outside of function", meta.ln)),
                    Some(cf) => {
                        let fty = ftable.funcs[cf].ty;
                        let val = self.traverse(exp, current_func, ftable, sym)?;
                        if fty == val {
                            meta.ty = fty;
                            Ok(fty)
                        } else {
                            Err(format!(
                                "L{}: Yield return expected {} but got {}",
                                meta.ln, fty, val
                            ))
                        }
                    }
                },
                Statement(meta, stmt) => {
                    self.traverse(stmt, current_func, ftable, sym)?;
                    meta.ty = Unit;
                    Ok(Unit)
                }
                FunctionCall(meta, fname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut pty = vec![];
                    for param in params.iter_mut() {
                        let ty = self.traverse(param, current_func, ftable, sym)?;
                        pty.push(ty);
                    }
                    let (expected_tys, ret_ty) = match sym.get(fname).or(self.stack.get(fname)) {
                        Some(Symbol {
                            ty: Type::Function(pty, rty),
                            ..
                        }) => {(pty, rty)}
                        Some(_) => return Err(format!("L{}: {} found but was not a function", meta.ln, fname)),
                        None => return Err(format!("L{}: function {} not declared", meta.ln, fname)),
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
                        let ty = self.traverse(param, current_func, ftable, sym)?;
                        pty.push(ty);
                    }
                    let (expected_tys, ret_ty) = match sym.get(coname).or(self.stack.get(coname)) {
                        Some(Symbol {
                            ty: Type::Function(pty, rty),
                            ..
                        }) => {(pty, rty)}
                        Some(_) => return Err(format!("L{}: {} found but was not a function", meta.ln, coname)),
                        None => return Err(format!("L{}: function {} not declared", meta.ln, coname)),
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
                    let ty = self.traverse(exp, current_func, ftable, sym)?;
                    if ty == I32 {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!("L{}: Expected i32 for printi got {}", meta.ln, ty))
                    }
                }
                Printiln(meta, exp) => {
                    let ty = self.traverse(exp, current_func, ftable, sym)?;
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
                    let ty = self.traverse(exp, current_func, ftable, sym)?;
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
                        ty = self.traverse(stmt, current_func, ftable, &mut meta.sym)?;
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
                        self.traverse(stmt, &Some(name.clone()), ftable, &mut meta.sym)?;
                    }
                    self.stack.pop();
                    Ok(*p)
                }
                Module(meta, funcs, cors) => {
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for func in funcs.iter_mut() {
                        self.traverse(func, &None, ftable, &mut meta.sym)?;
                    }
                    for cor in cors.iter_mut() {
                        self.traverse(cor, &None, ftable, &mut meta.sym)?;
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
        pub fn start_traverse(
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            ftable: &mut FunctionTable,
        ) -> Result<Primitive, String> {
            let mut sym = SymbolTable::new();
            match current_func {
                Some(cf) => {
                    for var in ftable.funcs[cf].vars.vars.iter() {
                        sym.add(&var.name, Type::Primitive(var.ty))?;
                    }
                }
                None => {}
            };
            
            // add functions to the symbol table: this is just a test
            for (f, fi) in ftable.funcs.iter() {
                let params = fi.params.iter().map(|(_, pty)| *pty).collect::<Vec<Primitive>>();
                sym.add(f, Type::Function(params, fi.ty))?;
            }

            let mut semantic = SemanticAnalyzer::new();
            semantic.traverse(ast, current_func, ftable, &mut sym)
        }

        use super::*;
        use crate::parser::{Ast, Primitive};

        #[test]
        pub fn test_integer() {
            let node = Ast::Integer(1, 5);
            let mut ft = FunctionTable::new();

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &mut ft,
            );
            assert_eq!(ty, Ok(Primitive::I32));
        }

        #[test]
        pub fn test_identifier() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_main".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable {
                        vars: vec![VarDecl {
                            name: "x".into(),
                            ty: Bool,
                            size: 4,
                            frame_offset: 4,
                        }],
                    },
                    label_count: 0,
                    ty: Unit,
                },
            );

            let node = Ast::Identifier(1, "x".into());

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(Primitive::Bool));
        }

        #[test]
        pub fn test_add() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: Unit,
                },
            );
            ft.funcs
                .get_mut("my_func")
                .unwrap()
                .vars
                .add_var("x", I32)
                .unwrap();
            ft.funcs
                .get_mut("my_func")
                .unwrap()
                .vars
                .add_var("b", Bool)
                .unwrap();
            // both operands are i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
        }

        #[test]
        pub fn test_mul() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: Unit,
                },
            );
            ft.funcs
                .get_mut("my_func")
                .unwrap()
                .vars
                .add_var("x", I32)
                .unwrap();
            ft.funcs
                .get_mut("my_func")
                .unwrap()
                .vars
                .add_var("b", Bool)
                .unwrap();
            // both operands are i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
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

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
        }

        #[test]
        pub fn test_boolean_ops() {
            let mut ft = FunctionTable::new();
            let tests: Vec<(PNode, Result<Primitive, String>)> = vec![(
                Ast::BAnd(
                    1,
                    Box::new(Ast::Boolean(1, true)),
                    Box::new(Ast::Integer(1, 5)),
                ),
                Err("L1: && expected operands of bool".into()),
            )];

            for (test, expected) in tests.iter() {
                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&test).unwrap(),
                    &None,
                    &mut ft,
                );
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_comparison_ops() {
            let mut ft = FunctionTable::new();
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
                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&test).unwrap(),
                    &None,
                    &mut ft,
                );
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_bind() {
            // RHS type matches the LHS type
            {
                let node = Ast::Bind(1, "x".into(), Primitive::I32, Box::new(Ast::Integer(1, 5)));
                let mut ft = FunctionTable::new();
                ft.funcs.insert(
                    "my_func".into(),
                    FunctionInfo {
                        params: vec![],
                        vars: VarTable::new(),
                        label_count: 0,
                        ty: Unit,
                    },
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // RHS type does not match LHS type
            {
                let node = Ast::Bind(1, "x".into(), Primitive::Bool, Box::new(Ast::Integer(1, 5)));
                let mut ft = FunctionTable::new();
                ft.funcs.insert(
                    "my_func".into(),
                    FunctionInfo {
                        params: vec![],
                        vars: VarTable::new(),
                        label_count: 0,
                        ty: Unit,
                    },
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Err("L1: Bind expected bool but got i32".into()));
            }

            // recursive definition
            {
                let node = Ast::Bind(
                    1,
                    "x".into(),
                    Primitive::I32,
                    Box::new(Ast::Identifier(1, "x".into())),
                );
                let mut ft = FunctionTable::new();
                ft.funcs.insert(
                    "my_func".into(),
                    FunctionInfo {
                        params: vec![],
                        vars: VarTable::new(),
                        label_count: 0,
                        ty: Unit,
                    },
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Err("L1: Variable x not declared".into()));
            }

            // use an unbound variable
            {
                let node = Ast::Identifier(1, "x".into());
                let mut ft = FunctionTable::new();
                ft.funcs.insert(
                    "my_func".into(),
                    FunctionInfo {
                        params: vec![],
                        vars: VarTable::new(),
                        label_count: 0,
                        ty: Unit,
                    },
                );

                let ty = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &mut ft,
                );
                assert_eq!(ty, Err("L1: Variable x not declared".into()));
            }
        }

        #[test]
        pub fn test_return_unit() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: Unit,
                },
            );
            let node = Ast::Return(1, None);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(Unit));
        }

        #[test]
        pub fn test_return_i32() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );
            let node = Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))));

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));
        }

        #[test]
        pub fn test_fn_call() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );
            let node = Ast::FunctionCall(1, "my_func".into(), vec![]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            ft.funcs.insert(
                "my_func2".into(),
                FunctionInfo {
                    params: vec![("x".into(), I32)],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );

            // test correct parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![Ast::Integer(1, 5)]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &mut ft,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to function: my_func2".into())
            );
        }

        #[test]
        pub fn test_co_init() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_co".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );
            let node = Ast::CoroutineInit(1, "my_co".into(), vec![]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            ft.funcs.insert(
                "my_co2".into(),
                FunctionInfo {
                    params: vec![("x".into(), I32)],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );

            // test correct parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![Ast::Integer(1, 5)]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &mut ft,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to coroutine: my_co2".into())
            );
        }

        #[test]
        pub fn test_yield_return() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_co".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: Unit,
                },
            );
            let node = Ast::YieldReturn(1, None);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(Unit));

            ft.funcs.insert(
                "my_co2".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );

            // test correct type for yield return
            let node = Ast::YieldReturn(1, Some(Box::new(Ast::Integer(1, 5))));
            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Ast::YieldReturn(1, None);
            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &mut ft,
            );
            assert_eq!(ty, Err("L1: Yield return expected i32 but got unit".into()));
        }

        #[test]
        fn test_yield() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_main".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable {
                        vars: vec![VarDecl {
                            name: "c".into(),
                            size: 4,
                            ty: I32,
                            frame_offset: 4,
                        }],
                    },
                    label_count: 0,
                    ty: Unit,
                },
            );
            ft.funcs.insert(
                "my_co2".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );
            let node = Ast::Yield(1, Box::new(Ast::Identifier(1, "c".into())));

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));
        }

        #[test]
        fn test_func_def() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_func".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );

            let node = Ast::FunctionDef(
                1,
                "my_func".into(),
                vec![],
                I32,
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
            );

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::FunctionDef(1, "my_func".into(), vec![], I32, vec![Ast::Return(1, None)]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &mut ft,
            );
            assert_eq!(ty, Err("L1: Return expected i32 type and got unit".into()));
        }

        #[test]
        fn test_coroutine_def() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_co".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );

            let node = Ast::CoroutineDef(
                1,
                "my_co".into(),
                vec![],
                I32,
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
            );

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &mut ft,
            );
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::CoroutineDef(1, "my_co".into(), vec![], I32, vec![Ast::Return(1, None)]);

            let ty = start_traverse(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &None,
                &mut ft,
            );
            assert_eq!(ty, Err("L1: Return expected i32 type and got unit".into()));
        }

        #[test]
        fn test_if_expression() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_main".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable {
                        vars: vec![VarDecl {
                            name: "c".into(),
                            size: 4,
                            ty: I32,
                            frame_offset: 4,
                        }],
                    },
                    label_count: 0,
                    ty: Unit,
                },
            );
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

                let result = start_traverse(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_main".into()),
                    &mut ft,
                );
                assert_eq!(result, ex);
            }
        }
    }
}
