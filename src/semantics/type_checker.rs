pub mod checker {
    use crate::ast;
    use crate::ast::{BinaryOperator, UnaryOperator};
    use crate::parser::PNode;
    use crate::semantics::semanticnode::SemanticNode;
    use crate::semantics::symbol_table::*;
    use crate::syntax::ast::Type;
    use crate::syntax::ast::Type::*;

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

        fn unary_op(
            &mut self,
            op: UnaryOperator,
            operand: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<ast::Type, String> {
            use UnaryOperator::*;

            let operand_ty = self.traverse(operand, current_func, sym)?;

            match op {
                Minus => {
                    if operand_ty == I32 {
                        Ok(I32)
                    } else {
                        Err(format!("{} expected i32 but found {}", op, operand_ty))
                    }
                }
                Not => {
                    if operand_ty == Bool {
                        Ok(Bool)
                    } else {
                        Err(format!("{} expected bool but found {}", op, operand_ty))
                    }
                }
            }
        }

        fn binary_op(
            &mut self,
            op: BinaryOperator,
            l: &mut SemanticNode,
            r: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<ast::Type, String> {
            use BinaryOperator::*;

            let lty = self.traverse(l, current_func, sym)?;
            let rty = self.traverse(r, current_func, sym)?;

            match op {
                Add | Sub | Mul | Div => {
                    if lty == I32 && rty == I32 {
                        Ok(I32)
                    } else {
                        Err(format!("{} expected i32 but found {} and {}", op, lty, rty))
                    }
                }
                BAnd | BOr => {
                    if lty == Bool && rty == Bool {
                        Ok(Bool)
                    } else {
                        Err(format!(
                            "{} expected bool but found {} and {}",
                            op, lty, rty
                        ))
                    }
                }
                Eq | NEq | Ls | LsEq | Gr | GrEq => {
                    if lty == rty {
                        Ok(Bool)
                    } else {
                        Err(format!("{} expected {} but found {}", op, lty, rty))
                    }
                }
            }
        }

        pub fn traverse(
            &mut self,
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<ast::Type, String> {
            self.analyize_node(ast, current_func, sym).map_err(|e| {
                if !e.starts_with("L") {
                    format!("L{}: {}", ast.get_metadata().ln, e)
                } else {
                    e
                }
            })
        }

        fn lookup<'a>(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&'a Symbol, String> {
            sym.get(id)
                .or(self.stack.get(id))
                .ok_or(format!("{} is not defined", id))
        }

        fn lookup_func_or_cor<'a>(
            &'a self,
            sym: &'a SymbolTable,
            id: &str,
        ) -> Result<(&Vec<ast::Type>, &ast::Type), String> {
            match self.lookup(sym, id)? {
                Symbol {
                    ty: Type::Coroutine(params, p),
                    ..
                }
                | Symbol {
                    ty: Type::Function(params, p),
                    ..
                } => Ok((params, p)),
                _ => return Err(format!("{} is not a coroutine or function", id)),
            }
        }

        fn lookup_coroutine<'a>(
            &'a self,
            sym: &'a SymbolTable,
            id: &str,
        ) -> Result<(&Vec<ast::Type>, &ast::Type), String> {
            match self.lookup(sym, id)? {
                Symbol {
                    ty: Type::Coroutine(params, p),
                    ..
                } => Ok((params, p)),
                _ => return Err(format!("{} is not a coroutine", id)),
            }
        }

        fn lookup_var<'a>(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&ast::Type, String> {
            let p = &self.lookup(sym, id)?.ty;
            match p {
                Custom(..) | CoroutineVal(_) | I32 | Bool => Ok(p),
                _ => return Err(format!("{} is not a variable", id)),
            }
        }

        fn analyize_node(
            &mut self,
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            sym: &mut SymbolTable,
        ) -> Result<ast::Type, String> {
            use ast::Ast::*;
            match ast {
                Integer(meta, _) => {
                    meta.ty = I32;
                    Ok(I32)
                }
                Boolean(meta, _) => {
                    meta.ty = Bool;
                    Ok(Bool)
                }
                CustomType(meta, name) => {
                    meta.ty = Custom(name.clone());
                    Ok(meta.ty.clone())
                }
                IdentifierDeclare(meta, _, p) => {
                    meta.ty = p.clone();
                    Ok(p.clone())
                }
                Identifier(meta, id) => match current_func {
                    None => Err(format!("Variable {} appears outside of function", id)),
                    Some(_) => {
                        match self.lookup(sym, id)? {
                            Symbol { ty: p, .. } => meta.ty = p.clone(),
                        };
                        Ok(meta.ty.clone())
                    }
                },
                MemberAccess(meta, src, member) => {
                    // Get the type of src and look up its struct definition
                    // Check the struct definition for the type of `member`
                    // if it exists, if it does not exist then return an error
                    let src_ty = self.traverse(src, current_func, sym)?;
                    match src_ty {
                        Custom(struct_name) => {
                            let member_ty = self
                                .lookup(sym, &struct_name)?
                                .ty
                                .get_member(&member)
                                .ok_or(format!(
                                    "{} does not have member {}",
                                    struct_name, member
                                ))?;
                            meta.ty = member_ty.clone();
                            Ok(meta.ty.clone())
                        }
                        _ => Err(format!("Type {} does not have members", src_ty)),
                    }
                }
                BinaryOp(meta, op, l, r) => {
                    let ty = self.binary_op(*op, l, r, current_func, sym)?;
                    meta.ty = ty;
                    Ok(meta.ty.clone())
                }
                UnaryOp(meta, op, operand) => {
                    let ty = self.unary_op(*op, operand, current_func, sym)?;
                    meta.ty = ty;
                    Ok(meta.ty.clone())
                }
                If(meta, cond, true_arm, false_arm) => {
                    let cond_ty = self.traverse(cond, current_func, sym)?;
                    if cond_ty == Bool {
                        let true_arm = self.traverse(true_arm, current_func, sym)?;
                        let false_arm = self.traverse(false_arm, current_func, sym)?;
                        if true_arm == false_arm {
                            meta.ty = true_arm;
                            Ok(meta.ty.clone())
                        } else {
                            Err(format!(
                                "If expression has mismatching arms: expected {} got {}",
                                true_arm, false_arm
                            ))
                        }
                    } else {
                        Err(format!(
                            "Expected boolean expression in if conditional, got: {}",
                            cond_ty
                        ))
                    }
                }
                Mutate(_, id, exp) => match current_func {
                    Some(_) => {
                        let rhs = self.traverse(exp, current_func, sym)?;
                        match self.lookup(sym, id)? {
                            symbol => {
                                if symbol.mutable {
                                    if symbol.ty == rhs {
                                        Ok(rhs.clone())
                                    } else {
                                        Err(format!(
                                            "{} is of type {} but is assigned {}",
                                            id, symbol.ty, rhs
                                        ))
                                    }
                                } else {
                                    Err(format!("Variable {} is not mutable", id))
                                }
                            }
                        }
                    }
                    None => Err(format!(
                        "Attempting to mutate a variable {} outside of function",
                        id
                    )),
                },
                Bind(meta, name, mutable, p, exp) => match current_func {
                    Some(_) => {
                        let rhs = self.traverse(exp, current_func, sym)?;
                        if *p == rhs {
                            sym.add(name, p.clone(), *mutable)?;
                            meta.ty = p.clone();
                            Ok(rhs)
                        } else {
                            Err(format!("Bind expected {} but got {}", p, rhs))
                        }
                    }
                    None => Err(format!(
                        "Attempting to bind variable {} outside of function",
                        name
                    )),
                },
                Return(meta, None) => match current_func {
                    None => Err(format!("Return called outside of a function")),
                    Some(cf) => {
                        let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                        if *fty == Unit {
                            meta.ty = Unit;
                            Ok(Unit)
                        } else {
                            Err(format!("Return expected {} type and got unit", fty))
                        }
                    }
                },
                Return(meta, Some(exp)) => match current_func {
                    None => Err(format!("Return appears outside of a function")),
                    Some(cf) => {
                        let val = self.traverse(exp, current_func, sym)?;
                        let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                        if *fty == val {
                            meta.ty = fty.clone();
                            Ok(meta.ty.clone())
                        } else {
                            Err(format!("Return expected {} but got {}", fty, val))
                        }
                    }
                },
                Yield(meta, exp) => match current_func {
                    None => Err(format!("Yield appears outside of function")),
                    Some(_) => {
                        let yield_target = self.traverse(exp, current_func, sym)?;
                        meta.ty = match yield_target {
                            CoroutineVal(ret_ty) => *ret_ty.clone(),
                            _ => {
                                return Err(format!("yield expects co<_> but got {}", yield_target))
                            }
                        };
                        Ok(meta.ty.clone())
                    }
                },
                YieldReturn(meta, None) => match current_func {
                    None => Err(format!("YRet appears outside of function")),
                    Some(cf) => {
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        if *ret_ty == Unit {
                            meta.ty = Unit;
                            Ok(Unit)
                        } else {
                            Err(format!("Yield return expected {} but got unit", ret_ty))
                        }
                    }
                },
                YieldReturn(meta, Some(exp)) => match current_func {
                    None => Err(format!("YRet appears outside of function")),
                    Some(cf) => {
                        let val = self.traverse(exp, current_func, sym)?;
                        let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                        if *ret_ty == val {
                            meta.ty = ret_ty.clone();
                            Ok(meta.ty.clone())
                        } else {
                            Err(format!("Yield return expected {} but got {}", ret_ty, val))
                        }
                    }
                },
                Statement(meta, stmt) => {
                    self.traverse(stmt, current_func, sym)?;
                    meta.ty = Unit;
                    Ok(Unit)
                }
                RoutineCall(meta, call, fname, params) => {
                    // test that the expressions passed to the function match the functions
                    // parameter types
                    let mut pty = vec![];
                    for param in params.iter_mut() {
                        let ty = self.traverse(param, current_func, sym)?;
                        pty.push(ty);
                    }
                    let (expected_param_tys, ret_ty) = match sym
                        .get(fname)
                        .or(self.stack.get(fname))
                    {
                        Some(Symbol {
                            ty: Type::Function(pty, rty),
                            ..
                        }) if *call == crate::syntax::ast::RoutineCall::Function => {
                            (pty, *rty.clone())
                        }
                        Some(Symbol {
                            ty: Type::Coroutine(pty, rty),
                            ..
                        }) if *call == crate::syntax::ast::RoutineCall::CoroutineInit => {
                            (pty, Type::CoroutineVal(rty.clone()))
                        }
                        Some(_) => return Err(format!("{} found but was not a function", fname)),
                        None => return Err(format!("function {} not declared", fname)),
                    };

                    if pty.len() != expected_param_tys.len() {
                        Err(format!(
                            "Incorrect number of parameters passed to routine: {}",
                            fname
                        ))
                    } else {
                        let z = pty.iter().zip(expected_param_tys.iter());
                        let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                        if all_params_match {
                            meta.ty = ret_ty;
                            Ok(meta.ty.clone())
                        } else {
                            Err(format!(
                                "One or more parameters had mismatching types for function {}",
                                fname
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
                        Err(format!("Expected i32 for printi got {}", ty))
                    }
                }
                Printiln(meta, exp) => {
                    let ty = self.traverse(exp, current_func, sym)?;
                    if ty == I32 {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!("Expected i32 for printiln got {}", ty))
                    }
                }
                Printbln(meta, exp) => {
                    let ty = self.traverse(exp, current_func, sym)?;
                    if ty == Bool {
                        meta.ty = Unit;
                        Ok(Unit)
                    } else {
                        Err(format!("Expected i32 for printbln got {}", ty))
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
                    Ok(meta.ty.clone())
                }
                RoutineDef(meta, _, name, params, p, body) => {
                    for (pname, pty) in params.iter() {
                        meta.sym.add(pname, pty.clone(), false)?;
                    }
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for stmt in body.iter_mut() {
                        self.traverse(stmt, &Some(name.clone()), &mut meta.sym)?;
                    }
                    self.stack.pop();
                    Ok(p.clone())
                }
                Module {
                    meta,
                    functions,
                    coroutines,
                    structs,
                } => {
                    let tmp_sym = sym.clone();
                    self.stack.push(tmp_sym);
                    for func in functions.iter_mut() {
                        self.traverse(func, &None, &mut meta.sym)?;
                    }
                    for cor in coroutines.iter_mut() {
                        self.traverse(cor, &None, &mut meta.sym)?;
                    }
                    for st in structs.iter_mut() {
                        self.traverse(st, &None, &mut meta.sym)?;
                    }
                    self.stack.pop();
                    meta.ty = Unit;
                    Ok(Unit)
                }
                StructDef(_, struct_name, members) => {
                    // Check the type of each member
                    for (mname, mtype) in members.iter() {
                        match mtype {
                            Custom(ty_name) => {
                                self.lookup(sym, ty_name).map_err(|e| {
                                    format!("member {}.{} invalid: {}", struct_name, mname, e)
                                })?;
                            }
                            _ => (),
                        }
                    }
                    Ok(Unit)
                }
                StructInit(meta, struct_name, params) => {
                    // Validate the types in the initialization parameters
                    // match their respective members in the struct
                    let struct_def = self.lookup(sym, &struct_name)?.ty.clone();
                    let expected_num_params =
                        struct_def.get_members().ok_or("Invalid structure")?.len();
                    if params.len() != expected_num_params {
                        return Err(format!(
                            "expected {} parameters but found {}",
                            expected_num_params,
                            params.len()
                        ));
                    }

                    for (pn, pv) in params.iter_mut() {
                        let pty = self.traverse(pv, current_func, sym)?;
                        let member_ty = struct_def
                            .get_member(pn)
                            .ok_or(format!("member {} not found on {}", pn, struct_name))?;
                        if pty != *member_ty {
                            return Err(format!(
                                "{}.{} expects {} but got {}",
                                struct_name, pn, member_ty, pty
                            ));
                        }
                    }
                    meta.ty = Custom(struct_name.clone());
                    Ok(meta.ty.clone())
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::ast;
        use crate::ast::Ast;
        use crate::lexer::Lexer;
        use crate::Token;
        use std::collections::HashMap;

        /*
         * map: function name -> (params, ret, vars)
         */
        struct FunInfo {
            params: Vec<(&'static str, ast::Type)>,
            ret: ast::Type,
            vars: Vec<(&'static str, bool, ast::Type)>,
        }

        impl FunInfo {
            pub fn new(
                params: Vec<(&'static str, ast::Type)>,
                ret: ast::Type,
                vars: Vec<(&'static str, bool, ast::Type)>,
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
                params: Vec<(&'static str, ast::Type)>,
                ret: ast::Type,
                vars: Vec<(&'static str, bool, ast::Type)>,
            ) {
                self.func
                    .insert(name.into(), FunInfo::new(params, ret, vars));
            }
        }

        fn start(
            ast: &mut SemanticNode,
            current_func: &Option<String>,
            scope: &Scope,
        ) -> Result<ast::Type, String> {
            let mut sym = SymbolTable::new();
            match current_func {
                Some(cf) => {
                    for (vname, mutable, vty) in scope
                        .func
                        .get(cf)
                        .ok_or(format!("could not find: {}", cf))?
                        .vars
                        .iter()
                    {
                        sym.add(&vname, vty.clone(), *mutable)?;
                    }
                }
                None => {}
            };

            // add functions to the symbol table: this is just a test
            for (f, fi) in scope.func.iter() {
                let params = fi
                    .params
                    .iter()
                    .map(|(_, pty)| pty.clone())
                    .collect::<Vec<ast::Type>>();

                if f.starts_with("my_co") {
                    sym.add(f, Type::Coroutine(params, Box::new(fi.ret.clone())), false)?;
                } else {
                    sym.add(f, Type::Function(params, Box::new(fi.ret.clone())), false)?;
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
            assert_eq!(ty, Ok(ast::Type::I32));
        }

        #[test]
        pub fn test_identifier() {
            let mut scope = Scope::new();
            scope.add("my_main", vec![], Unit, vec![("x", false, Bool)]);

            let node = Ast::Identifier(1, "x".into());

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &scope,
            );
            assert_eq!(ty, Ok(ast::Type::Bool));
        }

        #[test]
        pub fn test_unary_ops() {
            let mut scope = Scope::new();
            scope.add(
                "my_func",
                vec![],
                Unit,
                vec![("x", false, I32), ("b", false, Bool)],
            );
            // operand is i32
            {
                let node = Ast::UnaryOp(1, UnaryOperator::Minus, Box::new(Ast::Integer(1, 5)));

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Ok(ast::Type::I32));
            }
            // operand is not i32
            {
                let node = Ast::UnaryOp(1, UnaryOperator::Minus, Box::new(Ast::Boolean(1, true)));

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Err("L1: - expected i32 but found bool".into()));
            }
        }

        #[test]
        pub fn test_add() {
            let mut scope = Scope::new();
            scope.add(
                "my_func",
                vec![],
                Unit,
                vec![("x", false, I32), ("b", false, Bool)],
            );
            // both operands are i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Add,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Ok(ast::Type::I32));
            }

            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Add,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected i32 but found bool and i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Add,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected i32 but found i32 and bool".into()));
            }
            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Add,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: + expected i32 but found bool and bool".into()));
            }
        }

        #[test]
        pub fn test_mul() {
            let mut scope = Scope::new();
            scope.add(
                "my_func",
                vec![],
                Unit,
                vec![("x", false, I32), ("b", false, Bool)],
            );
            // both operands are i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(Ast::Integer(1, 5)),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &None,
                    &scope,
                );
                assert_eq!(ty, Ok(ast::Type::I32));
            }

            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected i32 but found bool and i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected i32 but found i32 and bool".into()));
            }
            // operands are not i32
            {
                let node = Ast::BinaryOp(
                    1,
                    BinaryOperator::Mul,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: * expected i32 but found bool and bool".into()));
            }
        }

        #[test]
        pub fn test_boolean_ops() {
            let scope = Scope::new();

            let tests: Vec<(PNode, Result<ast::Type, String>)> = vec![(
                Ast::BinaryOp(
                    1,
                    BinaryOperator::BAnd,
                    Box::new(Ast::Boolean(1, true)),
                    Box::new(Ast::Integer(1, 5)),
                ),
                Err("L1: && expected bool but found bool and i32".into()),
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
            let tests: Vec<(PNode, Result<ast::Type, String>)> = vec![
                (
                    Ast::BinaryOp(
                        1,
                        BinaryOperator::Eq,
                        Box::new(Ast::Integer(1, 3)),
                        Box::new(Ast::Integer(1, 5)),
                    ),
                    Ok(ast::Type::Bool),
                ),
                (
                    Ast::BinaryOp(
                        1,
                        BinaryOperator::Eq,
                        Box::new(Ast::Boolean(1, true)),
                        Box::new(Ast::Boolean(1, false)),
                    ),
                    Ok(ast::Type::Bool),
                ),
                (
                    Ast::BinaryOp(
                        1,
                        BinaryOperator::Eq,
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

                let node = Ast::Bind(
                    1,
                    "x".into(),
                    false,
                    ast::Type::I32,
                    Box::new(Ast::Integer(1, 5)),
                );
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Ok(ast::Type::I32));
            }

            // RHS type does not match LHS type
            {
                let mut scope = Scope::new();
                scope.add("my_func", vec![], Unit, vec![]);

                let node = Ast::Bind(
                    1,
                    "x".into(),
                    false,
                    ast::Type::Bool,
                    Box::new(Ast::Integer(1, 5)),
                );
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
                    false,
                    ast::Type::I32,
                    Box::new(Ast::Identifier(1, "x".into())),
                );

                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: x is not defined".into()));
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
                assert_eq!(ty, Err("L1: x is not defined".into()));
            }
        }

        #[test]
        pub fn test_mutate() {
            // RHS type matches the LHS type
            {
                let mut scope = Scope::new();
                scope.add(
                    "my_func",
                    vec![],
                    Unit,
                    vec![("x".into(), true, ast::Type::I32)],
                );

                let node = Ast::Mutate(1, "x".into(), Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Ok(ast::Type::I32));
            }
            // Variable is immutable
            {
                let mut scope = Scope::new();
                scope.add(
                    "my_func",
                    vec![],
                    Unit,
                    vec![("x".into(), false, ast::Type::I32)],
                );

                let node = Ast::Mutate(1, "x".into(), Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: Variable x is not mutable".into()));
            }
            // RHS type mismatch
            {
                let mut scope = Scope::new();
                scope.add(
                    "my_func",
                    vec![],
                    Unit,
                    vec![("x".into(), true, ast::Type::Bool)],
                );

                let node = Ast::Mutate(1, "x".into(), Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: x is of type bool but is assigned i32".into()));
            }
            // Variable does not exist
            {
                let mut scope = Scope::new();
                scope.add(
                    "my_func",
                    vec![],
                    Unit,
                    vec![("y".into(), false, ast::Type::I32)],
                );

                let node = Ast::Mutate(1, "x".into(), Box::new(Ast::Integer(1, 5)));
                let ty = start(
                    &mut SemanticNode::from_parser_ast(&node).unwrap(),
                    &Some("my_func".into()),
                    &scope,
                );
                assert_eq!(ty, Err("L1: x is not defined".into()));
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

            let node = Ast::RoutineCall(1, ast::RoutineCall::Function, "my_func".into(), vec![]);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            scope.add("my_func2", vec![("x", I32)], I32, vec![]);
            // test correct parameters passed in call
            let node = Ast::RoutineCall(
                1,
                ast::RoutineCall::Function,
                "my_func2".into(),
                vec![Ast::Integer(1, 5)],
            );

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &scope,
            );
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::RoutineCall(1, ast::RoutineCall::Function, "my_func2".into(), vec![]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_func2".into()),
                &scope,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to routine: my_func2".into())
            );
        }

        #[test]
        pub fn test_co_init() {
            let mut scope = Scope::new();
            scope.add("my_co", vec![], I32, vec![]);
            scope.add("my_co2", vec![("x", I32)], I32, vec![]);

            let node = Ast::RoutineCall(1, ast::RoutineCall::CoroutineInit, "my_co".into(), vec![]);
            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co".into()),
                &scope,
            );
            assert_eq!(ty, Ok(CoroutineVal(Box::new(I32))));

            // test correct parameters passed in call
            let node = Ast::RoutineCall(
                1,
                ast::RoutineCall::CoroutineInit,
                "my_co2".into(),
                vec![Ast::Integer(1, 5)],
            );

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(ty, Ok(CoroutineVal(Box::new(I32))));

            // test incorrect parameters passed in call
            let node =
                Ast::RoutineCall(1, ast::RoutineCall::CoroutineInit, "my_co2".into(), vec![]);

            let ty = start(
                &mut SemanticNode::from_parser_ast(&node).unwrap(),
                &Some("my_co2".into()),
                &scope,
            );
            assert_eq!(
                ty,
                Err("L1: Incorrect number of parameters passed to routine: my_co2".into())
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
            scope.add(
                "my_main",
                vec![],
                Unit,
                vec![("c", false, CoroutineVal(Box::new(I32)))],
            );
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

            let node = Ast::RoutineDef(
                1,
                ast::RoutineDef::Function,
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

            let node = Ast::RoutineDef(
                1,
                ast::RoutineDef::Function,
                "my_func".into(),
                vec![],
                I32,
                vec![Ast::Return(1, None)],
            );

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

            let node = Ast::RoutineDef(
                1,
                ast::RoutineDef::Coroutine,
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

            let node = Ast::RoutineDef(
                1,
                ast::RoutineDef::Coroutine,
                "my_co".into(),
                vec![],
                I32,
                vec![Ast::Return(1, None)],
            );

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
            scope.add("my_main", vec![], I32, vec![("c", false, I32)]);

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

        #[test]
        pub fn test_struct_init() {
            use crate::syntax::parser;
            for (text, expected) in vec![
                ("struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:1};}",
                Ok(())),
                ("struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:false};}", 
                Err("Semantic: L1: MyStruct.x expects i32 but got bool")),
                ("struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{};}", 
                Err("Semantic: L1: expected 1 parameters but found 0")),
                ("struct MyStruct{x:i32} fn test() -> i32 {return MyStruct{x:5};}", 
                Err("Semantic: L1: Return expected i32 but got MyStruct")),
            ] {
                let mut lexer = Lexer::new();
                let tokens: Vec<Token> = lexer
                    .tokenize(&text)
                    .into_iter()
                    .collect::<Result<_, _>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let result = type_check(&ast);
                match expected {
                    Ok(_) => assert!(result.is_ok()),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
        }

        #[test]
        pub fn test_member_access() {
            use crate::syntax::parser;
            for (text, expected) in vec![
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> i32 {return ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> i32 {return ms.y;}",
                Err("Semantic: L1: MyStruct does not have member y")),
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> bool{return ms.x;}",
                Err("Semantic: L1: Return expected bool but got i32")),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i32 {return ms.ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> MyStruct {return ms.ms;}",
                Ok(())),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i32 {return ms.ms.y;}",
                Err("Semantic: L1: MyStruct does not have member y")),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> bool {return ms.ms.x;}",
                Err("Semantic: L1: Return expected bool but got i32")),
                ("struct MyStruct{x:co i32} fn test(c: co i32) -> MyStruct {return MyStruct{x: c};}",
                Ok(())),
            ] {
                let mut lexer = Lexer::new();
                let tokens: Vec<Token> = lexer
                    .tokenize(&text)
                    .into_iter()
                    .collect::<Result<_, _>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let result = type_check(&ast);
                match expected {
                    Ok(_) => assert!(result.is_ok()),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
        }
    }
}
