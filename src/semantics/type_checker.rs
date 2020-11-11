use crate::parser;
use parser::Primitive;

pub mod checker {
    use crate::parser::{Ast, PNode, Primitive};
    use crate::semantics::vartable::*;
    use Primitive::*;

    #[derive(Clone, Debug, PartialEq)]
    pub struct SemanticMetadata {
        pub ln: u32,
        pub ty: Primitive,
    }

    pub type SemanticNode = Ast<SemanticMetadata>;

    type SM = SemanticMetadata;

    pub fn type_check(
        ast: &PNode,
        ftable: &mut FunctionTable,
    ) -> Result<(Primitive, Box<SemanticNode>), String> {
        traverse(ast, &None, ftable)
    }

    fn binary_op(
        op: String,
        ln: u32,
        l: &PNode,
        r: &PNode,
        current_func: &Option<String>,
        ftable: &mut FunctionTable,
        expected: Option<Primitive>,
    ) -> Result<(Primitive, Box<SemanticNode>, Box<SemanticNode>), String> {
        let (lty, lv) = traverse(l, current_func, ftable)?;
        let (rty, rv) = traverse(r, current_func, ftable)?;

        match expected {
            None => {
                if lty == rty {
                    Ok((lty, lv, rv))
                } else {
                    Err(format!(
                        "L{}: {} expected {} but found {}",
                        ln, op, lty, rty
                    ))
                }
            }
            Some(expected) => {
                if lty == expected && rty == expected {
                    Ok((lty, lv, rv))
                } else {
                    Err(format!("L{}: {} expected operands of {}", ln, op, expected))
                }
            }
        }
    }

    fn traverse(
        ast: &PNode,
        current_func: &Option<String>,
        ftable: &mut FunctionTable,
    ) -> Result<(Primitive, Box<SemanticNode>), String> {
        use Ast::*;
        match ast {
            Integer(ln, val) => Ok((I32, Box::new(Integer(SM { ln: *ln, ty: I32 }, *val)))),
            Boolean(ln, val) => Ok((Bool, Box::new(Boolean(SM { ln: *ln, ty: Bool }, *val)))),
            IdentifierDeclare(ln, name, p) => Ok((
                *p,
                Box::new(IdentifierDeclare(SM { ln: *ln, ty: *p }, name.clone(), *p)),
            )),
            Identifier(l, id) => match current_func {
                None => Err(format!(
                    "L{}: Variable {} appears outside of function",
                    l, id
                )),
                Some(cf) => {
                    let idty = ftable
                        .funcs
                        .get(cf)
                        .ok_or(format!("L{}: Undefined function {}", l, cf))?
                        .vars
                        .vars
                        .iter()
                        .find(|v| v.name == *id)
                        .map_or_else(
                            || Err(format!("L{}: Variable {} not declared", l, id)),
                            |v| Ok(v.ty),
                        )?;
                    Ok((
                        idty,
                        Box::new(Identifier(SM { ln: *l, ty: idty }, id.clone())),
                    ))
                }
            },
            ExpressionBlock(ln, body) => {
                let mut ty = Unit;
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = traverse(stmt, current_func, ftable)?;
                    ty = r.0;
                    nbody.push(*r.1);
                }
                Ok((ty, Box::new(ExpressionBlock(SM { ln: *ln, ty }, nbody))))
            }
            Statement(_, stmt) => {
                let (_, stmt) = traverse(stmt, current_func, ftable)?;
                Ok((Unit, stmt))
            }
            Mul(ln, ref l, ref r) => {
                let (ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, Some(I32))?;
                Ok((ty, Box::new(Mul(SM { ln: *ln, ty }, sl, sr))))
            }
            Add(ln, ref l, ref r) => {
                let (ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, Some(I32))?;
                Ok((ty, Box::new(Add(SM { ln: *ln, ty }, sl, sr))))
            }
            BAnd(ln, ref l, ref r) => {
                let (ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, Some(Bool))?;
                Ok((ty, Box::new(BAnd(SM { ln: *ln, ty }, sl, sr))))
            }
            BOr(ln, ref l, ref r) => {
                let (ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, Some(Bool))?;
                Ok((ty, Box::new(BOr(SM { ln: *ln, ty }, sl, sr))))
            }
            Ast::Eq(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(Eq(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            Ast::NEq(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(NEq(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            Ast::Gr(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(Gr(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            Ast::GrEq(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(GrEq(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            Ast::Ls(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(Ls(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            Ast::LsEq(ln, ref l, ref r) => {
                let (_ty, sl, sr) =
                    binary_op(ast.root_str(), *ln, l, r, current_func, ftable, None)?;
                Ok((Bool, Box::new(LsEq(SM { ln: *ln, ty: Bool }, sl, sr))))
            }
            If(ln, cond, true_arm, false_arm) => {
                let (cond_ty, cond_exp) = traverse(&cond, current_func, ftable)?;
                if cond_ty == Bool {
                    let true_arm = traverse(&true_arm, current_func, ftable)?;
                    let false_arm = traverse(&false_arm, current_func, ftable)?;
                    if true_arm.0 == false_arm.0 {
                        Ok((
                            true_arm.0,
                            Box::new(If(
                                SM {
                                    ln: *ln,
                                    ty: true_arm.0,
                                },
                                cond_exp,
                                true_arm.1,
                                false_arm.1,
                            )),
                        ))
                    } else {
                        Err(format!(
                            "L{}: If expression has mismatching arms: expected {} got {}",
                            ln, true_arm.0, false_arm.0
                        ))
                    }
                } else {
                    Err(format!(
                        "L{}: Expected boolean expression in if conditional, got: {}",
                        ln, cond_ty
                    ))
                }
            }
            Bind(l, name, p, ref exp) => match current_func {
                Some(cf) => {
                    let rhs = traverse(exp, current_func, ftable)?;
                    if *p == rhs.0 {
                        ftable
                            .funcs
                            .get_mut(cf)
                            .ok_or(format!(
                                "L{}: CRITICAL: Function {} not found in function table",
                                l, cf
                            ))?
                            .vars
                            .add_var(&name, *p)
                            .map_err(|msg| format!("L{}: {}", l, msg))?;
                        Ok((
                            *p,
                            Box::new(Bind(SM { ln: *l, ty: *p }, name.clone(), *p, rhs.1)),
                        ))
                    } else {
                        Err(format!("L{}: Bind expected {} but got {}", l, p, rhs.0))
                    }
                }
                None => Err(format!(
                    "L{}: Attempting to bind variable {} outside of function",
                    l, name
                )),
            },
            Return(l, None) => match current_func {
                None => Err(format!("L{}: Return called outside of a function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    if fty == Unit {
                        Ok((Unit, Box::new(Return(SM { ln: *l, ty: Unit }, None))))
                    } else {
                        Err(format!("L{}: Return expected {} type and got unit", l, fty))
                    }
                }
            },
            Return(l, Some(exp)) => match current_func {
                None => Err(format!("L{}: Return appears outside of a function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let val = traverse(&exp, current_func, ftable)?;
                    if fty == val.0 {
                        Ok((fty, Box::new(Return(SM { ln: *l, ty: fty }, Some(val.1)))))
                    } else {
                        Err(format!("L{}: Return expected {} but got {}", l, fty, val.0))
                    }
                }
            },
            Yield(l, box exp) => match current_func {
                None => Err(format!("L{}: Yield appears outside of function", l)),
                Some(cf) => match exp {
                    Ast::Identifier(l, coname) => {
                        let coty = ftable
                            .funcs
                            .get(cf)
                            .map(|fi| fi.vars.vars.iter().find(|v| v.name == *coname))
                            .flatten()
                            .map(|vd| vd.ty)
                            .ok_or(format!("L{}: Could not find coroutine: {}", l, coname))?;
                        Ok((
                            coty,
                            Box::new(Yield(
                                SM { ln: *l, ty: coty },
                                Box::new(Identifier(SM { ln: *l, ty: coty }, coname.clone())),
                            )),
                        ))
                    }
                    _ => Err(format!("L{}: Expected name of coroutine after yield", l)),
                },
            },
            YieldReturn(l, None) => match current_func {
                None => Err(format!("L{}: YRet appears outside of function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    if fty == Unit {
                        Ok((Unit, Box::new(YieldReturn(SM { ln: *l, ty: fty }, None))))
                    } else {
                        Err(format!(
                            "L{}: Yield return expected {} but got unit",
                            l, fty
                        ))
                    }
                }
            },
            YieldReturn(l, Some(exp)) => match current_func {
                None => Err(format!("L{}: YRet appears outside of function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let val = traverse(&exp, current_func, ftable)?;
                    if fty == val.0 {
                        Ok((
                            fty,
                            Box::new(YieldReturn(SM { ln: *l, ty: fty }, Some(val.1))),
                        ))
                    } else {
                        Err(format!(
                            "L{}: Yield return expected {} but got {}",
                            l, fty, val.0
                        ))
                    }
                }
            },
            FunctionDef(ln, fname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = traverse(stmt, &Some(fname.clone()), ftable)?;
                    nbody.push(*r.1);
                }
                Ok((
                    *p,
                    Box::new(FunctionDef(
                        SM { ln: *ln, ty: *p },
                        fname.clone(),
                        params.clone(),
                        *p,
                        nbody,
                    )),
                ))
            }
            CoroutineDef(ln, coname, params, p, body) => {
                let mut nbody = vec![];
                for stmt in body.iter() {
                    let r = traverse(stmt, &Some(coname.clone()), ftable)?;
                    nbody.push(*r.1);
                }
                Ok((
                    *p,
                    Box::new(CoroutineDef(
                        SM { ln: *ln, ty: *p },
                        coname.clone(),
                        params.clone(),
                        *p,
                        nbody,
                    )),
                ))
            }
            FunctionCall(l, fname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut pty = vec![];
                let mut nparams = vec![];
                for param in params.iter() {
                    let (ty, np) = traverse(param, current_func, ftable)?;
                    pty.push(ty);
                    nparams.push(*np);
                }
                let fpty: Vec<super::Primitive> = (ftable
                    .funcs
                    .get(fname)
                    .ok_or(format!("L{}: Unknown identifer or function: {}", l, fname))?)
                .params
                .iter()
                .map(|(_, p)| *p)
                .collect();
                if pty.len() != fpty.len() {
                    Err(format!(
                        "L{}: Incorrect number of parameters passed to function: {}",
                        l, fname
                    ))
                } else {
                    let z = pty.iter().zip(fpty.iter());
                    let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                    if all_params_match {
                        let fty = ftable.funcs[fname].ty;

                        Ok((
                            fty,
                            Box::new(FunctionCall(SM { ln: *l, ty: fty }, fname.clone(), nparams)),
                        ))
                    } else {
                        Err(format!(
                            "L{}: One or more parameters had mismatching types for function {}",
                            l, fname
                        ))
                    }
                }
            }
            CoroutineInit(l, coname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut pty = vec![];
                let mut nparams = vec![];
                for param in params.iter() {
                    let (ty, np) = traverse(param, current_func, ftable)?;
                    pty.push(ty);
                    nparams.push(*np);
                }
                let fpty: Vec<super::Primitive> = ftable.funcs[coname]
                    .params
                    .iter()
                    .map(|(_, p)| *p)
                    .collect();
                if pty.len() != fpty.len() {
                    Err(format!(
                        "L{}: Incorrect number of parameters passed to coroutine: {}",
                        l, coname
                    ))
                } else {
                    let z = pty.iter().zip(fpty.iter());
                    let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                    if all_params_match {
                        let fty = ftable.funcs[coname].ty;
                        Ok((
                            fty,
                            Box::new(CoroutineInit(
                                SM { ln: *l, ty: fty },
                                coname.clone(),
                                nparams,
                            )),
                        ))
                    } else {
                        Err(format!(
                            "L{}: Mismatching parameter types in init for coroutine {}",
                            l, coname
                        ))
                    }
                }
            }
            Printi(l, exp) => {
                let val = traverse(&exp, current_func, ftable)?;
                if val.0 == I32 {
                    Ok((Bool, Box::new(Printi(SM { ln: *l, ty: val.0 }, val.1))))
                } else {
                    Err(format!("L{}: Expected i32 for printi got {}", l, val.0))
                }
            }
            Printiln(l, exp) => {
                let val = traverse(&exp, current_func, ftable)?;
                if val.0 == I32 {
                    Ok((Bool, Box::new(Printiln(SM { ln: *l, ty: val.0 }, val.1))))
                } else {
                    Err(format!("L{}: Expected i32 for printi got {}", l, val.0))
                }
            }
            Printbln(l, exp) => {
                let val = traverse(&exp, current_func, ftable)?;
                if val.0 == Bool {
                    Ok((Bool, Box::new(Printbln(SM { ln: *l, ty: val.0 }, val.1))))
                } else {
                    Err(format!("L{}: Expected bool for printb got {}", l, val.0))
                }
            }
            Module(ln, funcs, cors) => {
                let mut nfuncs = vec![];
                for func in funcs.iter() {
                    nfuncs.push(*traverse(func, &None, ftable)?.1);
                }
                let mut ncors = vec![];
                for cor in cors.iter() {
                    ncors.push(*traverse(cor, &None, ftable)?.1);
                }
                Ok((
                    Unit,
                    Box::new(Module(SM { ln: *ln, ty: Unit }, nfuncs, ncors)),
                ))
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use crate::parser::{Ast, Primitive};

        #[test]
        pub fn test_integer() {
            let node = Ast::Integer(1, 5);
            let mut ft = FunctionTable::new();
            let ty = traverse(&node, &None, &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_main".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &None, &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Err("L1: + expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &None, &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(1, 10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Integer(1, 10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
                assert_eq!(ty, Err("L1: * expected operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&test, &None, &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&test, &None, &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_func".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft).map(|(ty, _)| ty);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![]);
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_co".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft).map(|(ty, _)| ty);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![]);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_co".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft).map(|(ty, _)| ty);
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Ast::YieldReturn(1, None);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &Some("my_main".into()), &mut ft).map(|(ty, _)| ty);
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
            let ty = traverse(&node, &None, &mut ft).map(|(ty, _)| ty);
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::FunctionDef(1, "my_func".into(), vec![], I32, vec![Ast::Return(1, None)]);
            let ty = traverse(&node, &None, &mut ft);
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
            let ty = traverse(&node, &None, &mut ft).map(|(ty, _)| ty);
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::CoroutineDef(1, "my_co".into(), vec![], I32, vec![Ast::Return(1, None)]);
            let ty = traverse(&node, &None, &mut ft);
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
                let result = traverse(&node, &Some("my_main".into()), &mut ft).map(|(ty, _)| ty);
                assert_eq!(result, ex);
            }
        }
    }
}
