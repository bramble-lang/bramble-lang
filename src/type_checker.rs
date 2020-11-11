use std::collections;

use crate::parser;
use parser::{Ast, PNode, Primitive};

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub size: i32,
    pub frame_offset: i32,
    pub ty: Primitive,
}

impl VarDecl {
    pub fn new(name: String, ty: Primitive, size: i32, frame_offset: i32) -> VarDecl {
        VarDecl {
            name,
            size,
            frame_offset,
            ty,
        }
    }
}
#[derive(Debug)]
pub struct VarTable {
    pub vars: Vec<VarDecl>,
}

impl VarTable {
    pub fn add_var(&mut self, name: &str, ty: Primitive) -> Result<(), String> {
        // check if variable is already declared
        if self.vars.iter().find(|vd| vd.name == name).is_some() {
            return Err(format!("{} already declared", name));
        }

        // Compute the frame offste and add the variable to the var table
        let size = 4;
        let frame_offset: i32 = self.vars.iter().map(|vd| vd.size).sum::<i32>() + size;
        self.vars.push(VarDecl {
            name: name.into(),
            size,
            frame_offset,
            ty,
        });
        Ok(())
    }
}

impl VarTable {
    pub fn new() -> VarTable {
        VarTable { vars: vec![] }
    }
    pub fn generate(ast: &PNode) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        match &ast {
            Ast::FunctionDef(_, _, params, _, _stmts) => {
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
                }

                /*for stmt in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(stmt, &mut vt, offset);
                }*/
            }
            Ast::CoroutineDef(_, _, params, _, _stmts) => {
                offset += 20;
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
                }

                /*for n in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(n, &mut vt, offset);
                }*/
            }
            _ => {}
        }
        if VarTable::has_duplicates(&vt) {
            panic!("An identifier was defined twice");
        }
        let unknowns = VarTable::check_for_unknown_types(&vt);
        if unknowns.len() > 0 {
            for v in unknowns {
                println!("Type Checker: {} has an unknown type", v.name);
            }
            panic!("An identifier(s) with an unknown type was found");
        }
        vt
    }

    fn find_bound_identifiers(ast: &PNode, output: &mut VarTable, total_offset: i32) -> i32 {
        match ast {
            Ast::Bind(_, id, id_type, _) => {
                output
                    .vars
                    .push(VarDecl::new(id.clone(), *id_type, 4, total_offset + 4));
                total_offset + 4
            }
            _ => total_offset,
        }
    }

    fn has_duplicates(var_table: &VarTable) -> bool {
        (1..var_table.vars.len()).any(|i| var_table.vars[i..].contains(&var_table.vars[i - 1]))
    }

    fn check_for_unknown_types(var_table: &VarTable) -> Vec<&VarDecl> {
        var_table
            .vars
            .iter()
            .filter(|var_decl| var_decl.ty == Primitive::Unknown)
            .collect()
    }
}

#[derive(Debug)]
pub struct FunctionTable {
    pub funcs: collections::HashMap<String, FunctionInfo>,
}

impl FunctionTable {
    pub fn new() -> FunctionTable {
        FunctionTable {
            funcs: collections::HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub params: Vec<(String, Primitive)>,
    pub vars: VarTable,
    pub label_count: u32,
    pub ty: Primitive,
}

impl FunctionTable {
    pub fn generate(ast: &PNode) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: collections::HashMap::new(),
        };

        match &ast {
            Ast::Module(_, functions, coroutines) => {
                for f in functions.iter() {
                    FunctionTable::traverse(f, &mut ft);
                }
                for co in coroutines.iter() {
                    FunctionTable::traverse(co, &mut ft);
                }
            }
            _ => panic!("Type analysis: expected Module at root level of the AST"),
        }

        ft
    }

    fn traverse(ast: &PNode, ft: &mut FunctionTable) {
        match &ast {
            Ast::FunctionDef(_, fn_name, params, ty, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                        ty: *ty,
                    },
                );
            }
            Ast::CoroutineDef(_, fn_name, params, ty, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                        ty: *ty,
                    },
                );
            }
            _ => panic!("Type analysis: invalid function"),
        }
    }
}

pub mod checker {
    use crate::parser::{Ast, PNode, Primitive};
    use Primitive::*;

    use super::FunctionTable;

    pub fn type_check(ast: &PNode, ftable: &mut FunctionTable) -> Result<Primitive, String> {
        traverse(ast, &None, ftable)
    }

    fn traverse(
        ast: &PNode,
        current_func: &Option<String>,
        ftable: &mut FunctionTable,
    ) -> Result<Primitive, String> {
        use Ast::*;
        match &ast {
            Integer(_) => Ok(I32),
            Boolean(_) => Ok(Bool),
            IdentifierDeclare(_, _, p) => Ok(*p),
            Identifier(l, id) => match current_func {
                None => Err(format!(
                    "L{}: Variable {} appears outside of function",
                    l, id
                )),
                Some(cf) => ftable
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
                    ),
            },
            Primitive(p) => Ok(*p),
            ExpressionBlock(_, body) => {
                let mut ty = Unit;
                for stmt in body.iter() {
                    ty = traverse(stmt, current_func, ftable)?;
                }
                Ok(ty)
            }
            Statement(_, exp) => {
                traverse(exp, current_func, ftable)?;
                Ok(Unit)
            }
            Mul(ln, ref l, ref r) | Add(ln, ref l, ref r) => {
                let lty = traverse(l, current_func, ftable)?;
                let rty = traverse(r, current_func, ftable)?;
                match (lty, rty) {
                    (I32, I32) => Ok(I32),
                    _ => Err(format!("L{}: */+ expect to have operands of i32", ln)),
                }
            }
            BAnd(ln, ref l, ref r) | BOr(ln, ref l, ref r) => {
                let lty = traverse(l, current_func, ftable)?;
                let rty = traverse(r, current_func, ftable)?;
                match (lty, rty) {
                    (Bool, Bool) => Ok(Bool),
                    _ => Err(format!(
                        "L{}: && and || expect to have operands of bool",
                        ln
                    )),
                }
            }
            Ast::Eq(ln, ref l, ref r) | Ast::NEq(ln, ref l, ref r) => {
                let lty = traverse(l, current_func, ftable);
                let rty = traverse(r, current_func, ftable);
                match (lty, rty) {
                    (Ok(lty), Ok(rty)) => {
                        if lty == rty {
                            Ok(Bool)
                        } else {
                            Err(format!(
                                "L{}: Expected {:?} after == but got {:?}",
                                ln, lty, rty
                            ))
                        }
                    }
                    _ => Err(format!(
                        "L{}: && and || expect to have operands of bool",
                        ln
                    )),
                }
            }
            GrEq(ln, ref l, ref r)
            | Gr(ln, ref l, ref r)
            | Ls(ln, ref l, ref r)
            | LsEq(ln, ref l, ref r) => {
                let lty = traverse(l, current_func, ftable);
                let rty = traverse(r, current_func, ftable);
                match (lty, rty) {
                    (Ok(I32), Ok(I32)) => Ok(Bool),
                    _ => Err(format!(
                        "L{}: >=, > , <, and <= expect to have operands of i32",
                        ln
                    )),
                }
            }
            If(l, cond, true_arm, false_arm) => {
                let cond_ty = traverse(&cond, current_func, ftable);
                if cond_ty == Ok(Bool) {
                    let true_ty = traverse(&true_arm, current_func, ftable);
                    let false_ty = traverse(&false_arm, current_func, ftable);
                    match (true_ty, false_ty) {
                        (Ok(true_ty), Ok(false_ty)) => {
                            if true_ty == false_ty {
                                Ok(true_ty)
                            } else {
                                Err(format!(
                                    "L{}: If expression has mismatching arms: expected {:?} got {:?}",
                                    l, true_ty, false_ty
                                ))
                            }
                        }
                        (Err(msg), Ok(_)) => Err(format!("True arm of if expression: {:?}", msg)),
                        (Ok(_), Err(msg)) => Err(format!("False arm of if expression: {:?}", msg)),
                        (Err(msg1), Err(msg2)) => Err(format!(
                            "L{}: Errors in if expression arms: {:?} and {:?}",
                            l, msg1, msg2
                        )),
                    }
                } else {
                    Err(format!(
                        "L{}: Expected boolean expression in if conditional, got: {:?}",
                        l, cond_ty
                    ))
                }
            }
            Bind(l, name, p, ref exp) => match current_func {
                Some(cf) => {
                    let ety = traverse(exp, current_func, ftable)?;
                    if *p == ety {
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
                        Ok(*p)
                    } else {
                        Err(format!("L{}: Bind expected {:?} but got {:?}", l, p, ety))
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
                        Ok(Unit)
                    } else {
                        Err(format!(
                            "L{}: Return expected {:?} type and got Unit",
                            l, fty
                        ))
                    }
                }
            },
            Return(l, Some(exp)) => match current_func {
                None => Err(format!("L{}: Return appears outside of a function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let rty = traverse(&exp, current_func, ftable).unwrap();
                    if fty == rty {
                        Ok(fty)
                    } else {
                        Err(format!(
                            "L{}: Return expected {:?} but got {:?}",
                            l, fty, rty
                        ))
                    }
                }
            },
            Yield(l, box exp) => match current_func {
                None => Err(format!("L{}: Yield appears outside of function", l)),
                Some(cf) => match exp {
                    Ast::Identifier(l, coname) => ftable
                        .funcs
                        .get(cf)
                        .map(|fi| fi.vars.vars.iter().find(|v| v.name == *coname))
                        .flatten()
                        .map(|vd| vd.ty)
                        .ok_or(format!("L{}: Could not find coroutine: {}", l, coname)),
                    _ => Err(format!("L{}: Expected name of coroutine after yield", l)),
                },
            },
            YieldReturn(l, None) => match current_func {
                None => Err(format!("L{}: YRet appears outside of function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    if fty == Unit {
                        Ok(Unit)
                    } else {
                        Err(format!(
                            "L{}: Yield return expected {:?} but got Unit",
                            l, fty
                        ))
                    }
                }
            },
            YieldReturn(l, Some(exp)) => match current_func {
                None => Err(format!("L{}: YRet appears outside of function", l)),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let yty = traverse(&exp, current_func, ftable).unwrap();
                    if fty == yty {
                        Ok(fty)
                    } else {
                        Err(format!(
                            "L{}: Yield return expected {:?} but got {:?}",
                            l, fty, yty
                        ))
                    }
                }
            },
            FunctionDef(_, fname, _, p, body) | CoroutineDef(_, fname, _, p, body) => {
                for stmt in body.iter() {
                    traverse(stmt, &Some(fname.clone()), ftable)?;
                }
                Ok(*p)
            }
            FunctionCall(l, fname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let pty: Vec<super::Primitive> = params
                    .iter()
                    .map(|p| traverse(p, current_func, ftable).unwrap())
                    .collect();
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
                        Ok(ftable.funcs[fname].ty)
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
                let pty: Vec<super::Primitive> = params
                    .iter()
                    .map(|p| traverse(p, current_func, ftable).unwrap())
                    .collect();
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
                        Ok(ftable.funcs[coname].ty)
                    } else {
                        Err(format!(
                            "L{}: Mismatching parameter types in init for coroutine {}",
                            l, coname
                        ))
                    }
                }
            }
            Printi(l, exp) | Printiln(l, exp) => {
                let ty = traverse(&exp, current_func, ftable)?;
                if ty == I32 {
                    Ok(I32)
                } else {
                    Err(format!("L{}: Expected i32 for printi got {:?}", l, ty))
                }
            }
            Printbln(l, exp) => {
                let ty = traverse(&exp, current_func, ftable)?;
                if ty == Bool {
                    Ok(Bool)
                } else {
                    Err(format!("L{}: Expected bool for printb got {:?}", l, ty))
                }
            }
            Module(_, funcs, cors) => {
                for func in funcs.iter() {
                    traverse(func, &None, ftable)?;
                }
                for cor in cors.iter() {
                    traverse(cor, &None, ftable)?;
                }
                Ok(Unit)
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::super::*;
        use super::*;
        use crate::parser::{Ast, Primitive};

        #[test]
        pub fn test_integer() {
            let node = Ast::Integer(5);
            let mut ft = FunctionTable::new();
            let ty = traverse(&node, &None, &mut ft);
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
            let ty = traverse(&node, &Some("my_main".into()), &mut ft);
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
                let node = Ast::Add(1, Box::new(Ast::Integer(5)), Box::new(Ast::Integer(10)));
                let ty = traverse(&node, &None, &mut ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Integer(10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Add(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
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
                let node = Ast::Mul(1, Box::new(Ast::Integer(5)), Box::new(Ast::Integer(10)));
                let ty = traverse(&node, &None, &mut ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Integer(10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Integer(10)),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Ast::Mul(
                    1,
                    Box::new(Ast::Identifier(1, "b".into())),
                    Box::new(Ast::Identifier(1, "b".into())),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: */+ expect to have operands of i32".into()));
            }
        }

        #[test]
        pub fn test_boolean_ops() {
            let mut ft = FunctionTable::new();
            let tests: Vec<(PNode, Result<Primitive, String>)> = vec![(
                Ast::BAnd(1, Box::new(Ast::Boolean(true)), Box::new(Ast::Integer(5))),
                Err("L1: && and || expect to have operands of bool".into()),
            )];

            for (test, expected) in tests.iter() {
                let ty = traverse(&test, &None, &mut ft);
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_bind() {
            // RHS type matches the LHS type
            {
                let node = Ast::Bind(1, "x".into(), Primitive::I32, Box::new(Ast::Integer(5)));
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // RHS type does not match LHS type
            {
                let node = Ast::Bind(1, "x".into(), Primitive::Bool, Box::new(Ast::Integer(5)));
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("L1: Bind expected Bool but got I32".into()));
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
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
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
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
            let ty = traverse(&node, &Some("my_func".into()), &mut ft);
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
            let node = Ast::Return(1, Some(Box::new(Ast::Integer(5))));
            let ty = traverse(&node, &Some("my_func".into()), &mut ft);
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
            let ty = traverse(&node, &Some("my_func".into()), &mut ft);
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
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![Ast::Integer(5)]);
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::FunctionCall(1, "my_func2".into(), vec![]);
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft);
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
            let ty = traverse(&node, &Some("my_co".into()), &mut ft);
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
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![Ast::Integer(5)]);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Ast::CoroutineInit(1, "my_co2".into(), vec![]);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
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
            let ty = traverse(&node, &Some("my_co".into()), &mut ft);
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
            let node = Ast::YieldReturn(1, Some(Box::new(Ast::Integer(5))));
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Ast::YieldReturn(1, None);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Err("L1: Yield return expected I32 but got Unit".into()));
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
            let ty = traverse(&node, &Some("my_main".into()), &mut ft);
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
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(5))))],
            );
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::FunctionDef(1, "my_func".into(), vec![], I32, vec![Ast::Return(1, None)]);
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Err("L1: Return expected I32 type and got Unit".into()));
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
                vec![Ast::Return(1, Some(Box::new(Ast::Integer(5))))],
            );
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Ok(I32));

            let node =
                Ast::CoroutineDef(1, "my_co".into(), vec![], I32, vec![Ast::Return(1, None)]);
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Err("L1: Return expected I32 type and got Unit".into()));
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
                    Ast::Boolean(true),
                    Ast::Integer(5),
                    Ast::Integer(7),
                    Ok(I32),
                ),
                (
                    Ast::Boolean(true),
                    Ast::Boolean(true),
                    Ast::Boolean(true),
                    Ok(Bool),
                ),
                (
                    Ast::Integer(13),
                    Ast::Integer(5),
                    Ast::Integer(7),
                    Err("L1: Expected boolean expression in if conditional, got: Ok(I32)".into()),
                ),
                (
                    Ast::Boolean(true),
                    Ast::Integer(5),
                    Ast::Boolean(true),
                    Err("L1: If expression has mismatching arms: expected I32 got Bool".into()),
                ),
                (
                    Ast::Boolean(true),
                    Ast::Boolean(true),
                    Ast::Integer(5),
                    Err("L1: If expression has mismatching arms: expected Bool got I32".into()),
                ),
            ] {
                let node = Ast::If(1, Box::new(c), Box::new(t), Box::new(f));
                let ty = traverse(&node, &Some("my_main".into()), &mut ft);
                assert_eq!(ty, ex);
            }
        }
    }
}
