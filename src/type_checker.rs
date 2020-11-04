use std::collections;

use crate::parser::Primitive;
use crate::Node;

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
    pub fn generate(ast: &Node) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        match ast {
            Node::FunctionDef(_, params, _, _stmts) => {
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
                }

                /*for stmt in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(stmt, &mut vt, offset);
                }*/
            }
            Node::CoroutineDef(_, params, _, _stmts) => {
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

    fn find_bound_identifiers(ast: &Node, output: &mut VarTable, total_offset: i32) -> i32 {
        match ast {
            Node::Bind(id, id_type, _) => {
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
    pub fn generate(ast: &Node) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: collections::HashMap::new(),
        };

        match ast {
            Node::Module(functions, coroutines) => {
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

    fn traverse(ast: &Node, ft: &mut FunctionTable) {
        match ast {
            Node::FunctionDef(fn_name, params, ty, _) => {
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
            Node::CoroutineDef(fn_name, params, ty, _) => {
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

    use crate::parser::{self, Node, Primitive};
    use Primitive::*;

    use super::FunctionTable;

    pub fn type_check(ast: &Node, ftable: &mut FunctionTable) -> Result<Primitive, String> {
        traverse(ast, &None, ftable)
    }

    fn traverse(
        ast: &Node,
        current_func: &Option<String>,
        ftable: &mut FunctionTable,
    ) -> Result<Primitive, String> {
        use Node::*;
        match ast {
            Integer(_) => Ok(I32),
            Boolean(_) => Ok(Bool),
            Identifier(id, _) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(cf) => ftable.funcs.get(cf).ok_or(format!("Undefined function {}", cf))?
                    .vars
                    .vars
                    .iter()
                    .find(|v| v.name == *id)
                    .map_or_else(
                        || Err(format!("Variable {} not declared", id)),
                        |v| Ok(v.ty),
                    ),
            },
            Primitive(p) => Ok(*p),
            Mul(l, r) | Add(l, r) => {
                let lty = traverse(l, current_func, ftable)?;
                let rty = traverse(r, current_func, ftable)?;
                match (lty, rty) {
                    (I32, I32) => Ok(I32),
                    _ => Err("*/+ expect to have operands of i32".into()),
                }
            }
            BAnd(l, r) | BOr(l, r) => {
                let lty = traverse(l, current_func, ftable)?;
                let rty = traverse(r, current_func, ftable)?;
                match (lty, rty) {
                    (Bool, Bool) => Ok(Bool),
                    _ => Err("&& and || expect to have operands of bool".into()),
                }
            }
            Node::Eq(l, r) | Node::NEq(l, r) => {
                let lty = traverse(l, current_func, ftable);
                let rty = traverse(r, current_func, ftable);
                match (lty, rty) {
                    (Ok(lty), Ok(rty)) => {
                        if lty == rty {
                            Ok(Bool)
                        } else {
                            Err(format!("Expected {:?} after == but got {:?}", lty, rty))
                        }
                    }
                    _ => Err("&& and || expect to have operands of bool".into()),
                }
            }
            GrEq(l, r) | Gr(l, r) | Ls(l, r) | LsEq(l, r) => {
                let lty = traverse(l, current_func, ftable);
                let rty = traverse(r, current_func, ftable);
                match (lty, rty) {
                    (Ok(I32), Ok(I32)) => Ok(Bool),
                    _ => Err(">=, > , <, and <= expect to have operands of i32".into()),
                }
            }
            If(cond, true_arm, false_arm) => {
                let cond_ty = traverse(cond, current_func, ftable);
                if cond_ty == Ok(Bool) {
                    let true_ty = traverse(true_arm, current_func, ftable);
                    let false_ty = traverse(false_arm, current_func, ftable);
                    match (true_ty, false_ty) {
                        (Ok(true_ty), Ok(false_ty)) => {
                            if true_ty == false_ty {
                                Ok(true_ty)
                            } else {
                                Err(format!(
                                    "If expression has mismatching arms: expected {:?} got {:?}",
                                    true_ty, false_ty
                                ))
                            }
                        }
                        (Err(msg), Ok(_)) => Err(format!("True arm of if expression: {:?}", msg)),
                        (Ok(_), Err(msg)) => Err(format!("False arm of if expression: {:?}", msg)),
                        (Err(msg1), Err(msg2)) => Err(format!(
                            "Errors in if expression arms: {:?} and {:?}",
                            msg1, msg2
                        )),
                    }
                } else {
                    Err(format!(
                        "Expected boolean expression in if conditional, got: {:?}",
                        cond_ty
                    ))
                }
            }
            Bind(name, p, exp) => match current_func {
                Some(cf) => {
                    let ety = traverse(exp, current_func, ftable)?;
                    if *p == ety {
                        ftable
                            .funcs
                            .get_mut(cf)
                            .ok_or(format!(
                                "CRITICAL: Function {} not found in function table",
                                cf
                            ))?
                            .vars
                            .add_var(name, *p)?;
                        Ok(*p)
                    } else {
                        Err(format!("Bind expected {:?} but got {:?}", *p, ety))
                    }
                }
                None => Err(format!(
                    "Attempting to bind variable {} outside of function",
                    name
                )),
            },
            Return(None) => match current_func {
                None => Err("Return called outside of a function".into()),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    if fty == Unit {
                        Ok(Unit)
                    } else {
                        Err(format!("Return expected {:?} type and got Unit", fty))
                    }
                }
            },
            Return(Some(exp)) => match current_func {
                None => Err("Return appears outside of a function".into()),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let rty = traverse(exp, current_func, ftable).unwrap();
                    if fty == rty {
                        Ok(fty)
                    } else {
                        Err(format!("Return expected {:?} but got {:?}", fty, rty))
                    }
                }
            },
            Yield(box exp) => match current_func {
                None => Err("Yield appears outside of function".into()),
                Some(cf) => match exp {
                    Node::Identifier(coname, _) => ftable
                        .funcs
                        .get(cf)
                        .map(|fi| fi.vars.vars.iter().find(|v| v.name == *coname))
                        .flatten()
                        .map(|vd| vd.ty)
                        .ok_or(format!("Could not find coroutine: {}", coname)),
                    _ => Err("Expected name of coroutine after yield".into()),
                },
            },
            YieldReturn(None) => match current_func {
                None => Err("YRet appears outside of function".into()),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    if fty == Unit {
                        Ok(Unit)
                    } else {
                        Err(format!("Yield return expected {:?} but got Unit", fty))
                    }
                }
            },
            YieldReturn(Some(exp)) => match current_func {
                None => Err("YRet appears outside of function".into()),
                Some(cf) => {
                    let fty = ftable.funcs[cf].ty;
                    let yty = traverse(exp, current_func, ftable).unwrap();
                    if fty == yty {
                        Ok(fty)
                    } else {
                        Err(format!("Yield return expected {:?} but got {:?}", fty, yty))
                    }
                }
            },
            FunctionDef(fname, _, p, body) | CoroutineDef(fname, _, p, body) => {
                for stmt in body.iter() {
                    traverse(stmt, &Some(fname.clone()), ftable)?;
                }
                Ok(*p)
            }
            FunctionCall(fname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let pty: Vec<parser::Primitive> = params
                    .iter()
                    .map(|p| traverse(p, current_func, ftable).unwrap())
                    .collect();
                let fpty: Vec<parser::Primitive> = (ftable
                    .funcs
                    .get(fname)
                    .ok_or(format!("Unknown identifer or function: {}", fname))?)
                .params
                .iter()
                .map(|(_, p)| *p)
                .collect();
                if pty.len() != fpty.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to function: {}",
                        fname
                    ))
                } else {
                    let z = pty.iter().zip(fpty.iter());
                    let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                    if all_params_match {
                        Ok(ftable.funcs[fname].ty)
                    } else {
                        Err(format!(
                            "One or more parameters had mismatching types for function {}",
                            fname
                        ))
                    }
                }
            }
            CoroutineInit(coname, params) => {
                // test that the expressions passed to the function match the functions
                // parameter types
                let pty: Vec<parser::Primitive> = params
                    .iter()
                    .map(|p| traverse(p, current_func, ftable).unwrap())
                    .collect();
                let fpty: Vec<parser::Primitive> = ftable.funcs[coname]
                    .params
                    .iter()
                    .map(|(_, p)| *p)
                    .collect();
                if pty.len() != fpty.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to coroutine: {}",
                        coname
                    ))
                } else {
                    let z = pty.iter().zip(fpty.iter());
                    let all_params_match = z.map(|(up, fp)| up == fp).fold(true, |x, y| x && y);
                    if all_params_match {
                        Ok(ftable.funcs[coname].ty)
                    } else {
                        Err(format!(
                            "Mismatching parameter types in init for coroutine {}",
                            coname
                        ))
                    }
                }
            }
            Printi(exp) | Printiln(exp) => {
                let ty = traverse(exp, current_func, ftable)?;
                if ty == I32 {
                    Ok(I32)
                } else {
                    Err(format!("Expected i32 for printi got {:?}", ty))
                }
            }
            Printbln(exp) => {
                let ty = traverse(exp, current_func, ftable)?;
                if ty == Bool {
                    Ok(Bool)
                } else {
                    Err(format!("Expected bool for printb got {:?}", ty))
                }
            }
            Module(funcs, cors) => {
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
        use crate::parser::{Node, Primitive};

        #[test]
        pub fn test_integer() {
            let node = Node::Integer(5);
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

            let node = Node::Identifier("x".into(), Primitive::Bool);
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
                let node = Node::Add(Box::new(Node::Integer(5)), Box::new(Node::Integer(10)));
                let ty = traverse(&node, &None, &mut ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                    Box::new(Node::Integer(10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Integer(10)),
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
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
                let node = Node::Mul(Box::new(Node::Integer(5)), Box::new(Node::Integer(10)));
                let ty = traverse(&node, &None, &mut ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                    Box::new(Node::Integer(10)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Integer(10)),
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                    Box::new(Node::Identifier("b".into(), Primitive::Bool)),
                );
                let ty = traverse(&node, &Some("my_func".into()), &mut ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
        }

        #[test]
        pub fn test_boolean_ops() {
            let mut ft = FunctionTable::new();
            let tests: Vec<(Node, Result<Primitive, String>)> = vec![
                (
                    Node::BAnd(
                        Box::new(Node::Boolean(true)),
                        Box::new(Node::Boolean(false)),
                    ),
                    Ok(Primitive::Bool),
                ),
                (
                    Node::BAnd(Box::new(Node::Integer(5)), Box::new(Node::Boolean(false))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::BAnd(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::BAnd(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::BOr(
                        Box::new(Node::Boolean(true)),
                        Box::new(Node::Boolean(false)),
                    ),
                    Ok(Primitive::Bool),
                ),
                (
                    Node::BOr(Box::new(Node::Integer(5)), Box::new(Node::Boolean(false))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::BOr(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::BOr(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Err("&& and || expect to have operands of bool".into()),
                ),
                (
                    Node::Eq(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Ok(Bool),
                ),
                (
                    Node::Eq(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err("Expected Bool after == but got I32".into()),
                ),
                (
                    Node::Eq(Box::new(Node::Integer(5)), Box::new(Node::Boolean(true))),
                    Err("Expected I32 after == but got Bool".into()),
                ),
                (
                    Node::Gr(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Ok(Bool),
                ),
                (
                    Node::Gr(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::Gr(Box::new(Node::Integer(5)), Box::new(Node::Boolean(true))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::GrEq(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Ok(Bool),
                ),
                (
                    Node::GrEq(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::GrEq(Box::new(Node::Integer(5)), Box::new(Node::Boolean(true))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::Ls(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Ok(Bool),
                ),
                (
                    Node::Ls(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::Ls(Box::new(Node::Integer(5)), Box::new(Node::Boolean(true))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::LsEq(Box::new(Node::Integer(7)), Box::new(Node::Integer(5))),
                    Ok(Bool),
                ),
                (
                    Node::LsEq(Box::new(Node::Boolean(true)), Box::new(Node::Integer(5))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
                (
                    Node::LsEq(Box::new(Node::Integer(5)), Box::new(Node::Boolean(true))),
                    Err(">=, > , <, and <= expect to have operands of i32".into()),
                ),
            ];

            for (test, expected) in tests.iter() {
                let ty = traverse(&test, &None, &mut ft);
                assert_eq!(ty, *expected);
            }
        }

        #[test]
        pub fn test_bind() {
            // RHS type matches the LHS type
            {
                let node = Node::Bind("x".into(), Primitive::I32, Box::new(Node::Integer(5)));
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
                let node = Node::Bind("x".into(), Primitive::Bool, Box::new(Node::Integer(5)));
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
                assert_eq!(ty, Err("Bind expected Bool but got I32".into()));
            }
           
            // recursive definition
            {
                let node = Node::Bind("x".into(), Primitive::I32, Box::new(Node::Identifier("x".into(), I32)));
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
                assert_eq!(ty, Err("Variable x not declared".into()));
            }
           
            // use an unbound variable
            {
                let node =  Box::new(Node::Identifier("x".into(), I32));
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
                assert_eq!(ty, Err("Variable x not declared".into()));
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
            let node = Node::Return(None);
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
            let node = Node::Return(Some(Box::new(Node::Integer(5))));
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
            let node = Node::FunctionCall("my_func".into(), vec![]);
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
            let node = Node::FunctionCall("my_func2".into(), vec![Node::Integer(5)]);
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Node::FunctionCall("my_func2".into(), vec![]);
            let ty = traverse(&node, &Some("my_func2".into()), &mut ft);
            assert_eq!(
                ty,
                Err("Incorrect number of parameters passed to function: my_func2".into())
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
            let node = Node::CoroutineInit("my_co".into(), vec![]);
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
            let node = Node::CoroutineInit("my_co2".into(), vec![Node::Integer(5)]);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Node::CoroutineInit("my_co2".into(), vec![]);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(
                ty,
                Err("Incorrect number of parameters passed to coroutine: my_co2".into())
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
            let node = Node::YieldReturn(None);
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
            let node = Node::YieldReturn(Some(Box::new(Node::Integer(5))));
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Node::YieldReturn(None);
            let ty = traverse(&node, &Some("my_co2".into()), &mut ft);
            assert_eq!(ty, Err("Yield return expected I32 but got Unit".into()));
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
            let node = Node::Yield(Box::new(Node::Identifier("c".into(), I32)));
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

            let node = Node::FunctionDef(
                "my_func".into(),
                vec![],
                I32,
                vec![Node::Return(Some(Box::new(Node::Integer(5))))],
            );
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Ok(I32));

            let node = Node::FunctionDef("my_func".into(), vec![], I32, vec![Node::Return(None)]);
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Err("Return expected I32 type and got Unit".into()));
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

            let node = Node::CoroutineDef(
                "my_co".into(),
                vec![],
                I32,
                vec![Node::Return(Some(Box::new(Node::Integer(5))))],
            );
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Ok(I32));

            let node = Node::FunctionDef("my_co".into(), vec![], I32, vec![Node::Return(None)]);
            let ty = traverse(&node, &None, &mut ft);
            assert_eq!(ty, Err("Return expected I32 type and got Unit".into()));
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
            {
                let node = Node::If(
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Integer(5)),
                    Box::new(Node::Integer(7)),
                );
                let ty = traverse(&node, &Some("my_main".into()), &mut ft);
                assert_eq!(ty, Ok(I32));
            }
            {
                let node = Node::If(
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Boolean(false)),
                );
                let ty = traverse(&node, &Some("my_main".into()), &mut ft);
                assert_eq!(ty, Ok(Bool));
            }
            {
                let node = Node::If(
                    Box::new(Node::Integer(5)),
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Boolean(false)),
                );
                let ty = traverse(&node, &Some("my_main".into()), &mut ft);
                assert_eq!(
                    ty,
                    Err("Expected boolean expression in if conditional, got: Ok(I32)".into())
                );
            }
            {
                let node = Node::If(
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Boolean(true)),
                    Box::new(Node::Integer(5)),
                );
                let ty = traverse(&node, &Some("my_main".into()), &mut ft);
                assert_eq!(
                    ty,
                    Err("If expression has mismatching arms: expected Bool got I32".into())
                );
            }
        }
    }
}
