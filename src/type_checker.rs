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
    pub fn new() -> VarTable {
        VarTable { vars: vec![] }
    }
    pub fn generate(ast: &Node) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        match ast {
            Node::FunctionDef(_, params, _, stmts) => {
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
                }

                for stmt in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(stmt, &mut vt, offset);
                }
            }
            Node::CoroutineDef(_, params, _, stmts) => {
                offset += 20;
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
                }

                for n in stmts.iter() {
                    offset = VarTable::find_bound_identifiers(n, &mut vt, offset);
                }
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

    pub fn type_check(ast: &Node, ftable: &FunctionTable) -> Result<Primitive, String> {
        traverse(ast, &None, ftable)
    }

    fn traverse(
        ast: &Node,
        current_func: &Option<String>,
        ftable: &FunctionTable,
    ) -> Result<Primitive, String> {
        use Node::*;
        match ast {
            Integer(_) => Ok(I32),
            Boolean(_) => Ok(Bool),
            Identifier(id, _) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(cf) => ftable.funcs[cf]
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
                let lty = traverse(l, current_func, ftable);
                let rty = traverse(r, current_func, ftable);
                match (lty, rty) {
                    (Ok(I32), Ok(I32)) => Ok(I32),
                    _ => Err("*/+ expect to have operands of i32".into()),
                }
            }
            Bind(_, p, exp) => {
                let ety = traverse(exp, current_func, ftable).unwrap();
                if *p == ety {
                    Ok(*p)
                } else {
                    Err(format!("Bind expected {:?} but got {:?}", *p, ety))
                }
            }
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
            Yield(exp) => traverse(exp, current_func, ftable),
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
                let fpty: Vec<parser::Primitive> =
                    ftable.funcs[fname].params.iter().map(|(_, p)| *p).collect();
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
            Print(exp) | Println(exp) => {
                let ty = traverse(exp, current_func, ftable);
                if ty == Ok(I32) {
                    Ok(I32)
                } else {
                    Err(format!("Expected i32 for print got {:?}", ty))
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
            let ft = FunctionTable::new();
            let ty = traverse(&node, &None, &ft);
            assert_eq!(ty, Ok(Primitive::I32));
        }

        #[test]
        pub fn test_identifier() {
            let node = Node::Identifier("x".into(), Primitive::Bool);
            let ft = FunctionTable::new();
            let ty = traverse(&node, &None, &ft);
            assert_eq!(ty, Ok(Primitive::Bool));
        }

        #[test]
        pub fn test_add() {
            // both operands are i32
            {
                let node = Node::Add(Box::new(Node::Integer(5)), Box::new(Node::Integer(10)));
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                    Box::new(Node::Integer(10)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Integer(10)),
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Add(
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                    Box::new(Node::Identifier("y".into(), Primitive::Bool)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
        }

        #[test]
        pub fn test_mul() {
            // both operands are i32
            {
                let node = Node::Mul(Box::new(Node::Integer(5)), Box::new(Node::Integer(10)));
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                    Box::new(Node::Integer(10)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Integer(10)),
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
            // operands are not i32
            {
                let node = Node::Mul(
                    Box::new(Node::Identifier("x".into(), Primitive::Bool)),
                    Box::new(Node::Identifier("y".into(), Primitive::Bool)),
                );
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("*/+ expect to have operands of i32".into()));
            }
        }

        #[test]
        pub fn test_bind() {
            // RHS type matches the LHS type
            {
                let node = Node::Bind("x".into(), Primitive::I32, Box::new(Node::Integer(5)));
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Ok(Primitive::I32));
            }

            // RHS type does not match LHS type
            {
                let node = Node::Bind("x".into(), Primitive::Bool, Box::new(Node::Integer(5)));
                let ft = FunctionTable::new();
                let ty = traverse(&node, &None, &ft);
                assert_eq!(ty, Err("Bind expected Bool but got I32".into()));
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
            let ty = traverse(&node, &Some("my_func".into()), &ft);
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
            let ty = traverse(&node, &Some("my_func".into()), &ft);
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
            let ty = traverse(&node, &Some("my_func".into()), &ft);
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
            let ty = traverse(&node, &Some("my_func2".into()), &ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Node::FunctionCall("my_func2".into(), vec![]);
            let ty = traverse(&node, &Some("my_func2".into()), &ft);
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
            let ty = traverse(&node, &Some("my_co".into()), &ft);
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
            let ty = traverse(&node, &Some("my_co2".into()), &ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect parameters passed in call
            let node = Node::CoroutineInit("my_co2".into(), vec![]);
            let ty = traverse(&node, &Some("my_co2".into()), &ft);
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
            let ty = traverse(&node, &Some("my_co".into()), &ft);
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
            let ty = traverse(&node, &Some("my_co2".into()), &ft);
            assert_eq!(ty, Ok(I32));

            // test incorrect type for yield return
            let node = Node::YieldReturn(None);
            let ty = traverse(&node, &Some("my_co2".into()), &ft);
            assert_eq!(ty, Err("Yield return expected I32 but got Unit".into()));
        }

        #[test]
        fn test_yield() {
            let mut ft = FunctionTable::new();
            ft.funcs.insert(
                "my_co2".into(),
                FunctionInfo {
                    params: vec![],
                    vars: VarTable::new(),
                    label_count: 0,
                    ty: I32,
                },
            );
            let node = Node::Yield(Box::new(Node::Identifier("my_co2".into(), I32)));
            let ty = traverse(&node, &None, &ft);
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
            let ty = traverse(&node, &None, &ft);
            assert_eq!(ty, Ok(I32));

            let node = Node::FunctionDef("my_func".into(), vec![], I32, vec![Node::Return(None)]);
            let ty = traverse(&node, &None, &ft);
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
            let ty = traverse(&node, &None, &ft);
            assert_eq!(ty, Ok(I32));

            let node = Node::FunctionDef("my_co".into(), vec![], I32, vec![Node::Return(None)]);
            let ty = traverse(&node, &None, &ft);
            assert_eq!(ty, Err("Return expected I32 type and got Unit".into()));
        }
    }
}
