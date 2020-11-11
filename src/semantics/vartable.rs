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
            }
            Ast::CoroutineDef(_, _, params, _, _stmts) => {
                offset += 20;
                for (param_name, param_ty) in params.iter() {
                    offset += 4;
                    vt.vars
                        .push(VarDecl::new(param_name.clone(), *param_ty, 4, offset));
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
