use std::collections;

use crate::parser;
use crate::semantics::symbol_table::*;
use crate::semantics::semanticnode::SemanticNode;
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
    pub fn generate<I>(ast: &Ast<I>) -> VarTable {
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

    pub fn get_var(&self, func: &str, id: &str) -> Result<&VarDecl, String> {
        Ok(self
            .funcs
            .get(func)
            .ok_or(format!("Undefined function {}", func))?
            .vars
            .vars
            .iter()
            .find(|v| v.name == id)
            .ok_or(format!("Variable {} not declared", id))?)
    }

    pub fn add_var(&mut self, func: &str, id: &str, ty: Primitive) -> Result<(), String> {
        self.funcs
            .get_mut(func)
            .ok_or(format!(
                "CRITICAL: Function {} not found in function table",
                func
            ))?
            .vars
            .add_var(id, ty)
            .map_err(|msg| format!("{}", msg))?;
        Ok(())
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

    pub fn from_semantic_ast(ast: &SemanticNode) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: collections::HashMap::new(),
        };

        match &ast {
            Ast::Module(_, functions, coroutines) => {
                for f in functions.iter() {
                    FunctionTable::traverse_semantic_ast(f, &mut ft, None);
                }
                for co in coroutines.iter() {
                    FunctionTable::traverse_semantic_ast(co, &mut ft, None);
                }
            }
            _ => panic!("Type analysis: expected Module at root level of the AST"),
        }

        ft
    }

    fn traverse_semantic_ast(
        ast: &SemanticNode,
        ft: &mut FunctionTable,
        current_func: Option<&str>,
    ) {
        match ast {
            Ast::FunctionDef(meta, name, params, ret_ty, body) => {
                let vars = VarTable::new();
                ft.funcs.insert(
                    name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                        ty: *ret_ty,
                    },
                );
                FunctionTable::add_symbol_table(ft, name, &meta.sym).unwrap();
                FunctionTable::traverse_semantic_ast(&body[0], ft, Some(name));
            }
            Ast::CoroutineDef(_, name, params, ret_ty, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                        ty: *ret_ty,
                    },
                );
            }
            Ast::ExpressionBlock(meta, body) => {
                let cf = current_func.unwrap();
                for sym in meta.sym.table().iter() {
                    match sym.ty {
                        Type::Primitive(p) => ft.add_var(cf, &sym.name, p).unwrap(),
                        _ => (),
                    };
                }
                body.iter()
                    .for_each(|exp| FunctionTable::traverse_semantic_ast(exp, ft, current_func));
            }
            Ast::Bind(_, _, _, exp) => {
                FunctionTable::traverse_semantic_ast(exp, ft, current_func);
            }
            Ast::If(_, cond, true_arm, false_arm) => {
                FunctionTable::traverse_semantic_ast(cond, ft, current_func);
                FunctionTable::traverse_semantic_ast(true_arm, ft, current_func);
                FunctionTable::traverse_semantic_ast(false_arm, ft, current_func);
            }
            Ast::FunctionCall(_, _, params) | Ast::CoroutineInit(_, _, params) => {
                params
                    .iter()
                    .for_each(|exp| FunctionTable::traverse_semantic_ast(exp, ft, current_func));
            }
            Ast::Mul(_, l, r)
            | Ast::Add(_, l, r)
            | Ast::BAnd(_, l, r)
            | Ast::BOr(_, l, r)
            | Ast::Eq(_, l, r)
            | Ast::NEq(_, l, r)
            | Ast::Gr(_, l, r)
            | Ast::GrEq(_, l, r)
            | Ast::Ls(_, l, r)
            | Ast::LsEq(_, l, r) => {
                FunctionTable::traverse_semantic_ast(l, ft, current_func);
                FunctionTable::traverse_semantic_ast(r, ft, current_func);
            }
            Ast::Printi(_, exp) | Ast::Printiln(_, exp) | Ast::Printbln(_, exp) => {
                FunctionTable::traverse_semantic_ast(exp, ft, current_func);
            }
            Ast::Return(_, Some(exp)) | Ast::Yield(_, exp) | Ast::YieldReturn(_, Some(exp)) => {
                FunctionTable::traverse_semantic_ast(exp, ft, current_func);
            }
            Ast::Return(_, None) | Ast::YieldReturn(_, None) => {}
            Ast::Identifier(_, _) | Ast::Integer(_, _) | Ast::Boolean(_, _) => {}
            _ => println!("Dunno: {}", ast.root_str()), //panic!("Type analysis: invalid function, coroutine, or expression block: {}", ast.root_str()),
        }
    }

    fn add_symbol_table(
        ft: &mut FunctionTable,
        func: &str,
        table: &SymbolTable,
    ) -> Result<(), String> {
        for sym in table.table().iter() {
            match sym.ty {
                Type::Primitive(p) => ft.add_var(func, &sym.name, p)?,
                _ => (),
            };
        }
        Ok(())
    }
}
