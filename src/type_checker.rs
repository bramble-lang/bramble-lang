use crate::Node;

#[derive(Debug)]
pub struct VarTable {
    pub vars: Vec<(String, i32, i32)>,
}

impl VarTable {
    pub fn generate(ast: &Node) -> VarTable {
        let mut vt = VarTable { vars: vec![] };
        let mut offset = 0;
        match ast {
            Node::FunctionDef(_, params, stmts) => {
                for p in params.iter() {
                    offset += 4;
                    vt.vars.push((p.clone(), 4, offset));
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
        vt
    }

    fn find_bound_identifiers(ast: &Node, output: &mut VarTable, total_offset: i32) -> i32 {
        match ast {
            Node::Bind(id, _) => {
                output.vars.push((id.clone(), 4, total_offset + 4));
                total_offset + 4
            }
            _ => total_offset,
        }
    }

    fn has_duplicates(var_table: &VarTable) -> bool {
        (1..var_table.vars.len()).any(|i| var_table.vars[i..].contains(&var_table.vars[i - 1]))
    }
}

#[derive(Debug)]
pub struct FunctionTable {
    pub funcs: std::collections::HashMap<String, FunctionInfo>,
}

#[derive(Debug)]
pub struct FunctionInfo {
    pub params: Vec<String>,
    pub vars: VarTable,
    pub label_count: u32,
}

impl FunctionTable {
    pub fn generate(ast: &Node) -> FunctionTable {
        let mut ft = FunctionTable {
            funcs: std::collections::HashMap::new(),
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
            Node::FunctionDef(fn_name, params, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: params.clone(),
                        vars,
                        label_count: 0,
                    },
                );
            }
            Node::CoroutineDef(fn_name, _) => {
                let vars = VarTable::generate(ast);
                ft.funcs.insert(
                    fn_name.clone(),
                    FunctionInfo {
                        params: vec![],
                        vars,
                        label_count: 0,
                    },
                );
            }
            _ => panic!("Type analysis: invalid function"),
        }
    }
}
