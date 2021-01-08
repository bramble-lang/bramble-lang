use crate::ast::{BinaryOperator, UnaryOperator};
use crate::semantics::semanticnode::{SemanticAst, SemanticNode};
use crate::semantics::symbol_table::*;
use crate::syntax::ast::Type;
use crate::syntax::ast::Type::*;
use crate::syntax::pnode::PNode;
use crate::{
    ast,
    diagnostics::config::{Tracing, TracingConfig},
};


pub fn type_check(ast: &PNode, trace: TracingConfig, trace_path: TracingConfig) -> Result<SemanticNode, String> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_parser_ast(&ast)?;
    SymbolTable::generate(&mut sm_ast)?;

    let mut root_table = SymbolTable::new();
    let mut semantic = SemanticAnalyzer::new(&sm_ast);
    semantic.set_tracing(trace);
    semantic.path_tracing = trace_path;
    let ast_typed = semantic
        .resolve_types(&mut root_table)
        .map_err(|e| format!("Semantic: {}", e))?;
    Ok(ast_typed)
}

pub struct SemanticAnalyzer<'a> {
    root: &'a SemanticNode,
    stack: ScopeStack,
    tracing: TracingConfig,
    path_tracing: TracingConfig,
}

impl<'a> Tracing for SemanticAnalyzer<'a> {
    fn set_tracing(&mut self, config: TracingConfig) {
        self.tracing = config;
    }
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(root: &'a SemanticNode) -> SemanticAnalyzer {
        SemanticAnalyzer {
            root,
            stack: ScopeStack::new(),
            tracing: TracingConfig::Off,
            path_tracing: TracingConfig::Off,
        }
    }

    fn trace(&self, node: &SemanticNode, current_func: &Option<String>, current_scope: &SymbolTable) {
        let md = node.get_metadata();
        let line = md.ln as usize;
        let print_trace = match self.tracing {
            TracingConfig::All => true,
            TracingConfig::After(start) if start <= line => true,
            TracingConfig::Before(end) if line <= end => true,
            TracingConfig::Between(start, end) if start <= line && line <= end => true,
            TracingConfig::Only(only) if line == only => true,
            _ => false,
        };
        let print_path = match self.path_tracing {
            TracingConfig::All => true,
            TracingConfig::After(start) if start <= line => true,
            TracingConfig::Before(end) if line <= end => true,
            TracingConfig::Between(start, end) if start <= line && line <= end => true,
            TracingConfig::Only(only) if line == only => true,
            _ => false,
        };

        if print_trace {
            let func = match current_func {
                Some(f) => format!("{}: ", f),
                None => "".into(),
            };
            println!("L{}: {}{}\n{}", line, func, node, self.stack);
        }

        if print_path {
            let func = match current_func {
                Some(f) => format!("{}: ", f),
                None => "".into(),
            };
            let path = self.stack.to_path(current_scope).map_or("[]".into(), |p| format!("{}", p));
            println!("L{}: {}{} <- {}", line, func, node, path);
        }
    }

    fn unary_op(
        &mut self,
        op: UnaryOperator,
        operand: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<(ast::Type, SemanticNode), String> {
        use UnaryOperator::*;

        let operand = self.traverse(operand, current_func, sym)?;

        match op {
            Minus => {
                if operand.get_type() == I32 {
                    Ok((I32, operand))
                } else {
                    Err(format!("{} expected i32 but found {}", op, operand.get_type()))
                }
            }
            Not => {
                if operand.get_type() == Bool {
                    Ok((Bool, operand))
                } else {
                    Err(format!("{} expected bool but found {}", op, operand.get_type()))
                }
            }
        }
    }

    fn binary_op(
        &mut self,
        op: BinaryOperator,
        l: &SemanticNode,
        r: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<(ast::Type, SemanticNode, SemanticNode), String> {
        use BinaryOperator::*;

        let l = self.traverse(l, current_func, sym)?;
        let r = self.traverse(r, current_func, sym)?;

        match op {
            Add | Sub | Mul | Div => {
                if l.get_type() == I32 && r.get_type() == I32 {
                    Ok((I32, l, r))
                } else {
                    Err(format!("{} expected i32 but found {} and {}", op, l.get_type(), r.get_type()))
                }
            }
            BAnd | BOr => {
                if l.get_type() == Bool && r.get_type() == Bool {
                    Ok((Bool, l, r))
                } else {
                    Err(format!(
                        "{} expected bool but found {} and {}",
                        op, l.get_type(), r.get_type()
                    ))
                }
            }
            Eq | NEq | Ls | LsEq | Gr | GrEq => {
                if l.get_type() == r.get_type() {
                    Ok((Bool, l, r))
                } else {
                    Err(format!("{} expected {} but found {}", op, l.get_type(), r.get_type()))
                }
            }
        }
    }

    fn resolve_types(&mut self, sym: &mut SymbolTable) -> Result<SemanticNode, String> {
        self.traverse(self.root, &None, sym)
    }

    fn traverse(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode, String> {
        self.trace(ast, current_func, sym);
        self.analyize_node(ast, current_func, sym).map_err(|e| {
            if !e.starts_with("L") {
                format!("L{}: {}", ast.get_metadata().ln, e)
            } else {
                e
            }
        })
    }

    fn lookup_path(&'a self, sym: &'a SymbolTable, path: &ast::Path) -> Result<Option<&'a Symbol>, String> {
        if path.len() > 1 {
            let current_path = self.stack.to_path(sym).expect("A valid path is expected");
            let mut path = path.to_canonical(&current_path)?;
            let item = path.truncate().unwrap();
            let node = self.root.go_to(&path).ok_or(format!("Could not find node with the given path: {}", path))?;
            Ok(node.get_metadata().sym.get(&item))
        } else {
            let item = &path[0];
            Ok(sym.get(item).or(self.stack.get(item)))
        }
    }

    fn lookup(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&'a Symbol, String> {
        sym.get(id)
            .or(self.stack.get(id))
            .ok_or(format!("{} is not defined", id))
    }

    fn lookup_func_or_cor(
        &'a self,
        sym: &'a SymbolTable,
        id: &str,
    ) -> Result<(&Vec<ast::Type>, &ast::Type), String> {
        match self.lookup(sym, id)? {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            }
            | Symbol {
                ty: Type::FunctionDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine or function", id)),
        }
    }

    fn lookup_coroutine(
        &'a self,
        sym: &'a SymbolTable,
        id: &str,
    ) -> Result<(&Vec<ast::Type>, &ast::Type), String> {
        match self.lookup(sym, id)? {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    fn lookup_var(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&ast::Type, String> {
        let p = &self.lookup(sym, id)?.ty;
        match p {
            Custom(..) | Coroutine(_) | I32 | Bool => Ok(p),
            _ => return Err(format!("{} is not a variable", id)),
        }
    }

    fn analyize_node(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode, String> {
        use ast::Ast::*;
        match &ast {
            &Integer(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = I32;
                Ok(Integer(meta, *v))
            }
            &Boolean(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Bool;
                Ok(Boolean(meta.clone(), *v))
            }
            &StringLiteral(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = ast::Type::StringLiteral;
                Ok(StringLiteral(meta.clone(), v.clone()))
            }
            &CustomType(meta, name) => {
                let mut meta = meta.clone();
                meta.ty = Custom(name.clone());
                Ok(CustomType(meta.clone(), name.clone()))
            }
            &IdentifierDeclare(meta, name, p) => {
                let mut meta = meta.clone();
                meta.ty = p.clone();
                Ok(IdentifierDeclare(meta.clone(), name.clone(), p.clone()))
            }
            Identifier(meta, id) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(_) => {
                    let mut meta = meta.clone();
                    match self.lookup(sym, &id)? {
                        Symbol { ty: p, .. } => meta.ty = p.clone(),
                    };
                    Ok(Identifier(meta.clone(), id.clone()))
                }
            },
            Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            &MemberAccess(meta, src, member) => {
                let mut meta = meta.clone();
                // Get the type of src and look up its struct definition
                // Check the struct definition for the type of `member`
                // if it exists, if it does not exist then return an error
                let src = self.traverse(&src, current_func, sym)?;
                match src.get_type() {
                    Custom(struct_name) => {
                        let member_ty = self
                            .lookup(sym, &struct_name)?
                            .ty
                            .get_member(&member)
                            .ok_or(format!("{} does not have member {}", struct_name, member))?;
                        meta.ty = member_ty.clone();
                        Ok(MemberAccess(meta.clone(), Box::new(src), member.clone()))
                    }
                    _ => Err(format!("Type {} does not have members", src.get_type())),
                }
            }
            &BinaryOp(meta, op, l, r) => {
                let mut meta = meta.clone();
                let (ty, l,r) = self.binary_op(*op, &l, &r, current_func, sym)?;
                meta.ty = ty;
                Ok(BinaryOp(meta.clone(), *op, Box::new(l), Box::new(r)))
            }
            &UnaryOp(meta, op, operand) => {
                let mut meta = meta.clone();
                let (ty, operand) = self.unary_op(*op, &operand, current_func, sym)?;
                meta.ty = ty;
                Ok(UnaryOp(meta.clone(), *op, Box::new(operand)))
            }
            &If(meta, cond, true_arm, false_arm) => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond, current_func, sym)?;
                if cond.get_type() == Bool {
                    let true_arm = self.traverse(&true_arm, current_func, sym)?;
                    let false_arm = self.traverse(&false_arm, current_func, sym)?;
                    if true_arm.get_type() == false_arm.get_type() {
                        meta.ty = true_arm.get_type().clone();
                        Ok(If(meta.clone(), Box::new(cond), Box::new(true_arm), Box::new(false_arm)))
                    } else {
                        Err(format!(
                            "If expression has mismatching arms: expected {} got {}",
                            true_arm.get_type(), false_arm.get_type()
                        ))
                    }
                } else {
                    Err(format!(
                        "Expected boolean expression in if conditional, got: {}",
                        cond.get_type()
                    ))
                }
            }
            &Mutate(meta, id, rhs) => match current_func {
                Some(_) => {
                    let mut meta = meta.clone();
                    let rhs = self.traverse(&rhs, current_func, sym)?;
                    match self.lookup(sym, &id)? {
                        symbol => {
                            if symbol.mutable {
                                if symbol.ty == rhs.get_type() {
                                    meta.ty = rhs.get_type().clone();
                                    Ok(Mutate(meta.clone(), id.clone(), Box::new(rhs)))
                                } else {
                                    Err(format!(
                                        "{} is of type {} but is assigned {}",
                                        id, symbol.ty, rhs.get_type()
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
            &Bind(meta, name, mutable, p, rhs) => match current_func {
                Some(_) => {
                    let mut meta = meta.clone();
                    let rhs = self.traverse(&rhs, current_func, sym)?;
                    if *p == rhs.get_type() {
                        sym.add(&name, p.clone(), *mutable)?;
                        meta.ty = p.clone();
                        Ok(Bind(meta.clone(), name.clone(), *mutable, p.clone(), Box::new(rhs)))
                    } else {
                        Err(format!("Bind expected {} but got {}", p, rhs.get_type()))
                    }
                }
                None => Err(format!(
                    "Attempting to bind variable {} outside of function",
                    name
                )),
            },
            &Return(meta, None) => match current_func {
                None => Err(format!("Return called outside of a function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                    if *fty == Unit {
                        meta.ty = Unit;
                        Ok(Return(meta.clone(), None))
                    } else {
                        Err(format!("Return expected {} type and got unit", fty))
                    }
                }
            },
            &Return(meta, Some(exp)) => match current_func {
                None => Err(format!("Return appears outside of a function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                    if *fty == exp.get_type() {
                        meta.ty = fty.clone();
                        Ok(Return(meta.clone(), Some(Box::new(exp))))
                    } else {
                        Err(format!("Return expected {} but got {}", fty, exp.get_type()))
                    }
                }
            },
            &Yield(meta, exp) => match current_func {
                None => Err(format!("Yield appears outside of function")),
                Some(_) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    meta.ty = match exp.get_type() {
                        Coroutine(ret_ty) => *ret_ty.clone(),
                        _ => return Err(format!("yield expects co<_> but got {}", exp.get_type())),
                    };
                    Ok(Yield(meta.clone(), Box::new(exp)))
                }
            },
            &YieldReturn(meta, None) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    if *ret_ty == Unit {
                        meta.ty = Unit;
                        Ok(YieldReturn(meta.clone(), None))
                    } else {
                        Err(format!("Yield return expected {} but got unit", ret_ty))
                    }
                }
            },
            &YieldReturn(meta, Some(exp)) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    if *ret_ty == exp.get_type() {
                        meta.ty = ret_ty.clone();
                        Ok(YieldReturn(meta.clone(), Some(Box::new(exp))))
                    } else {
                        Err(format!("Yield return expected {} but got {}", ret_ty, exp.get_type()))
                    }
                }
            },
            &Statement(meta, stmt) => {
                let mut meta = meta.clone();
                let stmt = self.traverse(&stmt, current_func, sym)?;
                meta.ty = Unit;
                Ok(Statement(meta.clone(), Box::new(stmt)))
            }
            &RoutineCall(meta, call, fname, params) => {
                let mut meta = meta.clone();
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut resolved_params = vec![];
                for param in params.iter() {
                    let ty = self.traverse(param, current_func, sym)?;

                    resolved_params.push(ty);
                }
                let symbol = self.lookup_path(sym, fname)?;
                
                let (expected_param_tys, ret_ty) = match symbol {
                    Some(Symbol {
                        ty: Type::FunctionDef(pty, rty),
                        ..
                    }) if *call == crate::syntax::ast::RoutineCall::Function => (pty, *rty.clone()),
                    Some(Symbol {
                        ty: Type::CoroutineDef(pty, rty),
                        ..
                    }) if *call == crate::syntax::ast::RoutineCall::CoroutineInit => {
                        (pty, Type::Coroutine(rty.clone()))
                    }
                    Some(_) => return Err(format!("{:?} found but was not a function", fname)),
                    None => return Err(format!("function {:?} not declared", fname)),
                };

                if resolved_params.len() != expected_param_tys.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to routine: {}",
                        fname
                    ))
                } else {
                    let z = resolved_params.iter().zip(expected_param_tys.iter());
                    let all_params_match = z.map(|(up, fp)| up.get_type() == *fp).fold(true, |x, y| x && y);
                    if all_params_match {
                        meta.ty = ret_ty;
                        Ok(RoutineCall(meta.clone(), *call, fname.clone(), resolved_params))
                    } else {
                        Err(format!(
                            "One or more parameters had mismatching types for function {}",
                            fname
                        ))
                    }
                }
            }
            &Printi(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Printi(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printi got {}", exp.get_type()))
                }
            }
            &Printiln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Printiln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printiln got {}", exp.get_type()))
                }
            }
            &Prints(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == ast::Type::StringLiteral {
                    meta.ty = Unit;
                    Ok(Prints(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected string for printiln got {}", exp.get_type()))
                }
            }
            &Printbln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == Bool {
                    meta.ty = Unit;
                    Ok(Printbln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printbln got {}", exp.get_type()))
                }
            }

            &ExpressionBlock(meta, body) => {
                let mut meta = meta.clone();
                let mut resolved_body = vec![];
                let mut ty = Unit;
                let tmp_sym = sym.clone();
                self.stack.push(tmp_sym);
                for stmt in body.iter() {
                    let exp = self.traverse(stmt, current_func, &mut meta.sym)?;
                    ty = exp.get_type().clone();
                    resolved_body.push(exp);
                }
                self.stack.pop();
                meta.ty = ty;
                Ok(ExpressionBlock(meta.clone(), resolved_body))
            }
            RoutineDef{meta, name, params, ty: p, body, ..} => {
                let mut meta = meta.clone();
                for (pname, pty) in params.iter() {
                    meta.sym.add(pname, pty.clone(), false)?;
                }
                let tmp_sym = sym.clone();
                self.stack.push(tmp_sym);
                let mut resolved_body = vec![];
                for stmt in body.iter() {
                    let exp = self.traverse(stmt, &Some(name.clone()), &mut meta.sym)?;
                    resolved_body.push(exp);
                }
                self.stack.pop();
                meta.ty = p.clone();
                Ok(RoutineDef(meta.clone(), def.clone(), name.clone(), params.clone(), p.clone(), resolved_body))
            }
            &Module {
                meta,
                name,
                functions,
                coroutines,
                structs,
            } => {
                let mut meta = meta.clone();
                let tmp_sym = sym.clone();
                self.stack.push(tmp_sym);
                let mut resolved_functions = vec![];
                for func in functions.iter() {
                    let func = self.traverse(func, &None, &mut meta.sym)?;
                    resolved_functions.push(func);
                }
                let mut resolved_coroutines = vec![];
                for cor in coroutines.iter() {
                    let cor = self.traverse(cor, &None, &mut meta.sym)?;
                    resolved_coroutines.push(cor);
                }
                let mut resolved_structs = vec![];
                for st in structs.iter() {
                    let st = self.traverse(st, &None, &mut meta.sym)?;
                    resolved_structs.push(st);
                }
                self.stack.pop();
                meta.ty = Unit;
                Ok(Module{meta: meta.clone(), name: name.clone(), functions: resolved_functions, coroutines: resolved_coroutines, structs: resolved_structs})
            }
            &StructDef(meta, struct_name, members) => {
                let mut meta = meta.clone();
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
                meta.ty = Unit;
                Ok(StructDef(meta.clone(), struct_name.clone(), members.clone()))
            }
            &StructExpression(meta, struct_name, params) => {
                let mut meta = meta.clone();
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

                let mut resolved_params = vec![];
                for (pn, pv) in params.iter() {
                    let param = self.traverse(pv, current_func, sym)?;
                    let member_ty = struct_def
                        .get_member(pn)
                        .ok_or(format!("member {} not found on {}", pn, struct_name))?;
                    if param.get_type() != *member_ty {
                        return Err(format!(
                            "{}.{} expects {} but got {}",
                            struct_name, pn, member_ty, param.get_type()
                        ));
                    }
                    resolved_params.push((pn.clone(), param));
                }

                let anonymouse_name = format!("!{}_{}", struct_name, meta.id);
                sym.add(&anonymouse_name, Type::Custom(struct_name.clone()), false)?;
                meta.ty = Custom(struct_name.clone());
                Ok(StructExpression(meta.clone(), struct_name.clone(), resolved_params))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast;
    use crate::ast::Ast;
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
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
    ) -> Result<SemanticNode, String> {
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
                sym.add(
                    f,
                    Type::CoroutineDef(params, Box::new(fi.ret.clone())),
                    false,
                )?;
            } else {
                sym.add(
                    f,
                    Type::FunctionDef(params, Box::new(fi.ret.clone())),
                    false,
                )?;
            }
        }

        let mut semantic = SemanticAnalyzer::new(ast);
        semantic.traverse(ast, current_func, &mut sym)
    }

    #[test]
    pub fn test_integer() {
        let node = Ast::Integer(1, 5);
        let scope = Scope::new();

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(ast::Type::I32));
    }

    #[test]
    pub fn test_identifier() {
        let mut scope = Scope::new();
        scope.add("my_main", vec![], Unit, vec![("x", false, Bool)]);

        let node = Ast::Identifier(1, "x".into());

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_main".into()),
            &scope,
        ).map(|n| n.get_type().clone()).unwrap();
        assert_eq!(ty, ast::Type::Bool);
    }

    #[test]
    pub fn test_path() {
        use crate::syntax::parser;
        // TODO: finish this test
        for (text, expected) in vec![
                ("mod my_mod{ 
                    fn test() -> i32{ return 0;} 
                    fn main() {
                        let i: i32 := self::test(); 
                        //let j: i32 := root::my_mod::test();
                        return;
                    }
                }",
                Ok(())),
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> i32 {return ms.y;}",
                Err("Semantic: L1: MyStruct does not have member y")),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_, _>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
                match expected {
                    Ok(_) => assert!(result.is_ok(), "{:?}", result),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
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

            let mut sa = SemanticAst::new();
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone()).unwrap();
            assert_eq!(ty, ast::Type::I32);
        }
        // operand is not i32
        {
            let node = Ast::UnaryOp(1, UnaryOperator::Minus, Box::new(Ast::Boolean(1, true)));

            let mut sa = SemanticAst::new();
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope);
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

            let mut sa = SemanticAst::new();
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone()).unwrap();
            assert_eq!(ty, ast::Type::I32);
        }

        // operands are not i32
        {
            let node = Ast::BinaryOp(
                1,
                BinaryOperator::Add,
                Box::new(Ast::Identifier(1, "b".into())),
                Box::new(Ast::Integer(1, 10)),
            );

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone()).unwrap();
            assert_eq!(ty, ast::Type::I32);
        }

        // operands are not i32
        {
            let node = Ast::BinaryOp(
                1,
                BinaryOperator::Mul,
                Box::new(Ast::Identifier(1, "b".into())),
                Box::new(Ast::Integer(1, 10)),
            );

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

        let mut sa = SemanticAst::new();
        for (test, expected) in tests.iter() {
            let ty = start(&mut sa.from_parser_ast(&test).unwrap(), &None, &scope).map(|n| n.get_type().clone());
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
            let mut sa = SemanticAst::new();
            let ty = start(&mut sa.from_parser_ast(&test).unwrap(), &None, &scope).map(|n| n.get_type().clone());
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            ).map(|n| n.get_type().clone());
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            ).map(|n| n.get_type().clone());
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
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

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Unit));
    }

    #[test]
    pub fn test_return_i32() {
        let mut scope = Scope::new();
        scope.add("my_func", vec![], I32, vec![]);

        let node = Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))));
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));
    }

    #[test]
    pub fn test_fn_call() {
        let mut scope = Scope::new();
        scope.add("my_func", vec![], I32, vec![]);

        let node = Ast::RoutineCall(1, ast::RoutineCall::Function, vec!["my_func"].into(), vec![]);
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        scope.add("my_func2", vec![("x", I32)], I32, vec![]);
        // test correct parameters passed in call
        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::Function,
            vec!["my_func2"].into(),
            vec![Ast::Integer(1, 5)],
        );

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func2".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        // test incorrect parameters passed in call
        let node = Ast::RoutineCall(1, ast::RoutineCall::Function, vec!["my_func2"].into(), vec![]);

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func2".into()),
            &scope,
        ).map(|n| n.get_type().clone());
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

        let node = Ast::RoutineCall(1, ast::RoutineCall::CoroutineInit, vec!["my_co"].into(), vec![]);
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Coroutine(Box::new(I32))));

        // test correct parameters passed in call
        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::CoroutineInit,
            vec!["my_co2"].into(),
            vec![Ast::Integer(1, 5)],
        );

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co2".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Coroutine(Box::new(I32))));

        // test incorrect parameters passed in call
        let node = Ast::RoutineCall(1, ast::RoutineCall::CoroutineInit, vec!["my_co2"].into(), vec![]);

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
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
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Unit));

        // test correct type for yield return
        let node = Ast::YieldReturn(1, Some(Box::new(Ast::Integer(1, 5))));
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co2".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        // test incorrect type for yield return
        let node = Ast::YieldReturn(1, None);
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
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
            vec![("c", false, Coroutine(Box::new(I32)))],
        );
        scope.add("my_co2", vec![], I32, vec![]);

        let node = Ast::Yield(1, Box::new(Ast::Identifier(1, "c".into())));
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_main".into()),
            &scope,
        ).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));
    }

    #[test]
    fn test_func_def() {
        let mut scope = Scope::new();
        scope.add("my_func", vec![], I32, vec![]);

        let node = Ast::RoutineDef{
            meta: 1,
            def: ast::RoutineDef::Function,
            name: "my_func".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        let node = Ast::RoutineDef{
            meta: 1,
            def: ast::RoutineDef::Function,
            name: "my_func".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, None)],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope);
        assert_eq!(ty, Err("L1: Return expected i32 type and got unit".into()));
    }

    #[test]
    fn test_coroutine_def() {
        let mut scope = Scope::new();
        scope.add("my_co", vec![], I32, vec![]);

        let node = Ast::RoutineDef{
            meta: 1,
            def: ast::RoutineDef::Coroutine,
            name: "my_co".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope).map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        let node = Ast::RoutineDef{
            meta: 1,
            def: ast::RoutineDef::Coroutine,
            name: "my_co".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, None)],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope);
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

            let mut sa = SemanticAst::new();
            let result = start(
                &mut sa.from_parser_ast(&node).unwrap(),
                &Some("my_main".into()),
                &scope,
            ).map(|n| n.get_type().clone());
            assert_eq!(result, ex);
        }
    }

    #[test]
    pub fn test_struct_init() {
        use crate::syntax::parser;
        for (text, expected) in vec![
            (
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:false};}",
                Err("Semantic: L1: MyStruct.x expects i32 but got bool"),
            ),
            (
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{};}",
                Err("Semantic: L1: expected 1 parameters but found 0"),
            ),
            (
                "struct MyStruct{x:i32} fn test() -> i32 {return MyStruct{x:5};}",
                Err("Semantic: L1: Return expected i32 but got MyStruct"),
            ),
            (
                "struct MyStruct{x:co i32} fn test(c: co i32) -> MyStruct {return MyStruct{x: c};}",
                Ok(()),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
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
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_, _>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
                match expected {
                    Ok(_) => assert!(result.is_ok()),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
    }
}
