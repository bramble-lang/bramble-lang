use crate::{ast::{Ast, Ast::*, BinaryOperator, UnaryOperator}, syntax::module};
use crate::semantics::semanticnode::{SemanticAst, SemanticNode};
use crate::semantics::symbol_table::*;
use crate::syntax::ty::Type;
use Type::*;
use crate::syntax::pnode::PNode;
use crate::syntax::path::Path;
use crate::{
    ast,
    diagnostics::config::{Tracing, TracingConfig},
};

use super::semanticnode::SemanticMetadata;

pub fn type_check(
    ast: &PNode,
    trace: TracingConfig,
    trace_path: TracingConfig,
) -> Result<SemanticNode, String> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_parser_ast(&ast)?;
    SymbolTable::from_ast(&mut sm_ast)?;

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

    fn trace(
        &self,
        node: &SemanticNode,
        current_func: &Option<String>,
        current_scope: &SymbolTable,
    ) {
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
            let path = self
                .stack
                .to_path(current_scope)
                .map_or("[]".into(), |p| format!("{}", p));
            println!("L{}: {}{} <- {}", line, func, node, path);
        }
    }

    fn unary_op(
        &mut self,
        op: UnaryOperator,
        operand: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<(Type, SemanticNode), String> {
        use UnaryOperator::*;

        let operand = self.traverse(operand, current_func, sym)?;

        match op {
            Minus => {
                if operand.get_type() == I32 {
                    Ok((I32, operand))
                } else {
                    Err(format!(
                        "{} expected i32 but found {}",
                        op,
                        operand.get_type()
                    ))
                }
            }
            Not => {
                if operand.get_type() == Bool {
                    Ok((Bool, operand))
                } else {
                    Err(format!(
                        "{} expected bool but found {}",
                        op,
                        operand.get_type()
                    ))
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
    ) -> Result<(Type, SemanticNode, SemanticNode), String> {
        use BinaryOperator::*;

        let l = self.traverse(l, current_func, sym)?;
        let r = self.traverse(r, current_func, sym)?;

        match op {
            Add | Sub | Mul | Div => {
                if l.get_type() == I32 && r.get_type() == I32 {
                    Ok((I32, l, r))
                } else {
                    Err(format!(
                        "{} expected i32 but found {} and {}",
                        op,
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
            BAnd | BOr => {
                if l.get_type() == Bool && r.get_type() == Bool {
                    Ok((Bool, l, r))
                } else {
                    Err(format!(
                        "{} expected bool but found {} and {}",
                        op,
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
            Eq | NEq | Ls | LsEq | Gr | GrEq => {
                if l.get_type() == r.get_type() {
                    Ok((Bool, l, r))
                } else {
                    Err(format!(
                        "{} expected {} but found {}",
                        op,
                        l.get_type(),
                        r.get_type()
                    ))
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

    fn get_current_path(&self, sym: &'a SymbolTable) -> Result<Path, String> {
        self.stack
            .to_path(sym)
            .ok_or("A valid path is expected".into())
    }

    /// Convert a path to its canonical form by merging with the ancestors in the AST.
    fn to_canonical(&self, sym: &'a SymbolTable, path: &Path) -> Result<Path, String> {
        let current_path = self.stack.to_path(sym).ok_or("A valid path is expected")?;
        path.to_canonical(&current_path)
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical(&self, sym: &'a SymbolTable, ty: &Type) -> Result<Type, String> {
        match ty {
            Custom(path) => Ok(Custom(path.to_canonical(&self.get_current_path(sym)?)?)),
            Coroutine(ty) => Ok(Coroutine(Box::new(self.type_to_canonical(sym, &ty)?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical_with_path(parent_path: &Path, ty: &Type) -> Result<Type, String> {
        match ty {
            Custom(path) => Ok(Custom(path.to_canonical(parent_path)?)),
            Coroutine(ty) => Ok(Coroutine(Box::new(Self::type_to_canonical_with_path(
                parent_path,
                &ty,
            )?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Convert any parameter that is a custom type, to its canonical form.
    fn params_to_canonical(
        &self,
        sym: &'a SymbolTable,
        params: &Vec<(String, Type)>,
    ) -> Result<Vec<(String, Type)>, String> {
        let mut canonical_params = vec![];
        for (name, ty) in params.iter() {
            canonical_params.push((name.clone(), self.type_to_canonical(sym, ty)?));
        }
        Ok(canonical_params)
    }

    fn lookup_symbol_by_path(
        &'a self,
        sym: &'a SymbolTable,
        path: &Path,
    ) -> Result<Option<(&'a Symbol, Path)>, String> {
        let canon_path = self.to_canonical(sym, path)?;
        if path.len() > 1 {
            let item = canon_path
                .item()
                .expect("Expected a canonical path with at least one step in it");
            let node = self
                .root
                .go_to_module(&canon_path.parent())
                .ok_or(format!("Could not find item with the given path: {}", path))?;
            Ok(node.get_metadata().sym.get(&item).map(|i| (i, canon_path)))
        } else if path.len() == 1 {
            let item = &path[0];
            Ok(sym
                .get(item)
                .or(self.stack.get(item))
                .map(|i| (i, canon_path)))
        } else {
            Err("empty path passed to lookup_path".into())
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
    ) -> Result<(&Vec<Type>, &Type), String> {
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
    ) -> Result<(&Vec<Type>, &Type), String> {
        match self.lookup(sym, id)? {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    fn lookup_var(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&Type, String> {
        let p = &self.lookup(sym, id)?.ty;
        match p {
            Custom(..) | Coroutine(_) | I32 | Bool => Ok(p),
            _ => return Err(format!("{} is not a variable", id)),
        }
    }

    fn extract_routine_type_info<'b>(
        symbol: &'b Symbol,
        call: &ast::RoutineCall,
        routine_path: &Path,
    ) -> Result<(&'b Vec<Type>, Type), String> {
        let routine_path_parent = routine_path.parent();
        let (expected_param_tys, ret_ty) = match symbol {
            Symbol {
                ty: Type::FunctionDef(pty, rty),
                ..
            } if *call == crate::syntax::ast::RoutineCall::Function => (
                pty,
                Self::type_to_canonical_with_path(&routine_path_parent, rty)?,
            ),
            Symbol {
                ty: Type::CoroutineDef(pty, rty),
                ..
            } if *call == crate::syntax::ast::RoutineCall::CoroutineInit => (
                pty,
                Type::Coroutine(Box::new(Self::type_to_canonical_with_path(
                    &routine_path_parent,
                    rty,
                )?)),
            ),
            _ => return Err(format!("{} found but was not a function", routine_path)),
        };

        Ok((expected_param_tys, ret_ty))
    }

    fn check_for_invalid_routine_parameters<'b>(
        routine_path: &Path,
        given: &'b Vec<SemanticNode>,
        expected_types: &'b Vec<Type>,
    ) -> Result<(), String> {
        let mut mismatches = vec![];
        let mut idx = 0;
        for (user, expected) in given.iter().zip(expected_types.iter()) {
            idx += 1;
            let user_ty = user.get_type();
            if user_ty != expected {
                mismatches.push((idx, user_ty, expected));
            }
        }
        if mismatches.len() > 0 || given.len() != expected_types.len() {
            let errors: Vec<String> = mismatches
                .iter()
                .map(|(idx, got, expected)| {
                    format!("parameter {} expected {} got {}", idx, expected, got)
                })
                .collect();
            Err(format!(
                "One or more parameters have mismatching types for function {}: {}",
                routine_path,
                errors.join(", ")
            ))
        } else {
            Ok(())
        }
    }

    fn analyize_node(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode, String> {
        match &ast {
            &Integer(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = I32;
                Ok(Integer(meta, *v))
            }
            Boolean(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Bool;
                Ok(Boolean(meta.clone(), *v))
            }
            Ast::StringLiteral(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::StringLiteral;
                Ok(Ast::StringLiteral(meta.clone(), v.clone()))
            }
            CustomType(meta, name) => {
                let mut meta = meta.clone();
                meta.ty = Custom(name.clone());
                Ok(CustomType(meta.clone(), name.clone()))
            }
            IdentifierDeclare(meta, name, p) => {
                let mut meta = meta.clone();
                meta.ty = p.clone();
                Ok(IdentifierDeclare(meta.clone(), name.clone(), p.clone()))
            }
            Identifier(meta, id) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(_) => {
                    let mut meta = meta.clone();
                    match self.lookup(sym, &id)? {
                        Symbol { ty: p, .. } => meta.ty = self.type_to_canonical(sym, p)?,
                    };
                    Ok(Identifier(meta.clone(), id.clone()))
                }
            },
            Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            MemberAccess(meta, src, member) => {
                let mut meta = meta.clone();
                // Get the type of src and look up its struct definition
                // Check the struct definition for the type of `member`
                // if it exists, if it does not exist then return an error
                let src = self.traverse(&src, current_func, sym)?;
                match src.get_type() {
                    Custom(struct_name) => {
                        let (struct_def, canonical_path) = self
                            .lookup_symbol_by_path(sym, &struct_name)?
                            .ok_or(format!("{} was not found", struct_name))?;
                        let member_ty = struct_def
                            .ty
                            .get_member(&member)
                            .ok_or(format!("{} does not have member {}", struct_name, member))?;
                        meta.ty =
                            Self::type_to_canonical_with_path(&canonical_path.parent(), member_ty)?;
                        meta.set_canonical_path(canonical_path);
                        Ok(MemberAccess(meta, Box::new(src), member.clone()))
                    }
                    _ => Err(format!("Type {} does not have members", src.get_type())),
                }
            }
            BinaryOp(meta, op, l, r) => {
                let mut meta = meta.clone();
                let (ty, l, r) = self.binary_op(*op, &l, &r, current_func, sym)?;
                meta.ty = ty;
                Ok(BinaryOp(meta.clone(), *op, Box::new(l), Box::new(r)))
            }
            UnaryOp(meta, op, operand) => {
                let mut meta = meta.clone();
                let (ty, operand) = self.unary_op(*op, &operand, current_func, sym)?;
                meta.ty = ty;
                Ok(UnaryOp(meta.clone(), *op, Box::new(operand)))
            }
            If(meta, cond, true_arm, false_arm) => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond, current_func, sym)?;
                if cond.get_type() == Bool {
                    let true_arm = self.traverse(&true_arm, current_func, sym)?;
                    let false_arm = self.traverse(&false_arm, current_func, sym)?;
                    if true_arm.get_type() == false_arm.get_type() {
                        meta.ty = true_arm.get_type().clone();
                        Ok(If(
                            meta.clone(),
                            Box::new(cond),
                            Box::new(true_arm),
                            Box::new(false_arm),
                        ))
                    } else {
                        Err(format!(
                            "If expression has mismatching arms: expected {} got {}",
                            true_arm.get_type(),
                            false_arm.get_type()
                        ))
                    }
                } else {
                    Err(format!(
                        "Expected boolean expression in if conditional, got: {}",
                        cond.get_type()
                    ))
                }
            }
            Mutate(meta, id, rhs) => match current_func {
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
                                        id,
                                        symbol.ty,
                                        rhs.get_type()
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
            Bind(meta, name, mutable, p, rhs) => match current_func {
                Some(_) => {
                    let mut meta = meta.clone();
                    meta.ty = self.type_to_canonical(sym, p)?;
                    let rhs = self.traverse(&rhs, current_func, sym)?;
                    if meta.ty == rhs.get_type() {
                        sym.add(&name, meta.ty.clone(), *mutable)?;
                        Ok(Bind(meta, name.clone(), *mutable, p.clone(), Box::new(rhs)))
                    } else {
                        Err(format!(
                            "Bind expected {} but got {}",
                            meta.ty,
                            rhs.get_type()
                        ))
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
            Return(meta, Some(exp)) => match current_func {
                None => Err(format!("Return appears outside of a function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                    let fty = self.type_to_canonical(sym, fty)?;
                    if fty == exp.get_type() {
                        meta.ty = fty;
                        Ok(Return(meta.clone(), Some(Box::new(exp))))
                    } else {
                        Err(format!(
                            "Return expected {} but got {}",
                            fty,
                            exp.get_type()
                        ))
                    }
                }
            },
            Yield(meta, exp) => match current_func {
                None => Err(format!("Yield appears outside of function")),
                Some(_) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    meta.ty = match exp.get_type() {
                        Coroutine(ret_ty) => self.type_to_canonical(sym, ret_ty)?,
                        _ => return Err(format!("yield expects co<_> but got {}", exp.get_type())),
                    };
                    Ok(Yield(meta, Box::new(exp)))
                }
            },
            YieldReturn(meta, None) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    if *ret_ty == Unit {
                        meta.ty = Unit;
                        Ok(YieldReturn(meta, None))
                    } else {
                        Err(format!("Yield return expected {} but got unit", ret_ty))
                    }
                }
            },
            YieldReturn(meta, Some(exp)) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    let ret_ty = self.type_to_canonical(sym, ret_ty)?;
                    if ret_ty == exp.get_type() {
                        meta.ty = ret_ty;
                        Ok(YieldReturn(meta, Some(Box::new(exp))))
                    } else {
                        Err(format!(
                            "Yield return expected {} but got {}",
                            ret_ty,
                            exp.get_type()
                        ))
                    }
                }
            },
            Statement(meta, stmt) => {
                let mut meta = meta.clone();
                let stmt = self.traverse(&stmt, current_func, sym)?;
                meta.ty = Unit;
                Ok(Statement(meta, Box::new(stmt)))
            }
            RoutineCall(meta, call, routine_path, params) => {
                let mut meta = meta.clone();
                // test that the expressions passed to the function match the functions
                // parameter types
                let mut resolved_params = vec![];
                for param in params.iter() {
                    let ty = self.traverse(param, current_func, sym)?;

                    resolved_params.push(ty);
                }
                let (symbol, routine_canon_path) =
                    self.lookup_symbol_by_path(sym, routine_path)?
                        .ok_or(format!("function {} not declared", routine_path))?;

                let (expected_param_tys, ret_ty) =
                    Self::extract_routine_type_info(symbol, call, &routine_path)?;

                let expected_param_tys = expected_param_tys
                    .iter()
                    .map(|pty| Self::type_to_canonical_with_path(&routine_canon_path.parent(), pty))
                    .collect::<Result<Vec<Type>, String>>()?;

                if resolved_params.len() != expected_param_tys.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to routine: {}",
                        routine_path
                    ))
                } else {
                    match Self::check_for_invalid_routine_parameters(
                        &routine_path,
                        &resolved_params,
                        &expected_param_tys,
                    ) {
                        Err(msg) => Err(msg),
                        Ok(()) => {
                            meta.ty = self.type_to_canonical(sym, &ret_ty)?;
                            Ok(RoutineCall(
                                meta.clone(),
                                *call,
                                routine_canon_path,
                                resolved_params,
                            ))
                        }
                    }
                }
            }
            Printi(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Printi(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printi got {}", exp.get_type()))
                }
            }
            Printiln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Printiln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printiln got {}", exp.get_type()))
                }
            }
            Prints(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == Type::StringLiteral {
                    meta.ty = Unit;
                    Ok(Prints(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!(
                        "Expected string for printiln got {}",
                        exp.get_type()
                    ))
                }
            }
            Printbln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == Bool {
                    meta.ty = Unit;
                    Ok(Printbln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printbln got {}", exp.get_type()))
                }
            }

            ExpressionBlock(meta, body) => {
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
            RoutineDef {
                meta,
                def,
                name,
                params,
                ty: p,
                body,
            } => {
                let mut meta = meta.clone();
                let canonical_params = self.params_to_canonical(sym, &params)?;
                for (pname, pty) in canonical_params.iter() {
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
                meta.ty = self.type_to_canonical(sym, p)?;

                let canonical_ret_ty = self.type_to_canonical(sym, &meta.ty)?;

                let canon_path = self
                    .stack
                    .to_path(sym)
                    .map(|mut p| {
                        p.push(name);
                        p
                    })
                    .expect("Failed to create canonical path for function");
                meta.set_canonical_path(canon_path);
                Ok(RoutineDef {
                    meta: meta.clone(),
                    def: def.clone(),
                    name: name.clone(),
                    params: canonical_params,
                    ty: canonical_ret_ty,
                    body: resolved_body,
                })
            }
            Module(m) => {
                let nmodule = self.analyze_module(m, sym)?;
                Ok(Module(nmodule))
            }
            Ast::StructDef(meta, struct_name, fields) => {
                let mut meta = meta.clone();
                // Check the type of each member
                for (field_name, field_type) in fields.iter() {
                    if let Custom(ty_name) = field_type {
                        self.lookup_symbol_by_path(sym, ty_name).map_err(|e| {
                            format!("member {}.{} invalid: {}", struct_name, field_name, e)
                        })?;
                    }
                }
                let canonical_fields = self.params_to_canonical(sym, &fields)?;
                meta.ty = Unit;
                meta.set_canonical_path(self.to_canonical(sym, &vec![struct_name.clone()].into())?);
                Ok(Ast::StructDef(
                    meta.clone(),
                    struct_name.clone(),
                    canonical_fields,
                ))
            }
            StructExpression(meta, struct_name, params) => {
                let mut meta = meta.clone();
                // Validate the types in the initialization parameters
                // match their respective members in the struct
                let (struct_def, canonical_path) =
                    self.lookup_symbol_by_path(sym, &struct_name)?
                        .ok_or(format!("Could not find struct {}", struct_name))?;
                let struct_def_ty = struct_def.ty.clone();
                let expected_num_params = struct_def_ty
                    .get_members()
                    .ok_or("Invalid structure")?
                    .len();
                if params.len() != expected_num_params {
                    return Err(format!(
                        "expected {} parameters but found {}",
                        expected_num_params,
                        params.len()
                    ));
                }

                let mut resolved_params = vec![];
                for (pn, pv) in params.iter() {
                    let member_ty = struct_def_ty
                        .get_member(pn)
                        .ok_or(format!("member {} not found on {}", pn, canonical_path))?;
                    let member_ty_canon =
                        Self::type_to_canonical_with_path(&canonical_path.parent(), member_ty)?;
                    let param = self.traverse(pv, current_func, sym)?;
                    if param.get_type() != member_ty_canon {
                        return Err(format!(
                            "{}.{} expects {} but got {}",
                            canonical_path,
                            pn,
                            member_ty_canon,
                            param.get_type()
                        ));
                    }
                    resolved_params.push((pn.clone(), param));
                }

                let anonymouse_name = format!("!{}_{}", canonical_path, meta.id);
                meta.ty = Custom(canonical_path.clone());
                sym.add(&anonymouse_name, meta.ty.clone(), false)?;
                Ok(StructExpression(
                    meta.clone(),
                    canonical_path,
                    resolved_params,
                ))
            }
        }
    }

    fn analyze_module(&mut self, m: &module::Module<SemanticMetadata>, sym: &mut SymbolTable) -> Result<module::Module<SemanticMetadata>, String> {
        let mut nmodule = module::Module::new(m.get_name(), m.get_metadata().clone());
        let mut meta = nmodule.get_metadata_mut().clone();

        let tmp_sym = sym.clone();
        self.stack.push(tmp_sym);

        *nmodule.get_modules_mut() = m.get_modules()
            .iter()
            .map(|m| self.analyze_module(m, &mut meta.sym))
            .collect::<Result<Vec<module::Module<SemanticMetadata>>, String>>()?;
        *nmodule.get_functions_mut() = m.get_functions()
            .iter()
            .map(|f| self.traverse(f, &None, &mut meta.sym))
            .collect::<Result<Vec<SemanticNode>, String>>()?;
        *nmodule.get_coroutines_mut() = m.get_coroutines()
            .iter()
            .map(|c| self.traverse(c, &None, &mut meta.sym))
            .collect::<Result<Vec<SemanticNode>, String>>()?;
        *nmodule.get_structs_mut() = m.get_structs()
            .iter()
            .map(|s| self.traverse(s, &None, &mut meta.sym))
            .collect::<Result<Vec<SemanticNode>, String>>()?;

        self.stack.pop();

        meta.ty = Unit;
        *nmodule.get_metadata_mut() = meta;
        Ok(nmodule)
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
        params: Vec<(&'static str, Type)>,
        ret: Type,
        vars: Vec<(&'static str, bool, Type)>,
    }

    impl FunInfo {
        pub fn new(
            params: Vec<(&'static str, Type)>,
            ret: Type,
            vars: Vec<(&'static str, bool, Type)>,
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
            params: Vec<(&'static str, Type)>,
            ret: Type,
            vars: Vec<(&'static str, bool, Type)>,
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
        let mut sym = SymbolTable::new_module("root");
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
                .collect::<Vec<Type>>();

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
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
            .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Type::I32));
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
        )
        .map(|n| n.get_type().clone())
        .unwrap();
        assert_eq!(ty, Type::Bool);
    }

    #[test]
    pub fn test_path_to_function() {
        use crate::syntax::parser;
        for (text, expected) in vec![
            (
                "mod my_mod{ 
                    fn test() -> i32{ 
                        return 0;
                    } 
                    fn main() {
                        let k: i32 := test();
                        let i: i32 := self::test(); 
                        let j: i32 := root::my_mod::test();
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                "mod my_mod{ 
                    fn test() -> i32{ return 0;} 
                    fn main() {
                        let i: i32 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test"),
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
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test]
    pub fn test_path_to_function_in_different_module() {
        use crate::syntax::parser;
        for (text,) in vec![
            ("mod my_mod{ 
                    fn test() -> i32{ return 0;} 
                }
                mod main_mod{
                    fn main() {
                        let j: i32 := root::my_mod::test();
                        return;
                    }
                }",),
            ("mod my_mod{ 
                    mod inner {
                        fn test() -> i32{ return 0;} 
                    }
                }
                mod main_mod{
                    fn main() {
                        let j: i32 := root::my_mod::inner::test();
                        return;
                    }
                }",),
            ("
                mod main_mod{
                    fn main() {
                        let j: i32 := root::main_mod::inner::test();
                        let k: i32 := inner::test();
                        let l: i32 := self::inner::test();
                        return;
                    }

                    mod inner {
                        fn test() -> i32{ return 0;} 
                    }
                }",),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            assert!(result.is_ok());
        }
    }

    #[test]
    pub fn test_path_to_struct() {
        use crate::syntax::parser;
        for (text, expected) in vec![
            (
                "mod my_mod{ 
                    struct test{i: i32}

                    fn main() {
                        let k: test := test{i: 5};
                        let i: self::test := self::test{i: 5}; 
                        let j: root::my_mod::test := root::my_mod::test{i: 5};
                        return;
                    }
                }",
                Ok(()),
            ),
            (
                "mod my_mod{ 
                    fn test() -> i32{ return 0;} 
                    fn main() {
                        let i: i32 := my_mod::test(); 
                        return;
                    }
                }",
                Err("Semantic: L4: Could not find item with the given path: my_mod::test"),
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
                Ok(_) => assert!(result.is_ok(), "{:?} got {:?}", expected, result),
                Err(msg) => assert_eq!(result.err(), Some(msg.into())),
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_expression_renamed_with_canonical_path() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{i: i32}

                fn main() {
                    let k: test := test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i32}

                fn main() {
                    let k: test := root::test{i: 5};
                    return;
                }
                ",
            "
                struct test{i: i32}

                fn main() {
                    let k: test := self::test{i: 5};
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Module(m) = result {
                if let RoutineDef { body, .. } = &m.get_functions()[0] {
                    if let Bind(.., exp) = &body[0] {
                        if let box StructExpression(_, struct_name, ..) = exp {
                            let expected: Path = vec!["root", "test"].into();
                            assert_eq!(struct_name, &expected)
                        } else {
                            panic!("Not a struct expression")
                        }
                    } else {
                        panic!("Not a bind")
                    }
                } else {
                    panic!("Not a function")
                }
            } else {
                panic!("Not a module")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_function_params_renamed_with_canonical_path() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{i: i32}

                fn main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Module(m) = result {
                if let RoutineDef { params, .. } = &m.get_functions()[0] {
                    if let (_, Custom(ty_path)) = &params[0] {
                        let expected: Path = vec!["root", "test"].into();
                        assert_eq!(ty_path, &expected)
                    } else {
                        panic!("Not a custom type")
                    }
                } else {
                    panic!("Not a function")
                }
            } else {
                panic!("Not a module")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_coroutine_params_renamed_with_canonical_path() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{i: i32}

                co main(t: test) {
                    return;
                }
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Module(m) = result {
                if let RoutineDef { params, .. } = &m.get_coroutines()[0] {
                    if let (_, Custom(ty_path)) = &params[0] {
                        let expected: Path = vec!["root", "test"].into();
                        assert_eq!(ty_path, &expected)
                    } else {
                        panic!("Not a custom type")
                    }
                } else {
                    panic!("Not a coroutine")
                }
            } else {
                panic!("Not a module")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_def_fields_are_converted_to_canonical_paths() {
        use crate::syntax::parser;
        for text in vec![
            "
                struct test{i: i32}

                struct test2{t: test}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_, _>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Module(m) = result {
                if let Ast::StructDef(_, _, fields) = &m.get_structs()[1] {
                    if let (_, Custom(ty_path)) = &fields[0] {
                        let expected: Path = vec!["root", "test"].into();
                        assert_eq!(ty_path, &expected)
                    } else {
                        panic!("Not a custom type")
                    }
                } else {
                    panic!("Not a structure")
                }
            } else {
                panic!("Not a module")
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
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
                .map(|n| n.get_type().clone())
                .unwrap();
            assert_eq!(ty, Type::I32);
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
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
                .map(|n| n.get_type().clone())
                .unwrap();
            assert_eq!(ty, Type::I32);
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
            let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
                .map(|n| n.get_type().clone())
                .unwrap();
            assert_eq!(ty, Type::I32);
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

        let tests: Vec<(PNode, Result<Type, String>)> = vec![(
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
            let ty = start(&mut sa.from_parser_ast(&test).unwrap(), &None, &scope)
                .map(|n| n.get_type().clone());
            assert_eq!(ty, *expected);
        }
    }

    #[test]
    pub fn test_comparison_ops() {
        let scope = Scope::new();
        let tests: Vec<(PNode, Result<Type, String>)> = vec![
            (
                Ast::BinaryOp(
                    1,
                    BinaryOperator::Eq,
                    Box::new(Ast::Integer(1, 3)),
                    Box::new(Ast::Integer(1, 5)),
                ),
                Ok(Type::Bool),
            ),
            (
                Ast::BinaryOp(
                    1,
                    BinaryOperator::Eq,
                    Box::new(Ast::Boolean(1, true)),
                    Box::new(Ast::Boolean(1, false)),
                ),
                Ok(Type::Bool),
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
            let ty = start(&mut sa.from_parser_ast(&test).unwrap(), &None, &scope)
                .map(|n| n.get_type().clone());
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
                Type::I32,
                Box::new(Ast::Integer(1, 5)),
            );
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            )
            .map(|n| n.get_type().clone());
            assert_eq!(ty, Ok(Type::I32));
        }

        // RHS type does not match LHS type
        {
            let mut scope = Scope::new();
            scope.add("my_func", vec![], Unit, vec![]);

            let node = Ast::Bind(
                1,
                "x".into(),
                false,
                Type::Bool,
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
                Type::I32,
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
                vec![("x".into(), true, Type::I32)],
            );

            let node = Ast::Mutate(1, "x".into(), Box::new(Ast::Integer(1, 5)));
            let mut sa = SemanticAst::new();
            let ty = start(
                &mut sa.from_parser_ast(&node).unwrap(),
                &Some("my_func".into()),
                &scope,
            )
            .map(|n| n.get_type().clone());
            assert_eq!(ty, Ok(Type::I32));
        }
        // Variable is immutable
        {
            let mut scope = Scope::new();
            scope.add(
                "my_func",
                vec![],
                Unit,
                vec![("x".into(), false, Type::I32)],
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
                vec![("x".into(), true, Type::Bool)],
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
                vec![("y".into(), false, Type::I32)],
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
        )
        .map(|n| n.get_type().clone());
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
        )
        .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));
    }

    #[test]
    pub fn test_fn_call() {
        let mut scope = Scope::new();
        scope.add("my_func", vec![], I32, vec![]);

        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::Function,
            vec!["my_func"].into(),
            vec![],
        );
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func".into()),
            &scope,
        )
        .map(|n| n.get_type().clone());
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
        )
        .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        // test incorrect parameters passed in call
        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::Function,
            vec!["my_func2"].into(),
            vec![],
        );

        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_func2".into()),
            &scope,
        )
        .map(|n| n.get_type().clone());
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

        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::CoroutineInit,
            vec!["my_co"].into(),
            vec![],
        );
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co".into()),
            &scope,
        )
        .map(|n| n.get_type().clone());
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
        )
        .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Coroutine(Box::new(I32))));

        // test incorrect parameters passed in call
        let node = Ast::RoutineCall(
            1,
            ast::RoutineCall::CoroutineInit,
            vec!["my_co2"].into(),
            vec![],
        );

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
        )
        .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(Unit));

        // test correct type for yield return
        let node = Ast::YieldReturn(1, Some(Box::new(Ast::Integer(1, 5))));
        let mut sa = SemanticAst::new();
        let ty = start(
            &mut sa.from_parser_ast(&node).unwrap(),
            &Some("my_co2".into()),
            &scope,
        )
        .map(|n| n.get_type().clone());
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
        )
        .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));
    }

    #[test]
    fn test_func_def() {
        let mut scope = Scope::new();
        scope.add("my_func", vec![], I32, vec![]);

        let node = Ast::RoutineDef {
            meta: 1,
            def: ast::RoutineDef::Function,
            name: "my_func".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
            .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        let node = Ast::RoutineDef {
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

        let node = Ast::RoutineDef {
            meta: 1,
            def: ast::RoutineDef::Coroutine,
            name: "my_co".into(),
            params: vec![],
            ty: I32,
            body: vec![Ast::Return(1, Some(Box::new(Ast::Integer(1, 5))))],
        };

        let mut sa = SemanticAst::new();
        let ty = start(&mut sa.from_parser_ast(&node).unwrap(), &None, &scope)
            .map(|n| n.get_type().clone());
        assert_eq!(ty, Ok(I32));

        let node = Ast::RoutineDef {
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
            )
            .map(|n| n.get_type().clone());
            assert_eq!(result, ex);
        }
    }

    #[test]
    pub fn test_struct_expression() {
        use crate::syntax::parser;
        for (line, text, expected) in vec![
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> root::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> self::MyStruct {return MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> MyStruct {return self::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> MyStruct {return root::MyStruct{x:1};}",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test() -> MyStruct 
                {
                    let x: MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct := {self::MyStruct{x: 1}};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i32}}
                fn test() -> my_mod::MyStruct 
                {
                    let x: root::my_mod::MyStruct := self::my_mod::MyStruct{x: 1};
                    return x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "mod my_mod{struct MyStruct{x:i32}}
                mod fn_mod {
                    fn test() -> self::super::my_mod::MyStruct 
                    {
                        let x: root::my_mod::MyStruct := super::my_mod::MyStruct{x: 1};
                        return x;
                    }
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                struct MyStruct2{ms: MyStruct}
                fn test() -> MyStruct2
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: root::MyStruct2 := self::MyStruct2{ ms: x};
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test2(ms: MyStruct) -> i32 {return ms.x;}
                fn test() -> i32
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i32 := test2(x);
                    return y;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                fn test2(ms: MyStruct) -> MyStruct {return ms;}
                fn test() -> i32
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: MyStruct := test2(x);
                    return y.x;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                co test2(ms: MyStruct) -> MyStruct { 
                    yret ms; 
                    return ms;
                }
                fn test() -> root::MyStruct
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: co self::MyStruct := init test2(x);
                    let z: MyStruct := yield (y);
                    return z;
                }",
                Ok(()),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                struct MyStruct2{ms: MyStruct}
                fn test2(ms2: MyStruct2) -> i32 {return ms2.ms.x;}
                fn test() -> i32
                {
                    let x: root::MyStruct := self::MyStruct{x: 1};
                    let y: i32 := test2(x);
                    return y;
                }",
                Err("Semantic: L7: One or more parameters have mismatching types for function test2: parameter 1 expected root::MyStruct2 got root::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i32}
                struct MyStruct2{x:i32}
                fn test() -> MyStruct 
                {
                    let x: root::MyStruct2 := self::MyStruct{x: 1};
                    return x;
                }",
                Err("Semantic: L5: Bind expected root::MyStruct2 but got root::MyStruct"),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{x:false};}",
                Err("Semantic: L1: root::MyStruct.x expects i32 but got bool"),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> MyStruct {return MyStruct{};}",
                Err("Semantic: L1: expected 1 parameters but found 0"),
            ),
            (
                line!(),
                "struct MyStruct{x:i32} fn test() -> i32 {return MyStruct{x:5};}",
                Err("Semantic: L1: Return expected i32 but got root::MyStruct"),
            ),
            (
                line!(),
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
                Ok(_) => {assert!(result.is_ok(), "\nL{}: {} => {:?}", line, text, result)},
                Err(msg) => assert_eq!(result.err().unwrap(), msg),
            }
        }
    }

    #[test]
    pub fn test_member_access() {
        use crate::syntax::parser;
        for (text, expected) in vec![
                ("struct MyStruct{x:i32}
                fn test(ms:MyStruct) -> i32 {
                    return ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> i32 {return ms.y;}",
                Err("Semantic: L1: root::MyStruct does not have member y")),
                ("struct MyStruct{x:i32} fn test(ms:MyStruct) -> bool{return ms.x;}",
                Err("Semantic: L1: Return expected bool but got i32")),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i32 {return ms.ms.x;}",
                Ok(())),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> MyStruct {return ms.ms;}",
                Ok(())),
                ("struct MyStruct{x:i32} struct MS2{ms:MyStruct} fn test(ms:MS2) -> i32 {return ms.ms.y;}",
                Err("Semantic: L1: root::MyStruct does not have member y")),
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
                    Ok(_) => assert!(result.is_ok(), "{} -> {:?}", text, result),
                    Err(msg) => assert_eq!(result.err().unwrap(), msg),
                }
            }
    }
}
