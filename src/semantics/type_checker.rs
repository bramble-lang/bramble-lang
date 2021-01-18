use crate::syntax::path::Path;
use crate::syntax::ty::Type;
use crate::{
    ast,
    diagnostics::config::{Tracing, TracingConfig},
};
use crate::{
    ast::{Ast, BinaryOperator, UnaryOperator},
    syntax::{
        module::{self, Item},
        routinedef,
    },
};
use crate::{parser::pnode::ParserInfo, semantics::symbol_table::*, syntax::structdef};
use crate::{
    semantics::semanticnode::{SemanticAst, SemanticNode},
    syntax::module::Module,
    syntax::statement,
};
use braid_lang::result::Result;
use Type::*;

use super::semanticnode::SemanticMetadata;

pub fn type_check(
    ast: &module::Module<ParserInfo>,
    trace: TracingConfig,
    trace_path: TracingConfig,
) -> Result<module::Module<SemanticMetadata>> {
    let mut sa = SemanticAst::new();
    let mut sm_ast = sa.from_module(&ast)?;
    SymbolTable::from_module(&mut sm_ast)?;

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
    root: &'a Module<SemanticMetadata>,
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
    pub fn new(root: &'a Module<SemanticMetadata>) -> SemanticAnalyzer {
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
    ) -> Result<(Type, SemanticNode)> {
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
    ) -> Result<(Type, SemanticNode, SemanticNode)> {
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
                        "{} expected {} but found {} and {}",
                        op,
                        l.get_type(),
                        l.get_type(),
                        r.get_type()
                    ))
                }
            }
        }
    }

    fn resolve_types(&mut self, sym: &mut SymbolTable) -> Result<module::Module<SemanticMetadata>> {
        self.analyze_module(self.root, sym)
    }

    fn traverse(
        &mut self,
        ast: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        self.trace(ast, current_func, sym);
        self.analyize_node(ast, current_func, sym).map_err(|e| {
            if !e.starts_with("L") {
                format!("L{}: {}", ast.get_metadata().ln, e)
            } else {
                e
            }
        })
    }

    fn get_current_path(&self, sym: &'a SymbolTable) -> Result<Path> {
        self.stack
            .to_path(sym)
            .ok_or("A valid path is expected".into())
    }

    /// Convert a path to its canonical form by merging with the ancestors in the AST.
    fn to_canonical(&self, sym: &'a SymbolTable, path: &Path) -> Result<Path> {
        let current_path = self.stack.to_path(sym).ok_or("A valid path is expected")?;
        path.to_canonical(&current_path)
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical(&self, sym: &'a SymbolTable, ty: &Type) -> Result<Type> {
        match ty {
            Custom(path) => Ok(Custom(path.to_canonical(&self.get_current_path(sym)?)?)),
            Coroutine(ty) => Ok(Coroutine(Box::new(self.type_to_canonical(sym, &ty)?))),
            _ => Ok(ty.clone()),
        }
    }

    /// Convert any custom type to its canonical form by merging with the current AST ancestors
    fn type_to_canonical_with_path(parent_path: &Path, ty: &Type) -> Result<Type> {
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
    ) -> Result<Vec<(String, Type)>> {
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
    ) -> Result<Option<(&'a Symbol, Path)>> {
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

    fn lookup(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&'a Symbol> {
        sym.get(id)
            .or(self.stack.get(id))
            .ok_or(format!("{} is not defined", id))
    }

    fn lookup_func_or_cor(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
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

    fn lookup_coroutine(&'a self, sym: &'a SymbolTable, id: &str) -> Result<(&Vec<Type>, &Type)> {
        match self.lookup(sym, id)? {
            Symbol {
                ty: Type::CoroutineDef(params, p),
                ..
            } => Ok((params, p)),
            _ => return Err(format!("{} is not a coroutine", id)),
        }
    }

    fn lookup_var(&'a self, sym: &'a SymbolTable, id: &str) -> Result<&Type> {
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
    ) -> Result<(&'b Vec<Type>, Type)> {
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
            _ => {
                let expected = match call {
                    ast::RoutineCall::Function => "function",
                    ast::RoutineCall::CoroutineInit => "coroutine",
                };
                return Err(format!(
                    "Expected {0} but {1} is a {2}",
                    expected, routine_path, symbol.ty
                ));
            }
        };

        Ok((expected_param_tys, ret_ty))
    }

    fn check_for_invalid_routine_parameters<'b>(
        routine_path: &Path,
        given: &'b Vec<SemanticNode>,
        expected_types: &'b Vec<Type>,
    ) -> Result<()> {
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
                    format!("parameter {} expected {} but got {}", idx, expected, got)
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
    ) -> Result<SemanticNode> {
        match &ast {
            &Ast::Integer(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = I32;
                Ok(Ast::Integer(meta, *v))
            }
            Ast::Boolean(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Bool;
                Ok(Ast::Boolean(meta.clone(), *v))
            }
            Ast::StringLiteral(meta, v) => {
                let mut meta = meta.clone();
                meta.ty = Type::StringLiteral;
                Ok(Ast::StringLiteral(meta.clone(), v.clone()))
            }
            Ast::CustomType(meta, name) => {
                let mut meta = meta.clone();
                meta.ty = Custom(name.clone());
                Ok(Ast::CustomType(meta.clone(), name.clone()))
            }
            Ast::IdentifierDeclare(meta, name, p) => {
                let mut meta = meta.clone();
                meta.ty = p.clone();
                Ok(Ast::IdentifierDeclare(
                    meta.clone(),
                    name.clone(),
                    p.clone(),
                ))
            }
            Ast::Identifier(meta, id) => match current_func {
                None => Err(format!("Variable {} appears outside of function", id)),
                Some(_) => {
                    let mut meta = meta.clone();
                    match self.lookup(sym, &id)? {
                        Symbol { ty: p, .. } => meta.ty = self.type_to_canonical(sym, p)?,
                    };
                    Ok(Ast::Identifier(meta.clone(), id.clone()))
                }
            },
            Ast::Path(..) => {
                todo!("Check to make sure that each identifier in the path is a valid module or a item in that module");
            }
            Ast::MemberAccess(meta, src, member) => {
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
                        Ok(Ast::MemberAccess(meta, Box::new(src), member.clone()))
                    }
                    _ => Err(format!("Type {} does not have members", src.get_type())),
                }
            }
            Ast::BinaryOp(meta, op, l, r) => {
                let mut meta = meta.clone();
                let (ty, l, r) = self.binary_op(*op, &l, &r, current_func, sym)?;
                meta.ty = ty;
                Ok(Ast::BinaryOp(meta.clone(), *op, Box::new(l), Box::new(r)))
            }
            Ast::UnaryOp(meta, op, operand) => {
                let mut meta = meta.clone();
                let (ty, operand) = self.unary_op(*op, &operand, current_func, sym)?;
                meta.ty = ty;
                Ok(Ast::UnaryOp(meta.clone(), *op, Box::new(operand)))
            }
            Ast::If(meta, cond, true_arm, false_arm) => {
                let mut meta = meta.clone();
                let cond = self.traverse(&cond, current_func, sym)?;
                if cond.get_type() == Bool {
                    let true_arm = self.traverse(&true_arm, current_func, sym)?;
                    let false_arm = self.traverse(&false_arm, current_func, sym)?;
                    if true_arm.get_type() == false_arm.get_type() {
                        meta.ty = true_arm.get_type().clone();
                        Ok(Ast::If(
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
            Ast::Mutate(..) => self.analyze_mutate(ast, current_func, sym),
            Ast::Bind(..) => self.analyze_bind(ast, current_func, sym),
            Ast::Return(meta, None) => match current_func {
                None => Err(format!("Return called outside of a function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                    if *fty == Unit {
                        meta.ty = Unit;
                        Ok(Ast::Return(meta.clone(), None))
                    } else {
                        Err(format!("Return expected {} but got unit", fty))
                    }
                }
            },
            Ast::Return(meta, Some(exp)) => match current_func {
                None => Err(format!("Return appears outside of a function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, fty) = self.lookup_func_or_cor(sym, cf)?;
                    let fty = self.type_to_canonical(sym, fty)?;
                    if fty == exp.get_type() {
                        meta.ty = fty;
                        Ok(Ast::Return(meta.clone(), Some(Box::new(exp))))
                    } else {
                        Err(format!(
                            "Return expected {} but got {}",
                            fty,
                            exp.get_type()
                        ))
                    }
                }
            },
            Ast::Yield(meta, exp) => match current_func {
                None => Err(format!("Yield appears outside of function")),
                Some(_) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    meta.ty = match exp.get_type() {
                        Coroutine(ret_ty) => self.type_to_canonical(sym, ret_ty)?,
                        _ => return Err(format!("Yield expects co<_> but got {}", exp.get_type())),
                    };
                    Ok(Ast::Yield(meta, Box::new(exp)))
                }
            },
            Ast::YieldReturn(meta, None) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    if *ret_ty == Unit {
                        meta.ty = Unit;
                        Ok(Ast::YieldReturn(meta, None))
                    } else {
                        Err(format!("Yield return expected {} but got unit", ret_ty))
                    }
                }
            },
            Ast::YieldReturn(meta, Some(exp)) => match current_func {
                None => Err(format!("YRet appears outside of function")),
                Some(cf) => {
                    let mut meta = meta.clone();
                    let exp = self.traverse(&exp, current_func, sym)?;
                    let (_, ret_ty) = self.lookup_coroutine(sym, cf)?;
                    let ret_ty = self.type_to_canonical(sym, ret_ty)?;
                    if ret_ty == exp.get_type() {
                        meta.ty = ret_ty;
                        Ok(Ast::YieldReturn(meta, Some(Box::new(exp))))
                    } else {
                        Err(format!(
                            "Yield return expected {} but got {}",
                            ret_ty,
                            exp.get_type()
                        ))
                    }
                }
            },
            Ast::Statement(..) => self.analyze_statement(ast, current_func, sym),
            Ast::RoutineCall(meta, call, routine_path, params) => {
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
                    .collect::<Result<Vec<Type>>>()?;

                if resolved_params.len() != expected_param_tys.len() {
                    Err(format!(
                        "Incorrect number of parameters passed to routine: {}. Expected {} but got {}",
                        routine_path,
                        expected_param_tys.len(),
                        resolved_params.len(),
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
                            Ok(Ast::RoutineCall(
                                meta.clone(),
                                *call,
                                routine_canon_path,
                                resolved_params,
                            ))
                        }
                    }
                }
            }
            Ast::Printi(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Ast::Printi(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printi got {}", exp.get_type()))
                }
            }
            Ast::Printiln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == I32 {
                    meta.ty = Unit;
                    Ok(Ast::Printiln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printiln got {}", exp.get_type()))
                }
            }
            Ast::Prints(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == Type::StringLiteral {
                    meta.ty = Unit;
                    Ok(Ast::Prints(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!(
                        "Expected string for printiln got {}",
                        exp.get_type()
                    ))
                }
            }
            Ast::Printbln(meta, exp) => {
                let mut meta = meta.clone();
                let exp = self.traverse(&exp, current_func, sym)?;
                if exp.get_type() == Bool {
                    meta.ty = Unit;
                    Ok(Ast::Printbln(meta.clone(), Box::new(exp)))
                } else {
                    Err(format!("Expected i32 for printbln got {}", exp.get_type()))
                }
            }

            Ast::ExpressionBlock(meta, body) => {
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
                Ok(Ast::ExpressionBlock(meta.clone(), resolved_body))
            }
            Ast::StructExpression(meta, struct_name, params) => {
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
                Ok(Ast::StructExpression(
                    meta.clone(),
                    canonical_path,
                    resolved_params,
                ))
            }
        }
    }

    fn analyze_module(
        &mut self,
        m: &module::Module<SemanticMetadata>,
        sym: &mut SymbolTable,
    ) -> Result<module::Module<SemanticMetadata>> {
        let mut nmodule = module::Module::new(m.get_name(), m.get_metadata().clone());
        let mut meta = nmodule.get_metadata_mut().clone();

        let tmp_sym = sym.clone();
        self.stack.push(tmp_sym);

        *nmodule.get_modules_mut() = m
            .get_modules()
            .iter()
            .map(|m| self.analyze_module(m, &mut meta.sym))
            .collect::<Result<Vec<module::Module<SemanticMetadata>>>>()?;
        *nmodule.get_functions_mut() = m
            .get_functions()
            .iter()
            .map(|f| self.analyze_item(f, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticMetadata>>>>()?;
        *nmodule.get_coroutines_mut() = m
            .get_coroutines()
            .iter()
            .map(|c| self.analyze_item(c, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticMetadata>>>>()?;
        *nmodule.get_structs_mut() = m
            .get_structs()
            .iter()
            .map(|s| self.analyze_item(s, &mut meta.sym))
            .collect::<Result<Vec<module::Item<SemanticMetadata>>>>()?;

        self.stack.pop();

        meta.ty = Unit;
        *nmodule.get_metadata_mut() = meta;
        Ok(nmodule)
    }

    fn analyze_item(
        &mut self,
        i: &module::Item<SemanticMetadata>,
        sym: &mut SymbolTable,
    ) -> Result<module::Item<SemanticMetadata>> {
        match i {
            Item::Struct(s) => self.analyze_structdef(s, sym).map(|s2| Item::Struct(s2)),
            Item::Routine(r) => self.analyze_routine(r, sym).map(|r2| Item::Routine(r2)),
        }
    }

    fn analyze_routine(
        &mut self,
        routine: &routinedef::RoutineDef<SemanticMetadata>,
        sym: &mut SymbolTable,
    ) -> Result<routinedef::RoutineDef<SemanticMetadata>> {
        let routinedef::RoutineDef {
            meta,
            name,
            def,
            params,
            body,
            ty: p,
            ..
        } = routine;
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
        Ok(routinedef::RoutineDef {
            meta: meta.clone(),
            def: def.clone(),
            name: name.clone(),
            params: canonical_params,
            ty: canonical_ret_ty,
            body: resolved_body,
        })
    }

    fn analyze_structdef(
        &mut self,
        struct_def: &structdef::StructDef<SemanticMetadata>,
        sym: &mut SymbolTable,
    ) -> Result<structdef::StructDef<SemanticMetadata>> {
        // Check the type of each member
        let fields = struct_def.get_fields();
        for (field_name, field_type) in fields.iter() {
            if let Custom(ty_name) = field_type {
                self.lookup_symbol_by_path(sym, ty_name).map_err(|e| {
                    format!(
                        "member {}.{} invalid: {}",
                        struct_def.get_name(),
                        field_name,
                        e
                    )
                })?;
            }
        }

        // Update all fields so that their types use the full canonical path of the type
        let canonical_fields = self.params_to_canonical(sym, &fields)?;

        // Update the metadata with canonical path information and set the type to Unit
        let mut meta = struct_def.get_metadata().clone();
        meta.ty = Unit;
        meta.set_canonical_path(
            self.to_canonical(sym, &vec![struct_def.get_name().clone()].into())?,
        );

        Ok(structdef::StructDef::new(
            struct_def.get_name().clone(),
            meta.clone(),
            canonical_fields,
        ))
    }

    fn analyze_statement(
        &mut self,
        stmt: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        use statement::Statement::*;
        if let Ast::Statement(statement) = stmt {

            let inner = match statement {
                | Bind(box b) => Bind(Box::new(self.analyze_bind(b, current_func, sym)?)),
                | Mutate(box b) => Mutate(Box::new(self.analyze_mutate(b, current_func, sym)?)),
                | Return(box x)  => Return(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Yield(box x)  => Yield(Box::new(self.analyize_node(x, current_func, sym)?)),
                | YieldReturn(box x)  => YieldReturn(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Printi(box x)  => Printi(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Printiln(box x)  => Printiln(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Printbln(box x)  => Printbln(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Prints(box x)  => Prints(Box::new(self.analyize_node(x, current_func, sym)?)),
                | Expression(box e) => Expression(Box::new(self.analyize_node(e, current_func, sym)?)),
            };

            Ok(Ast::Statement(inner))
        } else {
            panic!("Expected a statement, but got {}", stmt.root_str())
        }
    }

    fn analyze_bind(
        &mut self,
        bind: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        if let Ast::Bind(meta, name, mutable, p, rhs) = bind {
            match current_func {
                Some(_) => {
                    let mut meta = meta.clone();
                    meta.ty = self.type_to_canonical(sym, p)?;
                    let rhs = self.traverse(&rhs, current_func, sym)?;
                    if meta.ty == rhs.get_type() {
                        sym.add(&name, meta.ty.clone(), *mutable)?;
                        Ok(Ast::Bind(
                            meta,
                            name.clone(),
                            *mutable,
                            p.clone(),
                            Box::new(rhs),
                        ))
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
            }
        } else {
            panic!("Expected a bind, but got {}", bind.root_str())
        }
    }

    fn analyze_mutate(
        &mut self,
        mutate: &SemanticNode,
        current_func: &Option<String>,
        sym: &mut SymbolTable,
    ) -> Result<SemanticNode> {
        if let Ast::Mutate(meta, id, rhs) = mutate {
            match current_func {
                Some(_) => {
                    let mut meta = meta.clone();
                    let rhs = self.traverse(&rhs, current_func, sym)?;
                    match self.lookup(sym, &id)? {
                        symbol => {
                            if symbol.mutable {
                                if symbol.ty == rhs.get_type() {
                                    meta.ty = rhs.get_type().clone();
                                    Ok(Ast::Mutate(meta.clone(), id.clone(), Box::new(rhs)))
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
            }
        } else {
            panic!("Expected Mutate statement but got {}", mutate.root_str())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Ast, syntax::statement::{Bind, Statement}};
    use crate::lexer::lexer::Lexer;
    use crate::lexer::tokens::Token;
    use crate::parser::parser;
    use crate::syntax::{module::Item, routinedef};

    #[test]
    pub fn test_identifiers() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    return k;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return k;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> string {
                    let k: string := \"hello\";
                    return k;
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := false;
                    return k;
                }",
                Err("Semantic: L2: Bind expected i32 but got bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_path_to_function() {
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
                .collect::<Result<_>>()
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
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            assert!(result.is_ok());
        }
    }

    #[test]
    pub fn test_path_to_struct() {
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
                .collect::<Result<_>>()
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
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { body, .. }) = &result.get_functions()[0] {
                if let Ast::Statement(Statement::Bind(box Ast::Bind(.., exp))) = &body[0] {
                    if let box Ast::StructExpression(_, struct_name, ..) = exp {
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
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_function_params_renamed_with_canonical_path() {
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
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { params, .. }) = &result.get_functions()[0]
            {
                if let (_, Custom(ty_path)) = &params[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a function")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_coroutine_params_renamed_with_canonical_path() {
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
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Routine(routinedef::RoutineDef { params, .. }) =
                &result.get_coroutines()[0]
            {
                if let (_, Custom(ty_path)) = &params[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a coroutine")
            }
        }
    }

    #[test] // this test currently is not working, because Structs have not been updated to use paths.  Will do so after functions are finished
    pub fn test_struct_def_fields_are_converted_to_canonical_paths() {
        for text in vec![
            "
                struct test{i: i32}

                struct test2{t: test}
                ",
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let result = type_check(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();
            if let Item::Struct(s) = &result.get_structs()[1] {
                let fields = s.get_fields();
                if let (_, Custom(ty_path)) = &fields[0] {
                    let expected: Path = vec!["root", "test"].into();
                    assert_eq!(ty_path, &expected)
                } else {
                    panic!("Not a custom type")
                }
            } else {
                panic!("Not a structure")
            }
        }
    }

    #[test]
    pub fn test_unary_ops() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    return -k;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return -k;
                }",
                Err("Semantic: L3: - expected i32 but found bool"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := false;
                    return !k;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    return !k;
                }",
                Err("Semantic: L3: ! expected bool but found i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_add_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let k: i32 := 1 + 5;
                    return k + 3;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 1 + false;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i32 but found i32 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := \"hello\" + 5;
                    return k + 3;
                }",
                Err("Semantic: L2: + expected i32 but found string and i32"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k + 3;
                }",
                Err("Semantic: L3: + expected i32 but found bool and i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mul_op() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let k: i32 := 1 * 5;
                    return k * 3;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 1 * false;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i32 but found i32 and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := \"hello\" * 5;
                    return k * 3;
                }",
                Err("Semantic: L2: * expected i32 but found string and i32"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true;
                    return k * 3;
                }",
                Err("Semantic: L3: * expected i32 but found bool and i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., rhs))) = bind_stm {
                        assert_eq!(rhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_and_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true && false;
                    return k && true;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true && 1;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found bool and i32"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" && false;
                    return k && true;
                }",
                Err("Semantic: L2: && expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 5;
                    return k && true;
                }",
                Err("Semantic: L3: && expected bool but found i32 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_boolean_or_op() {
        for (text, expected) in vec![
            (
                "fn main() -> bool {
                    let k: bool := true || false;
                    return k || true;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> bool {
                    let k: bool := true || 1;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found bool and i32"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := \"hello\" || false;
                    return k || true;
                }",
                Err("Semantic: L2: || expected bool but found string and bool"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 5;
                    return k || true;
                }",
                Err("Semantic: L3: || expected bool but found i32 and bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // Validate that the return statement is the correct type
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_comparison_op() {
        for op in vec!["<", ">", "<=", ">=", "==", "!="] {
            for (text, expected) in vec![
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Ok(Bool),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := 1 {} true;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected i32 but found i32 and bool",
                        op
                    )),
                ),
                (
                    String::from(&format!(
                        "fn main() -> bool {{
                            let k: bool := false {} 5;
                            return k;
                        }}",
                        op
                    )),
                    Err(format!(
                        "Semantic: L2: {} expected bool but found bool and i32",
                        op
                    )),
                ),
            ] {
                let tokens: Vec<Token> = Lexer::new(&text)
                    .tokenize()
                    .into_iter()
                    .collect::<Result<_>>()
                    .unwrap();
                let ast = parser::parse(tokens).unwrap().unwrap();
                let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
                match expected {
                    Ok(expected_ty) => {
                        let module = module.unwrap();
                        let fn_main = module.get_functions()[0].to_routine().unwrap();

                        let bind_stm = &fn_main.get_body()[0];
                        assert_eq!(bind_stm.get_type(), Unit);

                        // validate that the RHS of the bind is the correct type
                        if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                            assert_eq!(lhs.get_type(), expected_ty);
                        } else {
                            panic!("Expected a bind statement");
                        }

                        // Validate that the return statement is the correct type
                        let ret_stm = &fn_main.get_body()[1];
                        assert_eq!(ret_stm.get_type(), expected_ty);
                        if let Ast::Return(_, Some(value)) = ret_stm {
                            assert_eq!(value.get_type(), expected_ty);
                        } else {
                            panic!("Expected a return statement")
                        }
                    }
                    Err(msg) => {
                        assert_eq!(module.unwrap_err(), msg);
                    }
                }
            }
        }
    }

    #[test]
    pub fn test_bind_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    return k;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> i32 {
                    let k: bool := 5;
                    return k;
                }",
                Err("Semantic: L2: Bind expected bool but got i32"),
            ),
            (
                "fn main() -> bool {
                    let k: i32 := 5;
                    return k;
                }",
                Err("Semantic: L3: Return expected bool but got i32"),
            ),
            (
                // Test recursive definition
                "fn main() -> bool {
                    let k: bool := k;
                    return k;
                }",
                Err("Semantic: L2: k is not defined"),
            ),
            (
                "fn main() -> bool {
                    let k: bool := x;
                    return k;
                }",
                Err("Semantic: L2: x is not defined"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // validate that the RHS of the bind is the correct type
                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the return statement is typed correctly
                    let ret_stm = &fn_main.get_body()[1];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, Some(value)) = ret_stm {
                        assert_eq!(value.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_mutate_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    let mut k: i32 := 5;
                    mut k := 3;
                    return k;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> i32 {
                    let mut k: i32 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: k is of type i32 but is assigned bool"),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    mut k := 3;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    mut k := false;
                    return k;
                }",
                Err("Semantic: L3: Variable k is not mutable"),
            ),
            (
                "fn main() -> i32 {
                    let k: i32 := 5;
                    mut x := false;
                    return k;
                }",
                Err("Semantic: L3: x is not defined"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }

                    // validate the mutate statement is typed correctly
                    let mut_stm = &fn_main.get_body()[1];
                    assert_eq!(mut_stm.get_type(), Unit);
                    if let Ast::Statement(Statement::Bind( box Ast::Mutate(_, _, rhs))) = mut_stm {
                        assert_eq!(rhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_return_statement() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    return 5;
                }",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    return false;
                }",
                Ok(Bool),
            ),
            (
                "fn main() -> string {
                    return \"hello\";
                }",
                Ok(Type::StringLiteral),
            ),
            (
                "fn main() {
                    return;
                }",
                Ok(Type::Unit),
            ),
            (
                "fn main() -> bool {
                    return 5;
                }",
                Err("Semantic: L2: Return expected bool but got i32"),
            ),
            (
                "fn main() {
                    return 5;
                }",
                Err("Semantic: L2: Return expected unit but got i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, value) = ret_stm {
                        let value_ty = value.clone().map(|v| v.get_type().clone()).unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_calls() {
        for (text, expected) in vec![
            (
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return 5;}
                ",
                Ok(I32),
            ),
            (
                // test recursion
                "fn main() -> i32 {
                    return number();
                }
                fn number() -> i32 {return number();}
                ",
                Ok(I32),
            ),
            (
                "fn main() -> bool {
                    return number();
                }
                fn number() -> i32 {return 5;}
                ",
                Err("Semantic: L2: Return expected bool but got i32"),
            ),
            (
                "fn main() -> bool {
                    return bad_fun();
                }
                fn number() -> i32 {return 5;}
                ",
                Err("Semantic: L2: function bad_fun not declared"),
            ),
            (
                "fn main() -> i32 {
                    return add(1, 2);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Ok(I32),
            ),
            (
                "fn main() -> i32 {
                    return add(false, 2);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function add: parameter 1 expected i32 but got bool"),
            ),
            (
                "fn main() -> i32 {
                    return add(1, true);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: One or more parameters have mismatching types for function add: parameter 2 expected i32 but got bool"),
            ),
            (
                "fn main() -> i32 {
                    return add(1);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 1"),
            ),
            (
                "fn main() -> i32 {
                    return add(1, 2, 3);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 3"),
            ),
            (
                "fn main() -> i32 {
                    return add(false);
                }
                fn add(a: i32, b: i32) -> i32 {return a + b;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: add. Expected 2 but got 1"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, value) = ret_stm {
                        let value_ty = value.clone().map(|v| v.get_type().clone()).unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_init() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i32 := init number();
                    return;
                }
                co number() -> i32 {return 5;}
                ",
                Ok(Coroutine(Box::new(I32))),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(3);
                    return;
                }
                co number() -> i32 {return 5;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 0 but got 1"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Ok(Coroutine(Box::new(I32))),
            ),
            (
                "fn main() {
                    let c: co i32 := init number();
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 1 but got 0"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5, 3);
                    return;
                }
                co number(i: i32) -> i32 {return i;}
                ",
                Err("Semantic: L2: Incorrect number of parameters passed to routine: number. Expected 1 but got 2"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number(5);
                    return;
                }
                fn number(i: i32) -> i32 {return i;}
                ",
                Err("Semantic: L2: Expected coroutine but number is a fn (i32) -> i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., lhs))) = bind_stm {
                        assert_eq!(lhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_return_statement() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i32 := init number();
                    return;
                }
                co number() -> i32 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(I32),
            ),
            (
                "fn main() {
                    let c: co i32 := init number();
                    return;
                }
                co number() -> i32 {
                    yret false;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i32 but got bool"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number();
                    return;
                }
                co number() -> i32 {
                    yret;
                    return 5;
                }
                ",
                Err("Semantic: L6: Yield return expected i32 but got unit"),
            ),
            /*
                Need to add a symbol for the unit type
            (
                "fn main() {
                    let c: co := init number();
                    return;
                }
                co number() {
                    yret 5;
                    return;
                }
                ",
                Err("Semantic: L6: Yield return expected unit but got i32"),
            ),*/
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_coroutines()[0].to_routine().unwrap();

                    let yret_stm = &co_number.get_body()[0];
                    assert_eq!(yret_stm.get_type(), Unit);

                    // validate that the RHS of the yield return is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::YieldReturn(.., Some(rhs)))) = yret_stm {
                        assert_eq!(rhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_yield_statement() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let c: co i32 := init number();
                    let i: i32 := yield c;
                    return;
                }
                co number() -> i32 {
                    yret 1;
                    return 5;
                }
                ",
                Ok(I32),
            ),
            (
                "fn main() {
                    let c: bool := false;
                    let i: i32 := yield c;
                    return;
                }
                co number() -> i32 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Yield expects co<_> but got bool"),
            ),
            (
                "fn main() {
                    let c: co i32 := init number();
                    let i: bool := yield c;
                    return;
                }
                co number() -> i32 {
                    yret 1;
                    return 5;
                }
                ",
                Err("Semantic: L3: Bind expected bool but got i32"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_number = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &co_number.get_body()[1];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // validate that the RHS of the bind is the correct type
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., rhs))) = bind_stm {
                        assert_eq!(rhs.get_type(), expected_ty);
                    } else {
                        panic!("Expected a bind statement");
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_function_definition() {
        for (text, expected) in vec![
            (
                "fn main(i: i32) -> i32 {
                    return i;
                }
                ",
                Ok(I32),
            ),
            (
                "fn main(b: bool) -> i32 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i32 but got bool"),
            ),
            (
                "fn main(b: bool) -> i32 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i32 but got unit"),
            ),
            (
                "fn main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &fn_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, value) = ret_stm {
                        let value_ty = value.clone().map(|v| v.get_type().clone()).unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_coroutine_definition() {
        for (text, expected) in vec![
            (
                "co main(i: i32) -> i32 {
                    return i;
                }
                ",
                Ok(I32),
            ),
            (
                "co main(b: bool) -> i32 {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected i32 but got bool"),
            ),
            (
                "co main(b: bool) -> i32 {
                    return;
                }
                ",
                Err("Semantic: L2: Return expected i32 but got unit"),
            ),
            (
                "co main(b: bool) {
                    return b;
                }
                ",
                Err("Semantic: L2: Return expected unit but got bool"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let co_main = module.get_coroutines()[0].to_routine().unwrap();

                    // Check the return value
                    let ret_stm = &co_main.get_body()[0];
                    assert_eq!(ret_stm.get_type(), expected_ty);
                    if let Ast::Return(_, value) = ret_stm {
                        let value_ty = value.clone().map(|v| v.get_type().clone()).unwrap_or(Unit);
                        assert_eq!(value_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_if_expressions() {
        for (text, expected) in vec![
            (
                "fn main() {
                    let x: i32 := if (true) {1} else {2};
                    return;
                }
                ",
                Ok(I32),
            ),
            (
                "fn main() {
                    let x: i32 := if (4) {1} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: Expected boolean expression in if conditional, got: i32"),
            ),
            (
                "fn main() {
                    let x: i32 := if (false) {true} else {2};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected bool got i32"),
            ),
            (
                "fn main() {
                    let x: i32 := if (false) {5} else {\"hello\"};
                    return;
                }
                ",
                Err("Semantic: L2: If expression has mismatching arms: expected i32 got string"),
            ),
            (
                "fn main() {
                    let x: i32 := if (false) {\"true\"} else {\"false\"};
                    return;
                }
                ",
                Err("Semantic: L2: Bind expected i32 but got string"),
            ),
        ] {
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = type_check(&ast, TracingConfig::Off, TracingConfig::Off);
            match expected {
                Ok(expected_ty) => {
                    let module = module.unwrap();
                    let fn_main = module.get_functions()[0].to_routine().unwrap();

                    let bind_stm = &fn_main.get_body()[0];
                    assert_eq!(bind_stm.get_type(), Unit);

                    // Check the return value
                    if let Ast::Statement(Statement::Bind(box Ast::Bind(.., rhs))) = bind_stm {
                        let rhs_ty = rhs.get_type();
                        assert_eq!(rhs_ty, expected_ty);
                    } else {
                        panic!("Expected a return statement")
                    }
                }
                Err(msg) => {
                    assert_eq!(module.unwrap_err(), msg);
                }
            }
        }
    }

    #[test]
    pub fn test_struct_expression() {
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
                Err("Semantic: L7: One or more parameters have mismatching types for function test2: parameter 1 expected root::MyStruct2 but got root::MyStruct"),
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
                .collect::<Result<_>>()
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
                    .collect::<Result<_>>()
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
