use crate::result::Result;
use log::debug;

use crate::{
    compiler::{ast::*, semantics::stack::SymbolTableScopeStack},
    diagnostics::config::TracingConfig,
    project::manifest::Manifest,
};

use super::{super::semanticnode::SemanticContext, foreach_mut::ForEachPreOrderMut};

/**
Canonize all the paths in the AST
 */
pub fn canonize_paths(
    module: &mut Module<SemanticContext>,
    imports: &[Manifest],
    tracing: TracingConfig,
) -> Result<()> {
    debug!("Start canonization of paths");

    let mut t = ForEachPreOrderMut::new("Canonize Paths", module, imports, tracing, |a| {
        format!("Path: {}", a.get_canonical_path())
    });
    t.for_each(module, |stack, node| node.canonize_context_path(stack))?;
    t.for_each(module, |s, n| n.canonize_type_refs(s))?;

    debug!("Finished canonization of paths");
    Ok(())
}

/// A collection of functions that are specific to Semantic Analysis
/// on the AST. Moving Semantic Analysis functions to a trait allows
/// the structural operations (traversal, scope stack) to be managed
/// by operators (e.g. ForEach) which can then apply transformations
/// that use the SemanticNode information
///
/// Because contextual information is need for semantic analysis
/// operations, the scope stack is passed into these functions.
pub trait Canonizable: Node<SemanticContext> {
    // TODO: make one canonize function that handles everything and then the special cases
    // do their own thing.  I think that will be easier than 3 separate functions
    fn canonize_context_path(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        default_canonize_context_path(self, stack)
    }

    fn canonize_type_refs(&mut self, _stack: &SymbolTableScopeStack) -> Result<()> {
        Ok(())
    }
}

fn default_canonize_context_path<T: Canonizable + ?Sized>(
    node: &mut T,
    stack: &SymbolTableScopeStack,
) -> Result<()> {
    // If this node has a name, then use the current stack to construct
    // a canonical path from the root of the AST to the current node
    // (this is for routine definitions, modules, and structure definitions)
    match node.name() {
        // Set SemanticAnnotation::canonical_path to CanonicalPath
        // Addresses RoutineDefs and StructDefs (for LLVM IR)
        Some(name) => {
            let cpath = stack.to_canonical(&vec![name].into())?;
            node.get_context_mut().set_canonical_path(cpath);
        }
        None => (),
    }
    Ok(())
}

/*fn default_canonize_annotation_type<T: SemanticNode + ?Sized>(
    node: &mut T,
    stack: &SymbolTableScopeStack,
) -> Result<()> {
    // Update the Type information in the annotation data
    let ctype = stack.canonize_local_type_ref(node.annotation().ty())?;
    node.annotation_mut().ty = ctype;
    Ok(())
}*/

impl Canonizable for Expression<SemanticContext> {
    /*fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let canon_ty = match self {
            Expression::ArrayValue(_, elements, len) => {
                if elements.len() == 0 {
                    return Err("Arrays with 0 length are not allowed".into());
                } else {
                    let el_ty = elements[0].annotation().ty.clone();
                    stack.canonize_local_type_ref(&Type::Array(Box::new(el_ty), *len))?
                }
            }
            Expression::StructExpression(_, struct_def, _) => Type::Custom(struct_def.clone()),
            Expression::I8(..) => Type::I8,
            Expression::I16(..) => Type::I16,
            Expression::I32(..) => Type::I32,
            Expression::I64(..) => Type::I64,
            Expression::U8(..) => Type::U8,
            Expression::U16(..) => Type::U16,
            Expression::U32(..) => Type::U32,
            Expression::U64(..) => Type::U64,
            Expression::Boolean(..) => Type::Bool,
            Expression::StringLiteral(..) => Type::StringLiteral,
            _ => Type::Unknown,
            //_ => default_canonize_annotation_type(self, stack)?,
        };
        self.annotation_mut().ty = canon_ty;
        Ok(())
    }*/

    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        match self {
            Expression::Path(_, ref mut path) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(_, canonical_path)| {
                            *path = canonical_path;
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            Expression::RoutineCall(_, ref mut call_target, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(sym, canonical_path)| {
                            *path = canonical_path;
                            if sym.is_extern {
                                *call_target = RoutineCall::Extern;
                            }
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            Expression::StructExpression(_, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .lookup_symbol_by_path(path)
                        .and_then(|(_, canonical_path)| {
                            *path = canonical_path;
                            Ok(())
                        })
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}
impl Canonizable for Statement<SemanticContext> {}
impl Canonizable for Bind<SemanticContext> {
    /*fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        self.annotation_mut().ty = stack.canonize_local_type_ref(self.get_type())?;
        Ok(())
    }*/

    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let canon_type = stack.canonize_type(self.get_type())?;
        self.set_type(canon_type);
        Ok(())
    }
}
impl Canonizable for Mutate<SemanticContext> {}
impl Canonizable for Module<SemanticContext> {
    fn canonize_context_path(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        // If this node has a name, then use the current stack to construct
        // a canonical path from the root of the AST to the current node
        // (this is for routine definitions, modules, and structure definitions)
        default_canonize_context_path(self, stack)?;

        // Canonize Symbol Table
        let sym = &mut self.get_context_mut().sym;
        for s in sym.table_mut().iter_mut() {
            let cty = stack.canonize_type(&s.ty)?;
            s.ty = cty;
        }

        Ok(())
    }
}
impl Canonizable for StructDef<SemanticContext> {}
impl Canonizable for RoutineDef<SemanticContext> {
    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_type(&self.ret_ty)?;
        self.ret_ty = ctype;
        Ok(())
    }
}
impl Canonizable for Extern<SemanticContext> {
    fn canonize_context_path(&mut self, _: &SymbolTableScopeStack) -> Result<()> {
        let name = match self.name() {
            Some(name) => name,
            None => panic!("Externs must have a name"),
        };
        let cpath = vec![name].into();
        self.get_context_mut().set_canonical_path(cpath);
        Ok(())
    }

    /*fn canonize_annotation_type(&mut self, _: &SymbolTableScopeStack) -> Result<()> {
        self.annotation_mut().ty = self.get_return_type().clone();
        Ok(())
    }*/

    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_type(&self.ty)?;
        self.ty = ctype;
        Ok(())
    }
}
impl Canonizable for Parameter<SemanticContext> {
    fn canonize_type_refs(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let ctype = stack.canonize_type(&self.ty)?;
        self.ty = ctype;
        Ok(())
    }
}
impl Canonizable for YieldReturn<SemanticContext> {
    /*fn canonize_annotation_type(&mut self, stack: &SymbolTableScopeStack) -> Result<()> {
        let current_fn = stack
            .get_current_fn()
            .ok_or("YieldReturn must appear within a function")?;
        let (_, ret_ty) = stack.lookup_coroutine(current_fn)?;
        self.annotation_mut().ty = stack.canonize_local_type_ref(ret_ty)?;
        Ok(())
    }*/
}
impl Canonizable for Return<SemanticContext> {}
