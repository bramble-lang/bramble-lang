use crate::compiler::{
    diagnostics::ViewErr,
    diagnostics::{Event, Logger},
    import::Import,
    semantics::error::SemanticError,
    source::SourceIr,
    CompilerError, Span,
};
use log::debug;

use crate::compiler::{ast::*, semantics::stack::SymbolTableScopeStack};

use super::{
    super::semanticnode::SemanticContext, foreach_mut::ForEachPreOrderMut, CanonizeResult,
};

/**
Canonize all the paths in the AST
 */
pub fn canonize_paths(
    module: &mut Module<SemanticContext>,
    imports: &[Import],
    logger: &Logger,
) -> CanonizeResult<()> {
    debug!("Start canonization of paths");

    let mut t = ForEachPreOrderMut::new("Canonize Paths", module, imports);
    t.for_each(module, |stack, node| {
        node.canonize_context_path(stack, logger)
    })?;
    t.for_each(module, |stack, node| node.canonize_type_refs(stack, logger))?;

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
    fn canonize_context_path(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        default_canonize_context_path(self, stack, logger)
    }

    fn canonize_type_refs(
        &mut self,
        _stack: &SymbolTableScopeStack,
        _logger: &Logger,
    ) -> CanonizeResult<()> {
        Ok(())
    }
}

fn default_canonize_context_path<T: Canonizable + ?Sized>(
    node: &mut T,
    stack: &SymbolTableScopeStack,
    logger: &Logger,
) -> CanonizeResult<()> {
    // If this node has a name, then use the current stack to construct
    // a canonical path from the root of the AST to the current node
    // (this is for routine definitions, modules, and structure definitions)
    match node.name() {
        // Set SemanticAnnotation::canonical_path to CanonicalPath
        // Addresses RoutineDefs and StructDefs (for LLVM IR)
        Some(name) => {
            let cpath = stack
                .to_canonical(&vec![Element::Id(name)].into())
                .map_err(|e| CompilerError::new(node.span(), e))
                .view_err(|e| {
                    logger.write(Event::<&Path, _> {
                        stage: "canonize-item-path",
                        input: node.span(),
                        msg: Err(e),
                    })
                })?;

            logger.write(Event::<_, SemanticError> {
                stage: "canonize-item-path",
                input: node.span(),
                msg: Ok(&cpath),
            });

            node.get_context_mut().set_canonical_path(cpath);
        }
        None => (),
    }
    Ok(())
}

impl Canonizable for Expression<SemanticContext> {
    fn canonize_type_refs(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let span = self.span();
        match self {
            Expression::Path(_, ref mut path) => {
                if !path.is_canonical() {
                    stack
                        .to_canonical(path)
                        .and_then(|canonical_path| {
                            record(span, Ok(&canonical_path), logger);

                            *path = canonical_path;

                            Ok(())
                        })
                        .map_err(|e| CompilerError::new(self.span(), e))
                } else {
                    Ok(())
                }
            }
            Expression::RoutineCall(_, _, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .to_canonical(path)
                        .and_then(|canonical_path| {
                            record(span, Ok(&canonical_path), logger);

                            *path = canonical_path;
                            Ok(())
                        })
                        .map_err(|e| CompilerError::new(self.span(), e))
                } else {
                    Ok(())
                }
            }
            Expression::StructExpression(_, ref mut path, _) => {
                if !path.is_canonical() {
                    stack
                        .to_canonical(path)
                        .and_then(|canonical_path| {
                            record(span, Ok(&canonical_path), logger);

                            *path = canonical_path;
                            Ok(())
                        })
                        .map_err(|e| CompilerError::new(self.span(), e))
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
        .view_err(|e| {
            logger.write(Event::<&Path, _> {
                stage: "canonize-type-ref",
                input: self.span(),
                msg: Err(e),
            })
        })
    }
}

impl Canonizable for Statement<SemanticContext> {}

impl Canonizable for Bind<SemanticContext> {
    fn canonize_type_refs(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let canon_type = stack
            .canonize_type(self.get_type())
            .map_err(|e| CompilerError::new(self.span(), e))
            .view_err(|e| {
                logger.write(Event::<&Path, _> {
                    stage: "canonize-type-ref",
                    input: self.span(),
                    msg: Err(e),
                })
            })?;

        canon_type
            .get_path()
            .map(|p| record(self.span(), Ok(p), logger));

        self.set_type(canon_type);
        Ok(())
    }
}

impl Canonizable for Mutate<SemanticContext> {}

impl Canonizable for Module<SemanticContext> {
    fn canonize_context_path(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        // If this node has a name, then use the current stack to construct
        // a canonical path from the root of the AST to the current node
        // (this is for routine definitions, modules, and structure definitions)
        default_canonize_context_path(self, stack, logger)?;

        // Canonize Symbol Table
        // The types used in the routine and structure definitions need to be
        // canonized, otherwise the type checker will not be able to find them,
        // as the type checker expects all type references to be canonized.
        let mut sym = self.context().sym().clone();
        for s in sym.table_mut().iter_mut() {
            let canonized_ty = stack
                .canonize_type(&s.ty)
                .map_err(|e| CompilerError::new(self.span(), e))
                .view_err(|e| {
                    logger.write(Event::<&Path, _> {
                        stage: "canonize-type-ref",
                        input: self.span(),
                        msg: Err(e),
                    })
                })?;
            s.ty = canonized_ty;
        }

        *self.get_context_mut() = self.context().with_sym(sym);

        Ok(())
    }
}

impl Canonizable for StructDef<SemanticContext> {}

impl Canonizable for RoutineDef<SemanticContext> {
    fn canonize_type_refs(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let ctype = stack
            .canonize_type(&self.ret_ty)
            .map_err(|e| CompilerError::new(self.span(), e))
            .view_err(|e| {
                logger.write(Event::<&Path, _> {
                    stage: "canonize-type-ref",
                    input: self.span(),
                    msg: Err(e),
                })
            })?;

        ctype.get_path().map(|p| record(self.span(), Ok(p), logger));

        self.ret_ty = ctype;
        Ok(())
    }
}

impl Canonizable for Extern<SemanticContext> {
    fn canonize_context_path(
        &mut self,
        _: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let name = match self.name() {
            Some(name) => name,
            None => panic!("Externs must have a name"),
        };
        let cpath: Path = vec![Element::Id(name)].into();

        logger.write(Event::<_, SemanticError> {
            stage: "canonize-item-path",
            input: self.span(),
            msg: Ok(&cpath),
        });

        self.get_context_mut().set_canonical_path(cpath);
        Ok(())
    }

    fn canonize_type_refs(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let ctype = stack
            .canonize_type(&self.ty)
            .map_err(|e| CompilerError::new(self.span(), e))
            .view_err(|e| {
                logger.write(Event::<&Path, _> {
                    stage: "canonize-type-ref",
                    input: self.span(),
                    msg: Err(e),
                })
            })?;

        ctype.get_path().map(|p| record(self.span(), Ok(p), logger));

        self.ty = ctype;
        Ok(())
    }
}

impl Canonizable for Parameter<SemanticContext> {
    fn canonize_type_refs(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        let ctype = stack
            .canonize_type(&self.ty)
            .map_err(|e| CompilerError::new(self.span(), e))
            .view_err(|e| {
                logger.write(Event::<&Path, _> {
                    stage: "canonize-type-ref",
                    input: self.span(),
                    msg: Err(e),
                })
            })?;

        ctype.get_path().map(|p| record(self.span(), Ok(p), logger));

        self.ty = ctype;
        Ok(())
    }

    fn canonize_context_path(
        &mut self,
        stack: &SymbolTableScopeStack,
        logger: &Logger,
    ) -> CanonizeResult<()> {
        default_canonize_context_path(self, stack, logger)
    }
}

impl Canonizable for YieldReturn<SemanticContext> {}

impl Canonizable for Return<SemanticContext> {}

fn record(span: Span, path: Result<&Path, &CompilerError<SemanticError>>, logger: &Logger) {
    logger.write(Event::<_, SemanticError> {
        stage: "canonize-type-ref",
        input: span,
        msg: path,
    })
}
