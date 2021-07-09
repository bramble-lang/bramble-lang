use braid_lang::result::Result;

use crate::{
    compiler::ast::Module, diagnostics::config::TracingConfig, project::manifest::Manifest,
};

use super::{
    foreach_mut::{ForEachPreOrderMut, SemanticNode},
    semanticnode::SemanticAnnotations,
    stack::SymbolTableScopeStack,
};

/**
Canonize all the paths in the AST
 */

pub fn canonize_paths(
    module: &mut Module<SemanticAnnotations>,
    imports: &[Manifest],
    tracing: TracingConfig,
) -> Result<()> {
    let mut t = ForEachPreOrderMut::new("annotation path", module, imports, tracing, |_| {
        "annotation path".into()
    });
    t.for_each(module, |s, n| canonize(s, n))?;

    /*
    let mut t = ForEachPreOrderMut::new("annotation type", module, tracing, |_| {
        "annotation type".into()
    });
    t.for_each(module, |s, n| n.canonize_annotation_type(s))?;
    */

    let mut t = ForEachPreOrderMut::new("type refs", module, imports, tracing, |_| "test".into());
    t.for_each(module, |s, n| n.canonize_type_refs(s))?;
    Ok(())
}

fn canonize(stack: &SymbolTableScopeStack, node: &mut dyn SemanticNode) -> Result<()> {
    node.canonize_annotation_path(stack)

    // What about Expression::CustomType, Path, RoutineCall, StructExpression?
    // What about SemanticAnnotations::Type => This will be a path to the TypeDefinition
    // What about Type::Custom? This will be a path to the type definition
    // Could I create a ToCanonical method on the Node trait that takes a &Stack and sets the canonical info
    // Or I could create a Transform trait which defines a function that is called by the ForEach operator
    //     and then, similar to Serde, define the implementation of the trait locally to the use of ForEach.
    // Or, can I make the trait a type Parameter and then define relevant traits within the process step scope
    //     e.g. a ToCanonical trait here with default impl for everything except Expression and Type.
    // Looks like the best option I have is to add a method to Node that will do to_canonical
}
