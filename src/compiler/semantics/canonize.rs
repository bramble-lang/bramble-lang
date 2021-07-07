use crate::{
    compiler::ast::{Module, Node},
    diagnostics::config::TracingConfig,
};

use super::{
    foreach_mut::ForEachPreOrderMut, semanticnode::SemanticAnnotations,
    stack::SymbolTableScopeStack,
};

/**
Canonize all the paths in the AST
 */

fn canonize_paths(module: &mut Module<SemanticAnnotations>) {
    let mut t = ForEachPreOrderMut::new("test", module, TracingConfig::Off, |_| "test".into());
    t.for_each(module, |s, n| canonize(s, n));
}

fn canonize(stack: &SymbolTableScopeStack, node: &mut dyn Node<SemanticAnnotations>) {
    match node.name() {
        // Set SemanticAnnotation::canonical_path to CanonicalPath
        // Addresses RoutineDefs and StructDefs (for LLVM IR)
        Some(name) => {
            let cpath = stack.to_canonical(&vec![name].into()).unwrap();
            node.annotation_mut().set_canonical_path(cpath);
        }
        None => (),
    }

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
