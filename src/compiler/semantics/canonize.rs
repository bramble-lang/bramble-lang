use braid_lang::result::Result;
use log::debug;

use crate::{
    compiler::ast::Module, diagnostics::config::TracingConfig, project::manifest::Manifest,
};

use super::{foreach_mut::ForEachPreOrderMut, semanticnode::SemanticAnnotations};

/**
Canonize all the paths in the AST
 */
pub fn canonize_paths(
    module: &mut Module<SemanticAnnotations>,
    imports: &[Manifest],
    tracing: TracingConfig,
) -> Result<()> {
    debug!("Start canonization of paths");

    let mut t = ForEachPreOrderMut::new("annotation path", module, imports, tracing, |_| {
        "annotation path".into()
    });
    t.for_each(module, |stack, node| node.canonize_annotation_path(stack))?;

    let mut t = ForEachPreOrderMut::new("type refs", module, imports, tracing, |_| "test".into());
    t.for_each(module, |s, n| n.canonize_type_refs(s))?;

    debug!("Finished canonization of paths");
    Ok(())
}
