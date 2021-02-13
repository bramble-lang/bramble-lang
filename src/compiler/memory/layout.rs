use super::{struct_table, symbol_table::Symbol};
use struct_table::ResolvedStructTable;

use crate::ast::{
    annotate::map::MapPreOrder,
    node::{Node, NodeType},
    routinedef::RoutineDefType,
};
use crate::{
    ast::module::Module,
    compiler::memory::scope::{CompilerAnnotation, LayoutData},
    semantics::semanticnode::SemanticAnnotations,
};
use braid_lang::result::Result;

/**
 * Compute the how every function and struct will be laid out in memory: for functions
 * this computes the size of the stack frame and the offset within the stack frame for
 * every function parameter and variable that is local to the function, for structures
 * it computes how large the structure is and what the relative offset of every field
 * in the structure is.
 */
pub fn compute_layout_for_program(
    ast: &Module<SemanticAnnotations>,
) -> Result<(Module<CompilerAnnotation>, ResolvedStructTable)> {
    let unresolved_struct_table = struct_table::UnresolvedStructTable::from_module(ast)?;
    let struct_table = unresolved_struct_table.resolve()?;

    let compiler_ast = generate_stackframe_layout(ast, &struct_table);

    Ok((compiler_ast, struct_table))
}

pub fn generate_stackframe_layout(
    ast: &Module<SemanticAnnotations>,
    struct_table: &ResolvedStructTable,
) -> Module<CompilerAnnotation> {
    let mut current_layout = LayoutData::new(0);
    let mut f = |n: &dyn Node<SemanticAnnotations>| {
        let (annotation, layout) = match n.node_type() {
            NodeType::Module => CompilerAnnotation::module_from(
                n.annotation(),
                n.name().expect("Modules must have a name"),
                struct_table,
            ),
            NodeType::FnDef => CompilerAnnotation::routine_from(
                n.annotation(),
                &RoutineDefType::Function,
                struct_table,
            ),
            NodeType::CoroutineDef => CompilerAnnotation::routine_from(
                n.annotation(),
                &RoutineDefType::Coroutine,
                struct_table,
            ),
            NodeType::StructDef => CompilerAnnotation::structdef_from(n.annotation()),
            NodeType::Parameter => {
                CompilerAnnotation::local_from(n.annotation(), struct_table, current_layout)
            }
            NodeType::Expression => {
                CompilerAnnotation::local_from(n.annotation(), struct_table, current_layout)
            }
            NodeType::Statement => {
                CompilerAnnotation::local_from(n.annotation(), struct_table, current_layout)
            }
            NodeType::RoutineCall | NodeType::BinOp => {
                let (mut a, layout) =
                    CompilerAnnotation::local_from(n.annotation(), struct_table, current_layout);
                current_layout = layout;

                // Allocate space on the stack for these intermediate results to be stored
                // so that we can use two registers for all expression evalutation. In the
                // future, this wil lbe updated to more efficiently use registers.
                for c in n.children() {
                    current_layout = allocate_into_stackframe(
                        &mut a,
                        c.annotation(),
                        current_layout,
                        struct_table,
                    );
                }
                (a, current_layout)
            }
        };
        current_layout = layout;
        annotation
    };

    let mapper = MapPreOrder::new("layout");
    mapper.for_module(ast, &mut f)
}

fn allocate_into_stackframe(
    current: &mut CompilerAnnotation,
    child: &SemanticAnnotations,
    layout: LayoutData,
    struct_table: &ResolvedStructTable,
) -> LayoutData {
    let anonymous_name = child.anonymous_name();
    let sz = struct_table
        .size_of(child.ty())
        .expect("Expected a size for an expression");

    let layout = LayoutData::new(layout.offset + sz);
    current.symbols.table.insert(
        anonymous_name.clone(),
        Symbol::new(&anonymous_name, sz, layout.offset),
    );
    layout
}
