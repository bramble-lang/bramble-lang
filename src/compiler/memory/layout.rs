use super::{struct_table, symbol_table::Symbol};
use struct_table::ResolvedStructTable;

use crate::ast::*;
use crate::{
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

fn generate_stackframe_layout(
    ast: &Module<SemanticAnnotations>,
    struct_table: &ResolvedStructTable,
) -> Module<CompilerAnnotation> {
    let mut current_layout = LayoutData::new(0);
    let f = |n: &dyn Node<SemanticAnnotations>| {
        let (annotation, layout) = match n.node_type() {
            NodeType::Module => CompilerAnnotation::module_from(
                n.annotation(),
                n.name().expect("Modules must have a name"),
                struct_table,
            ),
            NodeType::RoutineDef(rty) => {
                CompilerAnnotation::routine_from(n.annotation(), rty, struct_table)
            }
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

    let mut mapper = MapPreOrder::new("layout", f);
    mapper.apply(ast)
}

impl RoutineDef<CompilerAnnotation> {
    pub fn total_allocation(&self) -> i32 {
        let init = if self.def == RoutineDefType::Coroutine {
            40
        } else {
            0
        };
        self.iter_preorder().fold(init, |acc, n| {
            n.annotation()
                .symbols()
                .iter()
                .fold(acc, |acc, s| acc.max(s.1.offset))
        })
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        compiler::memory::symbol_table::SymbolTable,
        diagnostics::config::TracingConfig,
        lexer::{lexer::Lexer, tokens::Token},
        parser::parser,
        semantics::type_resolver::resolve_types,
    };
    use braid_lang::result::Result;

    #[test]
    fn function_symbols() {
        for (ln, text, expected, allocation) in vec![
            (
                line!(),
                "
                fn test() {
                    return;
                }
                    ",
                vec![],
                0,
            ),
            (
                line!(),
                "
                fn test(x: i32) {
                    return;
                }
                    ",
                vec![("x", 4, 4)],
                4,
            ),
            (
                line!(),
                "
                fn test(x: i32) {
                    let y: i64 := 50;
                    return;
                }
                    ",
                vec![("x", 4, 4), ("y", 8, 12)],
                12,
            ),
            (
                line!(),
                "
                fn test() -> i32 {
                    return 5i32;
                }
                    ",
                vec![],
                0,
            ),
            (
                line!(),
                "
                fn test() -> i64 {
                    return 5 + 6;
                }
                    ",
                vec![("!_3", 8, 8), ("!_4", 8, 16)],
                16,
            ),
            (
                line!(),
                "
                fn test() {
                    let x: i64 := 1;
                    let y: i64 := 2;
                    fake(x, y);
                    return;
                }
                fn fake(i: i64, j: i64) {return;}
                    ",
                vec![("x", 8, 8), ("y", 8, 16), ("!_5", 8, 24), ("!_6", 8, 32)],
                32,
            ),
        ] {
            println!("Running test: {}", ln);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();

            let (compiler_ast, ..) = compute_layout_for_program(&module).unwrap();
            if let Some(Item::Routine(func)) = compiler_ast.get_item("test") {
                assert_eq!(func.total_allocation(), allocation);
                check_symbols(func, expected).unwrap();
            } else {
                panic!("Could not find function test");
            }
        }
    }

    #[test]
    fn coroutine_symbol() {
        for (ln, text, expected, allocation) in vec![
            (
                line!(),
                "
                co test() {
                    return;
                }
                    ",
                vec![],
                40,
            ),
            (
                line!(),
                "
                co test(x: i32) {
                    return;
                }
                    ",
                vec![("x", 4, 44)],
                44,
            ),
            (
                line!(),
                "
                co test(x: i32) {
                    let y: i64 := 50;
                    return;
                }
                    ",
                vec![("x", 4, 44), ("y", 8, 52)],
                52,
            ),
            (
                line!(),
                "
                co test() -> i32 {
                    return 5i32;
                }
                    ",
                vec![],
                40,
            ),
            (
                line!(),
                "
                co test() -> i64 {
                    return 5 + 6;
                }
                    ",
                vec![("!_3", 8, 48), ("!_4", 8, 56)],
                56,
            ),
            (
                line!(),
                "
                co test() {
                    let x: i64 := 1;
                    let y: i64 := 2;
                    fake(x, y);
                    return;
                }
                fn fake(i: i64, j: i64) {return;}
                    ",
                vec![("x", 8, 48), ("y", 8, 56), ("!_9", 8, 64), ("!_10", 8, 72)],
                72,
            ),
        ] {
            println!("Running test: {}", ln);
            let tokens: Vec<Token> = Lexer::new(&text)
                .tokenize()
                .into_iter()
                .collect::<Result<_>>()
                .unwrap();
            let ast = parser::parse(tokens).unwrap().unwrap();
            let module = resolve_types(&ast, TracingConfig::Off, TracingConfig::Off).unwrap();

            let (compiler_ast, ..) = compute_layout_for_program(&module).unwrap();
            if let Some(Item::Routine(func)) = compiler_ast.get_item("test") {
                assert_eq!(func.total_allocation(), allocation);
                check_symbols(func, expected).unwrap();
            } else {
                panic!("Could not find function test");
            }
        }
    }

    fn find_all_symbols(rd: &RoutineDef<CompilerAnnotation>) -> SymbolTable {
        let mut symbols = SymbolTable::new();
        for n in rd.iter_preorder() {
            for (name, sym) in n.annotation().symbols().iter() {
                symbols.table.insert(name.into(), sym.clone());
            }
        }
        symbols
    }

    fn check_symbols(
        rd: &RoutineDef<CompilerAnnotation>,
        expected: Vec<(&str, i32, i32)>,
    ) -> Result<()> {
        let test = find_all_symbols(rd);
        if test.table.len() != expected.len() {
            return Err(format!(
                "Expected table size to be {} but got {}\n\nTest:\n{}",
                expected.len(),
                test.table.len(),
                test,
            ));
        }
        for (en, esz, eo) in expected {
            if let Some(s) = test.table.get(en) {
                if s.size != esz {
                    return Err(format!(
                        "{} expected size is {} but got {}",
                        en, esz, s.size
                    ));
                }
                if s.offset != eo {
                    return Err(format!(
                        "{} expected offset is {} but got {}",
                        en, eo, s.offset
                    ));
                }
            } else {
                return Err(format!("Does not contain {}: \n\n Test: {}", en, test));
            }
        }
        Ok(())
    }
}
