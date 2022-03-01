//! Converts the Bramble AST to the CFG MIR representation used for
//! dataflow analyses; such as, lifetime checking, variable initialization,
//! consistency rules checking, and so on.

// Transformer
// This process takes the AST for a compilation unit and transforms it into the
// CFG MIR used for dataflow analysis and LLVM IR generation by the Bramble
// compiler.

pub struct MirGenerator {}

impl MirGenerator {
    pub fn new() -> MirGenerator {
        todo!()
    }

    fn module(&self) {}

    fn func(&self) {}

    fn expr(&self) {}

    fn stmt(&self) {}

    fn ret(&self) {}

    fn bind(&self) {}
}
