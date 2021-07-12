use serde::{Deserialize, Serialize};

use crate::diagnostics::config::TracingConfig;
use crate::{
    compiler::{ast::*, parser::parser::ParserInfo},
    diagnostics::{Diag, DiagData},
};

use super::symbol_table::SymbolTable;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SemanticAnnotations {
    pub id: u32,
    pub ln: u32,
    pub ty: Type,
    pub sym: SymbolTable,
    pub canonical_path: Path,
}

impl Annotation for SemanticAnnotations {
    fn id(&self) -> u32 {
        self.id
    }

    fn line(&self) -> u32 {
        self.ln
    }
}

impl Diag for SemanticAnnotations {
    fn diag(&self) -> DiagData {
        let mut dd = DiagData::new(self.ln, self.id);

        if self.sym.size() > 0 {
            dd.add("sym", &format!("{}", self.sym));
        }

        if self.canonical_path.len() > 0 {
            dd.add("canon path", &format!("{}", self.canonical_path));
        }

        dd
    }
}

impl SemanticAnnotations {
    pub fn anonymous_name(&self) -> String {
        format!("!{}_{}", self.canonical_path, self.id)
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

pub type SemanticNode = Expression<SemanticAnnotations>;

impl SemanticNode {
    pub fn get_type(&self) -> &Type {
        let meta = self.annotation();
        &meta.ty
    }
}

impl Statement<SemanticAnnotations> {
    pub fn get_type(&self) -> &Type {
        let m = self.annotation();
        &m.ty
    }
}

impl SemanticAnnotations {
    pub fn new(id: u32, ln: u32, ty: Type) -> SemanticAnnotations {
        SemanticAnnotations {
            id,
            ln,
            ty,
            sym: SymbolTable::new(),
            canonical_path: Path::new(),
        }
    }

    pub fn new_routine(id: u32, ln: u32, name: &str, ty: Type) -> SemanticAnnotations {
        SemanticAnnotations {
            id,
            ln,
            ty,
            sym: SymbolTable::new_routine(name),
            canonical_path: Path::new(),
        }
    }

    pub fn new_module(id: u32, ln: u32, name: &str, ty: Type) -> SemanticAnnotations {
        SemanticAnnotations {
            id,
            ln,
            ty,
            sym: SymbolTable::new_module(name),
            canonical_path: Path::new(),
        }
    }

    pub fn get_canonical_path(&self) -> &Path {
        &self.canonical_path
    }

    pub fn set_canonical_path(&mut self, path: Path) {
        self.canonical_path = path;
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

pub struct SemanticAst {
    next_id: u32,
    tracing: TracingConfig,
}

impl SemanticAst {
    pub fn new() -> SemanticAst {
        SemanticAst {
            next_id: 0,
            tracing: TracingConfig::Off,
        }
    }

    pub fn from_module(
        &mut self,
        m: &Module<ParserInfo>,
        tracing: TracingConfig,
    ) -> Module<SemanticAnnotations> {
        let f = |n: &dyn Node<u32>| match n.node_type() {
            NodeType::Module => {
                let name = n.name().expect("Modules must have a name");
                self.module_semantic_annotations_from(*n.annotation(), name)
            }
            NodeType::RoutineDef(_) => {
                let name = n.name().expect("RoutineDefs must have a name");
                self.routine_semantic_annotations_from(*n.annotation(), name)
            }
            _ => self.semantic_annotations_from(*n.annotation()),
        };

        let mut mapper = MapPreOrder::new("parser-to-semantic", f, tracing);
        mapper.apply(m)
    }

    fn semantic_annotations_from(&mut self, ln: u32) -> SemanticAnnotations {
        let sm_data = SemanticAnnotations::new(self.next_id, ln, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn routine_semantic_annotations_from(&mut self, ln: u32, name: &str) -> SemanticAnnotations {
        let sm_data = SemanticAnnotations::new_routine(self.next_id, ln, name, Type::Unknown);
        self.next_id += 1;
        sm_data
    }

    fn module_semantic_annotations_from(&mut self, ln: u32, name: &str) -> SemanticAnnotations {
        let sm_data = SemanticAnnotations::new_module(self.next_id, ln, name, Type::Unknown);
        self.next_id += 1;
        sm_data
    }
}
