use super::path::Path;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    I64,
    Bool,
    StringLiteral,
    Unit,
    Custom(Path),
    StructDef(Vec<(String, Type)>),
    FunctionDef(Vec<Type>, Box<Type>),
    CoroutineDef(Vec<Type>, Box<Type>),
    Coroutine(Box<Type>),
    Unknown,
}

impl Type {
    pub fn get_members(&self) -> Option<&Vec<(String, Type)>> {
        match self {
            Type::StructDef(members) => Some(members),
            _ => None,
        }
    }

    pub fn get_member(&self, member: &str) -> Option<&Type> {
        self.get_members()
            .map(|ms| ms.iter().find(|(n, _)| n == member).map(|m| &m.1))
            .flatten()
    }
}

impl PartialEq<Type> for &Type {
    fn eq(&self, other: &Type) -> bool {
        *self == other
    }
}

impl PartialEq<&Type> for Type {
    fn eq(&self, other: &&Type) -> bool {
        self == *other
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Type::*;
        match self {
            I64 => f.write_str("i64"),
            Bool => f.write_str("bool"),
            StringLiteral => f.write_str("string"),
            Unit => f.write_str("unit"),
            Custom(path) => f.write_str(&format!("{}", path)),
            StructDef(members) => {
                let members = members
                    .iter()
                    .map(|m| format!("{}: {}", m.0, m.1))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_str(&members)
            }
            Type::CoroutineDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_fmt(format_args!("co ({}) -> {}", params, ret_ty))
            }
            Type::Coroutine(ret_ty) => f.write_fmt(format_args!("co<{}>", ret_ty)),
            Type::FunctionDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_fmt(format_args!("fn ({}) -> {}", params, ret_ty))
            }
            Unknown => f.write_str("unknown"),
        }
    }
}
