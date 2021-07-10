use serde::{Deserialize, Serialize};

use super::{path::Path, HasVarArgs};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    StringLiteral,
    Array(Box<Type>, usize),
    Unit,
    Custom(Path),
    StructDef(Vec<(String, Type)>),
    FunctionDef(Vec<Type>, Box<Type>),
    CoroutineDef(Vec<Type>, Box<Type>),
    Coroutine(Box<Type>),
    ExternDecl(Vec<Type>, HasVarArgs, Box<Type>),
    Unknown,
}

impl Type {
    pub fn get_path(&self) -> Option<&Path> {
        match self {
            Type::Custom(path) => Some(path),
            _ => None,
        }
    }
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

    pub fn is_integral(&self) -> bool {
        match self {
            Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64 => true,
            Type::Bool
            | Type::StringLiteral
            | Type::Array(_, _)
            | Type::Unit
            | Type::Custom(_)
            | Type::StructDef(_)
            | Type::FunctionDef(_, _)
            | Type::CoroutineDef(_, _)
            | Type::Coroutine(_)
            | Type::ExternDecl(..)
            | Type::Unknown => false,
        }
    }

    pub fn is_unsigned_int(&self) -> bool {
        match self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => true,
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Bool
            | Type::StringLiteral
            | Type::Array(_, _)
            | Type::Unit
            | Type::Custom(_)
            | Type::StructDef(_)
            | Type::FunctionDef(_, _)
            | Type::CoroutineDef(_, _)
            | Type::Coroutine(_)
            | Type::ExternDecl(..)
            | Type::Unknown => false,
        }
    }

    pub fn is_signed_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::Bool
            | Type::StringLiteral
            | Type::Array(_, _)
            | Type::Unit
            | Type::Custom(_)
            | Type::StructDef(_)
            | Type::FunctionDef(_, _)
            | Type::CoroutineDef(_, _)
            | Type::Coroutine(_)
            | Type::ExternDecl(..)
            | Type::Unknown => false,
        }
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
            U8 => f.write_str("u8"),
            U16 => f.write_str("u16"),
            U32 => f.write_str("u32"),
            U64 => f.write_str("u64"),
            I8 => f.write_str("i8"),
            I16 => f.write_str("i16"),
            I32 => f.write_str("i32"),
            I64 => f.write_str("i64"),
            Bool => f.write_str("bool"),
            StringLiteral => f.write_str("string"),
            Array(ty, len) => f.write_str(&format!("[{}; {}]", ty, len)),
            Unit => f.write_str("unit"),
            Custom(path) => f.write_str(&format!("{}", path)),
            StructDef(members) => {
                let members = members
                    .iter()
                    .map(|m| format!("{}: {}", m.0, m.1))
                    .collect::<Vec<String>>()
                    .join(",");
                f.write_fmt(format_args!("StructDef({})", &members))
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
            Type::ExternDecl(params, has_varargs, ret_ty) => {
                let mut params = params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(",");
                if *has_varargs {
                    params += ", ...";
                }
                f.write_fmt(format_args!("extern fn ({}) -> {}", params, ret_ty))
            }
            Unknown => f.write_str("unknown"),
        }
    }
}
