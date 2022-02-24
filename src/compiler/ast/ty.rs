use crate::{
    compiler::{CompilerDisplay, CompilerDisplayError, SourceMap},
    StringId, StringTable,
};

use super::{path::Path, HasVarArgs};

/**
The actual types which a value can have in Bramble.  This covers base types along
with aggregate types (the array and the structure).  This also includes the `Unknown`
type, which is used when a type for a value has not yet been resolved by the
Semantic Analyzer.
 */
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Null,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F64,
    Bool,
    StringLiteral,
    RawPointer(PointerMut, Box<Type>),
    Array(Box<Type>, usize),
    Unit,
    Custom(Path),
    StructDef(Vec<(StringId, Type)>),
    FunctionDef(Vec<Type>, Box<Type>),
    CoroutineDef(Vec<Type>, Box<Type>),
    Coroutine(Box<Type>),
    ExternDecl(Vec<Type>, HasVarArgs, Box<Type>),
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum PointerMut {
    Mut,
    Const,
}

impl std::fmt::Display for PointerMut {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PointerMut::Mut => f.write_str("mut"),
            PointerMut::Const => f.write_str("const"),
        }
    }
}

impl Type {
    /// Returns `true` if the the provided type can be assigned
    /// to variables of this [`Type`].
    /// 
    /// If this is [`Type::Null`] then this will always return
    /// false, because there is no addressable value which has typ
    /// [`Type::Null`].
    pub fn can_be_assigned(&self, r: &Self) -> bool {
        match self {
            Self::RawPointer(..) => {
                r == &Self::Null || self == r
            },
            Self::Null => r == &Self::Null || r.can_be_assigned(&Self::Null),
            Self::Array(ty, sz) => {
                if let Self::Array(rty, rsz) = r {
                    sz == rsz && ty.can_be_assigned(rty)
                } else {
                    false
                }
            }
            _ => self == r,
        }
    }

    /// Returns `true` if these types can be compared with
    /// each other
    pub fn can_be_compared(&self, r: &Self) -> bool {
        match self {
            Self::RawPointer(..) => {
                r == &Self::Null || self == r
            },
            Self::Null => {
                r == &Self::Null || r.can_be_compared(&Self::Null)
            }
            _ => self == r,
        }
    }

    pub fn can_be_cast(&self) -> bool {
        match self {
            Type::Null => false,
            Type::U8 => true,
            Type::U16 => true,
            Type::U32 => true,
            Type::U64 => true,
            Type::I8 => true,
            Type::I16 => true,
            Type::I32 => true,
            Type::I64 => true,
            Type::F64 => true,
            Type::Bool => true,
            Type::StringLiteral => false,
            Type::RawPointer(_, _) => true,
            Type::Array(_, _) => false,
            Type::Unit => false,
            Type::Custom(_) => false,
            Type::StructDef(_) => false,
            Type::FunctionDef(_, _) => false,
            Type::CoroutineDef(_, _) => false,
            Type::Coroutine(_) => false,
            Type::ExternDecl(_, _, _) => false,
            Type::Unknown => false,
        }
    }

    /// Returns whether this type can be cast to the target type.
    pub fn can_cast_to(&self, r: &Self) -> bool {
        self.can_be_cast() && r.can_be_cast()
    }
    
    pub fn get_path(&self) -> Option<&Path> {
        match self {
            Type::Custom(path) => Some(path),
            _ => None,
        }
    }

    pub fn get_members(&self) -> Option<&Vec<(StringId, Type)>> {
        match self {
            Type::StructDef(members) => Some(members),
            _ => None,
        }
    }

    pub fn get_member(&self, member: StringId) -> Option<&Type> {
        self.get_members()
            .map(|ms| ms.iter().find(|(n, _)| *n == member).map(|m| &m.1))
            .flatten()
    }

    pub fn is_number(&self) -> bool {
        match self {
            Type::F64
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64 => true,
            Type::Null
            | Type::Bool
            | Type::StringLiteral
            | Type::RawPointer(..)
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
            Type::Null
            | Type::Bool
            | Type::F64
            | Type::StringLiteral
            | Type::RawPointer(..)
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

    pub fn is_float(&self) -> bool {
        match self {
            | Type::F64 => true,
            Type::Null
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Bool
            | Type::StringLiteral
            | Type::RawPointer(..)
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
            Type::Null
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::F64
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
            | Type::RawPointer(..)
            | Type::Unknown => false,
        }
    }

    pub fn is_signed_int(&self) -> bool {
        match self {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 => true,
            Type::Null
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::F64
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
            | Type::RawPointer(..)
            | Type::Unknown => false,
        }
    }

    pub fn is_raw_pointer(&self) -> bool {
        match self {
            Type::RawPointer(..) => true,
            _ => false,
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

impl CompilerDisplay for Type {
    fn fmt(&self, sm: &SourceMap, st: &StringTable) -> Result<String, CompilerDisplayError> {
        match self {
            Type::Custom(path) => path.fmt(sm, st),
            Type::Coroutine(ty) => Ok(format!("co<{}>", ty.fmt(sm, st)?)),
            Type::Array(ty, sz) => Ok(format!("[{}; {}]", ty.fmt(sm, st)?, sz)),
            Type::RawPointer(m, ty) => Ok(format!("*{} {}", m, ty.fmt(sm, st)?)),
            Type::ExternDecl(params, has_varargs, ret_ty) => {
                let mut params = params
                    .iter()
                    .map(|p| p.fmt(sm, st))
                    .collect::<Result<Vec<String>, _>>()?
                    .join(",");
                if *has_varargs {
                    params += ", ...";
                }
                Ok(format!("extern fn ({}) -> {}", params, ret_ty))
            }
            Type::StructDef(fields) => {
                let fields = fields
                    .iter()
                    .map(|(sid, f)| {
                        st.get(*sid)
                            .map_err(|e| e.into())
                            .and_then(|fname| f.fmt(sm, st).map(|fs| format!("{}: {}", fname, fs)))
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .join(",");
                Ok(format!("StructDef({})", fields))
            }
            Type::FunctionDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| p.fmt(sm, st))
                    .collect::<Result<Vec<String>, _>>()?
                    .join(",");

                Ok(format!("fn ({}) -> {}", params, ret_ty))
            }
            Type::CoroutineDef(params, ret_ty) => {
                let params = params
                    .iter()
                    .map(|p| p.fmt(sm, st))
                    .collect::<Result<Vec<String>, _>>()?
                    .join(",");

                Ok(format!("co ({}) -> {}", params, ret_ty))
            }
            _ => Ok(format!("{}", self)),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Type::*;
        match self {
            Null => f.write_str("null"),
            U8 => f.write_str("u8"),
            U16 => f.write_str("u16"),
            U32 => f.write_str("u32"),
            U64 => f.write_str("u64"),
            I8 => f.write_str("i8"),
            I16 => f.write_str("i16"),
            I32 => f.write_str("i32"),
            I64 => f.write_str("i64"),
            F64 => f.write_str("f64"),
            Bool => f.write_str("bool"),
            StringLiteral => f.write_str("string"),
            RawPointer(mutability, ty) => {
                if *mutability == PointerMut::Mut {
                    f.write_str(&format!("*mut {}", ty))
                } else {
                    f.write_str(&format!("*const {}", ty))
                }
            }
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
