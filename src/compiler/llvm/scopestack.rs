/*!
The Scope Stack stores a mapping from Bramble Identifiers to LLVM registers
while preserving the scope hierarchy.
*/
use std::collections::HashMap;

use crate::result::Result;
use inkwell::values::AnyValueEnum;

struct IdToValueMap<'ctx> {
    map: HashMap<String, AnyValueEnum<'ctx>>,
}

impl<'ctx> IdToValueMap<'ctx> {
    fn new() -> IdToValueMap<'ctx> {
        IdToValueMap {
            map: HashMap::new(),
        }
    }
}

/**
Manages the LLVM registers and values which represent identifiers that
 are constructed while compiling a function into LLVM IR. Through this
 struct the user can look up what LLVM Value is associated with a given
 identifier in the current scope and then use that Value when compiling
 expressions which reference the Identifier.

 The semantics work around managing function scope and local scope. When
 starting the compilation of a new function call `open_fn` to start a
 function level scope and beging tracking Identifier->Value mappings within
 that scope.  `open_local` will start a new local scope and new mappings
 will be added to that local scope. `close_local` will close the current
 local scope and move up the hierarchy to the parent scope. `close_fn` will
 close the function scope and should only be called after all local scopes
 have been closed and the function is done being compiled.

 - `open_fn` will open a new function level scope. It will return an `Err` if
 the there is already an open scope.

 - `close_fn` will close out the function level scope.  If there are still open
 local scopes then the function level scope cannot be closed and this will
 return an `Err`

 - `open_local` will open a new local scope within a function level scope. If
 there is no open function scope then this will return an `Err`.

 - `close_local` will close the current local scope. If there is no open local
 scope this will return an `Err`.

 - `get` takes an identifier name and searches through the scopes starting at
 the top and moving down to the function scope. It will return the first
 match that it finds, otherwise it will return None.

 - `insert` will insert a new mapping into the current scope. It will return
 `Err` if no scope is open (e.g. if you attempt to `insert` after `close_fn`).
*/
pub struct RegisterLookup<'ctx> {
    stack: Vec<IdToValueMap<'ctx>>,
}

impl<'ctx> RegisterLookup<'ctx> {
    pub fn new() -> RegisterLookup<'ctx> {
        RegisterLookup { stack: Vec::new() }
    }

    /// Opens a new function level scope. This will return
    /// an `Err` if a function scope is already open.
    ///
    /// # Examples
    /// ``` ignore
    /// let mut rl = RegisterLookup::new();
    /// let r = rl.open_fn();
    /// assert_eq!(r, Ok(()));
    /// ```
    pub fn open_fn(&mut self) -> Result<()> {
        if self.stack.is_empty() {
            self.stack.push(IdToValueMap::new());
            Ok(())
        } else {
            Err("Cannot open fn scope: there is already one open".into())
        }
    }

    /// Closes the current function level scope.  This will return
    /// an `Err` if there are still open local scopes.
    ///
    /// # Examples
    /// ``` ignore
    /// let mut rl = RegisterLookup::new();
    /// rl.open_fn().unwrap();
    /// let r = rl.close_fn()
    /// assert_eq!(r, Ok(()));
    /// ```
    pub fn close_fn(&mut self) -> Result<()> {
        if self.stack.len() == 1 {
            self.stack.pop();
            Ok(())
        } else if self.stack.len() > 1 {
            Err("Cannot close fn scope: there is an open local scope".into())
        } else {
            Err("Cannot close fn scope: there is no open fn scope".into())
        }
    }

    /// Opens a new local scope. This will return an `Err` if there
    /// is no open function scope.
    pub fn open_local(&mut self) -> Result<()> {
        if self.stack.is_empty() {
            Err("Cannot open local scope: there is no open fn scope".into())
        } else {
            self.stack.push(IdToValueMap::new());
            Ok(())
        }
    }

    /// Closes the current local scope. If there is no open local scope
    /// this will return an `Err`
    pub fn close_local(&mut self) -> Result<()> {
        if self.stack.len() > 1 {
            self.stack.pop();
            Ok(())
        } else if self.stack.len() == 1 {
            Err("Cannot close local scope: there is no open local scope".into())
        } else {
            Err("Cannot close local scope: there is no open fn scope".into())
        }
    }

    /// Look up the given identifier and return the LLVM Value that it
    /// maps to.  This will search through all scopes in the current stack
    /// starting from the top and moving down.  Returning the first match
    /// that is found, and returning `None` if no match is found.
    pub fn get(&self, id: &str) -> Option<&AnyValueEnum<'ctx>> {
        self.stack.iter().rev().find_map(|t| t.map.get(id))
    }

    /// Inserts a new mapping from `id` to `value` into the current scope.
    /// If `id` is already in the current scope it will be overwritten.
    /// This will return an `Err` if there is no open scope.
    pub fn insert(&mut self, id: &str, value: AnyValueEnum<'ctx>) -> Result<()> {
        if self.stack.is_empty() {
            return Err("Cannot insert identifier: there is no open scope".into());
        }

        self.stack
            .last_mut()
            .and_then(|t| t.map.insert(id.into(), value));

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::assert_ne;

    use inkwell::context::Context;

    use super::*;

    #[test]
    fn fn_scope() {
        let context = Context::create();

        let mut rl = RegisterLookup::new();
        rl.open_fn().unwrap();

        let val = context.i8_type().const_int(1, true);
        rl.insert("test", val.into()).unwrap();
        assert_eq!(val, *rl.get("test").unwrap());

        // test that if the same identifier is inserted in the same scope
        // the previous value is overwritten
        let val2 = context.i8_type().const_int(2, true);
        rl.insert("test", val2.into()).unwrap();
        assert_eq!(val2, *rl.get("test").unwrap());
        assert_ne!(val, *rl.get("test").unwrap());

        rl.close_fn().unwrap();
    }

    #[test]
    fn fn_scope_fail() {
        let mut rl = RegisterLookup::new();
        let err = rl.close_fn().unwrap_err();
        assert_eq!("Cannot close fn scope: there is no open fn scope", err);

        rl.open_fn().unwrap();

        let err = rl.open_fn().unwrap_err();
        assert_eq!("Cannot open fn scope: there is already one open", err);
    }

    #[test]
    fn local_scope() {
        let context = Context::create();

        let mut rl = RegisterLookup::new();
        rl.open_fn().unwrap();

        // Test that nested scopes do not overwrite parent scopes
        let val = context.i8_type().const_int(0, true);
        rl.insert("test", val.into()).unwrap();

        {
            rl.open_local().unwrap();
            let val = context.i8_type().const_int(1, true);
            rl.insert("test", val.into()).unwrap();
            assert_eq!(val, *rl.get("test").unwrap());

            // test that if the same identifier is inserted in the same scope
            // the previous value is overwritten
            let val2 = context.i8_type().const_int(2, true);
            rl.insert("test", val2.into()).unwrap();
            assert_eq!(val2, *rl.get("test").unwrap());
            assert_ne!(val, *rl.get("test").unwrap());
            rl.close_local().unwrap();
        }

        // test nested local scopes
        {
            rl.open_local().unwrap();
            let val = context.i8_type().const_int(1, true);
            rl.insert("test", val.into()).unwrap();

            {
                rl.open_local().unwrap();
                let val = context.i8_type().const_int(2, true);
                rl.insert("test", val.into()).unwrap();
                rl.close_local().unwrap();
            }

            assert_eq!(val, *rl.get("test").unwrap());

            rl.close_local().unwrap();
        }

        assert_eq!(val, *rl.get("test").unwrap());

        rl.close_fn().unwrap();
    }

    #[test]
    fn local_scope_fail() {
        let mut rl = RegisterLookup::new();

        let err = rl.open_local().unwrap_err();
        assert_eq!("Cannot open local scope: there is no open fn scope", err);

        let err = rl.close_local().unwrap_err();
        assert_eq!("Cannot close local scope: there is no open fn scope", err);

        rl.open_fn().unwrap();

        let err = rl.close_local().unwrap_err();
        assert_eq!(
            "Cannot close local scope: there is no open local scope",
            err
        );

        rl.open_local().unwrap();
        let err = rl.close_fn().unwrap_err();
        assert_eq!("Cannot close fn scope: there is an open local scope", err);
    }
}
