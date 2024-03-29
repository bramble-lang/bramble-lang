use crate::compiler::diagnostics::Writable;
use crate::compiler::{CompilerDisplay, CompilerDisplayError, SourceMap};
use crate::{StringId, StringTable};

use super::PathCanonizationError;

pub const CANONICAL_ROOT: &str = "project";
pub const ROOT_PATH: &str = "root";
pub const SELF: &str = "self";
pub const SUPER: &str = "super";

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Element {
    FileRoot,
    CanonicalRoot,
    Selph,
    Super,
    Id(StringId),
}

impl CompilerDisplay for Element {
    fn fmt(&self, _: &SourceMap, st: &StringTable) -> Result<String, CompilerDisplayError> {
        Ok(match self {
            Element::FileRoot => ROOT_PATH.into(),
            Element::CanonicalRoot => CANONICAL_ROOT.into(),
            Element::Selph => SELF.into(),
            Element::Super => SUPER.into(),
            Element::Id(id) => st.get(*id)?,
        })
    }
}

impl std::fmt::Display for Element {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Element::FileRoot => f.write_str(ROOT_PATH),
            Element::CanonicalRoot => f.write_str(CANONICAL_ROOT),
            Element::Selph => f.write_str(SELF),
            Element::Super => f.write_str(SUPER),
            Element::Id(id) => f.write_fmt(format_args!("{}", id)),
        }
    }
}

/**
Provides the directions along the hierarchy of modules in a project to get to
a specific item (e.g. structure definition or function definition). [`Path`]s
allow the user to specify an item that exists in a different module than the
current one (i.e. non-local).

A [`Path`] may be relative or absolute.  A relative path starts at the current
module and uses modifies to either move up the hierarchy or into specific, named,
modules.  A canonical path always starts at the root of hte project and moves
down through the specifically named modules.

Keywords:
- `project`: if a path starts with this keyword, it is canonical
- `root`: if a path starts with this keyword, it begins at top level of the
current file.
- `self`: if a path begins with this keyword, it starts at the current module.
*/
#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    path: Vec<Element>,
    is_canonical: bool,
}

impl Default for Path {
    fn default() -> Self {
        Self {
            path: Default::default(),
            is_canonical: Default::default(),
        }
    }
}

impl Path {
    pub fn new() -> Path {
        Self::default()
    }

    pub fn is_canonical(&self) -> bool {
        self.is_canonical
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    pub fn first(&self) -> Option<&Element> {
        self.path.first()
    }

    pub fn last(&self) -> Option<&Element> {
        self.path.last()
    }

    pub fn push(&mut self, step: Element) {
        // Check if this is the creation of a canonical path
        if self.path.is_empty() && step == Element::CanonicalRoot {
            self.is_canonical = true;
        } else {
            self.path.push(step)
        }
    }

    pub fn pop(&mut self) -> Option<Element> {
        self.path.pop()
    }

    pub fn append(&mut self, p: &Path) {
        for s in p.iter() {
            self.path.push(*s);
        }
    }

    pub fn iter(&self) -> std::slice::Iter<Element> {
        self.path.iter()
    }

    pub fn item(&self) -> Option<StringId> {
        let l = self.path.len();
        if l == 0 {
            None
        } else if let Element::Id(id) = self.path[l - 1] {
            Some(id)
        } else {
            None
        }
    }

    pub fn parent(&self) -> Path {
        let mut path = Path {
            path: self.path.clone(),
            is_canonical: self.is_canonical,
        };
        path.path.pop();
        path
    }

    /**
    Converts this path into a canonical path by merging it
    with the given current path.

    - If this is already a canonical path (i.e. begins with `root`) then nothing will change.
    - If this path begins with `self` then `self` will be replaced with `current_path`
    - occurances of `super` will move up the current path
    */
    pub fn to_canonical(&self, current_path: &Path) -> Result<Path, PathCanonizationError> {
        // TODO: make this method move "self"?
        if !current_path.is_canonical() {
            panic!("Current path is not canonical: {}", current_path);
        }
        if self.is_canonical() {
            Ok(self.clone())
        } else {
            let mut uses_root = false;
            let path = if self.path[0] == Element::Selph {
                &self.path[1..]
            } else if self.path[0] == Element::FileRoot {
                uses_root = true;
                &self.path[1..]
            } else {
                &self.path
            };
            let current_path = if uses_root {
                &current_path.path[..1]
            } else {
                &current_path.path
            };
            let mut merged: Vec<Element> = current_path.into();
            for step in path.iter() {
                if *step == Element::Super {
                    merged.pop().ok_or(PathCanonizationError::SubceedingRoot)?;
                    if merged.is_empty() {
                        return Err(PathCanonizationError::SubceedingRoot);
                    }
                } else {
                    merged.push(*step);
                }
            }
            Ok(Path {
                path: merged,
                is_canonical: true,
            })
        }
    }

    pub fn to_label(&self, sm: &SourceMap, table: &StringTable) -> String {
        self.path
            .iter()
            .map(|element| element.fmt(sm, table).unwrap())
            .collect::<Vec<_>>()
            .join("_")
    }
}

impl<I: std::slice::SliceIndex<[Element]>> std::ops::Index<I> for Path {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&*self.path, index)
    }
}

impl CompilerDisplay for Path {
    fn fmt(&self, sm: &SourceMap, st: &StringTable) -> Result<String, CompilerDisplayError> {
        let mut ps: Vec<String> = vec![];

        for e in self.iter() {
            let es = e.fmt(sm, st)?;
            ps.push(es);
        }

        let ps = ps.join("::");

        if self.is_canonical() {
            Ok(format!("${}", ps))
        } else {
            Ok(ps)
        }
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_canonical() {
            f.write_str("$")?
        }
        let sv: Vec<String> = self.path.iter().map(|id| format!("{}", id)).collect();
        f.write_str(&sv.join("::"))
    }
}

impl From<Vec<Element>> for Path {
    fn from(v: Vec<Element>) -> Self {
        let is_canonical = v.first().map_or(false, |f| *f == Element::CanonicalRoot);
        let v = if is_canonical { &v[1..] } else { &v };
        Path {
            path: v.into(),
            is_canonical,
        }
    }
}

impl Writable for &Path {
    fn write(&self, w: &dyn crate::compiler::diagnostics::Writer) {
        w.write_text("\"");
        w.write_path(self);
        w.write_text("\"");
    }
}

#[cfg(test)]
mod test_path {
    use super::*;

    #[test]
    fn test_canonical_to_canonical() {
        let table = StringTable::new();
        let first_id = table.insert("first".into());
        let current_id = table.insert("current".into());
        let path: Path = vec![Element::FileRoot, Element::Id(first_id)].into();

        let current = vec![Element::CanonicalRoot, Element::Id(current_id)].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(current_id),
            Element::Id(first_id),
        ]
        .into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_to_canonical() {
        let table = StringTable::new();
        let relative_id = table.insert("relative".into());
        let current_id = table.insert("current".into());

        let path: Path = vec![Element::Id(relative_id)].into();
        let current = vec![Element::CanonicalRoot, Element::Id(current_id)].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![
            Element::CanonicalRoot,
            Element::Id(current_id),
            Element::Id(relative_id),
        ]
        .into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_self_to_canonical() {
        let table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));

        let path: Path = vec![Element::Selph, relative_id].into();
        let current = vec![Element::CanonicalRoot, current_id].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![Element::CanonicalRoot, current_id, relative_id].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_super_to_canonical() {
        let table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));
        let test_id = Element::Id(table.insert("test".into()));

        let path: Path = vec![Element::Super, relative_id].into();
        let current = vec![Element::CanonicalRoot, test_id, current_id].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![Element::CanonicalRoot, test_id, relative_id].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_post_super_to_canonical() {
        let table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));

        let path: Path = vec![relative_id, Element::Super].into();
        let current = vec![Element::CanonicalRoot, current_id].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![Element::CanonicalRoot, current_id].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_too_many_supers() {
        let table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));

        let path: Path = vec![Element::Super, Element::Super, relative_id].into();
        let current = vec![Element::CanonicalRoot, current_id].into();
        let canonized_path = path.to_canonical(&current);
        assert_eq!(canonized_path, Err(PathCanonizationError::SubceedingRoot));
    }

    #[test]
    fn test_relative_with_scattered_super_to_canonical() {
        let table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));
        let test_id = Element::Id(table.insert("test".into()));

        let path: Path = vec![Element::Super, relative_id, Element::Super].into();
        let current = vec![Element::CanonicalRoot, test_id, current_id].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec![Element::CanonicalRoot, test_id].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_push_step() {
        let table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));
        let test_id = Element::Id(table.insert("test".into()));

        let mut path: Path = vec![Element::Selph, item_id].into();
        path.push(test_id);

        let expected = vec![Element::Selph, item_id, test_id].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_to_label() {
        let sm = SourceMap::new();
        let table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));

        let path: Path = vec![Element::Selph, item_id].into();

        let expected = "self_item";
        assert_eq!(path.to_label(&sm, &table), expected);
    }

    #[test]
    fn test_item() {
        let table = StringTable::new();
        let item_id = table.insert("item".into());

        let path: Path = vec![Element::Selph, Element::Id(item_id)].into();

        assert_eq!(path.item(), Some(item_id));
    }

    #[test]
    fn test_item_empty_path() {
        let path: Path = Path::new();

        assert_eq!(path.item(), None);
    }

    #[test]
    fn test_parent() {
        let table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));

        let path: Path = vec![Element::Selph, item_id].into();

        let expected = vec![Element::Selph].into();
        assert_eq!(path.parent(), expected);
    }
}
