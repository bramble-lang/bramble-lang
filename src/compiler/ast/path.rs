use crate::compiler::{CompilerDisplay, CompilerDisplayError};
use crate::{StringId, StringTable};

use super::{ParsePathError, PathCanonizationError};

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
    fn fmt(&self, st: &StringTable) -> Result<String, CompilerDisplayError> {
        Ok(match self {
            Element::FileRoot => ROOT_PATH.into(),
            Element::CanonicalRoot => CANONICAL_ROOT.into(),
            Element::Selph => SELF.into(),
            Element::Super => SUPER.into(),
            Element::Id(id) => st.get(*id)?.into(),
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

#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    path: Vec<Element>,
    is_canonical: bool,
}

impl Path {
    pub fn new() -> Path {
        Path {
            path: vec![],
            is_canonical: false,
        }
    }

    pub fn is_canonical(&self) -> bool {
        self.is_canonical
    }

    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn first(&self) -> Option<&Element> {
        self.path.first()
    }

    pub fn last(&self) -> Option<&Element> {
        self.path.last()
    }

    pub fn push(&mut self, step: Element) {
        // Check if this is the creation of a canonical path
        if self.path.len() == 0 && step == Element::CanonicalRoot {
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
        } else {
            if let Element::Id(id) = self.path[l - 1] {
                Some(id)
            } else {
                None
            }
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
                    if merged.len() == 0 {
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

    pub fn to_label(&self, table: &StringTable) -> String {
        self.path
            .iter()
            .map(|element| element.fmt(table).unwrap())
            .collect::<Vec<_>>()
            .join("_")
    }

    /// Attempts to parse a string into a [`Path`].  This expects the same text format
    /// as the [`CompilerDisplay`] implementation outputs.
    pub fn parse(table: &mut StringTable, p: &str) -> Result<Path, ParsePathError> {
        /// Tests that an element is a valid identifier
        fn is_element_valid(el: &str) -> bool {
            let cs = el.chars().collect::<Vec<_>>();
            if cs.len() == 0 {
                // Element must have at least one character
                false
            } else {
                if !(cs[0].is_alphabetic() || cs[0] == '_') {
                    // Element can only start with a letter or underscore
                    false
                } else {
                    // Element can only contain alphanumerics and _
                    !cs.iter().any(|c| !(c.is_alphanumeric() || *c == '_'))
                }
            }
        }

        // Check if this is a canonical path, and remove the $ if it is
        let (p, is_canonical) = match p.strip_prefix("$") {
            Some(stripped) => (stripped, true),
            None => (p, false),
        };
        let elements = p.split("::");

        let mut path = vec![];
        if is_canonical {
            path.push(Element::CanonicalRoot)
        }

        for el in elements {
            if !is_element_valid(el) {
                return Err(ParsePathError);
            }
            match el {
                "self" => path.push(Element::Selph),
                "super" => path.push(Element::Super),
                "root" => path.push(Element::FileRoot),
                e => path.push(Element::Id(table.insert(e.into()))),
            }
        }

        Ok(path.into())
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
    fn fmt(&self, st: &StringTable) -> Result<String, CompilerDisplayError> {
        let mut ps: Vec<String> = vec![];

        for e in self.iter() {
            let es = e.fmt(st)?;
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

#[cfg(test)]
mod test_path {
    use super::*;

    #[test]
    fn test_canonical_to_canonical() {
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
        let relative_id = Element::Id(table.insert("relative".into()));
        let current_id = Element::Id(table.insert("current".into()));

        let path: Path = vec![Element::Super, Element::Super, relative_id].into();
        let current = vec![Element::CanonicalRoot, current_id].into();
        let canonized_path = path.to_canonical(&current);
        assert_eq!(canonized_path, Err(PathCanonizationError::SubceedingRoot));
    }

    #[test]
    fn test_relative_with_scattered_super_to_canonical() {
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));
        let test_id = Element::Id(table.insert("test".into()));

        let mut path: Path = vec![Element::Selph, item_id].into();
        path.push(test_id);

        let expected = vec![Element::Selph, item_id, test_id].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_to_label() {
        let mut table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));

        let path: Path = vec![Element::Selph, item_id].into();

        let expected = "self_item";
        assert_eq!(path.to_label(&table), expected);
    }

    #[test]
    fn test_item() {
        let mut table = StringTable::new();
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
        let mut table = StringTable::new();
        let item_id = Element::Id(table.insert("item".into()));

        let path: Path = vec![Element::Selph, item_id].into();

        let expected = vec![Element::Selph].into();
        assert_eq!(path.parent(), expected);
    }
}
