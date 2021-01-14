#[derive(Clone, Debug, PartialEq)]
pub struct Path {
    path: Vec<String>,
}

impl Path {
    pub fn new() -> Path {
        Path { path: vec![] }
    }
    pub fn len(&self) -> usize {
        self.path.len()
    }

    pub fn last(&self) -> Option<&String> {
        self.path.last()
    }

    pub fn push(&mut self, step: &str) {
        self.path.push(step.into())
    }

    pub fn iter(&self) -> std::slice::Iter<String> {
        self.path.iter()
    }

    pub fn item(&self) -> Option<&str> {
        let l = self.path.len();
        if l == 0 {
            None
        } else {
            Some(&self.path[l - 1])
        }
    }

    pub fn tail(&self) -> Path {
        let mut path = Path {
            path: self.path.clone(),
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
    pub fn to_canonical(&self, current_path: &Path) -> Result<Path, String> {
        if self.path[0] == "root" {
            Ok(self.clone())
        } else {
            let path = if self.path[0] == "self" {
                &self.path[1..]
            } else {
                &self.path
            };
            let mut merged = current_path.path.clone();
            for step in path.iter() {
                if step == "super" {
                    merged
                        .pop()
                        .ok_or("Use of super in path exceeded the depth of the current path")?;
                    if merged.len() == 0 {
                        return Err(
                            "Use of super in path exceeded the depth of the current path".into(),
                        );
                    }
                } else {
                    merged.push(step.clone());
                }
            }
            Ok(Path { path: merged })
        }
    }

    pub fn to_label(&self) -> String {
        self.path.join("_")
    }
}

impl<I: std::slice::SliceIndex<[String]>> std::ops::Index<I> for Path {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&*self.path, index)
    }
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.path.join("::"))
    }
}

impl From<Vec<String>> for Path {
    fn from(v: Vec<String>) -> Self {
        Path { path: v.clone() }
    }
}

impl From<Vec<&str>> for Path {
    fn from(v: Vec<&str>) -> Self {
        Path {
            path: v.into_iter().map(|e| e.into()).collect(),
        }
    }
}

impl std::hash::Hash for Path {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for s in self.path.iter() {
            s.hash(state);
        }
    }
}

#[cfg(test)]
mod test_path {
    use super::*;

    #[test]
    fn test_canonical_to_canonical() {
        let path: Path = vec!["root", "first"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "first"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_to_canonical() {
        let path: Path = vec!["relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_self_to_canonical() {
        let path: Path = vec!["self", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_super_to_canonical() {
        let path: Path = vec!["super", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "relative"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_relative_with_post_super_to_canonical() {
        let path: Path = vec!["relative", "super"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root", "current"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_too_many_supers() {
        let path: Path = vec!["super", "super", "relative"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = "Use of super in path exceeded the depth of the current path".into();
        assert_eq!(canonized_path, Err(expected));
    }

    #[test]
    fn test_relative_with_scattered_super_to_canonical() {
        let path: Path = vec!["super", "relative", "super"].into();
        let current = vec!["root", "current"].into();
        let canonized_path = path.to_canonical(&current);
        let expected = vec!["root"].into();
        assert_eq!(canonized_path, Ok(expected));
    }

    #[test]
    fn test_push_step() {
        let mut path: Path = vec!["self", "item"].into();
        path.push("test");

        let expected = vec!["self", "item", "test"].into();
        assert_eq!(path, expected);
    }

    #[test]
    fn test_to_label() {
        let path: Path = vec!["self", "item"].into();

        let expected = "self_item";
        assert_eq!(path.to_label(), expected);
    }

    #[test]
    fn test_item() {
        let path: Path = vec!["self", "item"].into();

        let expected = "item";
        assert_eq!(path.item(), Some(expected));
    }

    #[test]
    fn test_item_empty_path() {
        let path: Path = Path::new();

        assert_eq!(path.item(), None);
    }

    #[test]
    fn test_tail() {
        let path: Path = vec!["self", "item"].into();

        let expected = vec!["self"].into();
        assert_eq!(path.tail(), expected);
    }
}