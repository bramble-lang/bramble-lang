pub trait Annotation {
    fn id(&self) -> u32;
    fn line(&self) -> u32;
}

pub mod traversalmut;
pub mod map;
pub mod iter;