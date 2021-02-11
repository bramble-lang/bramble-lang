/**
 * RegSize stores the bit size of a register.  This is used by the compiler to
 * assign a minimum register size needed to store a value.  It is meant to be
 * independent of specific CPU architectures (e.g. x86 or ARM).
 */
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegSize {
    R8,
    R16,
    R32,
    R64,
}

impl RegSize {
    pub fn assign(nbytes: usize) -> Option<RegSize> {
        if nbytes == 0 {
            None
        } else if nbytes <= 1 {
            Some(RegSize::R8)
        } else if nbytes <= 2 {
            Some(RegSize::R16)
        } else if nbytes <= 4 {
            Some(RegSize::R32)
        } else if nbytes <= 8 {
            Some(RegSize::R64)
        } else {
            None
        }
    }
}

impl std::fmt::Display for RegSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            RegSize::R8 => f.write_str("R8"),
            RegSize::R16 => f.write_str("R16"),
            RegSize::R32 => f.write_str("R32"),
            RegSize::R64 => f.write_str("R64"),
        }
    }
}
