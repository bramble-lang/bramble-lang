use crate::compiler::arch::registers::RegSize;
use std::fmt::*;

/*
Assembly DSL
Registers are prefixed with %
Memory locations are within []
Each instruction is followed by a ;
Expressions to evaluate are in {}
labels are keywords which are not already named for registers
prefix labels with @
Think about @ for global labels and ^ for local labels
```
let x = 8;
assembly!(
    (buffer) {
        mov %eax, 4;
        mov %ebx, {x};
        mov [%ebx - {x}], {x * 2 + 4};
        jmp @label
    @label:
        mov %eax, %ebx
    ^local:
        jmp ^local
    }
)
```

would translate to:
```
    mov eax, 4
    mov ebx, 8
    mov [ebx - 8], 20
    jmp label

label:
    mov eax, ebx
.local:
    jmp .local
```

operand combinations:
binary:
%_, %_
%_, _
%_, @_
%_, [_]
[_], %_
[_], _
[_], @_
 but not [_], [_]  as there are no x86 instructions that take two explicit memory operands (the mov string ops implicitly use two mem locations)

unary:
%_
@_
[_]
_

*/

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg8 {
    Al,
    Bl,
    Cl,
    Dl,
    Dil
}

impl Display for Reg8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg8::*;
        match self {
            Al => f.write_str("al"),
            Bl => f.write_str("bl"),
            Cl => f.write_str("cl"),
            Dl => f.write_str("dl"),
            Dil => f.write_str("dil"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg16 {
    Ax,
    Bx,
    Cx,
    Dx,
    Di,
}

impl Display for Reg16 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg16::*;
        match self {
            Ax => f.write_str("ax"),
            Bx => f.write_str("bx"),
            Cx => f.write_str("cx"),
            Dx => f.write_str("dx"),
            Di => f.write_str("di"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg32 {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
    Edi,
    Esi,
}

impl Display for Reg32 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg32::*;
        match self {
            Eax => f.write_str("eax"),
            Ecx => f.write_str("ecx"),
            Edx => f.write_str("edx"),
            Ebx => f.write_str("ebx"),
            Esp => f.write_str("esp"),
            Ebp => f.write_str("ebp"),
            Edi => f.write_str("edi"),
            Esi => f.write_str("esi"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg64 {
    Rax,
    Rcx,
    Rdx,
    Rbx,
    Rsp,
    Rbp,
    Rdi,
    Rsi,
}

impl Display for Reg64 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg64::*;
        match self {
            Rax => f.write_str("rax"),
            Rcx => f.write_str("rcx"),
            Rdx => f.write_str("rdx"),
            Rbx => f.write_str("rbx"),
            Rsp => f.write_str("rsp"),
            Rbp => f.write_str("rbp"),
            Rdi => f.write_str("rdi"),
            Rsi => f.write_str("rsi"),
        }
    }
}

impl Reg64 {
    /**
     * scale will take a given 64 bit register and a target size
     * and returns the smallest subregister that can hold the given
     * register size.
     *
     * For example, given `Rax` and a target register size of
     * `R16` (16 bits), scale will return `Ax`.
     */
    pub fn scale(&self, sz: RegSize) -> Option<Reg> {
        match self {
            Reg64::Rax => Some(match sz {
                RegSize::R8 => Reg::R8(Reg8::Al),
                RegSize::R16 => Reg::R16(Reg16::Ax),
                RegSize::R32 => Reg::R32(Reg32::Eax),
                RegSize::R64 => Reg::R64(Reg64::Rax),
            }),
            Reg64::Rbx => Some(match sz {
                RegSize::R8 => Reg::R8(Reg8::Bl),
                RegSize::R16 => Reg::R16(Reg16::Bx),
                RegSize::R32 => Reg::R32(Reg32::Ebx),
                RegSize::R64 => Reg::R64(Reg64::Rbx),
            }),
            Reg64::Rcx => Some(match sz {
                RegSize::R8 => Reg::R8(Reg8::Cl),
                RegSize::R16 => Reg::R16(Reg16::Cx),
                RegSize::R32 => Reg::R32(Reg32::Ecx),
                RegSize::R64 => Reg::R64(Reg64::Rcx),
            }),
            Reg64::Rdx => Some(match sz {
                RegSize::R8 => Reg::R8(Reg8::Dl),
                RegSize::R16 => Reg::R16(Reg16::Dx),
                RegSize::R32 => Reg::R32(Reg32::Edx),
                RegSize::R64 => Reg::R64(Reg64::Rdx),
            }),
            Reg64::Rdi => Some(match sz {
                RegSize::R8 => Reg::R8(Reg8::Dil),
                RegSize::R16 => Reg::R16(Reg16::Di),
                RegSize::R32 => Reg::R32(Reg32::Edi),
                RegSize::R64 => Reg::R64(Reg64::Rdi),
            }),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Reg {
    R8(Reg8),
    R16(Reg16),
    R32(Reg32),
    R64(Reg64),
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg::*;
        match self {
            R8(r8) => f.write_fmt(format_args!("{}", r8)),
            R16(r16) => f.write_fmt(format_args!("{}", r16)),
            R32(r32) => f.write_fmt(format_args!("{}", r32)),
            R64(r64) => f.write_fmt(format_args!("{}", r64)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectOperand {
    Integer64(i64),
    UInteger64(u64),
    Integer32(i32),
    Register(Reg),
    Label(String),
}

impl DirectOperand {
    pub fn from_string(s: &str) -> DirectOperand {
        use DirectOperand::*;
        match s {
            "eax" => Register(Reg::R32(Reg32::Eax)),
            _ => panic!("Invalid register: {}", s),
        }
    }
}

impl From<u64> for DirectOperand {
    fn from(u: u64) -> Self {
        DirectOperand::UInteger64(u)
    }
}

impl From<i64> for DirectOperand {
    fn from(i: i64) -> Self {
        DirectOperand::Integer64(i)
    }
}

impl From<i32> for DirectOperand {
    fn from(i: i32) -> Self {
        DirectOperand::Integer32(i)
    }
}

impl From<&str> for DirectOperand {
    fn from(l: &str) -> Self {
        DirectOperand::Label(l.into())
    }
}

impl From<String> for DirectOperand {
    fn from(l: String) -> Self {
        DirectOperand::Label(l)
    }
}

impl From<Reg> for DirectOperand {
    fn from(r: Reg) -> Self {
        DirectOperand::Register(r)
    }
}

impl Display for DirectOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use DirectOperand::*;
        match self {
            Integer64(i) => f.write_fmt(format_args!("{}", i)),
            UInteger64(i) => f.write_fmt(format_args!("{}", i)),
            Integer32(i) => f.write_fmt(format_args!("{}", i)),
            Register(reg) => f.write_fmt(format_args!("{}", reg)),
            Label(lbl) => f.write_fmt(format_args!("{}", lbl)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Direct(DirectOperand),
    Memory(DirectOperand),
    IPRelativeMemory(DirectOperand),
    MemoryAddr(Reg, i32),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Operand::*;
        match self {
            Direct(d) => f.write_fmt(format_args!("{}", d)),
            Memory(mem) => f.write_fmt(format_args!("[rel {}]", mem)),
            IPRelativeMemory(mem) => f.write_fmt(format_args!("QWORD [rel {}]", mem)),
            MemoryAddr(mem, d) => {
                if *d < 0 {
                    f.write_fmt(format_args!("[rel {}-{}]", mem, -d))
                } else if *d > 0 {
                    f.write_fmt(format_args!("[rel {}+{}]", mem, d))
                } else {
                    f.write_fmt(format_args!("[rel {}]", mem))
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Comment(String),
    Include(String),
    Extern(String),
    Section(String),
    Global(String),
    Data(String, i32),
    Data64(String, i64),
    DataString(String, String),
    Label(String),

    Jmp(Operand),
    Jz(Operand),
    Call(Operand),
    Ret,
    Cdq,

    Push(Operand),
    Pop(Operand),
    Mov(Operand, Operand),
    Movzx(Operand, Operand),
    Lea(Operand, Operand),

    Add(Operand, Operand),
    Sub(Operand, Operand),
    IMul(Operand, Operand),
    IDiv(Reg),
    Neg(Reg),

    Cmp(Operand, Operand),

    And(Operand, Operand),
    Or(Operand, Operand),
    Xor(Operand, Operand),

    Sete(Reg8),
    Setne(Reg8),
    Setl(Reg8),
    Setle(Reg8),
    Setg(Reg8),
    Setge(Reg8),

    PrintStr(Operand),
    PrintString(String),
    PrintDec(Operand),
    NewLine,
}

impl Display for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Inst::*;
        // Add separating newline?
        match self {
            Label(lbl) if !lbl.starts_with(".") => f.write_str("\n")?,
            _ => f.write_str("")?,
        };

        // Indent instruction?
        match self {
            Label(_) | Include(_) | Global(_) | Section(_) => (),
            _ => f.write_str("    ")?,
        };

        match self {
            Comment(comment) => f.write_fmt(format_args!("; {}", comment)),
            Include(inc) => f.write_fmt(format_args!("%include \"{}\"", inc)),
            Extern(ext) => f.write_fmt(format_args!("extern {}", ext)),
            Section(section) => f.write_fmt(format_args!("\nsection {}", section)),
            Global(global) => f.write_fmt(format_args!("global {}", global)),
            Data(lbl, value) => f.write_fmt(format_args!("{}: dd {}", lbl, value)),
            Data64(lbl, value) => f.write_fmt(format_args!("{}: dq {}", lbl, value)),
            DataString(lbl, value) => f.write_fmt(format_args!("{}: db `{}`,0", lbl, value)),

            Jmp(a) => f.write_fmt(format_args!("jmp {}", a)),
            Jz(a) => f.write_fmt(format_args!("jz {}", a)),
            Call(a) => f.write_fmt(format_args!("call {}", a)),
            Ret => f.write_fmt(format_args!("ret")),

            Push(a) => f.write_fmt(format_args!("push {}", a)),
            Pop(a) => f.write_fmt(format_args!("pop {}", a)),

            Mov(a, b) => f.write_str(&format_binary_op("mov", a, b)),
            Movzx(a, b) => f.write_str(&format_binary_op("movzx", a, b)),

            Cdq => f.write_str("cdq"),
            Lea(a, b) => f.write_str(&format_binary_op("lea", a, b)),
            Add(a, b) => f.write_str(&format_binary_op("add", a, b)),
            Sub(a, b) => f.write_str(&format_binary_op("sub", a, b)),
            IMul(a, b) => f.write_str(&format_binary_op("imul", a, b)),
            IDiv(a) => f.write_fmt(format_args!("idiv {}", a)),
            Neg(a) => f.write_fmt(format_args!("neg {}", a)),
            Cmp(a, b) => f.write_str(&format_binary_op("cmp", a, b)),
            And(a, b) => f.write_str(&format_binary_op("and", a, b)),
            Or(a, b) => f.write_str(&format_binary_op("or", a, b)),
            Xor(a, b) => f.write_str(&format_binary_op("xor", a, b)),

            Sete(a) => f.write_fmt(format_args!("sete {}", a)),
            Setne(a) => f.write_fmt(format_args!("setne {}", a)),
            Setl(a) => f.write_fmt(format_args!("setl {}", a)),
            Setle(a) => f.write_fmt(format_args!("setle {}", a)),
            Setg(a) => f.write_fmt(format_args!("setg {}", a)),
            Setge(a) => f.write_fmt(format_args!("setge {}", a)),

            Label(lbl) => f.write_fmt(format_args!("{}:", lbl)),

            PrintStr(val) => f.write_fmt(format_args!("PRINT_STRING {}", val)),
            PrintString(s) => f.write_fmt(format_args!("PRINT_STRING \"{}\"", s)),
            PrintDec(val) => f.write_fmt(format_args!("PRINT_DEC 4, {}", val)),
            NewLine => f.write_fmt(format_args!("NEWLINE")),
        }
    }
}

fn format_binary_op(op: &str, a: &Operand, b: &Operand) -> String {
    let operands = match (a, b) {
        (Operand::Direct(a), b) => format!("{}, {}", a, b),
        (a, Operand::Direct(DirectOperand::Integer32(i))) => format!("DWORD {}, {}", a, i),
        (a, Operand::Direct(DirectOperand::Integer64(i))) => format!("QWORD {}, {}", a, i),
        (a, Operand::Direct(DirectOperand::UInteger64(i))) => format!("QWORD {}, {}", a, i),
        (a, b) => format!("{}, {}", a, b),
    };
    format!("{} {}", op, operands)
}

#[macro_export]
macro_rules! unit_op {
    (ret) => {
        Inst::Ret
    };
    (cdq) => {
        Inst::Cdq
    };
    (newline) => {
        Inst::NewLine
    };
}

#[macro_export]
macro_rules! unary_op {
    (jmp) => {
        Inst::Jmp
    };
    (jz) => {
        Inst::Jz
    };
    (call) => {
        Inst::Call
    };
    (push) => {
        Inst::Push
    };
    (pop) => {
        Inst::Pop
    };
    (print_dec) => {
        Inst::PrintDec
    };
    (print_str) => {
        Inst::PrintStr
    };
}

#[macro_export]
macro_rules! binary_op {
    (mov) => {
        Inst::Mov
    };
    (movzx) => {
        Inst::Movzx
    };
    (lea) => {
        Inst::Lea
    };
    (add) => {
        Inst::Add
    };
    (sub) => {
        Inst::Sub
    };
    (imul) => {
        Inst::IMul
    };
    (cmp) => {
        Inst::Cmp
    };
    (and) => {
        Inst::And
    };
    (or) => {
        Inst::Or
    };
    (xor) => {
        Inst::Xor
    };
}

#[macro_export]
macro_rules! reg8 {
    (al) => {
        Reg8::Al
    };
}

#[macro_export]
macro_rules! reg32 {
    (eax) => {
        Reg32::Eax
    };
    (ebx) => {
        Reg32::Ebx
    };
    (edi) => {
        Reg32::Edi
    };
    (esi) => {
        Reg32::Esi
    };
}

#[macro_export]
macro_rules! register {
    ({$reg:expr}) => {
        $reg
    };
    (rax) => {
        Reg::R64(Reg64::Rax)
    };
    (rcx) => {
        Reg::R64(Reg64::Rcx)
    };
    (rdx) => {
        Reg::R64(Reg64::Rdx)
    };
    (rbx) => {
        Reg::R64(Reg64::Rbx)
    };
    (rsp) => {
        Reg::R64(Reg64::Rsp)
    };
    (rbp) => {
        Reg::R64(Reg64::Rbp)
    };
    (rdi) => {
        Reg::R64(Reg64::Rdi)
    };
    (rsi) => {
        Reg::R64(Reg64::Rsi)
    };

    (eax) => {
        Reg::R32(Reg32::Eax)
    };
    (ecx) => {
        Reg::R64(Reg32::Ecx)
    };
    (edx) => {
        Reg::R32(Reg32::Edx)
    };
    (ebx) => {
        Reg::R32(Reg32::Ebx)
    };
    (esp) => {
        Reg::R32(Reg32::Esp)
    };
    (ebp) => {
        Reg::R32(Reg32::Ebp)
    };
    (edi) => {
        Reg::R32(Reg32::Edi)
    };
    (esi) => {
        Reg::R32(Reg32::Esi)
    };
    (al) => {
        Reg::R8(Reg8::Al)
    };
}

#[macro_export]
macro_rules! operand {
    // memory
    ([%$e:tt]) => {
        Operand::Memory(register!($e).into())
    };
    ([%$reg:tt-$d:literal]) => {
        Operand::MemoryAddr(register!($reg), -$d)
    };
    ([%$reg:tt+$d:literal]) => {
        Operand::MemoryAddr(register!($reg), $d)
    };
    ([%{$reg:expr}-{$e:expr}]) => {
        Operand::MemoryAddr($reg, -$e)
    };
    ([%$reg:tt-{$e:expr}]) => {
        Operand::MemoryAddr(register!($reg), -$e)
    };
    ([%{$reg:expr}+{$e:expr}]) => {
        Operand::MemoryAddr($reg, $e)
    };
    ([%$reg:tt+{$e:expr}]) => {
        Operand::MemoryAddr(register!($reg), $e)
    };
    ([%$e:tt]) => {
        Operand::Memory(register!($e).into())
    };
    ([@ {$e:expr}]) => {
        Operand::Memory($e.into())
    };
    ([rel @ $e:tt]) => {
        Operand::IPRelativeMemory(stringify!($e).into())
    };
    ([@ $e:tt]) => {
        Operand::Memory(stringify!($e).into())
    };
    ([^ {$e:expr}]) => {
        Operand::Memory(format!(".{}", $e).into())
    };
    ([^ $e:tt]) => {
        let label = format!(".{}", stringify!($e));
        Operand::Memory(label.into())
    };
    ({$e:expr}) => {
        Operand::Direct($e.into())
    };

    // register
    (%{$reg:expr}) => {
        Operand::Direct($reg.into())
    };
    (%$reg:tt) => {
        Operand::Direct(register!($reg).into())
    };
    ($e:literal) => {
        Operand::Direct($e.into())
    };
    (@{$e:expr}) => {
        Operand::Direct($e.into())
    };
    (@$e:tt) => {
        Operand::Direct(stringify!($e).into())
    };
    (^{$e:expr}) => {
        Operand::Direct(format!(".{}", $e).into())
    };
    (^$e:tt) => {
        Operand::Direct(format!(".{}", stringify!($e)).into())
    };
}

#[macro_export]
macro_rules! assembly {
    (($buf:expr) {}) => {
    };

    /********************/
    /*  MACRO OPERATIONS */
    /********************/
    // Append another set of instructions
    (($buf:expr) {{{$is:expr}} $($tail:tt)*}) => {
        for inst in $is.iter() {
            $buf.push(inst.clone());
        }
        assembly!(($buf) {$($tail)*})
    };


    /********************/
    /*     COMMENTS       */
    /********************/
    (($buf:expr) {;$comment:literal $($tail:tt)*}) => {
        $buf.push(Inst::Comment($comment.into()));
        assembly!(($buf) {$($tail)*})
    };

    (($buf:expr) {;{$comment:expr} $($tail:tt)*}) => {
        $buf.push(Inst::Comment($comment.into()));
        assembly!(($buf) {$($tail)*})
    };

    /********************/
    /*     LABELS       */
    /********************/
    (($buf:expr) {^{$label:expr}: $($tail:tt)*}) => {
        $buf.push(Inst::Label(format!(".{}", $label)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {^$label:tt: $($tail:tt)*}) => {
        $buf.push(Inst::Label(format!(".{}", stringify!($label))));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {@{$label:expr}: $($tail:tt)*}) => {
        $buf.push(Inst::Label($label.into()));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {@$label:tt: $($tail:tt)*}) => {
        $buf.push(Inst::Label(stringify!($label).into()));
        assembly!(($buf) {$($tail)*})
    };

    /********************/
    /* UNIT OPERATORS */
    /********************/
    (($buf:expr) {$inst:tt; $($tail:tt)*}) => {
        $buf.push(unit_op!($inst));
        assembly!(($buf) {$($tail)*})
    };

    /********************/
    /* Special Ops      */
    /********************/
    (($buf:expr) {include $inc:literal; $($tail:tt)*}) => {
        $buf.push(Inst::Include($inc.into()));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {section $inc:literal; $($tail:tt)*}) => {
        $buf.push(Inst::Section($inc.into()));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {global $glbl:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Global(stringify!($glbl).into()));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {data $lbl:tt: dd $val:expr; $($tail:tt)*}) => {
        $buf.push(Inst::Data(stringify!($lbl).into(), $val));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {data $lbl:tt: dq $val:expr; $($tail:tt)*}) => {
        $buf.push(Inst::Data64(stringify!($lbl).into(), $val));
        assembly!(($buf) {$($tail)*})
    };

    (($buf:expr) {neg % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Neg(register!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {idiv % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::IDiv(register!($a)));
        assembly!(($buf) {$($tail)*})
    };

    (($buf:expr) {sete % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Sete(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {setne % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Setne(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {setl % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Setl(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {setle % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Setle(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {setg % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Setg(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {setge % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Setge(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {print_string $a:literal; $($tail:tt)*}) => {
        $buf.push(Inst::PrintString($a.into()));
        assembly!(($buf) {$($tail)*})
    };

    /********************/
    /* UNARY OPERATORS */
    /********************/
    (($buf:expr) {$inst:tt $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {$inst:tt % $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!(%$a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {$inst:tt [ $($mem:tt)+ ]; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!([$($mem)+])));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {$inst:tt @ $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!(@$a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {$inst:tt ^ $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!(^$a)));
        assembly!(($buf) {$($tail)*})
    };

    /********************/
    /* BINARY OPERATORS */
    /********************/
    // reg, literal
    (($buf:expr) {$inst:tt % $a:tt, $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!($b)));
        assembly!(($buf) {$($tail)*})
    };
    // reg, reg
    (($buf:expr) {$inst:tt % $a:tt, % $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!(% $b)));
        assembly!(($buf) {$($tail)*})
    };
    // reg, global label
    (($buf:expr) {$inst:tt % $a:tt, @ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!(@ $b)));
        assembly!(($buf) {$($tail)*})
    };
    // reg, local label
    (($buf:expr) {$inst:tt % $a:tt, ^ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!(^ $b)));
        assembly!(($buf) {$($tail)*})
    };
    // reg, mem
    (($buf:expr) {$inst:tt % $a:tt, [ $($mem:tt)+ ]; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(%$a),  operand!([$($mem)+])));
        assembly!(($buf) {$($tail)*})
    };
    // mem, literal
    (($buf:expr) {$inst:tt [$($a:tt)+], $b:literal; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!($b)));
        assembly!(($buf) {$($tail)*})
    };
    // mem, reg
    (($buf:expr) {$inst:tt [$($a:tt)+], % $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(% $b)));
        assembly!(($buf) {$($tail)*})
    };
    // mem, global label
    (($buf:expr) {$inst:tt [$($a:tt)+], @ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(@ $b)));
        assembly!(($buf) {$($tail)*})
    };
    // mem, local label
    (($buf:expr) {$inst:tt [$($a:tt)+], ^ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(^ $b)));
        assembly!(($buf) {$($tail)*})
    };
}

#[macro_export]
macro_rules! assembly2 {
    (($buf:expr, $info:expr) {}) => {
    };

    /********************/
    /*  MACRO OPERATIONS */
    /********************/
    // Append another set of instructions
    (($buf:expr, $info:expr) {{{$is:expr}} $($tail:tt)*}) => {
        for inst in $is.iter() {
            $buf.push(inst.clone());
        }
        assembly2!(($buf, $info) {$($tail)*})
    };


    /********************/
    /*     COMMENTS       */
    /********************/
    (($buf:expr, $info:expr) {;$comment:literal $($tail:tt)*}) => {
        $buf.push(Inst::Comment($comment.into()));
        assembly2!(($buf, $info) {$($tail)*})
    };

    /********************/
    /*     LABELS       */
    /********************/
    // local
    (($buf:expr, $info:expr) {^{$label:expr}: $($tail:tt)*}) => {
        $buf.push(Inst::Label(format!(".{}_{}", $label, $info.id())));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {^$label:tt: $($tail:tt)*}) => {
        let lbl = stringify!($label);
        $buf.push(Inst::Label(format!(".{}_{}", lbl, $info.id())));
        assembly2!(($buf, $info) {$($tail)*})
    };

    // global
    (($buf:expr, $info:expr) {@{$label:expr}: $($tail:tt)*}) => {
        $buf.push(Inst::Label($label.into()));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {@$label:tt: $($tail:tt)*}) => {
        $buf.push(Inst::Label(stringify!($label).into()));
        assembly2!(($buf, $info) {$($tail)*})
    };

    /********************/
    /* UNIT OPERATORS */
    /********************/
    (($buf:expr, $info:expr) {$inst:tt; $($tail:tt)*}) => {
        $buf.push(unit_op!($inst));
        assembly2!(($buf, $info) {$($tail)*})
    };

    /********************/
    /* Special Ops      */
    /********************/
    (($buf:expr, $info:expr) {include $inc:literal; $($tail:tt)*}) => {
        $buf.push(Inst::Include($inc.into()));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {section $inc:literal; $($tail:tt)*}) => {
        $buf.push(Inst::Section($inc.into()));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {global $glbl:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Global(stringify!($glbl).into()));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {data $lbl:tt: dd $val:expr; $($tail:tt)*}) => {
        $buf.push(Inst::Data(stringify!($lbl).into(), $val));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {data $lbl:tt: dq $val:expr; $($tail:tt)*}) => {
        $buf.push(Inst::Data64(stringify!($lbl).into(), $val));
        assembly2!(($buf, $info) {$($tail)*})
    };

    (($buf:expr, $info:expr) {sete % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Sete(reg8!($a)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {print_str $a:literal; $($tail:tt)*}) => {
        $buf.push(Inst::PrintStr($a.into()));
        assembly2!(($buf, $info) {$($tail)*})
    };

    /********************/
    /* UNARY OPERATORS */
    /********************/
    (($buf:expr, $info:expr) {$inst:tt $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!($a)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {$inst:tt % $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!(%$a)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {$inst:tt [ $($mem:tt)+ ]; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!([$($mem)+])));
        assembly2!(($buf, $info) {$($tail)*})
    };
    (($buf:expr, $info:expr) {$inst:tt @ $a:tt; $($tail:tt)*}) => {
        $buf.push(unary_op!($inst)(operand!(@$a)));
        assembly2!(($buf, $info) {$($tail)*})
    };

    (($buf:expr, $info:expr) {$inst:tt ^{$a:expr}; $($tail:tt)*}) => {
        let lbl = Operand::Direct(format!(".{}_{}", $a, $info.id()).into());
        $buf.push(unary_op!($inst)(lbl));
        assembly2!(($buf, $info) {$($tail)*})
    };

    (($buf:expr, $info:expr) {$inst:tt ^ $a:tt; $($tail:tt)*}) => {
        let lbl = stringify!($a);
        let lbl = Operand::Direct(format!(".{}_{}", lbl, $info.id()).into());
        $buf.push(unary_op!($inst)(lbl));
        assembly2!(($buf, $info) {$($tail)*})
    };

    /********************/
    /* BINARY OPERATORS */
    /********************/
    // reg, literal
    (($buf:expr, $info:expr) {$inst:tt % $a:tt, $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!($b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // reg, reg
    (($buf:expr, $info:expr) {$inst:tt % $a:tt, % $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!(% $b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // reg, global label
    (($buf:expr, $info:expr) {$inst:tt % $a:tt, @ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(% $a), operand!(@ $b)));
        assembly2!(($buf, $info) {$($tail)*})
    };

    // reg, local label
    (($buf:expr, $info:expr) {$inst:tt % $a:tt, ^ $b:tt; $($tail:tt)*}) => {
        let lbl = stringify!($b);
        let lbl = Operand::Direct(format!(".{}_{}", lbl, $info.id()).into());
        $buf.push(binary_op!($inst)(operand!(% $a), lbl));
        assembly2!(($buf, $info) {$($tail)*})
    };

    // reg, mem
    (($buf:expr, $info:expr) {$inst:tt % $a:tt, [ $($mem:tt)+ ]; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!(%$a),  operand!([$($mem)+])));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // mem, literal
    (($buf:expr, $info:expr) {$inst:tt [$($a:tt)+], $b:literal; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!($b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // mem, reg
    (($buf:expr, $info:expr) {$inst:tt [$($a:tt)+], % $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(% $b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // mem, global label
    (($buf:expr, $info:expr) {$inst:tt [$($a:tt)+], @ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(@ $b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
    // mem, local label
    (($buf:expr, $info:expr) {$inst:tt [$($a:tt)+], ^ $b:tt; $($tail:tt)*}) => {
        $buf.push(binary_op!($inst)(operand!([$($a)+]), operand!(^ $b)));
        assembly2!(($buf, $info) {$($tail)*})
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn x64() {
        let mut code = vec![];
        assembly! {(code) {
            mov %rax, 0i64;
            push %rsp;
            pop %rbp;
        }}

        assert_eq!(code.len(), 3);
        let expected = Inst::Mov(
            Operand::Direct(DirectOperand::Register(Reg::R64(Reg64::Rax))),
            Operand::Direct(DirectOperand::Integer64(0)),
        );
        assert_eq!(code[0], expected);
        let expected = Inst::Push(Operand::Direct(DirectOperand::Register(Reg::R64(
            Reg64::Rsp,
        ))));
        assert_eq!(code[1], expected);
        let expected = Inst::Pop(Operand::Direct(DirectOperand::Register(Reg::R64(
            Reg64::Rbp,
        ))));
        assert_eq!(code[2], expected);
    }

    #[test]
    fn x64_data() {
        let mut code = vec![];
        assembly! {(code) {
            data some_value_64: dq 0;
        }}

        assert_eq!(code.len(), 1);
        let expected = Inst::Data64("some_value_64".into(), 0);
        assert_eq!(code[0], expected);
    }

    #[test]
    fn x86() {
        let mut code = vec![];
        assembly! {(code) {
            mov %eax, 0i64;
            push %esp;
            pop %ebp;
        }}

        assert_eq!(code.len(), 3);
        let expected = Inst::Mov(
            Operand::Direct(DirectOperand::Register(Reg::R32(Reg32::Eax))),
            Operand::Direct(DirectOperand::Integer64(0)),
        );
        assert_eq!(code[0], expected);
        let expected = Inst::Push(Operand::Direct(DirectOperand::Register(Reg::R32(
            Reg32::Esp,
        ))));
        assert_eq!(code[1], expected);
        let expected = Inst::Pop(Operand::Direct(DirectOperand::Register(Reg::R32(
            Reg32::Ebp,
        ))));
        assert_eq!(code[2], expected);
    }
}
