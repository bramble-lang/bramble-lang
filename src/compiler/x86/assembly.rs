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

#[derive(Clone)]
pub enum Reg8 {
    Al,
}

impl Display for Reg8 {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg8::*;
        match self {
            Al => f.write_str("al"),
        }
    }
}

#[derive(Clone)]
pub enum Reg32 {
    Eax,
    Ecx,
    Edx,
    Ebx,
    Esp,
    Ebp,
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
        }
    }
}

#[derive(Clone)]
pub enum Reg {
    R8(Reg8),
    R32(Reg32),
}

impl Display for Reg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Reg::*;
        match self {
            R8(r8) => f.write_fmt(format_args!("{}", r8)),
            R32(r32) => f.write_fmt(format_args!("{}", r32)),
        }
    }
}

#[derive(Clone)]
pub enum DirectOperand {
    Integer(i32),
    Register(Reg),
    Label(String),
}

impl Display for DirectOperand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use DirectOperand::*;
        match self {
            Integer(i) => f.write_fmt(format_args!("{}", i)),
            Register(reg) => f.write_fmt(format_args!("{}", reg)),
            Label(lbl) => f.write_fmt(format_args!("{}", lbl)),
        }
    }
}

#[derive(Clone)]
pub enum Operand {
    Direct(DirectOperand),
    Memory(DirectOperand),
    MemoryExpr(Reg, u32),
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use Operand::*;
        match self {
            Direct(d) => f.write_fmt(format_args!("{}", d)),
            Memory(mem) => f.write_fmt(format_args!("[{}]", mem)),
            MemoryExpr(mem, d) => f.write_fmt(format_args!("[{}-{}]", mem, d)),
        }
    }
}

#[derive(Clone)]
pub enum Inst {
    Include(String),
    Section(String),
    Global(String),
    Data(String, i32),
    Label(String),

    Jmp(Operand),
    Jz(Operand),
    Call(Operand),
    Ret,

    Push(Operand),
    Pop(Operand),
    Mov(Operand, Operand),
    Lea(Operand, Operand),

    Add(Operand, Operand),
    Sub(Operand, Operand),
    IMul(Operand, Operand),
    
    Cmp(Operand, Operand),
    
    And(Operand, Operand),
    Or(Operand, Operand),

    Sete(Reg8),

    PrintStr(String),
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
            Include(inc) => f.write_fmt(format_args!("%include \"{}\"", inc)),
            Section(section) => f.write_fmt(format_args!("\nsection {}", section)),
            Global(global) => f.write_fmt(format_args!("global {}", global)),
            Data(lbl, value) => f.write_fmt(format_args!("{}: dd {}", lbl, value)),

            Jmp(a) => f.write_fmt(format_args!("jmp {}", a)),
            Jz(a) => f.write_fmt(format_args!("jz {}", a)),
            Call(a) => f.write_fmt(format_args!("call {}", a)),
            Ret => f.write_fmt(format_args!("ret")),

            Push(a) => f.write_fmt(format_args!("push {}", a)),
            Pop(a) => f.write_fmt(format_args!("pop {}", a)),

            Mov(a, b) => f.write_fmt(format_args!("mov {}, {}", a, match b {
                Operand::Direct(DirectOperand::Integer(_)) | Operand::Memory(_) | Operand::MemoryExpr(_, _)=> format!("DWORD {}", b),
                _ => format!("{}", b),
            })),
            Lea(a, b) => f.write_fmt(format_args!("lea {}, {}", a, b)),
            Add(a, b) => f.write_fmt(format_args!("add {}, {}", a, b)),
            Sub(a, b) => f.write_fmt(format_args!("sub {}, {}", a, b)),
            IMul(a, b) => f.write_fmt(format_args!("imul {}, {}", a, b)),
            Cmp(a, b) => f.write_fmt(format_args!("cmp {}, {}", a, b)),
            And(a, b) => f.write_fmt(format_args!("and {}, {}", a, b)),
            Or(a, b) => f.write_fmt(format_args!("or {}, {}", a, b)),
            Sete(a) => f.write_fmt(format_args!("sete {}", a)),
            Label(lbl) => f.write_fmt(format_args!("{}:", lbl)),

            PrintStr(str) => f.write_fmt(format_args!("PRINT_STRING \"{}\"", str)),
            PrintDec(val) => f.write_fmt(format_args!("PRINT_DEC 4, {}", val)),
            NewLine => f.write_fmt(format_args!("NEWLINE")),
        }
    }
}

#[macro_export]
macro_rules! unit_op {
    (ret) => {
        Inst::Ret
    };
    (newline) => {
        Inst::NewLine
    }
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
}

#[macro_export]
macro_rules! binary_op {
    (mov) => {
        Inst::Mov
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
}

#[macro_export]
macro_rules! reg8 {
    (al) => {
        Reg8::Al
    };
}

#[macro_export]
macro_rules! register {
    (eax) => {
        Reg::R32(Reg32::Eax)
    };
    (ecx) => {
        Reg::R32(Reg32::Ecx)
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
    (al) => {
        Reg::R8(Reg8::Al)
    };
}

#[macro_export]
macro_rules! operand {
    // memory
    ([%$e:tt]) => {
        Operand::Memory(DirectOperand::Register(register!($e)))
    };
    ([%$reg:tt-$d:literal]) => {
        Operand::MemoryExpr(register!($reg), $d)
    };
    ([%$reg:tt-{$e:expr}]) => {
        Operand::MemoryExpr(register!($reg), $e)
    };
    ([%$e:tt]) => {
        Operand::Memory(DirectOperand::Register(register!($e)))
    };
    ([@ {$e:expr}]) => {
        Operand::Memory(DirectOperand::Label($e.into()))
    };
    ([@ $e:tt]) => {
        Operand::Memory(DirectOperand::Label(stringify!($e).into()))
    };
    ({$e:expr}) => {
        Operand::Direct(DirectOperand::Integer($e))
    };

    (%$reg:tt) => {
        Operand::Direct(DirectOperand::Register(register!($reg)))
    };
    ($e:literal) => {
        Operand::Direct(DirectOperand::Integer($e))
    };
    (@$e:tt) => {
        Operand::Direct(DirectOperand::Label(stringify!($e).into()))
    };
    (^$e:tt) => {
        Operand::Direct(DirectOperand::Label(format!(".{}", stringify!($e)).into()))
    };
}

#[macro_export]
macro_rules! assembly {
    (($buf:expr) {}) => {
    };

    /********************/
    /*     LABELS       */
    /********************/
    (($buf:expr) {^$label:tt: $($tail:tt)*}) => {
        $buf.push(Inst::Label(format!(".{}", stringify!($label))));
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

    (($buf:expr) {sete % $a:tt; $($tail:tt)*}) => {
        $buf.push(Inst::Sete(reg8!($a)));
        assembly!(($buf) {$($tail)*})
    };
    (($buf:expr) {print_str $a:literal; $($tail:tt)*}) => {
        $buf.push(Inst::PrintStr($a.into()));
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