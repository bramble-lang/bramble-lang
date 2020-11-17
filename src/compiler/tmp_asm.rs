#[derive(Debug, Copy, Clone)]
pub enum Register {
    Eax,
    Al,
    Ebx,
    Ecx,
    Edx,
    Ebp,
    Esp,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Eax => f.write_str("eax"),
            Register::Al => f.write_str("al"),
            Register::Ebx => f.write_str("ebx"),
            Register::Ecx => f.write_str("ecx"),
            Register::Edx => f.write_str("edx"),
            Register::Ebp => f.write_str("ebp"),
            Register::Esp => f.write_str("esp"),
        }
    }
}

#[derive(Debug)]
pub enum Location {
    Register(Register),
    Memory(String),
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Location::Register(reg) => {
                let s = format!("{}", reg);
                f.write_str(&s)
            }
            Location::Memory(m) => {
                let s = format!("[{}]", m);
                f.write_str(&s)
            }
        }
    }
}

#[derive(Debug)]
pub enum Source {
    Register(Register),
    Memory(String),
    Address(String),
    Integer(i32),
}

impl std::fmt::Display for Source {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Source::Register(reg) => {
                let s = format!("{}", reg);
                f.write_str(&s)
            }
            Source::Memory(m) => {
                let s = format!("[{}]", m);
                f.write_str(&s)
            }
            Source::Address(m) => {
                let s = format!("{}", m);
                f.write_str(&s)
            }
            Source::Integer(i) => {
                let s = format!("{}", i);
                f.write_str(&s)
            }
        }
    }
}

pub type Label = String;

#[derive(Debug)]
pub enum Instruction {
    Jmp(Label),
    Jz(Label),
    Call(Label),
    Ret,
    Mov(Location, Source),
    Movzx(Location, Source),
    Lea(Location, Source),
    Add(Register, Source),
    Sub(Register, Source),
    IMul(Register, Location),
    And(Register, Source),
    Or(Register, Source),
    Cmp(Register, Source),
    Setl(Register),
    Setle(Register),
    Setg(Register),
    Setge(Register),
    Sete(Register),
    Setne(Register),
    Push(Register),
    Pop(Register),
    Print(Source),
    PrintStr(String),
    Newline,
    Label(Label),
    Include(String),
    Section(String),
    Global(String),
    Data(String, i32),
}

pub fn print_x86_program(code: &Vec<Instruction>, output: &mut dyn std::io::Write) -> std::io::Result<()> {
    for inst in code.iter() {
        print_x86(&inst, output)?;
    }
    Ok(())
}

pub fn print_x86(inst: &Instruction, output: &mut dyn std::io::Write) -> std::io::Result<()> {
    use Instruction::*;
    match inst {
        Include(include) => {
            writeln!(output, "%include \"{}\"", include)?;
        }
        Section(section) => {
            writeln!(output, "\nsection {}", section)?;
        }
        Global(global) => {
            writeln!(output, "global {}", global)?;
        }
        Data(label, value) => {
            writeln!(output, "{}: dd {}", label, value)?;
        }
        Label(label) => {
            if !label.starts_with(".") {
                writeln!(output)?;
            }
            writeln!(output, "{}:", label)?;
        }
        inst => {
            write!(output, "    ")?;
            match inst {
                Mov(l, Source::Memory(s)) => writeln!(output, "mov {}, DWORD [{}]", l, s)?,
                Mov(l, Source::Address(s)) => writeln!(output, "mov {}, {}", l, s)?,
                Mov(l, Source::Integer(s)) => writeln!(output, "mov {}, DWORD {}", l, s)?,
                Mov(l, Source::Register(s)) => writeln!(output, "mov {}, {}", l, s)?,

                Movzx(l, Source::Memory(s)) => {
                    writeln!(output, "movzx {}, DWORD [{}]", l, s)?
                }
                Movzx(l, Source::Address(s)) => writeln!(output, "movzx {}, {}", l, s)?,
                Movzx(l, Source::Integer(s)) => {
                    writeln!(output, "movzx {}, DWORD {}", l, s)?
                }
                Movzx(l, Source::Register(s)) => writeln!(output, "movzx {}, {}", l, s)?,

                Lea(l, s) => writeln!(output, "lea {}, {}", l, s)?,
                Push(reg) => {
                    writeln!(output, "push {}", reg)?;
                }
                Pop(reg) => {
                    writeln!(output, "pop {}", reg)?;
                }
                Cmp(reg, s) => {
                    writeln!(output, "cmp {}, {}", reg, s)?;
                }
                Sete(reg) => {
                    writeln!(output, "sete {}", reg)?;
                }
                Setne(reg) => {
                    writeln!(output, "setne {}", reg)?;
                }
                Setl(reg) => {
                    writeln!(output, "setl {}", reg)?;
                }
                Setle(reg) => {
                    writeln!(output, "setle {}", reg)?;
                }
                Setg(reg) => {
                    writeln!(output, "setg {}", reg)?;
                }
                Setge(reg) => {
                    writeln!(output, "setge {}", reg)?;
                }
                IMul(reg, s) => {
                    writeln!(output, "imul {}, {}", reg, s)?;
                }
                Add(reg, s) => {
                    writeln!(output, "add {}, {}", reg, s)?;
                }
                Sub(reg, s) => {
                    writeln!(output, "sub {}, {}", reg, s)?;
                }
                And(reg, s) => {
                    writeln!(output, "and {}, {}", reg, s)?;
                }
                Or(reg, s) => {
                    writeln!(output, "or {}, {}", reg, s)?;
                }
                Jmp(loc) => {
                    writeln!(output, "jmp {}", loc)?;
                }
                Jz(loc) => {
                    writeln!(output, "jz {}", loc)?;
                }
                Call(label) => writeln!(output, "call {}", label)?,
                Ret => writeln!(output, "ret")?,
                Print(s) => {
                    writeln!(output, "PRINT_DEC 4, {}", s)?;
                }
                PrintStr(s) => {
                    writeln!(output, "PRINT_STRING \"{}\"", s)?;
                }
                Newline => {
                    writeln!(output, "NEWLINE")?;
                }
                _ => panic!("Compiler: unexpected instruction: {:?}", inst),
            }
        }
    }
    Ok(())
}