use std::fmt;

use arrayvec::ArrayVec;

use crate::{
    compiler::constant::Constant,
    util::typed_id_map::{new_id_type, IdMap},
};

new_id_type! {
    pub struct BlockId;
    pub struct InstId;
    pub struct Variable;
    pub struct ShadowVar;
}

pub type Offset = i32;
pub type ArgCount = u8;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    Not,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinComp {
    LessThan,
    LessEqual,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterEqual,
}

/// A single IR instruction.
///
/// Instructions are only allowed to appear in *once* across an entire IR.
///
/// IR instructions are always in SSA (Single Static Assignment) form and thus have an implicit
/// "output variable". Other instructions that use the output of a previous instruction will
/// reference that instruction directly via `InstId`.
///
/// In order for the IR to be well-formed, instructions are only allowed to appear *once* in an
/// entire IR, or in other words, the same `InstId` cannot be re-used either in the same block
/// or in different blocks. Also, all uses of an instruction must be "dominated" by its singular
/// definition -- in other words, all paths through the CFG (Control Flow Graph) starting with
/// `start_block` that reach any use of an instruction must always pass through its definition.
///
/// # SSA Form
///
/// SSA form requires that join points in the CFG have special instructions to select between
/// different data definitions depending on the path in the CFG that was taken. These are normally
/// called "phi functions" and are described by Cytron et al. here:
///
/// https://dl.acm.org/doi/pdf/10.1145/115372.115320
///
/// We use a slight modification to this system here. Instead of "phi" instructions referencing the
/// instructions they select between, instead a separate "upsilon" instruction writes to a "shadow
/// variable" that is present for every `Phi` instruction. The `ShadowVarId` type is the unique
/// identifier for this shadow variable in a single phi instruction.
///
/// This phi / upsilon SSA form was invented by Filip Pizlo is more deeply explained in this
/// document (where he calls it "pizlo-form"):
///
/// https://gist.github.com/pizlonator/79b0aa601912ff1a0eb1cb9253f5e98d
///
/// In order for the IR to be well-formed, any `ShadowVarId` identifier must be unique, owned by
/// a *single* `Phi` instruction. These shadow variables are Single Static *Use*, they are used
/// only once by a unique `Phi` instruction. Additionally, all paths through the CFG starting with
/// `start_block` that may reach a `Phi` instruction must have an `Upsilon` that assigns to that
/// `Phi`'s shadow variable to ensure that it has a defined value.
///
/// # Variables
///
/// IR "variables" represent notionally heap allocated values that are an escape hatch for SSA form.
/// Each `VarId` references a unique variable, and `GetVariable` and `SetVariable` instructions read
/// from and write to these variables.
///
/// The output of the compiler will use IR variables to represent actual variables in code, and
/// will rely on IR optimization to convert them to SSA form, potentially by inserting `Phi` and
/// `Upsilon` instructions.
///
/// Normally, all IR variables can be converted into SSA form in this way, however it is allowed
/// and normal for them to still be present during codegen! IR variables are also a way to represent
/// values shared mutably across separate closures, and they may remain after optimization if any
/// such shared values are present. Any `VarId` that remains after optimization really will be
/// turned into a heap allocated variable, allowing them to be mutably shared between different
/// closures with potentially different lifetimes.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Instruction<S> {
    NoOp,
    Copy(InstId),
    Undefined,
    Constant(Constant<S>),
    GetVariable(Variable),
    SetVariable(Variable, InstId),
    Phi(ShadowVar),
    Upsilon(ShadowVar, InstId),
    UnOp {
        source: InstId,
        op: UnOp,
    },
    BinOp {
        left: InstId,
        right: InstId,
        op: BinOp,
    },
    BinComp {
        left: InstId,
        right: InstId,
        comp: BinComp,
    },
    Push(InstId),
    Pop,
    Call {
        source: InstId,
        args: ArgCount,
        returns: ArgCount,
    },
}

pub const MAX_INSTRUCTION_SOURCES: usize = 2;

impl<S> Instruction<S> {
    pub fn sources(&self) -> ArrayVec<InstId, MAX_INSTRUCTION_SOURCES> {
        let mut sources = ArrayVec::new();

        match self {
            Instruction::NoOp => {}
            Instruction::Copy(source) => {
                sources.push(*source);
            }
            Instruction::Undefined => {}
            Instruction::Constant(_) => {}
            Instruction::GetVariable(_) => {}
            Instruction::SetVariable(_, source) => {
                sources.push(*source);
            }
            Instruction::Phi(_) => {}
            Instruction::Upsilon(_, source) => sources.push(*source),
            Instruction::UnOp { source, .. } => {
                sources.push(*source);
            }
            Instruction::BinOp { left, right, .. } => {
                sources.push(*left);
                sources.push(*right);
            }
            Instruction::BinComp { left, right, .. } => {
                sources.push(*left);
                sources.push(*right);
            }
            Instruction::Push(source) => {
                sources.push(*source);
            }
            Instruction::Pop => {}
            Instruction::Call { source, .. } => {
                sources.push(*source);
            }
        }

        sources
    }

    pub fn sources_mut(&mut self) -> ArrayVec<&mut InstId, MAX_INSTRUCTION_SOURCES> {
        let mut sources = ArrayVec::new();

        match self {
            Instruction::NoOp => {}
            Instruction::Copy(source) => {
                sources.push(source);
            }
            Instruction::Undefined => {}
            Instruction::Constant(_) => {}
            Instruction::GetVariable(_) => {}
            Instruction::SetVariable(_, source) => {
                sources.push(source);
            }
            Instruction::Phi(_) => {}
            Instruction::Upsilon(_, source) => sources.push(source),
            Instruction::UnOp { source, .. } => {
                sources.push(source);
            }
            Instruction::BinOp { left, right, .. } => {
                sources.push(left);
                sources.push(right);
            }
            Instruction::BinComp { left, right, .. } => {
                sources.push(left);
                sources.push(right);
            }
            Instruction::Push(source) => {
                sources.push(source);
            }
            Instruction::Pop => {}
            Instruction::Call { source, .. } => {
                sources.push(source);
            }
        }

        sources
    }

    pub fn has_value(&self) -> bool {
        match self {
            Instruction::NoOp => false,
            Instruction::Copy(_) => true,
            Instruction::Undefined => true,
            Instruction::Constant(_) => true,
            Instruction::GetVariable(_) => true,
            Instruction::SetVariable { .. } => false,
            Instruction::Phi(_) => true,
            Instruction::Upsilon(_, _) => false,
            Instruction::UnOp { .. } => true,
            Instruction::BinOp { .. } => true,
            Instruction::BinComp { .. } => true,
            Instruction::Push { .. } => false,
            Instruction::Pop => true,
            Instruction::Call { .. } => false,
        }
    }

    pub fn has_effect(&self) -> bool {
        match self {
            Instruction::NoOp => false,
            Instruction::Copy(_) => false,
            Instruction::Undefined => false,
            Instruction::Constant(_) => false,
            Instruction::GetVariable(_) => false,
            Instruction::SetVariable { .. } => true,
            Instruction::Phi(_) => false,
            Instruction::Upsilon(_, _) => true,
            Instruction::UnOp { .. } => false,
            Instruction::BinOp { .. } => false,
            Instruction::BinComp { .. } => false,
            Instruction::Push { .. } => true,
            Instruction::Pop => true,
            Instruction::Call { .. } => true,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Exit {
    Return {
        returns: ArgCount,
    },
    Jump(BlockId),
    Branch {
        cond: InstId,
        if_false: BlockId,
        if_true: BlockId,
    },
}

pub const MAX_BLOCK_SUCCESSORS: usize = 2;

impl Exit {
    pub fn cond_source(&self) -> Option<InstId> {
        match self {
            Exit::Branch { cond, .. } => Some(*cond),
            _ => None,
        }
    }

    pub fn successors(&self) -> ArrayVec<BlockId, MAX_BLOCK_SUCCESSORS> {
        let mut successors = ArrayVec::<_, 2>::new();

        match self {
            Exit::Return { .. } => {}
            Exit::Jump(block_id) => {
                successors.push(*block_id);
            }
            Exit::Branch {
                if_true, if_false, ..
            } => {
                successors.push(*if_true);
                successors.push(*if_false);
            }
        }

        successors
    }
}

#[derive(Debug)]
pub struct Block {
    pub instructions: Vec<InstId>,
    pub exit: Exit,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            exit: Exit::Return { returns: 0 },
        }
    }
}

pub struct FunctionParts<S> {
    pub instructions: IdMap<InstId, Instruction<S>>,
    pub blocks: IdMap<BlockId, Block>,
    pub variables: IdMap<Variable, ()>,
    pub shadow_vars: IdMap<ShadowVar, ()>,
}

impl<S> Default for FunctionParts<S> {
    fn default() -> Self {
        Self {
            instructions: Default::default(),
            blocks: Default::default(),
            variables: Default::default(),
            shadow_vars: Default::default(),
        }
    }
}

pub struct Function<S> {
    pub parts: FunctionParts<S>,
    pub start_block: BlockId,
}

impl<S: AsRef<str>> Function<S> {
    pub fn pretty_print(&self, f: &mut dyn fmt::Write, indent: u8) -> fmt::Result {
        let base_indent = indent as usize;
        let write_indent = |f: &mut dyn fmt::Write, indent: u8| -> fmt::Result {
            let indent = base_indent + indent as usize;
            write!(f, "{:indent$}", "")?;
            Ok(())
        };

        let write_block = |f: &mut dyn fmt::Write, block_id: BlockId| -> fmt::Result {
            let block = &self.parts.blocks[block_id];

            write_indent(f, 0)?;
            writeln!(f, "block B{}:", block_id.index())?;

            for &inst_id in &block.instructions {
                let inst = &self.parts.instructions[inst_id];

                write_indent(f, 4)?;
                write!(f, "I{}: ", inst_id.index())?;

                match inst {
                    Instruction::NoOp => {
                        writeln!(f, "no_op()")?;
                    }
                    Instruction::Copy(source) => {
                        writeln!(f, "copy(I{})", source.index())?;
                    }
                    Instruction::Undefined => {
                        writeln!(f, "undefined()")?;
                    }
                    Instruction::Constant(constant) => {
                        writeln!(f, "constant({:?})", constant.as_ref())?;
                    }
                    Instruction::GetVariable(var) => {
                        writeln!(f, "get_var(V{})", var.index())?;
                    }
                    Instruction::SetVariable(var, source) => {
                        writeln!(f, "set_var(V{}, I{})", var.index(), source.index())?;
                    }
                    Instruction::Phi(shadow) => {
                        writeln!(f, "phi(S{})", shadow.index())?;
                    }
                    Instruction::Upsilon(shadow, source) => {
                        writeln!(f, "upsilon(S{}, I{})", shadow.index(), source.index())?;
                    }
                    Instruction::UnOp { source, op } => match op {
                        UnOp::Not => {
                            writeln!(f, "not(I{})", source.index())?;
                        }
                    },
                    Instruction::BinOp { left, right, op } => match op {
                        BinOp::Add => {
                            writeln!(f, "add(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Sub => {
                            writeln!(f, "sub(I{}, I{})", left.index(), right.index())?;
                        }
                    },
                    Instruction::BinComp { left, right, comp } => match comp {
                        BinComp::LessThan => {
                            writeln!(f, "less_than(I{}, I{})", left.index(), right.index())?;
                        }
                        BinComp::LessEqual => {
                            writeln!(f, "less_equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinComp::Equal => {
                            writeln!(f, "equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinComp::NotEqual => {
                            writeln!(f, "not_equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinComp::GreaterThan => {
                            writeln!(f, "greater_than(I{}, I{})", left.index(), right.index())?;
                        }
                        BinComp::GreaterEqual => {
                            writeln!(f, "greater_equal(I{}, I{})", left.index(), right.index())?;
                        }
                    },
                    Instruction::Push(source) => {
                        writeln!(f, "push(I{})", source.index())?;
                    }
                    Instruction::Pop => {
                        writeln!(f, "pop()")?;
                    }
                    Instruction::Call {
                        source,
                        args,
                        returns,
                    } => {
                        writeln!(
                            f,
                            "call(I{}, args = {}, returns = {})",
                            source.index(),
                            args,
                            returns
                        )?;
                    }
                }
            }

            write_indent(f, 4)?;
            match block.exit {
                Exit::Return { returns } => {
                    writeln!(f, "return(args = {})", returns)?;
                }
                Exit::Jump(block_id) => {
                    writeln!(f, "jump(B{})", block_id.index())?;
                }
                Exit::Branch {
                    cond,
                    if_true,
                    if_false,
                } => {
                    writeln!(
                        f,
                        "branch(I{}, false => B{}, true => B{})",
                        cond.index(),
                        if_false.index(),
                        if_true.index()
                    )?;
                }
            }

            Ok(())
        };

        write_indent(f, 0)?;
        writeln!(f, "start_block(B{})", self.start_block.index())?;

        for block_id in self.parts.blocks.ids() {
            write_block(f, block_id)?;
        }

        Ok(())
    }
}

impl<S: AsRef<str>> fmt::Debug for Function<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function(")?;
        self.pretty_print(f, 4)?;
        writeln!(f, ")")?;
        Ok(())
    }
}
