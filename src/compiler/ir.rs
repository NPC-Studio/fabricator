use std::fmt;

use arrayvec::ArrayVec;

use crate::{
    compiler::constant::Constant,
    util::typed_id_map::{new_id_type, IdMap},
};

new_id_type! {
    pub struct VarId;
    pub struct BlockId;
    pub struct InstId;
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Instruction<S> {
    Constant(Constant<S>),
    GetVariable(VarId),
    SetVariable {
        source: InstId,
        dest: VarId,
    },
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
            Instruction::Constant(_) => {}
            Instruction::GetVariable(_) => {}
            Instruction::SetVariable { source, .. } => {
                sources.push(*source);
            }
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

    pub fn has_value(&self) -> bool {
        match self {
            Instruction::Constant(_) => true,
            Instruction::GetVariable(_) => true,
            Instruction::SetVariable { .. } => false,
            Instruction::UnOp { .. } => true,
            Instruction::BinOp { .. } => true,
            Instruction::BinComp { .. } => true,
            Instruction::Push { .. } => false,
            Instruction::Pop => true,
            Instruction::Call { .. } => false,
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
    pub variables: IdMap<VarId, ()>,
}

impl<S> Default for FunctionParts<S> {
    fn default() -> Self {
        Self {
            instructions: Default::default(),
            blocks: Default::default(),
            variables: Default::default(),
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
                    Instruction::Constant(constant) => {
                        writeln!(f, "constant({:?})", constant.as_ref())?;
                    }
                    Instruction::GetVariable(var_id) => {
                        writeln!(f, "get_var(V{})", var_id.index())?;
                    }
                    Instruction::SetVariable { source, dest } => {
                        writeln!(f, "set_var(V{}, I{})", dest.index(), source.index())?;
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
