use arrayvec::ArrayVec;

use crate::util::typed_id_map::{new_id_type, IdMap};

use super::constant::Constant;

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
    Greater,
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
        op: BinOp,
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
    Push {
        source: InstId,
    },
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
            Instruction::Push { source } => {
                sources.push(*source);
            }
            Instruction::Pop => {}
            Instruction::Call { source, .. } => {
                sources.push(*source);
            }
        }

        sources
    }
}

pub enum Exit {
    Return {
        returns: ArgCount,
    },
    Jump(BlockId),
    Branch {
        cond: InstId,
        if_true: BlockId,
        if_false: BlockId,
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
    pub heap_vars: IdMap<VarId, ()>,
}

impl<S> Default for FunctionParts<S> {
    fn default() -> Self {
        Self {
            instructions: Default::default(),
            blocks: Default::default(),
            heap_vars: Default::default(),
        }
    }
}

pub struct Function<S> {
    pub parts: FunctionParts<S>,
    pub start_block: BlockId,
}
