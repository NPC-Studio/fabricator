use crate::{
    constant::Constant,
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
