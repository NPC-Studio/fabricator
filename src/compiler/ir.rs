use crate::{
    constant::Constant,
    util::typed_id_map::{new_id_type, IdMap},
};

new_id_type!(
    pub struct VarId;
    pub struct BlockId;
    pub struct InstId;
);

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

pub enum Branch {
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
    pub branch: Branch,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            branch: Branch::Return { returns: 0 },
        }
    }
}

pub struct Function<S> {
    pub instructions: IdMap<InstId, Instruction<S>>,
    pub blocks: IdMap<BlockId, Block>,
    pub heap_vars: IdMap<VarId, ()>,
}

impl<S> Default for Function<S> {
    fn default() -> Self {
        Self {
            instructions: IdMap::new(),
            blocks: IdMap::new(),
            heap_vars: IdMap::new(),
        }
    }
}
