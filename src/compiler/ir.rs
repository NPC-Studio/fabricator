use crate::constant::Constant;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VarId(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct BlockId(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InstId(pub usize);

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
pub enum IrInst<S> {
    Constant(Constant<S>),
    GetVariable(VarId),
    SetVariable(VarId),
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

pub enum IrBranch {
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
