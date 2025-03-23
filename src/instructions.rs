pub type RegIdx = u8;
pub type ConstIdx = u16;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Instruction {
    Load {
        constant: ConstIdx,
        dest: RegIdx,
    },
    Move {
        source: RegIdx,
        dest: RegIdx,
    },
    Jump {
        offset: i16,
    },
    JumpIfLess {
        arg1: RegIdx,
        arg2: RegIdx,
        offset: i16,
    },
    JumpIfLessEqual {
        arg1: RegIdx,
        arg2: RegIdx,
        offset: i16,
    },
    IncAndTestLessEqual {
        inc: RegIdx,
        test: RegIdx,
        offset: i16,
    },
    Add {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    Sub {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    Push {
        source: RegIdx,
        len: u8,
    },
    Pop {
        dest: RegIdx,
        len: u8,
    },
    Call {
        func: RegIdx,
        args: u8,
        returns: u8,
    },
    Return {
        returns: u8,
    },
}
