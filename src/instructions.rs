pub type RegIdx = u8;
pub type HeapIdx = u8;
pub type ConstIdx = u16;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Instruction {
    LoadConstant {
        constant: ConstIdx,
        dest: RegIdx,
    },
    GetHeap {
        heap: HeapIdx,
        dest: RegIdx,
    },
    SetHeap {
        source: RegIdx,
        heap: HeapIdx,
    },
    Move {
        source: RegIdx,
        dest: RegIdx,
    },
    Not {
        arg: RegIdx,
        dest: RegIdx,
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
    TestEqual {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    TestNotEqual {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    TestLess {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    TestLessEqual {
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    },
    Jump {
        offset: i16,
    },
    JumpIf {
        arg: RegIdx,
        is_true: bool,
        offset: i16,
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
