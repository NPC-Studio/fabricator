use super::{dominators, ir};

mod verify_ir;

pub use self::verify_ir::verify_ir;

impl dominators::Node for ir::VarId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}

impl dominators::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}

impl dominators::Node for ir::InstId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
