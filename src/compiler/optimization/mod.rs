use super::{dominators, ir};

pub mod verify;

impl dominators::Node for ir::InstId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}

impl dominators::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
