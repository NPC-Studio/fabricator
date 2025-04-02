use crate::compiler::{graph, ir};

pub mod verify;

impl graph::Node for ir::InstId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
