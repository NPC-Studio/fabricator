use crate::compiler::{graph, ir};

pub mod instruction_liveness;

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
