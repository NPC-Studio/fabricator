use crate::compiler::{graph, ir};

pub mod block_liveness;
pub mod verify;

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
