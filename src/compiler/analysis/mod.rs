use crate::compiler::{graph, ir};

pub mod instruction_liveness;
pub mod shadow_liveness;
pub mod ssa_conversion;
pub mod vec_change_set;

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
