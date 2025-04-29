pub mod block_merge;
pub mod cleanup;
pub mod constant_folding;
pub mod dead_code_elim;
pub mod eliminate_copies;
pub mod instruction_liveness;
pub mod shadow_liveness;
pub mod shadow_reduction;
pub mod ssa_conversion;
pub mod vec_change_set;

use crate::compiler::{graph, ir};

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
