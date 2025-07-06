pub mod block_merge;
pub mod cleanup;
pub mod constant_folding;
pub mod dead_code_elim;
pub mod eliminate_copies;
pub mod instruction_liveness;
pub mod shadow_liveness;
pub mod shadow_reduction;
pub mod ssa_conversion;
pub mod variable_liveness;
pub mod vec_change_set;
pub mod verify_scopes;

use crate::{graph, ir};

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
