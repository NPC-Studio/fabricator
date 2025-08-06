pub mod block_simplification;
pub mod cleanup;
pub mod constant_folding;
pub mod dead_code_elim;
pub mod eliminate_copies;
pub mod instruction_liveness;
pub mod nested_scope_liveness;
pub mod scope_liveness;
pub mod shadow_liveness;
pub mod shadow_reduction;
pub mod ssa_conversion;
pub mod variable_liveness;
pub mod vec_change_set;
pub mod verify_arguments;
pub mod verify_references;

use crate::{graph, ir};

impl graph::Node for ir::BlockId {
    fn index(&self) -> usize {
        self.index() as usize
    }
}
