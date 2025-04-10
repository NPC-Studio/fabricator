use crate::{
    compiler::{graph::dfs::depth_first_search, ir},
    util::index_containers::IndexSet,
};

/// Remove all `NoOp` instructions and clear instructions not present in any block.
pub fn clean_instructions<S>(ir: &mut ir::Function<S>) {
    let mut used_instructions = IndexSet::new();

    for index in 0..ir.parts.blocks.index_upper_bound() {
        if let Some(block_id) = ir.parts.blocks.id_for_index(index) {
            let block = &mut ir.parts.blocks[block_id];
            block.instructions.retain(|&inst_id| {
                if let ir::Instruction::NoOp = &ir.parts.instructions[inst_id] {
                    false
                } else {
                    used_instructions.insert(inst_id.index() as usize);
                    true
                }
            });
        }
    }

    for index in 0..ir.parts.instructions.index_upper_bound() {
        if let Some(id) = ir.parts.instructions.id_for_index(index) {
            if !used_instructions.contains(index as usize) {
                ir.parts.instructions.remove(id);
            }
        }
    }
}

/// Remove any blocks that are not reachable from the `start_block`.
pub fn clean_unreachable_blocks<S>(ir: &mut ir::Function<S>) {
    let mut reachable_blocks = IndexSet::new();
    depth_first_search(
        ir.start_block,
        |b| ir.parts.blocks[b].exit.successors(),
        |b| {
            reachable_blocks.insert(b.index() as usize);
        },
        |_| {},
    );

    for index in 0..ir.parts.blocks.index_upper_bound() {
        if let Some(id) = ir.parts.blocks.id_for_index(index) {
            if !reachable_blocks.contains(index as usize) {
                ir.parts.blocks.remove(id);
            }
        }
    }
}
