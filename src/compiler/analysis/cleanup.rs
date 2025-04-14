use crate::{
    compiler::{graph::dfs::depth_first_search, ir},
    util::index_containers::IndexSet,
};

/// Remove all `NoOp` instructions and clear instructions not present in any block.
pub fn clean_instructions<S>(ir: &mut ir::Function<S>) {
    let mut used_instructions = IndexSet::new();

    for block in ir.blocks.values_mut() {
        block.instructions.retain(|&inst_id| {
            if let ir::Instruction::NoOp = &ir.instructions[inst_id] {
                false
            } else {
                used_instructions.insert(inst_id.index() as usize);
                true
            }
        });
    }

    ir.instructions
        .retain(|id, _| used_instructions.contains(id.index() as usize));
}

/// Remove any blocks that are not reachable from the `start_block`.
pub fn clean_unreachable_blocks<S>(ir: &mut ir::Function<S>) {
    let mut reachable_blocks = IndexSet::new();
    depth_first_search(
        ir.start_block,
        |b| ir.blocks[b].exit.successors(),
        |b| {
            reachable_blocks.insert(b.index() as usize);
        },
        |_| {},
    );

    ir.blocks
        .retain(|id, _| reachable_blocks.contains(id.index() as usize));
}

pub fn clean_unused_functions<S>(ir: &mut ir::Function<S>) {
    let mut used_functions = IndexSet::new();

    for inst in ir.instructions.values() {
        if let &ir::Instruction::Closure(func_id) = inst {
            used_functions.insert(func_id.index() as usize);
        }
    }

    ir.functions
        .retain(|id, _| used_functions.contains(id.index() as usize));
}
