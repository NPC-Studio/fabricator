use fabricator_util::index_containers::IndexSet;

use crate::{graph::dfs::depth_first_search, ir};

/// Remove all `NoOp` instructions and clear instructions not present in any block, then clean all
/// instruction spans for instructions that no longer exist.
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

    ir.spans.retain(|id, _| ir.instructions.contains(id));
}

/// Remove any blocks that are not reachable from the `start_block`.
pub fn clean_unreachable_blocks<S>(ir: &mut ir::Function<S>) {
    let mut reachable_blocks = IndexSet::new();
    depth_first_search(
        ir.start_block,
        |b| {
            reachable_blocks.insert(b.index() as usize);
            ir.blocks[b].exit.successors()
        },
        |_| {},
    );

    ir.blocks
        .retain(|id, _| reachable_blocks.contains(id.index() as usize));
}

pub fn clean_unused_variables<S>(ir: &mut ir::Function<S>) {
    let mut used_variables = IndexSet::new();

    for inst in ir.instructions.values() {
        match inst {
            ir::Instruction::GetVariable(variable) | ir::Instruction::SetVariable(variable, _) => {
                used_variables.insert(variable.index() as usize);
            }
            _ => {}
        }
    }

    for func in ir.functions.values() {
        for &parent_var in func.upvalues.values() {
            used_variables.insert(parent_var.index() as usize);
        }
    }

    ir.variables
        .retain(|id, _| used_variables.contains(id.index() as usize));
    ir.upvalues
        .retain(|id, _| used_variables.contains(id.index() as usize));
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
