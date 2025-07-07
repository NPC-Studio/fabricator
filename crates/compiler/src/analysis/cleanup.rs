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

/// Clean all variables that are never used in any block.
///
/// Any variable instruction counts as a use, as well as `Closure` instructions that reference an
/// upper variable.
pub fn clean_unused_variables<S>(ir: &mut ir::Function<S>) {
    let mut used_variables = IndexSet::new();

    for inst in ir.instructions.values() {
        match *inst {
            ir::Instruction::OpenVariable(var_id)
            | ir::Instruction::GetVariable(var_id)
            | ir::Instruction::SetVariable(var_id, _)
            | ir::Instruction::CloseVariable(var_id) => {
                used_variables.insert(var_id.index() as usize);
            }
            ir::Instruction::Closure(func) => {
                for var in ir.functions[func].variables.values() {
                    // Creating a closure uses every upper variable that the closure closes
                    // over.
                    if let &ir::Variable::Upper(var_id) = var {
                        used_variables.insert(var_id.index() as usize);
                    }
                }
            }
            _ => {}
        }
    }

    ir.variables
        .retain(|id, _| used_variables.contains(id.index() as usize));
}

pub fn clean_unused_shadow_vars<S>(ir: &mut ir::Function<S>) {
    let mut used_shadow_vars = IndexSet::new();

    for inst in ir.instructions.values() {
        match *inst {
            ir::Instruction::Phi(shadow_var) | ir::Instruction::Upsilon(shadow_var, _) => {
                used_shadow_vars.insert(shadow_var.index() as usize);
            }
            _ => {}
        }
    }

    ir.shadow_vars
        .retain(|id, _| used_shadow_vars.contains(id.index() as usize));
}

pub fn clean_unused_this_scopes<S>(ir: &mut ir::Function<S>) {
    let mut used_this_scopes = IndexSet::new();

    for inst in ir.instructions.values() {
        match *inst {
            ir::Instruction::OpenThisScope(this_scope, _)
            | ir::Instruction::CloseThisScope(this_scope) => {
                used_this_scopes.insert(this_scope.index() as usize);
            }
            _ => {}
        }
    }

    ir.this_scopes
        .retain(|id, _| used_this_scopes.contains(id.index() as usize));
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
