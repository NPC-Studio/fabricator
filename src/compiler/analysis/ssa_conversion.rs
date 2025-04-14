use std::collections::{hash_map, HashMap, HashSet};

use crate::{
    compiler::{
        graph::{dfs::depth_first_search_with, dominators::Dominators},
        ir,
    },
    util::{index_containers::IndexSet, typed_id_map::SecondaryMap},
};

use super::vec_change_set::VecChangeSet;

/// Convert uses of IR variables into SSA, possibly by inserting Phi and Upsilon instructions.
///
/// This will convert uses of variables in reachable blocks, blocks that are never executed will
/// not be modified.
///
/// All uses of variables without an assignment are converted into `Undefined`. This includes
/// `GetVariable` instructions used before a `SetVariable` to the same variable, and also any
/// required `Upsilon` instructions without a previous assignment.
///
/// The resulting phi placement will be *minimal* but not *pruned*.
pub fn convert_to_ssa<S>(ir: &mut ir::Function<S>) {
    let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());

    let mut assigning_blocks: SecondaryMap<ir::Variable, HashSet<ir::BlockId>> =
        SecondaryMap::new();

    // We don't do any SSA conversion of unreachable blocks.
    for block_id in dominators.topological_order() {
        let block = &ir.blocks[block_id];
        for &inst_id in &block.instructions {
            if let &ir::Instruction::SetVariable(variable, _) = &ir.instructions[inst_id] {
                let blocks = assigning_blocks.get_or_insert_default(variable);
                blocks.insert(block_id);
            }
        }
    }

    // Add phi functions by using dominance frontiers of blocks that write to variables (including
    // inserted phi functions).
    //
    // This algorithm is from Cytron et al. (1991)
    // https://bears.ece.ucsb.edu/class/ece253/papers/cytron91.pdf

    let mut phi_functions: SecondaryMap<ir::BlockId, HashMap<ir::Variable, ir::ShadowVar>> =
        SecondaryMap::new();
    let mut shadow_map: SecondaryMap<ir::ShadowVar, ir::Variable> = SecondaryMap::new();

    let mut work_queue = Vec::new();
    let mut work_added = IndexSet::new();
    for (variable, assigning_blocks) in assigning_blocks.iter() {
        assert!(work_queue.is_empty());
        work_added.clear();

        for &block_id in assigning_blocks {
            work_queue.push(block_id);
            work_added.insert(block_id.index() as usize);
        }

        while let Some(assigning_block_id) = work_queue.pop() {
            for frontier_block_id in dominators.dominance_frontier(assigning_block_id).unwrap() {
                if let hash_map::Entry::Vacant(vacant) = phi_functions
                    .get_or_insert_default(frontier_block_id)
                    .entry(variable)
                {
                    let shadow_var = ir.shadow_vars.insert(());
                    vacant.insert(shadow_var);
                    shadow_map.insert(shadow_var, variable);
                    if work_added.insert(frontier_block_id.index() as usize) {
                        work_queue.push(frontier_block_id);
                    }
                }
            }
        }
    }

    // Actually insert `Phi` instructions for every variable at the beginning of every block that we
    // have determined needs them.
    let mut inst_change_set = VecChangeSet::new();
    for block_id in dominators.topological_order() {
        let block = &mut ir.blocks[block_id];
        if let Some(phi_functions) = phi_functions.get(block_id) {
            for &shadow_var in phi_functions.values() {
                inst_change_set.insert(0, ir.instructions.insert(ir::Instruction::Phi(shadow_var)));
            }
            inst_change_set.apply(&mut block.instructions);
        }
    }

    // Rename all variables, converting into SSA form.
    //
    // This algorithm is also from Cytron et al. (1991)
    // https://bears.ece.ucsb.edu/class/ece253/papers/cytron91.pdf

    let mut current_vars: SecondaryMap<ir::Variable, Vec<ir::InstId>> =
        SecondaryMap::from_iter(assigning_blocks.ids().map(|var| (var, Vec::new())));
    let mut var_stack_bottom: SecondaryMap<ir::BlockId, HashMap<ir::Variable, usize>> =
        SecondaryMap::new();

    // We turn the recursive algorithm from Cytron et al. into an explicit DFS of the dominator
    // tree.
    let start_block = ir.start_block;
    depth_first_search_with(
        &mut (&mut current_vars, &mut var_stack_bottom),
        start_block,
        |(_, _), block_id| dominators.dominance_children(block_id).unwrap(),
        |(current_vars, var_stack_bottom), block_id| {
            let var_stack_bottom = var_stack_bottom.get_or_insert_default(block_id);
            for (var, stack) in current_vars.iter() {
                var_stack_bottom.insert(var, stack.len());
            }

            let block = &mut ir.blocks[block_id];
            for &inst_id in &block.instructions {
                let inst = &mut ir.instructions[inst_id];
                match &*inst {
                    &ir::Instruction::GetVariable(variable) => {
                        if let Some(top) =
                            current_vars.get(variable).and_then(|s| s.last().copied())
                        {
                            *inst = ir::Instruction::Copy(top);
                        } else {
                            // If the current variable has had no assignments, then we replace it
                            // with `Undefined`.
                            *inst = ir::Instruction::Undefined;
                        }
                    }
                    &ir::Instruction::SetVariable(variable, source) => {
                        *inst = ir::Instruction::NoOp;
                        current_vars.get_mut(variable).unwrap().push(source);
                    }
                    &ir::Instruction::Phi(shadow_var) => {
                        // If there is a `Phi` function we did not insert, we ignore it.
                        if let Some(&variable) = shadow_map.get(shadow_var) {
                            current_vars.get_mut(variable).unwrap().push(inst_id);
                        }
                    }
                    _ => {}
                }
            }

            // Loop through every successor block, for every phi function that was inserted into
            // that block, we must insert a matching upsilon.
            for succ in block.exit.successors() {
                if let Some(phi_functions) = phi_functions.get(succ) {
                    for (&variable, &shadow_var) in phi_functions {
                        let var_inst;
                        if let Some(top) =
                            current_vars.get(variable).and_then(|s| s.last().copied())
                        {
                            var_inst = top;
                        } else {
                            // If we don't have a value to add to an `Upsilon`, then we have to
                            // make one to keep the IR well-formed. This was use of a value that was
                            // undefined on this code path, so we set the value to undefined.
                            var_inst = ir.instructions.insert(ir::Instruction::Undefined);
                            block.instructions.push(var_inst);
                        }

                        block.instructions.push(
                            ir.instructions
                                .insert(ir::Instruction::Upsilon(shadow_var, var_inst)),
                        );
                    }
                }
            }
        },
        |(current_vars, var_stack_bottom), block_id| {
            for (&var, &stack_bottom) in &var_stack_bottom[block_id] {
                current_vars.get_mut(var).unwrap().truncate(stack_bottom);
            }
        },
    );
}
