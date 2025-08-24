use std::collections::{HashMap, HashSet};

use crate::{graph::predecessors::Predecessors, ir};

/// Merge blocks where a block A unconditionally jumps to block B and A only has B as its single
/// successor and block B only has A as its single predecessor.
pub fn merge_blocks<S>(ir: &mut ir::Function<S>) {
    let predecessors = Predecessors::compute(ir.blocks.ids(), |b| ir.blocks[b].exit.successors());

    let mut merge_next: HashMap<ir::BlockId, ir::BlockId> = HashMap::new();
    let mut merge_tails: HashSet<ir::BlockId> = HashSet::new();

    for (block_id, block) in ir.blocks.iter() {
        let mut merge = |target: ir::BlockId| {
            let mut preds = predecessors.get(target);
            if preds.len() == 1 {
                assert_eq!(preds.next().unwrap(), block_id);
                merge_next.insert(block_id, target);
                merge_tails.insert(target);
            }
        };

        match block.exit {
            ir::Exit::Jump(target) => {
                merge(target);
            }
            ir::Exit::Branch {
                if_false, if_true, ..
            } => {
                if if_false == if_true {
                    merge(if_false);
                }
            }
            ir::Exit::Return { .. } | ir::Exit::Throw(_) => {}
        }
    }

    let mut merges = Vec::new();
    for (&prev, &next) in &merge_next {
        if !merge_tails.contains(&prev) {
            merges.clear();
            merges.push(prev);
            merges.push(next);

            let mut next = next;
            while let Some(&after) = merge_next.get(&next) {
                merges.push(after);
                next = after;
            }

            let mut merged_block = ir::Block::default();

            for &block_id in &merges {
                merged_block
                    .instructions
                    .append(&mut ir.blocks[block_id].instructions);
            }

            let &first = merges.first().unwrap();
            let &last = merges.last().unwrap();

            merged_block.exit = ir.blocks[last].exit;

            ir.blocks[first] = merged_block;
        }
    }
}

/// Change block branch exits which jump to the same blocks into a jump exit.
pub fn block_branch_to_jump<S>(ir: &mut ir::Function<S>) {
    for block in ir.blocks.values_mut() {
        match block.exit {
            ir::Exit::Branch {
                if_false, if_true, ..
            } if if_false == if_true => {
                block.exit = ir::Exit::Jump(if_false);
            }
            _ => {}
        }
    }
}

/// For all empty blocks, try to redirect exits for blocks which jump to them.
pub fn redirect_empty_blocks<S>(ir: &mut ir::Function<S>) {
    // Gather a list of every empty block and a map from empty blocks' `BlockId` to `Exit`.
    let mut empty_block_redirects = HashMap::new();
    for (block_id, block) in ir.blocks.iter() {
        if block.instructions.is_empty() {
            empty_block_redirects.insert(block_id, block.exit);
        }
    }

    // A set to detect loops in the chain of empty jump targets.
    let mut encountered_jump_targets: HashSet<ir::BlockId> = HashSet::new();

    // Resolve all chains of jumps in the map of empty block exits so that each jump is the furthest
    // in the chain.
    for block_id in empty_block_redirects.keys().copied().collect::<Vec<_>>() {
        encountered_jump_targets.clear();
        encountered_jump_targets.insert(block_id);

        let mut next_block = block_id;
        while let Some(&empty_exit) = empty_block_redirects.get(&next_block) {
            empty_block_redirects.insert(block_id, empty_exit);

            let ir::Exit::Jump(target) = empty_exit else {
                break;
            };
            next_block = target;

            if !encountered_jump_targets.insert(next_block) {
                // If we encounter a loop, then modify the first empty block's exit to be itself
                // (the shortest infinite loop) and update the redirection map accordingly.
                ir.blocks[block_id].exit = ir::Exit::Jump(block_id);
                empty_block_redirects.insert(block_id, ir::Exit::Jump(block_id));
                break;
            }
        }
    }

    // Finally, replace every block exit which jumps to an empty block with that block's (now
    // furthest) target.

    for block in ir.blocks.values_mut() {
        match &mut block.exit {
            &mut ir::Exit::Jump(target) => {
                if let Some(&exit) = empty_block_redirects.get(&target) {
                    block.exit = exit;
                }
            }
            ir::Exit::Branch {
                if_false, if_true, ..
            } => {
                if let Some(&ir::Exit::Jump(target)) = empty_block_redirects.get(if_false) {
                    *if_false = target;
                }
                if let Some(&ir::Exit::Jump(target)) = empty_block_redirects.get(if_true) {
                    *if_true = target;
                }
            }
            ir::Exit::Return { .. } | ir::Exit::Throw(_) => {}
        }
    }
}
