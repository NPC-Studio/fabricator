use std::collections::{HashMap, HashSet};

use crate::compiler::{graph::predecessors::Predecessors, ir};

/// Merge blocks where a block A unconditionally jumps to block B and A only has a successor and
/// block B only has a single predecessor.
pub fn merge_blocks<S>(ir: &mut ir::Function<S>) {
    let predecessors = Predecessors::compute(ir.parts.blocks.ids(), |b| {
        ir.parts.blocks[b].exit.successors()
    });

    let mut merge_next: HashMap<ir::BlockId, ir::BlockId> = HashMap::new();
    let mut merge_tails: HashSet<ir::BlockId> = HashSet::new();

    for (block_id, block) in ir.parts.blocks.iter() {
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
            ir::Exit::Return { .. } => {}
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
                    .extend(&ir.parts.blocks[block_id].instructions)
            }

            let &first = merges.first().unwrap();
            let &last = merges.last().unwrap();
            let merged_block_id = ir.parts.blocks.insert(merged_block);

            for pred in predecessors.get(first) {
                for succ in ir.parts.blocks[pred].exit.successors_mut() {
                    if *succ == first {
                        *succ = merged_block_id;
                    }
                }
            }

            if ir.start_block == first {
                ir.start_block = merged_block_id;
            }

            let exit = ir.parts.blocks[last].exit;
            ir.parts.blocks[merged_block_id].exit = exit;

            for &block_id in &merges {
                ir.parts.blocks.remove(block_id);
            }
        }
    }
}
