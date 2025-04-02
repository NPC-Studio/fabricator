use std::collections::{HashMap, HashSet};

use arrayvec::ArrayVec;

use crate::{
    compiler::{graph::dfs::dfs_post_order, ir},
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Default)]
pub struct BlockLiveness {
    /// The set of all `InstId`s that are live on entry to this block.
    pub live_in: HashSet<ir::InstId>,

    /// The union of all `InstId`s that are live on any exit from this block.
    pub live_out: HashSet<ir::InstId>,

    /// For each instruction position in this block, list the instructions that are no longer used
    /// *after* this instruction.
    ///
    /// For each instruction position in the basic block, this will contain a list of instructions
    /// that are:
    ///   1) Defined in this block or present in `live_in` set.
    ///   2) Used by the block instruction located in this position.
    ///   3) Not used by any following instruction in the basic block.
    ///   4) Not in the `live_out` set.
    ///
    /// Or in other words, this lists every instruction whose lifetime "dies" after the given
    /// instruction position.
    ///
    /// Sources to the special `Exit` instruction are included in this set, and will be at index
    /// `block.instructions.len()`.
    pub endpoints: HashMap<usize, ArrayVec<ir::InstId, { ir::MAX_INSTRUCTION_SOURCES }>>,
}

pub fn compute<S>(function: &ir::Function<S>) -> SecondaryMap<ir::BlockId, BlockLiveness> {
    let post_order = dfs_post_order(function.start_block, |id| {
        function.parts.blocks[id].exit.successors()
    });

    let mut definitions: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> =
        post_order.iter().map(|&id| (id, HashSet::new())).collect();

    let mut uses: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> =
        post_order.iter().map(|&id| (id, HashSet::new())).collect();

    for &block_id in &post_order {
        let block_definitions = definitions.get_mut(block_id).unwrap();
        let block_uses = uses.get_mut(block_id).unwrap();

        for &inst_id in &function.parts.blocks[block_id].instructions {
            let inst = &function.parts.instructions[inst_id];

            if inst.has_value() {
                block_definitions.insert(inst_id);
            }

            for source_inst_id in inst.sources() {
                block_uses.insert(source_inst_id);
            }
        }
    }

    let mut live_in: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> =
        post_order.iter().map(|&id| (id, HashSet::new())).collect();
    let mut live_out = live_in.clone();

    // Backwards liveness analysis.
    //
    // We iterate in DFS post-order so that this algorithm converges faster.
    //
    // I can't find the original source for this algorithm, but it is in lecture notes here:
    // https://www.cs.mcgill.ca/~cs520/2021/slides/16-liveness.pdf
    let mut changed = true;
    while changed {
        changed = false;

        for &block_id in &post_order {
            let block_live_out = live_out.get_mut(block_id).unwrap();

            // Every variable that is live in in a successor must be live out in this block.
            for succ in function.parts.blocks[block_id].exit.successors() {
                for &inst_id in &live_in[succ] {
                    changed |= block_live_out.insert(inst_id);
                }
            }

            let block_live_in = live_in.get_mut(block_id).unwrap();
            let block_definitions = &definitions[block_id];

            // Every used instruction which is not defined in this block must be live in.
            for &inst_id in &uses[block_id] {
                if !block_definitions.contains(&inst_id) {
                    changed |= block_live_in.insert(inst_id);
                }
            }

            // Every live out instruction that is not defined in this block must also be live in.
            for &inst_id in &*block_live_out {
                if !block_definitions.contains(&inst_id) {
                    changed |= block_live_in.insert(inst_id);
                }
            }
        }
    }

    // For each variable that is used within a block and not in its `live_out` set, find the final
    // use of this instruction.
    let mut endpoints = SecondaryMap::new();
    for &block_id in &post_order {
        let block = &function.parts.blocks[block_id];
        let block_live_out = &live_out[block_id];

        let mut last_use_position = HashMap::new();
        for (pos, inst_id) in block.instructions.iter().copied().enumerate() {
            let inst = &function.parts.instructions[inst_id];

            // If an instruction produces a value, then that value must at least die here (if it is
            // not used later).
            if inst.has_value() && !block_live_out.contains(&inst_id) {
                last_use_position.insert(inst_id, pos);
            }

            for source_inst_id in inst.sources() {
                if !block_live_out.contains(&source_inst_id) {
                    last_use_position.insert(source_inst_id, pos);
                }
            }

            if let Some(source_inst_id) = block.exit.cond_source() {
                last_use_position.insert(source_inst_id, block.instructions.len());
            }
        }

        let mut block_endpoints: HashMap<
            usize,
            ArrayVec<ir::InstId, { ir::MAX_INSTRUCTION_SOURCES }>,
        > = HashMap::new();
        for (inst_id, pos) in last_use_position {
            block_endpoints.entry(pos).or_default().push(inst_id);
        }

        endpoints.insert(block_id, block_endpoints);
    }

    post_order
        .iter()
        .map(|&id| {
            (
                id,
                BlockLiveness {
                    live_in: live_in.remove(id).unwrap(),
                    live_out: live_out.remove(id).unwrap(),
                    endpoints: endpoints.remove(id).unwrap(),
                },
            )
        })
        .collect()
}
