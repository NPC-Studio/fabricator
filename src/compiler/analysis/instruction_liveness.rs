use std::collections::{hash_map, HashMap, HashSet};

use thiserror::Error;

use crate::{
    compiler::{
        graph::{dfs::dfs_post_order, dominators::Dominators},
        ir,
    },
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum InstructionVerificationError {
    #[error("instruction appears multiple times")]
    InstructionReused,
    #[error("instruction does not dominate its uses")]
    UseNotDominated,
    #[error("instruction source is of type Void")]
    SourceIsVoid,
}

/// The liveness range of an instruction within a block, specified in block instruction indexes.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InstructionLivenessRange {
    /// The instruction index in the block at which the instruction becomes live.
    ///
    /// If `None`, then this instruction is defined in a predecessor to this block.
    ///
    /// If this is `Some`, then this index will contain the instruction itself. All uses of an
    /// instruction must come after it, so the beginning of the range is always the instruction
    /// itself.
    pub start: Option<usize>,

    /// The instruction index in the block after which the instruction becomes dead.
    ///
    /// If `None`, then this instruction is used by a successor to this block.
    ///
    /// This may return the index for the special `Exit` instruction, which is 1 past the end of the
    /// normal block instruction list.
    pub end: Option<usize>,
}

impl InstructionLivenessRange {
    pub fn is_live(&self, inst_index: usize) -> bool {
        self.start.is_none_or(|start| inst_index >= start)
            && self.end.is_none_or(|end| inst_index <= end)
    }
}

#[derive(Debug, Default)]
pub struct InstructionLiveness {
    block_liveness: SecondaryMap<ir::BlockId, HashMap<ir::InstId, InstructionLivenessRange>>,
    live_blocks_for_instruction: SecondaryMap<ir::InstId, HashSet<ir::BlockId>>,
}

impl InstructionLiveness {
    /// Compute instruction liveness ranges for every block in the given IR.
    ///
    /// This also verifies all instructions within the IR and their use, namely that:
    ///   1) No instruction appears more than once within all blocks.
    ///   2) Every instruction and branch source is dominated by its definition.
    ///   3) No instruction source is of type `Void`.
    pub fn compute<S>(ir: &ir::Function<S>) -> Result<Self, InstructionVerificationError> {
        let dominators =
            Dominators::compute(ir.start_block, |b| ir.parts.blocks[b].exit.successors());

        let mut block_definitions: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> = ir
            .parts
            .blocks
            .ids()
            .map(|id| (id, HashSet::new()))
            .collect();

        let mut block_uses: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> = ir
            .parts
            .blocks
            .ids()
            .map(|id| (id, HashSet::new()))
            .collect();

        let mut inst_positions = SecondaryMap::new();
        for (block_id, block) in ir.parts.blocks.iter() {
            let block_definitions = block_definitions.get_mut(block_id).unwrap();
            let block_uses = block_uses.get_mut(block_id).unwrap();

            for (i, &inst_id) in block.instructions.iter().enumerate() {
                if inst_positions.insert(inst_id, (block_id, i)).is_some() {
                    return Err(InstructionVerificationError::InstructionReused);
                }

                let inst = &ir.parts.instructions[inst_id];

                if inst.has_value() {
                    block_definitions.insert(inst_id);
                }

                for source_inst_id in inst.sources() {
                    block_uses.insert(source_inst_id);
                }
            }
        }

        for (block_id, block) in ir.parts.blocks.iter() {
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                let inst = &ir.parts.instructions[inst_id];

                for source_inst in inst.sources() {
                    if !ir.parts.instructions[source_inst].has_value() {
                        return Err(InstructionVerificationError::SourceIsVoid);
                    }

                    let (source_block_id, source_index) = inst_positions[source_inst];
                    if block_id == source_block_id {
                        if source_index >= inst_index {
                            return Err(InstructionVerificationError::UseNotDominated);
                        }
                    } else {
                        if !dominators.dominates(source_block_id, block_id).unwrap() {
                            return Err(InstructionVerificationError::UseNotDominated);
                        }
                    }
                }
            }

            if let Some(cond) = block.exit.cond_source() {
                let (cond_block, _) = inst_positions[cond];
                if !dominators.dominates(cond_block, block_id).unwrap() {
                    return Err(InstructionVerificationError::UseNotDominated);
                }
            }
        }

        // Backwards liveness analysis.
        //
        // We iterate in DFS post-order so that this algorithm converges faster.
        //
        // I can't find the original source for this algorithm, but it is in lecture notes here:
        // https://www.cs.mcgill.ca/~cs520/2021/slides/16-liveness.pdf

        let post_order = dfs_post_order(ir.start_block, |id| ir.parts.blocks[id].exit.successors());

        let mut live_in: SecondaryMap<ir::BlockId, HashSet<ir::InstId>> =
            post_order.iter().map(|&id| (id, HashSet::new())).collect();
        let mut live_out = live_in.clone();

        let mut changed = true;
        while changed {
            changed = false;

            for &block_id in &post_order {
                let block_live_out = live_out.get_mut(block_id).unwrap();

                // Every variable that is live in in a successor must be live out in this block.
                for succ in ir.parts.blocks[block_id].exit.successors() {
                    for &inst_id in &live_in[succ] {
                        changed |= block_live_out.insert(inst_id);
                    }
                }

                let block_live_in = live_in.get_mut(block_id).unwrap();
                let definitions = &block_definitions[block_id];

                // Every used instruction which is not defined in this block must be live in.
                for &inst_id in &block_uses[block_id] {
                    if !definitions.contains(&inst_id) {
                        changed |= block_live_in.insert(inst_id);
                    }
                }

                // Every live out instruction that is not defined in this block must also be live
                // in.
                for &inst_id in &*block_live_out {
                    if !definitions.contains(&inst_id) {
                        changed |= block_live_in.insert(inst_id);
                    }
                }
            }
        }

        // Find the concrete instruction ranges using the computed live-in / live-out sets.

        let mut block_ranges = SecondaryMap::new();
        for &block_id in &post_order {
            let block = &ir.parts.blocks[block_id];
            let live_in = &live_in[block_id];
            let live_out = &live_out[block_id];

            let mut ranges: HashMap<ir::InstId, InstructionLivenessRange> = HashMap::new();
            let mut mark_use = |inst_id, index| match ranges.entry(inst_id) {
                hash_map::Entry::Occupied(mut occupied) => {
                    occupied.get_mut().end = Some(index);
                }
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(InstructionLivenessRange {
                        start: Some(index),
                        end: Some(index),
                    });
                }
            };

            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                let inst = &ir.parts.instructions[inst_id];

                if inst.has_value() {
                    mark_use(inst_id, inst_index);
                }

                for source_inst_id in inst.sources() {
                    mark_use(source_inst_id, inst_index);
                }
            }

            if let Some(source_inst_id) = block.exit.cond_source() {
                mark_use(source_inst_id, block.instructions.len());
            }

            for &inst_id in live_in {
                match ranges.entry(inst_id) {
                    hash_map::Entry::Occupied(mut occupied) => {
                        occupied.get_mut().start = None;
                    }
                    hash_map::Entry::Vacant(vacant) => {
                        assert!(
                            live_out.contains(&inst_id),
                            "unused instruction in live-in set must be in live-out set"
                        );
                        vacant.insert(InstructionLivenessRange {
                            start: None,
                            end: None,
                        });
                    }
                }
            }

            for &inst_id in live_out {
                ranges
                    .get_mut(&inst_id)
                    .expect("unused instruction in live-out set must be in live-in set")
                    .end = None;
            }

            block_ranges.insert(block_id, ranges);
        }

        // We verified that every instruction dominates is uses, so the start block should have no
        // live-in instructions.
        assert_eq!(
            block_ranges[ir.start_block]
                .values()
                .filter(|r| r.start.is_none())
                .count(),
            0
        );

        let mut live_blocks_for_instruction = SecondaryMap::new();
        for (block_id, ranges) in block_ranges.iter() {
            for &inst_id in ranges.keys() {
                live_blocks_for_instruction
                    .get_or_insert_with(inst_id, HashSet::new)
                    .insert(block_id);
            }
        }

        Ok(Self {
            block_liveness: block_ranges,
            live_blocks_for_instruction,
        })
    }

    /// Returns all instructions live in any block
    pub fn live_instructions(&self) -> impl Iterator<Item = ir::InstId> + '_ {
        self.live_blocks_for_instruction.ids()
    }

    /// Returns all blocks and ranges within that block in which an instruction is live.
    pub fn live_ranges(
        &self,
        inst_id: ir::InstId,
    ) -> impl Iterator<Item = (ir::BlockId, InstructionLivenessRange)> + '_ {
        self.live_blocks_for_instruction
            .get(inst_id)
            .into_iter()
            .flatten()
            .map(move |&block_id| (block_id, self.block_liveness[block_id][&inst_id]))
    }

    /// Returns all instructions that are live anywhere within the given block.
    pub fn live_for_block(
        &self,
        block_id: ir::BlockId,
    ) -> impl Iterator<Item = (ir::InstId, InstructionLivenessRange)> + '_ {
        self.block_liveness
            .get(block_id)
            .into_iter()
            .flatten()
            .map(move |(&inst_id, &range)| (inst_id, range))
    }

    /// Returns the liveness range for a single instruction in the given block, if it is live there.
    pub fn live_range_in_block(
        &self,
        block_id: ir::BlockId,
        inst_id: ir::InstId,
    ) -> Option<InstructionLivenessRange> {
        Some(*self.block_liveness.get(block_id)?.get(&inst_id)?)
    }
}
