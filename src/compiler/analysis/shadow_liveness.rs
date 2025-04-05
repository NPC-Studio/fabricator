use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    compiler::{graph::dfs::dfs_post_order, ir},
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum PhiUpsilonVerificationError {
    #[error("shadow variable used by multiple phi instructions")]
    ShadowReused,
    #[error("not all paths to a phi instruction contain an upsilon to define the shadow variable")]
    ShadowUndef,
}

/// The liveness range of a shadow variable within a block leading up to a `Phi` instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ShadowIncomingRange {
    /// The instruction index in the block at which the shadow variable becomes live.
    ///
    /// If this is `None`, then the shadow variable becomes live in a predecessor to this block.
    ///
    /// If this is `Some`, then this instruction is always an `Upsilon`. Since we are in the same
    /// block as the `Phi` instruction, the upsilon / phi pair are local to this block and this is
    /// the *entire* range of the shadow variable (all other `Upsilon` instructions are dead).
    start: Option<usize>,

    /// The instruction index in the block at which the shadow variable becomes dead (if there is
    /// not an overlapping outgoing range).
    ///
    /// This is always the single `Phi` instruction containing the shadow variable.
    end: usize,
}

/// The liveness range of a shadow variable within a block going from an `Upsilon` instruction out
/// of the block to block successor.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ShadowOutgoingRange {
    /// The instruction index in the block at which the shadow variable becomes live.
    ///
    /// If this is `Some`, then this is an `Upsilon` instruction that may be used by a `Phi`
    /// instruction that is in a successor block.
    ///
    /// If this is `None`, then there is an `Upsilon` instruction in a predecessor which may be used
    /// by a `Phi` instruction in a successor block.
    pub start: Option<usize>,
}

/// The liveness range of a shadow variable within a block.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct ShadowLivenessRange {
    /// If an incoming range is set, this block will always contain the `Phi` instruction, and this
    /// describes the `Phi` instruction liveness range leading backwards from the `Phi` instruction.
    pub incoming_range: Option<ShadowIncomingRange>,

    /// If an outgoing range is set, then a successor block contains the `Phi` instruction and its
    /// shadow variable's lifetime overlaps this block.
    ///
    /// The incoming and outging range fields are NOT exclusive in a particular block! A block may
    /// have *both* an incoming and an outgoing range if it contains a `Phi` instruction and is in a
    /// loop (in a block which is its own successor). The incoming range will define the range from
    /// the block start to the `Phi` instruction, and the outgoing range describes the range from
    /// the block exit backwards to the closest `Upsilon` instruction.
    ///
    /// It is also possible for the incoming and outgoing ranges to *overlap*, if the closest
    /// `Upsilon` instruction is before the `Phi` instruction (or in a predecessor block).
    pub outgoing_range: Option<ShadowOutgoingRange>,
}

/// If a shadow variable is live anywhere within a block, it will have an entry here.
pub type BlockShadowLiveness = HashMap<ir::ShadowVarId, ShadowLivenessRange>;

#[derive(Debug, Default)]
pub struct ShadowLiveness(SecondaryMap<ir::BlockId, BlockShadowLiveness>);

impl ShadowLiveness {
    /// Compute shadow variable liveness ranges for every block in the given IR.
    ///
    /// This also verifies all phi and upsilon instructions within the IR and their use, namely
    /// that:
    ///   1) No shadow variable is used by more than one `Phi` instruction.
    ///   2) All paths from `start_block` to a `Phi` instructions have an `Upsilon` which writes to
    ///      the `Phi`'s shadow variable.
    ///
    /// Certain `Upsilon` instructions may be "dead" within the IR, and this is not considered
    /// invalid. A "dead" `Upsilon` instruction will exist *outside* of the reported shadow variable
    /// range, and this should be taken to mean that the given `Upsilon` instruction cannot possibly
    /// have an effect on the value of a future `Phi`. Dead `Upsilon` instructions will either be
    /// post-dominated by another `Upsilon` instruction closer to the `Phi` that they write to, or
    /// there is no path in the CFG from the `Upsilon` to the `Phi` it writes to.
    pub fn compute<S>(ir: &ir::Function<S>) -> Result<Self, PhiUpsilonVerificationError> {
        // Collect the location of all `Upsilon` instructions per block in ascending instruction
        // position order.

        let mut upsilon_instructions: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVarId, Vec<usize>>,
        > = ir
            .parts
            .blocks
            .ids()
            .map(|id| (id, HashMap::new()))
            .collect();

        for (block_id, block) in ir.parts.blocks.iter() {
            let upsilon_map = upsilon_instructions.get_mut(block_id).unwrap();
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                if let &ir::Instruction::Upsilon(shadow_var_id, _) = &ir.parts.instructions[inst_id]
                {
                    let upsilon_list = upsilon_map.entry(shadow_var_id).or_default();
                    upsilon_list.push(inst_index);
                }
            }
        }

        // Compute the incoming ranges for the blocks containing phi instructions

        let mut shadow_vars: SecondaryMap<ir::ShadowVarId, ()> = SecondaryMap::new();
        let mut incoming_ranges: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVarId, ShadowIncomingRange>,
        > = ir
            .parts
            .blocks
            .ids()
            .map(|id| (id, HashMap::new()))
            .collect();

        for (block_id, block) in ir.parts.blocks.iter() {
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                if let &ir::Instruction::Phi(shadow_var_id) = &ir.parts.instructions[inst_id] {
                    if shadow_vars.insert(shadow_var_id, ()).is_some() {
                        return Err(PhiUpsilonVerificationError::ShadowReused);
                    }

                    let mut range = ShadowIncomingRange {
                        start: None,
                        end: inst_index,
                    };

                    // Find the farthest `Upsilon` instruction for this shadow variable in this
                    // block that is earlier than the `Phi` instruction. If one exists, that is the
                    // incoming range start.
                    for &i in upsilon_instructions[block_id]
                        .get(&shadow_var_id)
                        .into_iter()
                        .flatten()
                    {
                        if i < inst_index {
                            range.start = Some(i);
                        }
                    }

                    incoming_ranges
                        .get_mut(block_id)
                        .unwrap()
                        .insert(shadow_var_id, range);
                }
            }
        }

        let mut outgoing_ranges: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVarId, ShadowOutgoingRange>,
        > = ir
            .parts
            .blocks
            .ids()
            .map(|id| (id, HashMap::new()))
            .collect();

        let post_order = dfs_post_order(ir.start_block, |id| ir.parts.blocks[id].exit.successors());

        let mut live_in: SecondaryMap<ir::BlockId, HashSet<ir::ShadowVarId>> =
            post_order.iter().map(|&id| (id, HashSet::new())).collect();
        let mut live_out = live_in.clone();

        for (block_id, shadow_vars) in incoming_ranges.iter() {
            for (&shadow_var_id, range) in shadow_vars.iter() {
                if range.start.is_none() {
                    live_in.get_mut(block_id).unwrap().insert(shadow_var_id);
                }
            }
        }

        let mut changed = true;
        while changed {
            changed = false;

            for &block_id in &post_order {
                let block_live_out = live_out.get_mut(block_id).unwrap();

                // Every variable that is live in in a successor must be live out in this block.
                for succ in ir.parts.blocks[block_id].exit.successors() {
                    for &shadow_var_id in &live_in[succ] {
                        changed |= block_live_out.insert(shadow_var_id);
                    }
                }

                let block_live_in = live_in.get_mut(block_id).unwrap();
                let block_outgoing_ranges = outgoing_ranges.get_mut(block_id).unwrap();

                for &shadow_var_id in &*block_live_out {
                    if !block_outgoing_ranges.contains_key(&shadow_var_id) {
                        // The outgoing range for this shadow variable starts at the last `Upsilon`
                        // instruction in this block that sets it, if it exists.
                        let range = ShadowOutgoingRange {
                            start: upsilon_instructions[block_id]
                                .get(&shadow_var_id)
                                .and_then(|l| l.last().copied()),
                        };
                        block_outgoing_ranges.insert(shadow_var_id, range);

                        // If there was no upsilon for this shadow variable in this block, then it
                        // must be in a predecessor block.
                        if range.start.is_none() {
                            changed |= block_live_in.insert(shadow_var_id);
                        }
                    }
                }
            }
        }

        if !live_in[ir.start_block].is_empty() {
            return Err(PhiUpsilonVerificationError::ShadowUndef);
        }

        Ok(Self(
            post_order
                .into_iter()
                .map(|block_id| {
                    let mut block_liveness = BlockShadowLiveness::new();

                    for (&shadow_var_id, &incoming_range) in incoming_ranges[block_id].iter() {
                        block_liveness
                            .entry(shadow_var_id)
                            .or_default()
                            .incoming_range = Some(incoming_range);
                    }
                    for (&shadow_var_id, &outgoing_range) in outgoing_ranges[block_id].iter() {
                        block_liveness
                            .entry(shadow_var_id)
                            .or_default()
                            .outgoing_range = Some(outgoing_range);
                    }

                    (block_id, block_liveness)
                })
                .collect(),
        ))
    }

    /// Get the shadow variable liveness information for the given block.
    ///
    /// If a block is dead (unreachable from the IR start block), this return `None`.
    pub fn block_shadow_liveness(&self, block_id: ir::BlockId) -> Option<&BlockShadowLiveness> {
        self.0.get(block_id)
    }
}
