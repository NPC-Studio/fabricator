use std::collections::{HashMap, HashSet, hash_map};

use fabricator_util::typed_id_map::SecondaryMap;
use thiserror::Error;

use crate::{graph::dfs::dfs_post_order, ir};

#[derive(Debug, Error)]
pub enum ShadowVerificationError {
    #[error("shadow variable used by multiple phi instructions")]
    ShadowReused,
    #[error("not all paths to a phi instruction contain an upsilon to define the shadow variable")]
    ShadowUndef,
}

/// The liveness range of a shadow variable leading up to a `Phi` instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ShadowIncomingRange {
    /// The instruction index in the block at which the shadow variable becomes live.
    ///
    /// If this is `None`, then the shadow variable becomes live in a predecessor to this block.
    ///
    /// If this is `Some`, then this instruction is always an `Upsilon`. Since we are in the same
    /// block as the `Phi` instruction, the upsilon / phi pair are local to this block and this is
    /// the *entire* range of the shadow variable (all other `Upsilon` instructions are dead).
    pub start: Option<usize>,

    /// The instruction index in the block at which the shadow variable becomes dead (if there is
    /// not an overlapping outgoing range).
    ///
    /// This is always the single `Phi` instruction containing the shadow variable.
    pub end: usize,
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
///
/// For each `Phi` instruction in well-formed IR, there will be exactly one incoming range and zero
/// or more outgoing ranges.
///
/// All live `Upsilon` instructions will be at the `start` field of either `incoming_range` or
/// `outgoing_range`, and the `Phi` instruction will be at the `incoming_range.end` field in the one
/// block containing the `Phi` instruction.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct ShadowLivenessRange {
    /// If an incoming range is set, this block will always contain the single `Phi` instruction for
    /// this shadow variable, and this describes the `Phi` instruction liveness range leading to the
    /// `Phi` instruction.
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

#[derive(Debug)]
pub struct ShadowLiveness {
    block_liveness: SecondaryMap<ir::BlockId, HashMap<ir::ShadowVar, ShadowLivenessRange>>,
    live_blocks_for_shadow_var: SecondaryMap<ir::ShadowVar, HashSet<ir::BlockId>>,
}

impl ShadowLiveness {
    /// Compute shadow variable liveness ranges for every block in the given IR. A shadow variable
    /// is considered "live" when a `Phi` instruction may in the future read a value that has been
    /// written by an `Upsilon`.
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
    pub fn compute<S>(ir: &ir::Function<S>) -> Result<Self, ShadowVerificationError> {
        let post_order = dfs_post_order(ir.start_block, |id| ir.blocks[id].exit.successors());

        // Collect the location of all `Upsilon` instructions per block in ascending instruction
        // position order.

        let mut upsilon_instructions: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVar, Vec<usize>>,
        > = post_order.iter().map(|&id| (id, HashMap::new())).collect();

        for &block_id in &post_order {
            let upsilon_map = upsilon_instructions.get_mut(block_id).unwrap();
            for (inst_index, &inst_id) in ir.blocks[block_id].instructions.iter().enumerate() {
                if let &ir::Instruction::Upsilon(shadow_var, _) = &ir.instructions[inst_id] {
                    let upsilon_list = upsilon_map.entry(shadow_var).or_default();
                    upsilon_list.push(inst_index);
                }
            }
        }

        // Compute the incoming ranges for the blocks containing phi instructions

        let mut shadow_vars: SecondaryMap<ir::ShadowVar, ()> = SecondaryMap::new();
        let mut incoming_ranges: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVar, ShadowIncomingRange>,
        > = post_order.iter().map(|&id| (id, HashMap::new())).collect();

        for &block_id in &post_order {
            for (inst_index, &inst_id) in ir.blocks[block_id].instructions.iter().enumerate() {
                if let &ir::Instruction::Phi(shadow_var) = &ir.instructions[inst_id] {
                    if shadow_vars.insert(shadow_var, ()).is_some() {
                        return Err(ShadowVerificationError::ShadowReused);
                    }

                    let mut range = ShadowIncomingRange {
                        start: None,
                        end: inst_index,
                    };

                    // Find the farthest `Upsilon` instruction for this shadow variable in this
                    // block that is earlier than the `Phi` instruction. If one exists, that is the
                    // incoming range start.
                    for &i in upsilon_instructions[block_id]
                        .get(&shadow_var)
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
                        .insert(shadow_var, range);
                }
            }
        }

        let mut outgoing_ranges: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVar, ShadowOutgoingRange>,
        > = post_order.iter().map(|&id| (id, HashMap::new())).collect();

        let mut live_in: SecondaryMap<ir::BlockId, HashSet<ir::ShadowVar>> =
            post_order.iter().map(|&id| (id, HashSet::new())).collect();
        let mut live_out = live_in.clone();

        for (block_id, shadow_vars) in incoming_ranges.iter() {
            for (&shadow_var, range) in shadow_vars.iter() {
                if range.start.is_none() {
                    live_in.get_mut(block_id).unwrap().insert(shadow_var);
                }
            }
        }

        // This is a similar algorithm to the instruction liveness algorithm, but since we are no
        // longer dealing with SSA, `Upsilon` instructions notionally add their shadow variables
        // to the KILL list (the term in traditional liveness analysis for the set of assigned
        // variables, which stops future instructions from needing the variable's old value).

        let mut changed = true;
        while changed {
            changed = false;

            for &block_id in &post_order {
                let block_live_out = live_out.get_mut(block_id).unwrap();

                // Every variable that is live in in a successor must be live out in this block.
                for succ in ir.blocks[block_id].exit.successors() {
                    for &shadow_var in &live_in[succ] {
                        changed |= block_live_out.insert(shadow_var);
                    }
                }

                let block_live_in = live_in.get_mut(block_id).unwrap();
                let block_outgoing_ranges = outgoing_ranges.get_mut(block_id).unwrap();

                for &shadow_var in &*block_live_out {
                    if let hash_map::Entry::Vacant(vacant) = block_outgoing_ranges.entry(shadow_var)
                    {
                        // The outgoing range for this shadow variable starts at the last `Upsilon`
                        // instruction in this block that sets it, if it exists.
                        let range = ShadowOutgoingRange {
                            start: upsilon_instructions[block_id]
                                .get(&shadow_var)
                                .and_then(|l| l.last().copied()),
                        };
                        vacant.insert(range);

                        // If there was no upsilon for this shadow variable in this block, then it
                        // must be in a predecessor block.
                        if range.start.is_none() {
                            changed |= block_live_in.insert(shadow_var);
                        }
                    }
                }
            }
        }

        if !live_in[ir.start_block].is_empty() {
            return Err(ShadowVerificationError::ShadowUndef);
        }

        let mut block_liveness: SecondaryMap<
            ir::BlockId,
            HashMap<ir::ShadowVar, ShadowLivenessRange>,
        > = SecondaryMap::new();
        let mut live_blocks_for_shadow_var: SecondaryMap<ir::ShadowVar, HashSet<ir::BlockId>> =
            SecondaryMap::new();

        for &block_id in &post_order {
            let block_liveness = block_liveness.get_or_insert_default(block_id);

            for (&shadow_var, &incoming_range) in incoming_ranges[block_id].iter() {
                block_liveness.entry(shadow_var).or_default().incoming_range = Some(incoming_range);
                live_blocks_for_shadow_var
                    .get_or_insert_default(shadow_var)
                    .insert(block_id);
            }
            for (&shadow_var, &outgoing_range) in outgoing_ranges[block_id].iter() {
                block_liveness.entry(shadow_var).or_default().outgoing_range = Some(outgoing_range);
                live_blocks_for_shadow_var
                    .get_or_insert_default(shadow_var)
                    .insert(block_id);
            }
        }

        Ok(Self {
            block_liveness,
            live_blocks_for_shadow_var,
        })
    }

    /// Replace dead upsilon instructions (upsilon instructions which have no logical effect and are
    /// outside of the live range of their shadow variable) with `ir::Instruction::NoOp`.
    pub fn remove_dead_upsilons<S>(&self, ir: &mut ir::Function<S>) {
        for (block_id, block) in ir.blocks.iter() {
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                let inst = &mut ir.instructions[inst_id];
                if let &ir::Instruction::Upsilon(shadow_var, _) = &*inst {
                    if !self.is_live_upsilon(shadow_var, block_id, inst_index) {
                        *inst = ir::Instruction::NoOp;
                    }
                }
            }
        }
    }

    /// Returns true if the given instruction is a live upsilon instruction for the given shadow
    /// variable.
    pub fn is_live_upsilon(
        &self,
        shadow_var: ir::ShadowVar,
        block_id: ir::BlockId,
        inst_index: usize,
    ) -> bool {
        fn is_live_upsilon(live_range: &ShadowLivenessRange, inst_index: usize) -> bool {
            // All live upsilons are *always* the beginning of some incoming or outgoing range.
            if live_range
                .incoming_range
                .is_some_and(|r| r.start == Some(inst_index))
            {
                return true;
            }

            if live_range
                .outgoing_range
                .is_some_and(|r| r.start == Some(inst_index))
            {
                return true;
            }

            false
        }

        self.live_range_in_block(block_id, shadow_var)
            .is_some_and(|r| is_live_upsilon(&r, inst_index))
    }

    /// Returns all shadow variables used by any phi instruction in any block.
    pub fn live_shadow_vars(&self) -> impl Iterator<Item = ir::ShadowVar> + '_ {
        self.live_blocks_for_shadow_var.ids()
    }

    /// Returns all blocks and ranges within that block in which an shadow variable is live.
    pub fn live_ranges(
        &self,
        shadow_var: ir::ShadowVar,
    ) -> impl Iterator<Item = (ir::BlockId, ShadowLivenessRange)> + '_ {
        self.live_blocks_for_shadow_var
            .get(shadow_var)
            .into_iter()
            .flatten()
            .map(move |&block_id| (block_id, self.block_liveness[block_id][&shadow_var]))
    }

    /// Returns all shadow variables that are live anywhere within the given block.
    pub fn live_for_block(
        &self,
        block_id: ir::BlockId,
    ) -> impl Iterator<Item = (ir::ShadowVar, ShadowLivenessRange)> + '_ {
        self.block_liveness
            .get(block_id)
            .into_iter()
            .flatten()
            .map(move |(&shadow_var, &range)| (shadow_var, range))
    }

    /// Returns the liveness range for a single shadow variable in the given block, if it is live
    /// there.
    pub fn live_range_in_block(
        &self,
        block_id: ir::BlockId,
        shadow_var: ir::ShadowVar,
    ) -> Option<ShadowLivenessRange> {
        Some(*self.block_liveness.get(block_id)?.get(&shadow_var)?)
    }
}
