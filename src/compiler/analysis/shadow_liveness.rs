use std::collections::HashMap;

use thiserror::Error;

use crate::{compiler::ir, util::typed_id_map::SecondaryMap};

#[derive(Debug, Error)]
pub enum PhiUpsilonVerificationError {
    #[error("shadow variable used by multiple phi instructions")]
    ShadowReused,
    #[error("not all paths to a phi instruction contain an upsilon to define the shadow variable")]
    ShadowUndef,
}
/// The liveness range of a shadow variable within a block, specified in block instruction indexes.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ShadowLivenessRange(Option<usize>, Option<usize>);

impl ShadowLivenessRange {
    /// The instruction index in the block at which the shadow variable becomes live.
    ///
    /// If `None`, then this shadow variable becomes live in a predecessor to this block and is in
    /// the "live in" set.
    pub fn start_bound(&self) -> Option<usize> {
        self.0
    }

    /// The instruction index in the block at which the shadow variable becomes dead.
    ///
    /// If this points to an instruction, then this instruction will always be a `Phi` instruction,
    /// but not all `Phi` instructions end the lives of their shadow variable! If a `Phi`
    /// instruction is used in a loop, the shadow variable may live past the `Phi` instruction which
    /// owns it.
    ///
    /// If `None`, then this shadow variable dead successor to this block and is in the "live out"
    /// set.
    pub fn end_bound(&self) -> Option<usize> {
        self.1
    }
}

/// The liveness range of all instructions within a block.
#[derive(Debug, Clone, Default)]
pub struct BlockShadowLiveness(HashMap<ir::ShadowVarId, ShadowLivenessRange>);

impl BlockShadowLiveness {
    /// Returns an iterator over all shadow variables that are live anywhere within this block.
    pub fn live_shadow_variables(&self) -> impl Iterator<Item = ir::ShadowVarId> + '_ {
        self.0.keys().copied()
    }

    /// Returns all shadow variables which are live in predecessors to this block and must be live
    /// on entry to this block.
    pub fn live_in(&self) -> impl Iterator<Item = ir::ShadowVarId> + '_ {
        self.0.iter().filter_map(|(&inst_id, range)| {
            if range.start_bound().is_none() {
                Some(inst_id)
            } else {
                None
            }
        })
    }

    /// Returns all shadow variables which are live in some successor to this block.
    pub fn live_out(&self) -> impl Iterator<Item = ir::ShadowVarId> + '_ {
        self.0.iter().filter_map(|(&inst_id, range)| {
            if range.end_bound().is_none() {
                Some(inst_id)
            } else {
                None
            }
        })
    }

    /// Returns the range of this shadow variable within this block, if it is live anywhere within
    /// the block.
    pub fn range(&self, svar: ir::ShadowVarId) -> Option<ShadowLivenessRange> {
        self.0.get(&svar).copied()
    }
}

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
    pub fn compute<S>(_ir: &ir::Function<S>) -> Result<Self, PhiUpsilonVerificationError> {
        unimplemented!()
    }

    /// Get the shadow variable liveness information for the given block.
    ///
    /// If a block is dead (unreachable from the IR start block), this will not return liveness
    /// information for that block.
    pub fn block_ranges(&self, block_id: ir::BlockId) -> Option<&BlockShadowLiveness> {
        self.0.get(block_id)
    }
}
