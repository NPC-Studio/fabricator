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

    /// The instruction index in the block at which the shadow variable becomes dead.
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
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ShadowLivenessRange {
    /// If an incoming range is set, this block will always contain the `Phi` instruction, and this
    /// describes the `Phi` instruction liveness range leading backwards from the `Phi` instruction.
    pub incoming_range: Option<ShadowIncomingRange>,

    /// If an outgoing range is set, then a successor block contains the `Phi` instruction and its
    /// shadow variable's lifetime overlaps this blocks.
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
pub type BlockShadowLiveness = HashMap<ir::InstId, ShadowLivenessRange>;

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
    /// If a block is dead (unreachable from the IR start block), this return `None`.
    pub fn block_shadow_liveness(&self, block_id: ir::BlockId) -> Option<&BlockShadowLiveness> {
        self.0.get(block_id)
    }
}
