use std::collections::{HashMap, hash_map};

use thiserror::Error;

use crate::{
    graph::{dfs::try_depth_first_search, dominators::Dominators},
    ir,
};

#[derive(Debug, Copy, Clone, Error)]
pub enum ScopeLivenessError {
    #[error("scope close at {0} is not dominated by its open")]
    CloseNotDominated(ir::InstLocation),
    #[error("close instruction at {0} does not close an open scope")]
    DeadClose(ir::InstLocation),
    #[error("incoming edges for block {0} are not all open or all closed")]
    IndeterminateState(ir::BlockId),
}

/// The live range of a scope within a block, specified in block instruction indexes.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ScopeBlockLiveness {
    /// The instruction index in the block at which the scope is opened.
    ///
    /// If `None`, then this scope is opened in a predecessor to this block. If this is `Some`,
    /// then this index will contain the open instruction.
    pub start: Option<usize>,

    /// The instruction index in the block at which the scope is closed.
    ///
    /// If `None`, then this scope is closed by a successor to this block.
    ///
    /// If this is `Some`, then this index will either contain a close instruction or the index
    /// for the special `Exit` instruction, which is 1 past the end of the normal block instruction
    /// list.
    pub end: Option<usize>,
}

#[derive(Debug)]
pub struct ScopeLiveness {
    live_ranges: HashMap<ir::BlockId, ScopeBlockLiveness>,
}

impl ScopeLiveness {
    /// Compute the liveness of a statically bounded "scope" with a single open instruction and any
    /// number of close instructions.
    ///
    /// This verifies that:
    ///   1) Every close is dominated by the open.
    ///   2) Every instruction in the CFG has a definite opened or closed state. In other words,
    ///      there can be no ambiguous regions of the CFG, where the scope may or may not be opened
    ///      depending on the path taken at runtime.
    ///   3) Every close is not "dead", i.e. that it closes an open scope.
    pub fn compute<S>(
        ir: &ir::Function<S>,
        block_dominance: &Dominators<ir::BlockId>,
        open_loc: ir::InstLocation,
        close_locs: impl IntoIterator<Item = ir::InstLocation>,
    ) -> Result<Self, ScopeLivenessError> {
        // Gather all of the close instruction locations and ensure they are dominated by the open
        // instruction.

        let mut close_for_block = HashMap::<ir::BlockId, usize>::new();
        for close_loc in close_locs {
            if open_loc.block_id == close_loc.block_id {
                if close_loc.index < open_loc.index {
                    return Err(ScopeLivenessError::CloseNotDominated(close_loc));
                }
            } else if !block_dominance
                .dominates(open_loc.block_id, close_loc.block_id)
                .unwrap()
            {
                return Err(ScopeLivenessError::CloseNotDominated(close_loc));
            }

            match close_for_block.entry(close_loc.block_id) {
                hash_map::Entry::Vacant(vacant) => {
                    vacant.insert(close_loc.index);
                }
                hash_map::Entry::Occupied(occupied) => {
                    // We only allow for one open instruction, so no block can have two close
                    // instructions. Whichever one comes later is dead.
                    let later_close = (*occupied.get()).max(close_loc.index);
                    return Err(ScopeLivenessError::DeadClose(ir::InstLocation::new(
                        close_loc.block_id,
                        later_close,
                    )));
                }
            }
        }

        // DFS from the open block, stopping at any close block.
        //
        // Every block that we encounter should be strictly dominated by the open block (otherwise
        // this would be a block with indeterminate state). Every block that we reach this way has a
        // live range.

        let mut live_ranges: HashMap<ir::BlockId, ScopeBlockLiveness> = HashMap::new();
        try_depth_first_search(
            open_loc.block_id,
            |block_id| {
                let block = &ir.blocks[block_id];

                let mut range_start = None;
                let mut range_end = None;

                if block_id == open_loc.block_id {
                    range_start = Some(open_loc.index);
                }

                if let Some(&close_index) = close_for_block.get(&block_id) {
                    range_end = Some(close_index);
                }

                if range_end.is_none() && block.exit.exits_function() {
                    range_end = Some(block.instructions.len());
                }

                assert!(
                    live_ranges
                        .insert(
                            block_id,
                            ScopeBlockLiveness {
                                start: range_start,
                                end: range_end
                            }
                        )
                        .is_none()
                );

                if range_end.is_some() {
                    Ok(None.into_iter().flatten())
                } else if let Some(indeterminate_block) = block.exit.successors().find(|&b| {
                    b == open_loc.block_id
                        || !block_dominance.dominates(open_loc.block_id, b).unwrap()
                }) {
                    // We should not be able to reach a block that the open block does not
                    // strictly dominate without passing through close.
                    Err(ScopeLivenessError::IndeterminateState(indeterminate_block))
                } else {
                    Ok(Some(block.exit.successors()).into_iter().flatten())
                }
            },
            |_| Ok(()),
        )?;

        // Any close instruction in a block without a live range is a dead close instruction. Since
        // we already verified that no block has two close instructions, we already know every close
        // within a live block is live.

        for (&block_id, &inst_index) in close_for_block.iter() {
            if !live_ranges.contains_key(&block_id) {
                return Err(ScopeLivenessError::DeadClose(ir::InstLocation::new(
                    block_id, inst_index,
                )));
            }
        }

        // Go through all of the reacahble blocks whose outgoing edges are not live and check if
        // they have an outgoing edge to any block with a known live incoming edge. Since this
        // successor block has both a dead and live incoming edge, it is indeterminate.

        for block_id in block_dominance.topological_order() {
            let block = &ir.blocks[block_id];

            // Blocks with no live range or a close instruction have dead outgoing edges.
            let outgoing_edges_are_dead =
                live_ranges.get(&block_id).is_none_or(|b| b.end.is_some());

            if outgoing_edges_are_dead {
                for successor_block_id in block.exit.successors() {
                    // Blocks with a live range and no open instruction have live incoming edges.
                    let live_incoming_edge = successor_block_id != open_loc.block_id
                        && live_ranges.contains_key(&successor_block_id);

                    if live_incoming_edge {
                        return Err(ScopeLivenessError::IndeterminateState(successor_block_id));
                    }
                }
            }
        }

        Ok(Self { live_ranges })
    }

    pub fn live_blocks(&self) -> impl Iterator<Item = (ir::BlockId, ScopeBlockLiveness)> {
        self.live_ranges.iter().map(|(b, l)| (*b, *l))
    }

    pub fn for_block(&self, block_id: ir::BlockId) -> Option<ScopeBlockLiveness> {
        self.live_ranges.get(&block_id).copied()
    }
}
