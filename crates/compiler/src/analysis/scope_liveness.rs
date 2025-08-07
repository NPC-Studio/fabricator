use std::collections::HashMap;

use thiserror::Error;

use crate::{
    graph::{
        dfs::{depth_first_search, try_depth_first_search},
        dominators::Dominators,
    },
    ir,
};

#[derive(Debug, Copy, Clone, Error)]
pub enum ScopeLivenessError {
    #[error("scope close is not dominated by its open")]
    CloseNotDominated,
    #[error("range exists where a scope is not definitely open or definitely closed")]
    IndeterminateState,
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
    /// If this is `Some`, then this index will either contain the close instruction or the index
    /// for the special `Exit` instruction, which is 1 past the end of the normal block instruction
    /// list.
    pub end: Option<usize>,
}

#[derive(Debug)]
pub struct ScopeLiveness {
    live_ranges: HashMap<ir::BlockId, ScopeBlockLiveness>,
}

impl ScopeLiveness {
    /// Compute the liveness of a deterministically bounded "scope" with a single open and an
    /// optional close.
    ///
    /// This verifies that:
    ///   1) The close, if it exists, is dominated by the open.
    ///   2) Every instruction in the CFG has a definite opened or closed state. In other words,
    ///      there can be no ambiguous regions of the CFG, where the scope may or may not be opened
    ///      depending on the path taken at runtime.
    pub fn compute<S>(
        ir: &ir::Function<S>,
        block_dominance: &Dominators<ir::BlockId>,
        scope_open: (ir::BlockId, usize),
        scope_close: Option<(ir::BlockId, usize)>,
    ) -> Result<Self, ScopeLivenessError> {
        let (open_block_id, open_index) = scope_open;

        if let Some((close_block_id, close_index)) = scope_close {
            if open_block_id == close_block_id {
                if close_index < open_index {
                    return Err(ScopeLivenessError::CloseNotDominated);
                }
            } else {
                if !block_dominance
                    .dominates(open_block_id, close_block_id)
                    .unwrap()
                {
                    return Err(ScopeLivenessError::CloseNotDominated);
                }
            }
        }

        let mut live_ranges: HashMap<ir::BlockId, ScopeBlockLiveness> = HashMap::new();

        // DFS from the open block, stopping at the close block.
        //
        // Every block that we encounter should be strictly dominated by the open block (otherwise
        // this would be a block with indeterminate state). Every block that we reach this way has a
        // live range.

        try_depth_first_search(
            open_block_id,
            |block_id| {
                let mut range_start = None;
                let mut range_end = None;

                if block_id == open_block_id {
                    range_start = Some(open_index);
                }

                if let Some((close_block_id, close_index)) = scope_close {
                    if block_id == close_block_id {
                        range_end = Some(close_index);
                    }
                }

                let block = &ir.blocks[block_id];

                if range_end.is_none() {
                    if let ir::Exit::Return { .. } = block.exit {
                        range_end = Some(block.instructions.len());
                    }
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
                } else if block.exit.successors().any(|b| {
                    b == open_block_id || !block_dominance.dominates(open_block_id, b).unwrap()
                }) {
                    // We should not be able to reach a block that the open block does not
                    // strictly dominate without passing through close.
                    Err(ScopeLivenessError::IndeterminateState)
                } else {
                    Ok(Some(block.exit.successors()).into_iter().flatten())
                }
            },
            |_| Ok(()),
        )?;

        // If we have a close block, then find any indeterminate blocks by ensuring that all blocks
        // reachble from the close block that don't pass through the open block are not live.
        if let Some((close_block_id, _)) = scope_close {
            let mut indeterminate_block = false;
            depth_first_search(
                close_block_id,
                |block_id| {
                    let block = &ir.blocks[block_id];

                    if block
                        .exit
                        .successors()
                        .any(|b| b != open_block_id && live_ranges.contains_key(&b))
                    {
                        // We should not be able to reach an live block without passing through
                        // open.
                        indeterminate_block = true;
                        None
                    } else {
                        // As an extra optimization, we don't need to traverse through any blocks
                        // that are not dominated by the open block. We know none of those blocks
                        // are live because we already checked for non-dominted live blocks above.
                        Some(block.exit.successors().filter(|&b| {
                            b != open_block_id
                                || !block_dominance.dominates(open_block_id, b).unwrap()
                        }))
                    }
                    .into_iter()
                    .flatten()
                },
                |_| {},
            );

            if indeterminate_block {
                return Err(ScopeLivenessError::IndeterminateState);
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
