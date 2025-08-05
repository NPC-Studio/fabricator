use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use fabricator_util::typed_id_map::{self, SecondaryMap};
use thiserror::Error;

use crate::{
    analysis::scope_liveness::{ScopeBlockLiveness, ScopeLiveness, ScopeLivenessError},
    graph::dominators::Dominators,
    ir,
};

#[derive(Debug, Copy, Clone, Error)]
pub enum NestedScopeVerificationErrorKind<I> {
    #[error("scope is not opened exactly once")]
    BadOpen,
    #[error("scope is closed more than once")]
    MultipleClose,
    #[error("scope close is not dominated by its open")]
    CloseNotDominated,
    #[error("range exists where a scope is not definitely open or definitely closed")]
    IndeterminateState,
    #[error("scope use is not dominated by its open or may occur after a close")]
    UseNotInRange,
    #[error("scope is not strictly nested within another scope")]
    ScopeNotNested { other_scope: I },
    #[error("scope use is within an inner scope")]
    UseOverlapsInner,
}

#[derive(Debug, Copy, Clone, Error)]
#[error("{kind}")]
pub struct NestedScopeVerificationError<I> {
    pub kind: NestedScopeVerificationErrorKind<I>,
    pub scope: I,
}

#[derive(Debug)]
pub struct NestedScopeLiveness<I>
where
    I: typed_id_map::Id,
{
    live_ranges: SecondaryMap<I, ScopeLiveness>,
    live_scopes_for_block: SecondaryMap<ir::BlockId, HashSet<I>>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum ScopeInstType {
    Open,
    Use,
    Close,
}

impl<I> NestedScopeLiveness<I>
where
    I: typed_id_map::Id + Eq + Hash + Copy,
{
    /// Compute scope liveness ranges for every scope in the given IR. A scope is live
    /// after it is opened and dead when it is closed.
    ///
    /// This also verifies all scopes within the IR and their use, namely that:
    ///   1) Every scope has exactly one open instruction and at most one close instruction.
    ///   2) Every instruction in the CFG has a definite opened or closed state for every scope,
    ///      there are no ambiguous regions.
    ///   3) Every use of a scope is in its definitely-open region.
    ///   4) Scopes may be nested, but they must be strictly so.
    ///   5) Every use of a scope is not within an inner, nested scope.
    fn compute_with<S>(
        ir: &ir::Function<S>,
        scope_inst_type: impl Fn(&ir::Instruction<S>) -> Option<(I, ScopeInstType)>,
    ) -> Result<Self, NestedScopeVerificationError<I>> {
        let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());

        let mut scopes: HashSet<I> = HashSet::new();
        let mut scope_open: HashMap<I, (ir::BlockId, usize)> = HashMap::new();
        let mut scope_uses: HashMap<I, Vec<(ir::BlockId, usize)>> = HashMap::new();
        let mut scope_close: HashMap<I, (ir::BlockId, usize)> = HashMap::new();

        // A topological ordering of scope opens from earliest to latest. In well-formed IR, this is
        // also a topological ordering of scope *nesting*, where nested inner scopes are guaranteed
        // to have a larger index than their outer scope.
        let mut scope_topological_indexes = HashMap::<I, usize>::new();
        let mut current_topological_index = 0;

        for block_id in dominators.topological_order() {
            let block = &ir.blocks[block_id];
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                match scope_inst_type(&ir.instructions[inst_id]) {
                    Some((scope, ScopeInstType::Open)) => {
                        scopes.insert(scope);
                        scope_topological_indexes.insert(scope, current_topological_index);
                        current_topological_index += 1;
                        if scope_open.insert(scope, (block_id, inst_index)).is_some() {
                            return Err(NestedScopeVerificationError {
                                kind: NestedScopeVerificationErrorKind::BadOpen,
                                scope,
                            });
                        }
                    }
                    Some((scope, ScopeInstType::Use)) => {
                        scopes.insert(scope);
                        scope_uses
                            .entry(scope)
                            .or_default()
                            .push((block_id, inst_index));
                    }
                    Some((scope, ScopeInstType::Close)) => {
                        scopes.insert(scope);
                        if scope_close.insert(scope, (block_id, inst_index)).is_some() {
                            return Err(NestedScopeVerificationError {
                                kind: NestedScopeVerificationErrorKind::MultipleClose,
                                scope,
                            });
                        }
                    }
                    None => {}
                }
            }
        }

        let mut this = NestedScopeLiveness {
            live_ranges: SecondaryMap::new(),
            live_scopes_for_block: SecondaryMap::new(),
        };

        for &scope in &scopes {
            let &scope_open = scope_open.get(&scope).ok_or(NestedScopeVerificationError {
                kind: NestedScopeVerificationErrorKind::BadOpen,
                scope,
            })?;
            let scope_close = scope_close.get(&scope).copied();

            let scope_liveness = ScopeLiveness::compute(ir, &dominators, scope_open, scope_close)
                .map_err(|e| NestedScopeVerificationError {
                kind: match e {
                    ScopeLivenessError::CloseNotDominated => {
                        NestedScopeVerificationErrorKind::CloseNotDominated
                    }
                    ScopeLivenessError::IndeterminateState => {
                        NestedScopeVerificationErrorKind::IndeterminateState
                    }
                },
                scope,
            })?;

            for &(block_id, inst_index) in scope_uses.get(&scope).into_iter().flatten() {
                let live_range =
                    scope_liveness
                        .for_block(block_id)
                        .ok_or(NestedScopeVerificationError {
                            kind: NestedScopeVerificationErrorKind::UseNotInRange,
                            scope,
                        })?;

                if live_range.start.is_some_and(|start| inst_index <= start) {
                    return Err(NestedScopeVerificationError {
                        kind: NestedScopeVerificationErrorKind::UseNotInRange,
                        scope,
                    });
                }
                if live_range.end.is_some_and(|end| inst_index >= end) {
                    return Err(NestedScopeVerificationError {
                        kind: NestedScopeVerificationErrorKind::UseNotInRange,
                        scope,
                    });
                }
            }

            for (block_id, _) in scope_liveness.live_blocks() {
                this.live_scopes_for_block
                    .get_or_insert_default(block_id)
                    .insert(scope);
            }
            this.live_ranges.insert(scope, scope_liveness);
        }

        // Check scopes are strictly nested and that each scope use is not within an inner scope.

        fn in_range(inst_index: usize, liveness_range: &ScopeBlockLiveness) -> bool {
            if liveness_range.start.is_some_and(|start| inst_index < start) {
                return false;
            }

            if liveness_range.end.is_some_and(|end| inst_index > end) {
                return false;
            }

            true
        }

        for &scope in &scopes {
            let scope_topological_index = scope_topological_indexes[&scope];

            // A "close" counts as a use for the purposes of ensuring that scopes are strictly
            // nested.
            if let Some(&(close_block_id, close_inst_index)) = scope_close.get(&scope) {
                for (live_scope, liveness_range) in this.live_for_block(close_block_id) {
                    // Check all of the other live scopes in this block that have higher topological
                    // indexes. If a scope has a higher topological index, then its open must
                    // not have come *before* the open for the current scope. If the close of the
                    // current scope falls within the liveness range of one of these topologically
                    // later scopes, then we know both that these scopes' lifetimes overlap and are
                    // not strictly nested.
                    if scope_topological_indexes[&live_scope] > scope_topological_index
                        && in_range(close_inst_index, &liveness_range)
                    {
                        return Err(NestedScopeVerificationError {
                            kind: NestedScopeVerificationErrorKind::ScopeNotNested {
                                other_scope: live_scope,
                            },
                            scope,
                        });
                    }
                }
            }

            for &(use_block_id, use_inst_index) in scope_uses.get(&scope).into_iter().flatten() {
                for (live_scope, liveness_range) in this.live_for_block(use_block_id) {
                    // We have already verified that this use of the current scope is within the
                    // current scope's liveness range.
                    //
                    // Check all of the other live scopes in this block that have higher topological
                    // indexes. If a scope has a higher topological index, then its open must not
                    // have come *before* the open for the current scope. If a use of the current
                    // scope falls within the liveness range of one of these topologically later
                    // scopes, then we know that this use, and thus also the total lifetime of the
                    // scope, overlaps with this other live scope. We have already ensured that the
                    // scope must be properly nested, so we know this is an access of an outer scope
                    // within an inner scopes, which is disallowed.
                    if scope_topological_indexes[&live_scope] > scope_topological_index
                        && in_range(use_inst_index, &liveness_range)
                    {
                        return Err(NestedScopeVerificationError {
                            kind: NestedScopeVerificationErrorKind::UseOverlapsInner,
                            scope,
                        });
                    }
                }
            }
        }

        Ok(this)
    }

    /// Returns all owned scopes that are live anywhere within the given block.
    pub fn live_for_block(
        &self,
        block_id: ir::BlockId,
    ) -> impl Iterator<Item = (I, ScopeBlockLiveness)> + '_ {
        self.live_scopes_for_block
            .get(block_id)
            .into_iter()
            .flatten()
            .map(move |&scope| (scope, self.live_ranges[scope].for_block(block_id).unwrap()))
    }
}

pub type ThisScopeVerificationError = NestedScopeVerificationError<ir::ThisScope>;
pub type ThisScopeLiveness = NestedScopeLiveness<ir::ThisScope>;

impl ThisScopeLiveness {
    pub fn compute<S>(
        ir: &ir::Function<S>,
    ) -> Result<NestedScopeLiveness<ir::ThisScope>, ThisScopeVerificationError> {
        NestedScopeLiveness::compute_with(ir, |inst| match *inst {
            ir::Instruction::OpenThisScope(scope) => Some((scope, ScopeInstType::Open)),
            ir::Instruction::SetThis(scope, _) => Some((scope, ScopeInstType::Use)),
            ir::Instruction::CloseThisScope(scope) => Some((scope, ScopeInstType::Close)),
            _ => None,
        })
    }
}

pub type CallScopeVerificationError = NestedScopeVerificationError<ir::CallScope>;
pub type CallScopeLiveness = NestedScopeLiveness<ir::CallScope>;

impl CallScopeLiveness {
    pub fn compute<S>(
        ir: &ir::Function<S>,
    ) -> Result<NestedScopeLiveness<ir::CallScope>, CallScopeVerificationError> {
        NestedScopeLiveness::compute_with(ir, |inst| match *inst {
            ir::Instruction::OpenCall { scope, .. } => Some((scope, ScopeInstType::Open)),
            ir::Instruction::GetReturn(scope, _) => Some((scope, ScopeInstType::Use)),
            ir::Instruction::CloseCall(scope) => Some((scope, ScopeInstType::Close)),
            _ => None,
        })
    }
}

#[cfg(test)]
mod tests {
    use fabricator_vm::FunctionRef;

    use crate::constant::Constant;

    use super::*;

    #[test]
    fn test_scopes_loop_closes() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut this_scopes = ir::ThisScopeSet::new();

        let scope = this_scopes.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(scope)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(scope)));

        block_b.exit = ir::Exit::Jump(block_b_id);

        let ir = ir::Function {
            num_parameters: 0,
            is_constructor: false,
            reference: FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables: Default::default(),
            shadow_vars: Default::default(),
            this_scopes,
            call_scopes: Default::default(),
            functions: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            ThisScopeLiveness::compute(&ir),
            Err(NestedScopeVerificationError {
                kind: NestedScopeVerificationErrorKind::IndeterminateState,
                ..
            })
        ));
    }

    #[test]
    fn test_scopes_loop_reopens() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut this_scopes = ir::ThisScopeSet::new();

        let scope = this_scopes.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        let true_ = instructions.insert(ir::Instruction::Constant(Constant::Boolean(true)));
        block_a.instructions.push(true_);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(scope)));

        block_a.exit = ir::Exit::Branch {
            cond: true_,
            if_false: block_b_id,
            if_true: block_a_id,
        };

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(scope)));

        let ir = ir::Function {
            num_parameters: 0,
            is_constructor: false,
            reference: FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables: Default::default(),
            shadow_vars: Default::default(),
            this_scopes,
            call_scopes: Default::default(),
            functions: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            ThisScopeLiveness::compute(&ir),
            Err(NestedScopeVerificationError {
                kind: NestedScopeVerificationErrorKind::IndeterminateState,
                ..
            })
        ));
    }

    #[test]
    fn test_scope_not_nested() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut this_scopes = ir::ThisScopeSet::new();

        let outer_scope = this_scopes.insert(());
        let inner_scope = this_scopes.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(outer_scope)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(inner_scope)));

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(outer_scope)));

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(inner_scope)));

        let ir = ir::Function {
            num_parameters: 0,
            is_constructor: false,
            reference: FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables: Default::default(),
            shadow_vars: Default::default(),
            this_scopes,
            call_scopes: Default::default(),
            functions: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            ThisScopeLiveness::compute(&ir),
            Err(NestedScopeVerificationError {
                kind: NestedScopeVerificationErrorKind::ScopeNotNested { .. },
                ..
            })
        ));
    }
}
