use std::{collections::HashSet, hash::Hash};

use fabricator_util::typed_id_map::{self, SecondaryMap};
use thiserror::Error;

use crate::{
    analysis::scope_liveness::{ScopeBlockLiveness, ScopeLiveness, ScopeLivenessError},
    graph::{dfs::try_depth_first_search_with, dominators::Dominators},
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
    scope_meta: SecondaryMap<I, ScopeMeta<I>>,
    live_scopes_for_block: SecondaryMap<ir::BlockId, HashSet<I>>,
    nesting: usize,
}

#[derive(Debug)]
struct ScopeMeta<I> {
    liveness: ScopeLiveness,
    inner_scopes: Vec<I>,
    nesting_level: usize,
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
    /// Compute scope liveness ranges and nesting level for every scope in the given IR.
    ///
    /// A scope is live after it is opened and dead when it is closed.
    ///
    /// This also verifies all scopes within the IR and their use, namely that:
    ///   1) Every scope has exactly one open instruction and at most one close instruction.
    ///   2) Every instruction in the CFG has a definite opened or closed state for every scope,
    ///      there are no ambiguous regions.
    ///   3) Every use of a scope is in its definitely-open region.
    ///   4) Scopes may be nested, but they must be strictly so.
    ///   5) Every use of a scope is not within an inner nested scope.
    fn compute_with<S>(
        ir: &ir::Function<S>,
        scope_inst_type: impl Fn(&ir::Instruction<S>) -> Option<(I, ScopeInstType)>,
    ) -> Result<Self, NestedScopeVerificationError<I>> {
        let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());

        let mut scopes: SecondaryMap<I, ()> = SecondaryMap::new();
        let mut scope_open: SecondaryMap<I, (ir::BlockId, usize)> = SecondaryMap::new();
        let mut scope_uses: SecondaryMap<I, Vec<(ir::BlockId, usize)>> = SecondaryMap::new();
        let mut scope_close: SecondaryMap<I, (ir::BlockId, usize)> = SecondaryMap::new();

        for block_id in dominators.topological_order() {
            let block = &ir.blocks[block_id];
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                match scope_inst_type(&ir.instructions[inst_id]) {
                    Some((scope, ScopeInstType::Open)) => {
                        scopes.insert(scope, ());
                        if scope_open.insert(scope, (block_id, inst_index)).is_some() {
                            return Err(NestedScopeVerificationError {
                                kind: NestedScopeVerificationErrorKind::BadOpen,
                                scope,
                            });
                        }
                    }
                    Some((scope, ScopeInstType::Use)) => {
                        scopes.insert(scope, ());
                        scope_uses
                            .get_or_insert_default(scope)
                            .push((block_id, inst_index));
                    }
                    Some((scope, ScopeInstType::Close)) => {
                        scopes.insert(scope, ());
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
            scope_meta: SecondaryMap::new(),
            live_scopes_for_block: SecondaryMap::new(),
            nesting: 0,
        };

        for scope in scopes.ids() {
            let &scope_open = scope_open.get(scope).ok_or(NestedScopeVerificationError {
                kind: NestedScopeVerificationErrorKind::BadOpen,
                scope,
            })?;
            let scope_close = scope_close.get(scope).copied();

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

            for &(block_id, inst_index) in scope_uses.get(scope).into_iter().flatten() {
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
            this.scope_meta.insert(
                scope,
                ScopeMeta {
                    liveness: scope_liveness,
                    inner_scopes: Vec::new(),
                    nesting_level: 0,
                },
            );
        }

        // Check that scopes are strictly nested and assign a "nesting level" to each scope.
        //
        // We do a DFS on the graph and keep track of the current top-level scope. If we encounter a
        // close that is not the current top scope, we know that scopes are not strictly nested.

        let mut scope_stack = Vec::<I>::new();
        try_depth_first_search_with(
            &mut scope_stack,
            ir.start_block,
            |scope_stack, block_id| {
                for &inst_id in &ir.blocks[block_id].instructions {
                    match scope_inst_type(&ir.instructions[inst_id]) {
                        Some((scope, ScopeInstType::Open)) => {
                            this.scope_meta.get_mut(scope).unwrap().nesting_level =
                                scope_stack.len();
                            if let Some(&upper) = scope_stack.last() {
                                this.scope_meta
                                    .get_mut(upper)
                                    .unwrap()
                                    .inner_scopes
                                    .push(scope);
                            }
                            scope_stack.push(scope);
                        }
                        Some((scope, ScopeInstType::Close)) => {
                            let top_scope = scope_stack.pop().unwrap();
                            if scope != top_scope {
                                return Err(NestedScopeVerificationError {
                                    kind: NestedScopeVerificationErrorKind::ScopeNotNested {
                                        other_scope: scope,
                                    },
                                    scope: top_scope,
                                });
                            }
                        }
                        Some((_, ScopeInstType::Use)) | None => {}
                    }
                }

                Ok(ir.blocks[block_id].exit.successors())
            },
            |scope_stack, block_id| {
                for &inst_id in ir.blocks[block_id].instructions.iter().rev() {
                    match scope_inst_type(&ir.instructions[inst_id]) {
                        Some((scope, ScopeInstType::Close)) => {
                            scope_stack.push(scope);
                        }
                        Some((scope, ScopeInstType::Open)) => {
                            assert!(scope_stack.pop() == Some(scope));
                        }
                        Some((_, ScopeInstType::Use)) | None => {}
                    }
                }

                Ok(())
            },
        )?;

        this.nesting = this
            .scope_meta
            .values()
            .map(|m| m.nesting_level + 1)
            .max()
            .unwrap_or(0);

        // Check that every scope use is not within an inner scope.

        for scope in scopes.ids() {
            let nesting_level = this.scope_meta[scope].nesting_level;

            for &(use_block_id, use_inst_index) in scope_uses.get(scope).into_iter().flatten() {
                for (live_scope, liveness_range) in this.live_for_block(use_block_id) {
                    fn in_range(inst_index: usize, liveness_range: &ScopeBlockLiveness) -> bool {
                        if liveness_range.start.is_some_and(|start| inst_index < start) {
                            return false;
                        }

                        if liveness_range.end.is_some_and(|end| inst_index > end) {
                            return false;
                        }

                        true
                    }

                    if this.scope_meta[live_scope].nesting_level > nesting_level
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

    pub fn scopes(&self) -> impl Iterator<Item = I> {
        self.scope_meta.ids()
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
            .map(move |&scope| {
                (
                    scope,
                    self.scope_meta[scope].liveness.for_block(block_id).unwrap(),
                )
            })
    }

    /// Return all scopes which lie *immediately* inside the given scope.
    pub fn inner_scopes(&self, scope: I) -> impl Iterator<Item = I> {
        self.scope_meta
            .get(scope)
            .map(|m| m.inner_scopes.iter().copied())
            .into_iter()
            .flatten()
    }

    pub fn has_inner_scope(&self, scope: I) -> bool {
        self.scope_meta
            .get(scope)
            .map(|m| !m.inner_scopes.is_empty())
            .unwrap_or(false)
    }

    /// Return the scope with the deepest nesting level which encloses the given instruction.
    ///
    /// If the given instruction is itself a scope open or close, then this will return the *outer*
    /// scope for that instruction.
    pub fn deepest_for(&self, block_id: ir::BlockId, inst_index: usize) -> Option<I> {
        let mut deepest = None;
        for (scope, liveness) in self.live_for_block(block_id) {
            let within_bounds = liveness.start.is_none_or(|start| inst_index > start)
                && liveness.end.is_none_or(|end| inst_index < end);

            if within_bounds
                && deepest.is_none_or(|prev_scope| {
                    self.scope_meta[scope].nesting_level > self.scope_meta[prev_scope].nesting_level
                })
            {
                deepest = Some(scope);
            }
        }
        deepest
    }

    /// Returns how deeply scopes are nested. If no scope is nested within another scope, this will
    /// be 1. If no scopes were found, this will be 0.
    pub fn nesting(&self) -> usize {
        self.nesting
    }

    /// Returns the nesting level of the given scope. Top-level scopes are 0, every inner scope is 1
    /// larger than its outer scope.
    pub fn nesting_level(&self, scope: I) -> Option<usize> {
        Some(self.scope_meta.get(scope)?.nesting_level)
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

    #[test]
    fn test_scope_nesting_level() {
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
            .push(instructions.insert(ir::Instruction::NoOp));

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(inner_scope)));

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(outer_scope)));

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

        let liveness = ThisScopeLiveness::compute(&ir).unwrap();

        assert!(liveness.nesting() == 2);
        assert!(liveness.deepest_for(block_b_id, 1) == Some(inner_scope));
        assert!(liveness.inner_scopes(outer_scope).eq([inner_scope]));
        assert!(liveness.nesting_level(outer_scope) == Some(0));
        assert!(liveness.nesting_level(inner_scope) == Some(1));
    }
}
