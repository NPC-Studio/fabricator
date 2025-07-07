use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    graph::{dfs::depth_first_search, dominators::Dominators},
    ir,
};

#[derive(Debug, Error)]
pub enum ScopeVerificationError {
    #[error("scope is not opened exactly once")]
    BadOpen,
    #[error("scope is closed more than once")]
    MultipleClose,
    #[error("scope close is not dominated by its open")]
    CloseNotDominated,
    #[error("range exists where a scope is not definitely open or definitely closed")]
    IndeterminateState,
}

pub fn verify_scopes<S>(ir: &ir::Function<S>) -> Result<(), ScopeVerificationError> {
    let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());

    let mut scopes: HashSet<ir::ThisScope> = HashSet::new();
    let mut scope_open: HashMap<ir::ThisScope, (ir::BlockId, usize)> = HashMap::new();
    let mut scope_close: HashMap<ir::ThisScope, (ir::BlockId, usize)> = HashMap::new();

    for block_id in dominators.topological_order() {
        let block = &ir.blocks[block_id];
        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            match ir.instructions[inst_id] {
                ir::Instruction::OpenThisScope(scope, _) => {
                    scopes.insert(scope);
                    if scope_open.insert(scope, (block_id, inst_index)).is_some() {
                        return Err(ScopeVerificationError::BadOpen);
                    }
                }
                ir::Instruction::CloseThisScope(scope) => {
                    scopes.insert(scope);
                    if scope_close.insert(scope, (block_id, inst_index)).is_some() {
                        return Err(ScopeVerificationError::MultipleClose);
                    }
                }
                _ => {}
            }
        }
    }

    for &scope in &scopes {
        let &(open_block_id, open_index) = scope_open
            .get(&scope)
            .ok_or(ScopeVerificationError::BadOpen)?;

        let scope_close = scope_close.get(&scope).copied();

        if let Some((close_block_id, close_index)) = scope_close {
            if open_block_id == close_block_id {
                if close_index < open_index {
                    return Err(ScopeVerificationError::CloseNotDominated);
                }
            } else {
                if !dominators.dominates(open_block_id, close_block_id).unwrap() {
                    return Err(ScopeVerificationError::CloseNotDominated);
                }
            }
        }

        let mut live_blocks: HashSet<ir::BlockId> = HashSet::new();

        // DFS from the open block, stopping at the close block.
        //
        // Every block that we encounter should be strictly dominated by the open block (otherwise
        // this would be a block with indeterminate state).

        let mut indeterminate_block = false;
        depth_first_search(
            open_block_id,
            |block_id| {
                let mut block_has_close = false;

                if let Some((close_block_id, _)) = scope_close {
                    if block_id == close_block_id {
                        block_has_close = true;
                    }
                }

                let block = &ir.blocks[block_id];

                if !block_has_close {
                    if let ir::Exit::Return { .. } = block.exit {
                        block_has_close = true;
                    }
                }

                assert!(live_blocks.insert(block_id));

                if block_has_close {
                    None
                } else if block
                    .exit
                    .successors()
                    .any(|b| b == open_block_id || !dominators.dominates(open_block_id, b).unwrap())
                {
                    // We should not be able to reach a block that the open block does not
                    // strictly dominate without passing through close.
                    indeterminate_block = true;
                    None
                } else {
                    Some(block.exit.successors())
                }
                .into_iter()
                .flatten()
            },
            |_| {},
        );

        if indeterminate_block {
            return Err(ScopeVerificationError::IndeterminateState);
        }

        // If we have a close block, then find any indeterminate blocks by ensuring that all
        // blocks reachble from the close block that don't pass through the open block are not
        // live.
        if let Some((close_block_id, _)) = scope_close {
            let mut indeterminate_block = false;
            depth_first_search(
                close_block_id,
                |block_id| {
                    let block = &ir.blocks[block_id];

                    if block
                        .exit
                        .successors()
                        .any(|b| b != open_block_id && live_blocks.contains(&b))
                    {
                        // We should not be able to reach an live block without passing through
                        // open.
                        indeterminate_block = true;
                        None
                    } else {
                        // As an extra optimization, we don't need to traverse through any
                        // blocks that are not dominated by the open block. We know none of
                        // those blocks are live because we already checked for non-dominted
                        // live blocks above.
                        Some(block.exit.successors().filter(|&b| {
                            b != open_block_id || !dominators.dominates(open_block_id, b).unwrap()
                        }))
                    }
                    .into_iter()
                    .flatten()
                },
                |_| {},
            );

            if indeterminate_block {
                return Err(ScopeVerificationError::IndeterminateState);
            }
        }
    }

    Ok(())
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

        let this = instructions.insert(ir::Instruction::NewObject);
        block_a.instructions.push(this);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(scope, this)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(scope)));

        block_b.exit = ir::Exit::Jump(block_b_id);

        let ir = ir::Function {
            reference: FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables: Default::default(),
            shadow_vars: Default::default(),
            this_scopes,
            functions: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            verify_scopes(&ir),
            Err(ScopeVerificationError::IndeterminateState)
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

        let troo = instructions.insert(ir::Instruction::Constant(Constant::Boolean(true)));
        block_a.instructions.push(troo);
        let this = instructions.insert(ir::Instruction::NewObject);
        block_a.instructions.push(this);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenThisScope(scope, this)));

        block_a.exit = ir::Exit::Branch {
            cond: troo,
            if_false: block_b_id,
            if_true: block_a_id,
        };

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseThisScope(scope)));

        let ir = ir::Function {
            reference: FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables: Default::default(),
            shadow_vars: Default::default(),
            this_scopes,
            functions: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            verify_scopes(&ir),
            Err(ScopeVerificationError::IndeterminateState)
        ));
    }
}
