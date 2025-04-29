use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    compiler::{
        graph::{
            dfs::{depth_first_search, topological_order},
            dominators::Dominators,
        },
        ir,
    },
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum VariableVerificationError {
    #[error("upvalue variable has an open or close instruction")]
    OpenCloseUpValue,
    #[error("variable is not opened or opened more than once")]
    BadOpen,
    #[error("variable is closed more than once or may be closed before it is opened")]
    BadClose,
    #[error("variable use is not dominated by its open or may occur after a close")]
    UseNotInRange,
}

/// Verify that variable use always follows an open and preceeds a close
pub fn verify_vars<S>(ir: &ir::Function<S>) -> Result<(), VariableVerificationError> {
    let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());
    let reachable_blocks = topological_order(ir.start_block, |id| ir.blocks[id].exit.successors());

    let mut variables: HashSet<ir::Variable> = HashSet::new();
    let mut variable_opens: HashMap<ir::Variable, (ir::BlockId, usize)> = HashMap::new();
    let mut variable_closes: HashMap<ir::Variable, (ir::BlockId, usize)> = HashMap::new();
    let mut variable_uses: HashMap<ir::Variable, Vec<(ir::BlockId, usize)>> = HashMap::new();

    for &block_id in &reachable_blocks {
        let block = &ir.blocks[block_id];
        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            match ir.instructions[inst_id] {
                ir::Instruction::OpenVariable(var) => {
                    variables.insert(var);
                    if variable_opens.insert(var, (block_id, inst_index)).is_some() {
                        return Err(VariableVerificationError::BadOpen);
                    }
                }
                ir::Instruction::GetVariable(var) | ir::Instruction::SetVariable(var, _) => {
                    variables.insert(var);
                    variable_uses
                        .entry(var)
                        .or_default()
                        .push((block_id, inst_index));
                }
                ir::Instruction::CloseVariable(var) => {
                    variables.insert(var);
                    if variable_closes
                        .insert(var, (block_id, inst_index))
                        .is_some()
                    {
                        return Err(VariableVerificationError::BadClose);
                    }
                }
                _ => {}
            }
        }
    }

    for &var in &variables {
        if ir.upvalues.contains_key(&var) {
            if variable_opens.contains_key(&var) || variable_closes.contains_key(&var) {
                return Err(VariableVerificationError::OpenCloseUpValue);
            }
            continue;
        }

        let &(open_block, open_index) = variable_opens
            .get(&var)
            .ok_or(VariableVerificationError::BadOpen)?;

        struct BlockRange {
            start: Option<usize>,
            end: Option<usize>,
        }

        impl BlockRange {
            fn contains(&self, inst_index: usize) -> bool {
                self.start.is_none_or(|start| inst_index >= start)
                    && self.end.is_none_or(|end| inst_index <= end)
            }
        }

        // Instruction ranges in which we are sure that `VariableOpen` must have been called.
        let mut post_open: SecondaryMap<ir::BlockId, BlockRange> = SecondaryMap::new();
        depth_first_search(
            open_block,
            |block_id| {
                ir.blocks[block_id]
                    .exit
                    .successors()
                    .filter(|&b| dominators.dominates(open_block, b).unwrap())
            },
            |block_id| {
                post_open.insert(
                    block_id,
                    BlockRange {
                        start: if block_id == open_block {
                            Some(open_index)
                        } else {
                            None
                        },
                        end: None,
                    },
                );
            },
            |_| {},
        );

        for &(block_id, inst_index) in &variable_uses[&var] {
            if post_open
                .get(block_id)
                .is_none_or(|r| !r.contains(inst_index))
            {
                return Err(VariableVerificationError::UseNotInRange);
            }
        }

        if let Some(&(close_block, close_index)) = variable_closes.get(&var) {
            // Instruction ranges in which `VariableClose` may have been called.
            let mut post_close: SecondaryMap<ir::BlockId, BlockRange> = SecondaryMap::new();
            depth_first_search(
                close_block,
                |block_id| ir.blocks[block_id].exit.successors(),
                |block_id| {
                    post_close.insert(
                        block_id,
                        BlockRange {
                            start: if block_id == close_block {
                                Some(close_index)
                            } else {
                                None
                            },
                            end: None,
                        },
                    );
                },
                |_| {},
            );

            for &(block_id, inst_index) in &variable_uses[&var] {
                if post_close
                    .get(block_id)
                    .is_some_and(|r| r.contains(inst_index))
                {
                    return Err(VariableVerificationError::UseNotInRange);
                }
            }
        }
    }

    Ok(())
}
