use std::collections::{HashMap, HashSet};

use fabricator_util::typed_id_map::SecondaryMap;
use thiserror::Error;

use crate::{
    graph::{
        dfs::{depth_first_search, topological_order},
        dominators::Dominators,
    },
    ir,
};

#[derive(Debug, Error)]
pub enum VariableVerificationError {
    #[error("upvalue variable has an open or close instruction")]
    OpenCloseUpValue,
    #[error("owned variable is not opened exactly once")]
    BadOpen,
    #[error("owned variable is closed more than once")]
    MultipleClose,
    #[error("variable close is not dominated by its open")]
    CloseNotDominated,
    #[error("range exists that is not definitely open or definitely closed")]
    IndeterminateState,
    #[error("variable use is not dominated by its open or may occur after a close")]
    UseNotInRange,
}

/// The live range of a variable within a block, specified in block instruction indexes.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VariableLivenessRange {
    /// The instruction index in the block at which the variable is opened.
    ///
    /// If `None`, then this variable is opened in a predecessor to this block. If this is `Some`,
    /// then this index will contain the `VariableOpen` instruction.
    pub start: Option<usize>,

    /// The instruction index in the block at which the variable is closed.
    ///
    /// If `None`, then this variable is closed by a successor to this block.
    ///
    /// If this is `Some`, then this index will either contain the `VariableClose` instruction or
    /// the index for the special `Exit` instruction, which is 1 past the end of the normal block
    /// instruction list.
    pub end: Option<usize>,
}

#[derive(Debug)]
pub struct VariableLiveness {
    live_ranges: SecondaryMap<ir::Variable, HashMap<ir::BlockId, VariableLivenessRange>>,
    live_variables_for_block: SecondaryMap<ir::BlockId, HashSet<ir::Variable>>,
}

impl VariableLiveness {
    /// Compute variable liveness ranges for every block in the given IR. A variable is live after
    /// it is opened and dead when it is closed.
    ///
    /// This also verifies all variables within the IR and their use, namely that:
    ///   1) Every variable has exactly one `OpenVariable` instruction and at most one
    ///      `CloseVariable` instruction.
    ///   2) Every instruction in the CFG has a definite opened or closed state for every variable,
    ///      there are no ambiguous regions.
    ///   3) Every use of a variable (`GetVariable`, `SetVariable`, or `Closure`) is in a
    ///      definitely-open region.
    pub fn compute<S>(ir: &ir::Function<S>) -> Result<Self, VariableVerificationError> {
        let dominators = Dominators::compute(ir.start_block, |b| ir.blocks[b].exit.successors());

        let mut variables: HashSet<ir::Variable> = HashSet::new();
        let mut variable_open: HashMap<ir::Variable, (ir::BlockId, usize)> = HashMap::new();
        let mut variable_uses: HashMap<ir::Variable, Vec<(ir::BlockId, usize)>> = HashMap::new();
        let mut variable_close: HashMap<ir::Variable, (ir::BlockId, usize)> = HashMap::new();

        let topological_order =
            topological_order(ir.start_block, |b| ir.blocks[b].exit.successors());
        for &block_id in &topological_order {
            let block = &ir.blocks[block_id];
            for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
                match ir.instructions[inst_id] {
                    ir::Instruction::OpenVariable(var) => {
                        variables.insert(var);
                        if variable_open.insert(var, (block_id, inst_index)).is_some() {
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
                    ir::Instruction::Closure(func) => {
                        for &var in ir.functions[func].upvalues.values() {
                            variables.insert(var);
                            variable_uses
                                .entry(var)
                                .or_default()
                                .push((block_id, inst_index));
                        }
                    }
                    ir::Instruction::CloseVariable(var) => {
                        variables.insert(var);
                        if variable_close.insert(var, (block_id, inst_index)).is_some() {
                            return Err(VariableVerificationError::MultipleClose);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut this = VariableLiveness {
            live_ranges: SecondaryMap::new(),
            live_variables_for_block: SecondaryMap::new(),
        };

        for &var in &variables {
            if ir.upvalues.contains_key(&var) {
                if variable_open.contains_key(&var) || variable_close.contains_key(&var) {
                    return Err(VariableVerificationError::OpenCloseUpValue);
                }
                continue;
            }

            let &(open_block_id, open_index) = variable_open
                .get(&var)
                .ok_or(VariableVerificationError::BadOpen)?;

            let variable_close = variable_close.get(&var).copied();

            if let Some((close_block_id, close_index)) = variable_close {
                if open_block_id == close_block_id {
                    if close_index < open_index {
                        return Err(VariableVerificationError::CloseNotDominated);
                    }
                } else {
                    if !dominators.dominates(open_block_id, close_block_id).unwrap() {
                        return Err(VariableVerificationError::CloseNotDominated);
                    }
                }
            }

            let mut live_ranges: HashMap<ir::BlockId, VariableLivenessRange> = HashMap::new();

            // DFS from the open block, stopping at the close block.
            //
            // Every block that we encounter should be strictly dominated by the open block
            // (otherwise this would be a block with indeterminate state). Every block that we reach
            // this way has a live range.

            let mut indeterminate_block = false;
            depth_first_search(
                open_block_id,
                |block_id| {
                    let mut range_start = None;
                    let mut range_end = None;

                    if block_id == open_block_id {
                        range_start = Some(open_index);
                    }

                    if let Some((close_block_id, close_index)) = variable_close {
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
                                VariableLivenessRange {
                                    start: range_start,
                                    end: range_end
                                }
                            )
                            .is_none()
                    );

                    if range_end.is_some() {
                        None
                    } else if block.exit.successors().any(|b| {
                        b == open_block_id || !dominators.dominates(open_block_id, b).unwrap()
                    }) {
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
                return Err(VariableVerificationError::IndeterminateState);
            }

            // If we have a close block, then find any indeterminate blocks by ensuring that all
            // blocks reachble from the close block that don't pass through the open block are not
            // live.
            if let Some((close_block_id, _)) = variable_close {
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
                            // As an extra optimization, we don't need to traverse through any
                            // blocks that are not dominated by the open block. We know none of
                            // those blocks are live because we already checked for non-dominted
                            // live blocks above.
                            Some(block.exit.successors().filter(|&b| {
                                b != open_block_id
                                    || !dominators.dominates(open_block_id, b).unwrap()
                            }))
                        }
                        .into_iter()
                        .flatten()
                    },
                    |_| {},
                );

                if indeterminate_block {
                    return Err(VariableVerificationError::IndeterminateState);
                }
            }

            for &(block_id, inst_index) in &variable_uses[&var] {
                let live_range = live_ranges
                    .get(&block_id)
                    .ok_or(VariableVerificationError::UseNotInRange)?;

                if live_range.start.is_some_and(|start| inst_index <= start) {
                    return Err(VariableVerificationError::UseNotInRange);
                }
                if live_range.end.is_some_and(|end| inst_index >= end) {
                    return Err(VariableVerificationError::UseNotInRange);
                }
            }

            for &block_id in live_ranges.keys() {
                this.live_variables_for_block
                    .get_or_insert_default(block_id)
                    .insert(var);
            }
            this.live_ranges.insert(var, live_ranges);
        }

        Ok(this)
    }

    /// Returns all variables that are live anywhere within the given block.
    pub fn live_for_block(
        &self,
        block_id: ir::BlockId,
    ) -> impl Iterator<Item = (ir::Variable, VariableLivenessRange)> + '_ {
        self.live_variables_for_block
            .get(block_id)
            .into_iter()
            .flatten()
            .map(move |&var| (var, self.live_ranges[var][&block_id]))
    }
}

#[cfg(test)]
mod tests {
    use crate::constant::Constant;

    use super::*;

    #[test]
    fn test_variable_liveness_loop_closes() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut variables = ir::VariableSet::new();

        let var = variables.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenVariable(var)));

        block_a.exit = ir::Exit::Jump(block_b_id);

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseVariable(var)));

        block_b.exit = ir::Exit::Jump(block_b_id);

        let ir = ir::Function {
            num_parameters: 0,
            reference: ir::FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables,
            shadow_vars: Default::default(),
            functions: Default::default(),
            upvalues: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            VariableLiveness::compute(&ir),
            Err(VariableVerificationError::IndeterminateState)
        ));
    }

    #[test]
    fn test_variable_liveness_loop_reopens() {
        let mut instructions = ir::InstructionMap::<&'static str>::new();
        let mut blocks = ir::BlockMap::new();
        let mut variables = ir::VariableSet::new();

        let var = variables.insert(());

        let block_a_id = blocks.insert(ir::Block::default());
        let block_b_id = blocks.insert(ir::Block::default());

        let block_a = &mut blocks[block_a_id];

        let troo = instructions.insert(ir::Instruction::Constant(Constant::Boolean(true)));
        block_a.instructions.push(troo);
        block_a
            .instructions
            .push(instructions.insert(ir::Instruction::OpenVariable(var)));

        block_a.exit = ir::Exit::Branch {
            cond: troo,
            if_false: block_b_id,
            if_true: block_a_id,
        };

        let block_b = &mut blocks[block_b_id];

        block_b
            .instructions
            .push(instructions.insert(ir::Instruction::CloseVariable(var)));

        let ir = ir::Function {
            num_parameters: 0,
            reference: ir::FunctionRef::Chunk,
            instructions,
            spans: Default::default(),
            blocks,
            variables,
            shadow_vars: Default::default(),
            functions: Default::default(),
            upvalues: Default::default(),
            start_block: block_a_id,
        };

        assert!(matches!(
            VariableLiveness::compute(&ir),
            Err(VariableVerificationError::IndeterminateState)
        ));
    }
}
