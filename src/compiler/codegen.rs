use std::collections::HashMap;

use arrayvec::ArrayVec;
use thiserror::Error;

use crate::{
    closure::Prototype,
    compiler::{
        constant::Constant,
        dominators::Dominators,
        ir::{self, InstId, MAX_INSTRUCTION_SOURCES},
        optimization,
    },
    instructions::RegIdx,
    util::typed_id_map::SecondaryMap,
    value::String,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    IrVerification(#[from] optimization::verify::VerificationError),
    #[error("too many registers used")]
    RegisterOverflow,
}

pub fn generate<'gc>(function: ir::Function<String<'gc>>) -> Result<Prototype<'gc>, CodegenError> {
    optimization::verify::verify_ir(&function)?;

    let block_dominance = Dominators::compute(function.start_block, |b| {
        function.parts.blocks[b].exit.successors()
    });

    let block_order = block_dominance.dfs_pre_order().collect::<Vec<_>>();

    let mut inst_range_ends = SecondaryMap::new();

    let mut inst_index = 0;
    for &block_id in &block_order {
        let block = &function.parts.blocks[block_id];
        for &inst_id in &block.instructions {
            inst_range_ends.insert(inst_id, inst_index);

            for source in function.parts.instructions[inst_id].sources() {
                inst_range_ends[source] = inst_index;
            }

            inst_index += 1;
        }
    }

    let mut inst_scope_ends =
        (0..inst_index).map(|_| ArrayVec::<InstId, MAX_INSTRUCTION_SOURCES>::new());

    let mut available_registers = (RegIdx::MIN..=RegIdx::MAX).rev().collect::<Vec<_>>();
    let mut assigned_registers = SecondaryMap::<InstId, RegIdx>::new();

    let mut constants = HashMap::<Constant<String<'gc>>, usize>::new();
    let mut const_index = 0;

    let mut inst_index = 0;
    for &block_id in &block_order {
        let block = &function.parts.blocks[block_id];
        for &inst_id in &block.instructions {
            let inst = function.parts.instructions[inst_id];

            inst_index += 1;
        }
    }

    todo!()
}
