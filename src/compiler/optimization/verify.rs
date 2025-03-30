use thiserror::Error;

use crate::{
    compiler::{dominators::Dominators, ir},
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum VerificationError {
    #[error("instruction appears multiple times")]
    InstructionReused,
    #[error("instruction does not dominate its uses")]
    UseNotDominated,
}

pub fn verify_ir<S>(function: &ir::Function<S>) -> Result<(), VerificationError> {
    let block_dominance = Dominators::compute(function.start_block, |b| {
        function.parts.blocks[b].exit.successors()
    });

    let mut inst_positions = SecondaryMap::new();
    for (block_id, block) in function.parts.blocks.iter() {
        for (i, &inst_id) in block.instructions.iter().enumerate() {
            if inst_positions.insert(inst_id, (block_id, i)).is_some() {
                return Err(VerificationError::InstructionReused);
            }
        }
    }

    for (inst_id, inst) in function.parts.instructions.iter() {
        let (user_block, user_index) = inst_positions[inst_id];
        let check_use = |source_inst| -> Result<(), VerificationError> {
            let (source_block, source_index) = inst_positions[source_inst];
            if user_block == source_block {
                if source_index < user_index {
                    Ok(())
                } else {
                    Err(VerificationError::UseNotDominated)
                }
            } else {
                if block_dominance.dominates(source_block, user_block).unwrap() {
                    Ok(())
                } else {
                    Err(VerificationError::UseNotDominated)
                }
            }
        };

        for source in inst.sources() {
            check_use(source)?;
        }
    }

    Ok(())
}
