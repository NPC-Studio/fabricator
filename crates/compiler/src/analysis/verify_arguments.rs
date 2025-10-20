use thiserror::Error;

use crate::ir;

#[derive(Debug, Error)]
#[error("instruction at {location} references argument that is out of range")]
pub struct ArgumentVerificationError {
    pub location: ir::InstLocation,
    pub argument: usize,
}

/// Verify all references in all blocks and instructions.
///
/// Checks all blocks, does not consider block reachability.
pub fn verify_arguments<S>(ir: &ir::Function<S>) -> Result<(), ArgumentVerificationError> {
    for (block_id, block) in ir.blocks.iter() {
        for (inst_index, inst_id) in block.instructions.iter().copied().enumerate() {
            if let ir::InstructionKind::FixedArgument(index) = ir.instructions[inst_id].kind {
                if index >= ir.num_parameters {
                    return Err(ArgumentVerificationError {
                        location: ir::InstLocation::new(block_id, inst_index),
                        argument: index,
                    });
                }
            }
        }
    }

    Ok(())
}
