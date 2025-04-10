use crate::compiler::ir;

use super::shadow_liveness::{ShadowLiveness, ShadowVerificationError};

pub fn remove_dead_upsilons<S>(ir: &mut ir::Function<S>) -> Result<(), ShadowVerificationError> {
    let shadow_liveness = ShadowLiveness::compute(ir)?;

    for (block_id, block) in ir.parts.blocks.iter() {
        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            let inst = &mut ir.parts.instructions[inst_id];
            if let &ir::Instruction::Upsilon(shadow_var, _) = &*inst {
                let is_live = shadow_liveness
                    .live_range_in_block(block_id, shadow_var)
                    .is_some_and(|r| r.is_live(inst_index));

                if !is_live {
                    *inst = ir::Instruction::NoOp;
                }
            }
        }
    }

    Ok(())
}
