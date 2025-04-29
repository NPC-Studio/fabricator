use crate::compiler::{
    analysis::shadow_liveness::{ShadowLiveness, ShadowVerificationError},
    ir,
};

/// Removes dead upsilon instructions and simplifies instructions in phi / upsilon form to SSA where
/// possible.
pub fn reduce_shadows<S>(ir: &mut ir::Function<S>) -> Result<(), ShadowVerificationError> {
    let shadow_liveness = ShadowLiveness::compute(ir)?;

    // Replace dead upsilon instructions with `ir::Instruction::NoOp`.

    for (block_id, block) in ir.blocks.iter() {
        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            let inst = &mut ir.instructions[inst_id];
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

    // For each shadow variable, check and see if we can convert it to SSA form. We check to see if
    // it is "trivial", i.e. every upsilon sets the value from the same source instruction. If it
    // is, remove the upsilons and replace the phi with the source instruction.

    for shadow_var in shadow_liveness.live_shadow_vars() {
        // The shadow liveness ranges always contain every phi instruction as an `end` field and
        // every (live) upsilon instruction as a `start` field. Use this to find the phi and every
        // upsilon instruction.

        let mut phi = None;
        let mut upsilons = Vec::new();

        for (block_id, range) in shadow_liveness.live_ranges(shadow_var) {
            let block = &ir.blocks[block_id];
            if let Some(incoming) = range.incoming_range {
                phi = Some(block.instructions[incoming.end]);

                if let Some(start) = incoming.start {
                    upsilons.push(block.instructions[start]);
                }
            }

            if let Some(outgoing) = range.outgoing_range {
                if let Some(start) = outgoing.start {
                    upsilons.push(block.instructions[start]);
                }
            }
        }

        let phi = phi.unwrap();

        let get_upsilon_source = |inst_id| {
            if let ir::Instruction::Upsilon(_, source) = ir.instructions[inst_id] {
                source
            } else {
                unreachable!();
            }
        };

        let first_source = get_upsilon_source(upsilons[0]);
        let mut trivial = true;
        for &inst_id in &upsilons[1..] {
            if get_upsilon_source(inst_id) != first_source {
                trivial = false;
                break;
            }
        }

        if trivial {
            ir.instructions[phi] = ir::Instruction::Copy(first_source);
            for &inst_id in &upsilons {
                ir.instructions[inst_id] = ir::Instruction::NoOp;
            }
        }
    }

    Ok(())
}
