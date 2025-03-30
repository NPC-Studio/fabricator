use thiserror::Error;

use crate::{
    compiler::{dominators::Dominators, ir},
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum IrVerificationError {
    #[error("instruction appears multiple times")]
    InstructionReused,
    #[error("instruction does not dominate its uses")]
    UseNotDominated,
}

pub fn verify_ir<S>(function: &ir::Function<S>) -> Result<(), IrVerificationError> {
    let block_dominance = Dominators::compute(function.start_block, |b| {
        match function.parts.blocks.get(b).unwrap().exit {
            ir::Exit::Return { .. } => [None, None].into_iter().flatten(),
            ir::Exit::Jump(block_id) => [Some(block_id), None].into_iter().flatten(),
            ir::Exit::Branch {
                if_true, if_false, ..
            } => [Some(if_true), Some(if_false)].into_iter().flatten(),
        }
    });

    let mut inst_positions = SecondaryMap::new();
    for (block_id, block) in function.parts.blocks.iter() {
        for (i, &inst_id) in block.instructions.iter().enumerate() {
            dbg!(inst_id);
            if inst_positions.insert(inst_id, (block_id, i)).is_some() {
                return Err(IrVerificationError::InstructionReused);
            }
        }
    }

    for (inst_id, inst) in function.parts.instructions.iter() {
        let &(user_block, user_index) = inst_positions.get(inst_id).unwrap();
        let check_use = |target_inst| -> Result<(), IrVerificationError> {
            let &(target_block, target_index) = inst_positions.get(target_inst).unwrap();
            if user_block == target_block {
                if target_index < user_index {
                    Ok(())
                } else {
                    Err(IrVerificationError::UseNotDominated)
                }
            } else {
                if block_dominance.dominates(target_block, user_block).unwrap() {
                    Ok(())
                } else {
                    Err(IrVerificationError::UseNotDominated)
                }
            }
        };

        match inst {
            ir::Instruction::SetVariable { source, .. } => {
                check_use(*source)?;
            }
            ir::Instruction::UnOp { source, .. } => {
                check_use(*source)?;
            }
            ir::Instruction::BinOp { left, right, .. } => {
                check_use(*left)?;
                check_use(*right)?;
            }
            ir::Instruction::BinComp { left, right, .. } => {
                check_use(*left)?;
                check_use(*right)?;
            }
            ir::Instruction::Push { source } => {
                check_use(*source)?;
            }
            ir::Instruction::Call { source, .. } => {
                check_use(*source)?;
            }
            _ => {}
        }
    }

    Ok(())
}
