use thiserror::Error;

use crate::ir;

#[derive(Debug, Error)]
pub enum ReferenceVerificationError {
    #[error("instruction {} is invalid", inst_location)]
    BadInstruction {
        bad: ir::InstId,
        inst_location: ir::InstLocation,
    },
    #[error("{} successor is invalid", block)]
    BadSuccessor {
        bad: ir::BlockId,
        block: ir::BlockId,
    },
    #[error("instruction {} references invalid source", inst_location)]
    BadSource {
        bad: ir::InstId,
        inst_location: ir::InstLocation,
    },
    #[error("instruction {} references invalid variable", inst_location)]
    BadVariable {
        bad: ir::VarId,
        inst_location: ir::InstLocation,
    },
    #[error("instruction {} references invalid shadow variable", inst_location)]
    BadShadowVar {
        bad: ir::ShadowVar,
        inst_location: ir::InstLocation,
    },
    #[error("instruction {} references invalid 'this' scope", inst_location)]
    BadThisScope {
        bad: ir::ThisScope,
        inst_location: ir::InstLocation,
    },
    #[error("instruction {} references invalid sub-function", inst_location)]
    BadFunction {
        bad: ir::FuncId,
        inst_location: ir::InstLocation,
    },
}

/// Verify all references in all blocks and instructions.
///
/// Checks all blocks, does not consider block reachability.
pub fn verify_references<S>(ir: &ir::Function<S>) -> Result<(), ReferenceVerificationError> {
    for (block_id, block) in ir.blocks.iter() {
        for succ in block.exit.successors() {
            if !ir.blocks.contains(block_id) {
                return Err(ReferenceVerificationError::BadSuccessor {
                    bad: succ,
                    block: block_id,
                });
            }
        }

        for (inst_index, inst_id) in block.instructions.iter().copied().enumerate() {
            let inst_location = ir::InstLocation::new(block_id, inst_index);
            let inst =
                ir.instructions
                    .get(inst_id)
                    .ok_or(ReferenceVerificationError::BadInstruction {
                        bad: inst_id,
                        inst_location,
                    })?;

            for source in inst.sources() {
                if !ir.instructions.contains(source) {
                    return Err(ReferenceVerificationError::BadSource {
                        bad: source,
                        inst_location,
                    });
                }
            }

            match *inst {
                ir::Instruction::OpenVariable(var_id)
                | ir::Instruction::GetVariable(var_id)
                | ir::Instruction::SetVariable(var_id, _)
                | ir::Instruction::CloseVariable(var_id) => {
                    if !ir.variables.contains(var_id) {
                        return Err(ReferenceVerificationError::BadVariable {
                            bad: var_id,
                            inst_location,
                        });
                    }
                }

                ir::Instruction::Phi(shadow_var) | ir::Instruction::Upsilon(shadow_var, _) => {
                    if !ir.shadow_vars.contains(shadow_var) {
                        return Err(ReferenceVerificationError::BadShadowVar {
                            bad: shadow_var,
                            inst_location: ir::InstLocation::new(block_id, inst_index),
                        });
                    }
                }

                ir::Instruction::OpenThisScope(scope)
                | ir::Instruction::SetThis(scope, _)
                | ir::Instruction::CloseThisScope(scope) => {
                    if !ir.this_scopes.contains(scope) {
                        return Err(ReferenceVerificationError::BadThisScope {
                            bad: scope,
                            inst_location,
                        });
                    }
                }

                ir::Instruction::Closure(func_id) => {
                    if !ir.functions.contains(func_id) {
                        return Err(ReferenceVerificationError::BadFunction {
                            bad: func_id,
                            inst_location,
                        });
                    }
                }

                _ => {}
            }
        }
    }

    Ok(())
}
