use thiserror::Error;

use crate::ir;

#[derive(Debug, Error)]
pub enum ReferenceVerificationError {
    #[error("instruction B{}:{} is invalid", inst_location.0.index(), inst_location.1)]
    BadInstruction {
        bad: ir::InstId,
        inst_location: (ir::BlockId, usize),
    },
    #[error("B{} successor is invalid", block.index())]
    BadSuccessor {
        bad: ir::BlockId,
        block: ir::BlockId,
    },
    #[error("instruction B{}:{} references invalid source", inst_location.0.index(), inst_location.1)]
    BadSource {
        bad: ir::InstId,
        inst_location: (ir::BlockId, usize),
    },
    #[error("instruction B{}:{} references invalid variable", inst_location.0.index(), inst_location.1)]
    BadVariable {
        bad: ir::VarId,
        inst_location: (ir::BlockId, usize),
    },
    #[error("instruction B{}:{} references invalid shadow variable", inst_location.0.index(), inst_location.1)]
    BadShadowVar {
        bad: ir::ShadowVar,
        inst_location: (ir::BlockId, usize),
    },
    #[error("instruction B{}:{} references invalid 'this' scope", inst_location.0.index(), inst_location.1)]
    BadThisScope {
        bad: ir::ThisScope,
        inst_location: (ir::BlockId, usize),
    },
    #[error("instruction B{}:{} references invalid sub-function", inst_location.0.index(), inst_location.1)]
    BadFunction {
        bad: ir::FuncId,
        inst_location: (ir::BlockId, usize),
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
            let inst = ir.instructions.get(inst_id).ok_or_else(|| {
                ReferenceVerificationError::BadInstruction {
                    bad: inst_id,
                    inst_location: (block_id, inst_index),
                }
            })?;

            for source in inst.sources() {
                if !ir.instructions.contains(source) {
                    return Err(ReferenceVerificationError::BadSource {
                        bad: source,
                        inst_location: (block_id, inst_index),
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
                            inst_location: (block_id, inst_index),
                        });
                    }
                }

                ir::Instruction::Phi(shadow_var) | ir::Instruction::Upsilon(shadow_var, _) => {
                    if !ir.shadow_vars.contains(shadow_var) {
                        return Err(ReferenceVerificationError::BadShadowVar {
                            bad: shadow_var,
                            inst_location: (block_id, inst_index),
                        });
                    }
                }

                ir::Instruction::OpenThisScope(this_scope, _)
                | ir::Instruction::CloseThisScope(this_scope) => {
                    if !ir.this_scopes.contains(this_scope) {
                        return Err(ReferenceVerificationError::BadThisScope {
                            bad: this_scope,
                            inst_location: (block_id, inst_index),
                        });
                    }
                }

                ir::Instruction::Closure(func_id) => {
                    if !ir.functions.contains(func_id) {
                        return Err(ReferenceVerificationError::BadFunction {
                            bad: func_id,
                            inst_location: (block_id, inst_index),
                        });
                    }
                }

                _ => {}
            }
        }
    }

    Ok(())
}
