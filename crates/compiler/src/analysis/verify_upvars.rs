use fabricator_util::typed_id_map::SecondaryMap;
use thiserror::Error;

use crate::ir;

#[derive(Debug, Error)]
pub enum UpVarVerificationError {
    #[error("{function} references the same upper variable as {first} and {second}")]
    AliasedUpVar {
        function: ir::FuncId,
        first: ir::VarId,
        second: ir::VarId,
    },
    #[error("{function} has variable {variable} which references a non-existent upper variable")]
    MissingUpVar {
        function: ir::FuncId,
        variable: ir::VarId,
    },
    #[error("root function has an upper variable {variable}")]
    UpVarOnRoot { variable: ir::VarId },
}

/// Verify all upvars in immediate inner functions.
pub fn verify_upvars<S>(ir: &ir::Function<S>) -> Result<(), UpVarVerificationError> {
    let mut upper_refs = SecondaryMap::new();
    for (func_id, func) in ir.functions.iter() {
        upper_refs.clear();

        for (var_id, var) in func.variables.iter() {
            if let ir::Variable::Upper(upper_var_id) = *var {
                if let Some((_, prev_var_id)) = upper_refs.insert(upper_var_id, var_id) {
                    return Err(UpVarVerificationError::AliasedUpVar {
                        function: func_id,
                        first: prev_var_id,
                        second: var_id,
                    });
                }

                if !ir.variables.contains(upper_var_id) {
                    return Err(UpVarVerificationError::MissingUpVar {
                        function: func_id,
                        variable: var_id,
                    });
                }
            }
        }
    }

    Ok(())
}

/// Verify that the given root function has no upvars.
pub fn verify_no_root_upvars<S>(ir: &ir::Function<S>) -> Result<(), UpVarVerificationError> {
    for (var_id, var) in ir.variables.iter() {
        if let ir::Variable::Upper(_) = var {
            return Err(UpVarVerificationError::UpVarOnRoot { variable: var_id });
        }
    }

    Ok(())
}
