use std::collections::hash_map;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir;

/// Combine is_true and is_false branch conditions that reference test operations into just
/// branches, if possible.
pub fn simplify_branch_conditions<S>(ir: &mut ir::Function<S>) {
    // Find all the branch source instructions for is_true and is_false that are:
    //   1) Compatible testing UnOp or BinOp instructions.
    //   2) Not used by any other instruction or branch.
    //   3) Have no effects (supported UnOp and BinOp instructions can only error).

    let mut unique_branch_sources = FxHashMap::default();
    let mut duplicate_branch_sources = FxHashSet::default();

    for (block_id, block) in ir.blocks.iter_mut() {
        if let ir::ExitKind::Branch { cond, .. } = &mut block.exit.kind {
            match *cond {
                ir::BranchCondition::IsTrue(inst_id) | ir::BranchCondition::IsFalse(inst_id) => {
                    if !ir.instructions[inst_id].effects.has_effect() {
                        match unique_branch_sources.entry(inst_id) {
                            hash_map::Entry::Occupied(occupied) => {
                                // If this instruction was been used by another branch, then it is
                                // not uniquely used.
                                duplicate_branch_sources.insert(*occupied.key());
                            }
                            hash_map::Entry::Vacant(vacant) => {
                                vacant.insert(block_id);
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    for inst_id in duplicate_branch_sources {
        unique_branch_sources.remove(&inst_id);
    }

    for block in ir.blocks.values_mut() {
        for &inst_id in &block.instructions {
            // If an instruction is used by another instruction in any block, then it is not
            // uniquely used.
            for source_inst_id in ir.instructions[inst_id].kind.sources() {
                unique_branch_sources.remove(&source_inst_id);
            }
        }
    }

    // Now replace those conditions with an equivalent instruction that folds in the source
    // instruction, if possible.

    fn get_equivalent_condition<S>(
        test_inst: &ir::InstructionKind<S>,
    ) -> Option<ir::BranchCondition> {
        match *test_inst {
            ir::InstructionKind::UnOp { op, source } => match op {
                ir::UnOp::IsDefined => Some(ir::BranchCondition::IsDefined(source)),
                ir::UnOp::IsUndefined => Some(ir::BranchCondition::IsUndefined(source)),
                ir::UnOp::Test => Some(ir::BranchCondition::IsTrue(source)),
                ir::UnOp::Not => Some(ir::BranchCondition::IsFalse(source)),
                _ => None,
            },
            ir::InstructionKind::BinOp { left, op, right } => match op {
                ir::BinOp::Equal => Some(ir::BranchCondition::Equal(left, right)),
                ir::BinOp::NotEqual => Some(ir::BranchCondition::NotEqual(left, right)),
                ir::BinOp::LessThan => Some(ir::BranchCondition::LessThan(left, right)),
                ir::BinOp::LessEqual => Some(ir::BranchCondition::LessEqual(left, right)),
                ir::BinOp::GreaterThan => Some(ir::BranchCondition::GreaterThan(left, right)),
                ir::BinOp::GreaterEqual => Some(ir::BranchCondition::GreaterEqual(left, right)),
                _ => None,
            },
            _ => None,
        }
    }

    for &block_id in unique_branch_sources.values() {
        let ir::ExitKind::Branch { cond, .. } = &mut ir.blocks[block_id].exit.kind else {
            unreachable!()
        };

        match *cond {
            ir::BranchCondition::IsTrue(inst_id) => {
                if let Some(simplified_cond) =
                    get_equivalent_condition(&ir.instructions[inst_id].kind)
                {
                    *cond = simplified_cond;
                    ir.instructions[inst_id].set_kind(ir::InstructionKind::NoOp);
                }
            }
            ir::BranchCondition::IsFalse(inst_id) => {
                if let Some(simplified_cond) =
                    get_equivalent_condition(&ir.instructions[inst_id].kind)
                {
                    *cond = simplified_cond.reverse();
                    ir.instructions[inst_id].set_kind(ir::InstructionKind::NoOp);
                }
            }
            _ => {}
        }
    }
}
