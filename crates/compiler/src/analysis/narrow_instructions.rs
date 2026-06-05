use rustc_hash::FxHashMap;

use crate::{graph::dfs::topological_order, ir};

/// Narrow instruction types and effects based on inter-instruction information, and simplify
/// instructions where type information allows us to do so.
pub fn narrow_instructions<S>(ir: &mut ir::Function<S>) {
    let reachable_blocks =
        topological_order(ir.start_block, |b| ir.blocks[b].exit.kind.successors());

    let mut shadow_upsilon_sources: FxHashMap<ir::ShadowVar, Vec<ir::InstId>> =
        FxHashMap::default();

    // Iterate in topological order so we always encounter instruction sources before their uses.
    for &block_id in &reachable_blocks {
        let block = &mut ir.blocks[block_id];
        for &inst_id in &block.instructions {
            match ir.instructions[inst_id].kind {
                ir::InstructionKind::Upsilon(shadow, source_inst) => {
                    shadow_upsilon_sources
                        .entry(shadow)
                        .or_default()
                        .push(source_inst);
                }
                ir::InstructionKind::Phi(shadow) => {
                    let mut output_type = None;
                    for &source_inst in &shadow_upsilon_sources[&shadow] {
                        let source_type = ir.instructions[source_inst].output_type;

                        output_type = Some(if let Some(output_type) = output_type {
                            if output_type == source_type {
                                output_type
                            } else {
                                ir::OutputType::Any
                            }
                        } else {
                            source_type
                        });
                    }

                    ir.instructions[inst_id].output_type = output_type.unwrap();
                }
                ir::InstructionKind::Copy(source) => {
                    let source_type = ir.instructions[source].output_type;
                    ir.instructions[inst_id].output_type = source_type;
                }
                ir::InstructionKind::UnOp { op, source } => {
                    let source_type = ir.instructions[source].output_type;
                    let inst = &mut ir.instructions[inst_id];
                    match (op, source_type) {
                        (
                            ir::UnOp::Negate
                            | ir::UnOp::BitNegate
                            | ir::UnOp::Increment
                            | ir::UnOp::Decrement,
                            ir::OutputType::Scalar,
                        ) => {
                            inst.effects.can_error = false;
                        }
                        _ => {}
                    }
                }
                ir::InstructionKind::BinOp { left, op, right } => {
                    let left_type = ir.instructions[left].output_type;
                    let right_type = ir.instructions[right].output_type;
                    let inst = &mut ir.instructions[inst_id];
                    match op {
                        ir::BinOp::NullCoalesce => {
                            if left_type == ir::OutputType::Undefined {
                                inst.set_kind(ir::InstructionKind::Copy(right));
                                inst.output_type = right_type;
                            } else if left_type != ir::OutputType::Any {
                                inst.set_kind(ir::InstructionKind::Copy(left));
                                inst.output_type = left_type;
                            };
                        }
                        ir::BinOp::Add
                        | ir::BinOp::Sub
                        | ir::BinOp::Mult
                        | ir::BinOp::Div
                        | ir::BinOp::Rem
                        | ir::BinOp::IDiv
                        | ir::BinOp::LessThan
                        | ir::BinOp::LessEqual
                        | ir::BinOp::GreaterThan
                        | ir::BinOp::GreaterEqual
                        | ir::BinOp::BitAnd
                        | ir::BinOp::BitOr
                        | ir::BinOp::BitXor
                        | ir::BinOp::BitShiftLeft
                        | ir::BinOp::BitShiftRight => {
                            if left_type == ir::OutputType::Scalar
                                && right_type == ir::OutputType::Scalar
                            {
                                inst.effects.can_error = false;
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }
}
