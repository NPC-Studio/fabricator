use std::collections::{hash_map, HashMap, HashSet};

use arrayvec::ArrayVec;
use thiserror::Error;

use crate::{
    bytecode::{self, ByteCode},
    closure::{self, Prototype},
    compiler::{
        analysis::instruction_liveness::{InstructionLiveness, InstructionVerificationError},
        constant::Constant,
        ir,
    },
    instructions::{ConstIdx, HeapIdx, Instruction, RegIdx},
    util::typed_id_map::SecondaryMap,
    value::String,
};

use super::{
    analysis::shadow_liveness::{PhiUpsilonVerificationError, ShadowLiveness},
    graph::dfs::dfs_post_order,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    InstructionVerification(#[from] InstructionVerificationError),
    #[error(transparent)]
    PhiUpsilonVerification(#[from] PhiUpsilonVerificationError),
    #[error(transparent)]
    ByteCodeEncoding(#[from] bytecode::ByteCodeEncodingError),
    #[error("too many heap variables used")]
    HeapVarOverflow,
    #[error("too many registers used")]
    RegisterOverflow,
    #[error("too many constants used")]
    ConstantOverflow,
    #[error("jump out of range")]
    JumpOutOfRange,
}

pub fn generate<'gc>(ir: ir::Function<String<'gc>>) -> Result<Prototype<'gc>, CodegenError> {
    let instruction_liveness = InstructionLiveness::compute(&ir)?;
    let shadow_liveness = ShadowLiveness::compute(&ir)?;

    let mut heap_vars = SecondaryMap::<ir::VarId, HeapIdx>::new();
    let mut heap_index = 0;
    for var_id in ir.parts.variables.ids() {
        heap_vars.insert(var_id, heap_index);
        heap_index = heap_index
            .checked_add(1)
            .ok_or(CodegenError::HeapVarOverflow)?;
    }

    let mut constants = Vec::new();
    let mut constant_indexes = HashMap::<Constant<String<'gc>>, ConstIdx>::new();

    for block in ir.parts.blocks.values() {
        for &inst_id in &block.instructions {
            if let ir::Instruction::Constant(c) = ir.parts.instructions[inst_id] {
                if let hash_map::Entry::Vacant(vacant) = constant_indexes.entry(c) {
                    vacant.insert(
                        constants
                            .len()
                            .try_into()
                            .map_err(|_| CodegenError::ConstantOverflow)?,
                    );
                    constants.push(c);
                }
            }
        }
    }

    // The reverse of DFS post-order is a topological ordering, we want to iterate from the top
    // down.
    let mut block_order =
        dfs_post_order(ir.start_block, |id| ir.parts.blocks[id].exit.successors());
    block_order.reverse();

    let block_order_indexes: HashMap<ir::BlockId, usize> = block_order
        .iter()
        .copied()
        .enumerate()
        .map(|(i, b)| (b, i))
        .collect();

    let mut instruction_registers = SecondaryMap::<ir::InstId, RegIdx>::new();
    let mut shadow_registers = SecondaryMap::<ir::ShadowVarId, RegIdx>::new();
    let mut used_registers = 0;

    for &block_id in &block_order {
        let block = &ir.parts.blocks[block_id];
        let block_inst_liveness = instruction_liveness.block_liveness(block_id).unwrap();
        let block_shadow_liveness = shadow_liveness.block_liveness(block_id).unwrap();

        let mut live_in_registers = HashSet::new();

        let mut inst_life_starts = HashMap::new();
        let mut inst_life_ends = HashMap::new();
        for (&inst_id, &range) in block_inst_liveness {
            if let Some(start) = range.start {
                assert!(inst_life_starts.insert(start, inst_id).is_none());
            } else {
                live_in_registers.insert(*instruction_registers.get(inst_id).unwrap());
            }

            if let Some(end) = range.end {
                let life_ends = inst_life_ends.entry(end).or_insert_with(|| {
                    ArrayVec::<ir::InstId, { ir::MAX_INSTRUCTION_SOURCES + 1 }>::new()
                });
                life_ends.push(inst_id);
            }
        }

        let mut shadow_life_starts = HashMap::new();
        let mut shadow_life_ends = HashMap::new();
        for (&shadow_id, &range) in block_shadow_liveness {
            // This is extremely confusing, but the intention here is to find the earliest start
            // range for the shadow variable when accounting for incoming and outgoing range
            // overlap.
            let start = if let Some(incoming_range) = range.incoming_range {
                if let Some(incoming_start) = incoming_range.start {
                    if let Some(outgoing_range) = range.outgoing_range {
                        if let Some(outgoing_start) = outgoing_range.start {
                            Some(incoming_start.min(outgoing_start))
                        } else {
                            None
                        }
                    } else {
                        Some(incoming_start)
                    }
                } else {
                    None
                }
            } else {
                range.outgoing_range.unwrap().start
            };

            // Find the end of the liveness range, accounting for both the incoming and outgoing
            // ranges.
            //
            // We are purposefully ignoring the potential liveness gap between the incoming and
            // outgoing ranges.
            let end = if range.outgoing_range.is_some() {
                None
            } else {
                Some(range.incoming_range.unwrap().end)
            };

            if let Some(index) = start {
                assert!(shadow_life_starts.insert(index, shadow_id).is_none());
            } else {
                live_in_registers.insert(*shadow_registers.get(shadow_id).unwrap());
            }
            if let Some(index) = end {
                assert!(shadow_life_ends.insert(index, shadow_id).is_none());
            }
        }

        let mut available_registers = (0u8..=255u8)
            .rev()
            .flat_map(|index| {
                if live_in_registers.contains(&index) {
                    None
                } else {
                    Some(index)
                }
            })
            .collect::<Vec<_>>();

        for inst_index in 0..=block.instructions.len() {
            if let Some(&inst_life_start) = inst_life_starts.get(&inst_index) {
                let reg = available_registers
                    .pop()
                    .ok_or(CodegenError::RegisterOverflow)?;
                used_registers = used_registers.max(reg as usize + 1);
                assert!(instruction_registers.insert(inst_life_start, reg).is_none());
            }

            for &inst_life_end in inst_life_ends.get(&inst_index).into_iter().flatten() {
                available_registers
                    .push(instruction_registers.get(inst_life_end).copied().unwrap());
            }

            if let Some(&shadow_life_start) = shadow_life_starts.get(&inst_index) {
                let reg = available_registers
                    .pop()
                    .ok_or(CodegenError::RegisterOverflow)?;
                used_registers = used_registers.max(reg as usize + 1);
                assert!(shadow_registers.insert(shadow_life_start, reg,).is_none());
            }

            if let Some(&shadow_life_end) = shadow_life_ends.get(&inst_index) {
                available_registers.push(shadow_registers.get(shadow_life_end).copied().unwrap());
            }
        }
    }

    let mut vm_instructions = Vec::new();
    let mut block_vm_starts = SecondaryMap::<ir::BlockId, usize>::new();
    let mut block_vm_jumps = Vec::new();

    for (order_index, &block_id) in block_order.iter().enumerate() {
        let block = &ir.parts.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            match ir.parts.instructions[inst_id] {
                ir::Instruction::Constant(c) => {
                    vm_instructions.push(Instruction::LoadConstant {
                        constant: constant_indexes[&c],
                        dest: instruction_registers[inst_id],
                    });
                }
                ir::Instruction::GetVariable(var_id) => {
                    vm_instructions.push(Instruction::GetHeap {
                        heap: heap_vars[var_id],
                        dest: instruction_registers[inst_id],
                    });
                }
                ir::Instruction::SetVariable(dest, source) => {
                    vm_instructions.push(Instruction::SetHeap {
                        source: instruction_registers[source],
                        heap: heap_vars[dest],
                    });
                }
                ir::Instruction::Phi(shadow_id) => {
                    vm_instructions.push(Instruction::Move {
                        source: shadow_registers[shadow_id],
                        dest: instruction_registers[inst_id],
                    });
                }
                ir::Instruction::Upsilon(shadow_id, source) => {
                    if shadow_liveness.block_liveness(block_id).unwrap()[&shadow_id]
                        .is_live(inst_index)
                    {
                        vm_instructions.push(Instruction::Move {
                            source: instruction_registers[source],
                            dest: shadow_registers[shadow_id],
                        });
                    }
                }
                ir::Instruction::UnOp { source, op } => {
                    let output_reg = instruction_registers[inst_id];
                    match op {
                        ir::UnOp::Not => {
                            vm_instructions.push(Instruction::Not {
                                arg: instruction_registers[source],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    let output_reg = instruction_registers[inst_id];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions.push(Instruction::Add {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinOp::Sub => {
                            vm_instructions.push(Instruction::Sub {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::BinComp { left, right, comp } => {
                    let output_reg = instruction_registers[inst_id];
                    match comp {
                        ir::BinComp::LessThan => {
                            vm_instructions.push(Instruction::TestLess {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::LessEqual => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::Equal => {
                            vm_instructions.push(Instruction::TestEqual {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::NotEqual => {
                            vm_instructions.push(Instruction::TestNotEqual {
                                arg1: instruction_registers[left],
                                arg2: instruction_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::GreaterThan => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                arg1: instruction_registers[right],
                                arg2: instruction_registers[left],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::GreaterEqual => {
                            vm_instructions.push(Instruction::TestLess {
                                arg1: instruction_registers[right],
                                arg2: instruction_registers[left],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::Push(source) => {
                    vm_instructions.push(Instruction::Push {
                        source: instruction_registers[source],
                        len: 1,
                    });
                }
                ir::Instruction::Pop => {
                    vm_instructions.push(Instruction::Pop {
                        dest: instruction_registers[inst_id],
                        len: 1,
                    });
                }
                ir::Instruction::Call {
                    source,
                    args,
                    returns,
                } => {
                    vm_instructions.push(Instruction::Call {
                        func: instruction_registers[source],
                        args,
                        returns,
                    });
                }
            }
        }

        match block.exit {
            ir::Exit::Return { returns } => {
                vm_instructions.push(Instruction::Return { returns });
            }
            ir::Exit::Jump(block_id) => {
                // If we are the next block in output order, we don't need to add a jump
                if *block_order_indexes.get(&block_id).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), block_id));
                    vm_instructions.push(Instruction::Jump { offset: 0 });
                }
            }
            ir::Exit::Branch {
                cond,
                if_true,
                if_false,
            } => {
                // If we are the next block in output order, we don't need to add a jump.

                if *block_order_indexes.get(&if_true).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_true));
                    vm_instructions.push(Instruction::JumpIf {
                        arg: instruction_registers[cond],
                        is_true: true,
                        offset: 0,
                    });
                }

                if *block_order_indexes.get(&if_false).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_false));
                    vm_instructions.push(Instruction::JumpIf {
                        arg: instruction_registers[cond],
                        is_true: false,
                        offset: 0,
                    });
                }
            }
        }
    }

    for (index, block_id) in block_vm_jumps {
        let jump_offset = (block_vm_starts[block_id] as isize - index as isize)
            .try_into()
            .map_err(|_| CodegenError::JumpOutOfRange)?;
        match &mut vm_instructions[index] {
            Instruction::Jump { offset } => {
                *offset = jump_offset;
            }
            Instruction::JumpIf { offset, .. } => {
                *offset = jump_offset;
            }
            _ => panic!("instruction not a jump"),
        }
    }

    let bytecode = ByteCode::encode(&vm_instructions)?;

    let constants = constants
        .into_iter()
        .map(|c| match c {
            Constant::Undefined => closure::Constant::Undefined,
            Constant::Boolean(b) => closure::Constant::Boolean(b),
            Constant::Integer(i) => {
                if let Some(i) = i.try_into().ok() {
                    closure::Constant::Integer(i)
                } else {
                    closure::Constant::Float(i as f64)
                }
            }
            Constant::Float(f) => closure::Constant::Float(f),
            Constant::String(s) => closure::Constant::String(s),
        })
        .collect::<Vec<_>>();

    Ok(Prototype {
        bytecode,
        constants: constants.into_boxed_slice(),
        used_registers,
        used_heap: heap_vars.len(),
    })
}
