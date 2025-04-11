use std::collections::{hash_map, HashMap};

use thiserror::Error;

use crate::{
    bytecode::{self, ByteCode},
    closure::{self, Prototype},
    compiler::{
        analysis::{
            instruction_liveness::{InstructionLiveness, InstructionVerificationError},
            shadow_liveness::{ShadowLiveness, ShadowVerificationError},
        },
        codegen::register_alloc::RegisterAllocation,
        constant::Constant,
        graph::dfs::topological_order,
        ir,
    },
    instructions::{ConstIdx, HeapIdx, Instruction},
    string::String,
    util::typed_id_map::SecondaryMap,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    InstructionVerification(#[from] InstructionVerificationError),
    #[error(transparent)]
    PhiUpsilonVerification(#[from] ShadowVerificationError),
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

pub fn codegen<'gc>(ir: ir::Function<String<'gc>>) -> Result<Prototype<'gc>, CodegenError> {
    let instruction_liveness = InstructionLiveness::compute(&ir)?;
    let shadow_liveness = ShadowLiveness::compute(&ir)?;

    let reg_alloc = RegisterAllocation::allocate(&ir, &instruction_liveness, &shadow_liveness)
        .ok_or(CodegenError::RegisterOverflow)?;

    let mut heap_vars = SecondaryMap::<ir::Variable, HeapIdx>::new();
    let mut heap_index = 0;
    for var in ir.parts.variables.ids() {
        heap_vars.insert(var, heap_index);
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

    let block_order = topological_order(ir.start_block, |id| ir.parts.blocks[id].exit.successors());

    let block_order_indexes: HashMap<ir::BlockId, usize> = block_order
        .iter()
        .copied()
        .enumerate()
        .map(|(i, b)| (b, i))
        .collect();

    let mut vm_instructions = Vec::new();
    let mut block_vm_starts = SecondaryMap::<ir::BlockId, usize>::new();
    let mut block_vm_jumps = Vec::new();

    for (order_index, &block_id) in block_order.iter().enumerate() {
        let block = &ir.parts.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            match ir.parts.instructions[inst_id] {
                ir::Instruction::NoOp => {}
                ir::Instruction::Copy(source) => {
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    let source_reg = reg_alloc.instruction_registers[source];
                    if dest_reg != source_reg {
                        vm_instructions.push(Instruction::Move {
                            dest: dest_reg,
                            source: source_reg,
                        });
                    }
                }
                ir::Instruction::Undefined => {
                    vm_instructions.push(Instruction::Undefined {
                        dest: reg_alloc.instruction_registers[inst_id],
                    });
                }
                ir::Instruction::Constant(c) => {
                    vm_instructions.push(Instruction::LoadConstant {
                        dest: reg_alloc.instruction_registers[inst_id],
                        constant: constant_indexes[&c],
                    });
                }
                ir::Instruction::GetVariable(var) => {
                    vm_instructions.push(Instruction::GetHeap {
                        dest: reg_alloc.instruction_registers[inst_id],
                        heap: heap_vars[var],
                    });
                }
                ir::Instruction::SetVariable(dest, source) => {
                    vm_instructions.push(Instruction::SetHeap {
                        heap: heap_vars[dest],
                        source: reg_alloc.instruction_registers[source],
                    });
                }
                ir::Instruction::GetThis(key) => {
                    vm_instructions.push(Instruction::GetThis {
                        dest: reg_alloc.instruction_registers[inst_id],
                        key: reg_alloc.instruction_registers[key],
                    });
                }
                ir::Instruction::SetThis { key, value } => {
                    vm_instructions.push(Instruction::SetThis {
                        key: reg_alloc.instruction_registers[key],
                        value: reg_alloc.instruction_registers[value],
                    });
                }
                ir::Instruction::Phi(shadow_id) => {
                    let shadow_reg = reg_alloc.shadow_registers[shadow_id];
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    if shadow_reg != dest_reg {
                        vm_instructions.push(Instruction::Move {
                            dest: dest_reg,
                            source: shadow_reg,
                        });
                    }
                }
                ir::Instruction::Upsilon(shadow_id, source) => {
                    if shadow_liveness
                        .live_range_in_block(block_id, shadow_id)
                        .is_some_and(|r| r.is_live(inst_index))
                    {
                        let shadow_reg = reg_alloc.shadow_registers[shadow_id];
                        let source_reg = reg_alloc.instruction_registers[source];
                        if shadow_reg != source_reg {
                            vm_instructions.push(Instruction::Move {
                                dest: shadow_reg,
                                source: source_reg,
                            });
                        }
                    }
                }
                ir::Instruction::UnOp { source, op } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match op {
                        ir::UnOp::Not => {
                            vm_instructions.push(Instruction::Not {
                                dest: output_reg,
                                arg: reg_alloc.instruction_registers[source],
                            });
                        }
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions.push(Instruction::Add {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                        ir::BinOp::Sub => {
                            vm_instructions.push(Instruction::Sub {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                    }
                }
                ir::Instruction::BinComp { left, right, comp } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match comp {
                        ir::BinComp::LessThan => {
                            vm_instructions.push(Instruction::TestLess {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                        ir::BinComp::LessEqual => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                        ir::BinComp::Equal => {
                            vm_instructions.push(Instruction::TestEqual {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                        ir::BinComp::NotEqual => {
                            vm_instructions.push(Instruction::TestNotEqual {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[left],
                                arg2: reg_alloc.instruction_registers[right],
                            });
                        }
                        ir::BinComp::GreaterThan => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[right],
                                arg2: reg_alloc.instruction_registers[left],
                            });
                        }
                        ir::BinComp::GreaterEqual => {
                            vm_instructions.push(Instruction::TestLess {
                                dest: output_reg,
                                arg1: reg_alloc.instruction_registers[right],
                                arg2: reg_alloc.instruction_registers[left],
                            });
                        }
                    }
                }
                ir::Instruction::Push(source) => {
                    vm_instructions.push(Instruction::Push {
                        source: reg_alloc.instruction_registers[source],
                        len: 1,
                    });
                }
                ir::Instruction::Pop => {
                    vm_instructions.push(Instruction::Pop {
                        dest: reg_alloc.instruction_registers[inst_id],
                        len: 1,
                    });
                }
                ir::Instruction::Call {
                    source,
                    args,
                    returns,
                } => {
                    vm_instructions.push(Instruction::Call {
                        func: reg_alloc.instruction_registers[source],
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
                        arg: reg_alloc.instruction_registers[cond],
                        is_true: true,
                        offset: 0,
                    });
                }

                if *block_order_indexes.get(&if_false).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_false));
                    vm_instructions.push(Instruction::JumpIf {
                        arg: reg_alloc.instruction_registers[cond],
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
        used_registers: reg_alloc.used_registers,
        used_heap: heap_vars.len(),
    })
}
