use std::collections::{hash_map, HashMap};

use arrayvec::ArrayVec;
use thiserror::Error;

use crate::{
    bytecode::{self, ByteCode},
    closure::{self, Prototype},
    compiler::{
        constant::Constant,
        dominators::Dominators,
        ir::{self, BlockId, InstId, VarId, MAX_INSTRUCTION_SOURCES},
        optimization,
    },
    instructions::{ConstIdx, HeapIdx, Instruction, RegIdx},
    util::typed_id_map::SecondaryMap,
    value::String,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    IrVerification(#[from] optimization::verify::VerificationError),
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

pub fn generate<'gc>(function: ir::Function<String<'gc>>) -> Result<Prototype<'gc>, CodegenError> {
    optimization::verify::verify_ir(&function)?;

    let mut heap_vars = SecondaryMap::<VarId, HeapIdx>::new();
    let mut heap_index = 0;
    for var_id in function.parts.heap_vars.ids() {
        heap_vars.insert(var_id, heap_index);
        heap_index = heap_index
            .checked_add(1)
            .ok_or(CodegenError::HeapVarOverflow)?;
    }

    let block_dominance = Dominators::compute(function.start_block, |b| {
        function.parts.blocks[b].exit.successors()
    });

    let block_order = block_dominance.dfs_pre_order().collect::<Vec<_>>();

    let mut inst_range_ends = SecondaryMap::new();
    let mut inst_index = 0;

    let mut constants = Vec::new();
    let mut constant_indexes = HashMap::<Constant<String<'gc>>, ConstIdx>::new();

    for &block_id in &block_order {
        let block = &function.parts.blocks[block_id];
        for &inst_id in &block.instructions {
            let inst = function.parts.instructions[inst_id];
            if inst.has_value() {
                inst_range_ends.insert(inst_id, inst_index);
            }

            for source in inst.sources() {
                *inst_range_ends.get_mut(source).unwrap() = inst_index;
            }

            if let ir::Instruction::Constant(c) = inst {
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

            inst_index += 1;
        }
    }

    let inst_scope_ends = (0..inst_index)
        .map(|_| ArrayVec::<InstId, MAX_INSTRUCTION_SOURCES>::new())
        .collect::<Vec<_>>();

    let mut available_registers = (RegIdx::MIN..=RegIdx::MAX).rev().collect::<Vec<_>>();
    let mut assigned_registers = SecondaryMap::<InstId, RegIdx>::new();

    let mut inst_index = 0;
    let mut used_registers = 0;
    for &block_id in &block_order {
        let block = &function.parts.blocks[block_id];
        for &inst_id in &block.instructions {
            let inst = function.parts.instructions[inst_id];
            if inst.has_value() {
                let reg = available_registers
                    .pop()
                    .ok_or(CodegenError::RegisterOverflow)?;
                used_registers = used_registers.max(reg as usize);

                assigned_registers.insert(inst_id, reg);
            }

            for &end_inst in &inst_scope_ends[inst_index] {
                available_registers.push(assigned_registers[end_inst]);
            }

            inst_index += 1;
        }
    }

    let mut vm_instructions = Vec::new();
    let mut block_vm_starts = SecondaryMap::<BlockId, usize>::new();
    let mut block_vm_jumps = Vec::new();

    for &block_id in &block_order {
        let block = &function.parts.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for &inst_id in &block.instructions {
            match function.parts.instructions[inst_id] {
                ir::Instruction::Constant(c) => {
                    vm_instructions.push(Instruction::LoadConstant {
                        constant: constant_indexes[&c],
                        dest: assigned_registers[inst_id],
                    });
                }
                ir::Instruction::GetVariable(var_id) => {
                    vm_instructions.push(Instruction::GetHeap {
                        heap: heap_vars[var_id],
                        dest: assigned_registers[inst_id],
                    });
                }
                ir::Instruction::SetVariable { source, dest } => {
                    vm_instructions.push(Instruction::SetHeap {
                        source: assigned_registers[source],
                        heap: heap_vars[dest],
                    });
                }
                ir::Instruction::UnOp { source, op } => {
                    let output_reg = assigned_registers[inst_id];
                    match op {
                        ir::UnOp::Not => {
                            vm_instructions.push(Instruction::Not {
                                arg: assigned_registers[source],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    let output_reg = assigned_registers[inst_id];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions.push(Instruction::Add {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinOp::Sub => {
                            vm_instructions.push(Instruction::Sub {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::BinComp { left, right, comp } => {
                    let output_reg = assigned_registers[inst_id];
                    match comp {
                        ir::BinComp::LessThan => {
                            vm_instructions.push(Instruction::TestLess {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::LessEqual => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::Equal => {
                            vm_instructions.push(Instruction::TestEqual {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::NotEqual => {
                            vm_instructions.push(Instruction::TestNotEqual {
                                arg1: assigned_registers[left],
                                arg2: assigned_registers[right],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::GreaterThan => {
                            vm_instructions.push(Instruction::TestLess {
                                arg1: assigned_registers[right],
                                arg2: assigned_registers[left],
                                dest: output_reg,
                            });
                        }
                        ir::BinComp::Greater => {
                            vm_instructions.push(Instruction::TestLessEqual {
                                arg1: assigned_registers[right],
                                arg2: assigned_registers[left],
                                dest: output_reg,
                            });
                        }
                    }
                }
                ir::Instruction::Push { source } => {
                    vm_instructions.push(Instruction::Push {
                        source: assigned_registers[source],
                        len: 1,
                    });
                }
                ir::Instruction::Pop => {
                    vm_instructions.push(Instruction::Pop {
                        dest: assigned_registers[inst_id],
                        len: 1,
                    });
                }
                ir::Instruction::Call {
                    source,
                    args,
                    returns,
                } => {
                    vm_instructions.push(Instruction::Call {
                        func: assigned_registers[source],
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
                block_vm_jumps.push((vm_instructions.len(), block_id));
                vm_instructions.push(Instruction::Jump { offset: 0 });
            }
            ir::Exit::Branch {
                cond,
                if_true,
                if_false,
            } => {
                block_vm_jumps.push((vm_instructions.len(), if_true));
                vm_instructions.push(Instruction::JumpIf {
                    arg: assigned_registers[cond],
                    is_true: true,
                    offset: 0,
                });
                block_vm_jumps.push((vm_instructions.len(), if_false));
                vm_instructions.push(Instruction::JumpIf {
                    arg: assigned_registers[cond],
                    is_true: false,
                    offset: 0,
                });
            }
        }
    }

    dbg!(vm_instructions.len());
    for (index, block_id) in block_vm_jumps {
        let jump_offset = (block_vm_starts[block_id] as isize - index as isize)
            .try_into()
            .map_err(|_| CodegenError::JumpOutOfRange)?;
        dbg!(index, jump_offset);
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
            Constant::Integer(i) => closure::Constant::Integer(i),
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
