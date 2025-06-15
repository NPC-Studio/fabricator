use std::collections::{HashMap, hash_map};

use fabricator_util::typed_id_map::SecondaryMap;
use fabricator_vm::{
    self as vm, bytecode,
    debug::Span,
    instructions::{self, Instruction},
};
use gc_arena::{Gc, Mutation};
use thiserror::Error;

use crate::{
    analysis::{
        instruction_liveness::{InstructionLiveness, InstructionVerificationError},
        shadow_liveness::{ShadowLiveness, ShadowVerificationError},
        variable_liveness::{VariableLiveness, VariableVerificationError},
    },
    codegen::{heap_alloc::HeapAllocation, register_alloc::RegisterAllocation},
    constant::Constant,
    graph::dfs::topological_order,
    ir,
    magic_dict::MagicDict,
};

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error(transparent)]
    InstructionVerification(#[from] InstructionVerificationError),
    #[error(transparent)]
    PhiUpsilonVerification(#[from] ShadowVerificationError),
    #[error(transparent)]
    VariableVerification(#[from] VariableVerificationError),
    #[error(transparent)]
    ByteCodeEncoding(#[from] bytecode::ByteCodeEncodingError),
    #[error("too many registers used")]
    RegisterOverflow,
    #[error("too many heap variables used")]
    HeapVarOverflow,
    #[error("too many constants used")]
    ConstantOverflow,
    #[error("too many sub-functions")]
    PrototypeOverflow,
    #[error("jump out of range")]
    JumpOutOfRange,
    #[error("missing magic value")]
    NoSuchMagic,
    #[error("magic value index too large")]
    MagicIndexOverflow,
}

pub fn codegen<'gc>(
    mc: &Mutation<'gc>,
    ir: &ir::Function<vm::String<'gc>>,
    chunk: vm::Chunk<'gc>,
    magic_dict: impl MagicDict<vm::String<'gc>>,
) -> Result<vm::Prototype<'gc>, CodegenError> {
    codegen_function(mc, ir, chunk, &magic_dict, &SecondaryMap::new())
}

fn codegen_function<'gc>(
    mc: &Mutation<'gc>,
    ir: &ir::Function<vm::String<'gc>>,
    chunk: vm::Chunk<'gc>,
    magic_dict: &impl MagicDict<vm::String<'gc>>,
    parent_heap_indexes: &SecondaryMap<ir::Variable, instructions::HeapIdx>,
) -> Result<vm::Prototype<'gc>, CodegenError> {
    let instruction_liveness = InstructionLiveness::compute(ir)?;
    let shadow_liveness = ShadowLiveness::compute(ir)?;
    let variable_liveness = VariableLiveness::compute(ir)?;

    let reg_alloc = RegisterAllocation::allocate(ir, &instruction_liveness, &shadow_liveness)
        .ok_or(CodegenError::RegisterOverflow)?;
    let heap_alloc = HeapAllocation::allocate(ir, &variable_liveness, parent_heap_indexes)
        .ok_or(CodegenError::HeapVarOverflow)?;

    let mut prototypes = Vec::new();
    let mut prototype_indexes: SecondaryMap<ir::FuncId, instructions::ProtoIdx> =
        SecondaryMap::new();
    for (func_id, func) in ir.functions.iter() {
        prototype_indexes.insert(
            func_id,
            prototypes
                .len()
                .try_into()
                .map_err(|_| CodegenError::PrototypeOverflow)?,
        );
        prototypes.push(Gc::new(
            mc,
            codegen_function(mc, func, chunk, magic_dict, &heap_alloc.heap_indexes)?,
        ));
    }

    let mut constants = Vec::new();
    let mut constant_indexes = HashMap::<Constant<vm::String<'gc>>, instructions::ConstIdx>::new();

    for block in ir.blocks.values() {
        for &inst_id in &block.instructions {
            for &c in ir.instructions[inst_id].constants() {
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

    let block_order = topological_order(ir.start_block, |id| ir.blocks[id].exit.successors());

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
        let block = &ir.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            let span = ir.spans.get(inst_id).copied().unwrap_or_else(Span::null);
            match ir.instructions[inst_id] {
                ir::Instruction::NoOp => {}
                ir::Instruction::Copy(source) => {
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    let source_reg = reg_alloc.instruction_registers[source];
                    if dest_reg != source_reg {
                        vm_instructions.push((
                            Instruction::Move {
                                dest: dest_reg,
                                source: source_reg,
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::Undefined => {
                    vm_instructions.push((
                        Instruction::Undefined {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::Constant(c) => {
                    vm_instructions.push((
                        Instruction::LoadConstant {
                            dest: reg_alloc.instruction_registers[inst_id],
                            constant: constant_indexes[&c],
                        },
                        span,
                    ));
                }
                ir::Instruction::Closure(func_id) => {
                    vm_instructions.push((
                        Instruction::Closure {
                            dest: reg_alloc.instruction_registers[inst_id],
                            proto: prototype_indexes[func_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::OpenVariable(_) => {
                    // `OpenVariable` is an ephemeral instruction used only to allocate a heap
                    // index.
                }
                ir::Instruction::GetVariable(var) => {
                    vm_instructions.push((
                        Instruction::GetHeap {
                            dest: reg_alloc.instruction_registers[inst_id],
                            heap: heap_alloc.heap_indexes[var],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetVariable(dest, source) => {
                    vm_instructions.push((
                        Instruction::SetHeap {
                            heap: heap_alloc.heap_indexes[dest],
                            source: reg_alloc.instruction_registers[source],
                        },
                        span,
                    ));
                }
                ir::Instruction::CloseVariable(var) => {
                    vm_instructions.push((
                        Instruction::ResetHeap {
                            heap: heap_alloc.heap_indexes[var],
                        },
                        span,
                    ));
                }
                ir::Instruction::GetMagic(magic) => {
                    let magic = magic_dict
                        .magic_index(&magic)
                        .ok_or(CodegenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| CodegenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::GetMagic {
                            dest: reg_alloc.instruction_registers[inst_id],
                            magic,
                        },
                        span,
                    ));
                }
                ir::Instruction::SetMagic(magic, source) => {
                    let magic = magic_dict
                        .magic_index(&magic)
                        .ok_or(CodegenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| CodegenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::SetMagic {
                            magic,
                            source: reg_alloc.instruction_registers[source],
                        },
                        span,
                    ));
                }
                ir::Instruction::This => {
                    vm_instructions.push((
                        Instruction::This {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::NewObject => {
                    vm_instructions.push((
                        Instruction::NewObject {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::NewArray => {
                    vm_instructions.push((
                        Instruction::NewArray {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::Parameter(index) => {
                    vm_instructions.push((
                        Instruction::Param {
                            dest: reg_alloc.instruction_registers[inst_id],
                            index,
                        },
                        span,
                    ));
                }
                ir::Instruction::GetField { object, key } => {
                    vm_instructions.push((
                        Instruction::GetField {
                            dest: reg_alloc.instruction_registers[inst_id],
                            object: reg_alloc.instruction_registers[object],
                            key: reg_alloc.instruction_registers[key],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetField { object, key, value } => {
                    vm_instructions.push((
                        Instruction::SetField {
                            object: reg_alloc.instruction_registers[object],
                            key: reg_alloc.instruction_registers[key],
                            value: reg_alloc.instruction_registers[value],
                        },
                        span,
                    ));
                }
                ir::Instruction::GetFieldConst { object, key } => {
                    vm_instructions.push((
                        Instruction::GetFieldConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            object: reg_alloc.instruction_registers[object],
                            key: constant_indexes[&key],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetFieldConst { object, key, value } => {
                    vm_instructions.push((
                        Instruction::SetFieldConst {
                            object: reg_alloc.instruction_registers[object],
                            key: constant_indexes[&key],
                            value: reg_alloc.instruction_registers[value],
                        },
                        span,
                    ));
                }
                ir::Instruction::GetIndex { array, index } => {
                    vm_instructions.push((
                        Instruction::GetIndex {
                            dest: reg_alloc.instruction_registers[inst_id],
                            array: reg_alloc.instruction_registers[array],
                            index: reg_alloc.instruction_registers[index],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetIndex {
                    array,
                    index,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetIndex {
                            array: reg_alloc.instruction_registers[array],
                            index: reg_alloc.instruction_registers[index],
                            value: reg_alloc.instruction_registers[value],
                        },
                        span,
                    ));
                }
                ir::Instruction::GetIndexConst { array, index } => {
                    vm_instructions.push((
                        Instruction::GetIndexConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            array: reg_alloc.instruction_registers[array],
                            index: constant_indexes[&index],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetIndexConst {
                    array,
                    index,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetIndexConst {
                            array: reg_alloc.instruction_registers[array],
                            index: constant_indexes[&index],
                            value: reg_alloc.instruction_registers[value],
                        },
                        span,
                    ));
                }
                ir::Instruction::Phi(shadow) => {
                    let shadow_reg = reg_alloc.shadow_registers[shadow];
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    if shadow_reg != dest_reg {
                        vm_instructions.push((
                            Instruction::Move {
                                dest: dest_reg,
                                source: shadow_reg,
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::Upsilon(shadow, source) => {
                    if shadow_liveness.is_live_upsilon(shadow, block_id, inst_index) {
                        let shadow_reg = reg_alloc.shadow_registers[shadow];
                        let source_reg = reg_alloc.instruction_registers[source];
                        if shadow_reg != source_reg {
                            vm_instructions.push((
                                Instruction::Move {
                                    dest: shadow_reg,
                                    source: source_reg,
                                },
                                span,
                            ));
                        }
                    }
                }
                ir::Instruction::UnOp { source, op } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match op {
                        ir::UnOp::Not => {
                            vm_instructions.push((
                                Instruction::Not {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Neg => {
                            vm_instructions.push((
                                Instruction::Neg {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                    }
                }
                ir::Instruction::BinOp { left, right, op } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions.push((
                                Instruction::Add {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::Sub => {
                            vm_instructions.push((
                                Instruction::Sub {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::Mult => {
                            vm_instructions.push((
                                Instruction::Mult {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::Div => {
                            vm_instructions.push((
                                Instruction::Div {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::LessThan => {
                            vm_instructions.push((
                                Instruction::TestLess {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::LessEqual => {
                            vm_instructions.push((
                                Instruction::TestLessEqual {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::Equal => {
                            vm_instructions.push((
                                Instruction::TestEqual {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::NotEqual => {
                            vm_instructions.push((
                                Instruction::TestNotEqual {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[left],
                                    arg2: reg_alloc.instruction_registers[right],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::GreaterThan => {
                            vm_instructions.push((
                                Instruction::TestLess {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[right],
                                    arg2: reg_alloc.instruction_registers[left],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::GreaterEqual => {
                            vm_instructions.push((
                                Instruction::TestLessEqual {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[right],
                                    arg2: reg_alloc.instruction_registers[left],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::And => {
                            vm_instructions.push((
                                Instruction::And {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[right],
                                    arg2: reg_alloc.instruction_registers[left],
                                },
                                span,
                            ));
                        }
                        ir::BinOp::Or => {
                            vm_instructions.push((
                                Instruction::Or {
                                    dest: output_reg,
                                    arg1: reg_alloc.instruction_registers[right],
                                    arg2: reg_alloc.instruction_registers[left],
                                },
                                span,
                            ));
                        }
                    }
                }
                ir::Instruction::Call {
                    func,
                    ref args,
                    return_value,
                } => {
                    for &arg in args {
                        vm_instructions.push((
                            Instruction::Push {
                                source: reg_alloc.instruction_registers[arg],
                                len: 1,
                            },
                            span,
                        ));
                    }

                    if return_value {
                        vm_instructions.push((
                            Instruction::Call {
                                func: reg_alloc.instruction_registers[func],
                                returns: 1,
                            },
                            span,
                        ));
                        vm_instructions.push((
                            Instruction::Pop {
                                dest: reg_alloc.instruction_registers[inst_id],
                                len: 1,
                            },
                            span,
                        ));
                    } else {
                        vm_instructions.push((
                            Instruction::Call {
                                func: reg_alloc.instruction_registers[func],
                                returns: 0,
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::Method {
                    func,
                    this,
                    ref args,
                    return_value,
                } => {
                    for &arg in args {
                        vm_instructions.push((
                            Instruction::Push {
                                source: reg_alloc.instruction_registers[arg],
                                len: 1,
                            },
                            span,
                        ));
                    }

                    let this = reg_alloc.instruction_registers[this];
                    if return_value {
                        vm_instructions.push((
                            Instruction::Method {
                                this,
                                func: reg_alloc.instruction_registers[func],
                                returns: 1,
                            },
                            span,
                        ));
                        vm_instructions.push((
                            Instruction::Pop {
                                dest: reg_alloc.instruction_registers[inst_id],
                                len: 1,
                            },
                            span,
                        ));
                    } else {
                        vm_instructions.push((
                            Instruction::Method {
                                this,
                                func: reg_alloc.instruction_registers[func],
                                returns: 0,
                            },
                            span,
                        ));
                    }
                }
            }
        }

        match block.exit {
            ir::Exit::Return { value } => {
                if let Some(value) = value {
                    vm_instructions.push((
                        Instruction::Push {
                            source: reg_alloc.instruction_registers[value],
                            len: 1,
                        },
                        Span::null(),
                    ));
                }
                vm_instructions.push((Instruction::Return {}, Span::null()));
            }
            ir::Exit::Jump(block_id) => {
                // If we are the next block in output order, we don't need to add a jump
                if *block_order_indexes.get(&block_id).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), block_id));
                    vm_instructions.push((Instruction::Jump { offset: 0 }, Span::null()));
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
                    vm_instructions.push((
                        Instruction::JumpIf {
                            arg: reg_alloc.instruction_registers[cond],
                            is_true: true,
                            offset: 0,
                        },
                        Span::null(),
                    ));
                }

                if *block_order_indexes.get(&if_false).unwrap() != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_false));
                    vm_instructions.push((
                        Instruction::JumpIf {
                            arg: reg_alloc.instruction_registers[cond],
                            is_true: false,
                            offset: 0,
                        },
                        Span::null(),
                    ));
                }
            }
        }
    }

    for (index, block_id) in block_vm_jumps {
        let jump_offset = (block_vm_starts[block_id] as isize - index as isize)
            .try_into()
            .map_err(|_| CodegenError::JumpOutOfRange)?;
        match &mut vm_instructions[index].0 {
            Instruction::Jump { offset } => {
                *offset = jump_offset;
            }
            Instruction::JumpIf { offset, .. } => {
                *offset = jump_offset;
            }
            _ => panic!("instruction not a jump"),
        }
    }

    let bytecode = vm::ByteCode::encode(vm_instructions.into_iter())?;

    let constants = constants
        .into_iter()
        .map(|c| match c {
            Constant::Undefined => vm::Constant::Undefined,
            Constant::Boolean(b) => vm::Constant::Boolean(b),
            Constant::Integer(i) => {
                if let Ok(i) = i.try_into() {
                    vm::Constant::Integer(i)
                } else {
                    vm::Constant::Float(i as f64)
                }
            }
            Constant::Float(f) => vm::Constant::Float(f),
            Constant::String(s) => vm::Constant::String(s),
        })
        .collect::<Vec<_>>();

    let reference = match ir.reference {
        ir::FunctionRef::Named(name, span) => vm::ClosureRef::Named(name, span),
        ir::FunctionRef::Expression(span) => vm::ClosureRef::Expression(span),
        ir::FunctionRef::Chunk => vm::ClosureRef::Chunk,
    };

    Ok(vm::Prototype {
        chunk,
        reference,
        bytecode,
        constants: constants.into_boxed_slice(),
        prototypes: prototypes.into_boxed_slice(),
        used_registers: reg_alloc.used_registers,
        heap_vars: heap_alloc.heap_var_descriptors.into_boxed_slice(),
    })
}
