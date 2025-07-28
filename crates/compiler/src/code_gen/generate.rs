use std::{
    collections::{HashMap, hash_map},
    hash::Hash,
};

use fabricator_util::typed_id_map::SecondaryMap;
use fabricator_vm::{
    self as vm,
    debug::Span,
    instructions::{self, Instruction},
};

use crate::{
    code_gen::{
        ProtoGenError, heap_alloc::HeapAllocation, prototype::Prototype,
        register_alloc::RegisterAllocation,
    },
    constant::Constant,
    graph::dfs::topological_order,
    ir,
};

/// Generate a [`Prototype`] from IR.
///
/// # Panics
///
/// May panic if the provided IR is not well-formed.
pub fn gen_prototype<S: Clone + Eq + Hash>(
    ir: &ir::Function<S>,
    magic_index: impl Fn(&S) -> Option<usize>,
) -> Result<Prototype<S>, ProtoGenError> {
    codegen_function(ir, &magic_index, &SecondaryMap::new())
}

fn codegen_function<S: Clone + Eq + Hash>(
    ir: &ir::Function<S>,
    magic_index: &impl Fn(&S) -> Option<usize>,
    parent_heap_indexes: &SecondaryMap<ir::VarId, instructions::HeapIdx>,
) -> Result<Prototype<S>, ProtoGenError> {
    let reg_alloc = RegisterAllocation::allocate(ir)?;
    let heap_alloc = HeapAllocation::allocate(ir, parent_heap_indexes)?;

    let mut prototypes = Vec::new();
    let mut prototype_indexes: SecondaryMap<ir::FuncId, instructions::ProtoIdx> =
        SecondaryMap::new();
    for (func_id, func) in ir.functions.iter() {
        prototype_indexes.insert(
            func_id,
            prototypes
                .len()
                .try_into()
                .map_err(|_| ProtoGenError::PrototypeOverflow)?,
        );
        prototypes.push(codegen_function(
            func,
            magic_index,
            &heap_alloc.heap_indexes,
        )?);
    }

    let mut constants = Vec::new();
    let mut constant_indexes = HashMap::<Constant<S>, instructions::ConstIdx>::new();

    for block in ir.blocks.values() {
        for &inst_id in &block.instructions {
            for c in ir.instructions[inst_id].constants() {
                if let hash_map::Entry::Vacant(vacant) = constant_indexes.entry(c.clone()) {
                    vacant.insert(
                        constants
                            .len()
                            .try_into()
                            .map_err(|_| ProtoGenError::ConstantOverflow)?,
                    );
                    constants.push(c.clone());
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
                ir::Instruction::Constant(ref c) => {
                    vm_instructions.push((
                        Instruction::LoadConstant {
                            dest: reg_alloc.instruction_registers[inst_id],
                            constant: constant_indexes[c],
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
                ir::Instruction::GetMagic(ref magic_var) => {
                    let magic_idx = magic_index(magic_var)
                        .ok_or(ProtoGenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| ProtoGenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::GetMagic {
                            dest: reg_alloc.instruction_registers[inst_id],
                            magic: magic_idx,
                        },
                        span,
                    ));
                }
                ir::Instruction::SetMagic(ref magic_var, source) => {
                    let magic_idx = magic_index(magic_var)
                        .ok_or(ProtoGenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| ProtoGenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::SetMagic {
                            magic: magic_idx,
                            source: reg_alloc.instruction_registers[source],
                        },
                        span,
                    ));
                }
                ir::Instruction::Globals => {
                    vm_instructions.push((
                        Instruction::Globals {
                            dest: reg_alloc.instruction_registers[inst_id],
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
                ir::Instruction::Other => {
                    vm_instructions.push((
                        Instruction::Other {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::CurrentClosure => {
                    vm_instructions.push((
                        Instruction::CurrentClosure {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::OpenThisScope(_, this) => {
                    vm_instructions.push((
                        Instruction::PushThis {
                            source: reg_alloc.instruction_registers[this],
                        },
                        span,
                    ));
                }
                ir::Instruction::CloseThisScope(_) => {
                    vm_instructions.push((Instruction::PopThis {}, span));
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
                ir::Instruction::ArgumentCount => {
                    vm_instructions.push((
                        Instruction::ArgCount {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        span,
                    ));
                }
                ir::Instruction::Argument(index) => {
                    let index = index
                        .try_into()
                        .map_err(|_| ProtoGenError::ArgumentOverflow)?;

                    vm_instructions.push((
                        Instruction::Argument {
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
                ir::Instruction::GetFieldConst { object, ref key } => {
                    vm_instructions.push((
                        Instruction::GetFieldConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            object: reg_alloc.instruction_registers[object],
                            key: constant_indexes[key],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetFieldConst {
                    object,
                    ref key,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetFieldConst {
                            object: reg_alloc.instruction_registers[object],
                            key: constant_indexes[key],
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
                ir::Instruction::GetIndexConst { array, ref index } => {
                    vm_instructions.push((
                        Instruction::GetIndexConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            array: reg_alloc.instruction_registers[array],
                            index: constant_indexes[index],
                        },
                        span,
                    ));
                }
                ir::Instruction::SetIndexConst {
                    array,
                    ref index,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetIndexConst {
                            array: reg_alloc.instruction_registers[array],
                            index: constant_indexes[index],
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
                    if reg_alloc
                        .shadow_liveness
                        .is_live_upsilon(shadow, block_id, inst_index)
                    {
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
                ir::Instruction::UnOp { op, source } => {
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
                ir::Instruction::BinOp { left, op, right } => {
                    let dest = reg_alloc.instruction_registers[inst_id];
                    let left = reg_alloc.instruction_registers[left];
                    let right = reg_alloc.instruction_registers[right];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions.push((Instruction::Add { dest, left, right }, span));
                        }
                        ir::BinOp::Sub => {
                            vm_instructions.push((Instruction::Sub { dest, left, right }, span));
                        }
                        ir::BinOp::Mult => {
                            vm_instructions.push((Instruction::Mult { dest, left, right }, span));
                        }
                        ir::BinOp::Div => {
                            vm_instructions.push((Instruction::Div { dest, left, right }, span));
                        }
                        ir::BinOp::Rem => {
                            vm_instructions.push((Instruction::Rem { dest, left, right }, span));
                        }
                        ir::BinOp::IDiv => {
                            vm_instructions.push((Instruction::IDiv { dest, left, right }, span));
                        }
                        ir::BinOp::LessThan => {
                            vm_instructions
                                .push((Instruction::TestLess { dest, left, right }, span));
                        }
                        ir::BinOp::LessEqual => {
                            vm_instructions
                                .push((Instruction::TestLessEqual { dest, left, right }, span));
                        }
                        ir::BinOp::Equal => {
                            vm_instructions
                                .push((Instruction::TestEqual { dest, left, right }, span));
                        }
                        ir::BinOp::NotEqual => {
                            vm_instructions
                                .push((Instruction::TestNotEqual { dest, left, right }, span));
                        }
                        ir::BinOp::GreaterThan => {
                            vm_instructions.push((
                                Instruction::TestLess {
                                    dest,
                                    left: right,
                                    right: left,
                                },
                                span,
                            ));
                        }
                        ir::BinOp::GreaterEqual => {
                            vm_instructions.push((
                                Instruction::TestLessEqual {
                                    dest,
                                    left: right,
                                    right: left,
                                },
                                span,
                            ));
                        }
                        ir::BinOp::And => {
                            vm_instructions.push((Instruction::And { dest, left, right }, span));
                        }
                        ir::BinOp::Or => {
                            vm_instructions.push((Instruction::Or { dest, left, right }, span));
                        }
                        ir::BinOp::NullCoalesce => {
                            vm_instructions
                                .push((Instruction::NullCoalesce { dest, left, right }, span));
                        }
                    }
                }
                ir::Instruction::Call {
                    func,
                    this,
                    ref args,
                    return_value,
                } => {
                    let arg_count = args
                        .len()
                        .try_into()
                        .map_err(|_| ProtoGenError::ArgumentOverflow)?;

                    if let Some(this) = this {
                        vm_instructions.push((
                            Instruction::PushThis {
                                source: reg_alloc.instruction_registers[this],
                            },
                            span,
                        ));
                    }

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
                                arguments: arg_count,
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
                                arguments: arg_count,
                                returns: 0,
                            },
                            span,
                        ));
                    }

                    if this.is_some() {
                        vm_instructions.push((Instruction::PopThis {}, span));
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
                    vm_instructions.push((Instruction::Return { count: 1 }, Span::null()));
                } else {
                    vm_instructions.push((Instruction::Return { count: 0 }, Span::null()));
                }
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
            .map_err(|_| ProtoGenError::JumpOutOfRange)?;
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

    Ok(Prototype {
        is_constructor: ir.is_constructor,
        reference: ir.reference.clone(),
        bytecode,
        constants: constants.into_boxed_slice(),
        prototypes: prototypes.into_boxed_slice(),
        used_registers: reg_alloc.used_registers,
        heap_vars: heap_alloc.heap_var_descriptors.into_boxed_slice(),
    })
}
