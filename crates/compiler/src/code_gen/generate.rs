use std::{
    collections::{HashMap, HashSet, hash_map},
    hash::Hash,
};

use fabricator_util::typed_id_map::SecondaryMap;
use fabricator_vm::{
    self as vm,
    instructions::{self, Instruction},
};

use crate::{
    analysis::nested_scope_liveness::{CallScopeLiveness, ThisScopeLiveness},
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
    let mut reg_alloc = RegisterAllocation::allocate(ir)?;
    let heap_alloc = HeapAllocation::allocate(ir, parent_heap_indexes)?;

    let this_scope_liveness = ThisScopeLiveness::compute(ir).unwrap();
    let call_scope_liveness = CallScopeLiveness::compute(ir).unwrap();

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

    let mut get_const_index = |c: &Constant<S>| -> Result<instructions::ConstIdx, ProtoGenError> {
        Ok(match constant_indexes.entry(c.clone()) {
            hash_map::Entry::Vacant(vacant) => {
                let idx = constants
                    .len()
                    .try_into()
                    .map_err(|_| ProtoGenError::ConstantOverflow)?;
                vacant.insert(idx);
                constants.push(c.clone());
                idx
            }
            hash_map::Entry::Occupied(occupied) => *occupied.get(),
        })
    };

    let block_order = topological_order(ir.start_block, |id| ir.blocks[id].exit.kind.successors());

    let block_order_indexes: HashMap<ir::BlockId, usize> = block_order
        .iter()
        .copied()
        .enumerate()
        .map(|(i, b)| (b, i))
        .collect();

    let mut vm_instructions = Vec::new();
    let mut block_vm_starts = SecondaryMap::<ir::BlockId, usize>::new();
    let mut block_vm_jumps = Vec::new();

    // Whenever entering a scope, if we know that something inside that scope will need to change
    // and then restore the previous state, we save the state to restore upon entering the *outer*
    // scope.
    //
    // We do this because there are many more inner scopes than outer scopes, and this saves having
    // to repeatedly save the same values which are not modified.

    // For "this" scopes, any scope that has an inner scope will save the value to restore on open.
    // The number of required registers is equal to the "this" scope nesting level.

    let saved_other_registers = vec![reg_alloc.allocate_extra()?; this_scope_liveness.nesting()];

    // For "call" scopes, the story is not as simple. We need to save when a "call" scope has an
    // inner scope, but *also* for certain other instructions which temporarily modify the stack.
    // Search for which scopes contain those instructions and mark the containing scope as needing
    // to save state.

    let mut call_scopes_which_save = call_scope_liveness
        .scopes()
        .filter(|&s| call_scope_liveness.has_inner_scope(s))
        .collect::<HashSet<_>>();

    let mut saved_stack_top_registers =
        vec![reg_alloc.allocate_extra()?; call_scope_liveness.nesting()];

    for &block_id in &block_order {
        let mut needs_to_save = |inst_index: usize| -> Result<(), ProtoGenError> {
            if let Some(call_scope) =
                call_scope_liveness.deepest_for(ir::InstLocation::new(block_id, inst_index))
            {
                call_scopes_which_save.insert(call_scope);
                // If this is a most-inner scope, then we need to add another register to
                // save to.
                if call_scope_liveness.nesting_level(call_scope).unwrap()
                    >= saved_stack_top_registers.len()
                {
                    saved_stack_top_registers.push(reg_alloc.allocate_extra()?);
                }
            } else {
                // If there is no enclosing scope for this instruction, we need to ensure
                // that we at least save the bottom-level value.
                if saved_stack_top_registers.is_empty() {
                    saved_stack_top_registers.push(reg_alloc.allocate_extra()?);
                }
            }
            Ok(())
        };

        let block = &ir.blocks[block_id];
        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            match ir.instructions[inst_id].kind {
                ir::InstructionKind::GetIndex { .. } | ir::InstructionKind::SetIndex { .. } => {
                    needs_to_save(inst_index)?;
                }
                _ => {}
            }
        }

        if let ir::ExitKind::Return { .. } = block.exit.kind {
            needs_to_save(ir.instructions.len())?;
        }
    }

    let get_current_stack_top_register =
        |block_id: ir::BlockId, inst_index: usize| -> instructions::RegIdx {
            let idx = if let Some(enclosing_call_scope) =
                call_scope_liveness.deepest_for(ir::InstLocation::new(block_id, inst_index))
            {
                call_scope_liveness
                    .nesting_level(enclosing_call_scope)
                    .unwrap()
                    + 1
            } else {
                0
            };
            saved_stack_top_registers[idx]
        };

    // If we need to save the bottom-level value for scopes, do so.

    if !saved_other_registers.is_empty() {
        vm_instructions.push((
            Instruction::Other {
                dest: saved_other_registers[0],
            },
            ir.reference.span().start_span(),
        ));
    }

    if !saved_stack_top_registers.is_empty() {
        vm_instructions.push((
            Instruction::StackTop {
                dest: saved_stack_top_registers[0],
            },
            ir.reference.span().start_span(),
        ));
    }

    for (order_index, &block_id) in block_order.iter().enumerate() {
        let block = &ir.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            let inst = &ir.instructions[inst_id];
            match inst.kind {
                ir::InstructionKind::NoOp => {}
                ir::InstructionKind::Copy(source) => {
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    let source_reg = reg_alloc.instruction_registers[source];
                    if dest_reg != source_reg {
                        vm_instructions.push((
                            Instruction::Copy {
                                dest: dest_reg,
                                source: source_reg,
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::Undefined => {
                    vm_instructions.push((
                        Instruction::Undefined {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Boolean(is_true) => {
                    vm_instructions.push((
                        Instruction::Boolean {
                            dest: reg_alloc.instruction_registers[inst_id],
                            is_true,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Constant(ref c) => {
                    vm_instructions.push((
                        Instruction::LoadConstant {
                            dest: reg_alloc.instruction_registers[inst_id],
                            constant: get_const_index(c)?,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Closure { func, bind_this } => {
                    vm_instructions.push((
                        Instruction::Closure {
                            dest: reg_alloc.instruction_registers[inst_id],
                            proto: prototype_indexes[func],
                            bind_this,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::OpenVariable(_) => {
                    // `OpenVariable` is an ephemeral instruction used only to allocate a heap
                    // index.
                }
                ir::InstructionKind::GetVariable(var) => {
                    vm_instructions.push((
                        Instruction::GetHeap {
                            dest: reg_alloc.instruction_registers[inst_id],
                            heap: heap_alloc.heap_indexes[var],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::SetVariable(dest, source) => {
                    vm_instructions.push((
                        Instruction::SetHeap {
                            heap: heap_alloc.heap_indexes[dest],
                            source: reg_alloc.instruction_registers[source],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::CloseVariable(var) => {
                    vm_instructions.push((
                        Instruction::ResetHeap {
                            heap: heap_alloc.heap_indexes[var],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::GetMagic(ref magic_var) => {
                    let magic_idx = magic_index(magic_var)
                        .ok_or(ProtoGenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| ProtoGenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::GetMagic {
                            dest: reg_alloc.instruction_registers[inst_id],
                            magic: magic_idx,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::SetMagic(ref magic_var, source) => {
                    let magic_idx = magic_index(magic_var)
                        .ok_or(ProtoGenError::NoSuchMagic)?
                        .try_into()
                        .map_err(|_| ProtoGenError::MagicIndexOverflow)?;
                    vm_instructions.push((
                        Instruction::SetMagic {
                            magic: magic_idx,
                            source: reg_alloc.instruction_registers[source],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Globals => {
                    vm_instructions.push((
                        Instruction::Globals {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::This => {
                    vm_instructions.push((
                        Instruction::This {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Other => {
                    vm_instructions.push((
                        Instruction::Other {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::CurrentClosure => {
                    vm_instructions.push((
                        Instruction::CurrentClosure {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::OpenThisScope(scope) => {
                    vm_instructions.push((Instruction::SwapThisOther {}, inst.span));
                    if this_scope_liveness.has_inner_scope(scope) {
                        let nesting_level = this_scope_liveness.nesting_level(scope).unwrap();
                        vm_instructions.push((
                            Instruction::Other {
                                dest: saved_other_registers[nesting_level + 1],
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::SetThis(_, this) => {
                    vm_instructions.push((
                        Instruction::SetThis {
                            source: reg_alloc.instruction_registers[this],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::CloseThisScope(scope) => {
                    vm_instructions.push((Instruction::SwapThisOther {}, inst.span));
                    let nesting_level = this_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::SetOther {
                            source: saved_other_registers[nesting_level],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::NewObject => {
                    vm_instructions.push((
                        Instruction::NewObject {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::NewArray => {
                    vm_instructions.push((
                        Instruction::NewArray {
                            dest: reg_alloc.instruction_registers[inst_id],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::FixedArgument(index) => {
                    vm_instructions.push((
                        Instruction::StackGetConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            stack_pos: get_const_index(&Constant::Integer(
                                index.try_into().unwrap(),
                            ))?,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::ArgumentCount => {
                    if saved_stack_top_registers.is_empty() {
                        vm_instructions.push((
                            Instruction::StackTop {
                                dest: reg_alloc.instruction_registers[inst_id],
                            },
                            inst.span,
                        ));
                    } else {
                        vm_instructions.push((
                            Instruction::Copy {
                                dest: reg_alloc.instruction_registers[inst_id],
                                source: saved_stack_top_registers[0],
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::Argument(index) => {
                    vm_instructions.push((
                        Instruction::StackGet {
                            dest: reg_alloc.instruction_registers[inst_id],
                            stack_pos: reg_alloc.instruction_registers[index],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::GetField { object, key } => {
                    vm_instructions.push((
                        Instruction::GetField {
                            dest: reg_alloc.instruction_registers[inst_id],
                            object: reg_alloc.instruction_registers[object],
                            key: reg_alloc.instruction_registers[key],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::SetField { object, key, value } => {
                    vm_instructions.push((
                        Instruction::SetField {
                            object: reg_alloc.instruction_registers[object],
                            key: reg_alloc.instruction_registers[key],
                            value: reg_alloc.instruction_registers[value],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::GetFieldConst { object, ref key } => {
                    vm_instructions.push((
                        Instruction::GetFieldConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            object: reg_alloc.instruction_registers[object],
                            key: get_const_index(key)?,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::SetFieldConst {
                    object,
                    ref key,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetFieldConst {
                            object: reg_alloc.instruction_registers[object],
                            key: get_const_index(key)?,
                            value: reg_alloc.instruction_registers[value],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::GetIndex { array, ref indexes } => {
                    if indexes.len() == 1 {
                        vm_instructions.push((
                            Instruction::GetIndex {
                                dest: reg_alloc.instruction_registers[inst_id],
                                array: reg_alloc.instruction_registers[array],
                                index: reg_alloc.instruction_registers[indexes[0]],
                            },
                            inst.span,
                        ));
                    } else {
                        let stack_bottom = get_current_stack_top_register(block_id, inst_index);

                        for &index in indexes {
                            vm_instructions.push((
                                Instruction::StackPush {
                                    source: reg_alloc.instruction_registers[index],
                                },
                                inst.span,
                            ));
                        }

                        vm_instructions.push((
                            Instruction::GetIndexMulti {
                                dest: reg_alloc.instruction_registers[inst_id],
                                array: reg_alloc.instruction_registers[array],
                                stack_bottom,
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::SetIndex {
                    array,
                    ref indexes,
                    value,
                } => {
                    if indexes.len() == 1 {
                        vm_instructions.push((
                            Instruction::SetIndex {
                                array: reg_alloc.instruction_registers[array],
                                index: reg_alloc.instruction_registers[indexes[0]],
                                value: reg_alloc.instruction_registers[value],
                            },
                            inst.span,
                        ));
                    } else {
                        let stack_bottom = get_current_stack_top_register(block_id, inst_index);

                        for &index in indexes {
                            vm_instructions.push((
                                Instruction::StackPush {
                                    source: reg_alloc.instruction_registers[index],
                                },
                                inst.span,
                            ));
                        }

                        vm_instructions.push((
                            Instruction::SetIndexMulti {
                                array: reg_alloc.instruction_registers[array],
                                stack_bottom,
                                value: reg_alloc.instruction_registers[value],
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::GetIndexConst { array, ref index } => {
                    vm_instructions.push((
                        Instruction::GetIndexConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            array: reg_alloc.instruction_registers[array],
                            index: get_const_index(index)?,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::SetIndexConst {
                    array,
                    ref index,
                    value,
                } => {
                    vm_instructions.push((
                        Instruction::SetIndexConst {
                            array: reg_alloc.instruction_registers[array],
                            index: get_const_index(index)?,
                            value: reg_alloc.instruction_registers[value],
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::Phi(shadow) => {
                    let shadow_reg = reg_alloc.shadow_registers[shadow];
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    if shadow_reg != dest_reg {
                        vm_instructions.push((
                            Instruction::Copy {
                                dest: dest_reg,
                                source: shadow_reg,
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::Upsilon(shadow, source) => {
                    if reg_alloc
                        .shadow_liveness
                        .is_live_upsilon(shadow, block_id, inst_index)
                    {
                        let shadow_reg = reg_alloc.shadow_registers[shadow];
                        let source_reg = reg_alloc.instruction_registers[source];
                        if shadow_reg != source_reg {
                            vm_instructions.push((
                                Instruction::Copy {
                                    dest: shadow_reg,
                                    source: source_reg,
                                },
                                inst.span,
                            ));
                        }
                    }
                }
                ir::InstructionKind::UnOp { op, source } => {
                    let output_reg = reg_alloc.instruction_registers[inst_id];
                    match op {
                        ir::UnOp::IsUndefined => {
                            vm_instructions.push((
                                Instruction::IsUndefined {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::IsDefined => {
                            vm_instructions.push((
                                Instruction::IsDefined {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::Test => {
                            vm_instructions.push((
                                Instruction::Test {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::Not => {
                            vm_instructions.push((
                                Instruction::Not {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::Negate => {
                            vm_instructions.push((
                                Instruction::Negate {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::BitNegate => {
                            vm_instructions.push((
                                Instruction::BitNegate {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::Increment => {
                            vm_instructions.push((
                                Instruction::Increment {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                        ir::UnOp::Decrement => {
                            vm_instructions.push((
                                Instruction::Decrement {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                inst.span,
                            ));
                        }
                    }
                }
                ir::InstructionKind::BinOp { left, op, right } => {
                    let dest = reg_alloc.instruction_registers[inst_id];
                    let left = reg_alloc.instruction_registers[left];
                    let right = reg_alloc.instruction_registers[right];
                    match op {
                        ir::BinOp::Add => {
                            vm_instructions
                                .push((Instruction::Add { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Sub => {
                            vm_instructions
                                .push((Instruction::Subtract { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Mult => {
                            vm_instructions
                                .push((Instruction::Multiply { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Div => {
                            vm_instructions
                                .push((Instruction::Divide { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Rem => {
                            vm_instructions
                                .push((Instruction::Remainder { dest, left, right }, inst.span));
                        }
                        ir::BinOp::IDiv => {
                            vm_instructions
                                .push((Instruction::IntDivide { dest, left, right }, inst.span));
                        }
                        ir::BinOp::LessThan => {
                            vm_instructions
                                .push((Instruction::IsLess { dest, left, right }, inst.span));
                        }
                        ir::BinOp::LessEqual => {
                            vm_instructions
                                .push((Instruction::IsLessEqual { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Equal => {
                            vm_instructions
                                .push((Instruction::IsEqual { dest, left, right }, inst.span));
                        }
                        ir::BinOp::NotEqual => {
                            vm_instructions
                                .push((Instruction::IsNotEqual { dest, left, right }, inst.span));
                        }
                        ir::BinOp::GreaterThan => {
                            vm_instructions.push((
                                Instruction::IsLess {
                                    dest,
                                    left: right,
                                    right: left,
                                },
                                inst.span,
                            ));
                        }
                        ir::BinOp::GreaterEqual => {
                            vm_instructions.push((
                                Instruction::IsLessEqual {
                                    dest,
                                    left: right,
                                    right: left,
                                },
                                inst.span,
                            ));
                        }
                        ir::BinOp::And => {
                            vm_instructions
                                .push((Instruction::And { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Or => {
                            vm_instructions
                                .push((Instruction::Or { dest, left, right }, inst.span));
                        }
                        ir::BinOp::Xor => {
                            vm_instructions
                                .push((Instruction::Xor { dest, left, right }, inst.span));
                        }
                        ir::BinOp::BitAnd => {
                            vm_instructions
                                .push((Instruction::BitAnd { dest, left, right }, inst.span));
                        }
                        ir::BinOp::BitOr => {
                            vm_instructions
                                .push((Instruction::BitOr { dest, left, right }, inst.span));
                        }
                        ir::BinOp::BitXor => {
                            vm_instructions
                                .push((Instruction::BitXor { dest, left, right }, inst.span));
                        }
                        ir::BinOp::BitShiftLeft => {
                            vm_instructions
                                .push((Instruction::BitShiftLeft { dest, left, right }, inst.span));
                        }
                        ir::BinOp::BitShiftRight => {
                            vm_instructions.push((
                                Instruction::BitShiftRight { dest, left, right },
                                inst.span,
                            ));
                        }
                        ir::BinOp::NullCoalesce => {
                            vm_instructions
                                .push((Instruction::NullCoalesce { dest, left, right }, inst.span));
                        }
                    }
                }
                ir::InstructionKind::OpenCall {
                    scope,
                    func,
                    ref args,
                } => {
                    let nesting_level = call_scope_liveness.nesting_level(scope).unwrap();

                    for &arg in args {
                        vm_instructions.push((
                            Instruction::StackPush {
                                source: reg_alloc.instruction_registers[arg],
                            },
                            inst.span,
                        ));
                    }

                    vm_instructions.push((
                        Instruction::Call {
                            func: reg_alloc.instruction_registers[func],
                            stack_bottom: saved_stack_top_registers[nesting_level],
                        },
                        inst.span,
                    ));

                    if call_scopes_which_save.contains(&scope) {
                        vm_instructions.push((
                            Instruction::StackTop {
                                dest: saved_stack_top_registers[nesting_level + 1],
                            },
                            inst.span,
                        ));
                    }
                }
                ir::InstructionKind::FixedReturn(scope, index) => {
                    let nesting_level = call_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::StackGetOffset {
                            dest: reg_alloc.instruction_registers[inst_id],
                            stack_base: saved_stack_top_registers[nesting_level],
                            offset: get_const_index(&Constant::Integer(index.try_into().unwrap()))?,
                        },
                        inst.span,
                    ));
                }
                ir::InstructionKind::CloseCall(scope) => {
                    let nesting_level = call_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::StackResize {
                            stack_top: saved_stack_top_registers[nesting_level],
                        },
                        inst.span,
                    ));
                }
            }
        }

        match block.exit.kind {
            ir::ExitKind::Return { value } => {
                let stack_bottom =
                    get_current_stack_top_register(block_id, block.instructions.len());

                if let Some(value) = value {
                    vm_instructions.push((
                        Instruction::StackPush {
                            source: reg_alloc.instruction_registers[value],
                        },
                        block.exit.span,
                    ));
                }

                vm_instructions.push((Instruction::Return { stack_bottom }, block.exit.span));
            }
            ir::ExitKind::Throw(value) => {
                vm_instructions.push((
                    Instruction::Throw {
                        source: reg_alloc.instruction_registers[value],
                    },
                    block.exit.span,
                ));
            }
            ir::ExitKind::Jump(block_id) => {
                // If we are the next block in output order, we don't need to add a jump
                if block_order_indexes[&block_id] != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), block_id));
                    vm_instructions.push((Instruction::Jump { target: 0 }, block.exit.span));
                }
            }
            ir::ExitKind::Branch {
                cond,
                if_true,
                if_false,
            } => {
                // If we are jumping to the next block in output order, we don't need to add a jump.

                if block_order_indexes[&if_true] != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_true));
                    vm_instructions.push((
                        Instruction::JumpIf {
                            target: 0,
                            arg: reg_alloc.instruction_registers[cond],
                            is_true: true,
                        },
                        block.exit.span,
                    ));
                }

                if block_order_indexes[&if_false] != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_false));
                    vm_instructions.push((
                        Instruction::JumpIf {
                            target: 0,
                            arg: reg_alloc.instruction_registers[cond],
                            is_true: false,
                        },
                        block.exit.span,
                    ));
                }
            }
        }
    }

    for (index, block_id) in block_vm_jumps {
        let jump_offset = block_vm_starts[block_id].try_into().unwrap();
        match &mut vm_instructions[index].0 {
            Instruction::Jump { target } => {
                *target = jump_offset;
            }
            Instruction::JumpIf { target, .. } => {
                *target = jump_offset;
            }
            _ => panic!("instruction not a jump"),
        }
    }

    let bytecode = vm::ByteCode::encode(vm_instructions.into_iter())?;

    Ok(Prototype {
        reference: ir.reference.clone(),
        bytecode,
        constants: constants.into_boxed_slice(),
        prototypes: prototypes.into_boxed_slice(),
        used_registers: reg_alloc.used_registers,
        heap_vars: heap_alloc.heap_var_descriptors.into_boxed_slice(),
    })
}
