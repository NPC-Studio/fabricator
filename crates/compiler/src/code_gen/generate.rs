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
    let func_span = ir.reference.span();

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

    // Resize the stack to the expected number of arguments.
    vm_instructions.push((
        Instruction::StackResizeConst {
            stack_top: get_const_index(&Constant::Integer(ir.num_parameters.try_into().unwrap()))?,
        },
        func_span,
    ));

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
            match ir.instructions[inst_id] {
                ir::Instruction::GetIndex { .. } => todo!(),
                ir::Instruction::SetIndex { .. } => {
                    needs_to_save(inst_index)?;
                }
                _ => {}
            }
        }

        if let ir::Exit::Return { .. } = block.exit {
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
            func_span,
        ));
    }

    if !saved_stack_top_registers.is_empty() {
        vm_instructions.push((
            Instruction::StackTop {
                dest: saved_stack_top_registers[0],
            },
            func_span,
        ));
    }

    for (order_index, &block_id) in block_order.iter().enumerate() {
        let block = &ir.blocks[block_id];
        block_vm_starts.insert(block_id, vm_instructions.len());

        for (inst_index, &inst_id) in block.instructions.iter().enumerate() {
            let span = ir.spans.get(inst_id).copied().unwrap_or(func_span);
            match ir.instructions[inst_id] {
                ir::Instruction::NoOp => {}
                ir::Instruction::Copy(source) => {
                    let dest_reg = reg_alloc.instruction_registers[inst_id];
                    let source_reg = reg_alloc.instruction_registers[source];
                    if dest_reg != source_reg {
                        vm_instructions.push((
                            Instruction::Copy {
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
                ir::Instruction::Boolean(is_true) => {
                    vm_instructions.push((
                        Instruction::Boolean {
                            dest: reg_alloc.instruction_registers[inst_id],
                            is_true,
                        },
                        span,
                    ));
                }
                ir::Instruction::Constant(ref c) => {
                    vm_instructions.push((
                        Instruction::LoadConstant {
                            dest: reg_alloc.instruction_registers[inst_id],
                            constant: get_const_index(c)?,
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
                ir::Instruction::OpenThisScope(scope) => {
                    vm_instructions.push((Instruction::SwapThisOther {}, span));
                    if this_scope_liveness.has_inner_scope(scope) {
                        let nesting_level = this_scope_liveness.nesting_level(scope).unwrap();
                        vm_instructions.push((
                            Instruction::Other {
                                dest: saved_other_registers[nesting_level + 1],
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::SetThis(_, this) => {
                    vm_instructions.push((
                        Instruction::SetThis {
                            source: reg_alloc.instruction_registers[this],
                        },
                        span,
                    ));
                }
                ir::Instruction::CloseThisScope(scope) => {
                    vm_instructions.push((Instruction::SwapThisOther {}, span));
                    let nesting_level = this_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::SetOther {
                            source: saved_other_registers[nesting_level],
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
                ir::Instruction::Argument(index) => {
                    vm_instructions.push((
                        Instruction::StackGetConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            stack_pos: get_const_index(&Constant::Integer(
                                index.try_into().unwrap(),
                            ))?,
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
                            key: get_const_index(key)?,
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
                            key: get_const_index(key)?,
                            value: reg_alloc.instruction_registers[value],
                        },
                        span,
                    ));
                }
                ir::Instruction::GetIndex { array, ref indexes } => {
                    if indexes.len() == 1 {
                        vm_instructions.push((
                            Instruction::GetIndex {
                                dest: reg_alloc.instruction_registers[inst_id],
                                array: reg_alloc.instruction_registers[array],
                                index: reg_alloc.instruction_registers[indexes[0]],
                            },
                            span,
                        ));
                    } else {
                        let stack_bottom = get_current_stack_top_register(block_id, inst_index);

                        for &index in indexes {
                            vm_instructions.push((
                                Instruction::StackPush {
                                    source: reg_alloc.instruction_registers[index],
                                },
                                span,
                            ));
                        }

                        vm_instructions.push((
                            Instruction::GetIndexMulti {
                                dest: reg_alloc.instruction_registers[inst_id],
                                array: reg_alloc.instruction_registers[array],
                                stack_bottom,
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::SetIndex {
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
                            span,
                        ));
                    } else {
                        let stack_bottom = get_current_stack_top_register(block_id, inst_index);

                        for &index in indexes {
                            vm_instructions.push((
                                Instruction::StackPush {
                                    source: reg_alloc.instruction_registers[index],
                                },
                                span,
                            ));
                        }

                        vm_instructions.push((
                            Instruction::SetIndexMulti {
                                array: reg_alloc.instruction_registers[array],
                                stack_bottom,
                                value: reg_alloc.instruction_registers[value],
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::GetIndexConst { array, ref index } => {
                    vm_instructions.push((
                        Instruction::GetIndexConst {
                            dest: reg_alloc.instruction_registers[inst_id],
                            array: reg_alloc.instruction_registers[array],
                            index: get_const_index(index)?,
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
                            index: get_const_index(index)?,
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
                            Instruction::Copy {
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
                                Instruction::Copy {
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
                        ir::UnOp::IsUndefined => {
                            vm_instructions.push((
                                Instruction::IsUndefined {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::IsDefined => {
                            vm_instructions.push((
                                Instruction::IsDefined {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Test => {
                            vm_instructions.push((
                                Instruction::Test {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Not => {
                            vm_instructions.push((
                                Instruction::Not {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Negate => {
                            vm_instructions.push((
                                Instruction::Negate {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Increment => {
                            vm_instructions.push((
                                Instruction::Increment {
                                    dest: output_reg,
                                    arg: reg_alloc.instruction_registers[source],
                                },
                                span,
                            ));
                        }
                        ir::UnOp::Decrement => {
                            vm_instructions.push((
                                Instruction::Decrement {
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
                            vm_instructions
                                .push((Instruction::Subtract { dest, left, right }, span));
                        }
                        ir::BinOp::Mult => {
                            vm_instructions
                                .push((Instruction::Multiply { dest, left, right }, span));
                        }
                        ir::BinOp::Div => {
                            vm_instructions.push((Instruction::Divide { dest, left, right }, span));
                        }
                        ir::BinOp::Rem => {
                            vm_instructions
                                .push((Instruction::Remainder { dest, left, right }, span));
                        }
                        ir::BinOp::IDiv => {
                            vm_instructions
                                .push((Instruction::IntDivide { dest, left, right }, span));
                        }
                        ir::BinOp::LessThan => {
                            vm_instructions.push((Instruction::IsLess { dest, left, right }, span));
                        }
                        ir::BinOp::LessEqual => {
                            vm_instructions
                                .push((Instruction::IsLessEqual { dest, left, right }, span));
                        }
                        ir::BinOp::Equal => {
                            vm_instructions
                                .push((Instruction::IsEqual { dest, left, right }, span));
                        }
                        ir::BinOp::NotEqual => {
                            vm_instructions
                                .push((Instruction::IsNotEqual { dest, left, right }, span));
                        }
                        ir::BinOp::GreaterThan => {
                            vm_instructions.push((
                                Instruction::IsLess {
                                    dest,
                                    left: right,
                                    right: left,
                                },
                                span,
                            ));
                        }
                        ir::BinOp::GreaterEqual => {
                            vm_instructions.push((
                                Instruction::IsLessEqual {
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
                ir::Instruction::Throw(source) => {
                    vm_instructions.push((
                        Instruction::Throw {
                            source: reg_alloc.instruction_registers[source],
                        },
                        span,
                    ));
                }
                ir::Instruction::OpenCall {
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
                            span,
                        ));
                    }

                    vm_instructions.push((
                        Instruction::Call {
                            func: reg_alloc.instruction_registers[func],
                            stack_bottom: saved_stack_top_registers[nesting_level],
                        },
                        span,
                    ));

                    if call_scopes_which_save.contains(&scope) {
                        vm_instructions.push((
                            Instruction::StackTop {
                                dest: saved_stack_top_registers[nesting_level + 1],
                            },
                            span,
                        ));
                    }
                }
                ir::Instruction::GetReturn(scope, index) => {
                    let nesting_level = call_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::StackGetOffset {
                            dest: reg_alloc.instruction_registers[inst_id],
                            stack_base: saved_stack_top_registers[nesting_level],
                            offset: get_const_index(&Constant::Integer(index.try_into().unwrap()))?,
                        },
                        span,
                    ));
                }
                ir::Instruction::CloseCall(scope) => {
                    let nesting_level = call_scope_liveness.nesting_level(scope).unwrap();
                    vm_instructions.push((
                        Instruction::StackResize {
                            stack_top: saved_stack_top_registers[nesting_level],
                        },
                        span,
                    ));
                }
            }
        }

        match block.exit {
            ir::Exit::Return { value } => {
                let stack_bottom =
                    get_current_stack_top_register(block_id, block.instructions.len());

                if let Some(value) = value {
                    vm_instructions.push((
                        Instruction::StackPush {
                            source: reg_alloc.instruction_registers[value],
                        },
                        func_span,
                    ));
                }

                vm_instructions.push((Instruction::Return { stack_bottom }, func_span));
            }
            ir::Exit::Jump(block_id) => {
                // If we are the next block in output order, we don't need to add a jump
                if block_order_indexes[&block_id] != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), block_id));
                    vm_instructions.push((Instruction::Jump { target: 0 }, func_span));
                }
            }
            ir::Exit::Branch {
                cond,
                if_true,
                if_false,
            } => {
                // If we are the next block in output order, we don't need to add a jump.
                if block_order_indexes[&if_true] != order_index + 1 {
                    block_vm_jumps.push((vm_instructions.len(), if_true));
                    vm_instructions.push((
                        Instruction::JumpIf {
                            target: 0,
                            arg: reg_alloc.instruction_registers[cond],
                            is_true: true,
                        },
                        func_span,
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
                        func_span,
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
        is_constructor: ir.is_constructor,
        reference: ir.reference.clone(),
        bytecode,
        constants: constants.into_boxed_slice(),
        prototypes: prototypes.into_boxed_slice(),
        used_registers: reg_alloc.used_registers,
        heap_vars: heap_alloc.heap_var_descriptors.into_boxed_slice(),
    })
}
