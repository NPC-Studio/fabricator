use std::{
    fmt,
    mem::{self, MaybeUninit},
    ops::ControlFlow,
};

use gc_arena::Collect;
use thiserror::Error;

use crate::{
    debug::Span,
    instructions::instruction::{ConstIdx, HeapIdx, Instruction, MagicIdx, ProtoIdx, RegIdx},
};

#[derive(Debug, Error)]
pub enum ByteCodeEncodingError {
    #[error("decoded jump is to a non-existent instruction")]
    InvalidJump(usize),
    #[error("encoded jump out of range at instruction {0}")]
    JumpOutOfRange(usize),
    #[error("no return or jump as last instruction")]
    BadLastInstruction,
}

/// An encoded list of [`Instruction`]s.
///
/// Stored in a variable-length, optimized bytecode format internally.
#[derive(Clone, Collect)]
#[collect(require_static)]
pub struct ByteCode {
    // Encoded bytecode, each instruction is serialized directly into the byte array and jump
    // offsets are stored in byte offets.
    bytes: Box<[MaybeUninit<u8>]>,
    // Ordered list of each instruction bytecode start position and the original instruction index.
    inst_boundaries: Box<[(usize, usize)]>,
    // Span data for each instruction.
    inst_spans: Box<[Span]>,
}

impl ByteCode {
    /// Encode a list of instructions as bytecode.
    pub fn encode(
        insts: impl IntoIterator<Item = (Instruction, Span)>,
    ) -> Result<Self, ByteCodeEncodingError> {
        fn opcode_for_inst(inst: Instruction) -> OpCode {
            macro_rules! match_inst {
                ($(
                    [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
                )*) => {
                    match inst {
                        $(Instruction::$name { .. } => OpCode::$name),*
                    }
                };
            }
            for_each_instruction!(match_inst)
        }

        fn op_param_len(opcode: OpCode) -> usize {
            macro_rules! match_opcode {
                ($(
                    [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
                )*) => {
                    match opcode {
                        $(OpCode::$name { .. } => mem::size_of::<params::$name>()),*
                    }
                };
            }
            for_each_instruction!(match_opcode)
        }

        let insts_iter = insts.into_iter();
        let size_hint = insts_iter.size_hint().0;

        let mut inst_spans = Vec::with_capacity(size_hint);
        let mut insts = Vec::with_capacity(size_hint);

        for (inst, span) in insts_iter {
            insts.push(inst);
            inst_spans.push(span);
        }

        let check_jump = |i: usize, offset: i16| match i.checked_add_signed(offset as isize) {
            Some(i) if i < insts.len() => Ok(()),
            _ => Err(ByteCodeEncodingError::InvalidJump(i)),
        };

        for (i, &inst) in insts.iter().enumerate() {
            match inst {
                Instruction::Jump { offset } => {
                    check_jump(i, offset)?;
                }
                Instruction::JumpIf { offset, .. } => {
                    check_jump(i, offset)?;
                }
                _ => {}
            }
        }

        if !matches!(
            insts.last(),
            Some(Instruction::Jump { .. } | Instruction::Return { .. })
        ) {
            return Err(ByteCodeEncodingError::BadLastInstruction);
        }

        let mut inst_positions = Vec::new();
        let mut pos = 0;
        for &inst in &insts {
            inst_positions.push(pos);
            pos += 1 + op_param_len(opcode_for_inst(inst));
        }
        inst_positions.push(pos);

        let calc_jump = |cur: usize, offset: i16| {
            // Encode jumps from the end of the current instruction
            let cur_pos = inst_positions.get(cur + 1).copied().unwrap() as isize;
            let jump_pos = inst_positions[(cur as isize + offset as isize) as usize] as isize;
            i16::try_from(jump_pos - cur_pos)
                .map_err(|_| ByteCodeEncodingError::JumpOutOfRange(cur))
        };

        let mut bytes = Vec::new();
        let mut inst_boundaries = Vec::with_capacity(size_hint);
        for (i, mut inst) in insts.iter().copied().enumerate() {
            assert_eq!(inst_positions[i], bytes.len());
            inst_boundaries.push((bytes.len(), i));

            // Rewrite jump instruction offsets to be *bytecode* relative
            match &mut inst {
                Instruction::Jump { offset } => {
                    *offset = calc_jump(i, *offset)?;
                }
                Instruction::JumpIf { offset, .. } => {
                    *offset = calc_jump(i, *offset)?;
                }
                _ => {}
            };

            bytecode_write(&mut bytes, opcode_for_inst(inst));

            macro_rules! write_instruction {
                ($(
                    [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
                )*) => {
                    match inst {
                        $(Instruction::$name { $($field),* } => {
                            bytecode_write(&mut bytes, params::$name { $($field),* });
                        }),*
                    }
                };
            }
            for_each_instruction!(write_instruction);
        }

        Ok(Self {
            bytes: bytes.into_boxed_slice(),
            inst_boundaries: inst_boundaries.into_boxed_slice(),
            inst_spans: inst_spans.into_boxed_slice(),
        })
    }

    /// Fetch a single instruction by its program counter value.
    ///
    /// Returns the instruction index, instruction, and the debug span for that instruction.
    pub fn instruction_for_pc(&self, pc: usize) -> Option<(usize, Instruction, Span)> {
        let inst_index = self.inst_index_for_pc(pc)?;
        let inst = unsafe { self.decode_instruction(pc, inst_index) };
        let span = self.inst_spans[inst_index];
        Some((inst_index, inst, span))
    }

    /// Decode instructions from bytecode.
    pub fn decode(&self) -> impl Iterator<Item = (Instruction, Span)> + '_ {
        self.inst_boundaries.iter().map(|&(pc, inst_index)| {
            let inst = unsafe { self.decode_instruction(pc, inst_index) };
            (inst, self.inst_spans[inst_index])
        })
    }

    pub fn pretty_print(&self, f: &mut dyn fmt::Write, indent: u8) -> fmt::Result {
        for (i, inst) in self.decode().map(|(i, _)| i).enumerate() {
            write!(f, "{:indent$}", "", indent = indent as usize)?;
            write!(f, "{i}: ")?;
            inst.pretty_print(f)?;
            writeln!(f)?;
        }
        Ok(())
    }

    fn inst_index_for_pc(&self, pc: usize) -> Option<usize> {
        let i = self
            .inst_boundaries
            .binary_search_by_key(&pc, |&(offset, _)| offset)
            .ok()?;
        Some(self.inst_boundaries[i].1)
    }

    unsafe fn decode_instruction(&self, pc: usize, inst_index: usize) -> Instruction {
        unsafe {
            let mut ptr = self.bytes.as_ptr().add(pc);
            let opcode: OpCode = bytecode_read(&mut ptr);

            macro_rules! decode {
                (
                    $([simple] $(#[$_simple_attr:meta])* $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
                    $([jump] $(#[$_jump_attr:meta])* $jump_snake_name:ident = $jump_name:ident { offset: $jump_offset_ty:ty $(, $jump_field:ident : $jump_field_ty:ty )* };)*
                    $([call] $(#[$_call_attr:meta])* $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
                ) => {
                    match opcode {
                        $(OpCode::$simple_name => {
                            let params::$simple_name { $($simple_field),* } = bytecode_read(&mut ptr);
                            Instruction::$simple_name { $($simple_field),* }
                        })*
                        $(OpCode::$jump_name => {
                            let params::$jump_name { mut offset $(, $jump_field)* } = bytecode_read(&mut ptr);
                            let target_index = self.inst_index_for_pc(
                                (ptr.offset_from(self.bytes.as_ptr()) + offset as isize) as usize
                            ).unwrap();
                            offset = (target_index as isize - inst_index as isize) as $jump_offset_ty;
                            Instruction::$jump_name { offset $(, $jump_field)* }
                        })*
                        $(OpCode::$call_name => {
                            let params::$call_name { $($call_field),* } = bytecode_read(&mut ptr);
                            Instruction::$call_name { $($call_field),* }
                        })*
                    }
                };
            }

            for_each_instruction!(decode)
        }
    }
}

impl fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "ByteCode[")?;
        self.pretty_print(f, 4)?;
        writeln!(f, "]")?;
        Ok(())
    }
}

/// Read [`ByteCode`] in a optimized way and dispatch to instruction handlers.
///
/// Highly unsafe internally and relies on `ByteCode` being built correctly for soundness. Since
/// it is only possible to build `ByteCode` in-memory by validating a sequence of [`Instruction`]s,
/// this can provide a completely safe interface to highly optimized instruction dispatch.
pub struct Dispatcher<'a> {
    bytecode: &'a ByteCode,
    ptr: *const MaybeUninit<u8>,
}

impl<'a> Dispatcher<'a> {
    /// Construct a new `Dispatcher` for the given bytecode, restoring the given `pc` program
    /// counter.
    ///
    /// # Panics
    ///
    /// Panics if given a program counter that does not fall on an instruction start point. `0` is
    /// always a valid program counter for the starting instruction.
    #[inline]
    pub fn new(bytecode: &'a ByteCode, pc: usize) -> Self {
        assert!(pc == bytecode.bytes.len() || bytecode.inst_index_for_pc(pc).is_some());
        Self {
            bytecode,
            ptr: unsafe { bytecode.bytes.as_ptr().add(pc) },
        }
    }

    #[inline]
    pub fn bytecode(&self) -> &'a ByteCode {
        self.bytecode
    }

    /// Returns the current program counter.
    ///
    /// The "program counter" is the byte offset of the next instruction to execute in the bytecode.
    ///
    /// `Dispatcher` state can be completely restored by constructing a new `Dispatcher` with the
    /// same [`ByteCode`] and a stored program counter.
    #[inline]
    pub fn pc(&self) -> usize {
        unsafe { self.ptr.offset_from(self.bytecode.bytes.as_ptr()) as usize }
    }

    /// Returns true if this `Dispatcher` has reached the end of bytecode.
    pub fn at_end(&self) -> bool {
        self.pc() == self.bytecode.bytes.len()
    }

    /// Dispatch instructions to the given [`Dispatch`] impl.
    ///
    /// # Panics
    ///
    /// Panics if `Self::at_end()` returns true.
    #[inline]
    pub fn dispatch_loop<D: Dispatch>(&mut self, dispatch: &mut D) -> Result<D::Break, D::Error> {
        assert!(
            self.pc() != self.bytecode.bytes.len(),
            "dispatcher reached end of bytecode"
        );

        loop {
            let prev_ptr = self.ptr;
            match self.dispatch_one(dispatch) {
                Ok(ControlFlow::Continue(())) => {}
                Ok(ControlFlow::Break(b)) => {
                    return Ok(b);
                }
                Err(err) => {
                    // Reset the PC back to the instruction that caused the error.
                    self.ptr = prev_ptr;
                    return Err(err);
                }
            }
        }
    }

    #[inline]
    fn dispatch_one<D: Dispatch>(
        &mut self,
        dispatch: &mut D,
    ) -> Result<ControlFlow<D::Break>, D::Error> {
        unsafe {
            let opcode: OpCode = bytecode_read(&mut self.ptr);

            macro_rules! dispatch {
                (
                    $([simple] $(#[$_simple_attr:meta])* $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
                    $([jump] $(#[$_jump_attr:meta])* $jump_snake_name:ident = $jump_name:ident { $( $jump_field:ident : $jump_field_ty:ty ),* };)*
                    $([call] $(#[$_call_attr:meta])* $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
                ) => {
                    match opcode {
                        $(
                            OpCode::$simple_name => {
                                let params::$simple_name { $($simple_field),* } = bytecode_read(&mut self.ptr);
                                dispatch.$simple_snake_name($($simple_field),*)?;
                            }
                        )*

                        OpCode::Jump => {
                            let params::Jump { offset } = bytecode_read(&mut self.ptr);
                            self.ptr = self.ptr.offset(offset as isize);
                        }
                        OpCode::JumpIf => {
                            let params::JumpIf {
                                arg,
                                is_true,
                                offset,
                            } = bytecode_read(&mut self.ptr);
                            if dispatch.check(arg, is_true)? {
                                self.ptr = self.ptr.offset(offset as isize);
                            }
                        }
                        OpCode::Call => {
                            let params::Call {
                                func,
                                stack_bottom,
                            } = bytecode_read(&mut self.ptr);
                            if let ControlFlow::Break(b) = dispatch.call(func, stack_bottom)? {
                                return Ok(ControlFlow::Break(b));
                            }
                        }
                        OpCode::Return => {
                            let params::Return { stack_bottom } = bytecode_read(&mut self.ptr);
                            return dispatch.return_(stack_bottom).map(ControlFlow::Break);
                        }
                    }
                };
            }

            for_each_instruction!(dispatch);
        }

        Ok(ControlFlow::Continue(()))
    }
}

macro_rules! define_dispatch {
    (
        $([simple] $(#[$_simple_attr:meta])* $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
        $([jump] $(#[$_jump_attr:meta])* $jump_snake_name:ident = $jump_name:ident { $( $jump_field:ident : $jump_field_ty:ty ),* };)*
        $([call] $(#[$_call_attr:meta])* $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
    ) => {
        pub trait Dispatch {
            type Break;
            type Error;

            $(fn $simple_snake_name(&mut self, $($simple_field: $simple_field_ty),*) -> Result<(), Self::Error>;)*

            fn check(&mut self, arg: RegIdx, is_true: bool) -> Result<bool, Self::Error>;

            fn call(
                &mut self,
                func: RegIdx,
                stack_bottom: RegIdx,
            ) -> Result<ControlFlow<Self::Break>, Self::Error>;

            fn return_(&mut self, stack_bottom: RegIdx) -> Result<Self::Break, Self::Error>;
        }
    };
}
for_each_instruction!(define_dispatch);

mod params {
    use super::*;

    macro_rules! define_params {
        ($(
            [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
        )*) => {
            $(
                #[derive(Copy, Clone)]
                #[repr(packed)]
                pub struct $name {
                    $(pub $field: $field_ty),*
                }
            )*
        };
    }
    for_each_instruction!(define_params);
}

macro_rules! define_opcode {
    ($(
        [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
    )*) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #[repr(u8)]
        enum OpCode {
            $($name),*
        }
    };
}
for_each_instruction!(define_opcode);

#[inline]
fn bytecode_write<T: Copy>(buf: &mut Vec<MaybeUninit<u8>>, val: T) {
    unsafe {
        let len = buf.len();
        buf.reserve(mem::size_of::<T>());
        let p = buf.as_mut_ptr().add(len) as *mut T;
        p.write_unaligned(val);
        buf.set_len(len + mem::size_of::<T>());
    }
}

#[inline]
unsafe fn bytecode_read<T: Copy>(ptr: &mut *const MaybeUninit<u8>) -> T {
    unsafe {
        let p = *ptr as *const T;
        let v = p.read_unaligned();
        *ptr = ptr.add(mem::size_of::<T>());
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_decode() {
        let insts = &[
            Instruction::LoadConstant {
                constant: 1,
                dest: 2,
            },
            Instruction::Jump { offset: 1 },
            Instruction::IsEqual {
                left: 3,
                right: 4,
                dest: 5,
            },
            Instruction::JumpIf {
                offset: -2,
                arg: 6,
                is_true: true,
            },
            Instruction::Copy { source: 7, dest: 8 },
            Instruction::Return { stack_bottom: 0 },
        ];

        let bytecode = ByteCode::encode(insts.iter().map(|&i| (i, Span::null()))).unwrap();
        let decoded = bytecode.decode().map(|(i, _)| i).collect::<Vec<_>>();
        assert_eq!(insts, decoded.as_slice());
    }
}
