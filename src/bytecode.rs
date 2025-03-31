use std::{
    fmt, iter,
    mem::{self, MaybeUninit},
    ops::ControlFlow,
};

use bit_vec::BitVec;
use gc_arena::Collect;
use thiserror::Error;

use crate::instructions::{for_each_instruction, ConstIdx, HeapIdx, Instruction, RegIdx};

#[derive(Debug, Error)]
pub enum ByteCodeEncodingError {
    #[error("jump out of range")]
    JumpOutOfRange,
    #[error("no return or jump as last instruction")]
    BadlastInstruction,
}

/// An encoded list of [`Instruction`]s.
///
/// Stored in a variable-length, optimized bytecode format internally.
#[derive(Collect)]
#[collect(require_static)]
pub struct ByteCode {
    bytes: Box<[MaybeUninit<u8>]>,
    inst_boundaries: BitVec,
}

impl ByteCode {
    /// Encode a list of instructions as bytecode.
    pub fn encode(insts: &[Instruction]) -> Result<Self, ByteCodeEncodingError> {
        fn opcode_for_inst(inst: Instruction) -> OpCode {
            macro_rules! match_inst {
                ($(
                    $category:ident => $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
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
                    $category:ident => $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
                )*) => {
                    match opcode {
                        $(OpCode::$name { .. } => mem::size_of::<params::$name>()),*
                    }
                };
            }
            for_each_instruction!(match_opcode)
        }

        let check_jump = |i: usize, offset: i16| match i.checked_add_signed(offset as isize) {
            Some(i) if i < insts.len() => Ok(()),
            _ => Err(ByteCodeEncodingError::JumpOutOfRange),
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
            return Err(ByteCodeEncodingError::BadlastInstruction);
        }

        let mut inst_positions = Vec::new();
        let mut pos = 0;
        for &inst in insts {
            inst_positions.push(pos);
            pos += 1 + op_param_len(opcode_for_inst(inst));
        }
        let mut inst_boundaries = BitVec::new();
        inst_boundaries.grow(pos, false);

        let calc_jump = |cur: usize, offset: i16| {
            // Encode jumps from the end of the current instruction
            let cur_pos = inst_positions
                .get(cur + 1)
                .copied()
                .unwrap_or(inst_positions.len()) as isize;
            let jump_pos = inst_positions[(cur as isize + offset as isize) as usize] as isize;
            i16::try_from(jump_pos - cur_pos).map_err(|_| ByteCodeEncodingError::JumpOutOfRange)
        };

        let mut bytes = Vec::new();
        for (i, mut inst) in insts.iter().copied().enumerate() {
            assert_eq!(inst_positions[i], bytes.len());
            inst_boundaries.set(bytes.len(), true);

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
                    $category:ident => $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
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
            inst_boundaries,
        })
    }

    pub fn decode(&self) -> impl Iterator<Item = Instruction> + '_ {
        // Compute a table to go from bytecode positions back to instruction positions
        let mut inst_positions = vec![None; self.inst_boundaries.len()];
        let mut inst_count = 0;
        for (i, boundary) in self.inst_boundaries.iter().enumerate() {
            if boundary {
                inst_positions[i] = Some(inst_count);
                inst_count += 1;
            }
        }

        let mut pc = 0;
        let mut inst_count = 0;
        let bytes = &self.bytes;
        iter::from_fn(move || {
            if pc == bytes.len() {
                return None;
            }

            unsafe {
                let mut ptr = bytes.as_ptr().add(pc);
                let opcode: OpCode = bytecode_read(&mut ptr);

                macro_rules! decode {
                    (
                        $(simple => $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
                        $(jump => $jump_snake_name:ident = $jump_name:ident { offset: $jump_offset_ty:ty $(, $jump_field:ident : $jump_field_ty:ty )* };)*
                        $(call => $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
                    ) => {
                        match opcode {
                            $(OpCode::$simple_name => {
                                let params::$simple_name { $($simple_field),* } = bytecode_read(&mut ptr);
                                Instruction::$simple_name { $($simple_field),* }
                            })*
                            $(OpCode::$jump_name => {
                                let params::$jump_name { mut offset $(, $jump_field)* } = bytecode_read(&mut ptr);
                                let inst_pos = inst_positions[
                                    (ptr.offset_from(bytes.as_ptr()) + offset as isize) as usize
                                ].unwrap();
                                offset = (inst_pos as isize - inst_count) as $jump_offset_ty;
                                Instruction::$jump_name { offset $(, $jump_field)* }
                            })*
                            $(OpCode::$call_name => {
                                let params::$call_name { $($call_field),* } = bytecode_read(&mut ptr);
                                Instruction::$call_name { $($call_field),* }
                            })*
                        }
                    };
                }

                let inst = for_each_instruction!(decode);
                pc = ptr.offset_from(bytes.as_ptr()) as usize;
                inst_count += 1;
                Some(inst)
            }
        })
    }

    fn pretty_print(&self, f: &mut dyn fmt::Write, indent: u8) -> fmt::Result {
        for (i, inst) in self.decode().enumerate() {
            write!(f, "{:indent$}", "", indent = indent as usize)?;
            write!(f, "{}: ", i)?;
            inst.pretty_print(f)?;
            writeln!(f)?;
        }
        Ok(())
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
        assert!(pc == bytecode.bytes.len() || bytecode.inst_boundaries[pc]);
        Self {
            bytecode,
            ptr: unsafe { bytecode.bytes.as_ptr().add(pc) },
        }
    }

    #[inline]
    pub fn bytecode(&self) -> &'a ByteCode {
        self.bytecode
    }

    /// Returns the current program counter. `Dispatcher` state can be completely restored by
    /// constructing a new `Dispatcher` with the same [`ByteCode`] and a stored program counter.
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
            debug_assert!(self.bytecode.inst_boundaries[self.pc()]);

            unsafe {
                let opcode: OpCode = bytecode_read(&mut self.ptr);

                macro_rules! dispatch {
                    (
                        $(simple => $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
                        $(jump => $jump_snake_name:ident = $jump_name:ident { $( $jump_field:ident : $jump_field_ty:ty ),* };)*
                        $(call => $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
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
                                    args,
                                    returns,
                                } = bytecode_read(&mut self.ptr);
                                if let ControlFlow::Break(b) = dispatch.call(func, args, returns)? {
                                    return Ok(b);
                                }
                            }
                            OpCode::Return => {
                                let params::Return { returns } = bytecode_read(&mut self.ptr);
                                return dispatch.return_(returns);
                            }
                        }
                    };
                }

                for_each_instruction!(dispatch);
            }
        }
    }
}

macro_rules! define_dispatch {
    (
        $(simple => $simple_snake_name:ident = $simple_name:ident { $( $simple_field:ident : $simple_field_ty:ty ),* };)*
        $(jump => $jump_snake_name:ident = $jump_name:ident { $( $jump_field:ident : $jump_field_ty:ty ),* };)*
        $(call => $call_snake_name:ident = $call_name:ident { $( $call_field:ident : $call_field_ty:ty ),* };)*
    ) => {
        pub trait Dispatch {
            type Break;
            type Error;

            $(fn $simple_snake_name(&mut self, $($simple_field: $simple_field_ty),*) -> Result<(), Self::Error>;)*

            fn check(&mut self, arg: RegIdx, is_true: bool) -> Result<bool, Self::Error>;

            fn call(
                &mut self,
                func: RegIdx,
                args: u8,
                returns: u8,
            ) -> Result<ControlFlow<Self::Break>, Self::Error>;
            fn return_(&mut self, returns: u8) -> Result<Self::Break, Self::Error>;
        }
    };
}

for_each_instruction!(define_dispatch);

macro_rules! define_opcode {
    ($(
        $category:ident => $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
    )*) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
        #[repr(u8)]
        pub enum OpCode {
            $($name),*
        }
    };
}
for_each_instruction!(define_opcode);

mod params {
    use super::*;

    macro_rules! define_params {
        ($(
            $category:ident => $snake_name:ident = $name:ident { $( $field:ident : $field_ty:ty ),* };
        )*) => {
            $(
                #[repr(packed)]
                pub struct $name {
                    $(pub $field: $field_ty),*
                }
            )*
        };
    }
    for_each_instruction!(define_params);
}

#[inline]
fn bytecode_write<T>(buf: &mut Vec<MaybeUninit<u8>>, val: T) {
    unsafe {
        let len = buf.len();
        buf.reserve(mem::size_of::<T>());
        let p = buf.as_mut_ptr().add(len) as *mut T;
        p.write_unaligned(val);
        buf.set_len(len + mem::size_of::<T>());
    }
}

#[inline]
unsafe fn bytecode_read<T>(ptr: &mut *const MaybeUninit<u8>) -> T {
    unsafe {
        let p = *ptr as *const T;

        // I am not sure why this is not optimized automatically, but it's not.
        let v = if mem::align_of::<T>() == 1 {
            p.read()
        } else {
            p.read_unaligned()
        };

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
            Instruction::TestEqual {
                arg1: 3,
                arg2: 4,
                dest: 5,
            },
            Instruction::JumpIf {
                offset: -2,
                arg: 6,
                is_true: true,
            },
            Instruction::Move { source: 7, dest: 8 },
            Instruction::Return { returns: 0 },
        ];

        let bytecode = ByteCode::encode(insts).unwrap();
        let decoded = bytecode.decode().collect::<Vec<_>>();

        assert_eq!(insts, decoded.as_slice());
    }
}
