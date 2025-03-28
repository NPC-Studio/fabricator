use std::{
    mem::{self, MaybeUninit},
    ops::ControlFlow,
};

use bit_vec::BitVec;
use gc_arena::Collect;
use thiserror::Error;

use crate::instructions::{ConstIdx, HeapIdx, Instruction, RegIdx};

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
            match inst {
                Instruction::LoadConstant { .. } => OpCode::LoadConstant,
                Instruction::GetHeap { .. } => OpCode::GetHeap,
                Instruction::SetHeap { .. } => OpCode::SetHeap,
                Instruction::Move { .. } => OpCode::Move,
                Instruction::Not { .. } => OpCode::Not,
                Instruction::Add { .. } => OpCode::Add,
                Instruction::Sub { .. } => OpCode::Sub,
                Instruction::TestEqual { .. } => OpCode::TestEqual,
                Instruction::TestNotEqual { .. } => OpCode::TestNotEqual,
                Instruction::TestLess { .. } => OpCode::TestLess,
                Instruction::TestLessEqual { .. } => OpCode::TestLessEqual,
                Instruction::Jump { .. } => OpCode::Jump,
                Instruction::JumpIf { .. } => OpCode::JumpIf,
                Instruction::Push { .. } => OpCode::Push,
                Instruction::Pop { .. } => OpCode::Pop,
                Instruction::Call { .. } => OpCode::Call,
                Instruction::Return { .. } => OpCode::Return,
            }
        }

        fn op_param_len(opcode: OpCode) -> usize {
            match opcode {
                OpCode::LoadConstant => mem::size_of::<LoadConstantParams>(),
                OpCode::GetHeap => mem::size_of::<GetHeapParams>(),
                OpCode::SetHeap => mem::size_of::<SetHeapParams>(),
                OpCode::Move => mem::size_of::<MoveParams>(),
                OpCode::Not => mem::size_of::<NotParams>(),
                OpCode::Add => mem::size_of::<AddParams>(),
                OpCode::Sub => mem::size_of::<SubParams>(),
                OpCode::TestEqual => mem::size_of::<TestEqualParams>(),
                OpCode::TestNotEqual => mem::size_of::<TestNotEqualParams>(),
                OpCode::TestLess => mem::size_of::<TestLessParams>(),
                OpCode::TestLessEqual => mem::size_of::<TestLessEqualParams>(),
                OpCode::Jump => mem::size_of::<JumpParams>(),
                OpCode::JumpIf => mem::size_of::<JumpIfParams>(),
                OpCode::Push => mem::size_of::<PushParams>(),
                OpCode::Pop => mem::size_of::<PopParams>(),
                OpCode::Call => mem::size_of::<CallParams>(),
                OpCode::Return => mem::size_of::<ReturnParams>(),
            }
        }

        let check_jump = |i: usize, offset: i16| match i.checked_add_signed(offset as isize) {
            Some(i) if i < insts.len() => Ok(()),
            _ => {
                dbg!(i, i.checked_add_signed(offset as isize), insts.len());
                Err(ByteCodeEncodingError::JumpOutOfRange)
            }
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
        for (i, &inst) in insts.iter().enumerate() {
            assert_eq!(inst_positions[i], bytes.len());
            inst_boundaries.set(bytes.len(), true);

            bytecode_write(&mut bytes, opcode_for_inst(inst));

            match inst {
                Instruction::LoadConstant { constant, dest } => {
                    bytecode_write(&mut bytes, LoadConstantParams { constant, dest });
                }
                Instruction::GetHeap { heap, dest } => {
                    bytecode_write(&mut bytes, GetHeapParams { heap, dest });
                }
                Instruction::SetHeap { source, heap } => {
                    bytecode_write(&mut bytes, SetHeapParams { source, heap });
                }
                Instruction::Move { source, dest } => {
                    bytecode_write(&mut bytes, MoveParams { source, dest });
                }
                Instruction::Not { arg, dest } => {
                    bytecode_write(&mut bytes, NotParams { arg, dest });
                }
                Instruction::TestEqual { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, TestEqualParams { arg1, arg2, dest });
                }
                Instruction::TestNotEqual { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, TestNotEqualParams { arg1, arg2, dest });
                }
                Instruction::TestLess { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, TestLessParams { arg1, arg2, dest });
                }
                Instruction::TestLessEqual { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, TestLessEqualParams { arg1, arg2, dest });
                }
                Instruction::Jump { offset } => {
                    bytecode_write(
                        &mut bytes,
                        JumpParams {
                            offset: calc_jump(i, offset)?,
                        },
                    );
                }
                Instruction::JumpIf {
                    arg,
                    is_true,
                    offset,
                } => {
                    bytecode_write(
                        &mut bytes,
                        JumpIfParams {
                            arg,
                            is_true,
                            offset: calc_jump(i, offset)?,
                        },
                    );
                }
                Instruction::Add { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, AddParams { arg1, arg2, dest });
                }
                Instruction::Sub { arg1, arg2, dest } => {
                    bytecode_write(&mut bytes, SubParams { arg1, arg2, dest });
                }
                Instruction::Push { source, len } => {
                    bytecode_write(&mut bytes, PushParams { source, len });
                }
                Instruction::Pop { dest, len } => {
                    bytecode_write(&mut bytes, PopParams { dest, len });
                }
                Instruction::Call {
                    func,
                    args,
                    returns,
                } => {
                    bytecode_write(
                        &mut bytes,
                        CallParams {
                            func,
                            args,
                            returns,
                        },
                    );
                }
                Instruction::Return { returns } => {
                    bytecode_write(&mut bytes, ReturnParams { returns });
                }
            }
        }

        Ok(Self {
            bytes: bytes.into_boxed_slice(),
            inst_boundaries,
        })
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
        assert!(bytecode.inst_boundaries[pc]);
        Self {
            bytecode,
            ptr: unsafe { bytecode.bytes.as_ptr().offset(pc as isize) },
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

    /// Dispatch instructions to the given [`Dispatch`] impl.
    #[inline]
    pub fn dispatch_loop<D: Dispatch>(&mut self, dispatch: &mut D) -> Result<D::Break, D::Error> {
        loop {
            debug_assert!(self.bytecode.inst_boundaries[self.pc()]);

            unsafe {
                let opcode: OpCode = bytecode_read(&mut self.ptr);

                match opcode {
                    OpCode::LoadConstant => {
                        let LoadConstantParams { constant, dest } = bytecode_read(&mut self.ptr);
                        dispatch.load_constant(constant, dest);
                    }
                    OpCode::GetHeap => {
                        let GetHeapParams { heap, dest } = bytecode_read(&mut self.ptr);
                        dispatch.get_heap(heap, dest);
                    }
                    OpCode::SetHeap => {
                        let SetHeapParams { source, heap } = bytecode_read(&mut self.ptr);
                        dispatch.set_heap(source, heap);
                    }
                    OpCode::Move => {
                        let MoveParams { source, dest } = bytecode_read(&mut self.ptr);
                        dispatch.move_(source, dest);
                    }
                    OpCode::Not => {
                        let NotParams { arg, dest } = bytecode_read(&mut self.ptr);
                        dispatch.not(arg, dest)?;
                    }
                    OpCode::Add => {
                        let AddParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.add(arg1, arg2, dest)?;
                    }
                    OpCode::Sub => {
                        let SubParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.sub(arg1, arg2, dest)?;
                    }
                    OpCode::TestEqual => {
                        let TestEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.test_equal(arg1, arg2, dest)?;
                    }
                    OpCode::TestNotEqual => {
                        let TestNotEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.test_not_equal(arg1, arg2, dest)?;
                    }
                    OpCode::TestLess => {
                        let TestLessParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.test_less(arg1, arg2, dest)?;
                    }
                    OpCode::TestLessEqual => {
                        let TestLessEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                        dispatch.test_less_equal(arg1, arg2, dest)?;
                    }
                    OpCode::Jump => {
                        let JumpParams { offset } = bytecode_read(&mut self.ptr);
                        self.ptr = self.ptr.offset(offset as isize);
                    }
                    OpCode::JumpIf => {
                        let JumpIfParams {
                            arg,
                            is_true,
                            offset,
                        } = bytecode_read(&mut self.ptr);
                        if dispatch.check(arg, is_true)? {
                            self.ptr = self.ptr.offset(offset as isize);
                        }
                    }
                    OpCode::Push => {
                        let PushParams { source, len } = bytecode_read(&mut self.ptr);
                        dispatch.push(source, len)?;
                    }
                    OpCode::Pop => {
                        let PopParams { dest, len } = bytecode_read(&mut self.ptr);
                        dispatch.pop(dest, len)?;
                    }
                    OpCode::Call => {
                        let CallParams {
                            func,
                            args,
                            returns,
                        } = bytecode_read(&mut self.ptr);
                        if let ControlFlow::Break(b) = dispatch.call(func, args, returns)? {
                            return Ok(b);
                        }
                    }
                    OpCode::Return => {
                        let ReturnParams { returns } = bytecode_read(&mut self.ptr);
                        return dispatch.return_(returns);
                    }
                }
            }
        }
    }
}

pub trait Dispatch {
    type Break;
    type Error;

    fn load_constant(&mut self, constant: ConstIdx, dest: RegIdx);
    fn get_heap(&mut self, source: HeapIdx, dest: RegIdx);
    fn set_heap(&mut self, source: RegIdx, dest: HeapIdx);
    fn move_(&mut self, source: RegIdx, dest: RegIdx);
    fn not(&mut self, arg: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn add(&mut self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn sub(&mut self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_equal(&mut self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_not_equal(
        &mut self,
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    ) -> Result<(), Self::Error>;
    fn test_less(&mut self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_less_equal(
        &mut self,
        arg1: RegIdx,
        arg2: RegIdx,
        dest: RegIdx,
    ) -> Result<(), Self::Error>;
    fn check(&mut self, arg: RegIdx, is_true: bool) -> Result<bool, Self::Error>;
    fn push(&mut self, source: RegIdx, len: u8) -> Result<(), Self::Error>;
    fn pop(&mut self, dest: RegIdx, len: u8) -> Result<(), Self::Error>;
    fn call(
        &mut self,
        func: RegIdx,
        args: u8,
        returns: u8,
    ) -> Result<ControlFlow<Self::Break>, Self::Error>;
    fn return_(&mut self, returns: u8) -> Result<Self::Break, Self::Error>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
enum OpCode {
    LoadConstant,
    GetHeap,
    SetHeap,
    Move,
    Not,
    Add,
    Sub,
    TestEqual,
    TestNotEqual,
    TestLess,
    TestLessEqual,
    Jump,
    JumpIf,
    Push,
    Pop,
    Call,
    Return,
}

#[repr(packed)]
struct LoadConstantParams {
    constant: ConstIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct GetHeapParams {
    heap: HeapIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct SetHeapParams {
    source: RegIdx,
    heap: HeapIdx,
}

#[repr(packed)]
struct MoveParams {
    source: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct NotParams {
    arg: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct AddParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct SubParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct TestEqualParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct TestNotEqualParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct TestLessParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct TestLessEqualParams {
    arg1: RegIdx,
    arg2: RegIdx,
    dest: RegIdx,
}

#[repr(packed)]
struct JumpParams {
    offset: i16,
}

#[repr(packed)]
struct JumpIfParams {
    arg: RegIdx,
    is_true: bool,
    offset: i16,
}

#[repr(packed)]
struct PushParams {
    source: RegIdx,
    len: u8,
}

#[repr(packed)]
struct PopParams {
    dest: RegIdx,
    len: u8,
}

#[repr(packed)]
struct CallParams {
    func: RegIdx,
    args: u8,
    returns: u8,
}

#[repr(packed)]
struct ReturnParams {
    returns: u8,
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
    fn test_bytecode_dispatch() {
        fn check(insts: &[Instruction]) {
            struct Checker<'a> {
                insts: &'a [Instruction],
                pos: usize,
            }

            impl<'a> Dispatch for Checker<'a> {
                type Break = ();
                type Error = ();

                fn load_constant(&mut self, constant: ConstIdx, dest: RegIdx) {
                    assert!(self.insts[self.pos] == Instruction::LoadConstant { constant, dest });
                    self.pos += 1;
                }

                fn get_heap(&mut self, heap: HeapIdx, dest: RegIdx) {
                    assert!(self.insts[self.pos] == Instruction::GetHeap { heap, dest });
                    self.pos += 1;
                }

                fn set_heap(&mut self, source: HeapIdx, heap: RegIdx) {
                    assert!(self.insts[self.pos] == Instruction::SetHeap { source, heap });
                    self.pos += 1;
                }

                fn move_(&mut self, s: RegIdx, d: RegIdx) {
                    assert!(
                        matches!(self.insts[self.pos], Instruction::Move { source, dest } if source == s && dest == d)
                    );
                    self.pos += 1;
                }

                fn not(&mut self, arg: RegIdx, dest: RegIdx) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::Not { arg, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn add(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::Add { arg1, arg2, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn sub(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::Sub { arg1, arg2, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn test_equal(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::TestEqual { arg1, arg2, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn test_not_equal(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::TestNotEqual { arg1, arg2, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn test_less(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.insts[self.pos] == Instruction::TestLess { arg1, arg2, dest });
                    self.pos += 1;
                    Ok(())
                }

                fn test_less_equal(
                    &mut self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(
                        self.insts[self.pos] == Instruction::TestLessEqual { arg1, arg2, dest }
                    );
                    self.pos += 1;
                    Ok(())
                }

                fn check(&mut self, a: RegIdx, it: bool) -> Result<bool, Self::Error> {
                    assert!(matches!(self.insts[self.pos], Instruction::JumpIf {
                        arg,
                        is_true,
                        ..
                    } if arg == a && is_true == it));
                    self.pos += 1;
                    Ok(false)
                }

                fn push(&mut self, s: RegIdx, l: u8) -> Result<(), Self::Error> {
                    assert!(matches!(self.insts[self.pos], Instruction::Push {
                        source,
                        len,
                    } if source == s && len == l));
                    self.pos += 1;
                    Ok(())
                }

                fn pop(&mut self, d: RegIdx, l: u8) -> Result<(), Self::Error> {
                    assert!(matches!(self.insts[self.pos], Instruction::Pop {
                        dest,
                        len,
                    } if dest == d && len == l));
                    self.pos += 1;
                    Ok(())
                }

                fn call(
                    &mut self,
                    f: RegIdx,
                    a: u8,
                    r: u8,
                ) -> Result<ControlFlow<Self::Break>, Self::Error> {
                    assert!(matches!(self.insts[self.pos], Instruction::Call {
                        func,
                        args,
                        returns,
                    } if func == f && args == a && returns == r));
                    self.pos += 1;
                    Ok(ControlFlow::Continue(()))
                }

                fn return_(&mut self, r: u8) -> Result<Self::Break, Self::Error> {
                    assert!(matches!(self.insts[self.pos], Instruction::Return {
                        returns,
                    } if returns == r));
                    self.pos += 1;
                    Ok(())
                }
            }

            let mut checker = Checker { insts, pos: 0 };

            let bytecode = ByteCode::encode(&insts).unwrap();

            Dispatcher::new(&bytecode, 0)
                .dispatch_loop(&mut checker)
                .unwrap();
        }

        check(&[
            Instruction::LoadConstant {
                constant: 12,
                dest: 3,
            },
            Instruction::Move { source: 2, dest: 3 },
            Instruction::Not { arg: 3, dest: 1 },
            Instruction::Add {
                arg1: 3,
                arg2: 2,
                dest: 1,
            },
            Instruction::Sub {
                arg1: 1,
                arg2: 2,
                dest: 3,
            },
            Instruction::Push { source: 13, len: 4 },
            Instruction::Pop { dest: 14, len: 5 },
            Instruction::Call {
                func: 7,
                args: 2,
                returns: 1,
            },
            Instruction::TestEqual {
                arg1: 3,
                arg2: 2,
                dest: 3,
            },
            Instruction::TestNotEqual {
                arg1: 7,
                arg2: 9,
                dest: 7,
            },
            Instruction::TestLess {
                arg1: 5,
                arg2: 4,
                dest: 3,
            },
            Instruction::TestLessEqual {
                arg1: 7,
                arg2: 8,
                dest: 9,
            },
            Instruction::JumpIf {
                arg: 11,
                is_true: true,
                offset: -1,
            },
            Instruction::Push { source: 1, len: 2 },
            Instruction::Pop { dest: 2, len: 1 },
            Instruction::Call {
                func: 3,
                args: 2,
                returns: 1,
            },
            Instruction::GetHeap { heap: 9, dest: 10 },
            Instruction::SetHeap {
                source: 11,
                heap: 12,
            },
            Instruction::Return { returns: 0 },
        ]);
    }
}
