use std::{
    mem::{self, MaybeUninit},
    ops::ControlFlow,
};

use gc_arena::Collect;
use thiserror::Error;

use crate::instructions::{ConstIdx, Instruction, RegIdx};

#[derive(Debug, Error)]
pub enum ByteCodeEncodingError {
    #[error("register out of range")]
    RegisterOutOfRange,
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
    inst_boundaries: BoolVec,
    max_register: RegIdx,
    max_constant: ConstIdx,
}

impl ByteCode {
    /// Encode a list of instructions as bytecode.
    pub fn encode(insts: &[Instruction]) -> Result<Self, ByteCodeEncodingError> {
        let mut max_register = 0;
        let mut max_constant = 0;

        let mut check_register = |r: RegIdx, len: u8| {
            if len > 0 {
                match r.checked_add(len - 1) {
                    Some(r) => {
                        max_register = max_register.max(r);
                        Ok(())
                    }
                    None => Err(ByteCodeEncodingError::RegisterOutOfRange),
                }
            } else {
                Ok(())
            }
        };

        let mut check_constant = |c: ConstIdx| {
            max_constant = max_constant.max(c);
        };

        let check_jump = |i: usize, offset: i16| match i.checked_add_signed(offset as isize) {
            Some(i) if i < insts.len() => Ok(()),
            _ => {
                dbg!(i, i.checked_add_signed(offset as isize), insts.len());
                Err(ByteCodeEncodingError::JumpOutOfRange)
            }
        };

        for (i, &inst) in insts.iter().enumerate() {
            match inst {
                Instruction::Load { constant, dest } => {
                    check_constant(constant);
                    check_register(dest, 1)?;
                }
                Instruction::Move { source, dest } => {
                    check_register(source, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::Not { arg, dest } => {
                    check_register(arg, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::Add { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::Sub { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::TestEqual { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::TestNotEqual { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::TestLess { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::TestLessEqual { arg1, arg2, dest } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::Jump { offset } => {
                    check_jump(i, offset)?;
                }
                Instruction::JumpIf {
                    arg: test, offset, ..
                } => {
                    check_register(test, 1)?;
                    check_jump(i, offset)?;
                }
                Instruction::Push { source, len } => {
                    check_register(source, len)?;
                }
                Instruction::Pop { dest, len } => {
                    check_register(dest, len)?;
                }
                Instruction::Call { func, .. } => {
                    check_register(func, 1)?;
                }
                Instruction::Return { .. } => {}
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
            pos += 1 + OpCode::for_inst(inst).param_len();
        }
        let mut inst_boundaries = BoolVec::new(pos);

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

            bytecode_write(&mut bytes, OpCode::for_inst(inst));

            match inst {
                Instruction::Load { constant, dest } => {
                    bytecode_write(&mut bytes, LoadParams { constant, dest });
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
            max_register,
            max_constant,
        })
    }

    #[inline]
    pub fn max_register(&self) -> RegIdx {
        self.max_register
    }

    #[inline]
    pub fn max_constant(&self) -> ConstIdx {
        self.max_constant
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
        assert!(bytecode.inst_boundaries.get(pc));
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

    /// Dispatch a single instruction to the given [`Dispatch`] impl.
    #[inline]
    pub fn dispatch<D: Dispatch>(
        &mut self,
        dispatch: D,
    ) -> Result<ControlFlow<D::Break>, D::Error> {
        debug_assert!(self.bytecode.inst_boundaries.get(self.pc()));

        unsafe {
            let opcode: OpCode = bytecode_read(&mut self.ptr);

            Ok(match opcode {
                OpCode::Load => {
                    let LoadParams { constant, dest } = bytecode_read(&mut self.ptr);
                    dispatch.load(constant, dest);
                    ControlFlow::Continue(())
                }
                OpCode::Move => {
                    let MoveParams { source, dest } = bytecode_read(&mut self.ptr);
                    dispatch.move_(source, dest);
                    ControlFlow::Continue(())
                }
                OpCode::Not => {
                    let NotParams { arg, dest } = bytecode_read(&mut self.ptr);
                    dispatch.not(arg, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::Add => {
                    let AddParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.add(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::Sub => {
                    let SubParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.sub(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::TestEqual => {
                    let TestEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.test_equal(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::TestNotEqual => {
                    let TestNotEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.test_not_equal(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::TestLess => {
                    let TestLessParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.test_less(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::TestLessEqual => {
                    let TestLessEqualParams { arg1, arg2, dest } = bytecode_read(&mut self.ptr);
                    dispatch.test_less_equal(arg1, arg2, dest)?;
                    ControlFlow::Continue(())
                }
                OpCode::Jump => {
                    let JumpParams { offset } = bytecode_read(&mut self.ptr);
                    self.ptr = self.ptr.offset(offset as isize);
                    ControlFlow::Continue(())
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
                    ControlFlow::Continue(())
                }
                OpCode::Push => {
                    let PushParams { source, len } = bytecode_read(&mut self.ptr);
                    dispatch.push(source, len)?;
                    ControlFlow::Continue(())
                }
                OpCode::Pop => {
                    let PopParams { dest, len } = bytecode_read(&mut self.ptr);
                    dispatch.pop(dest, len)?;
                    ControlFlow::Continue(())
                }
                OpCode::Call => {
                    let CallParams {
                        func,
                        args,
                        returns,
                    } = bytecode_read(&mut self.ptr);
                    dispatch.call(func, args, returns)?
                }
                OpCode::Return => {
                    let ReturnParams { returns } = bytecode_read(&mut self.ptr);
                    ControlFlow::Break(dispatch.return_(returns)?)
                }
            })
        }
    }
}

pub trait Dispatch {
    type Break;
    type Error;

    fn load(self, constant: ConstIdx, dest: RegIdx);
    fn move_(self, source: RegIdx, dest: RegIdx);
    fn not(self, arg: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn add(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn sub(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_equal(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_not_equal(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_less(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn test_less_equal(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error>;
    fn check(self, arg: RegIdx, is_true: bool) -> Result<bool, Self::Error>;
    fn push(self, source: RegIdx, len: u8) -> Result<(), Self::Error>;
    fn pop(self, dest: RegIdx, len: u8) -> Result<(), Self::Error>;
    fn call(
        self,
        func: RegIdx,
        args: u8,
        returns: u8,
    ) -> Result<ControlFlow<Self::Break>, Self::Error>;
    fn return_(self, returns: u8) -> Result<Self::Break, Self::Error>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
enum OpCode {
    Load,
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

impl OpCode {
    #[inline]
    fn for_inst(inst: Instruction) -> Self {
        match inst {
            Instruction::Load { .. } => OpCode::Load,
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

    #[inline]
    fn param_len(self) -> usize {
        match self {
            OpCode::Load => mem::size_of::<LoadParams>(),
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
}

#[repr(packed)]
struct LoadParams {
    constant: ConstIdx,
    dest: RegIdx,
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

#[derive(Debug)]
struct BoolVec(Box<[u8]>);

impl BoolVec {
    fn new(min_len: usize) -> Self {
        Self(vec![0; min_len.div_ceil(8)].into_boxed_slice())
    }

    #[inline]
    fn set(&mut self, i: usize, val: bool) {
        let base = i / 8;
        let off = i % 8;
        if val {
            self.0[base] |= 1 << off;
        } else {
            self.0[base] &= !(1 << off);
        }
    }

    #[inline]
    fn get(&self, i: usize) -> bool {
        let base = i / 8;
        let off = i % 8;
        self.0[base] & (1 << off) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bool_vec() {
        let mut bv = BoolVec::new(17);

        bv.set(0, true);
        bv.set(7, true);
        bv.set(16, true);

        assert!(bv.get(0));
        assert!(bv.get(7));
        assert!(bv.get(16));
        assert!(!bv.get(1));
        assert!(!bv.get(6));
        assert!(!bv.get(8));
        assert!(!bv.get(15));
    }

    #[test]
    fn test_bytecode_dispatch() {
        fn check(insts: &[Instruction]) {
            struct Checker {
                inst: Instruction,
            }

            impl Dispatch for Checker {
                type Break = ();
                type Error = ();

                fn load(self, c: ConstIdx, d: RegIdx) {
                    assert!(matches!(self.inst, Instruction::Load {
                        constant,
                        dest,
                    } if constant == c && dest == d));
                }

                fn move_(self, s: RegIdx, d: RegIdx) {
                    assert!(
                        matches!(self.inst, Instruction::Move { source, dest } if source == s && dest == d)
                    );
                }

                fn not(self, arg: RegIdx, dest: RegIdx) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::Not { arg, dest });
                    Ok(())
                }

                fn add(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::Add { arg1, arg2, dest });
                    Ok(())
                }

                fn sub(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::Sub { arg1, arg2, dest });
                    Ok(())
                }

                fn test_equal(
                    self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::TestEqual { arg1, arg2, dest });
                    Ok(())
                }

                fn test_not_equal(
                    self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::TestNotEqual { arg1, arg2, dest });
                    Ok(())
                }

                fn test_less(
                    self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::TestLess { arg1, arg2, dest });
                    Ok(())
                }

                fn test_less_equal(
                    self,
                    arg1: RegIdx,
                    arg2: RegIdx,
                    dest: RegIdx,
                ) -> Result<(), Self::Error> {
                    assert!(self.inst == Instruction::TestLessEqual { arg1, arg2, dest });
                    Ok(())
                }

                fn check(self, a: RegIdx, it: bool) -> Result<bool, Self::Error> {
                    assert!(matches!(self.inst, Instruction::JumpIf {
                        arg,
                        is_true,
                        ..
                    } if arg == a && is_true == it));
                    Ok(false)
                }

                fn push(self, s: RegIdx, l: u8) -> Result<(), Self::Error> {
                    assert!(matches!(self.inst, Instruction::Push {
                        source,
                        len,
                    } if source == s && len == l));
                    Ok(())
                }

                fn pop(self, d: RegIdx, l: u8) -> Result<(), Self::Error> {
                    assert!(matches!(self.inst, Instruction::Pop {
                        dest,
                        len,
                    } if dest == d && len == l));
                    Ok(())
                }

                fn call(
                    self,
                    f: RegIdx,
                    a: u8,
                    r: u8,
                ) -> Result<ControlFlow<Self::Break>, Self::Error> {
                    assert!(matches!(self.inst, Instruction::Call {
                        func,
                        args,
                        returns,
                    } if func == f && args == a && returns == r));
                    Ok(ControlFlow::Continue(()))
                }

                fn return_(self, r: u8) -> Result<Self::Break, Self::Error> {
                    assert!(matches!(self.inst, Instruction::Return {
                        returns,
                    } if returns == r));
                    Ok(())
                }
            }

            let bytecode = ByteCode::encode(&insts).unwrap();
            let mut dispatcher = Dispatcher::new(&bytecode, 0);

            let mut pos = 0;
            loop {
                if dispatcher
                    .dispatch(Checker { inst: insts[pos] })
                    .unwrap()
                    .is_break()
                {
                    break;
                }

                pos += 1;
            }
        }

        check(&[
            Instruction::Load {
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
            Instruction::Jump { offset: 1 },
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
            Instruction::Return { returns: 0 },
        ]);
    }
}
