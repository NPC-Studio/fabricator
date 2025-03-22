use std::ops::ControlFlow;

use gc_arena::Collect;
use thiserror::Error;

pub type Register = u8;
pub type Constant = u16;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Instruction {
    Move {
        source: Register,
        dest: Register,
    },
    Jump {
        offset: i16,
    },
    JumpIfLess {
        arg1: Register,
        arg2: Register,
        offset: i16,
    },
    JumpIfLessEqual {
        arg1: Register,
        arg2: Register,
        offset: i16,
    },
    Add {
        arg1: Register,
        arg2: Register,
        dest: Register,
    },
    Sub {
        arg1: Register,
        arg2: Register,
        dest: Register,
    },
    Load {
        constant: Constant,
        dest: Register,
    },
    Push {
        source: Register,
        len: u8,
    },
    Pop {
        dest: Register,
        len: u8,
    },
    Call {
        func: Register,
        args: u8,
        returns: u8,
    },
    Return {
        returns: u8,
    },
}

pub trait Dispatch {
    type Return;

    fn move_(self, source: Register, dest: Register) -> ControlFlow<Self::Return>;
    fn test_less(self, arg1: Register, arg2: Register) -> ControlFlow<Self::Return, bool>;
    fn test_less_equal(self, arg1: Register, arg2: Register) -> ControlFlow<Self::Return, bool>;
    fn add(self, arg1: Register, arg2: Register, dest: Register) -> ControlFlow<Self::Return>;
    fn sub(self, arg1: Register, arg2: Register, dest: Register) -> ControlFlow<Self::Return>;
    fn load(self, constant: Constant, dest: Register) -> ControlFlow<Self::Return>;
    fn push(self, source: Register, len: u8) -> ControlFlow<Self::Return>;
    fn pop(self, dest: Register, len: u8) -> ControlFlow<Self::Return>;
    fn call(self, func: Register, args: u8, returns: u8) -> ControlFlow<Self::Return>;
    fn return_(self, returns: u8) -> Self::Return;
}

#[derive(Debug, Error)]
pub enum ByteCodeEncodingError {
    #[error("register out of range")]
    RegisterOutOfRange,
    #[error("jump out of range")]
    JumpOutOfRange,
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub struct ByteCode {
    instructions: Box<[Instruction]>,
    max_register: Register,
    max_constant: Constant,
}

impl ByteCode {
    pub fn encode(insts: &[Instruction]) -> Result<Self, ByteCodeEncodingError> {
        let mut max_register = 0;
        let mut max_constant = 0;

        let mut check_register = |r: Register, len: u8| {
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

        let mut check_constant = |c: Constant| {
            max_constant = max_constant.max(c);
        };

        let check_jump = |i: usize, offset: i16| match i.checked_add_signed(offset as isize) {
            Some(i) if i < insts.len() => Ok(()),
            _ => Err(ByteCodeEncodingError::JumpOutOfRange),
        };

        for (i, &inst) in insts.iter().enumerate() {
            match inst {
                Instruction::Move { source, dest } => {
                    check_register(source, 1)?;
                    check_register(dest, 1)?;
                }
                Instruction::Jump { offset } => {
                    check_jump(i, offset)?;
                }
                Instruction::JumpIfLess { arg1, arg2, offset } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_jump(i, offset)?;
                }
                Instruction::JumpIfLessEqual { arg1, arg2, offset } => {
                    check_register(arg1, 1)?;
                    check_register(arg2, 1)?;
                    check_jump(i, offset)?;
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
                Instruction::Load { constant, dest } => {
                    check_constant(constant);
                    check_register(dest, 1)?;
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

        Ok(Self {
            instructions: insts.to_vec().into_boxed_slice(),
            max_register,
            max_constant,
        })
    }

    #[inline]
    pub fn max_register(&self) -> Register {
        self.max_register
    }

    #[inline]
    pub fn max_constant(&self) -> Constant {
        self.max_constant
    }
}

pub struct Dispatcher<'a> {
    bytecode: &'a ByteCode,
    pc: usize,
}

impl<'a> Dispatcher<'a> {
    #[inline]
    pub fn new(bytecode: &'a ByteCode, pc: usize) -> Self {
        assert!(pc < bytecode.instructions.len());
        Self { bytecode, pc }
    }

    #[inline]
    pub fn bytecode(&self) -> &'a ByteCode {
        self.bytecode
    }

    #[inline]
    pub fn pc(&self) -> usize {
        self.pc
    }

    #[inline]
    pub fn dispatch<D: Dispatch>(&mut self, dispatch: D) -> ControlFlow<D::Return> {
        let inst = self.bytecode.instructions[self.pc];
        self.pc += 1;

        match inst {
            Instruction::Move { source, dest } => {
                dispatch.move_(source, dest)?;
            }
            Instruction::Jump { offset } => {
                self.pc = self.pc.checked_add_signed(offset as isize).unwrap();
            }
            Instruction::JumpIfLess { arg1, arg2, offset } => {
                if dispatch.test_less(arg1, arg2)? {
                    self.pc = self.pc.checked_add_signed(offset as isize).unwrap();
                }
            }
            Instruction::JumpIfLessEqual { arg1, arg2, offset } => {
                if dispatch.test_less_equal(arg1, arg2)? {
                    self.pc = self.pc.checked_add_signed(offset as isize).unwrap();
                }
            }
            Instruction::Add { arg1, arg2, dest } => {
                dispatch.add(arg1, arg2, dest)?;
            }
            Instruction::Sub { arg1, arg2, dest } => {
                dispatch.sub(arg1, arg2, dest)?;
            }
            Instruction::Load { constant, dest } => {
                dispatch.load(constant, dest)?;
            }
            Instruction::Push { source, len } => {
                dispatch.push(source, len)?;
            }
            Instruction::Pop { dest, len } => {
                dispatch.pop(dest, len)?;
            }
            Instruction::Call {
                func,
                args,
                returns,
            } => {
                dispatch.call(func, args, returns)?;
            }
            Instruction::Return { returns } => {
                return ControlFlow::Break(dispatch.return_(returns));
            }
        }

        ControlFlow::Continue(())
    }
}

/*
impl Instruction {
    #[inline]
    pub fn decode(bytecode: &[u8]) -> Option<(Instruction, usize)> {
        if bytecode.len() == 0 {
            return None;
        }

        let opcode = OpCode::decode(bytecode[0])?;
        let args = &bytecode[1..];

        Some(match opcode {
            OpCode::Move => (
                Instruction::Move(Inst::from_args(InstArgs::from_bytes(args))),
                <Move as Inst>::Args::LEN + 1,
            ),
            OpCode::Jump => (
                Instruction::Jump(Inst::from_args(InstArgs::from_bytes(args))),
                <Jump as Inst>::Args::LEN + 1,
            ),
            OpCode::JumpIfLess => (
                Instruction::JumpIfLess(Inst::from_args(InstArgs::from_bytes(args))),
                <JumpIfLess as Inst>::Args::LEN + 1,
            ),
            OpCode::JumpIfLessEqual => (
                Instruction::JumpIfLessEqual(Inst::from_args(InstArgs::from_bytes(args))),
                <JumpIfLessEqual as Inst>::Args::LEN + 1,
            ),
            OpCode::Add => (
                Instruction::Add(Inst::from_args(InstArgs::from_bytes(args))),
                <Add as Inst>::Args::LEN + 1,
            ),
            OpCode::Sub => (
                Instruction::Sub(Inst::from_args(InstArgs::from_bytes(args))),
                <Sub as Inst>::Args::LEN + 1,
            ),
            OpCode::Load => (
                Instruction::Load(Inst::from_args(InstArgs::from_bytes(args))),
                <Load as Inst>::Args::LEN + 1,
            ),
            OpCode::Push => (
                Instruction::Push(Inst::from_args(InstArgs::from_bytes(args))),
                <Push as Inst>::Args::LEN + 1,
            ),
            OpCode::Pop => (
                Instruction::Pop(Inst::from_args(InstArgs::from_bytes(args))),
                <Pop as Inst>::Args::LEN + 1,
            ),
            OpCode::Call => (
                Instruction::Call(Inst::from_args(InstArgs::from_bytes(args))),
                <Call as Inst>::Args::LEN + 1,
            ),
            OpCode::Return => (
                Instruction::Return(Inst::from_args(InstArgs::from_bytes(args))),
                <Return as Inst>::Args::LEN + 1,
            ),
        })
    }

    #[inline]
    pub fn encode(self, bytecode: &mut Vec<u8>) -> usize {
        todo!()
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
enum OpCode {
    Move,
    Jump,
    JumpIfLess,
    JumpIfLessEqual,
    Add,
    Sub,
    Load,
    Push,
    Pop,
    Call,
    Return,
}

impl OpCode {
    #[inline]
    fn decode(byte: u8) -> Option<Self> {
        // SAFETY: `OpCode` must be contiguous (the default with implicit discriminants) and the
        // specified `OpCode` value should be the last variant.
        if byte <= OpCode::Return as u8 {
            Some(unsafe { mem::transmute::<u8, OpCode>(byte) })
        } else {
            None
        }
    }
}

trait InstArgs {
    const LEN: usize;

    fn from_bytes(bytes: &[u8]) -> Self;
    fn to_bytes(self, bytes: &mut [u8]);
}

macro_rules! impl_args_num {
    ($ty:ty, $len:expr) => {
        impl InstArgs for $ty {
            const LEN: usize = $len;

            #[inline]
            fn from_bytes(bytes: &[u8]) -> Self {
                <$ty>::from_le_bytes(*<&[u8; $len]>::try_from(bytes).unwrap())
            }

            #[inline]
            fn to_bytes(self, bytes: &mut [u8]) {
                *<&mut [u8; $len]>::try_from(bytes).unwrap() = <$ty>::to_le_bytes(self);
            }
        }
    };
}

impl_args_num!(u8, 1);
impl_args_num!(u16, 2);
impl_args_num!(u32, 4);

impl_args_num!(i8, 1);
impl_args_num!(i16, 2);
impl_args_num!(i32, 4);

impl InstArgs for bool {
    const LEN: usize = 1;

    #[inline]
    fn from_bytes(bytes: &[u8]) -> Self {
        debug_assert!(bytes[0] == 0 || bytes[0] == 1);
        bytes[0] != 0
    }

    #[inline]
    fn to_bytes(self, bytes: &mut [u8]) {
        bytes[0] = if self { 1 } else { 0 }
    }
}

impl InstArgs for Register {
    const LEN: usize = u8::LEN;

    fn from_bytes(bytes: &[u8]) -> Self {
        Register(u8::from_bytes(bytes))
    }

    fn to_bytes(self, bytes: &mut [u8]) {
        self.0.to_bytes(bytes)
    }
}

impl InstArgs for Constant {
    const LEN: usize = u16::LEN;

    fn from_bytes(bytes: &[u8]) -> Self {
        Constant(u16::from_bytes(bytes))
    }

    fn to_bytes(self, bytes: &mut [u8]) {
        self.0.to_bytes(bytes)
    }
}

macro_rules! impl_tuple {
    ($($item:ident),* $(,)?) => (
        impl<$($item,)*> InstArgs for ($($item,)*)
        where
            $($item: InstArgs,)*
        {
            const LEN: usize = $($item::LEN +)* 0;

            #[inline]
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            #[allow(unused_mut)]
            #[allow(unused_assignments)]
            fn from_bytes(mut bytes: &[u8]) -> Self {
                let ($($item,)*);
                $(
                    $item = $item::from_bytes(&bytes[0..$item::LEN]);
                    bytes = &bytes[$item::LEN..];
                )*

                ($($item,)*)
            }

            #[inline]
            #[allow(non_snake_case)]
            #[allow(unused_variables)]
            #[allow(unused_mut)]
            #[allow(unused_assignments)]
            fn to_bytes(self, mut bytes: &mut [u8]) {
                let ($($item,)*) = self;
                $(
                    $item::to_bytes($item, &mut bytes[0..$item::LEN]);
                    bytes = &mut bytes[$item::LEN..];
                )*
            }
        }
    );
}

macro_rules! smaller_tuples_too {
    ($m: ident, $ty: ident) => {
        $m!{}
        $m!{$ty}
    };

    ($m: ident, $ty: ident, $($tt: ident),*) => {
        smaller_tuples_too!{$m, $($tt),*}
        $m!{$ty, $($tt),*}
    };
}

smaller_tuples_too!(impl_tuple, D, C, B, A);

trait Inst {
    const OPCODE: OpCode;

    type Args: InstArgs;

    fn into_args(self) -> Self::Args;
    fn from_args(args: Self::Args) -> Self;
}

impl Inst for Move {
    const OPCODE: OpCode = OpCode::Move;

    type Args = (Register, Register);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.source, self.dest)
    }

    #[inline]
    fn from_args((source, dest): Self::Args) -> Self {
        Self { source, dest }
    }
}

impl Inst for Jump {
    const OPCODE: OpCode = OpCode::Jump;

    type Args = i16;

    #[inline]
    fn into_args(self) -> Self::Args {
        self.offset
    }

    #[inline]
    fn from_args(offset: Self::Args) -> Self {
        Self { offset }
    }
}

impl Inst for JumpIfLess {
    const OPCODE: OpCode = OpCode::JumpIfLess;

    type Args = (Register, Register, i16);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.arg1, self.arg2, self.offset)
    }

    #[inline]
    fn from_args((arg1, arg2, offset): Self::Args) -> Self {
        Self { arg1, arg2, offset }
    }
}

impl Inst for JumpIfLessEqual {
    const OPCODE: OpCode = OpCode::JumpIfLessEqual;

    type Args = (Register, Register, i16);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.arg1, self.arg2, self.offset)
    }

    #[inline]
    fn from_args((arg1, arg2, offset): Self::Args) -> Self {
        Self { arg1, arg2, offset }
    }
}

impl Inst for Add {
    const OPCODE: OpCode = OpCode::Add;

    type Args = (Register, Register, Register);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.arg1, self.arg2, self.dest)
    }

    #[inline]
    fn from_args((arg1, arg2, dest): Self::Args) -> Self {
        Self { arg1, arg2, dest }
    }
}

impl Inst for Sub {
    const OPCODE: OpCode = OpCode::Sub;

    type Args = (Register, Register, Register);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.arg1, self.arg2, self.dest)
    }

    #[inline]
    fn from_args((arg1, arg2, dest): Self::Args) -> Self {
        Self { arg1, arg2, dest }
    }
}

impl Inst for Load {
    const OPCODE: OpCode = OpCode::Load;

    type Args = (Constant, Register);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.constant, self.dest)
    }

    #[inline]
    fn from_args((constant, dest): Self::Args) -> Self {
        Self { constant, dest }
    }
}

impl Inst for Push {
    const OPCODE: OpCode = OpCode::Push;

    type Args = (Register, u8);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.source, self.len)
    }

    #[inline]
    fn from_args((source, len): Self::Args) -> Self {
        Self { source, len }
    }
}

impl Inst for Pop {
    const OPCODE: OpCode = OpCode::Pop;

    type Args = (Register, u8);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.dest, self.len)
    }

    #[inline]
    fn from_args((dest, len): Self::Args) -> Self {
        Self { dest, len }
    }
}

impl Inst for Call {
    const OPCODE: OpCode = OpCode::Call;

    type Args = (Register, u8, u8);

    #[inline]
    fn into_args(self) -> Self::Args {
        (self.func, self.args, self.returns)
    }

    #[inline]
    fn from_args((func, args, returns): Self::Args) -> Self {
        Self {
            func,
            args,
            returns,
        }
    }
}

impl Inst for Return {
    const OPCODE: OpCode = OpCode::Return;

    type Args = u8;

    #[inline]
    fn into_args(self) -> Self::Args {
        self.returns
    }

    #[inline]
    fn from_args(returns: Self::Args) -> Self {
        Self { returns }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_inst_encoding() {
        fn test_encoding<O: Inst + Eq + std::fmt::Debug + Copy>(op: O) {
            assert_eq!(op, O::from_args(op.into_args()));
        }

        test_encoding(Move {
            source: Register(2),
            dest: Register(3),
        });
        test_encoding(Jump { offset: -7 });
        test_encoding(Add {
            arg1: Register(3),
            arg2: Register(4),
            dest: Register(5),
        });
        test_encoding(Sub {
            arg1: Register(1),
            arg2: Register(2),
            dest: Register(3),
        });
        test_encoding(Load {
            constant: Constant(7),
            dest: Register(3),
        });
        test_encoding(Push {
            source: Register(9),
            len: 1,
        });
        test_encoding(Pop {
            dest: Register(5),
            len: 1,
        });
        test_encoding(Call {
            func: Register(10),
            args: 3,
            returns: 2,
        });
    }
}
*/
