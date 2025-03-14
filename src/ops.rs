use gc_arena::Collect;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct Register(pub u8);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct Constant(pub u16);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(u8)]
pub enum OpCode {
    Move,
    Jump,
    IsLess,
    IsLessEqual,
    Add,
    Sub,
    Load,
    Push,
    Pop,
    Call,
    Return,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct OpArgs([u8; 3]);

impl OpArgs {
    #[inline]
    fn encode2(arg1: u16, arg2: u8) -> Self {
        let arg1_bytes = u16::to_le_bytes(arg1);
        Self([arg1_bytes[0], arg1_bytes[1], arg2])
    }

    #[inline]
    fn decode2(self) -> (u16, u8) {
        (u16::from_le_bytes([self.0[0], self.0[1]]), self.0[2])
    }

    #[inline]
    fn encode3(arg1: u8, arg2: u8, arg3: u8) -> Self {
        Self([arg1, arg2, arg3])
    }

    #[inline]
    fn decode3(self) -> (u8, u8, u8) {
        (self.0[0], self.0[1], self.0[2])
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct Instruction {
    opcode: OpCode,
    args: OpArgs,
}

impl Instruction {
    #[inline]
    pub fn new(opcode: OpCode, args: OpArgs) -> Self {
        Self { opcode, args }
    }

    #[inline]
    pub fn opcode(self) -> OpCode {
        self.opcode
    }

    #[inline]
    pub fn args(self) -> OpArgs {
        self.args
    }

    pub fn encode<O: Operation>(op: O) -> Self {
        Instruction {
            opcode: O::OPCODE,
            args: op.encode(),
        }
    }
}

pub trait Operation {
    const OPCODE: OpCode;

    fn encode(self) -> OpArgs;
    fn decode(args: OpArgs) -> Self;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Move {
    pub source: Register,
    pub dest: Register,
}

impl Operation for Move {
    const OPCODE: OpCode = OpCode::Move;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.source.0, self.dest.0, 0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (source, dest, _) = args.decode3();
        Self {
            source: Register(source),
            dest: Register(dest),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Jump {
    pub offset: i16,
}

impl Operation for Jump {
    const OPCODE: OpCode = OpCode::Jump;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode2(self.offset as u16, 0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (offset, _) = args.decode2();
        Self {
            offset: offset as i16,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IsLess {
    pub skip_if: bool,
    pub arg1: Register,
    pub arg2: Register,
}

impl Operation for IsLess {
    const OPCODE: OpCode = OpCode::IsLess;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.skip_if as u8, self.arg1.0, self.arg2.0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (skip_if, arg1, arg2) = args.decode3();
        Self {
            skip_if: skip_if != 0,
            arg1: Register(arg1),
            arg2: Register(arg2),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IsLessEqual {
    pub skip_if: bool,
    pub arg1: Register,
    pub arg2: Register,
}

impl Operation for IsLessEqual {
    const OPCODE: OpCode = OpCode::IsLessEqual;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.skip_if as u8, self.arg1.0, self.arg2.0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (skip_if, arg1, arg2) = args.decode3();
        Self {
            skip_if: skip_if != 0,
            arg1: Register(arg1),
            arg2: Register(arg2),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Add {
    pub arg1: Register,
    pub arg2: Register,
    pub dest: Register,
}

impl Operation for Add {
    const OPCODE: OpCode = OpCode::Add;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.arg1.0, self.arg2.0, self.dest.0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (arg1, arg2, dest) = args.decode3();
        Self {
            arg1: Register(arg1),
            arg2: Register(arg2),
            dest: Register(dest),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Sub {
    pub arg1: Register,
    pub arg2: Register,
    pub dest: Register,
}

impl Operation for Sub {
    const OPCODE: OpCode = OpCode::Sub;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.arg1.0, self.arg2.0, self.dest.0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (arg1, arg2, dest) = args.decode3();
        Self {
            arg1: Register(arg1),
            arg2: Register(arg2),
            dest: Register(dest),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Load {
    pub constant: Constant,
    pub dest: Register,
}

impl Operation for Load {
    const OPCODE: OpCode = OpCode::Load;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode2(self.constant.0, self.dest.0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (constant, dest) = args.decode2();
        Self {
            constant: Constant(constant),
            dest: Register(dest),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Push {
    pub source: Register,
    pub len: u8,
}

impl Operation for Push {
    const OPCODE: OpCode = OpCode::Push;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.source.0, self.len, 0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (start, len, _) = args.decode3();
        Self {
            source: Register(start),
            len,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Pop {
    pub dest: Register,
    pub len: u8,
}

impl Operation for Pop {
    const OPCODE: OpCode = OpCode::Pop;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.dest.0, self.len, 0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (dest, len, _) = args.decode3();
        Self {
            dest: Register(dest),
            len,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Call {
    pub func: Register,
    pub args: u8,
    pub returns: u8,
}

impl Operation for Call {
    const OPCODE: OpCode = OpCode::Call;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.func.0, self.args, self.returns)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (func, args, returns) = args.decode3();
        Self {
            func: Register(func),
            args,
            returns,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Return {
    pub returns: u8,
}

impl Operation for Return {
    const OPCODE: OpCode = OpCode::Return;

    #[inline]
    fn encode(self) -> OpArgs {
        OpArgs::encode3(self.returns, 0, 0)
    }

    #[inline]
    fn decode(args: OpArgs) -> Self {
        let (returns, _, _) = args.decode3();
        Self { returns }
    }
}

#[cfg(test)]
mod tests {
    pub use super::*;

    #[test]
    fn ensure_inst_size() {
        assert_eq!(std::mem::size_of::<Instruction>(), 4);
        assert!(std::mem::align_of::<Instruction>() <= 4);
    }

    #[test]
    fn test_arg_encoding() {
        assert_eq!(OpArgs::encode2(0xfab7, 0xaa).decode2(), (0xfab7, 0xaa));
        assert_eq!(
            OpArgs::encode3(0xff, 0xaa, 0xcc).decode3(),
            (0xff, 0xaa, 0xcc)
        );
    }

    #[test]
    fn test_inst_encoding() {
        fn test_encoding<O: Operation + Eq + std::fmt::Debug + Copy>(op: O) {
            assert_eq!(op, O::decode(op.encode()));
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
