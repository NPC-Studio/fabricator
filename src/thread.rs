use gc_arena::{Collect, Mutation};
use thiserror::Error;

use crate::{
    callback::Callback,
    constant::Constant,
    ops::{self, Instruction, OpCode, Operation as _},
    prototype::Prototype,
    stack::Stack,
    value::{String, Value},
};

#[derive(Debug, Error)]
pub enum VmError {
    #[error("bad op")]
    BadOp,
    #[error("bad call")]
    BadCall,
    #[error("stack underflow")]
    StackUnderflow,
}

#[derive(Default, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc> {
    registers: Vec<Value<'gc>>,
    stack: Vec<Value<'gc>>,
    frames: Vec<Frame>,
}

impl<'gc> Thread<'gc> {
    pub fn exec(
        &mut self,
        mc: &Mutation<'gc>,
        proto: &Prototype<'gc>,
    ) -> Result<Vec<Value<'gc>>, VmError> {
        self.registers.clear();
        self.stack.clear();
        self.frames.clear();

        self.registers.resize(256, Value::Undefined);
        self.frames.push(Frame {
            register_bottom: 0,
            stack_bottom: 0,
            pc: 0,
        });

        let frame = &mut self.frames[0];

        loop {
            match dispatch(
                &proto.instructions,
                &proto.constants,
                &mut frame.pc,
                (&mut self.registers[frame.register_bottom..frame.register_bottom + 256])
                    .try_into()
                    .unwrap(),
                Stack::new(&mut self.stack, frame.stack_bottom),
            )? {
                Next::Call {
                    func,
                    args,
                    returns,
                } => {
                    let arg_offset = self.stack.len() - args as usize;
                    func.call(mc, Stack::new(&mut self.stack, arg_offset));
                    self.stack
                        .resize(arg_offset + returns as usize, Value::Undefined);
                }
                Next::Return { returns } => {
                    self.stack.resize(returns as usize, Value::Undefined);
                    return Ok(self.stack[..].to_vec());
                }
            }
        }
    }
}

#[derive(Collect)]
#[collect(require_static)]
struct Frame {
    register_bottom: usize,
    stack_bottom: usize,
    pc: usize,
}

enum Next<'gc> {
    Call {
        func: Callback<'gc>,
        args: u8,
        returns: u8,
    },
    Return {
        returns: u8,
    },
}

fn dispatch<'gc>(
    ops: &[Instruction],
    constants: &[Constant<String<'gc>>],
    pc: &mut usize,
    registers: &mut [Value<'gc>; 256],
    mut stack: Stack<'gc, '_>,
) -> Result<Next<'gc>, VmError> {
    loop {
        let inst = ops[*pc];
        *pc += 1;

        match inst.opcode() {
            OpCode::Move => {
                let op = ops::Move::decode(inst.args());
                registers[op.dest.0 as usize] = registers[op.source.0 as usize];
            }
            OpCode::Jump => {
                let op = ops::Jump::decode(inst.args());
                *pc = add_offset(*pc, op.offset);
            }
            OpCode::IsLess => {
                let op = ops::IsLess::decode(inst.args());
                if registers[op.arg1.0 as usize].less_than(registers[op.arg2.0 as usize])
                    == op.skip_if
                {
                    *pc += 1;
                }
            }
            OpCode::IsLessEqual => {
                let op = ops::IsLess::decode(inst.args());
                if registers[op.arg1.0 as usize].less_equal(registers[op.arg2.0 as usize])
                    == op.skip_if
                {
                    *pc += 1;
                }
            }
            OpCode::Add => {
                let op = ops::Add::decode(inst.args());
                registers[op.dest.0 as usize] = registers[op.arg1.0 as usize]
                    .add(registers[op.arg2.0 as usize])
                    .ok_or(VmError::BadOp)?;
            }
            OpCode::Sub => {
                let op = ops::Sub::decode(inst.args());
                registers[op.dest.0 as usize] = registers[op.arg1.0 as usize]
                    .sub(registers[op.arg2.0 as usize])
                    .ok_or(VmError::BadOp)?;
            }
            OpCode::Load => {
                let op = ops::Load::decode(inst.args());
                registers[op.dest.0 as usize] = constants[op.constant.0 as usize].into();
            }
            OpCode::Push => {
                let op = ops::Push::decode(inst.args());
                for i in 0..op.len {
                    stack.push_back(registers[op.source.0 as usize + i as usize]);
                }
            }
            OpCode::Pop => {
                let op = ops::Pop::decode(inst.args());
                for i in (0..op.len).rev() {
                    registers[op.dest.0 as usize + i as usize] =
                        stack.pop_back().ok_or(VmError::StackUnderflow)?;
                }
            }
            OpCode::Call => {
                let call = ops::Call::decode(inst.args());
                match registers[call.func.0 as usize] {
                    Value::Callback(callback) => {
                        return Ok(Next::Call {
                            func: callback,
                            args: call.args,
                            returns: call.returns,
                        });
                    }
                    _ => return Err(VmError::BadCall),
                }
            }
            OpCode::Return => {
                let ret = ops::Return::decode(inst.args());
                return Ok(Next::Return {
                    returns: ret.returns,
                });
            }
        }
    }
}

fn add_offset(pc: usize, offset: i16) -> usize {
    ((pc as isize) + offset as isize) as usize
}
