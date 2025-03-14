use gc_arena::{Collect, Mutation};
use thiserror::Error;

use crate::{
    callback::Callback,
    closure::Closure,
    constant::Constant,
    ops::{self, Instruction, OpCode, Operation as _},
    stack::Stack,
    value::{Function, String, Value},
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
        func: Function<'gc>,
    ) -> Result<Vec<Value<'gc>>, VmError> {
        self.registers.clear();
        self.stack.clear();
        self.frames.clear();

        self.call(mc, func, 0)?;

        Ok(self.stack.drain(..).collect())
    }

    fn call(
        &mut self,
        mc: &Mutation<'gc>,
        function: Function<'gc>,
        stack_bottom: usize,
    ) -> Result<(), VmError> {
        match function {
            Function::Closure(closure) => self.call_closure(mc, closure, stack_bottom),
            Function::Callback(callback) => self.call_callback(mc, callback, stack_bottom),
        }
    }

    fn call_closure(
        &mut self,
        mc: &Mutation<'gc>,
        closure: Closure<'gc>,
        stack_bottom: usize,
    ) -> Result<(), VmError> {
        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);
        self.frames.push(Frame {
            register_bottom,
            stack_bottom,
            pc: 0,
        });

        let proto = &closure.0;

        loop {
            let frame = self.frames.last_mut().unwrap();

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
                    // Truncate the registers vec to only the required length to prevent storing all
                    // 256 registers for every function call.
                    self.registers
                        .truncate(frame.register_bottom + proto.max_register as usize + 1);

                    let register_bottom = frame.register_bottom;
                    let arg_bottom = (self.stack.len() - args as usize).max(frame.stack_bottom);

                    // Pad stack with undefined values to match the requested args len.
                    self.stack
                        .resize(arg_bottom + args as usize, Value::Undefined);

                    self.call(mc, func, arg_bottom)?;

                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .resize(arg_bottom + returns as usize, Value::Undefined);

                    // Resize the registers vec back to have a full 256 register bank.
                    self.registers
                        .resize(register_bottom + 256, Value::Undefined);
                }
                Next::Return { returns } => {
                    // Pad the stack with the requested number of returns.
                    self.stack
                        .resize(frame.stack_bottom + returns as usize, Value::Undefined);

                    return Ok(());
                }
            }
        }
    }

    fn call_callback(
        &mut self,
        mc: &Mutation<'gc>,
        callback: Callback<'gc>,
        stack_bottom: usize,
    ) -> Result<(), VmError> {
        callback.call(mc, Stack::new(&mut self.stack, stack_bottom));
        Ok(())
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
        func: Function<'gc>,
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
                return if let Some(func) = registers[call.func.0 as usize].to_function() {
                    Ok(Next::Call {
                        func,
                        args: call.args,
                        returns: call.returns,
                    })
                } else {
                    Err(VmError::BadCall)
                };
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
