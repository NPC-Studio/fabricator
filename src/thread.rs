use std::ops::ControlFlow;

use gc_arena::{Collect, Mutation};
use thiserror::Error;

use crate::{
    bytecode::{self, ByteCode},
    closure::Closure,
    constant::Constant,
    instructions::{ConstIdx, RegIdx},
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
}

impl<'gc> Thread<'gc> {
    pub fn exec(
        &mut self,
        mc: &Mutation<'gc>,
        closure: Closure<'gc>,
    ) -> Result<Vec<Value<'gc>>, VmError> {
        self.registers.clear();
        self.stack.clear();

        self.call(mc, closure, 0)?;

        Ok(self.stack.drain(..).collect())
    }

    fn call(
        &mut self,
        mc: &Mutation<'gc>,
        closure: Closure<'gc>,
        stack_bottom: usize,
    ) -> Result<(), VmError> {
        let proto = &closure.0;

        let register_bottom = self.registers.len();
        let register_len = proto.max_register as usize + 1;
        self.registers
            .resize(register_bottom + register_len, Value::Undefined);

        let mut pc: usize = 0;

        loop {
            match dispatch(
                mc,
                &proto.bytecode,
                &proto.constants,
                &mut pc,
                (&mut self.registers[register_bottom..register_bottom + register_len])
                    .try_into()
                    .unwrap(),
                Stack::new(&mut self.stack, stack_bottom),
            )? {
                Next::Call {
                    closure,
                    args,
                    returns,
                } => {
                    let arg_bottom = (self.stack.len() - args as usize).max(stack_bottom);

                    // Pad stack with undefined values to match the requested args len.
                    self.stack
                        .resize(arg_bottom + args as usize, Value::Undefined);

                    self.call(mc, closure, arg_bottom)?;

                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .resize(arg_bottom + returns as usize, Value::Undefined);
                }
                Next::Return { returns } => {
                    // Pad the stack with the requested number of returns.
                    self.stack
                        .resize(stack_bottom + returns as usize, Value::Undefined);

                    // Clear the registers for this frame.
                    self.registers.truncate(register_bottom);

                    return Ok(());
                }
            }
        }
    }
}

enum Next<'gc> {
    Call {
        closure: Closure<'gc>,
        args: u8,
        returns: u8,
    },
    Return {
        returns: u8,
    },
}

fn dispatch<'gc>(
    mc: &Mutation<'gc>,
    bytecode: &ByteCode,
    constants: &[Constant<String<'gc>>],
    pc: &mut usize,
    registers: &mut [Value<'gc>],
    mut stack: Stack<'gc, '_>,
) -> Result<Next<'gc>, VmError> {
    struct Dispatch<'gc, 'a> {
        mc: &'a Mutation<'gc>,
        constants: &'a [Constant<String<'gc>>],
        registers: &'a mut [Value<'gc>],
        stack: Stack<'gc, 'a>,
    }

    impl<'gc, 'a> bytecode::Dispatch for Dispatch<'gc, 'a> {
        type Return = Result<Next<'gc>, VmError>;

        fn move_(self, source: RegIdx, dest: RegIdx) -> ControlFlow<Self::Return> {
            self.registers[dest as usize] = self.registers[source as usize];
            ControlFlow::Continue(())
        }

        fn test_less(self, arg1: RegIdx, arg2: RegIdx) -> ControlFlow<Self::Return, bool> {
            ControlFlow::Continue(
                self.registers[arg1 as usize].less_than(self.registers[arg2 as usize]),
            )
        }

        fn test_less_equal(self, arg1: RegIdx, arg2: RegIdx) -> ControlFlow<Self::Return, bool> {
            ControlFlow::Continue(
                self.registers[arg1 as usize].less_equal(self.registers[arg2 as usize]),
            )
        }

        fn add(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> ControlFlow<Self::Return> {
            match self.registers[arg1 as usize].add(self.registers[arg2 as usize]) {
                Some(v) => {
                    self.registers[dest as usize] = v;
                    ControlFlow::Continue(())
                }
                None => ControlFlow::Break(Err(VmError::BadOp)),
            }
        }

        fn sub(self, arg1: RegIdx, arg2: RegIdx, dest: RegIdx) -> ControlFlow<Self::Return> {
            match self.registers[arg1 as usize].sub(self.registers[arg2 as usize]) {
                Some(v) => {
                    self.registers[dest as usize] = v;
                    ControlFlow::Continue(())
                }
                None => ControlFlow::Break(Err(VmError::BadOp)),
            }
        }

        fn load(self, constant: ConstIdx, dest: RegIdx) -> ControlFlow<Self::Return> {
            self.registers[dest as usize] = self.constants[constant as usize].into();
            ControlFlow::Continue(())
        }

        fn push(mut self, source: RegIdx, len: u8) -> ControlFlow<Self::Return> {
            for i in 0..len {
                self.stack
                    .push_back(self.registers[source as usize + i as usize]);
            }
            ControlFlow::Continue(())
        }

        fn pop(mut self, dest: RegIdx, len: u8) -> ControlFlow<Self::Return> {
            for i in (0..len).rev() {
                match self.stack.pop_back() {
                    Some(v) => {
                        self.registers[dest as usize + i as usize] = v;
                    }
                    None => {
                        return ControlFlow::Break(Err(VmError::StackUnderflow));
                    }
                }
            }
            ControlFlow::Continue(())
        }

        fn call(mut self, func: RegIdx, args: u8, returns: u8) -> ControlFlow<Self::Return> {
            match self.registers[func as usize].to_function() {
                Some(Function::Closure(closure)) => ControlFlow::Break(Ok(Next::Call {
                    closure,
                    args,
                    returns,
                })),
                Some(Function::Callback(callback)) => {
                    callback.call(self.mc, self.stack.reborrow());
                    ControlFlow::Continue(())
                }
                None => ControlFlow::Break(Err(VmError::BadCall)),
            }
        }

        fn return_(self, returns: u8) -> Self::Return {
            Ok(Next::Return { returns })
        }
    }

    let mut dispatcher = bytecode::Dispatcher::new(bytecode, *pc);

    loop {
        if let ControlFlow::Break(ret) = dispatcher.dispatch(Dispatch {
            mc,
            constants,
            registers,
            stack: stack.reborrow(),
        }) {
            return ret;
        }
    }
}
