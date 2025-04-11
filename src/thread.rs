use std::ops::ControlFlow;

use gc_arena::{Collect, Mutation};
use thiserror::Error;

use crate::{
    bytecode::{self, ByteCode},
    closure::{Closure, Constant, HeapVar},
    error::Error,
    instructions::{ConstIdx, HeapIdx, RegIdx},
    object::Object,
    stack::Stack,
    value::{Function, Value},
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
    heap: Vec<HeapVar<'gc>>,
}

impl<'gc> Thread<'gc> {
    pub fn exec(
        &mut self,
        mc: &Mutation<'gc>,
        closure: Closure<'gc>,
    ) -> Result<Vec<Value<'gc>>, Error> {
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
    ) -> Result<(), Error> {
        let proto = closure.prototype();

        assert!(proto.used_registers <= 256);

        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);

        let heap_bottom = self.heap.len();
        self.heap.resize_with(heap_bottom + proto.used_heap, || {
            HeapVar::new(mc, Value::Undefined)
        });

        let mut pc: usize = 0;

        loop {
            // We pass in a 256 slice of registers to avoid register bounds checks.
            match dispatch(
                mc,
                &proto.bytecode,
                &proto.constants,
                &mut pc,
                (&mut self.registers[register_bottom..register_bottom + 256])
                    .try_into()
                    .unwrap(),
                &mut self.heap[heap_bottom..],
                Stack::new(&mut self.stack, stack_bottom),
                closure.this(),
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

                    // We only preserve the registers that are intended to be used,
                    self.stack.truncate(register_bottom + proto.used_registers);
                    self.call(mc, closure, arg_bottom)?;
                    // Resize the register slice to be 256 wide.
                    self.stack.resize(register_bottom + 256, Value::Undefined);

                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .resize(arg_bottom + returns as usize, Value::Undefined);
                }
                Next::Return { returns } => {
                    // Pad the stack with the requested number of returns.
                    self.stack
                        .resize(stack_bottom + returns as usize, Value::Undefined);

                    // Clear the registers and heap vars for this frame.
                    self.registers.truncate(register_bottom);
                    self.heap.truncate(heap_bottom);

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
    constants: &[Constant<'gc>],
    pc: &mut usize,
    registers: &mut [Value<'gc>; 256],
    heap: &mut [HeapVar<'gc>],
    mut stack: Stack<'gc, '_>,
    this: Object<'gc>,
) -> Result<Next<'gc>, Error> {
    struct Dispatch<'gc, 'a> {
        mc: &'a Mutation<'gc>,
        constants: &'a [Constant<'gc>],
        registers: &'a mut [Value<'gc>],
        heap: &'a mut [HeapVar<'gc>],
        stack: Stack<'gc, 'a>,
        this: Object<'gc>,
    }

    impl<'gc, 'a> bytecode::Dispatch for Dispatch<'gc, 'a> {
        type Break = Next<'gc>;
        type Error = Error;

        #[inline]
        fn undefined(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Value::Undefined;
            Ok(())
        }

        #[inline]
        fn load_constant(&mut self, dest: RegIdx, constant: ConstIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.constants[constant as usize].to_value();
            Ok(())
        }

        #[inline]
        fn get_heap(&mut self, dest: RegIdx, heap: HeapIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.heap[heap as usize].get();
            Ok(())
        }

        #[inline]
        fn set_heap(&mut self, heap: HeapIdx, source: RegIdx) -> Result<(), Self::Error> {
            self.heap[heap as usize].set(self.mc, self.registers[source as usize]);
            Ok(())
        }

        #[inline]
        fn get_this(&mut self, dest: RegIdx, key: RegIdx) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadOp.into());
            };
            self.registers[dest as usize] = self.this.get(key).unwrap_or_default();
            Ok(())
        }

        #[inline]
        fn set_this(&mut self, key: RegIdx, value: RegIdx) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadOp.into());
            };
            self.this.set(self.mc, key, self.registers[value as usize]);
            Ok(())
        }

        #[inline]
        fn move_(&mut self, dest: RegIdx, source: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.registers[source as usize];
            Ok(())
        }

        #[inline]
        fn not(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Value::Boolean(self.registers[arg as usize].to_bool());
            Ok(())
        }

        #[inline]
        fn add(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.add(arg2).ok_or(VmError::BadOp)?;
            Ok(())
        }

        #[inline]
        fn sub(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.sub(arg2).ok_or(VmError::BadOp)?;
            Ok(())
        }

        #[inline]
        fn test_equal(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = Value::Boolean(arg1.equal(arg2).ok_or(VmError::BadOp)?);
            Ok(())
        }

        #[inline]
        fn test_not_equal(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = Value::Boolean(!arg1.equal(arg2).ok_or(VmError::BadOp)?);
            Ok(())
        }

        #[inline]
        fn test_less(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Value::Boolean(
                self.registers[arg1 as usize]
                    .less_than(self.registers[arg2 as usize])
                    .ok_or(VmError::BadOp)?,
            );
            Ok(())
        }

        #[inline]
        fn test_less_equal(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Value::Boolean(
                self.registers[arg1 as usize]
                    .less_equal(self.registers[arg2 as usize])
                    .ok_or(VmError::BadOp)?,
            );
            Ok(())
        }

        #[inline]
        fn check(&mut self, test: RegIdx, is_true: bool) -> Result<bool, Self::Error> {
            Ok(self.registers[test as usize].to_bool() == is_true)
        }

        #[inline]
        fn push(&mut self, source: RegIdx, len: u8) -> Result<(), Self::Error> {
            for i in 0..len {
                self.stack
                    .push_back(self.registers[source as usize + i as usize]);
            }
            Ok(())
        }

        #[inline]
        fn pop(&mut self, dest: RegIdx, len: u8) -> Result<(), Self::Error> {
            for i in (0..len).rev() {
                self.registers[dest as usize + i as usize] =
                    self.stack.pop_back().ok_or(VmError::StackUnderflow)?;
            }
            Ok(())
        }

        #[inline]
        fn call(
            &mut self,
            func: RegIdx,
            args: u8,
            returns: u8,
        ) -> Result<ControlFlow<Self::Break>, Self::Error> {
            let func = self.registers[func as usize]
                .to_function()
                .ok_or(VmError::BadCall)?;

            Ok(match func {
                Function::Closure(closure) => ControlFlow::Break(Next::Call {
                    closure,
                    args,
                    returns,
                }),
                Function::Callback(callback) => {
                    callback.call(self.mc, self.stack.reborrow())?;
                    ControlFlow::Continue(())
                }
            })
        }

        #[inline]
        fn return_(&mut self, returns: u8) -> Result<Self::Break, Self::Error> {
            Ok(Next::Return { returns })
        }
    }

    let mut dispatcher = bytecode::Dispatcher::new(bytecode, *pc);
    let ret = dispatcher.dispatch_loop(&mut Dispatch {
        mc,
        constants,
        registers,
        heap,
        stack: stack.reborrow(),
        this,
    });
    *pc = dispatcher.pc();
    ret
}
