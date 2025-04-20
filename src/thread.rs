use std::ops::ControlFlow;

use gc_arena::{Collect, Gc, Mutation, RefLock};
use thiserror::Error;

use crate::{
    bytecode,
    closure::Closure,
    context::Context,
    error::Error,
    instructions::{ConstIdx, HeapIdx, ProtoIdx, RegIdx},
    object::Object,
    stack::Stack,
    value::{Function, Value},
};

#[derive(Debug, Error)]
pub enum VmError {
    #[error("bad op")]
    BadOp,
    #[error("bad object")]
    BadObject,
    #[error("bad object key")]
    BadKey,
    #[error("bad call")]
    BadCall,
    #[error("bad closure")]
    BadClosure,
    #[error("stack underflow")]
    StackUnderflow,
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(Gc<'gc, ThreadInner<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
pub struct ThreadState<'gc> {
    registers: Vec<Value<'gc>>,
    stack: Vec<Value<'gc>>,
}

pub type ThreadInner<'gc> = RefLock<ThreadState<'gc>>;

impl<'gc> Thread<'gc> {
    pub fn new(ctx: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            &ctx,
            RefLock::new(ThreadState {
                registers: Vec::new(),
                stack: Vec::new(),
            }),
        ))
    }

    #[inline]
    pub fn from_inner(inner: Gc<'gc, ThreadInner<'gc>>) -> Self {
        Self(inner)
    }

    #[inline]
    pub fn into_inner(self) -> Gc<'gc, ThreadInner<'gc>> {
        self.0
    }

    pub fn exec(self, ctx: Context<'gc>, closure: Closure<'gc>) -> Result<Vec<Value<'gc>>, Error> {
        self.exec_with(ctx, closure, ctx.globals())
    }

    pub fn exec_with(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Object<'gc>,
    ) -> Result<Vec<Value<'gc>>, Error> {
        let mut thread = self.0.try_borrow_mut(&ctx).expect("thread locked");
        thread.registers.clear();
        thread.stack.clear();

        thread.call(ctx, closure, this, 0)?;

        Ok(thread.stack.drain(..).collect())
    }
}

impl<'gc> ThreadState<'gc> {
    fn call(
        &mut self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Object<'gc>,
        stack_bottom: usize,
    ) -> Result<(), Error> {
        let proto = closure.prototype();
        assert!(proto.used_registers <= 256);

        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);

        let mut pc: usize = 0;

        loop {
            // We pass in a 256 slice of registers to avoid register bounds checks.
            match dispatch(
                ctx,
                closure,
                closure.this().unwrap_or(this),
                &mut pc,
                (&mut self.registers[register_bottom..register_bottom + 256])
                    .try_into()
                    .unwrap(),
                Stack::new(&mut self.stack, stack_bottom),
            )? {
                Next::Call {
                    closure,
                    this,
                    args,
                    returns,
                } => {
                    let arg_bottom = (self.stack.len() - args as usize).max(stack_bottom);

                    // Pad stack with undefined values to match the requested args len.
                    self.stack
                        .resize(arg_bottom + args as usize, Value::Undefined);

                    // We only preserve the registers that are intended to be used,
                    self.stack.truncate(register_bottom + proto.used_registers);
                    self.call(ctx, closure, this, arg_bottom)?;
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

                    return Ok(());
                }
            }
        }
    }
}

enum Next<'gc> {
    Call {
        closure: Closure<'gc>,
        this: Object<'gc>,
        args: u8,
        returns: u8,
    },
    Return {
        returns: u8,
    },
}

fn dispatch<'gc>(
    ctx: Context<'gc>,
    closure: Closure<'gc>,
    this: Object<'gc>,
    pc: &mut usize,
    registers: &mut [Value<'gc>; 256],
    mut stack: Stack<'gc, '_>,
) -> Result<Next<'gc>, Error> {
    struct Dispatch<'gc, 'a> {
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Object<'gc>,
        registers: &'a mut [Value<'gc>],
        stack: Stack<'gc, 'a>,
    }

    impl<'gc, 'a> Dispatch<'gc, 'a> {
        #[inline]
        fn do_call(
            &mut self,
            this: Object<'gc>,
            func: Function<'gc>,
            args: u8,
            returns: u8,
        ) -> Result<ControlFlow<Next<'gc>>, Error> {
            Ok(match func {
                Function::Closure(closure) => ControlFlow::Break(Next::Call {
                    closure,
                    this,
                    args,
                    returns,
                }),
                Function::Callback(callback) => {
                    let arg_bottom = self.stack.len() - args as usize;

                    // Pad stack with undefined values to match the requested args len.
                    self.stack.resize(arg_bottom + args as usize);

                    callback.call_with(self.ctx, this, self.stack.sub_stack(arg_bottom))?;

                    // Pad stack with undefined values to match the expected return len.
                    self.stack.resize(arg_bottom + returns as usize);

                    ControlFlow::Continue(())
                }
            })
        }
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
            self.registers[dest as usize] =
                self.closure.prototype().constants[constant as usize].to_value();
            Ok(())
        }

        #[inline]
        fn closure(&mut self, dest: RegIdx, proto: ProtoIdx) -> Result<(), Self::Error> {
            let proto = *self
                .closure
                .prototype()
                .prototypes
                .get(proto as usize)
                .ok_or(VmError::BadClosure)?;
            self.registers[dest as usize] = Value::Closure(Closure::with_upvalues(
                &self.ctx,
                proto,
                self.closure.heap(),
            )?);
            Ok(())
        }

        #[inline]
        fn get_heap(&mut self, dest: RegIdx, heap: HeapIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.closure.heap()[heap as usize].get();
            Ok(())
        }

        #[inline]
        fn set_heap(&mut self, heap: HeapIdx, source: RegIdx) -> Result<(), Self::Error> {
            self.closure.heap()[heap as usize].set(&self.ctx, self.registers[source as usize]);
            Ok(())
        }

        #[inline]
        fn get_this(&mut self, dest: RegIdx, key: RegIdx) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.registers[dest as usize] = self.this.get(key).unwrap_or_default();
            Ok(())
        }

        #[inline]
        fn set_this(&mut self, key: RegIdx, value: RegIdx) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.this
                .set(&self.ctx, key, self.registers[value as usize]);
            Ok(())
        }

        fn get_field(
            &mut self,
            dest: RegIdx,
            object: RegIdx,
            key: RegIdx,
        ) -> Result<(), Self::Error> {
            let Value::Object(object) = self.registers[object as usize] else {
                return Err(VmError::BadObject.into());
            };
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.registers[dest as usize] = object.get(key).unwrap_or_default();
            Ok(())
        }

        fn set_field(
            &mut self,
            object: RegIdx,
            key: RegIdx,
            value: RegIdx,
        ) -> Result<(), Self::Error> {
            let Value::Object(object) = self.registers[object as usize] else {
                return Err(VmError::BadObject.into());
            };
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            object.set(&self.ctx, key, self.registers[value as usize]);
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

            self.do_call(self.this, func, args, returns)
        }

        fn method(
            &mut self,
            this: RegIdx,
            func: RegIdx,
            args: u8,
            returns: u8,
        ) -> Result<ControlFlow<Self::Break>, Self::Error> {
            let Value::Object(this) = self.registers[this as usize] else {
                return Err(VmError::BadObject.into());
            };

            let func = self.registers[func as usize]
                .to_function()
                .ok_or(VmError::BadCall)?;

            self.do_call(this, func, args, returns)
        }

        #[inline]
        fn return_(&mut self, returns: u8) -> Result<Self::Break, Self::Error> {
            Ok(Next::Return { returns })
        }
    }

    let mut dispatcher = bytecode::Dispatcher::new(&closure.prototype().as_ref().bytecode, *pc);
    let ret = dispatcher.dispatch_loop(&mut Dispatch {
        ctx,
        closure,
        this,
        registers,
        stack: stack.reborrow(),
    });
    *pc = dispatcher.pc();
    ret
}
