use std::ops::ControlFlow;

use gc_arena::{Collect, Gc, Lock, Mutation, RefLock};
use thiserror::Error;

use crate::{
    array::Array,
    bytecode,
    closure::{Closure, Constant, HeapVar, HeapVarDescriptor},
    error::Error,
    instructions::{ConstIdx, HeapIdx, MagicIdx, ParamIdx, ProtoIdx, RegIdx},
    interpreter::Context,
    object::Object,
    stack::Stack,
    string::String,
    value::{Function, Value},
};

#[derive(Debug, Error)]
pub enum VmError {
    #[error("bad op")]
    BadOp,
    #[error("bad object")]
    BadObject,
    #[error("bad key")]
    BadKey,
    #[error("bad array")]
    BadArray,
    #[error("bad index")]
    BadIndex,
    #[error("no such field")]
    NoSuchField,
    #[error("bad call")]
    BadCall,
    #[error("bad closure index")]
    BadClosureIdx,
    #[error("bad magic index")]
    BadMagicIdx,
    #[error("stack underflow")]
    StackUnderflow,
    #[error("only owned heap values can be reset")]
    ResetHeapNotOwned,
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(Gc<'gc, ThreadInner<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
pub struct ThreadState<'gc> {
    registers: Vec<Value<'gc>>,
    stack: Vec<Value<'gc>>,
    heap: Vec<OwnedHeapVar<'gc>>,
}

pub type ThreadInner<'gc> = RefLock<ThreadState<'gc>>;

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                registers: Vec::new(),
                stack: Vec::new(),
                heap: Vec::new(),
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
        self.exec_with(ctx, closure, ctx.globals().into())
    }

    pub fn exec_with(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Value<'gc>,
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
        this: Value<'gc>,
        stack_bottom: usize,
    ) -> Result<(), Error> {
        let proto = closure.prototype();
        assert!(proto.used_registers <= 256);

        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);

        let params_top = self.stack.len();

        let heap_bottom = self.heap.len();
        for hd in closure.heap() {
            if let &HeapVar::Owned(idx) = hd {
                let idx = heap_bottom + idx as usize;
                self.heap
                    .resize_with(idx + 1, || OwnedHeapVar::unique(Value::Undefined));
            }
        }

        let mut pc: usize = 0;

        loop {
            // We pass in a 256 slice of registers to avoid register bounds checks.
            match dispatch(
                ctx,
                closure,
                if closure.this().is_undefined() {
                    this
                } else {
                    closure.this()
                },
                &mut pc,
                (&mut self.registers[register_bottom..register_bottom + 256])
                    .try_into()
                    .unwrap(),
                &mut self.heap[heap_bottom..],
                Stack::new(&mut self.stack, stack_bottom),
                params_top - stack_bottom,
            )? {
                Next::Call {
                    closure,
                    this,
                    returns,
                } => {
                    // We only preserve the registers that the prototype claims to use,
                    self.stack.truncate(register_bottom + proto.used_registers);
                    self.call(ctx, closure, this, params_top)?;
                    // Resize the register slice to be 256 wide.
                    self.stack.resize(register_bottom + 256, Value::Undefined);
                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .resize(params_top + returns as usize, Value::Undefined);
                }
                Next::Return => {
                    // Clear the registers for this frame.
                    self.registers.truncate(register_bottom);
                    // Drain all of the parameters, everything left on the stack for this frame is
                    // a return.
                    self.stack.drain(stack_bottom..params_top);

                    return Ok(());
                }
            }
        }
    }
}

enum Next<'gc> {
    Call {
        closure: Closure<'gc>,
        this: Value<'gc>,
        returns: u8,
    },
    Return,
}

type SharedHeap<'gc> = Gc<'gc, Lock<Value<'gc>>>;

#[derive(Debug, Collect)]
#[collect(no_drop)]
enum OwnedHeapVar<'gc> {
    // We lie here, if a "heap" variable is only uniquely referenced by the closure that owns it, we
    // don't bother to actually allocate it on the heap.
    //
    // Once a closure is created that must share this value, it will be moved to the heap as a
    // `OwnedHeapVar::Shared` value so that it can be shared across closures.
    Unique(Value<'gc>),
    Shared(SharedHeap<'gc>),
}

impl<'gc> OwnedHeapVar<'gc> {
    fn unique(value: Value<'gc>) -> Self {
        Self::Unique(value)
    }

    fn get(&self) -> Value<'gc> {
        match self {
            OwnedHeapVar::Unique(v) => *v,
            OwnedHeapVar::Shared(v) => v.get(),
        }
    }

    fn set(&mut self, mc: &Mutation<'gc>, value: Value<'gc>) {
        match self {
            OwnedHeapVar::Unique(v) => *v = value,
            OwnedHeapVar::Shared(v) => v.set(mc, value),
        }
    }

    fn make_shared(&mut self, mc: &Mutation<'gc>) -> SharedHeap<'gc> {
        match *self {
            OwnedHeapVar::Unique(v) => {
                let gc = Gc::new(mc, Lock::new(v));
                *self = OwnedHeapVar::Shared(gc);
                gc
            }
            OwnedHeapVar::Shared(v) => v,
        }
    }
}

fn dispatch<'gc>(
    ctx: Context<'gc>,
    closure: Closure<'gc>,
    this: Value<'gc>,
    pc: &mut usize,
    registers: &mut [Value<'gc>; 256],
    heap: &mut [OwnedHeapVar<'gc>],
    stack: Stack<'gc, '_>,
    num_params: usize,
) -> Result<Next<'gc>, Error> {
    struct Dispatch<'gc, 'a> {
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Value<'gc>,
        registers: &'a mut [Value<'gc>],
        heap: &'a mut [OwnedHeapVar<'gc>],
        stack: Stack<'gc, 'a>,
        num_params: usize,
    }

    impl<'gc, 'a> Dispatch<'gc, 'a> {
        #[inline]
        fn do_call(
            &mut self,
            this: Value<'gc>,
            func: Function<'gc>,
            returns: u8,
        ) -> Result<ControlFlow<Next<'gc>>, Error> {
            Ok(match func {
                Function::Closure(closure) => ControlFlow::Break(Next::Call {
                    closure,
                    this,
                    returns,
                }),
                Function::Callback(callback) => {
                    callback.call_with(self.ctx, this, self.stack.sub_stack(self.num_params))?;

                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .sub_stack(self.num_params)
                        .resize(returns as usize);

                    ControlFlow::Continue(())
                }
            })
        }

        #[inline]
        fn do_get_field(&mut self, obj: Value<'gc>, key: String<'gc>) -> Result<Value<'gc>, Error> {
            match obj {
                Value::Object(object) => object.get(key).ok_or(VmError::NoSuchField.into()),
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.get_field(self.ctx, user_data, key)
                    } else {
                        Err(VmError::BadObject.into())
                    }
                }
                _ => Err(VmError::BadObject.into()),
            }
        }

        #[inline]
        fn do_set_field(
            &mut self,
            obj: Value<'gc>,
            key: String<'gc>,
            value: Value<'gc>,
        ) -> Result<(), Error> {
            match obj {
                Value::Object(object) => {
                    object.set(&self.ctx, key, value);
                }
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.set_field(self.ctx, user_data, key, value)?;
                    } else {
                        return Err(VmError::BadObject.into());
                    }
                }
                _ => return Err(VmError::BadObject.into()),
            }

            Ok(())
        }

        #[inline]
        fn do_get_index(
            &mut self,
            array: Value<'gc>,
            index: Value<'gc>,
        ) -> Result<Value<'gc>, Error> {
            match array {
                Value::Array(array) => {
                    let index = index
                        .to_integer()
                        .and_then(|i| i.try_into().ok())
                        .ok_or(VmError::BadIndex)?;
                    Ok(array.get(index))
                }
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.get_index(self.ctx, user_data, index)
                    } else {
                        Err(VmError::BadArray.into())
                    }
                }
                _ => Err(VmError::BadArray.into()),
            }
        }

        #[inline]
        fn do_set_index(
            &mut self,
            array: Value<'gc>,
            index: Value<'gc>,
            value: Value<'gc>,
        ) -> Result<(), Error> {
            match array {
                Value::Array(array) => {
                    let index = index
                        .to_integer()
                        .and_then(|i| i.try_into().ok())
                        .ok_or(VmError::BadIndex)?;
                    array.set(&self.ctx, index, value);
                    Ok(())
                }
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.set_index(self.ctx, user_data, index, value)
                    } else {
                        Err(VmError::BadObject.into())
                    }
                }
                _ => Err(VmError::BadArray.into()),
            }
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
                .ok_or(VmError::BadClosureIdx)?;

            let mut heap = Vec::new();
            for &hd in &proto.heap_vars {
                match hd {
                    HeapVarDescriptor::Owned(idx) => {
                        heap.push(HeapVar::Owned(idx));
                    }
                    HeapVarDescriptor::UpValue(idx) => {
                        heap.push(HeapVar::UpValue(match self.closure.heap()[idx as usize] {
                            HeapVar::Owned(idx) => self.heap[idx as usize].make_shared(&self.ctx),
                            HeapVar::UpValue(v) => v,
                        }));
                    }
                }
            }

            // inner closures inherit the set of magic values from the parent, and inherit the
            // current `this` value.
            self.registers[dest as usize] = Closure::from_parts(
                &self.ctx,
                proto,
                self.closure.magic(),
                self.this,
                Gc::new(&self.ctx, heap.into_boxed_slice()),
            )?
            .into();
            Ok(())
        }

        #[inline]
        fn get_heap(&mut self, dest: RegIdx, heap: HeapIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = match self.closure.heap()[heap as usize] {
                HeapVar::Owned(idx) => self.heap[idx as usize].get(),
                HeapVar::UpValue(v) => v.get(),
            };
            Ok(())
        }

        #[inline]
        fn set_heap(&mut self, heap: HeapIdx, source: RegIdx) -> Result<(), Self::Error> {
            let source = self.registers[source as usize];
            match self.closure.heap()[heap as usize] {
                HeapVar::Owned(idx) => self.heap[idx as usize].set(&self.ctx, source),
                HeapVar::UpValue(v) => v.set(&self.ctx, source),
            };
            Ok(())
        }

        #[inline]
        fn reset_heap(&mut self, heap: HeapIdx) -> Result<(), Self::Error> {
            match self.closure.heap()[heap as usize] {
                HeapVar::Owned(idx) => {
                    self.heap[idx as usize] = OwnedHeapVar::unique(Value::Undefined);
                    Ok(())
                }
                HeapVar::UpValue(_) => Err(VmError::ResetHeapNotOwned.into()),
            }
        }

        #[inline]
        fn global(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.ctx.globals().into();
            Ok(())
        }

        #[inline]
        fn this(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.this;
            Ok(())
        }

        #[inline]
        fn new_object(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Object::new(&self.ctx).into();
            Ok(())
        }

        #[inline]
        fn new_array(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Array::new(&self.ctx).into();
            Ok(())
        }

        #[inline]
        fn param(&mut self, dest: RegIdx, index: ParamIdx) -> Result<(), Self::Error> {
            if (index as usize) < self.num_params {
                self.registers[dest as usize] = self.stack.get(index as usize);
            } else {
                self.registers[dest as usize] = Value::Undefined;
            }
            Ok(())
        }

        #[inline]
        fn get_field(
            &mut self,
            dest: RegIdx,
            object: RegIdx,
            key: RegIdx,
        ) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };

            self.registers[dest as usize] =
                self.do_get_field(self.registers[object as usize], key)?;
            Ok(())
        }

        #[inline]
        fn set_field(
            &mut self,
            object: RegIdx,
            key: RegIdx,
            value: RegIdx,
        ) -> Result<(), Self::Error> {
            let Value::String(key) = self.registers[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.do_set_field(
                self.registers[object as usize],
                key,
                self.registers[value as usize],
            )
        }

        #[inline]
        fn get_field_const(
            &mut self,
            dest: RegIdx,
            object: RegIdx,
            key: ConstIdx,
        ) -> Result<(), Self::Error> {
            let Constant::String(key) = self.closure.prototype().constants[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.registers[dest as usize] =
                self.do_get_field(self.registers[object as usize], key)?;
            Ok(())
        }

        #[inline]
        fn set_field_const(
            &mut self,
            object: RegIdx,
            key: ConstIdx,
            value: RegIdx,
        ) -> Result<(), Self::Error> {
            let Constant::String(key) = self.closure.prototype().constants[key as usize] else {
                return Err(VmError::BadKey.into());
            };
            self.do_set_field(
                self.registers[object as usize],
                key,
                self.registers[value as usize],
            )
        }

        #[inline]
        fn get_index(
            &mut self,
            dest: RegIdx,
            array: RegIdx,
            index: RegIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.do_get_index(
                self.registers[array as usize],
                self.registers[index as usize],
            )?;
            Ok(())
        }

        #[inline]
        fn set_index(
            &mut self,
            array: RegIdx,
            index: RegIdx,
            value: RegIdx,
        ) -> Result<(), Self::Error> {
            self.do_set_index(
                self.registers[array as usize],
                self.registers[index as usize],
                self.registers[value as usize],
            )?;
            Ok(())
        }

        #[inline]
        fn get_index_const(
            &mut self,
            dest: RegIdx,
            array: RegIdx,
            index: ConstIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.do_get_index(
                self.registers[array as usize],
                self.closure.prototype().constants[index as usize].to_value(),
            )?;
            Ok(())
        }

        #[inline]
        fn set_index_const(
            &mut self,
            array: RegIdx,
            index: ConstIdx,
            value: RegIdx,
        ) -> Result<(), Self::Error> {
            self.do_set_index(
                self.registers[array as usize],
                self.closure.prototype().constants[index as usize].to_value(),
                self.registers[value as usize],
            )?;
            Ok(())
        }

        #[inline]
        fn move_(&mut self, dest: RegIdx, source: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.registers[source as usize];
            Ok(())
        }

        #[inline]
        fn not(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = (!self.registers[arg as usize].to_bool()).into();
            Ok(())
        }

        #[inline]
        fn neg(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.registers[arg as usize]
                .negate()
                .ok_or(VmError::BadOp)?;
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

        fn mult(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.mult(arg2).ok_or(VmError::BadOp)?;
            Ok(())
        }

        fn div(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.div(arg2).ok_or(VmError::BadOp)?;
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
            *dest = arg1.equal(arg2).into();
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
            *dest = (!arg1.equal(arg2)).into();
            Ok(())
        }

        #[inline]
        fn test_less(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.registers[arg1 as usize]
                .less_than(self.registers[arg2 as usize])
                .ok_or(VmError::BadOp)?
                .into();
            Ok(())
        }

        #[inline]
        fn test_less_equal(
            &mut self,
            dest: RegIdx,
            arg1: RegIdx,
            arg2: RegIdx,
        ) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.registers[arg1 as usize]
                .less_equal(self.registers[arg2 as usize])
                .ok_or(VmError::BadOp)?
                .into();
            Ok(())
        }

        fn and(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = (arg1.to_bool() && arg2.to_bool()).into();
            Ok(())
        }

        fn or(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = (arg1.to_bool() || arg2.to_bool()).into();
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
                    .sub_stack(self.num_params)
                    .push_back(self.registers[source as usize + i as usize]);
            }
            Ok(())
        }

        #[inline]
        fn pop(&mut self, dest: RegIdx, len: u8) -> Result<(), Self::Error> {
            for i in (0..len).rev() {
                self.registers[dest as usize + i as usize] = self
                    .stack
                    .sub_stack(self.num_params)
                    .pop_back()
                    .ok_or(VmError::StackUnderflow)?;
            }
            Ok(())
        }

        #[inline]
        fn get_magic(&mut self, dest: RegIdx, magic: MagicIdx) -> Result<(), Self::Error> {
            let magic = self
                .closure
                .magic()
                .get(magic as usize)
                .ok_or(VmError::BadMagicIdx)?;
            self.registers[dest as usize] = magic.get(self.ctx)?;
            Ok(())
        }

        #[inline]
        fn set_magic(&mut self, magic: MagicIdx, source: RegIdx) -> Result<(), Self::Error> {
            let magic = self
                .closure
                .magic()
                .get(magic as usize)
                .ok_or(VmError::BadMagicIdx)?;
            magic.set(self.ctx, self.registers[source as usize])?;
            Ok(())
        }

        #[inline]
        fn call(
            &mut self,
            func: RegIdx,
            returns: u8,
        ) -> Result<ControlFlow<Self::Break>, Self::Error> {
            let func = self.registers[func as usize]
                .to_function()
                .ok_or(VmError::BadCall)?;

            self.do_call(self.this, func, returns)
        }

        #[inline]
        fn method(
            &mut self,
            this: RegIdx,
            func: RegIdx,
            returns: u8,
        ) -> Result<ControlFlow<Self::Break>, Self::Error> {
            let this = self.registers[this as usize];
            let func = self.registers[func as usize]
                .to_function()
                .ok_or(VmError::BadCall)?;

            self.do_call(this, func, returns)
        }

        #[inline]
        fn return_(&mut self) -> Result<Self::Break, Self::Error> {
            Ok(Next::Return)
        }
    }

    let mut dispatcher = bytecode::Dispatcher::new(&closure.prototype().as_ref().bytecode, *pc);
    let ret = dispatcher.dispatch_loop(&mut Dispatch {
        ctx,
        closure,
        this,
        registers,
        heap,
        stack,
        num_params,
    });
    *pc = dispatcher.pc();
    ret
}
