use std::{fmt, ops::ControlFlow};

use gc_arena::{Collect, Gc, Lock, Mutation, RefLock};
use thiserror::Error;

use crate::{
    array::Array,
    bytecode,
    closure::{Closure, Constant, HeapVar, HeapVarDescriptor, SharedValue},
    debug::{LineNumber, RefName},
    error::Error,
    instructions::{ArgIdx, ConstIdx, HeapIdx, Instruction, MagicIdx, ProtoIdx, RegIdx},
    interpreter::Context,
    object::Object,
    stack::Stack,
    string::String,
    value::{Function, Value},
};

#[derive(Debug, Copy, Clone, Error)]
pub enum OpError {
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
    #[error("bad heap index")]
    BadHeapIdx,
    #[error("bad closure index")]
    BadClosureIdx,
    #[error("bad magic index")]
    BadMagicIdx,
    #[error("stack underflow")]
    StackUnderflow,
    #[error("only owned heap values can be reset")]
    ResetHeapNotOwned,
}

#[derive(Debug, Error)]
pub struct VmError {
    #[source]
    pub error: Error,
    pub chunk_name: RefName,
    pub instruction_index: usize,
    pub instruction: Instruction,
    pub line_number: LineNumber,
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VM error in {} at line {} instruction {}: {}",
            self.chunk_name, self.line_number, self.instruction_index, self.error,
        )
    }
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
        let globals = ctx.globals().into();
        self.exec_with(ctx, closure, globals, globals)
    }

    pub fn exec_with(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: Value<'gc>,
        other: Value<'gc>,
    ) -> Result<Vec<Value<'gc>>, Error> {
        let mut thread = self.0.try_borrow_mut(&ctx).expect("thread locked");
        thread.registers.clear();
        thread.stack.clear();

        thread.call(ctx, closure, this, other, 0)?;

        Ok(thread.stack.drain(..).collect())
    }
}

impl<'gc> ThreadState<'gc> {
    fn call(
        &mut self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        mut this: Value<'gc>,
        mut other: Value<'gc>,
        stack_bottom: usize,
    ) -> Result<(), Error> {
        let proto = closure.prototype();
        assert!(proto.used_registers <= 256);

        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);

        let num_args = self.stack.len() - stack_bottom;

        let heap_bottom = self.heap.len();
        for hd in closure.heap() {
            if let &HeapVar::Owned(idx) = hd {
                let idx = heap_bottom + idx as usize;
                self.heap
                    .resize_with(idx + 1, || OwnedHeapVar::unique(Value::Undefined));
            }
        }

        let mut pc: usize = 0;

        if !closure.this().is_undefined() {
            other = this;
            this = closure.this();
        }

        loop {
            // We pass in a 256 slice of registers to avoid register bounds checks.
            match dispatch(
                ctx,
                closure,
                &mut this,
                &mut other,
                &mut pc,
                (&mut self.registers[register_bottom..register_bottom + 256])
                    .try_into()
                    .unwrap(),
                &mut self.heap[heap_bottom..],
                Stack::new(&mut self.stack, stack_bottom),
                num_args,
            )? {
                Next::Call {
                    closure,
                    arguments,
                    returns,
                } => {
                    // We only preserve the registers that the prototype claims to use,
                    self.registers
                        .truncate(register_bottom + proto.used_registers);

                    let call_args_bottom = self.stack.len() - arguments as usize;
                    self.call(ctx, closure, this, other, call_args_bottom)?;

                    // Resize the register slice to be 256 wide.
                    self.registers
                        .resize(register_bottom + 256, Value::Undefined);

                    // Pad stack with undefined values to match the expected return len.
                    self.stack
                        .resize(call_args_bottom + returns as usize, Value::Undefined);
                }
                Next::Return { count } => {
                    // Clear the registers for this frame.
                    self.registers.truncate(register_bottom);
                    // Drain everything on the stack up until the returns.
                    self.stack
                        .drain(stack_bottom..self.stack.len() - count as usize);

                    return Ok(());
                }
            }
        }
    }
}

enum Next<'gc> {
    Call {
        closure: Closure<'gc>,
        arguments: ArgIdx,
        returns: ArgIdx,
    },
    Return {
        count: ArgIdx,
    },
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
enum OwnedHeapVar<'gc> {
    // We lie here, if a "heap" variable is only uniquely referenced by the closure that owns it, we
    // don't bother to actually allocate it on the heap.
    //
    // Once a closure is created that must share this value, it will be moved to the heap as a
    // `OwnedHeapVar::Shared` value so that it can be shared across closures.
    Unique(Value<'gc>),
    Shared(SharedValue<'gc>),
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

    fn make_shared(&mut self, mc: &Mutation<'gc>) -> SharedValue<'gc> {
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
    this: &mut Value<'gc>,
    other: &mut Value<'gc>,
    pc: &mut usize,
    registers: &mut [Value<'gc>; 256],
    heap: &mut [OwnedHeapVar<'gc>],
    stack: Stack<'gc, '_>,
    num_args: usize,
) -> Result<Next<'gc>, VmError> {
    struct Dispatch<'gc, 'a> {
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: &'a mut Value<'gc>,
        other: &'a mut Value<'gc>,
        registers: &'a mut [Value<'gc>],
        heap: &'a mut [OwnedHeapVar<'gc>],
        stack: Stack<'gc, 'a>,
        num_args: usize,
    }

    impl<'gc, 'a> Dispatch<'gc, 'a> {
        #[inline]
        fn do_get_field(&mut self, obj: Value<'gc>, key: String<'gc>) -> Result<Value<'gc>, Error> {
            match obj {
                Value::Object(object) => object.get(key).ok_or(OpError::NoSuchField.into()),
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.get_field(self.ctx, user_data, key)
                    } else {
                        Err(OpError::BadObject.into())
                    }
                }
                _ => Err(OpError::BadObject.into()),
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
                        return Err(OpError::BadObject.into());
                    }
                }
                _ => return Err(OpError::BadObject.into()),
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
                        .ok_or(OpError::BadIndex)?;
                    Ok(array.get(index))
                }
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.get_index(self.ctx, user_data, index)
                    } else {
                        Err(OpError::BadArray.into())
                    }
                }
                _ => Err(OpError::BadArray.into()),
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
                        .ok_or(OpError::BadIndex)?;
                    array.set(&self.ctx, index, value);
                    Ok(())
                }
                Value::UserData(user_data) => {
                    if let Some(methods) = user_data.methods() {
                        methods.set_index(self.ctx, user_data, index, value)
                    } else {
                        Err(OpError::BadObject.into())
                    }
                }
                _ => Err(OpError::BadArray.into()),
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
                .ok_or(OpError::BadClosureIdx)?;

            let mut heap = Vec::new();
            for &hd in &proto.heap_vars {
                match hd {
                    HeapVarDescriptor::Owned(idx) => {
                        heap.push(HeapVar::Owned(idx));
                    }
                    HeapVarDescriptor::Static(idx) => {
                        heap.push(HeapVar::Shared(proto.static_vars[idx as usize]))
                    }
                    HeapVarDescriptor::UpValue(idx) => {
                        heap.push(HeapVar::Shared(match self.closure.heap()[idx as usize] {
                            HeapVar::Owned(idx) => self.heap[idx as usize].make_shared(&self.ctx),
                            HeapVar::Shared(v) => v,
                        }));
                    }
                }
            }

            // inner closures inherit the set of magic values from the parent, and inherit the
            // current `this` value.
            self.registers[dest as usize] = Closure::from_parts(
                &self.ctx,
                proto,
                *self.this,
                Gc::new(&self.ctx, heap.into_boxed_slice()),
            )?
            .into();
            Ok(())
        }

        #[inline]
        fn get_heap(&mut self, dest: RegIdx, heap: HeapIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = match *self
                .closure
                .heap()
                .get(heap as usize)
                .ok_or(OpError::BadHeapIdx)?
            {
                HeapVar::Owned(idx) => self.heap[idx as usize].get(),
                HeapVar::Shared(v) => v.get(),
            };
            Ok(())
        }

        #[inline]
        fn set_heap(&mut self, heap: HeapIdx, source: RegIdx) -> Result<(), Self::Error> {
            let source = self.registers[source as usize];
            match *self
                .closure
                .heap()
                .get(heap as usize)
                .ok_or(OpError::BadHeapIdx)?
            {
                HeapVar::Owned(idx) => self.heap[idx as usize].set(&self.ctx, source),
                HeapVar::Shared(v) => v.set(&self.ctx, source),
            };
            Ok(())
        }

        #[inline]
        fn reset_heap(&mut self, heap: HeapIdx) -> Result<(), Self::Error> {
            match *self
                .closure
                .heap()
                .get(heap as usize)
                .ok_or(OpError::BadHeapIdx)?
            {
                HeapVar::Owned(idx) => {
                    self.heap[idx as usize] = OwnedHeapVar::unique(Value::Undefined);
                    Ok(())
                }
                HeapVar::Shared(_) => Err(OpError::ResetHeapNotOwned.into()),
            }
        }

        #[inline]
        fn globals(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = self.ctx.globals().into();
            Ok(())
        }

        #[inline]
        fn this(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = *self.this;
            Ok(())
        }

        #[inline]
        fn other(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = *self.other;
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
        fn arg_count(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
            self.registers[dest as usize] = Value::Integer(self.num_args as i64);
            Ok(())
        }

        #[inline]
        fn argument(&mut self, dest: RegIdx, index: ArgIdx) -> Result<(), Self::Error> {
            if (index as usize) < self.num_args {
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
                return Err(OpError::BadKey.into());
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
                return Err(OpError::BadKey.into());
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
                return Err(OpError::BadKey.into());
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
                return Err(OpError::BadKey.into());
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
                .ok_or(OpError::BadOp)?;
            Ok(())
        }

        #[inline]
        fn add(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.add(arg2).ok_or(OpError::BadOp)?;
            Ok(())
        }

        #[inline]
        fn sub(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.sub(arg2).ok_or(OpError::BadOp)?;
            Ok(())
        }

        fn mult(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.mult(arg2).ok_or(OpError::BadOp)?;
            Ok(())
        }

        fn div(&mut self, dest: RegIdx, arg1: RegIdx, arg2: RegIdx) -> Result<(), Self::Error> {
            let arg1 = self.registers[arg1 as usize];
            let arg2 = self.registers[arg2 as usize];
            let dest = &mut self.registers[dest as usize];
            *dest = arg1.div(arg2).ok_or(OpError::BadOp)?;
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
                .ok_or(OpError::BadOp)?
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
                .ok_or(OpError::BadOp)?
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
        fn push(&mut self, source: RegIdx, len: ArgIdx) -> Result<(), Self::Error> {
            for i in 0..len {
                self.stack
                    .sub_stack(self.num_args)
                    .push_back(self.registers[source as usize + i as usize]);
            }
            Ok(())
        }

        #[inline]
        fn pop(&mut self, dest: RegIdx, len: ArgIdx) -> Result<(), Self::Error> {
            for i in (0..len).rev() {
                self.registers[dest as usize + i as usize] = self
                    .stack
                    .sub_stack(self.num_args)
                    .pop_back()
                    .ok_or(OpError::StackUnderflow)?;
            }
            Ok(())
        }

        #[inline]
        fn push_this(&mut self, source: RegIdx) -> Result<(), Self::Error> {
            let prev_this = *self.this;
            let prev_other = *self.other;
            self.stack.sub_stack(self.num_args).push_back(prev_other);
            *self.other = prev_this;
            *self.this = self.registers[source as usize];
            Ok(())
        }

        #[inline]
        fn pop_this(&mut self) -> Result<(), Self::Error> {
            let old_other = self
                .stack
                .sub_stack(self.num_args)
                .pop_back()
                .ok_or(OpError::StackUnderflow)?;
            *self.this = *self.other;
            *self.other = old_other;
            Ok(())
        }

        #[inline]
        fn get_magic(&mut self, dest: RegIdx, magic: MagicIdx) -> Result<(), Self::Error> {
            let magic = self
                .closure
                .prototype()
                .magic
                .get(magic as usize)
                .map_err(|_| OpError::BadMagicIdx)?;
            self.registers[dest as usize] = magic.get(self.ctx)?;
            Ok(())
        }

        #[inline]
        fn set_magic(&mut self, magic: MagicIdx, source: RegIdx) -> Result<(), Self::Error> {
            let magic = self
                .closure
                .prototype()
                .magic
                .get(magic as usize)
                .map_err(|_| OpError::BadMagicIdx)?;
            magic.set(self.ctx, self.registers[source as usize])?;
            Ok(())
        }

        #[inline]
        fn call(
            &mut self,
            func: RegIdx,
            arguments: ArgIdx,
            returns: ArgIdx,
        ) -> Result<ControlFlow<Self::Break>, Self::Error> {
            let func = self.registers[func as usize]
                .to_function()
                .ok_or(OpError::BadCall)?;

            if self.stack.len() < self.num_args + arguments as usize {
                return Err(OpError::StackUnderflow.into());
            }

            Ok(match func {
                Function::Closure(closure) => ControlFlow::Break(Next::Call {
                    closure,
                    arguments,
                    returns,
                }),
                Function::Callback(callback) => {
                    let call_bottom = self.stack.len() - arguments as usize;

                    callback.call_with(self.ctx, *self.this, self.stack.sub_stack(call_bottom))?;

                    // Pad stack with undefined values to match the expected return len.
                    self.stack.sub_stack(call_bottom).resize(returns as usize);

                    ControlFlow::Continue(())
                }
            })
        }

        #[inline]
        fn return_(&mut self, count: ArgIdx) -> Result<Self::Break, Self::Error> {
            if self.stack.len() < self.num_args + count as usize {
                return Err(OpError::StackUnderflow.into());
            }

            Ok(Next::Return { count })
        }
    }

    let mut dispatcher = bytecode::Dispatcher::new(&closure.prototype().as_ref().bytecode, *pc);
    let ret = dispatcher.dispatch_loop(&mut Dispatch {
        ctx,
        closure,
        this,
        other,
        registers,
        heap,
        stack,
        num_args,
    });
    *pc = dispatcher.pc();

    match ret {
        Ok(next) => Ok(next),
        Err(error) => {
            let prototype = closure.prototype();
            let (ind, inst, span) = prototype.bytecode.instruction_for_pc(*pc).unwrap();
            Err(VmError {
                error,
                chunk_name: prototype.chunk.name().clone(),
                instruction_index: ind,
                instruction: inst,
                line_number: prototype.chunk.line_number(span.start()),
            })
        }
    }
}
