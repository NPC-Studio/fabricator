use std::{fmt, mem, ops::ControlFlow};

use gc_arena::{Collect, Gc, Lock, Mutation, RefLock};
use thiserror::Error;

use crate::{
    array::Array,
    closure::{Closure, Constant, HeapVar, HeapVarDescriptor, SharedValue},
    debug::{LineNumber, RefName},
    error::Error,
    instructions::{self, ConstIdx, HeapIdx, Instruction, MagicIdx, ProtoIdx, RegIdx},
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
    #[error("bad stack index")]
    BadStackIdx,
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
        let used_registers = proto.used_registers();
        assert!(used_registers <= 256);

        let register_bottom = self.registers.len();
        self.registers
            .resize(register_bottom + 256, Value::Undefined);

        let heap_bottom = self.heap.len();
        for hd in closure.heap() {
            if let &HeapVar::Owned(idx) = hd {
                let idx = heap_bottom + idx as usize;
                self.heap
                    .resize_with(idx + 1, || OwnedHeapVar::unique(Value::Undefined));
            }
        }

        if !closure.this().is_undefined() {
            other = this;
            this = closure.this();
        }

        let mut frame = Frame {
            closure,
            this,
            other,
            pc: 0,
        };

        loop {
            let mut dispatcher = instructions::Dispatcher::new(
                &frame.closure.prototype().as_ref().bytecode(),
                frame.pc,
            );

            let ret = dispatcher.dispatch_loop(&mut Dispatch {
                ctx,
                frame: &mut frame,
                registers: (&mut self.registers[register_bottom..register_bottom + 256])
                    .try_into()
                    .unwrap(),
                heap: &mut self.heap[heap_bottom..],
                stack: Stack::new(&mut self.stack, stack_bottom),
            });
            frame.pc = dispatcher.pc();

            match ret {
                Ok(next) => match next {
                    Next::Call {
                        closure,
                        args_bottom,
                    } => {
                        // We only preserve the registers that the prototype claims to use,
                        self.registers.truncate(register_bottom + used_registers);

                        self.call(
                            ctx,
                            closure,
                            frame.this,
                            frame.other,
                            stack_bottom + args_bottom,
                        )?;

                        // Resize the register slice to be 256 wide.
                        self.registers
                            .resize(register_bottom + 256, Value::Undefined);
                    }
                    Next::Return { returns_bottom } => {
                        // Clear the registers for this frame.
                        self.registers.truncate(register_bottom);
                        // Drain everything on the stack up until the returns.
                        self.stack
                            .drain(stack_bottom..stack_bottom + returns_bottom);

                        return Ok(());
                    }
                },
                Err(error) => {
                    let prototype = frame.closure.prototype();
                    let (ind, inst, span) =
                        prototype.bytecode().instruction_for_pc(frame.pc).unwrap();
                    let chunk = prototype.chunk();
                    return Err(VmError {
                        error,
                        chunk_name: chunk.name().clone(),
                        instruction_index: ind,
                        instruction: inst,
                        line_number: chunk.line_number(span.start()),
                    }
                    .into());
                }
            }
        }
    }
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
    #[inline]
    fn unique(value: Value<'gc>) -> Self {
        Self::Unique(value)
    }

    #[inline]
    fn get(&self) -> Value<'gc> {
        match self {
            OwnedHeapVar::Unique(v) => *v,
            OwnedHeapVar::Shared(v) => v.get(),
        }
    }

    #[inline]
    fn set(&mut self, mc: &Mutation<'gc>, value: Value<'gc>) {
        match self {
            OwnedHeapVar::Unique(v) => *v = value,
            OwnedHeapVar::Shared(v) => v.set(mc, value),
        }
    }

    #[inline]
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

struct Frame<'gc> {
    closure: Closure<'gc>,
    this: Value<'gc>,
    other: Value<'gc>,
    pc: usize,
}

enum Next<'gc> {
    Call {
        closure: Closure<'gc>,
        args_bottom: usize,
    },
    Return {
        returns_bottom: usize,
    },
}

struct Dispatch<'gc, 'a> {
    ctx: Context<'gc>,
    frame: &'a mut Frame<'gc>,
    // The register slice is fixed size to avoid bounds checks.
    registers: &'a mut [Value<'gc>; 256],
    heap: &'a mut [OwnedHeapVar<'gc>],
    stack: Stack<'gc, 'a>,
}

impl<'gc, 'a> Dispatch<'gc, 'a> {
    #[inline]
    fn do_get_field(&self, obj: Value<'gc>, key: String<'gc>) -> Result<Value<'gc>, Error> {
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
        &self,
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
    fn do_get_index(&self, array: Value<'gc>, indexes: &[Value<'gc>]) -> Result<Value<'gc>, Error> {
        match array {
            Value::Array(array) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadIndex.into());
                }
                let index = indexes[0]
                    .to_integer()
                    .and_then(|i| i.try_into().ok())
                    .ok_or(OpError::BadIndex)?;
                Ok(array.get(index))
            }
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    methods.get_index(self.ctx, user_data, indexes)
                } else {
                    Err(OpError::BadArray.into())
                }
            }
            _ => Err(OpError::BadArray.into()),
        }
    }

    #[inline]
    fn do_set_index(
        &self,
        array: Value<'gc>,
        indexes: &[Value<'gc>],
        value: Value<'gc>,
    ) -> Result<(), Error> {
        match array {
            Value::Array(array) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadIndex.into());
                }

                let index = indexes[0]
                    .to_integer()
                    .and_then(|i| i.try_into().ok())
                    .ok_or(OpError::BadIndex)?;
                array.set(&self.ctx, index, value);
                Ok(())
            }
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    methods.set_index(self.ctx, user_data, indexes, value)
                } else {
                    Err(OpError::BadObject.into())
                }
            }
            _ => Err(OpError::BadArray.into()),
        }
    }
}

impl<'gc, 'a> instructions::Dispatch for Dispatch<'gc, 'a> {
    type Break = Next<'gc>;
    type Error = Error;

    #[inline]
    fn undefined(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = Value::Undefined;
        Ok(())
    }

    #[inline]
    fn boolean(&mut self, dest: RegIdx, is_true: bool) -> Result<(), Self::Error> {
        self.registers[dest as usize] = Value::Boolean(is_true);
        Ok(())
    }

    #[inline]
    fn load_constant(&mut self, dest: RegIdx, constant: ConstIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] =
            self.frame.closure.prototype().constants()[constant as usize].to_value();
        Ok(())
    }

    #[inline]
    fn get_heap(&mut self, dest: RegIdx, heap: HeapIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = match self.frame.closure.heap()[heap as usize] {
            HeapVar::Owned(idx) => self.heap[idx as usize].get(),
            HeapVar::Shared(v) => v.get(),
        };
        Ok(())
    }

    #[inline]
    fn set_heap(&mut self, heap: HeapIdx, source: RegIdx) -> Result<(), Self::Error> {
        let source = self.registers[source as usize];
        match self.frame.closure.heap()[heap as usize] {
            HeapVar::Owned(idx) => self.heap[idx as usize].set(&self.ctx, source),
            HeapVar::Shared(v) => v.set(&self.ctx, source),
        };
        Ok(())
    }

    #[inline]
    fn reset_heap(&mut self, heap: HeapIdx) -> Result<(), Self::Error> {
        match self.frame.closure.heap()[heap as usize] {
            HeapVar::Owned(idx) => {
                self.heap[idx as usize] = OwnedHeapVar::unique(Value::Undefined);
                Ok(())
            }
            HeapVar::Shared(_) => panic!("reset of shared heap var"),
        }
    }

    #[inline]
    fn globals(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.ctx.globals().into();
        Ok(())
    }

    #[inline]
    fn this(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.frame.this;
        Ok(())
    }

    #[inline]
    fn set_this(&mut self, source: RegIdx) -> Result<(), Self::Error> {
        self.frame.this = self.registers[source as usize];
        Ok(())
    }

    #[inline]
    fn other(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.frame.other;
        Ok(())
    }

    #[inline]
    fn set_other(&mut self, source: RegIdx) -> Result<(), Self::Error> {
        self.frame.other = self.registers[source as usize];
        Ok(())
    }

    #[inline]
    fn swap_this_other(&mut self) -> Result<(), Self::Error> {
        mem::swap(&mut self.frame.this, &mut self.frame.other);
        Ok(())
    }

    #[inline]
    fn closure(&mut self, dest: RegIdx, proto: ProtoIdx) -> Result<(), Self::Error> {
        let proto = self.frame.closure.prototype().prototypes()[proto as usize];

        let mut heap = Vec::new();
        for &hd in proto.heap_vars() {
            match hd {
                HeapVarDescriptor::Owned(idx) => {
                    heap.push(HeapVar::Owned(idx));
                }
                HeapVarDescriptor::Static(idx) => {
                    heap.push(HeapVar::Shared(proto.static_vars()[idx as usize]))
                }
                HeapVarDescriptor::UpValue(idx) => {
                    heap.push(HeapVar::Shared(
                        match self.frame.closure.heap()[idx as usize] {
                            HeapVar::Owned(idx) => self.heap[idx as usize].make_shared(&self.ctx),
                            HeapVar::Shared(v) => v,
                        },
                    ));
                }
            }
        }

        // inner closures inherit the set of magic values from the parent, and inherit the
        // current `this` value.
        self.registers[dest as usize] = Closure::from_parts(
            &self.ctx,
            proto,
            self.frame.this,
            Gc::new(&self.ctx, heap.into_boxed_slice()),
        )?
        .into();
        Ok(())
    }

    #[inline]
    fn current_closure(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.frame.closure.into();
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
    fn get_field(&mut self, dest: RegIdx, object: RegIdx, key: RegIdx) -> Result<(), Self::Error> {
        let Value::String(key) = self.registers[key as usize] else {
            return Err(OpError::BadKey.into());
        };

        self.registers[dest as usize] = self.do_get_field(self.registers[object as usize], key)?;
        Ok(())
    }

    #[inline]
    fn set_field(&mut self, object: RegIdx, key: RegIdx, value: RegIdx) -> Result<(), Self::Error> {
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
        let Constant::String(key) = self.frame.closure.prototype().constants()[key as usize] else {
            panic!("const key is not a string");
        };
        self.registers[dest as usize] = self.do_get_field(self.registers[object as usize], key)?;
        Ok(())
    }

    #[inline]
    fn set_field_const(
        &mut self,
        object: RegIdx,
        key: ConstIdx,
        value: RegIdx,
    ) -> Result<(), Self::Error> {
        let Constant::String(key) = self.frame.closure.prototype().constants()[key as usize] else {
            panic!("const key is not a string");
        };
        self.do_set_field(
            self.registers[object as usize],
            key,
            self.registers[value as usize],
        )
    }

    #[inline]
    fn get_index(&mut self, dest: RegIdx, array: RegIdx, index: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.do_get_index(
            self.registers[array as usize],
            &[self.registers[index as usize]],
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
            &[self.registers[index as usize]],
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
            &[self.frame.closure.prototype().constants()[index as usize].to_value()],
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
            &[self.frame.closure.prototype().constants()[index as usize].to_value()],
            self.registers[value as usize],
        )?;
        Ok(())
    }

    #[inline]
    fn copy(&mut self, dest: RegIdx, source: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[source as usize];
        Ok(())
    }

    #[inline]
    fn is_defined(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = (!self.registers[arg as usize].is_undefined()).into();
        Ok(())
    }

    #[inline]
    fn is_undefined(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[arg as usize].is_undefined().into();
        Ok(())
    }

    #[inline]
    fn test(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[arg as usize].to_bool().into();
        Ok(())
    }

    #[inline]
    fn not(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = (!self.registers[arg as usize].to_bool()).into();
        Ok(())
    }

    #[inline]
    fn negate(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[arg as usize]
            .negate()
            .ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn increment(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[arg as usize]
            .add(Value::Integer(1))
            .ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn decrement(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[arg as usize]
            .sub(Value::Integer(1))
            .ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn add(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.add(right).ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn subtract(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.sub(right).ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn multiply(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.mult(right).ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn divide(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.div(right).ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn remainder(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.rem(right).ok_or(OpError::BadOp)?;
        Ok(())
    }

    #[inline]
    fn int_divide(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.idiv(right).ok_or(OpError::BadOp)?.into();
        Ok(())
    }

    #[inline]
    fn is_equal(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.equal(right).into();
        Ok(())
    }

    #[inline]
    fn is_not_equal(
        &mut self,
        dest: RegIdx,
        left: RegIdx,
        right: RegIdx,
    ) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = (!left.equal(right)).into();
        Ok(())
    }

    #[inline]
    fn is_less(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[left as usize]
            .less_than(self.registers[right as usize])
            .ok_or(OpError::BadOp)?
            .into();
        Ok(())
    }

    #[inline]
    fn is_less_equal(
        &mut self,
        dest: RegIdx,
        left: RegIdx,
        right: RegIdx,
    ) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.registers[left as usize]
            .less_equal(self.registers[right as usize])
            .ok_or(OpError::BadOp)?
            .into();
        Ok(())
    }

    #[inline]
    fn and(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = (left.to_bool() && right.to_bool()).into();
        Ok(())
    }

    #[inline]
    fn or(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = (left.to_bool() || right.to_bool()).into();
        Ok(())
    }

    #[inline]
    fn null_coalesce(
        &mut self,
        dest: RegIdx,
        left: RegIdx,
        right: RegIdx,
    ) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = if left.is_undefined() { right } else { left };
        Ok(())
    }

    #[inline]
    fn check(&mut self, test: RegIdx, is_true: bool) -> Result<bool, Self::Error> {
        Ok(self.registers[test as usize].to_bool() == is_true)
    }

    #[inline]
    fn stack_top(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = (self.stack.len() as i64).into();
        Ok(())
    }

    #[inline]
    fn stack_resize(&mut self, stack_top: RegIdx) -> Result<(), Self::Error> {
        let stack_top = self.registers[stack_top as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        self.stack.resize(stack_top);
        Ok(())
    }

    #[inline]
    fn stack_resize_const(&mut self, stack_pos: ConstIdx) -> Result<(), Self::Error> {
        let stack_top = self.frame.closure.prototype().constants()[stack_pos as usize]
            .to_value()
            .to_integer()
            .expect("const index is not integer")
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        self.stack.resize(stack_top);
        Ok(())
    }

    #[inline]
    fn stack_get(&mut self, dest: RegIdx, stack_pos: RegIdx) -> Result<(), Self::Error> {
        let stack_idx = self.registers[stack_pos as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        // We return `Undefined` if the `stack_idx` is out of range.
        self.registers[dest as usize] = self.stack.get(stack_idx);
        Ok(())
    }

    #[inline]
    fn stack_get_const(&mut self, dest: RegIdx, stack_pos: ConstIdx) -> Result<(), Self::Error> {
        let stack_idx = self.frame.closure.prototype().constants()[stack_pos as usize]
            .to_value()
            .to_integer()
            .expect("const index is not integer")
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        // We return `Undefined` if the `stack_idx` is out of range.
        self.registers[dest as usize] = self.stack.get(stack_idx);
        Ok(())
    }

    #[inline]
    fn stack_get_offset(
        &mut self,
        dest: RegIdx,
        stack_base: RegIdx,
        offset: ConstIdx,
    ) -> Result<(), Self::Error> {
        let offset = self.frame.closure.prototype().constants()[offset as usize]
            .to_value()
            .to_integer()
            .expect("const index is not integer");
        let stack_idx = self.registers[stack_base as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .checked_add(offset)
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        // We return `Undefined` if the `stack_idx` is out of range.
        self.registers[dest as usize] = self.stack.get(stack_idx);
        Ok(())
    }

    #[inline]
    fn stack_set(&mut self, source: RegIdx, stack_pos: RegIdx) -> Result<(), Self::Error> {
        let stack_idx: usize = self.registers[stack_pos as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        // We need to implicitly grow the stack if the register is out of range.
        if stack_idx >= self.stack.len() {
            self.stack.resize(stack_idx + 1);
        }
        self.stack[stack_idx] = self.registers[source as usize];
        Ok(())
    }

    #[inline]
    fn stack_push(&mut self, source: RegIdx) -> Result<(), Self::Error> {
        self.stack.push_back(self.registers[source as usize]);
        Ok(())
    }

    #[inline]
    fn stack_pop(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = self.stack.pop_back().unwrap_or_default();
        Ok(())
    }

    #[inline]
    fn get_index_multi(
        &mut self,
        dest: RegIdx,
        array: RegIdx,
        stack_bottom: RegIdx,
    ) -> Result<(), Self::Error> {
        let mut stack_bottom: usize = self.registers[stack_bottom as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        stack_bottom = stack_bottom.min(self.stack.len());

        self.registers[dest as usize] =
            self.do_get_index(self.registers[array as usize], &self.stack[stack_bottom..])?;
        Ok(())
    }

    #[inline]
    fn set_index_multi(
        &mut self,
        array: RegIdx,
        stack_bottom: RegIdx,
        value: RegIdx,
    ) -> Result<(), Self::Error> {
        let mut stack_bottom: usize = self.registers[stack_bottom as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        stack_bottom = stack_bottom.min(self.stack.len());

        self.do_set_index(
            self.registers[array as usize],
            &self.stack[stack_bottom..],
            self.registers[value as usize],
        )?;
        Ok(())
    }

    #[inline]
    fn get_magic(&mut self, dest: RegIdx, magic: MagicIdx) -> Result<(), Self::Error> {
        let magic = self
            .frame
            .closure
            .prototype()
            .magic()
            .get(magic as usize)
            .expect("magic idx is not valid");
        self.registers[dest as usize] = magic.get(self.ctx)?;
        Ok(())
    }

    #[inline]
    fn set_magic(&mut self, magic: MagicIdx, source: RegIdx) -> Result<(), Self::Error> {
        let magic = self
            .frame
            .closure
            .prototype()
            .magic()
            .get(magic as usize)
            .expect("magic idx is not valid");
        magic.set(self.ctx, self.registers[source as usize])?;
        Ok(())
    }

    #[inline]
    fn call(
        &mut self,
        func: RegIdx,
        stack_bottom: RegIdx,
    ) -> Result<ControlFlow<Self::Break>, Self::Error> {
        let func = self.registers[func as usize]
            .to_function()
            .ok_or(OpError::BadCall)?;

        let stack_bottom: usize = self.registers[stack_bottom as usize]
            .to_integer()
            .ok_or(OpError::BadStackIdx)?
            .try_into()
            .map_err(|_| OpError::BadStackIdx)?;
        if stack_bottom > self.stack.len() {
            self.stack.resize(stack_bottom);
        }

        Ok(match func {
            Function::Closure(closure) => ControlFlow::Break(Next::Call {
                closure,
                args_bottom: stack_bottom,
            }),
            Function::Callback(callback) => {
                callback.call_with(
                    self.ctx,
                    self.frame.this,
                    self.stack.sub_stack(stack_bottom),
                )?;
                ControlFlow::Continue(())
            }
        })
    }

    #[inline]
    fn return_(&mut self, stack_bottom: RegIdx) -> Result<Self::Break, Self::Error> {
        let stack_bottom = usize::try_from(
            self.registers[stack_bottom as usize]
                .to_integer()
                .ok_or(OpError::BadStackIdx)?,
        )
        .map_err(|_| OpError::BadStackIdx)?
        .min(self.stack.len());

        Ok(Next::Return {
            returns_bottom: stack_bottom,
        })
    }
}
