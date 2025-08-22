use std::{fmt, mem, ops::ControlFlow};

use gc_arena::{Collect, Gc, Lock, Mutation, RefLock};
use thiserror::Error;

use crate::{
    array::Array,
    closure::{Closure, Constant, HeapVar, HeapVarDescriptor, SharedValue},
    debug::{LineNumber, RefName},
    error::{Error, ExternError, ExternValue, RuntimeError, ScriptError},
    instructions::{self, ConstIdx, HeapIdx, Instruction, MagicIdx, ProtoIdx, RegIdx},
    interpreter::Context,
    object::Object,
    stack::Stack,
    string::String,
    value::{Function, Value},
};

#[derive(Debug, Clone, Error)]
pub enum OpError {
    #[error("bad unary op {op:?} {arg:?}")]
    BadUnOp { op: &'static str, arg: ExternValue },
    #[error("bad binary op {left:?} {op:?} {right:?}")]
    BadBinOp {
        op: &'static str,
        left: ExternValue,
        right: ExternValue,
    },
    #[error("bad object {object:?}")]
    BadObject { object: ExternValue },
    #[error("bad key {key:?}")]
    BadKey { key: ExternValue },
    #[error("bad array {array:?}")]
    BadArray { array: ExternValue },
    #[error("bad index {index:?} of {target:?}")]
    BadIndex {
        target: ExternValue,
        index: ExternValue,
    },
    #[error("{target:?} does not allow multi-indexing")]
    BadMultiIndex { target: ExternValue },
    #[error("no such field {field:?} in {target:?}")]
    NoSuchField {
        target: ExternValue,
        field: ExternValue,
    },
    #[error("bad call of {target:?}")]
    BadCall { target: &'static str },
    #[error("bad stack index {index}")]
    BadStackIdx { index: ExternValue },
    #[error("bad stack index {index} and offset {offset}")]
    BadStackIdxOffset {
        index: ExternValue,
        offset: ExternValue,
    },
}

#[derive(Debug)]
pub struct VmError<'gc> {
    pub error: Error<'gc>,
    pub chunk_name: RefName,
    pub instruction_index: usize,
    pub instruction: Instruction,
    pub line_number: LineNumber,
}

impl<'gc> fmt::Display for VmError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VM error in {} at line {} instruction {}: {}",
            self.chunk_name, self.line_number, self.instruction_index, self.error,
        )
    }
}

impl<'gc> VmError<'gc> {
    pub fn into_extern(self) -> ExternVmError {
        ExternVmError {
            error: self.error.into_extern(),
            chunk_name: self.chunk_name,
            instruction_index: self.instruction_index,
            instruction: self.instruction,
            line_number: self.line_number,
        }
    }
}

#[derive(Debug, Error)]
pub struct ExternVmError {
    #[source]
    pub error: ExternError,
    pub chunk_name: RefName,
    pub instruction_index: usize,
    pub instruction: Instruction,
    pub line_number: LineNumber,
}

impl fmt::Display for ExternVmError {
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

    /// Calls `Thread::exec_with` with both `this` and `other` set to `ctx.globals()`.
    pub fn exec(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
    ) -> Result<Vec<Value<'gc>>, ExternVmError> {
        let globals = ctx.globals().into();
        self.exec_with(ctx, closure, globals, globals)
    }

    /// Execute a closure on this `Thread`.
    ///
    /// if `this` or `other` are `Value::Undefined`, then they will be automatically set to
    /// `ctx.globals()`.
    pub fn exec_with(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        mut this: Value<'gc>,
        mut other: Value<'gc>,
    ) -> Result<Vec<Value<'gc>>, ExternVmError> {
        let mut thread = self.0.try_borrow_mut(&ctx).expect("thread locked");
        assert!(thread.registers.is_empty() && thread.stack.is_empty() && thread.heap.is_empty());

        struct DropGuard<'gc, 'a>(&'a mut ThreadState<'gc>);

        impl<'gc, 'a> Drop for DropGuard<'gc, 'a> {
            fn drop(&mut self) {
                self.0.registers.clear();
                self.0.stack.clear();
                self.0.heap.clear();
            }
        }

        this = this.null_coalesce(ctx.globals().into());
        other = other.null_coalesce(ctx.globals().into());

        let guard = DropGuard(&mut *thread);

        guard
            .0
            .call(ctx, closure, this, other, 0)
            .map_err(VmError::into_extern)?;

        Ok(guard.0.stack.drain(..).collect())
    }
}

/// An execution context for some `Thread`.
///
/// This type is passed to all callbacks to allow them to manipulate the call stack and call FML
/// code using the calling `Thread`.
pub struct Execution<'gc, 'a> {
    thread: &'a mut ThreadState<'gc>,
    stack_bottom: usize,
    this: Value<'gc>,
    other: Value<'gc>,
}

impl<'gc, 'a> Execution<'gc, 'a> {
    /// Return a slice of the current call stack containing callback arguments and returns.
    #[inline]
    pub fn stack(&mut self) -> Stack<'gc, '_> {
        Stack::new(&mut self.thread.stack, self.stack_bottom)
    }

    /// Return the current `this` value.
    #[inline]
    pub fn this(&self) -> Value<'gc> {
        self.this
    }

    /// Return the current `other` value.
    #[inline]
    pub fn other(&self) -> Value<'gc> {
        self.other
    }

    /// Return a new execution context with potentially changed stack bottom and  `this` / `other`
    /// values.
    ///
    /// If the provided `stack_bottom` is not 0, then the returned `Execution` will have a stack
    /// starting at this new provided bottom value.
    ///
    /// If the provided `this` value is not `Value::Undefined`, then returns an `Execution` with the
    /// `this` set as the provided value and the `other` set as the previous value of `this`.
    pub fn with(&mut self, stack_bottom: usize, this: Value<'gc>) -> Execution<'gc, '_> {
        assert!(self.thread.stack.len() >= self.stack_bottom + stack_bottom);

        if this.is_undefined() {
            Execution {
                thread: self.thread,
                stack_bottom: self.stack_bottom + stack_bottom,
                this: self.this,
                other: self.other,
            }
        } else {
            Execution {
                thread: self.thread,
                stack_bottom: self.stack_bottom + stack_bottom,
                this: this,
                other: self.this,
            }
        }
    }

    /// Return a new `Execution` with the same stack bottom and `this` / `other` values.
    ///
    /// Equivalent to `Execution::with(0, Value::Undefined)`.
    pub fn reborrow(&mut self) -> Execution<'gc, '_> {
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom,
            this: self.this,
            other: self.other,
        }
    }

    /// Within a callback, call the given closure using the parent `Thread`.
    ///
    /// Arguments to the closure will be taken from the stack and returns placed back into the
    /// stack.
    pub fn call_closure(
        &mut self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
    ) -> Result<(), VmError<'gc>> {
        self.thread
            .call(ctx, closure, self.this, self.other, self.stack_bottom)
    }

    /// Call a `Function` within a callback.
    ///
    /// Arguments to the function will be taken from the stack and returns placed back into the
    /// stack.
    ///
    /// If the provided `function` is a closure, then any returned `VmError` will be turned into an
    /// `ExternVmError` and placed into a `RuntimeError`.
    pub fn call(&mut self, ctx: Context<'gc>, function: Function<'gc>) -> Result<(), RuntimeError> {
        match function {
            Function::Closure(closure) => Ok(self
                .call_closure(ctx, closure)
                .map_err(|e| e.into_extern())?),
            Function::Callback(callback) => callback.call(ctx, self.reborrow()),
        }
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
    ) -> Result<(), VmError<'gc>> {
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

            let mut error = None;
            match ret {
                Ok(next) => match next {
                    Next::Call {
                        function,
                        args_bottom,
                    } => {
                        match function {
                            Function::Closure(closure) => {
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
                            Function::Callback(callback) => {
                                let execution = Execution {
                                    thread: self,
                                    stack_bottom: stack_bottom + args_bottom,
                                    this: frame.this,
                                    other: frame.other,
                                };
                                if let Err(err) = callback.call(ctx, execution) {
                                    error = Some(err.into());
                                }
                            }
                        }
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
                Err(err) => {
                    error = Some(err);
                }
            }

            if let Some(err) = error {
                let prototype = frame.closure.prototype();
                let bytecode = prototype.bytecode();
                let inst_index = bytecode.instruction_index_for_pc(frame.pc).unwrap();
                let inst = bytecode.instruction(inst_index);
                let span = bytecode.span(inst_index);
                let chunk = prototype.chunk();
                return Err(VmError {
                    error: err,
                    chunk_name: chunk.name().clone(),
                    instruction_index: inst_index,
                    instruction: inst,
                    line_number: chunk.line_number(span.start()),
                }
                .into());
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
        function: Function<'gc>,
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
    fn do_get_field(&self, obj: Value<'gc>, key: String<'gc>) -> Result<Value<'gc>, RuntimeError> {
        match obj {
            Value::Object(object) => object.get(key).ok_or_else(|| {
                OpError::NoSuchField {
                    target: obj.into(),
                    field: Value::from(key).into(),
                }
                .into()
            }),
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    Ok(methods.get_field(self.ctx, user_data, key)?)
                } else {
                    Err(OpError::BadObject { object: obj.into() }.into())
                }
            }
            _ => Err(OpError::BadObject { object: obj.into() }.into()),
        }
    }

    #[inline]
    fn do_set_field(
        &self,
        obj: Value<'gc>,
        key: String<'gc>,
        value: Value<'gc>,
    ) -> Result<(), RuntimeError> {
        match obj {
            Value::Object(object) => {
                object.set(&self.ctx, key, value);
            }
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    methods.set_field(self.ctx, user_data, key, value)?;
                } else {
                    return Err(OpError::BadObject { object: obj.into() }.into());
                }
            }
            _ => {
                return Err(OpError::BadObject { object: obj.into() }.into());
            }
        }

        Ok(())
    }

    #[inline]
    fn do_get_index(
        &self,
        target: Value<'gc>,
        indexes: &[Value<'gc>],
    ) -> Result<Value<'gc>, RuntimeError> {
        match target {
            Value::Object(object) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadMultiIndex {
                        target: target.into(),
                    }
                    .into());
                }
                let Some(index) = indexes[0].cast_string(self.ctx) else {
                    return Err(OpError::BadIndex {
                        target: target.into(),
                        index: indexes[0].into(),
                    }
                    .into());
                };
                Ok(object.get(index).unwrap_or_default())
            }
            Value::Array(array) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadMultiIndex {
                        target: target.into(),
                    }
                    .into());
                }
                let index = indexes[0]
                    .cast_integer()
                    .and_then(|i| i.try_into().ok())
                    .ok_or_else(|| OpError::BadIndex {
                        target: target.into(),
                        index: indexes[0].into(),
                    })?;
                Ok(array.get(index))
            }
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    Ok(methods.get_index(self.ctx, user_data, indexes)?)
                } else {
                    Err(OpError::BadArray {
                        array: target.into(),
                    }
                    .into())
                }
            }
            _ => Err(OpError::BadArray {
                array: target.into(),
            }
            .into()),
        }
    }

    #[inline]
    fn do_set_index(
        &self,
        target: Value<'gc>,
        indexes: &[Value<'gc>],
        value: Value<'gc>,
    ) -> Result<(), RuntimeError> {
        match target {
            Value::Object(object) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadMultiIndex {
                        target: target.into(),
                    }
                    .into());
                }
                let Some(index) = indexes[0].cast_string(self.ctx) else {
                    return Err(OpError::BadIndex {
                        target: target.into(),
                        index: indexes[0].into(),
                    }
                    .into());
                };
                object.set(&self.ctx, index, value);
                Ok(())
            }
            Value::Array(array) => {
                if indexes.len() != 1 {
                    return Err(OpError::BadMultiIndex {
                        target: target.into(),
                    }
                    .into());
                }

                let index = indexes[0]
                    .cast_integer()
                    .and_then(|i| i.try_into().ok())
                    .ok_or_else(|| OpError::BadIndex {
                        target: target.into(),
                        index: indexes[0].into(),
                    })?;
                array.set(&self.ctx, index, value);
                Ok(())
            }
            Value::UserData(user_data) => {
                if let Some(methods) = user_data.methods() {
                    Ok(methods.set_index(self.ctx, user_data, indexes, value)?)
                } else {
                    Err(OpError::BadArray {
                        array: target.into(),
                    }
                    .into())
                }
            }
            _ => Err(OpError::BadArray {
                array: target.into(),
            }
            .into()),
        }
    }
}

impl<'gc, 'a> instructions::Dispatch for Dispatch<'gc, 'a> {
    type Break = Next<'gc>;
    type Error = Error<'gc>;

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
        let key_val = self.registers[key as usize];
        let Some(key) = key_val.cast_string(self.ctx) else {
            return Err(OpError::BadKey {
                key: key_val.into(),
            }
            .into());
        };

        self.registers[dest as usize] = self.do_get_field(self.registers[object as usize], key)?;
        Ok(())
    }

    #[inline]
    fn set_field(&mut self, object: RegIdx, key: RegIdx, value: RegIdx) -> Result<(), Self::Error> {
        let key_val = self.registers[key as usize];
        let Some(key) = key_val.cast_string(self.ctx) else {
            return Err(OpError::BadKey {
                key: key_val.into(),
            }
            .into());
        };
        Ok(self.do_set_field(
            self.registers[object as usize],
            key,
            self.registers[value as usize],
        )?)
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
        Ok(self.do_set_field(
            self.registers[object as usize],
            key,
            self.registers[value as usize],
        )?)
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
        self.registers[dest as usize] = self.registers[arg as usize].cast_bool().into();
        Ok(())
    }

    #[inline]
    fn not(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = (!self.registers[arg as usize].cast_bool()).into();
        Ok(())
    }

    #[inline]
    fn negate(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        let arg = self.registers[arg as usize];
        self.registers[dest as usize] = arg.negate().ok_or_else(|| OpError::BadUnOp {
            op: "negate",
            arg: arg.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn bit_negate(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        let arg = self.registers[arg as usize];
        self.registers[dest as usize] = arg
            .bit_negate()
            .ok_or_else(|| OpError::BadUnOp {
                op: "bit_negate",
                arg: arg.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn increment(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        let arg = self.registers[arg as usize];
        self.registers[dest as usize] =
            arg.add(Value::Integer(1)).ok_or_else(|| OpError::BadUnOp {
                op: "increment",
                arg: arg.into(),
            })?;
        Ok(())
    }

    #[inline]
    fn decrement(&mut self, dest: RegIdx, arg: RegIdx) -> Result<(), Self::Error> {
        let arg = self.registers[arg as usize];
        self.registers[dest as usize] =
            arg.sub(Value::Integer(1)).ok_or_else(|| OpError::BadUnOp {
                op: "decrement",
                arg: arg.into(),
            })?;
        Ok(())
    }

    #[inline]
    fn add(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.add(right).ok_or_else(|| OpError::BadBinOp {
            op: "add",
            left: left.into(),
            right: right.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn subtract(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.sub(right).ok_or_else(|| OpError::BadBinOp {
            op: "subtract",
            left: left.into(),
            right: right.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn multiply(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.mult(right).ok_or_else(|| OpError::BadBinOp {
            op: "multiply",
            left: left.into(),
            right: right.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn divide(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.div(right).ok_or_else(|| OpError::BadBinOp {
            op: "divide",
            left: left.into(),
            right: right.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn remainder(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.rem(right).ok_or_else(|| OpError::BadBinOp {
            op: "remainder",
            left: left.into(),
            right: right.into(),
        })?;
        Ok(())
    }

    #[inline]
    fn int_divide(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .idiv(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "int_divide",
                left: left.into(),
                right: right.into(),
            })?
            .into();
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
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        self.registers[dest as usize] = left
            .less_than(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "is_less",
                left: left.into(),
                right: right.into(),
            })?
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
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        self.registers[dest as usize] = left
            .less_equal(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "is_less_equal",
                left: left.into(),
                right: right.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn and(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.and(right).into();
        Ok(())
    }

    #[inline]
    fn or(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.or(right).into();
        Ok(())
    }

    #[inline]
    fn xor(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left.xor(right).into();
        Ok(())
    }

    #[inline]
    fn bit_and(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .bit_and(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "bit_and",
                left: left.into(),
                right: right.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn bit_or(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .bit_or(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "bit_or",
                left: left.into(),
                right: right.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn bit_xor(&mut self, dest: RegIdx, left: RegIdx, right: RegIdx) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .bit_xor(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "bit_xor",
                left: left.into(),
                right: right.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn bit_shift_left(
        &mut self,
        dest: RegIdx,
        left: RegIdx,
        right: RegIdx,
    ) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .bit_shift_left(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "bit_shift_left",
                left: left.into(),
                right: right.into(),
            })?
            .into();
        Ok(())
    }

    #[inline]
    fn bit_shift_right(
        &mut self,
        dest: RegIdx,
        left: RegIdx,
        right: RegIdx,
    ) -> Result<(), Self::Error> {
        let left = self.registers[left as usize];
        let right = self.registers[right as usize];
        let dest = &mut self.registers[dest as usize];
        *dest = left
            .bit_shift_right(right)
            .ok_or_else(|| OpError::BadBinOp {
                op: "bit_shift_right",
                left: left.into(),
                right: right.into(),
            })?
            .into();
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
        *dest = left.null_coalesce(right);
        Ok(())
    }

    #[inline]
    fn jump_if(&mut self, test: RegIdx, is_true: bool) -> Result<bool, Self::Error> {
        Ok(self.registers[test as usize].cast_bool() == is_true)
    }

    #[inline]
    fn stack_top(&mut self, dest: RegIdx) -> Result<(), Self::Error> {
        self.registers[dest as usize] = (self.stack.len() as i64).into();
        Ok(())
    }

    #[inline]
    fn stack_resize(&mut self, stack_top: RegIdx) -> Result<(), Self::Error> {
        let stack_top = self.registers[stack_top as usize];
        let stack_top = stack_top
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_top.into(),
            })?;
        self.stack.resize(stack_top);
        Ok(())
    }

    #[inline]
    fn stack_resize_const(&mut self, stack_top: ConstIdx) -> Result<(), Self::Error> {
        let stack_top = self.frame.closure.prototype().constants()[stack_top as usize];
        let stack_top = stack_top
            .to_value()
            .cast_integer()
            .expect("const index is not integer")
            .try_into()
            .map_err(|_| OpError::BadStackIdx {
                index: stack_top.to_value().into(),
            })?;
        self.stack.resize(stack_top);
        Ok(())
    }

    #[inline]
    fn stack_get(&mut self, dest: RegIdx, stack_pos: RegIdx) -> Result<(), Self::Error> {
        let stack_idx = self.registers[stack_pos as usize];
        let stack_idx = stack_idx
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_idx.into(),
            })?;
        // We return `Undefined` if the `stack_idx` is out of range.
        self.registers[dest as usize] = self.stack.get(stack_idx);
        Ok(())
    }

    #[inline]
    fn stack_get_const(&mut self, dest: RegIdx, stack_pos: ConstIdx) -> Result<(), Self::Error> {
        let stack_idx = self.frame.closure.prototype().constants()[stack_pos as usize];
        let stack_idx = stack_idx
            .to_value()
            .cast_integer()
            .expect("const index is not integer")
            .try_into()
            .map_err(|_| OpError::BadStackIdx {
                index: stack_idx.to_value().into(),
            })?;
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
            .cast_integer()
            .expect("const index is not integer");
        let stack_idx = self.registers[stack_base as usize];
        let stack_idx = stack_idx
            .cast_integer()
            .and_then(|i| i.checked_add(offset))
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdxOffset {
                index: stack_idx.into(),
                offset: Value::Integer(offset).into(),
            })?;
        // We return `Undefined` if the `stack_idx` is out of range.
        self.registers[dest as usize] = self.stack.get(stack_idx);
        Ok(())
    }

    #[inline]
    fn stack_set(&mut self, source: RegIdx, stack_pos: RegIdx) -> Result<(), Self::Error> {
        let stack_idx = self.registers[stack_pos as usize];
        let stack_idx: usize = stack_idx
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_idx.into(),
            })?;
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
        let stack_bottom = self.registers[stack_bottom as usize];
        let mut stack_bottom: usize = stack_bottom
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_bottom.into(),
            })?;
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
        let stack_bottom = self.registers[stack_bottom as usize];
        let mut stack_bottom: usize = stack_bottom
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_bottom.into(),
            })?;
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
    fn throw(&mut self, source: RegIdx) -> Result<(), Self::Error> {
        Err(ScriptError::new(self.registers[source as usize]).into())
    }

    #[inline]
    fn call(
        &mut self,
        func: RegIdx,
        stack_bottom: RegIdx,
    ) -> Result<ControlFlow<Self::Break>, Self::Error> {
        let func = self.registers[func as usize];
        let func = func.to_function().ok_or_else(|| OpError::BadCall {
            target: func.type_name(),
        })?;

        let stack_bottom = self.registers[stack_bottom as usize];
        let stack_bottom: usize = stack_bottom
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_bottom.into(),
            })?;
        if stack_bottom > self.stack.len() {
            self.stack.resize(stack_bottom);
        }

        Ok(ControlFlow::Break(Next::Call {
            function: func,
            args_bottom: stack_bottom,
        }))
    }

    #[inline]
    fn return_(&mut self, stack_bottom: RegIdx) -> Result<Self::Break, Self::Error> {
        let stack_bottom = self.registers[stack_bottom as usize];
        let stack_bottom: usize = stack_bottom
            .cast_integer()
            .and_then(|i| i.try_into().ok())
            .ok_or_else(|| OpError::BadStackIdx {
                index: stack_bottom.into(),
            })?;
        let stack_bottom = stack_bottom.min(self.stack.len());

        Ok(Next::Return {
            returns_bottom: stack_bottom,
        })
    }
}
