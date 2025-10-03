use std::{error::Error as StdError, fmt};

use gc_arena::{Collect, Gc, Lock, Mutation, RefLock};

use crate::{
    callback::Callback,
    closure::{Closure, SharedValue},
    debug::LineNumber,
    error::{Error, ExternError, RawGc, RuntimeError},
    instructions,
    interpreter::Context,
    stack::Stack,
    string::SharedStr,
    thread::dispatch,
    value::{Function, Value},
};

#[derive(Debug)]
pub struct VmError<'gc> {
    pub error: Error<'gc>,
    pub backtrace: Vec<BacktraceFrame<'gc>>,
}

impl<'gc> fmt::Display for VmError<'gc> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.error)?;
        write!(f, "VM backtrace:")?;
        for (i, frame) in self.backtrace.iter().rev().enumerate() {
            writeln!(f)?;
            write!(f, "{:>4}: ", i)?;
            match frame {
                BacktraceFrame::Closure(closure_frame) => {
                    write!(
                        f,
                        "{}:{}",
                        closure_frame.chunk_name(),
                        closure_frame.line_number()
                    )?;
                }
                BacktraceFrame::Callback(callback) => {
                    write!(f, "<callback {:p}>", callback.into_inner())?;
                }
            }
        }
        Ok(())
    }
}

impl<'gc> VmError<'gc> {
    pub fn into_extern(self) -> ExternVmError {
        ExternVmError {
            error: self.error.into_extern(),
            backtrace: self.backtrace.into_iter().map(|f| f.to_extern()).collect(),
        }
    }
}

#[derive(Debug)]
pub struct ExternVmError {
    pub error: ExternError,
    pub backtrace: Vec<ExternBacktraceFrame>,
}

impl StdError for ExternVmError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.error.source()
    }
}

impl fmt::Display for ExternVmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.error {
            ExternError::Script(script_error) => writeln!(f, "script error: {script_error}")?,
            ExternError::Runtime(runtime_error) => {
                // Try not to print huge stacks of stack traces, check to see if the causal error
                // is an `ExternVmError` and walk the chain until we get something that is not
                // `ExternVmError`.
                let mut runtime_error = runtime_error;
                while let Some(extern_error) = runtime_error.downcast_ref::<ExternVmError>() {
                    if let ExternError::Runtime(rte) = &extern_error.error {
                        runtime_error = rte;
                    } else {
                        break;
                    }
                }
                writeln!(f, "runtime error: {runtime_error}")?;
            }
        }

        write!(f, "VM backtrace:")?;
        for (i, frame) in self.backtrace.iter().rev().enumerate() {
            writeln!(f)?;
            write!(f, "{:>4}: ", i)?;
            match frame {
                ExternBacktraceFrame::Closure(closure_frame) => {
                    write!(
                        f,
                        "{}:{}",
                        closure_frame.chunk_name, closure_frame.line_number
                    )?;
                }
                ExternBacktraceFrame::Callback(callback) => {
                    write!(f, "<callback {:p}>", callback)?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(Gc<'gc, ThreadInner<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
pub struct ThreadState<'gc> {
    frames: Vec<Frame<'gc>>,
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
                frames: Vec::new(),
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

    /// Calls `Thread::run` with `this` set to `ctx.globals()` and `other` set to
    /// `Value::Undefined`.
    ///
    /// This is a convenience method for calling a closure using `Thread::with_exec` for top-level
    /// closures which are not expected to take parameters or return values.
    pub fn run(self, ctx: Context<'gc>, closure: Closure<'gc>) -> Result<(), ExternVmError> {
        self.run_with(ctx, closure, ctx.globals(), Value::Undefined)
    }

    /// Run a closure on this `Thread` with the given values of `this` and `other`.
    ///
    /// This is a convenience method for calling a closure using `Thread::with_exec` for top-level
    /// closures which are not expected to take parameters or return values.
    pub fn run_with(
        self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        this: impl Into<Value<'gc>>,
        other: impl Into<Value<'gc>>,
    ) -> Result<(), ExternVmError> {
        self.with_state(&ctx, |state| {
            state
                .call(ctx, closure, this.into(), other.into(), 0)
                .map_err(VmError::into_extern)
        })
    }

    /// Create a top-level [`Execution`] context outside of a callback.
    ///
    /// The provided `Execution` will have `this` set to `ctx.globals()` and `other` set to
    /// `Value::Undefined` by default.
    pub fn with_exec<R>(self, ctx: Context<'gc>, f: impl FnOnce(Execution<'gc, '_>) -> R) -> R {
        self.with_state(&ctx, |state| {
            f(Execution {
                thread: state,
                stack_bottom: 0,
                this: ctx.globals().into(),
                other: Value::Undefined,
            })
        })
    }

    fn with_state<R>(self, mc: &Mutation<'gc>, f: impl FnOnce(&mut ThreadState<'gc>) -> R) -> R {
        let mut thread = self.0.try_borrow_mut(mc).expect("thread locked");
        assert!(
            thread.frames.is_empty()
                && thread.registers.is_empty()
                && thread.stack.is_empty()
                && thread.heap.is_empty()
        );

        struct DropGuard<'gc, 'a>(&'a mut ThreadState<'gc>);

        impl<'gc, 'a> Drop for DropGuard<'gc, 'a> {
            fn drop(&mut self) {
                self.0.frames.clear();
                self.0.registers.clear();
                self.0.stack.clear();
                self.0.heap.clear();
            }
        }

        let guard = DropGuard(&mut thread);
        f(guard.0)
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

    /// Return a new execution context with a stack starting at the new provided bottom value.
    #[inline]
    pub fn with_stack_bottom(&mut self, stack_bottom: usize) -> Execution<'gc, '_> {
        assert!(self.thread.stack.len() >= self.stack_bottom + stack_bottom);
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom + stack_bottom,
            this: self.this,
            other: self.other,
        }
    }

    /// Return a new execution context with the `this` value set to the one provided, and the
    /// `other` value set as the previous `this` value.
    #[inline]
    pub fn with_this(&mut self, this: impl Into<Value<'gc>>) -> Execution<'gc, '_> {
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom,
            this: this.into(),
            other: self.this,
        }
    }

    /// Return a new execution context with the `this` and `other` values set to the ones provided.
    #[inline]
    pub fn with_this_other(
        &mut self,
        this: impl Into<Value<'gc>>,
        other: impl Into<Value<'gc>>,
    ) -> Execution<'gc, '_> {
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom,
            this: this.into(),
            other: other.into(),
        }
    }

    /// Return a new, unmodified `Execution` which borrows from this one.
    #[inline]
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
    #[inline]
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
    #[inline]
    pub fn call(&mut self, ctx: Context<'gc>, function: Function<'gc>) -> Result<(), RuntimeError> {
        match function {
            Function::Closure(closure) => Ok(self
                .call_closure(ctx, closure)
                .map_err(|e| e.into_extern())?),
            Function::Callback(callback) => {
                self.thread.frames.push(Frame::Callback(callback));
                let res = callback.call(ctx, self.reborrow());
                assert!(matches!(self.thread.frames.pop(), Some(Frame::Callback(_))));
                res
            }
        }
    }

    /// Returns the current execution frame depth.
    ///
    /// Every function call, both normal script closures and Rust callbacks, increase the frame
    /// depth by 1.
    ///
    /// This will always be at least 1 for the callback currently executing.
    #[inline]
    pub fn frame_depth(&self) -> usize {
        self.thread.frames.len()
    }

    /// Return a descriptor for this frame or an upper frame.
    ///
    /// The index 0 will return *this* frame, which will always be a callback frame.
    ///
    /// Any higher index will return upper frames, starting with the immediate caller and ending
    /// with the top-level executing frame.
    ///
    /// # Panics
    ///
    /// Panics if given an index that is larger than the return value of [`Execution::frame_depth`].
    #[inline]
    pub fn upper_frame(&self, index: usize) -> BacktraceFrame<'gc> {
        assert!(index < self.thread.frames.len());
        self.thread.frames[self.thread.frames.len() - 1 - index].backtrace_frame()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ClosureBacktraceFrame<'gc> {
    pub closure: Closure<'gc>,
    pub instruction: usize,
}

impl<'gc> ClosureBacktraceFrame<'gc> {
    pub fn chunk_name(&self) -> &SharedStr {
        self.closure.prototype().chunk().name()
    }

    pub fn line_number(&self) -> LineNumber {
        let chunk = self.closure.prototype().chunk();
        let prototype = self.closure.prototype();
        let bytecode = prototype.bytecode();
        let span = bytecode.span(self.instruction);
        chunk.line_number(span.start())
    }

    pub fn to_extern(&self) -> ExternClosureBacktraceFrame {
        ExternClosureBacktraceFrame {
            closure: RawGc::new(self.closure.into_inner()),
            instruction: self.instruction,
            line_number: self.line_number(),
            chunk_name: self.chunk_name().clone(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum BacktraceFrame<'gc> {
    Closure(ClosureBacktraceFrame<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> BacktraceFrame<'gc> {
    pub fn to_extern(&self) -> ExternBacktraceFrame {
        match self {
            BacktraceFrame::Closure(closure_backtrace_frame) => {
                ExternBacktraceFrame::Closure(closure_backtrace_frame.to_extern())
            }
            BacktraceFrame::Callback(callback) => {
                ExternBacktraceFrame::Callback(RawGc::new(callback.into_inner()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExternClosureBacktraceFrame {
    pub closure: RawGc,
    pub instruction: usize,
    pub line_number: LineNumber,
    pub chunk_name: SharedStr,
}

#[derive(Debug, Clone)]
pub enum ExternBacktraceFrame {
    Closure(ExternClosureBacktraceFrame),
    Callback(RawGc),
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub(super) enum OwnedHeapVar<'gc> {
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
    pub(super) fn unique(value: Value<'gc>) -> Self {
        Self::Unique(value)
    }

    #[inline]
    pub(super) fn get(&self) -> Value<'gc> {
        match self {
            OwnedHeapVar::Unique(v) => *v,
            OwnedHeapVar::Shared(v) => v.get(),
        }
    }

    #[inline]
    pub(super) fn set(&mut self, mc: &Mutation<'gc>, value: Value<'gc>) {
        match self {
            OwnedHeapVar::Unique(v) => *v = value,
            OwnedHeapVar::Shared(v) => v.set(mc, value),
        }
    }

    #[inline]
    pub(super) fn make_shared(&mut self, mc: &Mutation<'gc>) -> SharedValue<'gc> {
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

#[derive(Collect)]
#[collect(no_drop)]
struct ClosureFrame<'gc> {
    closure: Closure<'gc>,
    this: Value<'gc>,
    other: Value<'gc>,
    register_bottom: usize,
    stack_bottom: usize,
    heap_bottom: usize,
    dispatcher: instructions::Dispatcher<'gc>,
}

#[derive(Collect)]
#[collect(no_drop)]
enum Frame<'gc> {
    Closure(ClosureFrame<'gc>),
    Callback(Callback<'gc>),
}

impl<'gc> Frame<'gc> {
    fn backtrace_frame(&self) -> BacktraceFrame<'gc> {
        match self {
            Frame::Closure(script_frame) => BacktraceFrame::Closure(ClosureBacktraceFrame {
                closure: script_frame.closure,
                instruction: script_frame.dispatcher.instruction_index().unwrap(),
            }),
            &Frame::Callback(callback) => BacktraceFrame::Callback(callback),
        }
    }
}

impl<'gc> ThreadState<'gc> {
    fn call(
        &mut self,
        ctx: Context<'gc>,
        initial_closure: Closure<'gc>,
        mut initial_this: Value<'gc>,
        mut initial_other: Value<'gc>,
        initial_stack_bottom: usize,
    ) -> Result<(), VmError<'gc>> {
        let bottom_frame = self.frames.len();

        if !initial_closure.this().is_undefined() {
            initial_other = initial_this;
            initial_this = initial_closure.this();
        }

        self.frames.push({
            let register_bottom = self.registers.len();
            // Registers are resized at the beginning of the bytecode dispatch.

            let heap_bottom = self.heap.len();
            self.heap.resize_with(
                heap_bottom + initial_closure.prototype().owned_heap(),
                || OwnedHeapVar::unique(Value::Undefined),
            );

            Frame::Closure(ClosureFrame {
                closure: initial_closure,
                this: initial_this,
                other: initial_other,
                register_bottom,
                stack_bottom: initial_stack_bottom,
                heap_bottom,
                dispatcher: instructions::Dispatcher::new(
                    initial_closure.prototype().bytecode(),
                    0,
                ),
            })
        });

        loop {
            let Frame::Closure(frame) = self.frames.last_mut().unwrap() else {
                unreachable!()
            };

            // For speed, the slice of registers is always 256 wide to avoid bounds checks, and
            // we try to resize the vector the absolute *minimal* amount between script calls and
            // returns.
            //
            // On a call, the next frames `register_bottom` value is set to the calling frame's
            // `register_bottom` value plus the `used_registers` for the calling prototype. At the
            // beginning of the next loop (right below), the register vector is resized to be 256
            // above the new bottom. After a return, the registers vector is resized to be 256 above
            // the *previous* `register_bottom`.
            //
            // In this way, there is always the expected slice of 256 registers for the top script
            // frame. Additionally, the amount that the registers vector is resized is minimal:
            // it is only grown by the `used_registers` value on a call and it is only shrunk by
            // the `used_registers` value on a return, and the `used_registers` value is usually
            // small, especially for small functions.
            //
            // The sliding register slice for frames will have overlap, so garbage may be left
            // in the calling frame's register slice when the called frame returns. This will be
            // important once coroutines are added, so to make sure minimal GC values are kept alive
            // by a suspended thread, the registers vector should be truncated to the suspending
            // frame's `register_bottom` plus the `used_registers` value on yield.
            //
            // The performance impact of not aggressively truncating and growing the registers
            // vector (or equivalently just setting the overlapping slice to `Value::Undefined`) is
            // *incredible* for lots of calls of small functions, so the slight added complexity is
            // worth it.
            self.registers
                .resize(frame.register_bottom + 256, Value::Undefined);

            let registers = (&mut self.registers
                [frame.register_bottom..frame.register_bottom + 256])
                .try_into()
                .unwrap();
            let heap = &mut self.heap[frame.heap_bottom..];
            let stack = Stack::new(&mut self.stack, frame.stack_bottom);
            let ret = frame.dispatcher.dispatch_loop(&mut dispatch::Dispatch::new(
                ctx,
                frame.closure,
                &mut frame.this,
                &mut frame.other,
                registers,
                heap,
                stack,
            ));

            let mut error = None;
            match ret {
                Ok(next) => match next {
                    dispatch::Next::Call {
                        function,
                        args_bottom,
                    } => {
                        match function {
                            Function::Closure(closure) => {
                                let mut this = frame.this;
                                let mut other = frame.other;
                                if !closure.this().is_undefined() {
                                    other = this;
                                    this = closure.this();
                                }

                                // We only need to preserve the registers that the prototype claims
                                // to use, so resize the registers vec to be 256 above the registers
                                // we are preserving.
                                debug_assert!(closure.prototype().used_registers() <= 256);
                                let register_bottom = frame.register_bottom
                                    + frame.closure.prototype().used_registers();

                                let stack_bottom = frame.stack_bottom + args_bottom;

                                let heap_bottom = self.heap.len();
                                self.heap.resize_with(
                                    heap_bottom + closure.prototype().owned_heap(),
                                    || OwnedHeapVar::unique(Value::Undefined),
                                );

                                self.frames.push(Frame::Closure(ClosureFrame {
                                    closure,
                                    this,
                                    other,
                                    register_bottom,
                                    stack_bottom,
                                    heap_bottom,
                                    dispatcher: instructions::Dispatcher::new(
                                        closure.prototype().bytecode(),
                                        0,
                                    ),
                                }));
                            }
                            Function::Callback(callback) => {
                                let stack_bottom = frame.stack_bottom + args_bottom;
                                let this = frame.this;
                                let other = frame.other;
                                self.frames.push(Frame::Callback(callback));
                                let execution = Execution {
                                    thread: self,
                                    stack_bottom,
                                    this,
                                    other,
                                };
                                if let Err(err) = callback.call(ctx, execution) {
                                    error = Some(err.into());
                                }
                                assert!(matches!(self.frames.pop(), Some(Frame::Callback(_))));
                            }
                        }
                    }
                    dispatch::Next::Return { returns_bottom } => {
                        // The registers vector will be resized at the beginning of the next loop to
                        // be 256 above the lower frame's `register_bottom`.

                        // Drain everything on the stack up until the returns.
                        self.stack
                            .drain(frame.stack_bottom..frame.stack_bottom + returns_bottom);

                        // Clear the heap values for this frame.
                        self.heap.truncate(frame.heap_bottom);

                        // Pop the returning frame.
                        self.frames.pop().unwrap();

                        // If we are now below our initial frame, then return.
                        if self.frames.len() == bottom_frame {
                            return Ok(());
                        }
                    }
                },
                Err(err) => {
                    error = Some(err);
                }
            }

            if let Some(err) = error {
                let backtrace = self.frames.iter().map(|f| f.backtrace_frame()).collect();
                self.frames.truncate(bottom_frame);

                return Err(VmError {
                    error: err,
                    backtrace,
                });
            }
        }
    }
}
