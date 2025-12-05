use gc_arena::{
    Collect, Gc, Lock, Mutation, RefLock,
    collect::{DynCollect, dyn_collect},
};

use crate::{
    callback::Callback,
    closure::{Closure, SharedValue},
    error::Error,
    error::RuntimeError,
    instructions,
    interpreter::Context,
    stack::Stack,
    thread::{
        dispatch,
        error::{BacktraceFrame, CallError, ClosureBacktraceFrame},
    },
    value::{Function, Value},
};

use super::error::VmError;

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Thread<'gc>(Gc<'gc, ThreadInner<'gc>>);

pub type ThreadInner<'gc> = RefLock<ThreadState<'gc>>;

#[derive(Collect)]
#[collect(no_drop)]
pub struct ThreadState<'gc> {
    frames: Vec<Frame<'gc>>,
    registers: Vec<Value<'gc>>,
    stack: Vec<Value<'gc>>,
    heap: Vec<OwnedHeapVar<'gc>>,
    hook_state: Option<HookState<'gc>>,
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                frames: Vec::new(),
                registers: Vec::new(),
                stack: Vec::new(),
                heap: Vec::new(),
                hook_state: None,
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

    pub fn set_hook(self, ctx: Context<'gc>, mut hook: impl Hook<'gc> + Collect<'gc> + 'gc) {
        let hook_step_next = hook.on_step_count(ctx);
        self.0.borrow_mut(&ctx).hook_state = Some(HookState {
            hook: Box::new(hook),
            hook_step_next,
            hook_step_remain: hook_step_next,
        });
    }

    pub fn clear_hook(self, mc: &Mutation<'gc>) {
        self.0.borrow_mut(mc).hook_state = None;
    }

    /// Run a function on this `Thread` with `this` set to `ctx.globals()` and `other` set to
    /// `Value::Undefined`.
    ///
    /// This is a convenience method for calling a function using `Thread::with_exec` for top-level
    /// functions which are not expected to take parameters or return values.
    pub fn run(
        self,
        ctx: Context<'gc>,
        function: impl Into<Function<'gc>>,
    ) -> Result<(), CallError> {
        self.with_exec(ctx, |mut exec| exec.call(ctx, function))
    }

    /// Run a function on this `Thread` with the given values of `this` and `other`.
    ///
    /// This is a convenience method for calling a function using `Thread::with_exec` for top-level
    /// functions which are not expected to take parameters or return values.
    pub fn run_with(
        self,
        ctx: Context<'gc>,
        function: impl Into<Function<'gc>>,
        this: impl Into<Value<'gc>>,
        other: impl Into<Value<'gc>>,
    ) -> Result<(), CallError> {
        self.with_exec(ctx, |mut exec| {
            exec.with_this_other(this, other).call(ctx, function)
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

        let guard = DropGuard(&mut *thread);
        f(guard.0)
    }
}

/// An execution context for some `Thread`.
///
/// This type is passed to all callbacks to allow them to manipulate the call stack and call
/// functions code using the calling `Thread`.
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

    #[inline]
    pub fn call_callback(
        &mut self,
        ctx: Context<'gc>,
        callback: Callback<'gc>,
    ) -> Result<(), RuntimeError> {
        self.thread.frames.push(Frame::Callback(callback));

        struct DropCallbackFrame<'gc, 'a>(Execution<'gc, 'a>);

        impl<'gc, 'a> Drop for DropCallbackFrame<'gc, 'a> {
            fn drop(&mut self) {
                assert!(matches!(
                    self.0.thread.frames.pop(),
                    Some(Frame::Callback(_))
                ));
            }
        }

        let mut drop_frame = DropCallbackFrame(self.reborrow());
        let mut this = drop_frame.0.reborrow();

        if let Some(hook_state) = &mut this.thread.hook_state {
            hook_state.hook.on_call(
                ctx,
                Backtrace {
                    frames: &this.thread.frames,
                },
            )?;
        }

        let res = callback.call(ctx, this.reborrow());

        if let Some(hook_state) = &mut this.thread.hook_state {
            hook_state.hook.on_return(
                ctx,
                Backtrace {
                    frames: &this.thread.frames,
                },
            );
        }

        res
    }

    /// Call a `Function` within a callback.
    ///
    /// Arguments to the function will be taken from the stack and returns placed back into the
    /// stack.
    ///
    /// Closure and callback errors are converted into `CallError` in a smart way appropriate for
    /// calling a function from within a callback on its calling thread. If the provided function is
    /// a callback that errors and the returned `RuntimeError` wraps a `CallError`, then the inner
    /// `CallError` will be returned. If the provided function is a closure which errors and the
    /// returned `VmError` contains a `CallError`, then the inner `CallError` will be returned
    /// with an inner VM backtrace if present, or the outer VM backtrace if not present. In this
    /// way, callbacks that call functions using `Execution::call` will not add extra layers
    /// of `CallError`, only the *innermost* errors and backtraces will be returned, and since
    /// execution took place on the same `Thread`, the backtrace will already show all outer
    /// callbacks.
    #[inline]
    pub fn call(
        &mut self,
        ctx: Context<'gc>,
        function: impl Into<Function<'gc>>,
    ) -> Result<(), CallError> {
        match function.into() {
            Function::Closure(closure) => {
                if let Err(vm_err) = self.call_closure(ctx, closure) {
                    if let Error::Runtime(rte) = &vm_err.error {
                        if let Some(call_err) = rte.downcast_ref::<CallError>() {
                            return Err(match call_err {
                                CallError::Runtime(runtime_error) => CallError::Vm {
                                    error: runtime_error.clone().into(),
                                    backtrace: vm_err
                                        .backtrace
                                        .into_iter()
                                        .map(|f| f.to_extern())
                                        .collect(),
                                },
                                CallError::Vm { .. } => call_err.clone(),
                            });
                        }
                    }

                    return Err(CallError::Vm {
                        error: vm_err.error.into_extern(),
                        backtrace: vm_err
                            .backtrace
                            .into_iter()
                            .map(|f| f.to_extern())
                            .collect(),
                    });
                }
            }
            Function::Callback(callback) => {
                let res = self.call_callback(ctx, callback);
                if let Err(err) = res {
                    if let Some(call_err) = err.downcast_ref::<CallError>() {
                        return Err(call_err.clone());
                    }
                }
            }
        }

        Ok(())
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

/// A backtrace context for some `Thread`, provided to execution hooks.
pub struct Backtrace<'gc, 'a> {
    frames: &'a [Frame<'gc>],
}

impl<'gc, 'a> Backtrace<'gc, 'a> {
    /// Returns the current execution frame depth.
    ///
    /// Every function call, both normal script closures and Rust callbacks, increase the frame
    /// depth by 1.
    ///
    /// This will always be at least 1 for the callback currently executing.
    #[inline]
    pub fn frame_depth(&self) -> usize {
        self.frames.len()
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
    pub fn frame(&self, index: usize) -> BacktraceFrame<'gc> {
        assert!(index < self.frames.len());
        self.frames[self.frames.len() - 1 - index].backtrace_frame()
    }
}

pub trait Hook<'gc>: 'gc + DynCollect<'gc> {
    /// Called whenever a [`Closure`] or [`Callback`] is called using the owning [`Thread`].
    ///
    /// At the time of call, the frame for the callee will be newly pushed onto the frame stack, so
    /// calling `backtrace.upper_frame(0)` will return the function that has just been called.
    fn on_call(
        &mut self,
        _ctx: Context<'gc>,
        _backtrace: Backtrace<'gc, '_>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }

    /// Called whenever a closure or callback returns using the owning [`Thread`].
    ///
    /// At the time of call, the frame for the returning function will still be on the frame stack,
    /// so calling `backtrace.upper_frame(0)` will return the function that has just returned.
    ///
    /// This function will be called unconditionally whenever a frame is popped, *even* when the
    /// returning frame is unwinding due to an error.
    ///
    /// This thread hook *cannot* generate synthetic runtime errors because it is too confusing: if
    /// it were allowed to generate an error and did so, the same hook still must be called after
    /// this repeatedly for every upper unwinding frame.
    fn on_return(&mut self, _ctx: Context<'gc>, _backtrace: Backtrace<'gc, '_>) {}

    /// If this method returns a non-zero N, then every [`Hook::on_step`] will be called every
    /// N VM instructions.
    ///
    /// This counter is kept between calls and returns, even totally independent thread calls. The
    /// `on_step_count` method itself is called when the hook implementation is set, as well as
    /// immediately after every call to `on_step`.
    fn on_step_count(&mut self, _ctx: Context<'gc>) -> u32 {
        0
    }

    /// Called every N VM instructions, where N is the value returned from the last call to
    /// [`Hook::on_step_count`].
    fn on_step(
        &mut self,
        _ctx: Context<'gc>,
        _backtrace: Backtrace<'gc, '_>,
    ) -> Result<(), RuntimeError> {
        Ok(())
    }
}

dyn_collect!(dyn Hook<'gc>);

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
struct HookState<'gc> {
    hook: Box<dyn Hook<'gc>>,
    hook_step_next: u32,
    hook_step_remain: u32,
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
                instruction: script_frame.dispatcher.instruction_index(),
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

        if let Some(hook_state) = &mut self.hook_state {
            if let Err(err) = hook_state.hook.on_call(
                ctx,
                Backtrace {
                    frames: &self.frames,
                },
            ) {
                let backtrace = self.frames.iter().map(|f| f.backtrace_frame()).collect();
                self.frames.truncate(bottom_frame);

                return Err(VmError {
                    error: err.into(),
                    backtrace,
                }
                .into());
            }
        }

        let err = loop {
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
            // frame. Additionally, the amount that the registers vector is resized is minimal: it
            // is only grown by the `used_registers` value on a call and it is only shrunk by the
            // `used_registers` value on a return, and the `used_registers` value is usually small,
            // especially for small functions.
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
            let mut dispatch = dispatch::Dispatch::new(
                ctx,
                frame.closure,
                &mut frame.this,
                &mut frame.other,
                registers,
                heap,
                stack,
            );

            let next = if let Some(hook_state) = &mut self.hook_state
                && hook_state.hook_step_next != 0
            {
                match frame
                    .dispatcher
                    .dispatch_count(&mut dispatch, hook_state.hook_step_remain)
                {
                    Some((res, count)) => {
                        hook_state.hook_step_remain = count;
                        match res {
                            Ok(next) => Some(next),
                            Err(err) => break err,
                        }
                    }
                    None => None,
                }
            } else {
                match frame.dispatcher.dispatch_loop(&mut dispatch) {
                    Ok(next) => Some(next),
                    Err(err) => break err,
                }
            };

            match next {
                Some(next) => match next {
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

                                if let Some(hook_state) = &mut self.hook_state {
                                    if let Err(err) = hook_state.hook.on_call(
                                        ctx,
                                        Backtrace {
                                            frames: &self.frames,
                                        },
                                    ) {
                                        break err.into();
                                    }
                                }
                            }
                            Function::Callback(callback) => {
                                let stack_bottom = frame.stack_bottom + args_bottom;
                                let this = frame.this;
                                let other = frame.other;

                                if let Err(err) = (Execution {
                                    thread: self,
                                    stack_bottom,
                                    this,
                                    other,
                                })
                                .call_callback(ctx, callback)
                                {
                                    break err.into();
                                }
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

                        if let Some(hook_state) = &mut self.hook_state {
                            hook_state.hook.on_return(
                                ctx,
                                Backtrace {
                                    frames: &self.frames,
                                },
                            );
                        }

                        // Pop the returning frame.
                        self.frames.pop().unwrap();

                        // If we have returned from our initial frame, then we can stop executing.
                        if self.frames.len() == bottom_frame {
                            return Ok(());
                        }
                    }
                },
                None => {
                    let hook_state = self.hook_state.as_mut().unwrap();
                    if let Err(err) = hook_state.hook.on_step(
                        ctx,
                        Backtrace {
                            frames: &self.frames,
                        },
                    ) {
                        break err.into();
                    }
                    hook_state.hook_step_next = hook_state.hook.on_step_count(ctx);
                    hook_state.hook_step_remain = hook_state.hook_step_next;
                }
            }
        };

        let backtrace = self.frames.iter().map(|f| f.backtrace_frame()).collect();

        if let Some(hook_state) = &mut self.hook_state {
            while self.frames.len() > bottom_frame {
                hook_state.hook.on_return(
                    ctx,
                    Backtrace {
                        frames: &self.frames,
                    },
                );
                self.frames.pop();
            }
        } else {
            self.frames.truncate(bottom_frame);
        }

        Err(VmError {
            error: err,
            backtrace,
        }
        .into())
    }
}
