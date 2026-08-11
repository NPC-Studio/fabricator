use gc_arena::{
    Collect, Gc, Lock, Mutation, RefLock,
    collect::{DynCollect, dyn_collect},
};

use crate::{
    callback::Callback,
    closure::{Closure, SharedValue},
    error::{Error, RuntimeError},
    instructions,
    interpreter::Context,
    thread::{
        dispatch,
        error::{BacktraceFrame, CallError, ClosureBacktraceFrame},
        stack::Stack,
        vec_end_slice::VecEndSlice,
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
    registers: RegisterVec<'gc>,
    stack: Vec<Value<'gc>>,
    stack_frame_boundaries: Vec<usize>,
    this: Vec<Value<'gc>>,
    heap: Vec<OwnedHeapVar<'gc>>,
    hook_state: Option<HookState<'gc>>,
}

impl<'gc> Thread<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Thread<'gc> {
        Thread(Gc::new(
            mc,
            RefLock::new(ThreadState {
                frames: Vec::new(),
                registers: RegisterVec::default(),
                stack: Vec::new(),
                stack_frame_boundaries: Vec::new(),
                this: Vec::new(),
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

    pub fn set_hook(self, ctx: Context<'gc>, hook: impl Hook<'gc> + Collect<'gc> + 'gc) {
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

    /// Run a function on this `Thread` and discard all return values.
    pub fn run(
        self,
        ctx: Context<'gc>,
        function: impl Into<Function<'gc>>,
    ) -> Result<(), CallError> {
        self.exec(ctx, |mut exec| exec.call(ctx, function))
    }

    /// Run a function on this `Thread` with the given value of `this` and discard all return
    /// values.
    pub fn run_with(
        self,
        ctx: Context<'gc>,
        function: impl Into<Function<'gc>>,
        this: impl Into<Value<'gc>>,
    ) -> Result<(), CallError> {
        self.exec(ctx, |mut exec| exec.with_this(this).call(ctx, function))
    }

    /// Create a top-level [`Execution`] context outside of a callback.
    pub fn exec<R>(self, ctx: Context<'gc>, f: impl FnOnce(Execution<'gc, '_>) -> R) -> R {
        self.enter_state(&ctx, |state| {
            f(Execution {
                thread: state,
                stack_bottom: 0,
                this_bottom: 0,
            })
        })
    }

    fn enter_state<R>(self, mc: &Mutation<'gc>, f: impl FnOnce(&mut ThreadState<'gc>) -> R) -> R {
        let mut thread = self.0.try_borrow_mut(mc).expect("thread locked");
        assert!(
            thread.frames.is_empty()
                && thread.registers.len() == 0
                && thread.stack.is_empty()
                && thread.stack_frame_boundaries.is_empty()
                && thread.this.is_empty()
                && thread.heap.is_empty(),
            "cannot enter thread state, thread is poisoned"
        );

        let ret = f(&mut *thread);

        thread.registers.clear();
        thread.stack.clear();

        assert!(thread.frames.is_empty());
        assert!(thread.stack_frame_boundaries.is_empty());
        assert!(thread.this.is_empty());
        assert!(thread.heap.is_empty());

        ret
    }
}

/// An execution context for some `Thread`.
///
/// This type is passed to all callbacks to allow them to manipulate the call stack and call
/// functions code using the calling `Thread`.
pub struct Execution<'gc, 'a> {
    thread: &'a mut ThreadState<'gc>,
    stack_bottom: usize,
    this_bottom: usize,
}

impl<'gc, 'a> Drop for Execution<'gc, 'a> {
    fn drop(&mut self) {
        self.thread.this.truncate(self.this_bottom);
    }
}

impl<'gc, 'a> Execution<'gc, 'a> {
    /// Return a slice of the current call stack containing callback arguments and returns.
    #[inline]
    pub fn stack(&mut self) -> Stack<'gc, '_> {
        Stack::new(&mut self.thread.stack, self.stack_bottom)
    }

    /// Return the current number of *explicitly set* values on the `this` stack.
    ///
    /// There is always implicitly an unlimited number of `ctx.globals()` present below the last
    /// explicit `this` value.
    ///
    /// You can add `1` to this value to get indexes for all of the explicitly set `this` values as
    /// well as one copy of the implicit `ctx.globals()` at the bottom.
    #[inline]
    pub fn this_depth(&self) -> usize {
        self.thread.this.len()
    }

    /// Return the nth `this` value.
    ///
    /// The 0th `this` value is the topmost one, the 1th `this` value is the current value of
    /// `other`, etc.
    ///
    /// Any value out of range will always return `ctx.globals()`.
    #[inline]
    pub fn this(&self, ctx: Context<'gc>, nth: usize) -> Value<'gc> {
        self.thread
            .this
            .iter()
            .copied()
            .rev()
            .nth(nth)
            .unwrap_or(ctx.globals().into())
    }

    /// Return a new execution context with a stack starting at the new provided bottom value.
    #[track_caller]
    #[inline]
    pub fn with_stack_bottom(&mut self, stack_bottom: usize) -> Execution<'gc, '_> {
        assert!(self.thread.stack.len() >= self.stack_bottom + stack_bottom);
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom + stack_bottom,
            this_bottom: self.this_bottom,
        }
    }

    /// Return a new execution context with a new `this` value pushed from the one provided.
    ///
    /// On drop, the `this` stack will be reset to its previous state.
    #[inline]
    pub fn with_this(&mut self, this: impl Into<Value<'gc>>) -> Execution<'gc, '_> {
        let this_bottom = self.thread.this.len();
        self.thread.this.push(this.into());
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom,
            this_bottom,
        }
    }

    /// Return a new, unmodified `Execution` which borrows from this one.
    #[inline]
    pub fn reborrow(&mut self) -> Execution<'gc, '_> {
        Execution {
            thread: self.thread,
            stack_bottom: self.stack_bottom,
            this_bottom: self.this_bottom,
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
        self.thread.call(ctx, closure, self.stack_bottom)
    }

    #[inline]
    pub fn call_callback(
        &mut self,
        ctx: Context<'gc>,
        callback: Callback<'gc>,
    ) -> Result<(), RuntimeError> {
        self.thread.frames.push(Frame::Callback(callback));

        // Push the callback's bound `this` value if it has one.
        let mut exec = if let this = callback.this()
            && !this.is_undefined()
        {
            self.with_this(this)
        } else {
            self.reborrow()
        };

        if let Some(hook_state) = &mut exec.thread.hook_state {
            hook_state.hook.on_call(
                ctx,
                Backtrace {
                    frames: &exec.thread.frames,
                },
            )?;
        }

        let res = callback.call(ctx, exec.reborrow());

        if let Some(hook_state) = &mut exec.thread.hook_state {
            hook_state.hook.on_return(
                ctx,
                Backtrace {
                    frames: &exec.thread.frames,
                },
            );
        }

        drop(exec);
        assert!(matches!(self.thread.frames.pop(), Some(Frame::Callback(_))));

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

                    Err(CallError::Vm {
                        error: vm_err.error.into_extern(),
                        backtrace: vm_err
                            .backtrace
                            .into_iter()
                            .map(|f| f.to_extern())
                            .collect(),
                    })
                } else {
                    Ok(())
                }
            }
            Function::Callback(callback) => {
                let res = self.call_callback(ctx, callback);
                if let Err(err) = res {
                    if let Some(call_err) = err.downcast_ref::<CallError>() {
                        Err(call_err.clone())
                    } else {
                        Err(CallError::Runtime(err))
                    }
                } else {
                    Ok(())
                }
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
    #[track_caller]
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
    #[track_caller]
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
    fn on_step_count(&self, _ctx: Context<'gc>) -> u32 {
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
    register_bottom: usize,
    stack_bottom: usize,
    stack_frame_boundaries_bottom: usize,
    this_bottom: usize,
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
    // Call a closure with arguments starting at `stack_bottom`.
    fn call(
        &mut self,
        ctx: Context<'gc>,
        closure: Closure<'gc>,
        stack_bottom: usize,
    ) -> Result<(), VmError<'gc>> {
        let bottom_frame = self.frames.len();

        self.frames.push({
            let register_bottom = self.registers.len();
            // We only need to preserve the registers that the prototype claims to use.
            self.registers
                .set_len(register_bottom + closure.prototype().used_registers());

            let stack_frame_boundaries_bottom = self.stack_frame_boundaries.len();

            // Push the closure's bound `this` value, if it has one.
            let this_bottom = self.this.len();
            if let closure_this = closure.this()
                && !closure_this.is_undefined()
            {
                self.this.push(closure_this)
            }

            let heap_bottom = self.heap.len();
            self.heap
                .resize_with(heap_bottom + closure.prototype().owned_heap(), || {
                    OwnedHeapVar::unique(Value::Undefined)
                });

            Frame::Closure(ClosureFrame {
                closure: closure,
                register_bottom,
                stack_bottom,
                stack_frame_boundaries_bottom,
                this_bottom,
                heap_bottom,
                dispatcher: instructions::Dispatcher::new(closure.prototype().bytecode(), 0),
            })
        });

        fn unwind_closure_frame<'gc>(this: &mut ThreadState<'gc>, ctx: Context<'gc>) {
            if let Some(hook_state) = &mut this.hook_state {
                hook_state.hook.on_return(
                    ctx,
                    Backtrace {
                        frames: &this.frames,
                    },
                );
            }

            match this.frames.pop().unwrap() {
                Frame::Closure(closure_frame) => {
                    this.registers.set_len(closure_frame.register_bottom);
                    this.stack.truncate(closure_frame.stack_bottom);
                    this.stack_frame_boundaries
                        .truncate(closure_frame.stack_frame_boundaries_bottom);
                    this.this.truncate(closure_frame.this_bottom);
                    this.heap.truncate(closure_frame.heap_bottom);
                }
                _ => panic!("not a closure frame"),
            }
        }

        if let Some(hook_state) = &mut self.hook_state {
            if let Err(err) = hook_state.hook.on_call(
                ctx,
                Backtrace {
                    frames: &self.frames,
                },
            ) {
                let backtrace = self.frames.iter().map(|f| f.backtrace_frame()).collect();
                unwind_closure_frame(self, ctx);

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

            let registers = self.registers.frame(frame.register_bottom);
            let stack = VecEndSlice::new(&mut self.stack, frame.stack_bottom);
            let stack_frame_boundaries = VecEndSlice::new(
                &mut self.stack_frame_boundaries,
                frame.stack_frame_boundaries_bottom,
            );
            let this = VecEndSlice::new(&mut self.this, frame.this_bottom);
            let heap = &mut self.heap[frame.heap_bottom..];
            let mut dispatch = dispatch::Dispatch::new(
                ctx,
                frame.closure,
                registers,
                stack,
                stack_frame_boundaries,
                this,
                heap,
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
                        this,
                    } => {
                        match function {
                            Function::Closure(closure) => {
                                let register_bottom = self.registers.len();
                                // We only need to preserve the registers that the prototype claims
                                // to use.
                                self.registers.set_len(
                                    register_bottom + closure.prototype().used_registers(),
                                );

                                let stack_bottom = frame.stack_bottom + args_bottom;

                                let stack_frame_boundaries_bottom =
                                    self.stack_frame_boundaries.len();

                                // Push the closure's bound `this` value or the provided `this` if
                                // either is defined.
                                let this_bottom = self.this.len();
                                if let this = closure.this().null_coalesce(this)
                                    && !this.is_undefined()
                                {
                                    self.this.push(this)
                                }

                                let heap_bottom = self.heap.len();
                                self.heap.resize_with(
                                    heap_bottom + closure.prototype().owned_heap(),
                                    || OwnedHeapVar::unique(Value::Undefined),
                                );

                                self.frames.push(Frame::Closure(ClosureFrame {
                                    closure,
                                    register_bottom,
                                    stack_bottom,
                                    stack_frame_boundaries_bottom,
                                    this_bottom,
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

                                // Push the provided `this` value if the callback does not have
                                // one bound, otherwise the bound `this` value will be pushed by
                                // `Execution::call_callback`.
                                let this_bottom = self.this.len();
                                if !this.is_undefined() && callback.this().is_undefined() {
                                    self.this.push(this)
                                }

                                if let Err(err) = (Execution {
                                    thread: self,
                                    stack_bottom,
                                    this_bottom,
                                })
                                .call_callback(ctx, callback)
                                {
                                    break err.into();
                                }
                            }
                        }
                    }
                    dispatch::Next::Return { returns_bottom } => {
                        // Truncate the register vec on return.
                        self.registers.set_len(frame.register_bottom);

                        // Drain everything on the stack up until the returns.
                        self.stack
                            .drain(frame.stack_bottom..frame.stack_bottom + returns_bottom);

                        // Clear any unpopped stack frames.
                        self.stack_frame_boundaries
                            .truncate(frame.stack_frame_boundaries_bottom);

                        // Clear any unpopped `this` values.
                        self.this.truncate(frame.this_bottom);

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

        while self.frames.len() > bottom_frame {
            unwind_closure_frame(self, ctx);
        }

        Err(VmError {
            error: err,
            backtrace,
        }
        .into())
    }
}

/// The register vector.
///
/// For speed, we want the slice of registers that the VM operates on to be exactly 256 wide to
/// avoid bounds checks, and we want to resize the register vector as little as absolutely possible
/// between calls and returns to avoid having to memset the entire register frame. We use this type
/// to accomplish this.
#[derive(Default, Collect)]
#[collect(no_drop)]
struct RegisterVec<'gc> {
    registers: Vec<Value<'gc>>,
    length: usize,
}

impl<'gc> RegisterVec<'gc> {
    /// Get the current logical length of the register vector.
    #[inline]
    fn len(&self) -> usize {
        self.length
    }

    /// Set the current logical length of the register vector.
    #[inline]
    fn set_len(&mut self, length: usize) {
        self.length = length;
        // Truncate to the extent of the furthest possible valid frame.
        self.registers.truncate(length + 256);
    }

    /// Request a 256 wide slice of registers somewhere in the valid range of the register vector.
    ///
    /// The `bottom` value must be less than or equal to the logical length, but the returned slice
    /// may extend beyond this in order to be exactly 256 wide.
    #[track_caller]
    #[inline]
    fn frame(&mut self, bottom: usize) -> &mut [Value<'gc>; 256] {
        assert!(bottom <= self.length);
        if self.registers.len() < bottom + 256 {
            self.registers.resize(bottom + 256, Value::Undefined);
        }
        (&mut self.registers[bottom..bottom + 256])
            .try_into()
            .unwrap()
    }

    /// Clear the vector and set the logical length to 0.
    #[inline]
    fn clear(&mut self) {
        self.registers.clear();
        self.length = 0;
    }
}
