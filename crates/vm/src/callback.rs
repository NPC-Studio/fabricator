use std::fmt;

use gc_arena::{Collect, Gc, Mutation};

use crate::{error::Error, interpreter::Context, stack::Stack, value::Value};

pub trait CallbackFn<'gc> {
    fn call(&self, ctx: Context<'gc>, this: Value<'gc>, stack: Stack<'gc, '_>)
    -> Result<(), Error>;
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct CallbackInner<'gc> {
    callback_fn: Gc<'gc, dyn CallbackFn<'gc>>,
    this: Value<'gc>,
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + Collect<'gc> + 'gc>(
        mc: &Mutation<'gc>,
        callback: C,
        this: Value<'gc>,
    ) -> Self {
        let callback_fn = gc_arena::unsize!(Gc::new(mc, callback) => dyn CallbackFn);
        Self(Gc::new(mc, CallbackInner { callback_fn, this }))
    }

    /// Call the contained callback.
    ///
    /// The provided `this` object will be used if no `this` object is bound to the callback.
    pub fn call_with(
        self,
        ctx: Context<'gc>,
        this: Value<'gc>,
        stack: Stack<'gc, '_>,
    ) -> Result<(), Error> {
        self.0.callback_fn.call(
            ctx,
            if self.0.this.is_undefined() {
                this
            } else {
                self.0.this
            },
            stack,
        )
    }

    /// Call the contained callback.
    ///
    /// If no `this` object is bound to the callback, then the `this` object will be set as
    /// `ctx.globals()`.
    pub fn call(self, ctx: Context<'gc>, stack: Stack<'gc, '_>) -> Result<(), Error> {
        self.call_with(ctx, ctx.globals().into(), stack)
    }

    /// Return a clone of this callback with the embedded `this` value changed to the provided one.
    ///
    /// If `Value::Undefined` is provided, then the bound `this` object will be removed.
    pub fn rebind(self, mc: &Mutation<'gc>, this: Value<'gc>) -> Callback<'gc> {
        Self(Gc::new(
            mc,
            CallbackInner {
                callback_fn: self.0.callback_fn,
                this,
            },
        ))
    }

    /// Returns the currently bound `this` object, if one is set.
    pub fn this(self) -> Value<'gc> {
        self.0.this
    }

    /// Create a callback from a Rust function.
    ///
    /// The function must be `'static` because Rust closures cannot implement `Collect`. If you need
    /// to associate GC data with this function, use [`Callback::from_fn_with`].
    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static + Fn(Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        Self::from_fn_with_root(mc, (), move |_, ctx, this, stack| call(ctx, this, stack))
    }

    /// Create a callback from a Rust function together with an error context.
    ///
    /// If the callback errors, the context will be added to the error.
    pub fn from_fn_with_err_ctx<C, F>(mc: &Mutation<'gc>, err_ctx: C, call: F) -> Callback<'gc>
    where
        C: fmt::Display + Clone + Send + Sync + 'static,
        F: 'static + Fn(Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        Self::from_fn_with_root_and_err_ctx(mc, (), err_ctx, move |_, ctx, this, stack| {
            call(ctx, this, stack)
        })
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with_root<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect<'gc>,
        F: 'static + Fn(&R, Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> CallbackFn<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect<'gc>,
            F: 'static + Fn(&R, Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                this: Value<'gc>,
                stack: Stack<'gc, '_>,
            ) -> Result<(), Error> {
                (self.call)(&self.root, ctx, this, stack)
            }
        }

        Callback::new(mc, RootCallback { root, call }, Value::Undefined)
    }

    /// Create a callback from a Rust function together with a GC object and an error context object.
    pub fn from_fn_with_root_and_err_ctx<R, C, F>(
        mc: &Mutation<'gc>,
        root: R,
        err_ctx: C,
        call: F,
    ) -> Callback<'gc>
    where
        R: 'gc + Collect<'gc>,
        C: fmt::Display + Clone + Send + Sync + 'static,
        F: 'static + Fn(&R, Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, C, F> {
            root: R,
            #[collect(require_static)]
            err_ctx: C,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, C, F> CallbackFn<'gc> for RootCallback<R, C, F>
        where
            R: 'gc + Collect<'gc>,
            C: fmt::Display + Clone + Send + Sync + 'static,
            F: 'static + Fn(&R, Context<'gc>, Value<'gc>, Stack<'gc, '_>) -> Result<(), Error>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                this: Value<'gc>,
                stack: Stack<'gc, '_>,
            ) -> Result<(), Error> {
                (self.call)(&self.root, ctx, this, stack)
                    .map_err(|e| e.context(self.err_ctx.clone()))
            }
        }

        Callback::new(
            mc,
            RootCallback {
                root,
                err_ctx,
                call,
            },
            Value::Undefined,
        )
    }

    pub fn from_inner(inner: Gc<'gc, CallbackInner<'gc>>) -> Self {
        Self(inner)
    }

    pub fn into_inner(self) -> Gc<'gc, CallbackInner<'gc>> {
        self.0
    }
}

impl<'gc> fmt::Debug for Callback<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Callback")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Callback<'gc> {
    fn eq(&self, other: &Callback<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Callback<'gc> {}
