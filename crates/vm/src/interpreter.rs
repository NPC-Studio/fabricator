use std::ops;

use gc_arena::{arena::Root, Arena, Collect, Gc, Mutation, Rootable};

use crate::{
    callback::Callback,
    magic::MagicSet,
    object::Object,
    registry::{Registry, Singleton},
    stash::{Fetchable, Stashable},
    string::String,
    value::Value,
};

#[derive(Copy, Clone)]
pub struct Context<'gc> {
    mutation: &'gc Mutation<'gc>,
    state: &'gc State<'gc>,
}

impl<'gc> ops::Deref for Context<'gc> {
    type Target = Mutation<'gc>;

    fn deref(&self) -> &Self::Target {
        self.mutation
    }
}

impl<'gc> Context<'gc> {
    /// Get a reference to [`Mutation`] (the `gc-arena` mutation handle) out of the `Context`
    /// object.
    ///
    /// This can also be done automatically with [`ops::Deref`] coercion.
    pub fn mutation(self) -> &'gc Mutation<'gc> {
        self.mutation
    }

    pub fn stdlib(self) -> Gc<'gc, MagicSet<'gc>> {
        self.state.stdlib
    }

    pub fn globals(self) -> Object<'gc> {
        self.state.globals
    }

    pub fn registry(self) -> Registry<'gc> {
        self.state.registry
    }

    /// Calls `ctx.registry().singleton::<S>(ctx)`.
    pub fn singleton<S>(self) -> &'gc Root<'gc, S>
    where
        S: for<'a> Rootable<'a> + 'static,
        Root<'gc, S>: Sized + Singleton<'gc> + Collect<'gc>,
    {
        self.state.registry.singleton::<S>(self)
    }

    /// Calls `ctx.registry().stash(ctx, s)`.
    pub fn stash<S: Stashable<'gc>>(self, s: S) -> S::Stashed {
        self.state.registry.stash(&self, s)
    }

    /// Calls `ctx.registry().fetch(f)`.
    pub fn fetch<F: Fetchable + Fetchable>(self, f: &F) -> F::Fetched<'gc> {
        self.state.registry.fetch(f)
    }
}

pub struct Interpreter {
    arena: Arena<Rootable![State<'_>]>,
}

impl Interpreter {
    // Create a new `Interpreter` instance with no stdlib loaded.
    pub fn empty() -> Self {
        Self {
            arena: Arena::<Rootable![State<'_>]>::new(|mc| State::new(mc)),
        }
    }

    /// Create a new `Interpreter` instance with a small testing stdlib loaded.
    pub fn testing() -> Self {
        let mut this = Self::empty();

        this.arena.mutate_root(|mc, root| {
            let mut stdlib = MagicSet::new();

            let assert = Callback::from_fn(mc, |_, _, stack| {
                for i in 0..stack.len() {
                    if !stack.get(i).to_bool() {
                        return Err("assert failed".into());
                    }
                }
                Ok(())
            });
            stdlib
                .add_constant(mc, String::new(mc, "assert"), assert.into())
                .unwrap();

            let print = Callback::from_fn(mc, |_, _, stack| {
                for i in 0..stack.len() {
                    print!("{:?}", stack.get(i));
                    if i != stack.len() - 1 {
                        print!("\t");
                    }
                }
                println!();
                Ok(())
            });
            stdlib
                .add_constant(mc, String::new(mc, "print"), print.into())
                .unwrap();

            let method = Callback::from_fn(mc, |ctx, _, mut stack| {
                let Some(func) = stack.get(1).to_function() else {
                    return Err("`method` must be called on a callback or closure".into());
                };

                match stack.get(0) {
                    obj @ (Value::Undefined | Value::Object(_) | Value::UserData(_)) => {
                        stack.clear();
                        stack.push_back(func.rebind(&ctx, obj).into());
                        Ok(())
                    }
                    _ => {
                        Err("`method` self value must be an object, userdata, or undefined".into())
                    }
                }
            });
            stdlib
                .add_constant(mc, String::new(mc, "method"), method.into())
                .unwrap();

            let black_box = Callback::from_fn(mc, |_, _, _| Ok(()));
            stdlib
                .add_constant(mc, String::new(mc, "black_box"), black_box.into())
                .unwrap();

            root.stdlib = Gc::new(mc, stdlib);
        });

        this
    }

    pub fn enter<F, T>(&mut self, f: F) -> T
    where
        F: for<'gc> FnOnce(Context<'gc>) -> T,
    {
        const COLLECTOR_GRANULARITY: f64 = 1024.0;

        let r = self.arena.mutate(move |mc, state| f(state.ctx(mc)));
        if self.arena.metrics().allocation_debt() > COLLECTOR_GRANULARITY {
            self.arena.collect_debt();
        }
        r
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
struct State<'gc> {
    stdlib: Gc<'gc, MagicSet<'gc>>,
    globals: Object<'gc>,
    registry: Registry<'gc>,
}

impl<'gc> State<'gc> {
    fn new(mc: &Mutation<'gc>) -> State<'gc> {
        Self {
            stdlib: Gc::new(mc, MagicSet::default()),
            globals: Object::new(mc),
            registry: Registry::new(mc),
        }
    }

    fn ctx(&'gc self, mutation: &'gc Mutation<'gc>) -> Context<'gc> {
        Context {
            mutation,
            state: self,
        }
    }
}
