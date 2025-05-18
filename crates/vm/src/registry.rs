use std::{any::TypeId, collections::HashMap};

use gc_arena::{Collect, DynamicRootSet, Gc, Mutation, RefLock, Rootable, arena::Root};

use crate::{
    any::Any,
    interpreter::Context,
    stash::{Fetchable, Stashable},
};

/// A type which can have a single registered value per `Interpreter` instance.
pub trait Singleton<'gc> {
    fn create(ctx: Context<'gc>) -> Self;
}

impl<'gc, T: Default> Singleton<'gc> for T {
    fn create(_: Context<'gc>) -> Self {
        Self::default()
    }
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Registry<'gc> {
    roots: DynamicRootSet<'gc>,
    singletons: Gc<'gc, RefLock<HashMap<TypeId, Option<Any<'gc>>>>>,
}

impl<'gc> Registry<'gc> {
    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            roots: DynamicRootSet::new(mc),
            singletons: Gc::new(mc, RefLock::new(HashMap::new())),
        }
    }

    /// Create an instance of a type that exists at most once per `Interpreter` instance.
    ///
    /// If the type has already been created, returns the already created instance, otherwise calls
    /// `S::create` to create a new instance and returns it.
    ///
    /// # Panics
    ///
    /// Singletons may depend on each other, however if a called [`Singleton::create`] method tries
    /// to (either directly or indirectly) access the singleton it is creating, this will result in
    /// a panic.
    pub fn singleton<S>(&self, ctx: Context<'gc>) -> &'gc Root<'gc, S>
    where
        S: for<'a> Rootable<'a> + 'static,
        Root<'gc, S>: Sized + Singleton<'gc> + Collect<'gc>,
    {
        let type_id = TypeId::of::<S>();
        match self.singletons.borrow().get(&type_id) {
            Some(Some(singleton)) => return singleton.downcast::<S>().unwrap(),
            Some(None) => panic!("singleton creation depends on itself"),
            None => {}
        }

        // Insert a marker `None` value to guard against recursive dependencies.
        self.singletons.borrow_mut(&ctx).insert(type_id, None);

        // Don't hold the singletons lock during creation to allow singletons do depend on each
        // other.
        let v = Root::<'gc, S>::create(ctx);

        let mut singletons = self.singletons.borrow_mut(&ctx);
        let any = Any::new::<S>(&ctx, v);
        *singletons.get_mut(&type_id).unwrap() = Some(any);
        any.downcast::<S>().unwrap()
    }

    /// Returns the inner [`DynamicRootSet`] held inside the global registry.
    ///
    /// This can be used to create `'static` roots directly without having to deal with the
    /// [`Stashable`] trait.
    pub fn roots(&self) -> DynamicRootSet<'gc> {
        self.roots
    }

    /// "Stash" a value with a `'gc` branding lifetime in the global registry, creating a `'static`
    /// handle to it.
    ///
    /// This works for any type that implements the [`Stashable`] trait.
    ///
    /// Values stashed in the global registry produce handles without the `'gc` lifetime branding,
    /// which makes them completely unrestricted. They are `'static` Rust types, which means that
    /// the borrow checker will not stop you from storing them *anywhere*, including within the
    /// `Interpreter` state itself. Do not do this!
    ///
    /// Registry stashed values are not meant to be held within the `Interpreter` state. Stashed
    /// handles are not traced like normal GC types and do not have full cycle collection, so any
    /// stashed value will only be freed when the returned handle is *dropped*. This means that if
    /// there is a cycle through a stashed handle (e.g. the stashed handle points to a fabricator
    /// value which in turn directly or indirectly points to the handle), the handle will never be
    /// dropped so the value (and anything it transitively points to) can never be freed.
    ///
    /// Values stashed in the registry are designed to be held *completely outside* of the
    /// `Interpreter` state by outer Rust code. If storing a value inside the interpreter state,
    /// always use a proper garbage collected type, which in addition to allowing full cycle
    /// collection will also be cheaper.
    pub fn stash<S: Stashable<'gc>>(&self, mc: &Mutation<'gc>, s: S) -> S::Stashed {
        s.stash(mc, self.roots)
    }

    /// "Fetch" the real value for a handle that has been returned from `Registry::stash`.
    ///
    /// It can be implemented for external types by implementing the `Fetchable` trait.
    ///
    /// # Panics
    ///
    /// If the given handle was not stashed using the global registry for *this* `Interpreter`
    /// instance, then this method will panic.
    pub fn fetch<F: Fetchable>(&self, f: &F) -> F::Fetched<'gc> {
        f.fetch(self.roots)
    }
}
