use core::fmt;

use gc_arena::{DynamicRoot, DynamicRootSet, Gc, Mutation, Rootable};

use crate::{
    UserDataMethods,
    closure::{Closure, ClosureInner, Prototype},
    magic::MagicSet,
    object::{Object, ObjectInner},
    thread::{Thread, ThreadInner},
    userdata::{UserData, UserDataInner},
};

/// A trait for types that can be stashed into a [`DynamicRootSet`].
///
/// This trait is simpler to work with than having to manually specify `Rootable` projections and
/// can work with more types than just those that wrap a single `Gc` pointer.
pub trait Stashable<'gc> {
    type Stashed;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed;
}

/// A trait for types that can be fetched from a [`DynamicRootSet`].
pub trait Fetchable {
    type Fetched<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc>;
}

#[derive(Clone)]
pub struct StashedObject(DynamicRoot<Rootable![ObjectInner<'_>]>);

impl fmt::Debug for StashedObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedObject")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Object<'gc> {
    type Stashed = StashedObject;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedObject(roots.stash::<Rootable![ObjectInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedObject {
    type Fetched<'gc> = Object<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Object::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedClosure(DynamicRoot<Rootable![ClosureInner<'_>]>);

impl fmt::Debug for StashedClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedClosure")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Closure<'gc> {
    type Stashed = StashedClosure;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedClosure(roots.stash::<Rootable![ClosureInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedClosure {
    type Fetched<'gc> = Closure<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Closure::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedUserData(DynamicRoot<Rootable![UserDataInner<'_>]>);

impl fmt::Debug for StashedUserData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedUserData")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for UserData<'gc> {
    type Stashed = StashedUserData;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedUserData(roots.stash::<Rootable![UserDataInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedUserData {
    type Fetched<'gc> = UserData<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        UserData::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedUserDataMethods(DynamicRoot<Rootable!['gc => dyn UserDataMethods<'gc> + 'gc]>);

impl fmt::Debug for StashedUserDataMethods {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedUserDataMethods")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Gc<'gc, dyn UserDataMethods<'gc>> {
    type Stashed = StashedUserDataMethods;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedUserDataMethods(
            roots.stash::<Rootable!['_gc => dyn UserDataMethods<'_gc> + '_gc]>(mc, self),
        )
    }
}

impl Fetchable for StashedUserDataMethods {
    type Fetched<'gc> = Gc<'gc, dyn UserDataMethods<'gc>>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        roots.fetch(&self.0)
    }
}

#[derive(Clone)]
pub struct StashedThread(DynamicRoot<Rootable![ThreadInner<'_>]>);

impl fmt::Debug for StashedThread {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("StashedThread")
            .field(&self.0.as_ptr())
            .finish()
    }
}

impl<'gc> Stashable<'gc> for Thread<'gc> {
    type Stashed = StashedThread;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedThread(roots.stash::<Rootable![ThreadInner<'_>]>(mc, self.into_inner()))
    }
}

impl Fetchable for StashedThread {
    type Fetched<'gc> = Thread<'gc>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        Thread::from_inner(roots.fetch(&self.0))
    }
}

#[derive(Clone)]
pub struct StashedPrototype(DynamicRoot<Rootable![Prototype<'_>]>);

impl<'gc> Stashable<'gc> for Gc<'gc, Prototype<'gc>> {
    type Stashed = StashedPrototype;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedPrototype(roots.stash::<Rootable![Prototype<'_>]>(mc, self))
    }
}

impl Fetchable for StashedPrototype {
    type Fetched<'gc> = Gc<'gc, Prototype<'gc>>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        roots.fetch(&self.0)
    }
}

#[derive(Clone)]
pub struct StashedMagicSet(DynamicRoot<Rootable![MagicSet<'_>]>);

impl<'gc> Stashable<'gc> for Gc<'gc, MagicSet<'gc>> {
    type Stashed = StashedMagicSet;

    fn stash(self, mc: &Mutation<'gc>, roots: DynamicRootSet<'gc>) -> Self::Stashed {
        StashedMagicSet(roots.stash::<Rootable![MagicSet<'_>]>(mc, self))
    }
}

impl Fetchable for StashedMagicSet {
    type Fetched<'gc> = Gc<'gc, MagicSet<'gc>>;

    fn fetch<'gc>(&self, roots: DynamicRootSet<'gc>) -> Self::Fetched<'gc> {
        roots.fetch(&self.0)
    }
}
