use std::fmt;

use gc_arena::{Collect, Gc, Lock, Mutation};

use crate::{
    bytecode::ByteCode,
    constant::Constant,
    value::{String, Value},
};

#[derive(Collect)]
#[collect(no_drop)]
pub struct Prototype<'gc> {
    pub bytecode: ByteCode,
    pub constants: Box<[Constant<String<'gc>>]>,
    pub used_registers: usize,
    pub used_heap: usize,
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct HeapVar<'gc>(Gc<'gc, Lock<Value<'gc>>>);

impl<'gc> HeapVar<'gc> {
    pub fn new(mc: &Mutation<'gc>, value: Value<'gc>) -> Self {
        Self(Gc::new(mc, Lock::new(value)))
    }

    pub fn get(self) -> Value<'gc> {
        self.0.get()
    }

    pub fn set(self, mc: &Mutation<'gc>, state: Value<'gc>) {
        self.0.set(mc, state)
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct ClosureInner<'gc> {
    proto: Gc<'gc, Prototype<'gc>>,
}

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(Gc<'gc, ClosureInner<'gc>>);

impl<'gc> Closure<'gc> {
    pub fn new(mc: &Mutation<'gc>, proto: Gc<'gc, Prototype<'gc>>) -> Self {
        Self(Gc::new(mc, ClosureInner { proto }))
    }

    pub fn prototype(self) -> Gc<'gc, Prototype<'gc>> {
        self.0.proto
    }
}

impl<'gc> fmt::Debug for Closure<'gc> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_tuple("Function")
            .field(&Gc::as_ptr(self.0))
            .finish()
    }
}

impl<'gc> PartialEq for Closure<'gc> {
    fn eq(&self, other: &Closure<'gc>) -> bool {
        Gc::ptr_eq(self.0, other.0)
    }
}

impl<'gc> Eq for Closure<'gc> {}
