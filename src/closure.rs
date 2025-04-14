use std::fmt;

use gc_arena::{Collect, Gc, Lock, Mutation};
use thiserror::Error;

use crate::{bytecode::ByteCode, object::Object, string::String, value::Value};

#[derive(Debug, Copy, Clone, PartialEq, Collect)]
#[collect(no_drop)]
pub enum Constant<'gc> {
    Undefined,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String<'gc>),
}

impl<'gc> Constant<'gc> {
    pub fn to_value(self) -> Value<'gc> {
        match self {
            Constant::Undefined => Value::Undefined,
            Constant::Boolean(b) => Value::Boolean(b),
            Constant::Integer(i) => Value::Integer(i),
            Constant::Float(f) => Value::Float(f),
            Constant::String(s) => Value::String(s),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub enum HeapVarDescriptor {
    Owned,
    UpValue(usize),
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

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Prototype<'gc> {
    pub bytecode: ByteCode,
    pub constants: Box<[Constant<'gc>]>,
    pub prototypes: Box<[Gc<'gc, Prototype<'gc>>]>,
    pub used_registers: usize,
    pub heap_vars: Box<[HeapVarDescriptor]>,
}

#[derive(Debug, Error)]
#[error("missing upvalue")]
pub struct MissingUpValue;

#[derive(Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Closure<'gc>(Gc<'gc, ClosureInner<'gc>>);

#[derive(Collect)]
#[collect(no_drop)]
pub struct ClosureInner<'gc> {
    proto: Gc<'gc, Prototype<'gc>>,
    this: Object<'gc>,
    heap: Box<[HeapVar<'gc>]>,
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

impl<'gc> Closure<'gc> {
    /// Create a new top-level closure.
    ///
    /// Given prototype must not have any upvalues.
    pub fn new(
        mc: &Mutation<'gc>,
        proto: Gc<'gc, Prototype<'gc>>,
        this: Object<'gc>,
    ) -> Result<Self, MissingUpValue> {
        Self::with_upvalues(mc, proto, this, &[])
    }

    /// Create a new closure using the given `upvalues` array to lookup any required upvalues.
    pub fn with_upvalues(
        mc: &Mutation<'gc>,
        proto: Gc<'gc, Prototype<'gc>>,
        this: Object<'gc>,
        upvalues: &[HeapVar<'gc>],
    ) -> Result<Self, MissingUpValue> {
        let mut heap = Vec::new();
        for &heap_desc in &proto.heap_vars {
            match heap_desc {
                HeapVarDescriptor::Owned => {
                    heap.push(HeapVar::new(mc, Value::Undefined));
                }
                HeapVarDescriptor::UpValue(index) => {
                    heap.push(*upvalues.get(index).ok_or(MissingUpValue)?);
                }
            }
        }

        Ok(Self(Gc::new(
            mc,
            ClosureInner {
                proto,
                this,
                heap: heap.into_boxed_slice(),
            },
        )))
    }

    #[inline]
    pub fn from_inner(inner: Gc<'gc, ClosureInner<'gc>>) -> Self {
        Self(inner)
    }

    #[inline]
    pub fn into_inner(self) -> Gc<'gc, ClosureInner<'gc>> {
        self.0
    }

    #[inline]
    pub fn prototype(self) -> Gc<'gc, Prototype<'gc>> {
        self.0.proto
    }

    #[inline]
    pub fn this(self) -> Object<'gc> {
        self.0.this
    }

    #[inline]
    pub fn heap(self) -> &'gc [HeapVar<'gc>] {
        &self.0.as_ref().heap
    }
}
