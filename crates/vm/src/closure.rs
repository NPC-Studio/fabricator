use std::fmt;

use gc_arena::{Collect, Gc, Lock, Mutation};
use thiserror::Error;

use crate::{
    bytecode::ByteCode,
    debug::{Chunk, RefName, Span},
    instructions::HeapIdx,
    magic::MagicSet,
    string::String,
    value::Value,
};

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

/// A shared [`Value`] that can be referenced by multiple closures with independent lifetimes.
pub type SharedValue<'gc> = Gc<'gc, Lock<Value<'gc>>>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub enum HeapVarDescriptor {
    /// This heap variable is owned by a closure.
    ///
    /// Contains a "slot" for an owned heap variable. Slots may be re-used for a set of variables if
    /// their lifetimes do not overlap with each other and have `ResetHeap` instructions in-between.
    ///
    /// When the running closure created from this prototype executes and uses this heap variable,
    /// it will create a new, unique instance of it.
    Owned(HeapIdx),
    /// This heap variable is a prototype-level static.
    ///
    /// The index must be a valid index into the `static_vars` prototype table.
    Static(HeapIdx),
    /// This heap variable is a reference to a heap variable from a parent closure.
    ///
    /// Having a non-owned heap variable means that this prototype represents a function that closes
    /// over an upper variable, and the two functions need to share this variable with potentially
    /// different lifetimes.
    ///
    /// Contains the index into the *parent* heap variable list for the heap variable that this
    /// upvalue references.
    UpValue(HeapIdx),
}

#[derive(Debug, Clone, Collect)]
#[collect(require_static)]
pub enum FunctionRef {
    Named(RefName, Span),
    Expression(Span),
    Chunk,
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Prototype<'gc> {
    pub chunk: Chunk<'gc>,
    pub reference: FunctionRef,
    pub magic: Gc<'gc, MagicSet<'gc>>,
    pub bytecode: ByteCode,
    pub constants: Box<[Constant<'gc>]>,
    pub prototypes: Box<[Gc<'gc, Prototype<'gc>>]>,
    pub static_vars: Box<[SharedValue<'gc>]>,
    pub used_registers: usize,
    pub heap_vars: Box<[HeapVarDescriptor]>,
}

impl<'gc> Prototype<'gc> {
    pub fn has_upvalues(&self) -> bool {
        for &h in &self.heap_vars {
            if matches!(h, HeapVarDescriptor::UpValue(_)) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub enum HeapVar<'gc> {
    /// A `HeapVarDescriptor::Owned` heap variable.
    Owned(HeapIdx),
    /// A shared heap variable (either a static or an upvalue).
    Shared(SharedValue<'gc>),
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
    this: Value<'gc>,
    heap: Gc<'gc, Box<[HeapVar<'gc>]>>,
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
        this: Value<'gc>,
    ) -> Result<Self, MissingUpValue> {
        let mut heap = Vec::new();
        for &h in &proto.heap_vars {
            match h {
                HeapVarDescriptor::Owned(idx) => {
                    heap.push(HeapVar::Owned(idx));
                }
                HeapVarDescriptor::Static(idx) => {
                    heap.push(HeapVar::Shared(proto.static_vars[idx as usize]))
                }
                HeapVarDescriptor::UpValue(_) => {
                    return Err(MissingUpValue);
                }
            }
        }
        Self::from_parts(mc, proto, this, Gc::new(mc, heap.into_boxed_slice()))
    }

    /// Create a new closure using the given `upvalues` array to lookup any required upvalues.
    pub fn from_parts(
        mc: &Mutation<'gc>,
        proto: Gc<'gc, Prototype<'gc>>,
        this: Value<'gc>,
        heap: Gc<'gc, Box<[HeapVar<'gc>]>>,
    ) -> Result<Self, MissingUpValue> {
        Ok(Self(Gc::new(mc, ClosureInner { proto, this, heap })))
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

    /// Returns the currently bound `this` object.
    ///
    /// Will return `Value::Undefined` if there is no bound `this` object set.
    #[inline]
    pub fn this(self) -> Value<'gc> {
        self.0.this
    }

    /// Return a clone of this closure with the embedded `this` value changed to the provided one.
    ///
    /// If `Value::Undefined` is provided, then the bound `this` object will be removed.
    #[inline]
    pub fn rebind(self, mc: &Mutation<'gc>, this: Value<'gc>) -> Closure<'gc> {
        Self(Gc::new(
            mc,
            ClosureInner {
                proto: self.0.proto,
                heap: self.0.heap,
                this,
            },
        ))
    }

    #[inline]
    pub fn heap(self) -> &'gc [HeapVar<'gc>] {
        &self.0.as_ref().heap
    }
}
