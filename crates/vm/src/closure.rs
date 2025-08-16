use std::fmt;

use gc_arena::{Collect, Gc, Lock, Mutation};
use thiserror::Error;

use crate::{
    debug::{Chunk, FunctionRef},
    instructions::{ByteCode, ConstIdx, HeapIdx, Instruction, MagicIdx, ProtoIdx, RegIdx},
    magic::MagicSet,
    object::Object,
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

#[derive(Debug, Error)]
pub enum PrototypeVerificationError {
    #[error("inner prototype has an invalid upvalue idx {0}, for prototype {1}")]
    BadUpValueIdx(HeapIdx, usize),
    #[error("register index {0} is not in range of `used_registers` at instruction {1}")]
    BadRegIdx(RegIdx, usize),
    #[error("const idx {0} out of range at instruction {1}")]
    BadConstIdx(ConstIdx, usize),
    #[error("heap idx {0} out of range at instruction {1}")]
    BadHeapIdx(HeapIdx, usize),
    #[error("proto idx {0} out of range at instruction {1}")]
    BadProtoIdx(ProtoIdx, usize),
    #[error("no magic variable with index {0} at instruction {1}")]
    BadMagicIdx(MagicIdx, usize),
    #[error("field constant {0} is not a `Constant::String` at instruction {1}")]
    FieldIsNotString(ConstIdx, usize),
    #[error("index constant {0} is not a `Constant::Integer` at instruction {1}")]
    IndexIsNotInt(ConstIdx, usize),
    #[error("cannot reset a shared heap variable {0} at instruction {1}")]
    ResetSharedHeap(HeapIdx, usize),
}

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Prototype<'gc> {
    chunk: Chunk<'gc>,
    reference: FunctionRef,
    magic: Gc<'gc, MagicSet<'gc>>,
    bytecode: ByteCode,
    constants: Box<[Constant<'gc>]>,
    prototypes: Box<[Gc<'gc, Prototype<'gc>>]>,
    static_vars: Box<[SharedValue<'gc>]>,
    heap_vars: Box<[HeapVarDescriptor]>,
    used_registers: usize,
    constructor_super: Gc<'gc, Lock<Option<Object<'gc>>>>,
}

impl<'gc> Prototype<'gc> {
    pub fn new(
        mc: &Mutation<'gc>,
        chunk: Chunk<'gc>,
        reference: FunctionRef,
        magic: Gc<'gc, MagicSet<'gc>>,
        bytecode: ByteCode,
        constants: Box<[Constant<'gc>]>,
        prototypes: Box<[Gc<'gc, Prototype<'gc>>]>,
        static_vars: Box<[SharedValue<'gc>]>,
        heap_vars: Box<[HeapVarDescriptor]>,
        used_registers: usize,
    ) -> Result<Self, PrototypeVerificationError> {
        for (inner_proto_idx, inner) in prototypes.iter().enumerate() {
            for &inner_heap_var in &inner.heap_vars {
                match inner_heap_var {
                    HeapVarDescriptor::Owned(_) | HeapVarDescriptor::Static(_) => {}
                    HeapVarDescriptor::UpValue(upvalue_idx) => {
                        if (upvalue_idx as usize) >= heap_vars.len() {
                            return Err(PrototypeVerificationError::BadUpValueIdx(
                                upvalue_idx,
                                inner_proto_idx,
                            ));
                        }
                    }
                }
            }
        }

        for (inst_index, (inst, _)) in bytecode.decode().enumerate() {
            let verify_reg_idx = |reg_idx: RegIdx| {
                if (reg_idx as usize) < used_registers {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::BadRegIdx(reg_idx, inst_index))
                }
            };

            let verify_const_idx = |const_idx: ConstIdx| {
                if (const_idx as usize) < constants.len() {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::BadConstIdx(
                        const_idx, inst_index,
                    ))
                }
            };

            let verify_heap_idx = |heap_idx: HeapIdx| {
                if (heap_idx as usize) < heap_vars.len() {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::BadHeapIdx(heap_idx, inst_index))
                }
            };

            let verify_proto_idx = |proto_idx: ProtoIdx| {
                if (proto_idx as usize) < prototypes.len() {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::BadHeapIdx(
                        proto_idx, inst_index,
                    ))
                }
            };

            let verify_magic_idx = |magic_idx: MagicIdx| {
                magic
                    .get(magic_idx as usize)
                    .map_err(|_| PrototypeVerificationError::BadMagicIdx(magic_idx, inst_index))
            };

            let verify_const_as_field = |const_idx: MagicIdx| {
                if matches!(constants[const_idx as usize], Constant::String(_)) {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::FieldIsNotString(
                        const_idx, inst_index,
                    ))
                }
            };

            let verify_const_as_index = |const_idx: MagicIdx| {
                if matches!(constants[const_idx as usize], Constant::Integer(_)) {
                    Ok(())
                } else {
                    Err(PrototypeVerificationError::IndexIsNotInt(
                        const_idx, inst_index,
                    ))
                }
            };

            match inst {
                Instruction::Undefined { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::Boolean { dest, .. } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::LoadConstant { dest, constant } => {
                    verify_reg_idx(dest)?;
                    verify_const_idx(constant)?;
                }
                Instruction::GetHeap { dest, heap } => {
                    verify_reg_idx(dest)?;
                    verify_heap_idx(heap)?;
                }
                Instruction::SetHeap { heap, source } => {
                    verify_heap_idx(heap)?;
                    verify_reg_idx(source)?;
                }
                Instruction::ResetHeap { heap } => {
                    verify_heap_idx(heap)?;
                    if !matches!(heap_vars[heap as usize], HeapVarDescriptor::Owned(_)) {
                        return Err(PrototypeVerificationError::ResetSharedHeap(
                            heap, inst_index,
                        ));
                    }
                }
                Instruction::Globals { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::This { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::SetThis { source } => {
                    verify_reg_idx(source)?;
                }
                Instruction::Other { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::SetOther { source } => {
                    verify_reg_idx(source)?;
                }
                Instruction::SwapThisOther {} => {}
                Instruction::Closure { dest, proto } => {
                    verify_reg_idx(dest)?;
                    verify_proto_idx(proto)?;
                }
                Instruction::CurrentClosure { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::NewObject { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::NewArray { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::GetField { dest, object, key } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(object)?;
                    verify_reg_idx(key)?;
                }
                Instruction::SetField { object, key, value } => {
                    verify_reg_idx(object)?;
                    verify_reg_idx(key)?;
                    verify_reg_idx(value)?;
                }
                Instruction::GetFieldConst { dest, object, key } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(object)?;
                    verify_const_idx(key)?;
                    verify_const_as_field(key)?;
                }
                Instruction::SetFieldConst { object, key, value } => {
                    verify_reg_idx(object)?;
                    verify_const_idx(key)?;
                    verify_const_as_field(key)?;
                    verify_reg_idx(value)?;
                }
                Instruction::GetIndex { dest, array, index } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(array)?;
                    verify_reg_idx(index)?;
                }
                Instruction::SetIndex {
                    array,
                    index,
                    value,
                } => {
                    verify_reg_idx(array)?;
                    verify_reg_idx(index)?;
                    verify_reg_idx(value)?;
                }
                Instruction::GetIndexConst { dest, array, index } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(array)?;
                    verify_const_idx(index)?;
                }
                Instruction::SetIndexConst {
                    array,
                    index,
                    value,
                } => {
                    verify_reg_idx(array)?;
                    verify_const_idx(index)?;
                    verify_reg_idx(value)?;
                }
                Instruction::Copy { dest, source } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(source)?;
                }
                Instruction::IsDefined { dest, arg }
                | Instruction::IsUndefined { dest, arg }
                | Instruction::Test { dest, arg }
                | Instruction::Not { dest, arg }
                | Instruction::Negate { dest, arg }
                | Instruction::BitNegate { dest, arg }
                | Instruction::Increment { dest, arg }
                | Instruction::Decrement { dest, arg } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(arg)?;
                }
                Instruction::Add { dest, left, right }
                | Instruction::Subtract { dest, left, right }
                | Instruction::Multiply { dest, left, right }
                | Instruction::Divide { dest, left, right }
                | Instruction::Remainder { dest, left, right }
                | Instruction::IntDivide { dest, left, right }
                | Instruction::IsEqual { dest, left, right }
                | Instruction::IsNotEqual { dest, left, right }
                | Instruction::IsLess { dest, left, right }
                | Instruction::IsLessEqual { dest, left, right }
                | Instruction::And { dest, left, right }
                | Instruction::Or { dest, left, right }
                | Instruction::Xor { dest, left, right }
                | Instruction::BitAnd { dest, left, right }
                | Instruction::BitOr { dest, left, right }
                | Instruction::BitXor { dest, left, right }
                | Instruction::BitShiftLeft { dest, left, right }
                | Instruction::BitShiftRight { dest, left, right }
                | Instruction::NullCoalesce { dest, left, right } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(left)?;
                    verify_reg_idx(right)?;
                }
                Instruction::StackTop { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::StackResize { stack_top } => {
                    verify_reg_idx(stack_top)?;
                }
                Instruction::StackResizeConst { stack_top } => {
                    verify_const_idx(stack_top)?;
                    verify_const_as_index(stack_top)?;
                }
                Instruction::StackGet { dest, stack_pos } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(stack_pos)?;
                }
                Instruction::StackGetConst { dest, stack_pos } => {
                    verify_reg_idx(dest)?;
                    verify_const_idx(stack_pos)?;
                    verify_const_as_index(stack_pos)?;
                }
                Instruction::StackGetOffset {
                    dest,
                    stack_base,
                    offset,
                } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(stack_base)?;
                    verify_const_idx(offset)?;
                    verify_const_as_index(offset)?;
                }
                Instruction::StackSet { source, stack_pos } => {
                    verify_reg_idx(source)?;
                    verify_reg_idx(stack_pos)?;
                }
                Instruction::StackPush { source } => {
                    verify_reg_idx(source)?;
                }
                Instruction::StackPop { dest } => {
                    verify_reg_idx(dest)?;
                }
                Instruction::GetIndexMulti {
                    dest,
                    array,
                    stack_bottom,
                } => {
                    verify_reg_idx(dest)?;
                    verify_reg_idx(array)?;
                    verify_reg_idx(stack_bottom)?;
                }
                Instruction::SetIndexMulti {
                    array,
                    stack_bottom,
                    value,
                } => {
                    verify_reg_idx(array)?;
                    verify_reg_idx(stack_bottom)?;
                    verify_reg_idx(value)?;
                }
                Instruction::GetMagic { dest, magic } => {
                    verify_reg_idx(dest)?;
                    verify_magic_idx(magic)?;
                }
                Instruction::SetMagic { magic, source } => {
                    verify_magic_idx(magic)?;
                    verify_reg_idx(source)?;
                }
                Instruction::Throw { source } => {
                    verify_reg_idx(source)?;
                }
                Instruction::Jump { .. } => {}
                Instruction::JumpIf { arg, .. } => {
                    verify_reg_idx(arg)?;
                }
                Instruction::Call { func, stack_bottom } => {
                    verify_reg_idx(func)?;
                    verify_reg_idx(stack_bottom)?;
                }
                Instruction::Return { stack_bottom } => {
                    verify_reg_idx(stack_bottom)?;
                }
            }
        }

        let constructor_super = Gc::new(mc, Lock::new(None));

        Ok(Self {
            chunk,
            reference,
            magic,
            bytecode,
            constants,
            prototypes,
            static_vars,
            heap_vars,
            used_registers,
            constructor_super,
        })
    }

    #[inline]
    pub fn chunk(&self) -> Chunk<'gc> {
        self.chunk
    }

    #[inline]
    pub fn reference(&self) -> &FunctionRef {
        &self.reference
    }

    #[inline]
    pub fn magic(&self) -> Gc<'gc, MagicSet<'gc>> {
        self.magic
    }

    #[inline]
    pub fn bytecode(&self) -> &ByteCode {
        &self.bytecode
    }

    #[inline]
    pub fn constants(&self) -> &[Constant<'gc>] {
        &self.constants
    }

    #[inline]
    pub fn prototypes(&self) -> &[Gc<'gc, Prototype<'gc>>] {
        &self.prototypes
    }

    #[inline]
    pub fn static_vars(&self) -> &[SharedValue<'gc>] {
        &self.static_vars
    }

    #[inline]
    pub fn heap_vars(&self) -> &[HeapVarDescriptor] {
        &self.heap_vars
    }

    /// If it is not already created, associate a new `Object` with this prototype that defines the
    /// super-object of all constructed objects and return it.
    ///
    /// If it is already created, then return the existing super object.
    #[inline]
    pub fn init_constructor_super(&self, mc: &Mutation<'gc>) -> Object<'gc> {
        match self.constructor_super.get() {
            Some(obj) => obj,
            None => {
                let obj = Object::new(mc);
                self.constructor_super.set(mc, Some(obj));
                obj
            }
        }
    }

    #[inline]
    pub fn used_registers(&self) -> usize {
        self.used_registers
    }

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
