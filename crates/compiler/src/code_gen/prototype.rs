use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation};

use crate::constant::Constant;

/// A compiler generated prototype with compiled bytecode.
///
/// This is distinct from a VM prototype in that it may not use VM interned strings, does not store
/// child prototypes as a `Gc` pointer, and does not have a reference to a concrete `MagicSet`.
#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub struct Prototype<S> {
    pub reference: vm::FunctionRef,
    pub bytecode: vm::ByteCode,
    pub constants: Box<[Constant<S>]>,
    pub prototypes: Box<[Prototype<S>]>,
    pub used_registers: usize,
    pub heap_vars: Box<[vm::closure::HeapVarDescriptor]>,
}

impl<S> Prototype<S> {
    pub fn map_string<S2>(self, map: impl Fn(S) -> S2) -> Prototype<S2> {
        let Self {
            reference,
            bytecode,
            constants,
            prototypes,
            used_registers,
            heap_vars,
        } = self;

        let constants = constants.into_iter().map(|c| c.map_string(&map)).collect();
        let prototypes = prototypes.into_iter().map(|p| p.map_string(&map)).collect();

        Prototype {
            reference,
            bytecode,
            constants,
            prototypes,
            used_registers,
            heap_vars,
        }
    }
}

impl<'gc> Prototype<vm::String<'gc>> {
    /// The given `MagicSet` pointer must match the magic variables provided during codegen.
    pub fn into_vm(
        self,
        mc: &Mutation<'gc>,
        chunk: vm::Chunk<'gc>,
        magic: Gc<'gc, vm::MagicSet<'gc>>,
    ) -> Gc<'gc, vm::Prototype<'gc>> {
        let Self {
            reference,
            bytecode,
            constants,
            prototypes,
            used_registers,
            heap_vars,
        } = self;

        let constants = constants
            .into_iter()
            .map(|c| match c {
                Constant::Undefined => vm::Constant::Undefined,
                Constant::Boolean(b) => vm::Constant::Boolean(b),
                Constant::Integer(i) => vm::Constant::Integer(i),
                Constant::Float(f) => vm::Constant::Float(f),
                Constant::String(s) => vm::Constant::String(s),
            })
            .collect();

        let prototypes = prototypes
            .into_iter()
            .map(|p| p.into_vm(mc, chunk, magic))
            .collect();

        Gc::new(
            mc,
            vm::Prototype {
                chunk,
                reference,
                magic,
                bytecode,
                constants,
                prototypes,
                used_registers,
                heap_vars,
            },
        )
    }
}
