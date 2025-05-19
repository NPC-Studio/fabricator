use std::{
    collections::{HashMap, hash_map},
    string::String as StdString,
};

use gc_arena::{Collect, Gc, Mutation};
use thiserror::Error;

use crate::{error::Error, interpreter::Context, string::String, value::Value};

#[derive(Debug, Error)]
#[error("cannot write to a read only magic value")]
pub struct MagicReadOnly;

/// A trait for "magic" global values in FML.
///
/// Magic values are always available in every scope, and can only be shadowed by a local `var`
/// declaration (never via `self`). This means that any time a *free* variable is referenced with
/// the name of a magic variable, it will always refer to the magic variable (and never to, for
/// example, a global).
///
/// Magic values can be used to provide an API to scripts that is usable no matter the current
/// `self` value, without having to explicitly reference the `global` table.
///
/// Magic values can optionally be writeable. This does not *replace* the value magic value like
/// would occur normally in FML, instead it triggers a write callback for that particular magic
/// value.
pub trait Magic<'gc> {
    fn get(&self, ctx: Context<'gc>) -> Result<Value<'gc>, Error>;

    fn set(&self, _ctx: Context<'gc>, _value: Value<'gc>) -> Result<(), Error> {
        Err(MagicReadOnly.into())
    }

    // Magic value should be treated as read-only, calling `Magic::set` will error.
    fn read_only(&self) -> bool {
        true
    }
}

#[derive(Debug, Error)]
#[error("magic value name {0:?} is not unique")]
pub struct DuplicateMagicName(StdString);

/// A set for all magic values available to some FML script.
///
/// Magic values are always referenced in the VM by their index for speed, rather than by name.
#[derive(Clone, Default, Collect)]
#[collect(no_drop)]
pub struct MagicSet<'gc> {
    registered: Vec<Gc<'gc, dyn Magic<'gc>>>,
    names: HashMap<String<'gc>, usize>,
}

impl<'gc> MagicSet<'gc> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(
        &mut self,
        name: String<'gc>,
        value: Gc<'gc, dyn Magic<'gc>>,
    ) -> Result<usize, DuplicateMagicName> {
        let index = self.registered.len();
        let hash_map::Entry::Vacant(name_entry) = self.names.entry(name) else {
            return Err(DuplicateMagicName(name.as_str().to_owned()));
        };
        name_entry.insert(index);
        self.registered.push(value);
        Ok(index)
    }

    /// Add a read-only magic variable which always contains a constant value.
    pub fn add_constant(
        &mut self,
        mc: &Mutation<'gc>,
        name: String<'gc>,
        value: Value<'gc>,
    ) -> Result<usize, DuplicateMagicName> {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct MagicConstant<'gc>(Value<'gc>);

        impl<'gc> Magic<'gc> for MagicConstant<'gc> {
            fn get(&self, _ctx: Context<'gc>) -> Result<Value<'gc>, Error> {
                Ok(self.0)
            }
        }

        self.add(
            name,
            gc_arena::unsize!(Gc::new(mc, MagicConstant(value)) => dyn Magic),
        )
    }

    pub fn find(&self, name: &str) -> Option<usize> {
        self.names.get(name).copied()
    }

    pub fn get(&self, index: usize) -> Option<Gc<'gc, dyn Magic<'gc>>> {
        self.registered.get(index).copied()
    }

    pub fn merge(&mut self, other: &MagicSet<'gc>) -> Result<(), DuplicateMagicName> {
        for (&name, &index) in &other.names {
            self.add(name, other.registered[index])?;
        }
        Ok(())
    }
}
