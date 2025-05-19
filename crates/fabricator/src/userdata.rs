use std::collections::HashMap;

use anyhow::{Context as _, Error, anyhow};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation, Rootable, arena::Root, barrier};

#[derive(Collect)]
#[collect(require_static)]
pub struct UserDataProperties<U: for<'a> Rootable<'a>> {
    properties: HashMap<String, Property<U>>,
}

struct Property<U: for<'a> Rootable<'a>> {
    read: Box<dyn for<'gc> Fn(vm::Context<'gc>, &Root<'gc, U>) -> Result<vm::Value<'gc>, Error>>,
    write: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &barrier::Write<Root<'gc, U>>,
            vm::Value<'gc>,
        ) -> Result<(), Error>,
    >,
}

impl<U: for<'a> Rootable<'a>> Default for UserDataProperties<U> {
    fn default() -> Self {
        Self {
            properties: Default::default(),
        }
    }
}

impl<U> UserDataProperties<U>
where
    U: for<'a> Rootable<'a> + 'static,
    for<'gc> Root<'gc, U>: Sized,
{
    pub fn add_ro_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(vm::Context<'gc>, &Root<'gc, U>) -> Result<vm::Value<'gc>, Error>
        + 'static,
    ) {
        let name = name.into();
        self.properties.insert(
            name.clone(),
            Property {
                read: Box::new(read),
                write: Box::new(move |_, _, _| {
                    Err(Error::msg(format!(
                        "cannot set read only property {:?}",
                        name
                    )))
                }),
            },
        );
    }

    pub fn add_rw_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(vm::Context<'gc>, &Root<'gc, U>) -> Result<vm::Value<'gc>, Error>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &barrier::Write<Root<'gc, U>>,
            vm::Value<'gc>,
        ) -> Result<(), Error>
        + 'static,
    ) {
        self.properties.insert(
            name.into(),
            Property {
                read: Box::new(read),
                write: Box::new(write),
            },
        );
    }

    pub fn into_methods<'gc>(self, mc: &Mutation<'gc>) -> Gc<'gc, dyn vm::UserDataMethods<'gc>> {
        gc_arena::unsize!(Gc::new(mc, self) => dyn vm::UserDataMethods)
    }
}

impl<'gc, U> vm::UserDataMethods<'gc> for UserDataProperties<U>
where
    U: for<'a> Rootable<'a> + 'static,
    Root<'gc, U>: Sized,
{
    fn get_field(
        &self,
        ctx: fabricator_vm::Context<'gc>,
        ud: fabricator_vm::UserData<'gc>,
        key: fabricator_vm::String<'gc>,
    ) -> Result<fabricator_vm::Value<'gc>, fabricator_vm::Error> {
        let u = ud.downcast::<U>()?;
        let prop = self
            .properties
            .get(key.as_str())
            .with_context(|| anyhow!("no such field {:?}", key.as_str()))?;
        Ok((prop.read)(ctx, u)?)
    }

    fn set_field(
        &self,
        ctx: fabricator_vm::Context<'gc>,
        ud: fabricator_vm::UserData<'gc>,
        key: fabricator_vm::String<'gc>,
        value: fabricator_vm::Value<'gc>,
    ) -> Result<(), fabricator_vm::Error> {
        let u = ud.downcast_write::<U>(&ctx)?;
        let prop = self
            .properties
            .get(key.as_str())
            .with_context(|| anyhow!("no such field {:?}", key.as_str()))?;
        Ok((prop.write)(ctx, u, value)?)
    }

    fn get_index(
        &self,
        _ctx: fabricator_vm::Context<'gc>,
        _ud: fabricator_vm::UserData<'gc>,
        _index: fabricator_vm::Value<'gc>,
    ) -> Result<fabricator_vm::Value<'gc>, fabricator_vm::Error> {
        Err(vm::Error::msg("no index access"))
    }

    fn set_index(
        &self,
        _ctx: fabricator_vm::Context<'gc>,
        _ud: fabricator_vm::UserData<'gc>,
        _index: fabricator_vm::Value<'gc>,
        _value: fabricator_vm::Value<'gc>,
    ) -> Result<(), fabricator_vm::Error> {
        Err(vm::Error::msg("no index access"))
    }
}

#[derive(Collect)]
#[collect(require_static)]
pub struct StaticUserDataProperties<U: 'static> {
    properties: UserDataProperties<gc_arena::Static<U>>,
}

impl<U: 'static> Default for StaticUserDataProperties<U> {
    fn default() -> Self {
        Self {
            properties: Default::default(),
        }
    }
}

impl<U: 'static> StaticUserDataProperties<U> {
    pub fn add_ro_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(vm::Context<'gc>, &U) -> Result<vm::Value<'gc>, Error> + 'static,
    ) {
        self.properties
            .add_ro_property(name, move |ctx, u| read(ctx, &u.0));
    }

    pub fn add_rw_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(vm::Context<'gc>, &U) -> Result<vm::Value<'gc>, Error> + 'static,
        write: impl for<'gc> Fn(vm::Context<'gc>, &U, vm::Value<'gc>) -> Result<(), Error> + 'static,
    ) {
        self.properties.add_rw_property(
            name,
            move |ctx, u| read(ctx, &u.0),
            move |ctx, u, val| write(ctx, &u.0, val),
        );
    }

    pub fn into_methods<'gc>(self, mc: &Mutation<'gc>) -> Gc<'gc, dyn vm::UserDataMethods<'gc>> {
        self.properties.into_methods(mc)
    }
}
