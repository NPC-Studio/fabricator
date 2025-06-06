use std::collections::HashMap;

use anyhow::{Context as _, Error, anyhow};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Mutation, Rootable, arena::Root, barrier};

#[derive(Collect)]
#[collect(require_static)]
pub struct UserDataProperties<U: for<'a> Rootable<'a>> {
    properties: HashMap<String, Property<U>>,
    read_custom: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
        ) -> Result<Option<vm::Value<'gc>>, Error>,
    >,
    write_custom: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), Error>,
    >,
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
            read_custom: Box::new(|_, _, name| {
                Err(Error::msg(anyhow!("no such field {:?}", name)))
            }),
            write_custom: Box::new(|_, _, name, _| {
                Err(Error::msg(anyhow!("no such field {:?}", name)))
            }),
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

    pub fn enable_custom_properties(
        &mut self,
        read: impl for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
        ) -> Result<Option<vm::Value<'gc>>, Error>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), Error>
        + 'static,
    ) {
        self.read_custom = Box::new(read);
        self.write_custom = Box::new(write);
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
        if let Some(prop) = self.properties.get(key.as_str()) {
            Ok((prop.read)(ctx, u)?)
        } else {
            Ok((self.read_custom)(ctx, u, key)?
                .with_context(|| anyhow!("missing field {:?}", key))?)
        }
    }

    fn set_field(
        &self,
        ctx: fabricator_vm::Context<'gc>,
        ud: fabricator_vm::UserData<'gc>,
        key: fabricator_vm::String<'gc>,
        value: fabricator_vm::Value<'gc>,
    ) -> Result<(), fabricator_vm::Error> {
        let u = ud.downcast_write::<U>(&ctx)?;
        if let Some(prop) = self.properties.get(key.as_str()) {
            Ok((prop.write)(ctx, u, value)?)
        } else {
            Ok((self.write_custom)(ctx, u, key, value)?)
        }
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

    pub fn enable_custom_properties(
        &mut self,
        read: impl for<'gc> Fn(
            vm::Context<'gc>,
            &U,
            vm::String<'gc>,
        ) -> Result<Option<vm::Value<'gc>>, Error>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &U,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), Error>
        + 'static,
    ) {
        self.properties.enable_custom_properties(
            move |ctx, u, key| read(ctx, &u.0, key),
            move |ctx, u, key, value| write(ctx, &u.0, key, value),
        );
    }
}
