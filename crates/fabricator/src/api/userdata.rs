use std::{collections::HashMap, marker::PhantomData};

use fabricator_util::typed_id_map;
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
        ) -> Result<Option<vm::Value<'gc>>, vm::RuntimeError>,
    >,
    write_custom: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), vm::RuntimeError>,
    >,
}

struct Property<U: for<'a> Rootable<'a>> {
    read: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
        ) -> Result<vm::Value<'gc>, vm::RuntimeError>,
    >,
    write: Box<
        dyn for<'gc> Fn(
            vm::Context<'gc>,
            &barrier::Write<Root<'gc, U>>,
            vm::Value<'gc>,
        ) -> Result<(), vm::RuntimeError>,
    >,
}

impl<U: for<'a> Rootable<'a>> Default for UserDataProperties<U> {
    fn default() -> Self {
        Self {
            properties: Default::default(),
            read_custom: Box::new(|_, _, name| {
                Err(vm::RuntimeError::msg(format!("no such field {}", name)))
            }),
            write_custom: Box::new(|_, _, name, _| {
                Err(vm::RuntimeError::msg(format!("no such field {}", name)))
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
        read: impl for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
        ) -> Result<vm::Value<'gc>, vm::RuntimeError>
        + 'static,
    ) {
        let name = name.into();
        self.properties.insert(
            name.clone(),
            Property {
                read: Box::new(read),
                write: Box::new(move |_, _, _| {
                    Err(vm::RuntimeError::msg(format!(
                        "cannot set read only property {name:?}"
                    )))
                }),
            },
        );
    }

    pub fn add_rw_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
        ) -> Result<vm::Value<'gc>, vm::RuntimeError>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &barrier::Write<Root<'gc, U>>,
            vm::Value<'gc>,
        ) -> Result<(), vm::RuntimeError>
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
        ) -> Result<Option<vm::Value<'gc>>, vm::RuntimeError>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &Root<'gc, U>,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), vm::RuntimeError>
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
        ud: vm::UserData<'gc>,
        ctx: vm::Context<'gc>,
        key: vm::String<'gc>,
    ) -> Result<vm::Value<'gc>, vm::RuntimeError> {
        let u = ud.downcast::<U>().unwrap();
        if let Some(prop) = self.properties.get(key.as_str()) {
            Ok((prop.read)(ctx, u)?)
        } else {
            (self.read_custom)(ctx, u, key)?
                .ok_or_else(|| vm::RuntimeError::msg(format!("missing field {:?}", key)))
        }
    }

    fn set_field(
        &self,
        ud: vm::UserData<'gc>,
        ctx: vm::Context<'gc>,
        key: vm::String<'gc>,
        value: vm::Value<'gc>,
    ) -> Result<(), vm::RuntimeError> {
        let u = ud.downcast_write::<U>(&ctx).unwrap();
        if let Some(prop) = self.properties.get(key.as_str()) {
            Ok((prop.write)(ctx, u, value)?)
        } else {
            Ok((self.write_custom)(ctx, u, key, value)?)
        }
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
        read: impl for<'gc> Fn(vm::Context<'gc>, &U) -> Result<vm::Value<'gc>, vm::RuntimeError>
        + 'static,
    ) {
        self.properties
            .add_ro_property(name, move |ctx, u| read(ctx, &u.0));
    }

    pub fn add_rw_property(
        &mut self,
        name: impl Into<String>,
        read: impl for<'gc> Fn(vm::Context<'gc>, &U) -> Result<vm::Value<'gc>, vm::RuntimeError>
        + 'static,
        write: impl for<'gc> Fn(vm::Context<'gc>, &U, vm::Value<'gc>) -> Result<(), vm::RuntimeError>
        + 'static,
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
        ) -> Result<Option<vm::Value<'gc>>, vm::RuntimeError>
        + 'static,
        write: impl for<'gc> Fn(
            vm::Context<'gc>,
            &U,
            vm::String<'gc>,
            vm::Value<'gc>,
        ) -> Result<(), vm::RuntimeError>
        + 'static,
    ) {
        self.properties.enable_custom_properties(
            move |ctx, u, key| read(ctx, &u.0, key),
            move |ctx, u, key, value| write(ctx, &u.0, key, value),
        );
    }
}

/// A wrapper around a `typed_id_map::Id` ID type.
///
/// Can be coerced to an integer in scripts, which yields the id's index.
#[derive(Debug, Copy, Clone)]
pub struct IdUserData<I> {
    pub id: I,
}

impl<I> IdUserData<I>
where
    I: typed_id_map::Id + 'static,
{
    pub fn new<'gc>(ctx: vm::Context<'gc>, id: I) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct Methods<I>(PhantomData<I>);

        impl<'gc, I> vm::UserDataMethods<'gc> for Methods<I>
        where
            I: typed_id_map::Id + 'static,
        {
            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(ud.downcast_static::<IdUserData<I>>().unwrap().id.index() as i64)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop, bound = "")]
        struct MethodsSingleton<'gc, I>(Gc<'gc, dyn vm::UserDataMethods<'gc>>, PhantomData<I>);

        impl<'gc, I> vm::Singleton<'gc> for MethodsSingleton<'gc, I>
        where
            I: typed_id_map::Id + 'static,
        {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, Methods::<I>(PhantomData));
                MethodsSingleton(
                    gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>),
                    PhantomData,
                )
            }
        }

        let methods = ctx.singleton::<Rootable![MethodsSingleton<'_, I>]>().0;

        let userdata = vm::UserData::new_static(&ctx, IdUserData { id });
        userdata.set_methods(&ctx, Some(methods));

        userdata
    }

    pub fn downcast<'gc>(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast_static::<IdUserData<I>>()
    }
}

/// A wrapper around a `typed_id_map::Id` ID type with an assigned name.
///
/// Can be coerced to an integer or string in scripts. Coercing to an integer yields the id's index,
/// coercing to a string yields the given name.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct NamedIdUserData<'gc, I> {
    #[collect(require_static)]
    pub id: I,
    pub name: vm::String<'gc>,
}

impl<'gc, I> NamedIdUserData<'gc, I>
where
    I: typed_id_map::Id + 'static,
{
    pub fn new(ctx: vm::Context<'gc>, id: I, name: vm::String<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct Methods<I>(PhantomData<I>);

        impl<'gc, I> vm::UserDataMethods<'gc> for Methods<I>
        where
            I: typed_id_map::Id + 'static,
        {
            fn coerce_string(
                &self,
                ud: vm::UserData<'gc>,
                _ctx: vm::Context<'gc>,
            ) -> Option<vm::String<'gc>> {
                Some(
                    ud.downcast::<Rootable![NamedIdUserData<'_, I>]>()
                        .unwrap()
                        .name,
                )
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(
                    ud.downcast::<Rootable![NamedIdUserData<'_, I>]>()
                        .unwrap()
                        .id
                        .index() as i64,
                )
            }
        }

        #[derive(Collect)]
        #[collect(no_drop, bound = "")]
        struct MethodsSingleton<'gc, I>(Gc<'gc, dyn vm::UserDataMethods<'gc>>, PhantomData<I>);

        impl<'gc, I> vm::Singleton<'gc> for MethodsSingleton<'gc, I>
        where
            I: typed_id_map::Id + 'static,
        {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, Methods::<I>(PhantomData));
                MethodsSingleton(
                    gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>),
                    PhantomData,
                )
            }
        }

        let methods = ctx.singleton::<Rootable![MethodsSingleton<'_, I>]>().0;

        let userdata = vm::UserData::new::<Rootable![NamedIdUserData<'_, I>]>(
            &ctx,
            NamedIdUserData { id, name },
        );
        userdata.set_methods(&ctx, Some(methods));

        userdata
    }

    pub fn downcast(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast::<Rootable![NamedIdUserData<'_, I>]>()
    }
}
