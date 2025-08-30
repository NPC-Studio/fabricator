use std::collections::HashMap;

use fabricator_collision::bound_box_tree::BoundBoxQuery;
use fabricator_math::{Box2, Vec2};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::{
        instance::InstanceUserData,
        magic::{DuplicateMagicName, MagicExt as _},
    },
    project::ObjectEvent,
    state::{Configuration, Instance, InstanceState, ObjectId, State},
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct ObjectUserData<'gc> {
    #[collect(require_static)]
    pub id: ObjectId,
    pub name: vm::String<'gc>,
}

impl<'gc> ObjectUserData<'gc> {
    pub fn new(ctx: vm::Context<'gc>, id: ObjectId, name: vm::String<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct Methods;

        impl<'gc> vm::UserDataMethods<'gc> for Methods {
            fn get_field(
                &self,
                ud: vm::UserData<'gc>,
                ctx: vm::Context<'gc>,
                key: vm::String<'gc>,
            ) -> Result<vm::Value<'gc>, vm::RuntimeError> {
                let object_id = ud.downcast::<Rootable![ObjectUserData<'_>]>().unwrap().id;
                State::ctx_with(ctx, |state| {
                    let mut found = false;
                    let mut value = vm::Value::Undefined;
                    for instance in state.instances.values() {
                        if instance.object == object_id {
                            if found {
                                return Err(vm::RuntimeError::msg(
                                    "propery access on objects only allowed on singletons",
                                ));
                            }
                            found = true;

                            value = ctx.fetch(&instance.properties).get(key).unwrap_or_default();
                        }
                    }

                    Ok(value)
                })?
            }

            fn coerce_string(
                &self,
                ud: vm::UserData<'gc>,
                _ctx: vm::Context<'gc>,
            ) -> Option<vm::String<'gc>> {
                Some(ud.downcast::<Rootable![ObjectUserData<'_>]>().unwrap().name)
            }

            fn coerce_integer(&self, ud: vm::UserData<'gc>, _ctx: vm::Context<'gc>) -> Option<i64> {
                Some(
                    ud.downcast::<Rootable![ObjectUserData<'_>]>()
                        .unwrap()
                        .id
                        .index() as i64,
                )
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct MethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for MethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, Methods);
                MethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![MethodsSingleton<'_>]>().0;

        let ud =
            vm::UserData::new::<Rootable![ObjectUserData<'_>]>(&ctx, ObjectUserData { id, name });
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast::<Rootable![ObjectUserData<'_>]>()
    }
}

pub fn no_one<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct NoOne;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            #[derive(Collect)]
            #[collect(no_drop)]
            struct Methods<'gc> {
                null_iter: vm::Callback<'gc>,
            }

            impl<'gc> vm::UserDataMethods<'gc> for Methods<'gc> {
                fn iter(
                    &self,
                    _ud: vm::UserData<'gc>,
                    _ctx: vm::Context<'gc>,
                ) -> Result<(vm::Function<'gc>, vm::Value<'gc>), vm::RuntimeError> {
                    Ok((self.null_iter.into(), vm::Value::Undefined))
                }
            }

            let ud = vm::UserData::new_static(&ctx, NoOne);
            let null_iter = vm::Callback::from_fn(&ctx, |_, _| Ok(()));
            let methods =
                gc_arena::unsize!(Gc::new(&ctx, Methods { null_iter }) => dyn vm::UserDataMethods);
            ud.set_methods(&ctx, Some(methods));

            Singleton(ud)
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn all<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct All;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            #[derive(Collect)]
            #[collect(no_drop)]
            struct Methods<'gc> {
                instance_iter: vm::Callback<'gc>,
            }

            let instance_iter = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
                let mut idx: u32 = exec.stack().consume(ctx)?;
                let next_instance = State::ctx_with(ctx, |state| {
                    while idx < state.instances.index_upper_bound() {
                        if let Some(id) = state.instances.id_for_index(idx) {
                            return Some(ctx.fetch(&state.instances[id].this));
                        }
                        idx += 1;
                    }
                    None
                })?;

                if let Some(next_instance) = next_instance {
                    exec.stack().replace(ctx, (idx + 1, next_instance))
                } else {
                    exec.stack().clear();
                }
                Ok(())
            });

            impl<'gc> vm::UserDataMethods<'gc> for Methods<'gc> {
                fn iter(
                    &self,
                    _ud: vm::UserData<'gc>,
                    _ctx: vm::Context<'gc>,
                ) -> Result<(vm::Function<'gc>, vm::Value<'gc>), vm::RuntimeError> {
                    Ok((self.instance_iter.into(), 0.into()))
                }
            }

            let methods = gc_arena::unsize!(Gc::new(&ctx, Methods { instance_iter }) => dyn vm::UserDataMethods);

            let ud = vm::UserData::new_static(&ctx, All);
            ud.set_methods(&ctx, Some(methods));

            Singleton(ud)
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn object_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    magic
        .add_constant(&ctx, ctx.intern("noone"), no_one(ctx))
        .unwrap();

    magic
        .add_constant(&ctx, ctx.intern("all"), all(ctx))
        .unwrap();

    for object in config.objects.values() {
        magic.add_constant(&ctx, ctx.intern(&object.name), ctx.fetch(&object.userdata))?;
    }

    let object_get_name = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let object: vm::UserData = exec.stack().consume(ctx)?;
        let object_id = ObjectUserData::downcast(object)?.id;
        State::ctx_with_mut(ctx, |state| {
            exec.stack()
                .replace(ctx, ctx.intern(&state.config.objects[object_id].name));
            Ok(())
        })?
    });
    magic
        .add_constant(&ctx, ctx.intern("object_get_name"), object_get_name)
        .unwrap();

    let instance_create_depth = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (x, y, depth, object, set_properties): (
            f64,
            f64,
            i32,
            vm::UserData,
            Option<vm::Object>,
        ) = exec.stack().consume(ctx)?;

        let object = ObjectUserData::downcast(object)?;

        let (create_script, instance_id, instance_ud) = State::ctx_with_mut(ctx, |state| {
            let properties = vm::Object::new(&ctx);
            if let Some(set_properties) = set_properties {
                // We only copy properties from the childmost object, the documentation of GMS2 is
                // vague on this point.
                //
                // TODO: Actually check the behavior against GMS2
                let set_properties = set_properties.borrow();
                for (&key, &value) in &set_properties.map {
                    properties.set(&ctx, key, value);
                }
            }

            let instance_id = state.instances.insert_with_id(|instance_id| Instance {
                object: object.id,
                active: true,
                destroyed: false,
                position: Vec2::new(x, y),
                rotation: 0.0,
                depth,
                this: ctx.stash(InstanceUserData::new(ctx, instance_id)),
                properties: ctx.stash(properties),
                event_closures: HashMap::new(),
                animation_time: 0.0,
            });

            for (&event, script) in state
                .scripts
                .object_events
                .get(&object.id)
                .into_iter()
                .flatten()
            {
                if event != ObjectEvent::Create {
                    state.instances[instance_id]
                        .event_closures
                        .insert(event, ctx.stash(script.create_closure(ctx)));
                }
            }

            (
                state
                    .scripts
                    .object_events
                    .get(&object.id)
                    .and_then(|evs| evs.get(&ObjectEvent::Create))
                    .cloned(),
                instance_id,
                ctx.fetch(&state.instances[instance_id].this),
            )
        })?;

        if let Some(create_script) = create_script {
            InstanceState::ctx_cell(ctx).freeze(&InstanceState { instance_id }, || {
                exec.with_this(instance_ud.into())
                    .call_closure(ctx, create_script.create_closure(ctx))
                    .map_err(|e| e.into_extern())
            })?;
        }

        exec.stack().replace(ctx, instance_ud);

        Ok(())
    });
    magic.add_constant(
        &ctx,
        ctx.intern("instance_create_depth"),
        instance_create_depth,
    )?;

    let instance_exists = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let object_or_instance: vm::UserData = exec.stack().consume(ctx)?;
        let found = if let Ok(object) = ObjectUserData::downcast(object_or_instance) {
            State::ctx_with(ctx, |state| {
                for instance in state.instances.values() {
                    if instance.object == object.id {
                        return true;
                    }
                }
                false
            })?
        } else if let Ok(instance) = InstanceUserData::downcast(object_or_instance) {
            State::ctx_with(ctx, |state| state.instances.contains(instance.id))?
        } else {
            return Err(vm::RuntimeError::msg(
                "`instance_exists` expects an object or instance",
            ));
        };
        exec.stack().replace(ctx, found);
        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("instance_exists"), instance_exists)
        .unwrap();

    let instance_deactivate_object = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let object_or_instance: vm::UserData = exec.stack().consume(ctx)?;
        if let Ok(object) = ObjectUserData::downcast(object_or_instance) {
            State::ctx_with_mut(ctx, |state| {
                for instance in state.instances.values_mut() {
                    if instance.object == object.id {
                        instance.active = false;
                    }
                }
            })?;
        } else if let Ok(instance) = InstanceUserData::downcast(object_or_instance) {
            State::ctx_with_mut(ctx, |state| {
                if let Some(instance) = state.instances.get_mut(instance.id) {
                    instance.active = false;
                }
            })?;
        } else {
            return Err(vm::RuntimeError::msg(
                "`instance_deactivate_object` expects an object or instance",
            ));
        };
        Ok(())
    });
    magic
        .add_constant(
            &ctx,
            ctx.intern("instance_deactivate_object"),
            instance_deactivate_object,
        )
        .unwrap();

    let instance_activate_region = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (left, top, width, height, inside): (f64, f64, f64, f64, bool) =
            exec.stack().consume(ctx)?;
        if !inside {
            return Err(vm::RuntimeError::msg(
                "outside instance activation unsupported",
            ));
        }
        State::ctx_with_mut(ctx, |state| {
            let mut query = BoundBoxQuery::default();
            for &instance_id in query.intersects(
                &state.instance_bound_tree,
                Box2::with_size(Vec2::new(left, top), Vec2::new(width, height)),
            ) {
                state.instances[instance_id].active = true;
            }
        })?;
        Ok(())
    });
    magic
        .add_constant(
            &ctx,
            ctx.intern("instance_activate_region"),
            instance_activate_region,
        )
        .unwrap();

    let instance_destroy = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let object_or_instance: Option<vm::UserData> = exec.stack().consume(ctx)?;
        let object_or_instance = if let Some(obj_inst) = object_or_instance {
            obj_inst
        } else {
            let instance_id = InstanceState::ctx_with(ctx, |instance| instance.instance_id)?;
            State::ctx_with(ctx, |state| ctx.fetch(&state.instances[instance_id].this))?
        };

        if let Ok(object) = ObjectUserData::downcast(object_or_instance) {
            State::ctx_with_mut(ctx, |state| {
                for instance in state.instances.values_mut() {
                    if instance.object == object.id {
                        instance.destroyed = true;
                    }
                }
            })?;
        } else if let Ok(instance) = InstanceUserData::downcast(object_or_instance) {
            State::ctx_with_mut(ctx, |state| {
                if let Some(instance) = state.instances.get_mut(instance.id) {
                    instance.destroyed = true;
                }
            })?;
        } else {
            return Err(vm::RuntimeError::msg(
                "`instance_destroy` expects an object or instance",
            ));
        };
        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("instance_destroy"), instance_destroy)
        .unwrap();

    Ok(magic)
}
