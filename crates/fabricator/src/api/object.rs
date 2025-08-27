use std::collections::HashMap;

use anyhow::Context as _;
use fabricator_math::Vec2;
use fabricator_vm as vm;
use gc_arena::{Collect, Rootable};

use crate::{
    api::{
        instance::create_instance_ud,
        magic::{DuplicateMagicName, MagicExt as _},
    },
    project::ObjectEvent,
    state::{Configuration, Instance, InstanceState, ObjectId, State},
};

pub fn no_one<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct NoOne;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            Singleton(vm::UserData::new_static(&ctx, NoOne))
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
            Singleton(vm::UserData::new_static(&ctx, All))
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

    for (object_id, object) in config.objects.iter() {
        magic.add_constant(
            &ctx,
            ctx.intern(&object.name),
            vm::UserData::new_static(&ctx, object_id),
        )?;
    }

    let instance_create_depth = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (x, y, depth, object, set_properties): (
            f64,
            f64,
            i32,
            vm::UserData,
            Option<vm::Object>,
        ) = exec.stack().consume(ctx)?;

        let object = *object.downcast_static::<ObjectId>()?;

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
                object,
                position: Vec2::new(x, y),
                rotation: 0.0,
                depth,
                this: ctx.stash(create_instance_ud(ctx, instance_id)),
                properties: ctx.stash(properties),
                event_closures: HashMap::new(),
                animation_time: 0.0,
            });

            for (&event, script) in state
                .scripts
                .object_events
                .get(&object)
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
                    .get(&object)
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
                    .context("creating an instance in `create_instance_depth`")
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

    Ok(magic)
}
