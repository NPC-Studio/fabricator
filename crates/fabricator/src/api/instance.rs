use std::f64;

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::{magic::MagicExt as _, userdata::StaticUserDataProperties},
    project::ObjectEvent,
    state::{InstanceId, State},
};

#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub struct InstanceUserData {
    pub id: InstanceId,
}

impl InstanceUserData {
    pub fn new<'gc>(ctx: vm::Context<'gc>, id: InstanceId) -> vm::UserData<'gc> {
        let methods = ctx.singleton::<Rootable![InstanceMethodsSingleton<'_>]>().0;
        let ud = vm::UserData::new_static::<InstanceUserData>(&ctx, InstanceUserData { id });
        ud.set_methods(&ctx, Some(methods));
        ud
    }

    pub fn downcast<'gc>(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast_static::<InstanceUserData>()
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct InstanceMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

impl<'gc> vm::Singleton<'gc> for InstanceMethodsSingleton<'gc> {
    fn create(ctx: vm::Context<'gc>) -> Self {
        let mut properties = StaticUserDataProperties::<InstanceUserData>::default();
        properties.add_rw_property(
            "x",
            |ctx, &instance| {
                State::ctx_with(ctx, |root| {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    Ok(instance.position[0].into())
                })?
            },
            |ctx, &instance, val| {
                State::ctx_with_mut(ctx, |root| {
                    let instance = root
                        .instances
                        .get_mut(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    instance.position[0] = val
                        .cast_float()
                        .ok_or_else(|| vm::RuntimeError::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.add_rw_property(
            "y",
            |ctx, &instance| {
                State::ctx_with(ctx, |root| {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    Ok(instance.position[1].into())
                })?
            },
            |ctx, &instance, val| {
                State::ctx_with_mut(ctx, |root| {
                    let instance = root
                        .instances
                        .get_mut(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    instance.position[1] = val
                        .cast_float()
                        .ok_or_else(|| vm::RuntimeError::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.add_rw_property(
            "image_angle",
            |ctx, &instance| {
                State::ctx_with(ctx, |root| {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    Ok((-instance.rotation.to_degrees()).into())
                })?
            },
            |ctx, &instance, val| {
                State::ctx_with_mut(ctx, |root| {
                    let instance = root
                        .instances
                        .get_mut(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    let angle_deg = val
                        .cast_float()
                        .ok_or_else(|| vm::RuntimeError::msg("field must be set to number"))?;
                    instance.rotation = -angle_deg.to_radians() % (f64::consts::PI * 2.0);
                    Ok(())
                })?
            },
        );

        properties.enable_custom_properties(
            |ctx, &instance, key| {
                State::ctx_with(ctx, |root| {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    Ok(ctx.fetch(&instance.properties).get(key))
                })?
            },
            |ctx, &instance, key, value| {
                State::ctx_with(ctx, |root| {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .ok_or_else(|| vm::RuntimeError::msg("expired instance"))?;
                    ctx.fetch(&instance.properties).set(&ctx, key, value);
                    Ok(())
                })?
            },
        );

        Self(properties.into_methods(&ctx))
    }
}

pub fn instance_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    #[derive(Debug, Copy, Clone)]
    enum EventType {
        Create,
        Destroy,
        CleanUp,
        Step,
        Other,
    }

    #[derive(Debug, Copy, Clone)]
    enum StepEvent {
        Normal,
        Begin,
        End,
    }

    #[derive(Debug, Copy, Clone)]
    enum OtherEvent {
        RoomStart,
        RoomEnd,
    }

    for (event_type, name) in [
        (EventType::Create, "ev_create"),
        (EventType::Destroy, "ev_destroy"),
        (EventType::CleanUp, "ev_cleanup"),
        (EventType::Step, "ev_step"),
        (EventType::Other, "ev_other"),
    ] {
        magic
            .add_constant(
                &ctx,
                ctx.intern(name),
                vm::UserData::new_static(&ctx, event_type),
            )
            .unwrap();
    }

    for (step_event, name) in [
        (StepEvent::Normal, "ev_step_normal"),
        (StepEvent::Begin, "ev_step_begin"),
        (StepEvent::End, "ev_step_end"),
    ] {
        magic
            .add_constant(
                &ctx,
                ctx.intern(name),
                vm::UserData::new_static(&ctx, step_event),
            )
            .unwrap();
    }

    for (other_event, name) in [
        (OtherEvent::RoomStart, "ev_room_start"),
        (OtherEvent::RoomEnd, "ev_room_end"),
    ] {
        magic
            .add_constant(
                &ctx,
                ctx.intern(name),
                vm::UserData::new_static(&ctx, other_event),
            )
            .unwrap();
    }

    let event_perform = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (event_type, sub_event): (vm::UserData, Option<vm::UserData>) =
            exec.stack().consume(ctx)?;

        let instance_ud: vm::UserData = vm::FromValue::from_value(ctx, exec.this())?;
        let instance_id = InstanceUserData::downcast(instance_ud)?.id;

        let event = match *event_type.downcast_static::<EventType>()? {
            EventType::Create => ObjectEvent::Create,
            EventType::Destroy => ObjectEvent::Destroy,
            EventType::CleanUp => ObjectEvent::CleanUp,
            EventType::Step => {
                match *sub_event
                    .ok_or_else(|| vm::RuntimeError::msg("expected sub-event for `ev_step`"))?
                    .downcast_static::<StepEvent>()?
                {
                    StepEvent::Normal => ObjectEvent::Step,
                    StepEvent::Begin => ObjectEvent::BeginStep,
                    StepEvent::End => ObjectEvent::EndStep,
                }
            }
            EventType::Other => {
                match *sub_event
                    .ok_or_else(|| vm::RuntimeError::msg("expected sub-event for `ev_other`"))?
                    .downcast_static::<OtherEvent>()?
                {
                    OtherEvent::RoomStart => ObjectEvent::RoomStart,
                    OtherEvent::RoomEnd => ObjectEvent::RoomEnd,
                }
            }
        };

        if let Some(closure) = State::ctx_with(ctx, |state| {
            state.instances[instance_id]
                .event_closures
                .get(&event)
                .cloned()
        })? {
            exec.with_this(instance_ud)
                .call_closure(ctx, ctx.fetch(&closure))
                .map_err(|e| e.into_extern())?;
            exec.stack().clear();
        }

        Ok(())
    });
    magic
        .add_constant(&ctx, ctx.intern("event_perform"), event_perform)
        .unwrap();

    magic
}
