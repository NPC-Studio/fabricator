use std::f64;

use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::userdata::StaticUserDataProperties,
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
