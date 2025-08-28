use std::f64;

use anyhow::{Context as _, Error};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::userdata::StaticUserDataProperties,
    state::{InstanceId, State},
};

pub fn create_instance_ud<'gc>(ctx: vm::Context<'gc>, id: InstanceId) -> vm::UserData<'gc> {
    let methods = ctx.singleton::<Rootable![InstanceMethodsSingleton<'_>]>().0;

    let ud = vm::UserData::new_static::<InstanceId>(&ctx, id);

    ud.set_methods(&ctx, Some(methods));
    ud
}

#[derive(Collect)]
#[collect(no_drop)]
struct InstanceMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

impl<'gc> vm::Singleton<'gc> for InstanceMethodsSingleton<'gc> {
    fn create(ctx: vm::Context<'gc>) -> Self {
        let mut properties = StaticUserDataProperties::<InstanceId>::default();
        properties.add_rw_property(
            "x",
            |ctx, &id| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get(id).context("expired instance")?;
                    Ok(instance.position[0].into())
                })?
            },
            |ctx, &id, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get_mut(id).context("expired instance")?;
                    instance.position[0] = val
                        .cast_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.add_rw_property(
            "y",
            |ctx, &id| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get(id).context("expired instance")?;
                    Ok(instance.position[1].into())
                })?
            },
            |ctx, &id, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get_mut(id).context("expired instance")?;
                    instance.position[1] = val
                        .cast_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.add_rw_property(
            "image_angle",
            |ctx, &id| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get(id).context("expired instance")?;
                    Ok((-instance.rotation.to_degrees()).into())
                })?
            },
            |ctx, &id, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get_mut(id).context("expired instance")?;
                    let angle_deg = val
                        .cast_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    instance.rotation = -angle_deg.to_radians() % (f64::consts::PI * 2.0);
                    Ok(())
                })?
            },
        );

        properties.enable_custom_properties(
            |ctx, &id, key| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get(id).context("expired instance")?;
                    Ok(ctx.fetch(&instance.properties).get(key))
                })?
            },
            |ctx, &id, key, value| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root.instances.get(id).context("expired instance")?;
                    ctx.fetch(&instance.properties).set(&ctx, key, value);
                    Ok(())
                })?
            },
        );

        Self(properties.into_methods(&ctx))
    }
}
