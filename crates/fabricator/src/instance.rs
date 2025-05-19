use anyhow::{Context as _, Error};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    state::{InstanceId, State},
    userdata::UserDataProperties,
};

#[derive(Collect)]
#[collect(no_drop)]
pub struct InstanceUserData<'gc> {
    #[collect(require_static)]
    pub id: InstanceId,
    pub custom_properties: vm::Object<'gc>,
}

impl<'gc> InstanceUserData<'gc> {
    pub fn create(ctx: vm::Context<'gc>, id: InstanceId) -> vm::UserData<'gc> {
        let methods = ctx
            .registry()
            .singleton::<Rootable![InstanceMethodsSingleton<'_>]>(ctx)
            .0;

        let ud = vm::UserData::new::<Rootable![InstanceUserData<'_>]>(
            &ctx,
            InstanceUserData {
                id,
                custom_properties: vm::Object::new(&ctx),
            },
        );

        ud.set_methods(&ctx, Some(methods));
        ud
    }
}

#[derive(Collect)]
#[collect(no_drop)]
struct InstanceMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

impl<'gc> vm::Singleton<'gc> for InstanceMethodsSingleton<'gc> {
    fn create(ctx: vm::Context<'gc>) -> Self {
        let mut properties = UserDataProperties::<Rootable![InstanceUserData<'_>]>::default();
        properties.add_rw_property(
            "x",
            |ctx, instance| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .context("expired instance")?;
                    Ok(instance.position[0].into())
                })?
            },
            |ctx, instance, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get_mut(instance.id)
                        .context("expired instance")?;
                    instance.position[0] = val
                        .to_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.add_rw_property(
            "y",
            |ctx, instance| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get(instance.id)
                        .context("expired instance")?;
                    Ok(instance.position[1].into())
                })?
            },
            |ctx, instance, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get_mut(instance.id)
                        .context("expired instance")?;
                    instance.position[1] = val
                        .to_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );

        properties.enable_custom_properties(
            |_, instance, key| Ok(instance.custom_properties.get(key)),
            |ctx, instance, key, value| {
                instance.custom_properties.set(&ctx, key, value);
                Ok(())
            },
        );

        Self(properties.into_methods(&ctx))
    }
}
