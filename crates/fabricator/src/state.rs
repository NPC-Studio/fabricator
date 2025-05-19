use std::collections::HashMap;

use anyhow::{Context as _, Error};
use fabricator_math::Vec2;
use fabricator_util::{
    freeze::{AccessError, Freeze, Frozen},
    typed_id_map::{IdMap, new_id_type},
};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{project::ObjectEvent, userdata::StaticUserDataProperties};

pub struct State {
    pub sprites: IdMap<SpriteId, Sprite>,
    pub objects: IdMap<ObjectId, Object>,
    pub rooms: IdMap<RoomId, Room>,

    pub current_room: RoomId,
    pub instances: IdMap<InstanceId, Instance>,
}

new_id_type! {
    pub struct TextureId;
    pub struct ObjectId;
    pub struct SpriteId;
    pub struct RoomId;
    pub struct InstanceId;
}

pub struct Sprite {
    pub frames: Vec<TextureId>,
}

pub struct Object {
    pub sprite: Option<SpriteId>,
    pub event_scripts: HashMap<ObjectEvent, vm::StashedPrototype>,
}

pub struct InstanceTemplate {
    pub object: ObjectId,
    pub position: Vec2<f64>,
}

pub struct Layer {
    pub depth: i32,
    pub instances: Vec<InstanceTemplate>,
}

pub struct Room {
    pub size: Vec2<u32>,
    pub layers: HashMap<String, Layer>,
}

pub struct Instance {
    pub object: ObjectId,
    pub position: Vec2<f64>,
    pub depth: i32,
    pub this: vm::StashedUserData,
    pub step_closure: Option<vm::StashedClosure>,
}

impl State {
    pub fn ctx_set<'a, 'gc, R>(
        ctx: vm::Context<'gc>,
        root: &'a mut State,
        f: impl FnOnce() -> R,
    ) -> R {
        let rs = ctx.singleton::<StateSingleton>();
        rs.0.set(root, || f())
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&State) -> R,
    ) -> Result<R, AccessError> {
        let rs = ctx.singleton::<StateSingleton>();
        rs.0.with(|root| f(&**root))
    }

    pub fn ctx_with_mut<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&mut State) -> R,
    ) -> Result<R, AccessError> {
        let rs = ctx.singleton::<StateSingleton>();
        rs.0.with_mut(|root| f(&mut **root))
    }

    pub fn instance_methods<'gc>(ctx: vm::Context<'gc>) -> Gc<'gc, dyn vm::UserDataMethods<'gc>> {
        ctx.registry()
            .singleton::<Rootable![InstanceMethodsSingleton<'_>]>(ctx)
            .0
    }
}

type StateSingleton = gc_arena::Static<Frozen<Freeze![&'freeze mut State]>>;

#[derive(Collect)]
#[collect(no_drop)]
struct InstanceMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

impl<'gc> vm::Singleton<'gc> for InstanceMethodsSingleton<'gc> {
    fn create(ctx: vm::Context<'gc>) -> Self {
        let mut properties = StaticUserDataProperties::<InstanceId>::default();
        properties.add_rw_property(
            "x",
            |ctx, instance_id| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get(*instance_id)
                        .context("expired instance")?;
                    Ok(instance.position[0].into())
                })?
            },
            |ctx, instance_id, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get_mut(*instance_id)
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
            |ctx, instance_id| {
                State::ctx_with(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get(*instance_id)
                        .context("expired instance")?;
                    Ok(instance.position[1].into())
                })?
            },
            |ctx, instance_id, val| {
                State::ctx_with_mut(ctx, |root| -> Result<_, Error> {
                    let instance = root
                        .instances
                        .get_mut(*instance_id)
                        .context("expired instance")?;
                    instance.position[1] = val
                        .to_float()
                        .ok_or_else(|| Error::msg("field must be set to number"))?;
                    Ok(())
                })?
            },
        );
        Self(properties.into_methods(&ctx))
    }
}
