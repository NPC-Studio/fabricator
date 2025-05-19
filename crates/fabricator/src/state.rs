use std::collections::HashMap;

use fabricator_math::Vec2;
use fabricator_util::{
    freeze::{AccessError, Freeze, Frozen},
    typed_id_map::{IdMap, new_id_type},
};
use fabricator_vm as vm;

use crate::project::ObjectEvent;

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

#[derive(Copy, Clone)]
pub struct InstanceTemplate {
    pub object: ObjectId,
    pub position: Vec2<f64>,
}

#[derive(Clone)]
pub struct Layer {
    pub depth: i32,
    pub instances: Vec<InstanceTemplate>,
}

#[derive(Clone)]
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
}

type StateSingleton = gc_arena::Static<Frozen<Freeze![&'freeze mut State]>>;
