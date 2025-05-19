pub mod create;
pub mod instance;
pub mod update;

use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use fabricator_math::Vec2;
use fabricator_util::{
    freeze::{AccessError, Freeze, Frozen},
    typed_id_map::{IdMap, new_id_type},
};
use fabricator_vm as vm;

use crate::project::ObjectEvent;

new_id_type! {
    pub struct TextureId;
    pub struct ObjectId;
    pub struct SpriteId;
    pub struct InstanceTemplateId;
    pub struct RoomId;
    pub struct InstanceId;
}

pub struct State {
    pub main_thread: vm::StashedThread,
    pub magic: vm::StashedMagicSet,
    pub tick_rate: f64,

    pub textures: IdMap<TextureId, Texture>,
    pub sprites: IdMap<SpriteId, Sprite>,
    pub objects: IdMap<ObjectId, Object>,
    pub instance_templates: IdMap<InstanceTemplateId, InstanceTemplate>,
    pub rooms: IdMap<RoomId, Room>,

    pub current_room: RoomId,
    pub next_room: Option<RoomId>,
    pub persistent_instances: HashSet<InstanceTemplateId>,
    pub instances: IdMap<InstanceId, Instance>,
}

#[derive(Debug)]
pub struct Texture {
    pub texture_group: String,
    pub image_path: PathBuf,
    pub size: Vec2<u32>,
}

pub struct AnimationFrame {
    pub texture: TextureId,
    pub frame_start: f64,
}

pub struct Sprite {
    pub playback_speed: f64,
    pub playback_length: f64,
    pub origin: Vec2<f64>,
    pub frames: Vec<AnimationFrame>,
}

pub struct Object {
    pub sprite: Option<SpriteId>,
    pub persistent: bool,
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
    pub instances: Vec<InstanceTemplateId>,
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
    pub animation_time: f64,
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
