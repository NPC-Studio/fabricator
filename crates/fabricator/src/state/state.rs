use std::{
    collections::{HashMap, HashSet},
    f64, fmt,
    time::Instant,
};

use fabricator_collision::{
    bound_box_tree::BoundBoxTree, support::SupportMap, support_ext::SupportMapExt as _,
};
use fabricator_math::Vec2;
use fabricator_util::{
    freeze::{AccessError, Freeze, FreezeCell},
    typed_id_map::{IdMap, SecondaryMap, new_id_type},
};
use fabricator_vm as vm;
use gc_arena::Gc;

use crate::{
    project::ObjectEvent,
    state::configuration::{Configuration, InstanceTemplateId, ObjectId, RoomId, TextureId},
};

new_id_type! {
    pub struct TexturePageId;
    pub struct InstanceId;
}

#[derive(Debug)]
pub struct TexturePage {
    pub size: Vec2<u32>,
    pub border: u32,
    pub textures: SecondaryMap<TextureId, Vec2<u32>>,
    pub userdata: vm::StashedUserData,
}

#[derive(Clone)]
pub struct ScriptPrototype {
    prototype: vm::StashedPrototype,
}

impl ScriptPrototype {
    pub fn new<'gc>(
        ctx: vm::Context<'gc>,
        prototype: Gc<'gc, vm::Prototype<'gc>>,
    ) -> ScriptPrototype {
        assert!(!prototype.has_upvalues());
        ScriptPrototype {
            prototype: ctx.stash(prototype),
        }
    }

    pub fn identifier<'gc>(&self, ctx: vm::Context<'gc>) -> impl fmt::Display {
        ctx.fetch(&self.prototype).identifier()
    }

    pub fn create_closure<'gc>(&self, ctx: vm::Context<'gc>) -> vm::Closure<'gc> {
        vm::Closure::new(&ctx, ctx.fetch(&self.prototype), vm::Value::Undefined).unwrap()
    }
}

pub struct Scripts {
    pub magic: vm::StashedMagicSet,
    pub scripts: Vec<ScriptPrototype>,
    pub object_events: HashMap<ObjectId, HashMap<ObjectEvent, ScriptPrototype>>,
}

pub struct Instance {
    pub object: ObjectId,
    pub active: bool,
    pub destroyed: bool,
    pub position: Vec2<f64>,
    pub rotation: f64,
    pub depth: i32,
    pub this: vm::StashedUserData,
    pub properties: vm::StashedObject,
    pub event_closures: HashMap<ObjectEvent, vm::StashedClosure>,
    pub animation_time: f64,
}

pub struct State {
    pub start_instant: Instant,
    pub config: Configuration,
    pub texture_pages: IdMap<TexturePageId, TexturePage>,
    pub texture_page_for_texture: SecondaryMap<TextureId, TexturePageId>,
    pub scripts: Scripts,

    pub current_room: Option<RoomId>,
    pub next_room: Option<RoomId>,
    pub persistent_instances: HashSet<InstanceTemplateId>,
    pub instances: IdMap<InstanceId, Instance>,
    pub instance_bound_tree: BoundBoxTree<f64, InstanceId>,
}

impl State {
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc StateCell {
        &ctx.singleton::<gc_arena::Static<StateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&State) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }

    pub fn ctx_with_mut<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&mut State) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with_mut(|state| f(state))
    }

    pub fn instance_collision(
        &self,
        instance_id: InstanceId,
    ) -> Option<impl SupportMap<f64, Context = Vec2<f64>>> {
        let instance = self.instances.get(instance_id)?;

        let sprite = self
            .config
            .sprites
            .get(self.config.objects[instance.object].sprite?)?;

        let rotation = if sprite.collision_rotates {
            instance.rotation
        } else {
            0.0
        };

        Some(
            sprite
                .collision
                .support_map()
                .rotate(rotation)
                .translate(instance.position),
        )
    }
}

pub type StateCell = FreezeCell<Freeze![&'freeze mut State]>;

pub struct InstanceState {
    pub instance_id: InstanceId,
}

impl InstanceState {
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc InstanceStateCell {
        &ctx.singleton::<gc_arena::Static<InstanceStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&InstanceState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }
}

pub type InstanceStateCell = FreezeCell<Freeze![&'freeze InstanceState]>;
