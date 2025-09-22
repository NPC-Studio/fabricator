use std::{
    collections::{HashMap, HashSet},
    f64,
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

use crate::{
    project::ObjectEvent,
    state::configuration::{Configuration, InstanceTemplateId, ObjectId, RoomId, TextureId},
};

new_id_type! {
    pub struct TexturePageId;
    pub struct LayerId;
    pub struct InstanceId;
}

#[derive(Debug)]
pub struct TexturePage {
    pub size: Vec2<u32>,
    pub border: u32,
    pub textures: SecondaryMap<TextureId, Vec2<u32>>,
    pub userdata: vm::StashedUserData,
}

pub struct Scripts {
    pub magic: vm::StashedMagicSet,
    pub scripts: Vec<vm::StashedClosure>,
    pub object_events: HashMap<ObjectId, HashMap<ObjectEvent, vm::StashedClosure>>,
}

pub struct Layer {
    pub depth: i32,
    pub visible: bool,
    pub this: vm::StashedUserData,
}

pub struct Instance {
    pub object: ObjectId,
    pub active: bool,
    pub dead: bool,
    pub position: Vec2<f64>,
    pub rotation: f64,
    pub layer: LayerId,
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

    pub layers: IdMap<LayerId, Layer>,
    pub named_layers: HashMap<String, LayerId>,

    pub instances: IdMap<InstanceId, Instance>,
    pub instance_for_template: SecondaryMap<InstanceTemplateId, InstanceId>,
    pub instances_for_object: SecondaryMap<ObjectId, HashSet<InstanceId>>,
    pub instances_for_layer: SecondaryMap<LayerId, HashSet<InstanceId>>,
    pub instance_bound_tree: BoundBoxTree<f64, InstanceId>,
}

impl State {
    #[inline]
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc StateCell {
        &ctx.singleton::<gc_arena::Static<StateCell>>().0
    }

    #[inline]
    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&State) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }

    #[inline]
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

    pub fn event_closures<'gc>(
        &self,
        mut object_id: ObjectId,
    ) -> HashMap<ObjectEvent, vm::StashedClosure> {
        let mut event_closures = HashMap::new();
        loop {
            for (&event, closure) in self
                .scripts
                .object_events
                .get(&object_id)
                .into_iter()
                .flatten()
            {
                event_closures
                    .entry(event)
                    .or_insert_with(|| closure.clone());
            }

            if let Some(parent_object_id) = self.config.objects[object_id].parent {
                object_id = parent_object_id;
            } else {
                break;
            }
        }

        event_closures
    }
}

pub type StateCell = FreezeCell<Freeze![&'freeze mut State]>;
