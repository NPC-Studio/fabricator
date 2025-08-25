use std::{
    collections::{HashMap, HashSet},
    f64, fmt,
    path::PathBuf,
};

use bitflags::bitflags;
use fabricator_collision::{
    bound_box_tree::BoundBoxTree,
    support::{SupportMap, SupportPoint},
    support_ext::SupportMapExt as _,
    support_maps,
};
use fabricator_math::{Box2, Vec2};
use fabricator_util::{
    freeze::{AccessError, Freeze, FreezeCell},
    typed_id_map::{IdMap, new_id_type},
};
use fabricator_vm as vm;
use gc_arena::Gc;

use crate::project::ObjectEvent;

new_id_type! {
    pub struct TextureId;
    pub struct ObjectId;
    pub struct SpriteId;
    pub struct InstanceTemplateId;
    pub struct RoomId;
    pub struct InstanceId;
}

bitflags! {
    #[derive(Debug, Copy, Clone, Default)]
    pub struct MouseButtons: u8 {
        const Left = 0b00000001;
        const Middle = 0b00000010;
        const Right = 0b00000100;
    }
}

#[derive(Debug, Clone, Default)]
pub struct InputState {
    pub mouse_position: Vec2<f32>,
    pub mouse_pressed: MouseButtons,
}

impl InputState {
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc InputStateCell {
        &ctx.singleton::<gc_arena::Static<InputStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&InputState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }
}

pub type InputStateCell = FreezeCell<Freeze![&'freeze InputState]>;

pub struct Configuration {
    pub tick_rate: f64,

    pub textures: IdMap<TextureId, Texture>,
    pub sprites: IdMap<SpriteId, Sprite>,
    pub objects: IdMap<ObjectId, Object>,
    pub instance_templates: IdMap<InstanceTemplateId, InstanceTemplate>,
    pub rooms: IdMap<RoomId, Room>,
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
        struct Identifier {
            chunk_name: vm::RefName,
            line_number: Option<vm::LineNumber>,
            function_ref_name: Option<vm::RefName>,
        }

        impl fmt::Display for Identifier {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let chunk_name = &self.chunk_name;
                match (&self.function_ref_name, &self.line_number) {
                    (Some(line_number), Some(function_ref_name)) => {
                        write!(f, "{chunk_name}:{function_ref_name}:{line_number}")
                    }
                    (Some(line_number), None) => write!(f, "{chunk_name}:{line_number}"),
                    _ => write!(f, "{chunk_name}"),
                }
            }
        }

        let prototype = ctx.fetch(&self.prototype);
        let chunk = prototype.chunk();
        match prototype.reference() {
            vm::FunctionRef::Named(ref_name, span) => Identifier {
                chunk_name: chunk.name().clone(),
                line_number: Some(chunk.line_number(span.start())),
                function_ref_name: Some(ref_name.clone()),
            },
            vm::FunctionRef::Expression(span) => Identifier {
                chunk_name: chunk.name().clone(),
                line_number: Some(chunk.line_number(span.start())),
                function_ref_name: None,
            },
            vm::FunctionRef::Chunk => Identifier {
                chunk_name: chunk.name().clone(),
                line_number: None,
                function_ref_name: None,
            },
        }
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

#[derive(Debug, Copy, Clone)]
pub enum SpriteCollisionKind {
    Rect,
    Ellipse,
    Diamond,
}

#[derive(Debug, Copy, Clone)]
pub struct SpriteCollision {
    pub kind: SpriteCollisionKind,
    pub bounds: Box2<f64>,
}

impl SpriteCollision {
    pub fn support_map(&self) -> impl SupportMap<f64, Context = Vec2<f64>> {
        enum SM<A, B, C> {
            A(A),
            B(B),
            C(C),
        }

        impl<A, B, C> SupportMap<f64> for SM<A, B, C>
        where
            A: SupportMap<f64, Context = Vec2<f64>>,
            B: SupportMap<f64, Context = Vec2<f64>>,
            C: SupportMap<f64, Context = Vec2<f64>>,
        {
            type Context = Vec2<f64>;

            fn support_point(&self, ndir: Vec2<f64>) -> SupportPoint<f64, Self::Context> {
                match self {
                    SM::A(a) => a.support_point(ndir),
                    SM::B(b) => b.support_point(ndir),
                    SM::C(c) => c.support_point(ndir),
                }
            }
        }

        match self.kind {
            SpriteCollisionKind::Rect => SM::A(support_maps::AABox(self.bounds)),
            SpriteCollisionKind::Ellipse => SM::B(support_maps::Ellipse {
                center: self.bounds.center(),
                radius: self.bounds.size() / 2.0,
            }),
            SpriteCollisionKind::Diamond => SM::C(
                support_maps::AABox(Box2::with_center(
                    Vec2::zero(),
                    self.bounds.size() / f64::consts::SQRT_2,
                ))
                .rotate(f64::consts::FRAC_PI_4)
                .translate(self.bounds.center()),
            ),
        }
    }
}

pub struct Sprite {
    pub name: String,
    pub playback_speed: f64,
    pub playback_length: f64,
    pub origin: Vec2<f64>,
    pub collision: SpriteCollision,
    pub collision_rotates: bool,
    pub frames: Vec<AnimationFrame>,
}

pub struct Object {
    pub name: String,
    pub sprite: Option<SpriteId>,
    pub persistent: bool,
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
    pub name: String,
    pub size: Vec2<u32>,
    pub layers: HashMap<String, Layer>,
}

pub struct Instance {
    pub object: ObjectId,
    pub position: Vec2<f64>,
    pub rotation: f64,
    pub depth: i32,
    pub this: vm::StashedUserData,
    pub properties: vm::StashedObject,
    pub event_closures: HashMap<ObjectEvent, vm::StashedClosure>,
    pub animation_time: f64,
}

pub struct State {
    pub config: Configuration,
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

pub enum DrawnSpriteFrame {
    CurrentAnimation,
    Frame(usize),
}

pub struct DrawnSprite {
    pub instance: InstanceId,
    pub sprite: SpriteId,
    pub sub_img: DrawnSpriteFrame,
    pub position: Vec2<f64>,
}

#[derive(Default)]
pub struct DrawingState {
    pub drawn_sprites: Vec<DrawnSprite>,
}

impl DrawingState {
    pub fn clear(&mut self) {
        self.drawn_sprites.clear();
    }

    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc DrawingStateCell {
        &ctx.singleton::<gc_arena::Static<DrawingStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&DrawingState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }

    pub fn ctx_with_mut<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&mut DrawingState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with_mut(|state| f(state))
    }
}

pub type DrawingStateCell = FreezeCell<Freeze![&'freeze mut DrawingState]>;

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
