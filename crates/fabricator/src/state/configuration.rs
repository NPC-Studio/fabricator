use std::{collections::HashMap, f64, path::PathBuf};

use fabricator_collision::{
    support::{SupportMap, SupportPoint},
    support_ext::SupportMapExt as _,
    support_maps,
};
use fabricator_math::{Box2, Vec2};
use fabricator_util::typed_id_map::{IdMap, new_id_type};

new_id_type! {
    pub struct TextureId;
    pub struct ObjectId;
    pub struct SpriteId;
    pub struct InstanceTemplateId;
    pub struct RoomId;
}

pub struct Configuration {
    pub data_path: PathBuf,
    pub tick_rate: f64,

    pub textures: IdMap<TextureId, Texture>,
    pub sprites: IdMap<SpriteId, Sprite>,
    pub objects: IdMap<ObjectId, Object>,
    pub instance_templates: IdMap<InstanceTemplateId, InstanceTemplate>,
    pub rooms: IdMap<RoomId, Room>,
}

#[derive(Debug)]
pub struct Texture {
    pub texture_group: String,
    pub image_path: PathBuf,
    pub size: Vec2<u32>,
}

#[derive(Clone)]
pub struct Room {
    pub name: String,
    pub size: Vec2<u32>,
    pub layers: HashMap<String, Layer>,
}

#[derive(Clone)]
pub struct Layer {
    pub depth: i32,
    pub instances: Vec<InstanceTemplateId>,
}

pub struct AnimationFrame {
    pub texture: TextureId,
    pub frame_start: f64,
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

pub struct Sprite {
    pub name: String,
    pub playback_speed: f64,
    pub playback_length: f64,
    pub origin: Vec2<f64>,
    pub collision: SpriteCollision,
    pub collision_rotates: bool,
    pub frames: Vec<AnimationFrame>,
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
