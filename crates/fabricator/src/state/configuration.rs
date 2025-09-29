use std::{
    collections::{HashMap, HashSet},
    f64,
    path::PathBuf,
    rc::Rc,
    time::Duration,
};

use fabricator_collision::{
    support::{SupportMap, SupportPoint},
    support_ext::SupportMapExt as _,
    support_maps,
};
use fabricator_math::{Box2, Vec2};
use fabricator_util::typed_id_map::{IdMap, SecondaryMap, new_id_type};
use fabricator_vm as vm;

new_id_type! {
    pub struct TextureId;
    pub struct TexturePageId;
    pub struct FontId;
    pub struct ShaderId;
    pub struct SoundId;
    pub struct TileSetId;
    pub struct ObjectId;
    pub struct SpriteId;
    pub struct InstanceTemplateId;
    pub struct RoomId;
}

pub struct Configuration {
    pub data_path: PathBuf,
    pub tick_rate: f64,

    pub sprites: IdMap<SpriteId, Rc<Sprite>>,
    pub textures: IdMap<TextureId, Rc<Texture>>,
    pub texture_pages: IdMap<TexturePageId, Rc<TexturePage>>,
    pub texture_page_for_texture: SecondaryMap<TextureId, TexturePageId>,
    pub fonts: IdMap<FontId, Rc<Font>>,
    pub shaders: IdMap<ShaderId, Rc<Shader>>,
    pub sounds: IdMap<SoundId, Rc<Sound>>,
    pub tile_sets: IdMap<TileSetId, Rc<TileSet>>,
    pub tile_set_dict: HashMap<String, TileSetId>,
    pub objects: IdMap<ObjectId, Rc<Object>>,
    pub object_dict: HashMap<String, ObjectId>,
    pub instance_templates: IdMap<InstanceTemplateId, InstanceTemplate>,
    pub rooms: IdMap<RoomId, Rc<Room>>,
    pub room_dict: HashMap<String, RoomId>,
    pub first_room: RoomId,
    pub last_room: RoomId,
}

pub struct Texture {
    pub texture_group: String,
    pub image_path: PathBuf,
    pub size: Vec2<u32>,
    pub cropped_size: Vec2<u32>,
    pub cropped_offset: Vec2<u32>,
}

pub struct TexturePage {
    pub size: Vec2<u32>,
    pub border: u32,
    pub group_name: String,
    pub group_number: usize,
    pub textures: SecondaryMap<TextureId, Vec2<u32>>,
    pub userdata: vm::StashedUserData,
}

pub struct Font {
    pub name: String,
    pub userdata: vm::StashedUserData,
}

pub struct Shader {
    pub name: String,
    pub userdata: vm::StashedUserData,
}

pub struct Sound {
    pub name: String,
    pub duration: Duration,
    pub userdata: vm::StashedUserData,
}

pub struct TileSet {
    pub name: String,
    pub tile_count: u32,
    pub userdata: vm::StashedUserData,
}

pub struct Room {
    pub name: String,
    pub size: Vec2<u32>,
    pub layers: HashMap<String, RoomLayer>,
    pub userdata: vm::StashedUserData,
    pub tags: HashSet<String>,
}

pub struct RoomLayer {
    pub name: String,
    pub depth: i32,
    pub visible: bool,
    pub layer_type: RoomLayerType,
}

pub enum RoomLayerType {
    Instances(Vec<InstanceTemplateId>),
    Assets,
    Tile(RoomTileLayer),
    Background,
}

pub struct RoomTileLayer {
    pub position: Vec2<f64>,
    pub tile_set: Option<TileSetId>,
    pub grid_dimensions: Vec2<u32>,
    pub grid: Vec<Option<u32>>,
}

pub struct AnimationFrame {
    pub texture: TextureId,
    pub frame_start: f64,
}

pub struct Object {
    pub name: String,
    pub parent: Option<ObjectId>,
    pub sprite: Option<SpriteId>,
    pub persistent: bool,
    pub userdata: vm::StashedUserData,
    pub tags: HashSet<String>,
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
    pub size: Vec2<u32>,
    pub origin: Vec2<u32>,
    pub collision: SpriteCollision,
    pub collision_rotates: bool,
    pub frames: Vec<AnimationFrame>,
    pub userdata: vm::StashedUserData,
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
