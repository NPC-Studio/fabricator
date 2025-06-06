mod loading;
mod strip_json_trailing_commas;

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use anyhow::Error;

use self::loading::load_project;

#[derive(Debug)]
pub struct Frame {
    pub name: String,
    pub image_path: PathBuf,
}

#[derive(Debug)]
pub struct AnimationFrame {
    pub frame: String,
    pub length: f64,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum CollisionKind {
    Rectangle,
    RectangleWithRotation,
    Ellipse,
    Diamond,
}

impl CollisionKind {
    pub fn from_code(code: u32) -> Option<Self> {
        match code {
            1 => Some(Self::Rectangle),
            2 => Some(Self::Ellipse),
            3 => Some(Self::Diamond),
            5 => Some(Self::RectangleWithRotation),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Sprite {
    pub name: String,
    pub base_path: PathBuf,
    pub texture_group: String,
    pub frames: HashMap<String, Frame>,
    pub width: u32,
    pub height: u32,
    pub playback_speed: f64,
    pub playback_length: f64,
    pub origin_x: u32,
    pub origin_y: u32,
    pub collision_kind: CollisionKind,
    pub bbox_bottom: i32,
    pub bbox_left: i32,
    pub bbox_top: i32,
    pub bbox_right: i32,
    pub animation_frames: Vec<AnimationFrame>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ObjectEvent {
    Create,
    Step,
    Draw,
}

impl ObjectEvent {
    pub fn all() -> impl Iterator<Item = ObjectEvent> {
        [Self::Create, Self::Step, Self::Draw].into_iter()
    }

    pub fn path(self) -> &'static str {
        match self {
            Self::Create => "Create_0.gml",
            Self::Step => "Step_0.gml",
            Self::Draw => "Draw_0.gml",
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ScriptMode {
    Compat,
    Full,
}

#[derive(Debug)]
pub struct EventScript {
    pub event: ObjectEvent,
    pub path: PathBuf,
    pub mode: ScriptMode,
}

#[derive(Debug)]
pub struct Object {
    pub name: String,
    pub base_path: PathBuf,
    pub persistent: bool,
    pub sprite: Option<String>,
    pub event_scripts: HashMap<ObjectEvent, EventScript>,
}

#[derive(Debug)]
pub struct Instance {
    pub object: String,
    pub x: f64,
    pub y: f64,
    pub scale_x: f64,
    pub scale_y: f64,
    pub rotation: f64,
}

#[derive(Debug)]
pub struct Layer {
    pub name: String,
    pub visible: bool,
    pub depth: i32,
    pub instances: Vec<Instance>,
}

#[derive(Debug)]
pub struct Room {
    pub name: String,
    pub base_path: PathBuf,
    pub width: u32,
    pub height: u32,
    pub layers: HashMap<String, Layer>,
}

#[derive(Debug)]
pub struct TextureGroup {
    pub name: String,
    pub auto_crop: bool,
    pub border: u8,
}

#[derive(Debug)]
pub struct Project {
    pub name: String,
    pub base_path: PathBuf,
    pub texture_groups: HashMap<String, TextureGroup>,
    pub sprites: HashMap<String, Sprite>,
    pub objects: HashMap<String, Object>,
    pub rooms: HashMap<String, Room>,
    pub room_order: Vec<String>,
}

impl Project {
    pub fn load(project_file: &Path) -> Result<Project, Error> {
        load_project(project_file)
    }
}
