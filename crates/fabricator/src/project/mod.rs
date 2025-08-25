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

    pub fn file_stem(self) -> &'static str {
        match self {
            Self::Create => "Create_0",
            Self::Step => "Step_0",
            Self::Draw => "Draw_0",
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ScriptMode {
    Compat,
    Modern,
}

impl ScriptMode {
    pub fn extension(self) -> &'static str {
        match self {
            ScriptMode::Compat => "gml",
            ScriptMode::Modern => "fml",
        }
    }
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
pub struct Script {
    pub name: String,
    pub path: PathBuf,
    pub mode: ScriptMode,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FfiType {
    /// An `f64`.
    Number,
    /// A `*const ()`, usually a `CStr`.
    Pointer,
}

#[derive(Debug)]
pub struct ExtensionFunction {
    pub name: String,
    pub external_name: String,
    pub arg_types: Vec<FfiType>,
    pub return_type: FfiType,
}

#[derive(Debug)]
pub struct ExtensionFile {
    /// A list of module alternatives, one per supported platform.
    pub module_paths: Vec<PathBuf>,
    pub functions: Vec<ExtensionFunction>,
}

#[derive(Debug)]
pub struct Extension {
    pub name: String,
    pub files: Vec<ExtensionFile>,
}

#[derive(Debug)]
pub struct Glyph {
    pub character: char,
    pub x: u16,
    pub y: u16,
    pub width: u16,
    pub height: u16,
    pub offset: i16,
    pub shift: i16,
}

#[derive(Debug)]
pub struct KerningPair {
    pub first: char,
    pub second: char,
    pub amount: i16,
}

#[derive(Debug)]
pub struct Font {
    pub name: String,
    pub font_name: String,
    pub font_image_path: PathBuf,
    pub glyphs: HashMap<char, Glyph>,
    pub kerning_pairs: Vec<KerningPair>,
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
    pub scripts: HashMap<String, Script>,
    pub extensions: HashMap<String, Extension>,
    pub fonts: HashMap<String, Font>,
    pub room_order: Vec<String>,
}

impl Project {
    pub fn load(project_file: &Path) -> Result<Project, Error> {
        log::info!("loading project file {project_file:?}...");
        let project = load_project(project_file)?;
        log::info!("finished loading project file {project_file:?}!");
        Ok(project)
    }
}
