use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use anyhow::{Context as _, Error, bail};
use fabricator_yy_format as yy;
use serde::Deserialize;

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
    pub animation_frames: Vec<AnimationFrame>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ObjectEvent {
    Create,
    Step,
}

impl ObjectEvent {
    pub fn all() -> impl Iterator<Item = ObjectEvent> {
        [Self::Create, Self::Step].into_iter()
    }

    pub fn path(self) -> &'static str {
        match self {
            Self::Create => "Create_0.gml",
            Self::Step => "Step_0.gml",
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
        fn load_yy_val(path: &Path) -> Result<yy::Value, Error> {
            let mut file = File::open(path)?;
            let mut buf = String::new();
            file.read_to_string(&mut buf)?;
            Ok(yy::Value::parse(&buf)?)
        }

        #[derive(Deserialize)]
        struct YyResource {
            id: YyId,
        }

        #[derive(Deserialize)]
        struct YyRoomNode {
            #[serde(rename = "roomId")]
            room_id: YyId,
        }

        #[derive(Deserialize)]
        struct YyTextureGroup {
            name: String,
            autocrop: bool,
            border: u8,
        }

        #[derive(Deserialize)]
        struct YyProject {
            name: String,
            resources: Vec<YyResource>,
            #[serde(rename = "TextureGroups")]
            texture_groups: Vec<YyTextureGroup>,
            #[serde(rename = "RoomOrderNodes")]
            room_order: Vec<YyRoomNode>,
        }

        let yy_project: YyProject = yy::from_value(load_yy_val(project_file)?)?;

        let mut texture_groups = HashMap::new();
        for yytg in yy_project.texture_groups {
            let tg = TextureGroup {
                name: yytg.name,
                auto_crop: yytg.autocrop,
                border: yytg.border,
            };
            texture_groups.insert(tg.name.clone(), tg);
        }

        let room_order = yy_project
            .room_order
            .into_iter()
            .map(|i| i.room_id.name)
            .collect();

        let mut project = Project {
            name: yy_project.name,
            base_path: project_file.parent().expect("no base path").to_owned(),
            texture_groups,
            sprites: HashMap::new(),
            objects: HashMap::new(),
            rooms: HashMap::new(),
            room_order,
        };

        for resource in &yy_project.resources {
            let resource_path = project.base_path.join(&resource.id.path);

            let yy_resource = load_yy_val(&resource_path)?
                .into_object()
                .context("resource yy is not object")?;

            let base_path = resource_path.parent().expect("no base path").to_owned();

            let resource_type = yy_resource
                .get("resourceType")
                .and_then(|v| v.as_string())
                .context("'resourceType' field is not a string")?;

            match resource_type.as_str() {
                "GMSprite" => {
                    let sprite = read_sprite(base_path, yy_resource)?;
                    project.sprites.insert(sprite.name.clone(), sprite);
                }
                "GMObject" => {
                    let object = read_object(base_path, yy_resource)?;
                    project.objects.insert(object.name.clone(), object);
                }
                "GMRoom" => {
                    let room = read_room(base_path, yy_resource)?;
                    project.rooms.insert(room.name.clone(), room);
                }
                _ => {}
            }
        }

        Ok(project)
    }
}

#[derive(Deserialize)]
struct YyId {
    path: String,
    name: String,
}

fn read_sprite(base_path: PathBuf, yy_resource: yy::Object) -> Result<Sprite, Error> {
    #[derive(Deserialize)]
    struct YyFrame {
        name: String,
    }

    #[derive(Deserialize)]
    struct YyKeyFrameChannel {
        #[serde(rename = "Id")]
        id: YyId,
    }

    #[derive(Deserialize)]
    struct YyKeyFrame {
        #[serde(rename = "Length")]
        length: f64,
        #[serde(rename = "Channels")]
        channels: HashMap<String, YyKeyFrameChannel>,
    }

    #[derive(Deserialize)]
    struct YyKeyFramesStore {
        #[serde(rename = "Keyframes")]
        keyframes: Vec<YyKeyFrame>,
    }

    #[derive(Deserialize)]
    struct YyTrack {
        name: String,
        keyframes: YyKeyFramesStore,
    }

    #[derive(Deserialize)]
    struct YySequence {
        #[serde(rename = "playbackSpeed")]
        playback_speed: f64,
        length: f64,
        tracks: Vec<YyTrack>,
        xorigin: u32,
        yorigin: u32,
    }

    #[derive(Deserialize)]
    struct YySprite {
        name: String,
        frames: Vec<YyFrame>,
        width: u32,
        height: u32,
        #[serde(rename = "textureGroupId")]
        texture_group: YyId,
        sequence: YySequence,
    }

    let yy_sprite: YySprite = yy::from_value(yy_resource.into())?;

    let mut frames = HashMap::new();
    for frame in yy_sprite.frames {
        let image_path = base_path.join(format!("{}.png", &frame.name));
        frames.insert(
            frame.name.clone(),
            Frame {
                name: frame.name,
                image_path,
            },
        );
    }

    let mut animation_frames = Vec::new();

    let mut tracks = yy_sprite
        .sequence
        .tracks
        .into_iter()
        .map(|t| (t.name.clone(), t))
        .collect::<HashMap<_, _>>();
    let frames_track = tracks
        .remove("frames")
        .context("no track named `frames` found")?;

    for mut keyframe in frames_track.keyframes.keyframes {
        let channel = keyframe
            .channels
            .remove("0")
            .context("no channel named `0` found")?;
        animation_frames.push(AnimationFrame {
            frame: channel.id.name,
            length: keyframe.length,
        });
    }

    Ok(Sprite {
        name: yy_sprite.name,
        base_path,
        texture_group: yy_sprite.texture_group.name,
        frames,
        width: yy_sprite.width,
        height: yy_sprite.height,
        playback_speed: yy_sprite.sequence.playback_speed,
        playback_length: yy_sprite.sequence.length,
        origin_x: yy_sprite.sequence.xorigin,
        origin_y: yy_sprite.sequence.yorigin,
        animation_frames,
    })
}

fn read_object(base_path: PathBuf, yy_resource: yy::Object) -> Result<Object, Error> {
    #[derive(Deserialize)]
    struct YyObject {
        name: String,
        persistent: bool,
        #[serde(rename = "spriteId")]
        sprite_id: Option<YyId>,
    }

    let yy_object: YyObject = yy::from_value(yy_resource.into())?;

    let mut event_scripts = HashMap::new();
    for event in ObjectEvent::all() {
        let path = base_path.join(event.path());
        let ext = path.extension().context("missing script extension")?;
        let mode = if ext.eq_ignore_ascii_case("gml") {
            ScriptMode::Compat
        } else if ext.eq_ignore_ascii_case("fml") {
            ScriptMode::Full
        } else {
            bail!("unknown script extension {:?}", ext);
        };
        if path.exists() {
            event_scripts.insert(event, EventScript { event, path, mode });
        }
    }

    Ok(Object {
        name: yy_object.name,
        base_path,
        persistent: yy_object.persistent,
        sprite: yy_object.sprite_id.map(|i| i.name),
        event_scripts,
    })
}

fn read_room(base_path: PathBuf, yy_resource: yy::Object) -> Result<Room, Error> {
    #[derive(Deserialize)]
    struct YyInstance {
        #[serde(rename = "objectId")]
        object_id: YyId,
        x: f64,
        y: f64,
        #[serde(rename = "scaleX")]
        scale_x: f64,
        #[serde(rename = "scaleY")]
        scale_y: f64,
        rotation: f64,
    }

    #[derive(Deserialize)]
    struct YyLayer {
        name: String,
        depth: i32,
        visible: bool,
        #[serde(default)]
        instances: Vec<YyInstance>,
    }

    #[derive(Deserialize)]
    struct YyRoomSettings {
        #[serde(rename = "Width")]
        width: u32,
        #[serde(rename = "Height")]
        height: u32,
    }

    #[derive(Deserialize)]
    struct YyRoom {
        name: String,
        #[serde(rename = "roomSettings")]
        room_settings: YyRoomSettings,
        layers: Vec<YyLayer>,
    }

    let yy_room: YyRoom = yy::from_value(yy_resource.into())?;

    let mut layers = HashMap::new();

    for yy_layer in yy_room.layers {
        let mut instances = Vec::new();
        for yy_instance in yy_layer.instances {
            instances.push(Instance {
                object: yy_instance.object_id.name,
                x: yy_instance.x,
                y: yy_instance.y,
                scale_x: yy_instance.scale_x,
                scale_y: yy_instance.scale_y,
                rotation: yy_instance.rotation,
            });
        }

        let layer = Layer {
            name: yy_layer.name,
            depth: yy_layer.depth,
            visible: yy_layer.visible,
            instances,
        };

        layers.insert(layer.name.clone(), layer);
    }

    Ok(Room {
        name: yy_room.name,
        base_path,
        width: yy_room.room_settings.width,
        height: yy_room.room_settings.height,
        layers,
    })
}
