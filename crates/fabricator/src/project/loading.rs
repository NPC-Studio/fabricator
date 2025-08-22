use std::{
    collections::{HashMap, HashSet},
    fs::{self, File},
    io::BufReader,
    path::{Path, PathBuf},
};

use anyhow::{Context as _, Error, bail};
use serde::Deserialize;
use serde_json as json;

use crate::project::{
    AnimationFrame, CollisionKind, EventScript, Extension, ExtensionFile, ExtensionFunction,
    FfiType, Frame, Instance, Layer, Object, ObjectEvent, Project, Room, Script, ScriptMode,
    Sprite, TextureGroup, strip_json_trailing_commas::StripJsonTrailingCommas,
};

pub fn load_project(project_file: &Path) -> Result<Project, Error> {
    fn load_yy<T: for<'de> Deserialize<'de>>(path: &Path) -> Result<T, Error> {
        Ok(json::from_reader(StripJsonTrailingCommas::new(
            BufReader::new(File::open(path)?),
        ))?)
    }

    let yy_project: YyProject = load_yy(project_file)?;

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
        scripts: HashMap::new(),
        extensions: HashMap::new(),
        room_order,
    };

    for resource in &yy_project.resources {
        let resource_path = project.base_path.join(&resource.id.path);
        let base_path = resource_path.parent().expect("no base path").to_owned();

        match load_yy(&resource_path)? {
            YyResource::Sprite(yy_sprite) => {
                let sprite = read_sprite(base_path, yy_sprite)?;
                project.sprites.insert(sprite.name.clone(), sprite);
            }
            YyResource::Object(yy_object) => {
                let object = read_object(base_path, yy_object)?;
                project.objects.insert(object.name.clone(), object);
            }
            YyResource::Room(yy_room) => {
                let room = read_room(base_path, yy_room)?;
                project.rooms.insert(room.name.clone(), room);
            }
            YyResource::Script(yy_script) => {
                let script = read_script(base_path, yy_script)?;
                project.scripts.insert(script.name.clone(), script);
            }
            YyResource::Extension(yy_extension) => {
                let extension = read_extension(base_path, yy_extension)?;
                project.extensions.insert(extension.name.clone(), extension);
            }
            YyResource::Other => {}
        }
    }

    Ok(project)
}

#[derive(Deserialize)]
struct YyId {
    path: String,
    name: String,
}

#[derive(Deserialize)]
struct YyResourceRef {
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
    resources: Vec<YyResourceRef>,
    #[serde(rename = "TextureGroups")]
    texture_groups: Vec<YyTextureGroup>,
    #[serde(rename = "RoomOrderNodes")]
    room_order: Vec<YyRoomNode>,
}

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
    #[serde(rename = "collisionKind")]
    collision_kind: u32,
    bbox_bottom: i32,
    bbox_left: i32,
    bbox_top: i32,
    bbox_right: i32,
    #[serde(rename = "textureGroupId")]
    texture_group: YyId,
    sequence: YySequence,
}

#[derive(Deserialize)]
struct YyObject {
    name: String,
    persistent: bool,
    #[serde(rename = "spriteId")]
    sprite_id: Option<YyId>,
}

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

#[derive(Deserialize)]
struct YyScript {
    name: String,
}

#[derive(Deserialize)]
struct YyExtensionProxyFile {
    name: String,
}

#[derive(Deserialize)]
#[serde(try_from = "u8")]
enum YyFfiType {
    String,
    Number,
}

impl Into<FfiType> for YyFfiType {
    fn into(self) -> FfiType {
        match self {
            YyFfiType::String => FfiType::Pointer,
            YyFfiType::Number => FfiType::Number,
        }
    }
}

impl TryFrom<u8> for YyFfiType {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(YyFfiType::String),
            2 => Ok(YyFfiType::Number),
            n => Err(format!("invalid FFI type code {n}")),
        }
    }
}

#[derive(Deserialize)]
struct YyExtensionFunction {
    name: String,
    #[serde(rename = "externalName")]
    external_name: String,
    args: Vec<YyFfiType>,
    #[serde(rename = "returnType")]
    return_type: YyFfiType,
}

#[derive(Deserialize)]
struct YyExtensionFile {
    filename: String,
    #[serde(rename = "ProxyFiles")]
    proxy_files: Vec<YyExtensionProxyFile>,
    functions: Vec<YyExtensionFunction>,
}

#[derive(Deserialize)]
struct YyExtension {
    name: String,
    files: Vec<YyExtensionFile>,
}

#[derive(Deserialize)]
#[serde(tag = "resourceType")]
enum YyResource {
    #[serde(rename = "GMSprite")]
    Sprite(YySprite),
    #[serde(rename = "GMObject")]
    Object(YyObject),
    #[serde(rename = "GMRoom")]
    Room(YyRoom),
    #[serde(rename = "GMScript")]
    Script(YyScript),
    #[serde(rename = "GMExtension")]
    Extension(YyExtension),
    #[serde(other)]
    Other,
}

fn read_sprite(base_path: PathBuf, yy_sprite: YySprite) -> Result<Sprite, Error> {
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

    let collision_kind = CollisionKind::from_code(yy_sprite.collision_kind)
        .context("unrecognized sprite `collisionKind`")?;

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
        collision_kind,
        bbox_bottom: yy_sprite.bbox_bottom + 1,
        bbox_left: yy_sprite.bbox_left,
        bbox_top: yy_sprite.bbox_top,
        bbox_right: yy_sprite.bbox_right + 1,
        animation_frames,
    })
}

fn read_object(base_path: PathBuf, yy_object: YyObject) -> Result<Object, Error> {
    let mut event_scripts = HashMap::new();
    for entry in fs::read_dir(&base_path)? {
        let entry = entry?;
        let path = entry.path();

        if !path.is_file() {
            continue;
        }

        for event in ObjectEvent::all() {
            let stem = path.file_stem().unwrap();
            if stem.eq_ignore_ascii_case(event.file_stem()) {
                let ext = path.extension().context("missing event script extension")?;
                if ext.eq_ignore_ascii_case("yy") {
                    continue;
                }

                let mode = if ext.eq_ignore_ascii_case(ScriptMode::Compat.extension()) {
                    ScriptMode::Compat
                } else if ext.eq_ignore_ascii_case(ScriptMode::Modern.extension()) {
                    ScriptMode::Modern
                } else {
                    bail!("unknown event script extension {:?}", ext);
                };
                event_scripts.insert(
                    event,
                    EventScript {
                        event,
                        path: path.clone(),
                        mode,
                    },
                );
            }
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

fn read_room(base_path: PathBuf, yy_room: YyRoom) -> Result<Room, Error> {
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

fn read_script(base_path: PathBuf, yy_script: YyScript) -> Result<Script, Error> {
    for entry in fs::read_dir(&base_path)? {
        let entry = entry?;
        let path = entry.path();

        if !path.is_file() {
            continue;
        }

        let stem = path.file_stem().unwrap();
        if stem.eq_ignore_ascii_case(&yy_script.name) {
            let ext = path.extension().context("missing script extension")?;
            if ext.eq_ignore_ascii_case("yy") {
                continue;
            }

            let mode = if ext.eq_ignore_ascii_case(ScriptMode::Compat.extension()) {
                ScriptMode::Compat
            } else if ext.eq_ignore_ascii_case(ScriptMode::Modern.extension()) {
                ScriptMode::Modern
            } else {
                bail!("unknown event script extension {:?}", ext);
            };
            return Ok(Script {
                name: yy_script.name,
                path,
                mode,
            });
        }
    }

    bail!(
        "could not find a script file with the stem {:?} in {:?}",
        yy_script.name,
        base_path
    );
}

fn read_extension(base_path: PathBuf, yy_extension: YyExtension) -> Result<Extension, Error> {
    let mut extension = Extension {
        name: yy_extension.name,
        files: Vec::new(),
    };

    for file in yy_extension.files {
        let module_paths: HashSet<PathBuf> = file
            .proxy_files
            .into_iter()
            .map(|f| f.name)
            .chain([file.filename])
            .map(|n| base_path.join(n))
            .collect();

        let functions = file
            .functions
            .into_iter()
            .map(|f| {
                // An empty external name means that the symbol name is the same as the regular
                // name.
                let external_name = if f.external_name.is_empty() {
                    f.name.clone()
                } else {
                    f.external_name
                };

                ExtensionFunction {
                    name: f.name,
                    external_name,
                    arg_types: f.args.into_iter().map(|t| t.into()).collect(),
                    return_type: f.return_type.into(),
                }
            })
            .collect();

        extension.files.push(ExtensionFile {
            module_paths: module_paths.into_iter().collect(),
            functions,
        });
    }

    Ok(extension)
}
