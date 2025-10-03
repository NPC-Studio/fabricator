use std::{
    collections::{HashMap, HashSet},
    fs::{self, File},
    io::BufReader,
    iter,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{Context as _, Error, bail, ensure};
use rayon::iter::{IntoParallelIterator, ParallelIterator as _};
use serde::Deserialize;
use serde_json as json;

use crate::project::{
    AnimationFrame, CollisionKind, EventScript, Extension, ExtensionFile, ExtensionFunction,
    FfiType, Font, Frame, Glyph, Instance, KerningPair, Layer, LayerType, Object, ObjectEvent,
    Project, Room, Script, ScriptMode, Shader, Sound, Sprite, TextureGroup, TileLayer, TileSet,
    strip_json_trailing_commas::StripJsonTrailingCommas,
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
        room_order,
        sprites: HashMap::new(),
        objects: HashMap::new(),
        rooms: HashMap::new(),
        scripts: HashMap::new(),
        extensions: HashMap::new(),
        fonts: HashMap::new(),
        shaders: HashMap::new(),
        sounds: HashMap::new(),
        tile_sets: HashMap::new(),
    };

    enum LoadedResource {
        Sprite(Sprite),
        Object(Object),
        Room(Room),
        Script(Script),
        Extension(Extension),
        Font(Font),
        Shader(Shader),
        Sound(Sound),
        TileSet(TileSet),
        Other,
    }

    let load_resource = |resource_path: &Path| -> Result<LoadedResource, Error> {
        let base_path = resource_path.parent().expect("no base path").to_owned();

        let resource = match load_yy(resource_path)? {
            YyResource::Sprite(yy_sprite) => {
                LoadedResource::Sprite(read_sprite(base_path, yy_sprite)?)
            }
            YyResource::Object(yy_object) => {
                LoadedResource::Object(read_object(base_path, yy_object)?)
            }
            YyResource::Room(yy_room) => LoadedResource::Room(read_room(base_path, yy_room)?),
            YyResource::Script(yy_script) => {
                LoadedResource::Script(read_script(base_path, yy_script)?)
            }
            YyResource::Extension(yy_extension) => {
                LoadedResource::Extension(read_extension(base_path, yy_extension)?)
            }
            YyResource::Font(yy_font) => LoadedResource::Font(read_font(base_path, yy_font)?),
            YyResource::Shader(yy_shader) => {
                LoadedResource::Shader(read_shader(base_path, yy_shader)?)
            }
            YyResource::Sound(yy_sound) => LoadedResource::Sound(read_sound(base_path, yy_sound)?),
            YyResource::TileSet(yy_tile_set) => {
                LoadedResource::TileSet(read_tile_set(base_path, yy_tile_set)?)
            }
            YyResource::Other => LoadedResource::Other,
        };

        log::debug!("loaded resource at {resource_path:?}");

        Ok(resource)
    };

    let loaded_resources = yy_project
        .resources
        .into_par_iter()
        .map(|resource| {
            let resource_path = project.base_path.join(&resource.id.path);
            load_resource(&resource_path)
                .with_context(|| format!("error loading resource {:?}", resource_path))
        })
        .collect::<Result<Vec<_>, Error>>()?;

    for resource in loaded_resources {
        match resource {
            LoadedResource::Sprite(sprite) => {
                project.sprites.insert(sprite.name.clone(), sprite);
            }
            LoadedResource::Object(object) => {
                project.objects.insert(object.name.clone(), object);
            }
            LoadedResource::Room(room) => {
                project.rooms.insert(room.name.clone(), room);
            }
            LoadedResource::Script(script) => {
                project.scripts.insert(script.name.clone(), script);
            }
            LoadedResource::Extension(extension) => {
                project.extensions.insert(extension.name.clone(), extension);
            }
            LoadedResource::Font(font) => {
                project.fonts.insert(font.name.clone(), font);
            }
            LoadedResource::Shader(shader) => {
                project.shaders.insert(shader.name.clone(), shader);
            }
            LoadedResource::Sound(sound) => {
                project.sounds.insert(sound.name.clone(), sound);
            }
            LoadedResource::TileSet(tile_set) => {
                project.tile_sets.insert(tile_set.name.clone(), tile_set);
            }
            LoadedResource::Other => {}
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
    #[serde(rename = "parentObjectId")]
    parent_object_id: Option<YyId>,
    #[serde(rename = "spriteId")]
    sprite_id: Option<YyId>,
    #[serde(default)]
    tags: Vec<String>,
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
struct YyLayerTiles {
    #[serde(rename = "SerialiseWidth")]
    serialize_width: u32,
    #[serde(rename = "SerialiseHeight")]
    serialize_height: u32,
    #[serde(rename = "TileSerialiseData", default)]
    serialize_data: Option<Vec<i32>>,
    #[serde(rename = "TileCompressedData", default)]
    compressed_data: Option<Vec<i32>>,
    #[serde(rename = "TileDataFormat", default)]
    data_format: Option<u32>,
}

#[derive(Deserialize)]
#[serde(tag = "resourceType")]
enum YyLayer {
    #[serde(rename = "GMRAssetLayer")]
    Asset {
        name: String,
        depth: i32,
        visible: bool,
    },
    #[serde(rename = "GMRInstanceLayer")]
    Instance {
        name: String,
        depth: i32,
        visible: bool,
        instances: Vec<YyInstance>,
    },
    #[serde(rename = "GMRTileLayer")]
    Tile {
        name: String,
        depth: i32,
        visible: bool,
        x: f64,
        y: f64,
        #[serde(rename = "gridX")]
        grid_x: u32,
        #[serde(rename = "gridY")]
        grid_y: u32,
        tiles: YyLayerTiles,
        #[serde(rename = "tilesetId")]
        tile_set_id: Option<YyId>,
    },
    #[serde(rename = "GMRBackgroundLayer")]
    Background {
        name: String,
        depth: i32,
        visible: bool,
    },
    #[serde(rename = "GMRLayer")]
    Layer { layers: Vec<YyLayer> },
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
    #[serde(default)]
    tags: Vec<String>,
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

impl From<YyFfiType> for FfiType {
    fn from(yy_ffi: YyFfiType) -> FfiType {
        match yy_ffi {
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
struct YyGlyph {
    character: u32,
    x: u16,
    y: u16,
    w: u16,
    h: u16,
    offset: i16,
    shift: i16,
}

#[derive(Deserialize)]
struct YyKerningPair {
    first: u32,
    second: u32,
    amount: i16,
}

#[derive(Deserialize)]
struct YyFont {
    name: String,
    #[serde(rename = "fontName")]
    font_name: String,
    glyphs: HashMap<String, YyGlyph>,
    #[serde(rename = "kerningPairs")]
    kerning_pairs: Vec<YyKerningPair>,
}

#[derive(Deserialize)]
struct YyShader {
    name: String,
}

#[derive(Deserialize)]
struct YySound {
    name: String,
    duration: f64,
    #[serde(rename = "sampleRate")]
    sample_rate: u32,
    #[serde(rename = "soundFile")]
    sound_file: String,
}

#[derive(Deserialize)]
struct YyTileSet {
    name: String,
    #[serde(rename = "spriteId")]
    sprite_id: YyId,

    #[serde(rename = "tileWidth")]
    tile_width: u32,
    #[serde(rename = "tileHeight")]
    tile_height: u32,
    #[serde(rename = "tilehsep")]
    tile_hsep: u32,
    #[serde(rename = "tilevsep")]
    tile_vsep: u32,
    #[serde(rename = "tilexoff")]
    tile_xoff: u32,
    #[serde(rename = "tileyoff")]
    tile_yoff: u32,
    tile_count: u32,

    out_columns: u32,
    out_tilehborder: u32,
    out_tilevborder: u32,
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
    #[serde(rename = "GMFont")]
    Font(YyFont),
    #[serde(rename = "GMShader")]
    Shader(YyShader),
    #[serde(rename = "GMSound")]
    Sound(YySound),
    #[serde(rename = "GMTileSet")]
    TileSet(YyTileSet),
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
        parent_object: yy_object.parent_object_id.map(|i| i.name),
        base_path,
        persistent: yy_object.persistent,
        sprite: yy_object.sprite_id.map(|i| i.name),
        event_scripts,
        tags: yy_object.tags.into_iter().collect(),
    })
}

fn read_room(base_path: PathBuf, yy_room: YyRoom) -> Result<Room, Error> {
    fn decode_tile_data(
        &YyLayerTiles {
            serialize_width,
            serialize_height,
            ref serialize_data,
            ref compressed_data,
            data_format,
        }: &YyLayerTiles,
    ) -> Result<Vec<Option<u32>>, Error> {
        let mut out = Vec::new();

        ensure!(
            serialize_data.is_some() && compressed_data.is_none()
                || serialize_data.is_none() && compressed_data.is_some(),
            "exactly one of `TileSerialiseData` or `TileCompressedData` is expected"
        );

        if let Some(serialize_data) = serialize_data {
            assert!(compressed_data.is_none());
            ensure!(
                serialize_data.is_empty(),
                "`TileSerialisedData` unsupported"
            );
        } else {
            let Some(compressed_data) = compressed_data else {
                unreachable!();
            };

            if let Some(data_format) = data_format {
                ensure!(
                    data_format == 1,
                    "unrecognized tile data format {data_format}"
                );
            } else {
                bail!("`data_format` required for compressed tile data");
            }

            fn get_value(value: i32) -> Result<Option<u32>, Error> {
                Ok(if value == i32::MIN {
                    None
                } else {
                    ensure!(
                        value >= 0,
                        "tile data value {value} is expected to be positive"
                    );
                    Some(value as u32)
                })
            }

            let mut read_index = 0;
            while compressed_data.len() > read_index {
                let length = compressed_data[read_index];
                read_index += 1;

                if length < 0 {
                    // This chunk is a run-length encoding of a block of single tiles
                    let run_length = -length as usize;

                    ensure!(
                        compressed_data.len() > read_index,
                        "run-length block must have a value"
                    );

                    let value = get_value(compressed_data[read_index])?;
                    out.extend(iter::repeat_n(value, run_length));
                    read_index += 1;
                } else {
                    // This chunk is a set of individual tile values.
                    let literal_length = length as usize;

                    ensure!(
                        compressed_data.len() >= read_index + literal_length,
                        "literal block does not have {literal_length} values following"
                    );

                    for i in 0..literal_length {
                        out.push(get_value(compressed_data[read_index + i])?);
                    }
                    read_index += literal_length;
                }
            }

            ensure!(
                out.len() == serialize_width as usize * serialize_height as usize,
                "tile data does not fit expected dimensions {serialize_width}x{serialize_height}"
            );
        }

        Ok(out)
    }

    fn read_layer(layer_map: &mut HashMap<String, Layer>, yy_layer: YyLayer) -> Result<(), Error> {
        match yy_layer {
            YyLayer::Asset {
                name,
                depth,
                visible,
            } => {
                layer_map.insert(
                    name.clone(),
                    Layer {
                        name,
                        visible,
                        depth,
                        layer_type: LayerType::Assets,
                    },
                );
            }
            YyLayer::Instance {
                name,
                depth,
                visible,
                instances: yy_instances,
            } => {
                let mut instances = Vec::new();
                for yy_instance in yy_instances {
                    instances.push(Instance {
                        object: yy_instance.object_id.name,
                        x: yy_instance.x,
                        y: yy_instance.y,
                        scale_x: yy_instance.scale_x,
                        scale_y: yy_instance.scale_y,
                        rotation: yy_instance.rotation,
                    });
                }
                layer_map.insert(
                    name.clone(),
                    Layer {
                        name,
                        visible,
                        depth,
                        layer_type: LayerType::Instances(instances),
                    },
                );
            }
            YyLayer::Tile {
                name,
                depth,
                visible,
                x,
                y,
                grid_x,
                grid_y,
                tiles,
                tile_set_id,
            } => {
                layer_map.insert(
                    name.clone(),
                    Layer {
                        name,
                        visible,
                        depth,
                        layer_type: LayerType::Tile(TileLayer {
                            x,
                            y,
                            tile_set: tile_set_id.map(|id| id.name),
                            cell_width: grid_x,
                            cell_height: grid_y,
                            grid_width: tiles.serialize_width,
                            grid_height: tiles.serialize_height,
                            tile_grid: decode_tile_data(&tiles)?,
                        }),
                    },
                );
            }
            YyLayer::Background {
                name,
                depth,
                visible,
            } => {
                layer_map.insert(
                    name.clone(),
                    Layer {
                        name,
                        visible,
                        depth,
                        layer_type: LayerType::Background,
                    },
                );
            }
            YyLayer::Layer { layers } => {
                for layer in layers {
                    read_layer(layer_map, layer)?;
                }
            }
        }

        Ok(())
    }

    let mut layers = HashMap::new();
    for yy_layer in yy_room.layers {
        read_layer(&mut layers, yy_layer)?;
    }

    Ok(Room {
        name: yy_room.name,
        base_path,
        width: yy_room.room_settings.width,
        height: yy_room.room_settings.height,
        layers,
        tags: yy_room.tags.into_iter().collect(),
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

fn read_font(base_path: PathBuf, yy_font: YyFont) -> Result<Font, Error> {
    let font_image_path = base_path.join(format!("{}.png", yy_font.name));
    let mut font = Font {
        name: yy_font.name,
        font_name: yy_font.font_name,
        font_image_path,
        glyphs: HashMap::new(),
        kerning_pairs: Vec::new(),
    };

    fn convert_char(c: u32) -> Result<char, Error> {
        char::from_u32(c).with_context(|| format!("invalid character with number {c:?}"))
    }

    for (_, glyph) in yy_font.glyphs {
        let character = convert_char(glyph.character)?;

        font.glyphs.insert(
            character,
            Glyph {
                character,
                x: glyph.x,
                y: glyph.y,
                width: glyph.w,
                height: glyph.h,
                offset: glyph.offset,
                shift: glyph.shift,
            },
        );
    }

    for kerning_pair in yy_font.kerning_pairs {
        font.kerning_pairs.push(KerningPair {
            first: convert_char(kerning_pair.first)?,
            second: convert_char(kerning_pair.second)?,
            amount: kerning_pair.amount,
        });
    }

    Ok(font)
}

fn read_shader(base_path: PathBuf, yy_shader: YyShader) -> Result<Shader, Error> {
    let fragment_shader_path = base_path.join(format!("{}.fsh", yy_shader.name));
    let vertex_shader_path = base_path.join(format!("{}.vsh", yy_shader.name));

    Ok(Shader {
        name: yy_shader.name,
        fragment_shader: fragment_shader_path,
        vertex_shader: vertex_shader_path,
    })
}

fn read_sound(base_path: PathBuf, yy_sound: YySound) -> Result<Sound, Error> {
    Ok(Sound {
        name: yy_sound.name,
        duration: Duration::from_secs_f64(yy_sound.duration),
        sample_rate: yy_sound.sample_rate,
        sound_file: base_path.join(&yy_sound.sound_file),
    })
}

fn read_tile_set(base_path: PathBuf, yy_tile_set: YyTileSet) -> Result<TileSet, Error> {
    let output_image = base_path.join("output_tileset.png").to_owned();
    Ok(TileSet {
        name: yy_tile_set.name,
        sprite: yy_tile_set.sprite_id.name,
        tile_width: yy_tile_set.tile_width,
        tile_height: yy_tile_set.tile_height,
        tile_horiz_separation: yy_tile_set.tile_hsep,
        tile_vert_separation: yy_tile_set.tile_vsep,
        tile_x_offset: yy_tile_set.tile_xoff,
        tile_y_offset: yy_tile_set.tile_yoff,
        tile_count: yy_tile_set.tile_count,
        output_image,
        output_columns: yy_tile_set.out_columns,
        output_tile_horiz_border: yy_tile_set.out_tilehborder,
        output_tile_vert_border: yy_tile_set.out_tilevborder,
    })
}
