use std::{
    collections::HashMap,
    fs::File,
    io::Read,
    path::{Path, PathBuf},
};

use anyhow::{Context as _, Error};
use fabricator_yy_format as yy;
use serde::Deserialize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ObjectScript {
    Step,
}

impl ObjectScript {
    pub fn all() -> impl Iterator<Item = ObjectScript> {
        [Self::Step].into_iter()
    }

    pub fn path(self) -> &'static str {
        match self {
            Self::Step => "Step_0.gml",
        }
    }
}

#[derive(Debug)]
pub struct Frame {
    pub name: String,
    pub image_path: PathBuf,
}

#[derive(Debug)]
pub struct Sprite {
    pub name: String,
    pub base_path: PathBuf,
    pub texture_group: String,
    pub frames: Vec<Frame>,
    pub width: u32,
    pub height: u32,
}

#[derive(Debug)]
pub struct Object {
    pub name: String,
    pub base_path: PathBuf,
    pub persistent: bool,
    pub sprite: Option<String>,
    pub scripts: HashMap<ObjectScript, PathBuf>,
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
        struct YyId {
            path: String,
            name: String,
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
                    #[derive(Deserialize)]
                    struct YyFrame {
                        name: String,
                    }

                    #[derive(Deserialize)]
                    struct YySprite {
                        name: String,
                        frames: Vec<YyFrame>,
                        width: u32,
                        height: u32,
                        #[serde(rename = "textureGroupId")]
                        texture_group: YyId,
                    }

                    let yy_sprite: YySprite = yy::from_value(yy_resource.into())?;

                    let mut frames = Vec::new();
                    for frame in yy_sprite.frames {
                        let image_path = base_path.join(format!("{}.png", &frame.name));
                        frames.push(Frame {
                            name: frame.name,
                            image_path,
                        })
                    }

                    let sprite = Sprite {
                        name: yy_sprite.name,
                        base_path,
                        texture_group: yy_sprite.texture_group.name,
                        frames,
                        width: yy_sprite.width,
                        height: yy_sprite.height,
                    };
                    project.sprites.insert(sprite.name.clone(), sprite);
                }
                "GMObject" => {
                    #[derive(Deserialize)]
                    struct YyObject {
                        name: String,
                        persistent: bool,
                        #[serde(rename = "spriteId")]
                        sprite_id: Option<YyId>,
                    }

                    let yy_object: YyObject = yy::from_value(yy_resource.into())?;

                    let mut scripts = HashMap::new();
                    for script in ObjectScript::all() {
                        let path = base_path.join(script.path());
                        if path.exists() {
                            scripts.insert(script, path);
                        }
                    }

                    let object = Object {
                        name: yy_object.name,
                        base_path,
                        persistent: yy_object.persistent,
                        sprite: yy_object.sprite_id.map(|i| i.name),
                        scripts,
                    };
                    project.objects.insert(object.name.clone(), object);
                }
                "GMRoom" => {
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

                    let room = Room {
                        name: yy_room.name,
                        base_path,
                        width: yy_room.room_settings.width,
                        height: yy_room.room_settings.height,
                        layers,
                    };
                    project.rooms.insert(room.name.clone(), room);
                }
                _ => {}
            }
        }

        Ok(project)
    }
}
