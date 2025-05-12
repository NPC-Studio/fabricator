use std::{collections::HashMap, fs::File, io::Read as _, path::PathBuf, rc::Rc};

use anyhow::{Context as _, Error, anyhow, bail};
use fabricator_compiler as compiler;
use fabricator_math::Vec2;
use fabricator_util::{
    freeze::{Freeze, Frozen},
    typed_id_map::{IdMap, SecondaryMap, new_id_type},
};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc};

use crate::{
    maxrects::MaxRects,
    project::{ObjectScript, Project},
};

new_id_type! {
    pub struct TextureId;
    pub struct TexturePageId;
}

#[derive(Debug)]
pub struct Texture {
    pub image_path: PathBuf,
    pub size: Vec2<u32>,
}

#[derive(Debug)]
pub struct TexturePage {
    pub size: Vec2<u32>,
    pub border: u32,
    pub textures: SecondaryMap<TextureId, Vec2<u32>>,
}

#[derive(Debug)]
pub struct Quad {
    pub texture: TextureId,
    pub position: Vec2<f32>,
    pub depth: i32,
}

#[derive(Debug)]
pub struct Render {
    pub quads: Vec<Quad>,
    pub room_size: Vec2<u32>,
}

impl Default for Render {
    fn default() -> Self {
        Self {
            quads: Vec::new(),
            room_size: Vec2::zero(),
        }
    }
}

impl Render {
    pub fn clear(&mut self) {
        self.quads.clear();
        self.room_size = Vec2::zero();
    }
}

pub struct Game {
    interpreter: vm::Interpreter,
    thread: vm::StashedThread,

    sprites: IdMap<SpriteId, Sprite>,
    objects: IdMap<ObjectId, Object>,
    rooms: IdMap<RoomId, Room>,

    current_room: RoomId,
    instances: IdMap<InstanceId, Instance>,

    textures: IdMap<TextureId, Texture>,
    texture_pages: IdMap<TexturePageId, TexturePage>,
}

impl Game {
    pub fn new(project: Project) -> Result<Self, Error> {
        let mut interpreter = vm::Interpreter::empty();
        let thread = interpreter.enter(|ctx| ctx.stash(vm::Thread::new(&ctx)));

        let mut sprites = IdMap::<SpriteId, Sprite>::new();
        let mut textures = IdMap::<TextureId, Texture>::new();

        let mut sprite_dict = HashMap::<String, SpriteId>::new();
        let mut texture_groups = HashMap::<String, Vec<TextureId>>::new();

        for (sprite_name, sprite) in project.sprites {
            let size = Vec2::new(sprite.width, sprite.height);

            let texture_group = texture_groups.entry(sprite.texture_group).or_default();

            let mut frames = Vec::new();
            for frame in sprite.frames {
                let texture_id = textures.insert(Texture {
                    image_path: frame.image_path,
                    size,
                });
                texture_group.push(texture_id);
                frames.push(texture_id);
            }

            let sprite_id = sprites.insert(Sprite { frames });
            sprite_dict.insert(sprite_name, sprite_id);
        }

        let mut texture_pages = IdMap::<TexturePageId, TexturePage>::new();

        for (group_name, group) in texture_groups {
            let project_texture_group = project
                .texture_groups
                .get(&group_name)
                .with_context(|| anyhow!("invalid texture group name {:?}", group_name))?;

            let border = project_texture_group.border as u32;

            let mut to_place = group
                .into_iter()
                .map(|texture_id| (texture_id, ()))
                .collect::<SecondaryMap<_, _>>();

            while !to_place.is_empty() {
                let mut packer = MaxRects::new(TEXTURE_PAGE_SIZE);

                for texture_id in to_place.ids() {
                    let padded_size = textures[texture_id].size + Vec2::splat(border * 2);
                    if padded_size[0] > TEXTURE_PAGE_SIZE[0]
                        || padded_size[1] > TEXTURE_PAGE_SIZE[1]
                    {
                        bail!(
                            "texture size {:?} is greater than the texture page size",
                            padded_size
                        );
                    }
                    packer.add(texture_id, padded_size);
                }

                let texture_page_id = texture_pages.insert(TexturePage {
                    size: TEXTURE_PAGE_SIZE,
                    textures: SecondaryMap::new(),
                    border,
                });
                let texture_page = &mut texture_pages[texture_page_id];

                let prev_place_len = to_place.len();
                for packed in packer.pack() {
                    if let Some(mut position) = packed.placement {
                        position += Vec2::splat(border);
                        texture_page.textures.insert(packed.item, position);
                        to_place.remove(packed.item);
                    }
                }

                assert!(
                    to_place.len() < prev_place_len,
                    "should always add at least a single texture per iteration"
                );
            }
        }

        let mut objects = IdMap::<ObjectId, Object>::new();
        let mut object_dict = HashMap::<String, ObjectId>::new();

        for (object_name, object) in project.objects {
            let sprite = object
                .sprite
                .map(|sprite_name| {
                    sprite_dict
                        .get(&sprite_name)
                        .copied()
                        .with_context(|| anyhow!("missing sprite named {:?}", sprite_name))
                })
                .transpose()?;
            let step_script = if let Some(script_path) = object.scripts.get(&ObjectScript::Step) {
                let script = interpreter.enter(|ctx| -> Result<_, Error> {
                    let mut code = String::new();
                    File::open(script_path)?.read_to_string(&mut code)?;

                    Ok(ctx.stash(Gc::new(&ctx, compiler::compile(&ctx, ctx.stdlib(), &code)?)))
                })?;
                Some(script)
            } else {
                None
            };
            let object_id = objects.insert(Object {
                sprite,
                step_script,
            });
            object_dict.insert(object_name, object_id);
        }

        let mut rooms = IdMap::<RoomId, Room>::new();

        for (_, room) in project.rooms {
            let mut layers = HashMap::new();

            for (layer_name, layer) in room.layers {
                let mut instances = Vec::new();

                for instance in layer.instances {
                    instances.push(InstanceTemplate {
                        object: *object_dict.get(&instance.object).with_context(|| {
                            anyhow!("missing object named {:?}", instance.object)
                        })?,
                        position: Vec2::new(instance.x, instance.y),
                    });
                }

                layers.insert(
                    layer_name,
                    Layer {
                        depth: layer.depth,
                        instances,
                    },
                );
            }

            rooms.insert(Room {
                size: Vec2::new(room.width, room.height),
                layers,
            });
        }

        let current_room = rooms
            .id_for_index(0)
            .context("must have at least one room defined")?;

        let mut instances = IdMap::<InstanceId, Instance>::new();
        for layer in rooms[current_room].layers.values() {
            for instance in &layer.instances {
                let object = &objects[instance.object];
                let step_closure = if let Some(step_script) = object.step_script.as_ref() {
                    let instance_self = InstanceSelf::default();
                    let closure = interpreter.enter(|ctx| {
                        let step_prototype = ctx.fetch(step_script);
                        let userdata = vm::UserData::new_static(&ctx, instance_self.clone());
                        userdata.set_methods(&ctx, Some(gc_arena::unsize!(Gc::new(&ctx, InstanceMethods) => dyn vm::UserDataMethods)));
                        ctx.stash(
                            vm::Closure::new(
                                &ctx,
                                step_prototype,
                                ctx.stdlib(),
                                userdata.into(),
                            )
                            .unwrap(),
                        )
                    });
                    Some((closure, instance_self))
                } else {
                    None
                };

                instances.insert(Instance {
                    object: instance.object,
                    position: instance.position,
                    depth: layer.depth,
                    step_closure,
                });
            }
        }

        Ok(Game {
            interpreter,
            thread,
            sprites,
            objects,
            rooms,
            current_room,
            instances,
            textures,
            texture_pages,
        })
    }

    pub fn texture_pages(&self) -> impl Iterator<Item = (TexturePageId, &TexturePage)> + '_ {
        self.texture_pages.iter()
    }

    pub fn texture(&self, texture_id: TextureId) -> &Texture {
        &self.textures[texture_id]
    }

    pub fn tick_rate(&self) -> f64 {
        TICK_RATE
    }

    pub fn tick(&mut self, render: &mut Render) -> Result<(), Error> {
        render.clear();

        render.room_size = self.rooms[self.current_room].size;

        for instance in self.instances.values_mut() {
            self.interpreter.enter(|ctx| -> Result<(), Error> {
                if let Some((step_closure, instance_self)) = instance.step_closure.as_ref() {
                    let instance_self = instance_self.clone();
                    let thread = ctx.fetch(&self.thread);
                    let closure = ctx.fetch(step_closure);
                    instance_self.set(instance, || {
                        thread
                            .exec(ctx, closure)
                            .map_err(|e| Error::from_boxed(e.into_boxed_err()))
                    })?;
                }
                Ok(())
            })?;

            let object = &self.objects[instance.object];
            if let Some(sprite_id) = object.sprite {
                let sprite = &self.sprites[sprite_id];
                if let Some(texture) = sprite.frames.first().copied() {
                    render.quads.push(Quad {
                        texture,
                        position: instance.position.cast(),
                        depth: instance.depth,
                    });
                }
            }
        }

        Ok(())
    }
}

// TODO: Hard coded options which are normally configured by
// 'options/<platform>/options_<platform>.yy'.
const TICK_RATE: f64 = 60.0;
const TEXTURE_PAGE_SIZE: Vec2<u32> = Vec2::new(2048, 2048);

new_id_type! {
    struct ObjectId;
    struct SpriteId;
    struct RoomId;
    struct InstanceId;
}

struct Sprite {
    frames: Vec<TextureId>,
}

struct Object {
    sprite: Option<SpriteId>,
    step_script: Option<vm::StashedPrototype>,
}

struct InstanceTemplate {
    object: ObjectId,
    position: Vec2<f64>,
}

struct Layer {
    depth: i32,
    instances: Vec<InstanceTemplate>,
}

struct Room {
    size: Vec2<u32>,
    layers: HashMap<String, Layer>,
}

struct Instance {
    object: ObjectId,
    position: Vec2<f64>,
    depth: i32,
    step_closure: Option<(vm::StashedClosure, InstanceSelf)>,
}

type InstanceSelf = Rc<Frozen<Freeze![&'freeze mut Instance]>>;

#[derive(Collect)]
#[collect(require_static)]
struct InstanceMethods;

impl<'gc> vm::UserDataMethods<'gc> for InstanceMethods {
    fn get_field(
        &self,
        _ctx: vm::Context<'gc>,
        ud: vm::UserData<'gc>,
        key: vm::String<'gc>,
    ) -> Result<vm::Value<'gc>, vm::Error> {
        let instance = ud.downcast_static::<InstanceSelf>()?;
        instance.with(|instance| match key.as_str() {
            "x" => Ok(vm::Value::Float(instance.position[0])),
            "y" => Ok(vm::Value::Float(instance.position[1])),
            _ => Err("no such field".into()),
        })?
    }

    fn set_field(
        &self,
        _ctx: vm::Context<'gc>,
        ud: vm::UserData<'gc>,
        key: vm::String<'gc>,
        value: vm::Value<'gc>,
    ) -> Result<(), vm::Error> {
        let instance = ud.downcast_static::<InstanceSelf>()?;
        let val = value
            .to_float()
            .ok_or_else(|| "field must be set to number")?;
        instance.with_mut(|instance| {
            match key.as_str() {
                "x" => {
                    instance.position[0] = val;
                }
                "y" => {
                    instance.position[1] = val;
                }
                _ => {
                    return Err("no such field".into());
                }
            }
            Ok(())
        })?
    }

    fn get_index(
        &self,
        _ctx: vm::Context<'gc>,
        _ud: vm::UserData<'gc>,
        _index: vm::Value<'gc>,
    ) -> Result<vm::Value<'gc>, vm::Error> {
        Err("no index access".into())
    }

    fn set_index(
        &self,
        _ctx: vm::Context<'gc>,
        _ud: vm::UserData<'gc>,
        _index: vm::Value<'gc>,
        _value: vm::Value<'gc>,
    ) -> Result<(), vm::Error> {
        Err("no index access".into())
    }
}
