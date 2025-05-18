use std::{collections::HashMap, fs::File, io::Read as _, path::PathBuf};

use anyhow::{Context as _, Error, anyhow, bail};
use fabricator_compiler as compiler;
use fabricator_math::Vec2;
use fabricator_stdlib::StdlibContext as _;
use fabricator_util::typed_id_map::{IdMap, SecondaryMap, new_id_type};
use fabricator_vm as vm;
use gc_arena::{Collect, Gc};

use crate::{
    maxrects::MaxRects,
    project::{ObjectEvent, Project, ScriptMode},
    root::{
        Instance, InstanceId, InstanceTemplate, Layer, Object, ObjectId, Room, RoomId, Root,
        Sprite, SpriteId, TextureId,
    },
};

new_id_type! {
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
    root: Root,
    textures: IdMap<TextureId, Texture>,
    texture_pages: IdMap<TexturePageId, TexturePage>,
}

impl Game {
    pub fn new(project: Project) -> Result<Self, Error> {
        let mut interpreter = vm::Interpreter::new();
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

            let mut event_scripts = HashMap::new();

            interpreter.enter(|ctx| -> Result<_, Error> {
                let mut code = String::new();
                for (&event, script) in &object.event_scripts {
                    File::open(&script.path)?.read_to_string(&mut code)?;
                    let proto = match script.mode {
                        ScriptMode::Compat => compiler::compile_compat(&ctx, ctx.stdlib(), &code)?,
                        ScriptMode::Full => compiler::compile(&ctx, ctx.stdlib(), &code)?,
                    };
                    event_scripts.insert(event, ctx.stash(Gc::new(&ctx, proto)));
                }

                Ok(())
            })?;

            let object_id = objects.insert(Object {
                sprite,
                event_scripts,
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
                let step_closure =
                    if let Some(step_script) = object.event_scripts.get(&ObjectEvent::Step) {
                        let closure = interpreter.enter(|ctx| {
                            ctx.stash(
                                vm::Closure::new(
                                    &ctx,
                                    ctx.fetch(step_script),
                                    ctx.stdlib(),
                                    vm::Value::Undefined,
                                )
                                .unwrap(),
                            )
                        });
                        Some(closure)
                    } else {
                        None
                    };

                let instance_id = instances.insert(Instance {
                    object: instance.object,
                    position: instance.position,
                    depth: layer.depth,
                    step_closure,
                });

                if let Some(create_script) = object.event_scripts.get(&ObjectEvent::Create) {
                    interpreter
                        .enter(|ctx| -> Result<_, vm::Error> {
                            let thread = ctx.fetch(&thread);
                            let instance_ud = vm::UserData::new_static(&ctx, instance_id);
                            instance_ud.set_methods(&ctx, Some(gc_arena::unsize!(Gc::new(&ctx, InstanceMethods) => dyn vm::UserDataMethods)));

                            thread.exec_with(
                                ctx,
                                vm::Closure::new(
                                    &ctx,
                                    ctx.fetch(create_script),
                                    ctx.stdlib(),
                                    vm::Value::Undefined,
                                )
                                .unwrap(),
                                instance_ud.into(),
                            )?;
                            Ok(())
                        })
                        .map_err(|e| e.into_inner())?;
                }
            }
        }

        Ok(Game {
            interpreter,
            thread,
            root: Root {
                sprites,
                objects,
                rooms,
                current_room,
                instances,
            },
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

        render.room_size = self.root.rooms[self.root.current_room].size;

        let to_update = self.root.instances.ids().collect::<Vec<_>>();

        for instance_id in to_update {
            self.interpreter.enter(|ctx| -> Result<(), Error> {
                if let Some(step_closure) = self.root.instances[instance_id].step_closure.as_ref() {
                    let thread = ctx.fetch(&self.thread);
                    let closure = ctx.fetch(step_closure);
                    let instance_ud = vm::UserData::new_static(&ctx, instance_id);
                    instance_ud.set_methods(&ctx, Some(gc_arena::unsize!(Gc::new(&ctx, InstanceMethods) => dyn vm::UserDataMethods)));

                    Root::ctx_set(ctx, &mut self.root, || {
                        thread
                            .exec_with(ctx, closure, instance_ud.into())
                            .map_err(|e| e.into_inner())
                    })?;
                }
                Ok(())
            })?;

            let instance = &self.root.instances[instance_id];

            let object = &self.root.objects[instance.object];
            if let Some(sprite_id) = object.sprite {
                let sprite = &self.root.sprites[sprite_id];
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

#[derive(Collect)]
#[collect(require_static)]
struct InstanceMethods;

impl<'gc> vm::UserDataMethods<'gc> for InstanceMethods {
    fn get_field(
        &self,
        ctx: vm::Context<'gc>,
        ud: vm::UserData<'gc>,
        key: vm::String<'gc>,
    ) -> Result<vm::Value<'gc>, vm::Error> {
        let instance_id = *ud.downcast_static::<InstanceId>()?;
        Root::ctx_with(ctx, |root| -> Result<_, vm::Error> {
            let instance = root
                .instances
                .get(instance_id)
                .context("expired instance")?;
            match key.as_str() {
                "x" => Ok(vm::Value::Float(instance.position[0])),
                "y" => Ok(vm::Value::Float(instance.position[1])),
                _ => Err(vm::Error::msg("no such field")),
            }
        })?
    }

    fn set_field(
        &self,
        ctx: vm::Context<'gc>,
        ud: vm::UserData<'gc>,
        key: vm::String<'gc>,
        value: vm::Value<'gc>,
    ) -> Result<(), vm::Error> {
        let instance_id = *ud.downcast_static::<InstanceId>()?;
        let val = value
            .to_float()
            .ok_or_else(|| vm::Error::msg("field must be set to number"))?;
        Root::ctx_with_mut(ctx, |root| -> Result<_, vm::Error> {
            let instance = root
                .instances
                .get_mut(instance_id)
                .context("expired instance")?;
            match key.as_str() {
                "x" => {
                    instance.position[0] = val;
                }
                "y" => {
                    instance.position[1] = val;
                }
                _ => {
                    return Err(vm::Error::msg("no such field"));
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
        Err(vm::Error::msg("no index access"))
    }

    fn set_index(
        &self,
        _ctx: vm::Context<'gc>,
        _ud: vm::UserData<'gc>,
        _index: vm::Value<'gc>,
        _value: vm::Value<'gc>,
    ) -> Result<(), vm::Error> {
        Err(vm::Error::msg("no index access"))
    }
}
