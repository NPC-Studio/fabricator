use std::{
    collections::{HashMap, HashSet},
    fs::File,
    io::Read as _,
};

use anyhow::{Context as _, Error, anyhow};
use fabricator_compiler as compiler;
use fabricator_math::Vec2;
use fabricator_stdlib::StdlibContext as _;
use fabricator_util::typed_id_map::IdMap;
use fabricator_vm as vm;
use gc_arena::{Collect, Gc};

use crate::{
    project::{Project, ScriptMode},
    state::{
        AnimationFrame, Instance, InstanceId, InstanceTemplate, InstanceTemplateId, Layer, Object,
        ObjectId, Room, RoomId, Sprite, SpriteId, State, Texture, TextureId,
    },
};

impl State {
    pub fn create(interpreter: &mut vm::Interpreter, project: &Project) -> Result<Self, Error> {
        // TODO: Hard code tick rate, normally configured by 'options/main/options_main.yy'.
        const TICK_RATE: f64 = 60.0;

        let thread = interpreter.enter(|ctx| ctx.stash(vm::Thread::new(&ctx)));

        let mut sprites = IdMap::<SpriteId, Sprite>::new();
        let mut textures = IdMap::<TextureId, Texture>::new();

        let mut sprite_dict = HashMap::<String, SpriteId>::new();

        for (sprite_name, sprite) in &project.sprites {
            let size = Vec2::new(sprite.width, sprite.height);

            let mut frame_dict = HashMap::new();
            for frame in sprite.frames.values() {
                let texture_id = textures.insert(Texture {
                    texture_group: sprite.texture_group.clone(),
                    image_path: frame.image_path.clone(),
                    size,
                });
                frame_dict.insert(frame.name.clone(), texture_id);
            }

            let mut frames = Vec::new();
            let mut start_time = 0.0;
            for animation_frame in &sprite.animation_frames {
                frames.push(AnimationFrame {
                    texture: frame_dict
                        .get(&animation_frame.frame)
                        .copied()
                        .with_context(|| {
                            anyhow!("invalid frame named {:?}", animation_frame.frame)
                        })?,
                    frame_start: start_time,
                });
                start_time += animation_frame.length;
            }

            let sprite_id = sprites.insert(Sprite {
                playback_speed: sprite.playback_speed,
                playback_length: sprite.playback_length,
                origin: Vec2::new(sprite.origin_x, sprite.origin_y).cast(),
                frames,
            });
            sprite_dict.insert(sprite_name.clone(), sprite_id);
        }

        let mut objects = IdMap::<ObjectId, Object>::new();
        let mut object_dict = HashMap::<String, ObjectId>::new();

        for (object_name, object) in &project.objects {
            let sprite = object
                .sprite
                .as_ref()
                .map(|sprite_name| {
                    sprite_dict
                        .get(sprite_name)
                        .copied()
                        .with_context(|| anyhow!("missing sprite named {:?}", sprite_name))
                })
                .transpose()?;

            let object_id = objects.insert(Object {
                sprite,
                persistent: object.persistent,
                event_scripts: HashMap::new(),
            });
            object_dict.insert(object_name.clone(), object_id);
        }

        let mut rooms = IdMap::<RoomId, Room>::new();
        let mut room_dict = HashMap::<String, RoomId>::new();

        let mut instance_templates = IdMap::<InstanceTemplateId, InstanceTemplate>::new();

        for room in project.rooms.values() {
            let mut layers = HashMap::new();

            for (layer_name, layer) in &room.layers {
                let mut instances = Vec::new();

                for instance in &layer.instances {
                    let template_id = instance_templates.insert(InstanceTemplate {
                        object: *object_dict.get(&instance.object).with_context(|| {
                            anyhow!("missing object named {:?}", instance.object)
                        })?,
                        position: Vec2::new(instance.x, instance.y),
                    });
                    instances.push(template_id);
                }

                layers.insert(
                    layer_name.clone(),
                    Layer {
                        depth: layer.depth,
                        instances,
                    },
                );
            }

            let room_id = rooms.insert(Room {
                size: Vec2::new(room.width, room.height),
                layers,
            });

            room_dict.insert(room.name.clone(), room_id);
        }

        let first_room = project.room_order.first().context("no first room")?;
        let first_room = *room_dict
            .get(first_room)
            .with_context(|| "no such room `{first_room:?}`")?;

        let magic = interpreter
            .enter(|ctx| -> Result<_, vm::Error> {
                let mut magic = vm::MagicSet::new();
                for (room_name, &room_id) in &room_dict {
                    magic.add_constant(
                        &ctx,
                        vm::String::new(&ctx, room_name),
                        vm::UserData::new_static(&ctx, room_id).into(),
                    )?;
                }

                #[derive(Collect)]
                #[collect(require_static)]
                struct RoomMagic;

                impl<'gc> vm::Magic<'gc> for RoomMagic {
                    fn get(&self, ctx: vm::Context<'gc>) -> Result<vm::Value<'gc>, vm::Error> {
                        State::ctx_with(ctx, |state| {
                            Ok(vm::UserData::new_static(&ctx, state.current_room).into())
                        })?
                    }

                    fn set(
                        &self,
                        ctx: vm::Context<'gc>,
                        value: vm::Value<'gc>,
                    ) -> Result<(), vm::Error> {
                        State::ctx_with_mut(ctx, |state| {
                            let room = match value {
                                fabricator_vm::Value::UserData(user_data) => {
                                    *user_data.downcast_static::<RoomId>()?
                                }
                                _ => {
                                    return Err(vm::Error::msg(
                                        "cannot set room to non-room value",
                                    ));
                                }
                            };
                            state.next_room = Some(room);
                            Ok(())
                        })?
                    }

                    fn read_only(&self) -> bool {
                        false
                    }
                }

                let room_magic = gc_arena::unsize!(Gc::new(&ctx, RoomMagic) => dyn vm::Magic);
                magic.add(vm::String::new(&ctx, "room"), room_magic)?;

                magic.merge(&ctx.stdlib())?;

                Ok(ctx.stash(Gc::new(&ctx, magic)))
            })
            .map_err(|e| e.into_inner())?;

        for (object_name, proj_object) in &project.objects {
            let object = &mut objects[object_dict[object_name]];

            interpreter.enter(|ctx| -> Result<_, Error> {
                let mut code = String::new();
                for (&event, script) in &proj_object.event_scripts {
                    code.clear();
                    File::open(&script.path)?.read_to_string(&mut code)?;
                    let proto = match script.mode {
                        ScriptMode::Compat => {
                            compiler::compile_compat(&ctx, ctx.fetch(&magic), &code)?
                        }
                        ScriptMode::Full => compiler::compile(&ctx, ctx.fetch(&magic), &code)?,
                    };
                    object
                        .event_scripts
                        .insert(event, ctx.stash(Gc::new(&ctx, proto)));
                }

                Ok(())
            })?;
        }

        Ok(State {
            main_thread: thread.clone(),
            magic: magic.clone(),
            tick_rate: TICK_RATE,
            sprites,
            objects,
            instance_templates,
            rooms,
            current_room: None,
            next_room: Some(first_room),
            persistent_instances: HashSet::new(),
            textures,
            instances: IdMap::<InstanceId, Instance>::new(),
        })
    }
}
