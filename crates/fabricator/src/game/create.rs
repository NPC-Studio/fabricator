use std::{collections::HashMap, f64, fs::File, io::Read as _};

use anyhow::{Context as _, Error, anyhow};
use fabricator_compiler as compiler;
use fabricator_math::{Box2, Vec2};
use fabricator_stdlib::StdlibContext as _;
use fabricator_util::typed_id_map::IdMap;
use fabricator_vm as vm;
use gc_arena::Gc;

use crate::{
    api::{
        collision::collision_api, drawing::drawing_api, magic::MagicExt as _, object::object_api,
        platform::platform_api, room::room_api,
    },
    ffi::load_extension_file,
    project::{CollisionKind, ObjectEvent, Project, ScriptMode},
    state::{
        AnimationFrame, Configuration, InstanceTemplate, InstanceTemplateId, Layer, Object,
        ObjectId, Room, RoomId, ScriptPrototype, Scripts, Sprite, SpriteCollision,
        SpriteCollisionKind, SpriteId, State, Texture, TextureId,
    },
};

pub fn create_state(
    interpreter: &mut vm::Interpreter,
    project: &Project,
    config_name: &str,
) -> Result<State, Error> {
    // TODO: Hard coded tick rate, normally configured by 'options/main/options_main.yy'.
    const TICK_RATE: f64 = 60.0;

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
                    .with_context(|| anyhow!("invalid frame named {:?}", animation_frame.frame))?,
                frame_start: start_time,
            });
            start_time += animation_frame.length;
        }

        let sprite_origin = Vec2::new(sprite.origin_x, sprite.origin_y).cast::<f64>();

        let collision_bounds = Box2::<f64>::new(
            Vec2::new(sprite.bbox_left, sprite.bbox_top).cast(),
            Vec2::new(sprite.bbox_right, sprite.bbox_bottom).cast(),
        )
        .translate(-sprite_origin);

        let (collision_kind, collision_rotates) = match sprite.collision_kind {
            CollisionKind::Rectangle => (SpriteCollisionKind::Rect, false),
            CollisionKind::RectangleWithRotation => (SpriteCollisionKind::Rect, true),
            CollisionKind::Ellipse => (SpriteCollisionKind::Ellipse, false),
            CollisionKind::Diamond => (SpriteCollisionKind::Diamond, false),
        };

        let sprite_id = sprites.insert(Sprite {
            name: sprite_name.clone(),
            playback_speed: sprite.playback_speed,
            playback_length: sprite.playback_length,
            origin: sprite_origin,
            collision: SpriteCollision {
                kind: collision_kind,
                bounds: collision_bounds,
            },
            collision_rotates,
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
            name: object_name.clone(),
            sprite,
            persistent: object.persistent,
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
                    object: *object_dict
                        .get(&instance.object)
                        .with_context(|| anyhow!("missing object named {:?}", instance.object))?,
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
            name: room.name.clone(),
            size: Vec2::new(room.width, room.height),
            layers,
        });

        room_dict.insert(room.name.clone(), room_id);
    }

    let config = Configuration {
        tick_rate: TICK_RATE,
        textures,
        sprites,
        objects,
        instance_templates,
        rooms,
    };

    let scripts = interpreter.enter(|ctx| -> Result<_, Error> {
        let mut object_events = HashMap::<ObjectId, HashMap<ObjectEvent, ScriptPrototype>>::new();
        let mut magic = vm::MagicSet::new();

        magic.merge_unique(&ctx.stdlib())?;

        magic.merge_unique(&platform_api(ctx))?;
        magic.merge_unique(&collision_api(ctx))?;
        magic.merge_unique(&object_api(ctx, &config)?)?;
        magic.merge_unique(&room_api(ctx, &config)?)?;
        magic.merge_unique(&drawing_api(ctx, &config)?)?;

        for extension in project.extensions.values() {
            for file in &extension.files {
                if let Some(callbacks) = load_extension_file(ctx, file)? {
                    for (name, callback) in callbacks {
                        magic.add_constant(&ctx, name, callback.into())?;
                    }
                }
            }
        }

        let magic = Gc::new(&ctx, magic);

        let mut script_compiler =
            compiler::Compiler::new(ctx, config_name, compiler::ImportItems::from_magic(magic));

        let mut scripts = project.scripts.values().collect::<Vec<_>>();

        // Compile scripts in a deterministic order (lexicographically sorted by name).
        scripts.sort_by_key(|s| &s.name);

        let mut code_buf = String::new();
        for script in scripts {
            code_buf.clear();
            File::open(&script.path)?.read_to_string(&mut code_buf)?;
            script_compiler.add_chunk(
                match script.mode {
                    ScriptMode::Compat => compiler::CompileSettings::compat(),
                    ScriptMode::Modern => compiler::CompileSettings::modern(),
                },
                script.path.to_string_lossy().into_owned(),
                &code_buf,
            )?;
        }

        let (scripts, script_imports, _) = script_compiler.compile()?;

        for (object_name, proj_object) in &project.objects {
            for (&event, script) in &proj_object.event_scripts {
                code_buf.clear();
                File::open(&script.path)?.read_to_string(&mut code_buf)?;
                let name = script.path.to_string_lossy();
                let (proto, _, _) = compiler::Compiler::compile_chunk(
                    ctx,
                    config_name,
                    script_imports,
                    match script.mode {
                        ScriptMode::Compat => compiler::CompileSettings::compat(),
                        ScriptMode::Modern => compiler::CompileSettings::modern(),
                    },
                    name.into_owned(),
                    &code_buf,
                )?;
                object_events
                    .entry(object_dict[object_name])
                    .or_default()
                    .insert(event, ScriptPrototype::new(ctx, proto));
            }
        }

        Ok(Scripts {
            magic: ctx.stash(magic),
            scripts: scripts
                .into_iter()
                .map(|proto| ScriptPrototype::new(ctx, proto))
                .collect(),
            object_events,
        })
    })?;

    let first_room = project.room_order.first().context("no first room")?;
    let first_room = *room_dict
        .get(first_room)
        .with_context(|| "no such room `{first_room:?}`")?;

    Ok(State {
        config,
        scripts,
        current_room: None,
        next_room: Some(first_room),
        persistent_instances: Default::default(),
        instances: Default::default(),
        instance_bound_tree: Default::default(),
    })
}
