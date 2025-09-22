use std::{collections::HashMap, f64, fs::File, io::Read as _, time::Instant};

use anyhow::{Context as _, Error, anyhow, bail};
use fabricator_compiler as compiler;
use fabricator_math::{Box2, Vec2};
use fabricator_stdlib::StdlibContext as _;
use fabricator_util::typed_id_map::{IdMap, SecondaryMap};
use fabricator_vm as vm;
use gc_arena::Gc;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::{
    api::{
        asset::assets_api,
        collision::collision_api,
        drawing::{
            ShaderUserData, SpriteUserData, TexturePageUserData, TileSetUserData, drawing_api,
        },
        font::{FontUserData, font_api},
        instance::instance_api,
        layer::layers_api,
        magic::MagicExt as _,
        object::{ObjectUserData, object_api},
        os::os_api,
        platform::platform_api,
        room::{RoomUserData, room_api},
        sound::{SoundUserData, sound_api},
        stub::stub_api,
        tile::tiles_api,
    },
    ffi::load_extension_file,
    game::maxrects::MaxRects,
    project::{CollisionKind, LayerType, ObjectEvent, Project, ScriptMode},
    state::{
        AnimationFrame, Configuration, InstanceTemplate, InstanceTemplateId, Object, ObjectId,
        Room, RoomId, RoomLayer, Scripts, Sprite, SpriteCollision, SpriteCollisionKind, SpriteId,
        State, Texture, TextureId, TexturePage, TexturePageId,
        configuration::{
            Font, FontId, RoomLayerType, Shader, ShaderId, Sound, SoundId, TileSet, TileSetId,
        },
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

        let sprite_size = Vec2::new(sprite.width, sprite.height);
        let sprite_origin = Vec2::new(sprite.origin_x, sprite.origin_y);

        let collision_bounds = Box2::<f64>::new(
            Vec2::new(sprite.bbox_left, sprite.bbox_top).cast(),
            Vec2::new(sprite.bbox_right, sprite.bbox_bottom).cast(),
        )
        .translate(-sprite_origin.cast::<f64>());

        let (collision_kind, collision_rotates) = match sprite.collision_kind {
            CollisionKind::Rectangle => (SpriteCollisionKind::Rect, false),
            CollisionKind::RectangleWithRotation => (SpriteCollisionKind::Rect, true),
            CollisionKind::Ellipse => (SpriteCollisionKind::Ellipse, false),
            CollisionKind::Diamond => (SpriteCollisionKind::Diamond, false),
        };

        let sprite_id = sprites.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(SpriteUserData::new(ctx, id, ctx.intern(sprite_name))));
            Sprite {
                name: sprite_name.clone(),
                playback_speed: sprite.playback_speed,
                playback_length: sprite.playback_length,
                size: sprite_size,
                origin: sprite_origin,
                collision: SpriteCollision {
                    kind: collision_kind,
                    bounds: collision_bounds,
                },
                collision_rotates,
                frames,
                userdata,
            }
        });
        sprite_dict.insert(sprite_name.clone(), sprite_id);
    }

    let mut fonts = IdMap::<FontId, Font>::new();
    for font in project.fonts.values() {
        fonts.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(FontUserData::new(ctx, id, ctx.intern(&font.name))));
            Font {
                name: font.name.clone(),
                userdata,
            }
        });
    }

    let mut sounds = IdMap::<SoundId, Sound>::new();
    for sound in project.sounds.values() {
        sounds.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(SoundUserData::new(ctx, id, ctx.intern(&sound.name))));
            Sound {
                name: sound.name.clone(),
                duration: sound.duration,
                userdata,
            }
        });
    }

    let mut shaders = IdMap::<ShaderId, Shader>::new();
    for shader in project.shaders.values() {
        shaders.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(ShaderUserData::new(ctx, id, ctx.intern(&shader.name))));
            Shader {
                name: shader.name.clone(),
                userdata,
            }
        });
    }

    let mut tile_sets = IdMap::<TileSetId, TileSet>::new();
    for tile_set in project.tile_sets.values() {
        tile_sets.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(TileSetUserData::new(ctx, id, ctx.intern(&tile_set.name))));
            TileSet {
                name: tile_set.name.clone(),
                userdata,
            }
        });
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

        let object_id = objects.insert_with_id(|id| {
            let userdata = interpreter
                .enter(|ctx| ctx.stash(ObjectUserData::new(ctx, id, ctx.intern(&object_name))));
            Object {
                name: object_name.clone(),
                parent: None,
                sprite,
                persistent: object.persistent,
                userdata,
                tags: object.tags.clone(),
            }
        });
        if object_dict.insert(object_name.clone(), object_id).is_some() {
            bail!("duplicate object named {object_name:?}");
        };
    }

    for (object_name, object) in &project.objects {
        let object_id = object_dict[object_name];
        if let Some(parent_name) = &object.parent_object {
            let &parent_object_id = object_dict
                .get(parent_name)
                .with_context(|| anyhow!("no such parent object named {:?}", parent_name))?;
            objects[object_id].parent = Some(parent_object_id);
        }
    }

    let mut rooms = IdMap::<RoomId, Room>::new();
    let mut room_dict = HashMap::<String, RoomId>::new();

    let mut instance_templates = IdMap::<InstanceTemplateId, InstanceTemplate>::new();

    for room in project.rooms.values() {
        let mut layers = HashMap::new();

        for (layer_name, layer) in &room.layers {
            let layer_type = match &layer.layer_type {
                LayerType::Instances(instances) => {
                    let mut template_ids = Vec::new();

                    for instance in instances {
                        let template_id = instance_templates.insert(InstanceTemplate {
                            object: *object_dict.get(&instance.object).with_context(|| {
                                anyhow!("missing object named {:?}", instance.object)
                            })?,
                            position: Vec2::new(instance.x, instance.y),
                        });
                        template_ids.push(template_id);
                    }

                    RoomLayerType::Instances(template_ids)
                }
                LayerType::Assets => RoomLayerType::Assets,
                LayerType::Tile => RoomLayerType::Tile,
                LayerType::Background => RoomLayerType::Background,
            };

            layers.insert(
                layer_name.clone(),
                RoomLayer {
                    name: layer.name.clone(),
                    depth: layer.depth,
                    visible: layer.visible,
                    layer_type,
                },
            );
        }

        let room_id = rooms.insert_with_id(|id| {
            let room_ud = interpreter
                .enter(|ctx| ctx.stash(RoomUserData::new(ctx, id, ctx.intern(&room.name))));
            Room {
                name: room.name.clone(),
                size: Vec2::new(room.width, room.height),
                layers,
                userdata: room_ud,
                tags: room.tags.clone(),
            }
        });

        room_dict.insert(room.name.clone(), room_id);
    }

    let first_room = project.room_order.first().context("no first room")?;
    let first_room = *room_dict
        .get(first_room)
        .with_context(|| "no such room `{first_room:?}`")?;
    let last_room = project.room_order.last().context("no last room")?;
    let last_room = *room_dict
        .get(last_room)
        .with_context(|| "no such room `{last_room:?}`")?;

    let config = Configuration {
        data_path: project.base_path.join("datafiles"),
        tick_rate: TICK_RATE,
        sprites,
        textures,
        fonts,
        sounds,
        shaders,
        tile_sets,
        objects,
        object_dict,
        instance_templates,
        rooms,
        room_dict,
        first_room,
        last_room,
    };

    let mut texture_pages_res = None;
    let mut scripts_res = None;
    rayon::in_place_scope(|scope| {
        let textures = &config.textures;
        scope.spawn(|_| {
            texture_pages_res = Some(compute_texture_pages(project, textures));
        });

        scripts_res = Some(load_scripts(project, &config, config_name, interpreter));
    });

    let texture_page_list = texture_pages_res.unwrap()?;
    let scripts = scripts_res.unwrap()?;

    let mut texture_pages = IdMap::<TexturePageId, TexturePage>::new();

    for page_data in texture_page_list {
        texture_pages.insert_with_id(|id| {
            let userdata = interpreter.enter(|ctx| ctx.stash(TexturePageUserData::new(ctx, id)));
            TexturePage {
                size: page_data.size,
                border: page_data.border,
                textures: page_data.textures,
                userdata,
            }
        });
    }

    let mut texture_page_for_texture = SecondaryMap::new();
    for (texture_page_id, texture_page) in texture_pages.iter() {
        for texture_id in texture_page.textures.ids() {
            texture_page_for_texture.insert(texture_id, texture_page_id);
        }
    }

    Ok(State {
        start_instant: Instant::now(),
        config,
        texture_pages,
        texture_page_for_texture,
        scripts,
        current_room: None,
        next_room: Some(first_room),
        layers: Default::default(),
        named_layers: Default::default(),
        instances: Default::default(),
        instance_for_template: Default::default(),
        instances_for_object: Default::default(),
        instances_for_layer: Default::default(),
        instance_bound_tree: Default::default(),
    })
}

fn load_scripts(
    project: &Project,
    config: &Configuration,
    config_name: &str,
    interpreter: &mut vm::Interpreter,
) -> Result<Scripts, Error> {
    let scripts = interpreter.enter(|ctx| -> Result<_, Error> {
        let mut object_events =
            HashMap::<ObjectId, HashMap<ObjectEvent, vm::StashedClosure>>::new();
        let mut magic = vm::MagicSet::new();

        magic.merge_unique(&ctx.stdlib())?;

        magic.merge_unique(&os_api(ctx))?;
        magic.merge_unique(&platform_api(ctx))?;
        magic.merge_unique(&collision_api(ctx))?;
        magic.merge_unique(&stub_api(ctx))?;
        magic.merge_unique(&object_api(ctx, &config)?)?;
        magic.merge_unique(&instance_api(ctx))?;
        magic.merge_unique(&room_api(ctx, &config)?)?;
        magic.merge_unique(&drawing_api(ctx, &config)?)?;
        magic.merge_unique(&font_api(ctx, &config)?)?;
        magic.merge_unique(&sound_api(ctx, &config)?)?;
        magic.merge_unique(&assets_api(ctx, &config)?)?;
        magic.merge_unique(&tiles_api(ctx))?;
        magic.merge_unique(&layers_api(ctx))?;

        for extension in project.extensions.values() {
            for file in &extension.files {
                if let Some(callbacks) = load_extension_file(ctx, file)? {
                    for (name, callback) in callbacks {
                        magic.add_constant(&ctx, name, callback)?;
                    }
                }
            }
        }

        let magic = Gc::new(&ctx, magic);

        log::info!("compiling all global scripts...");
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
        log::info!("finished compiling all global scripts!");

        log::info!("compiling all object scripts...");
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
                    }
                    .export_top_level_functions(false),
                    name.into_owned(),
                    &code_buf,
                )?;
                object_events
                    .entry(config.object_dict[object_name])
                    .or_default()
                    .insert(
                        event,
                        ctx.stash(vm::Closure::new(&ctx, proto, vm::Value::Undefined).unwrap()),
                    );
            }
        }
        log::info!("finished compiling all object scripts!");

        Ok(Scripts {
            magic: ctx.stash(magic),
            scripts: scripts
                .into_iter()
                .map(|proto| {
                    ctx.stash(vm::Closure::new(&ctx, proto, vm::Value::Undefined).unwrap())
                })
                .collect(),
            object_events,
        })
    })?;

    Ok(scripts)
}

struct TexturePageData {
    pub size: Vec2<u32>,
    pub border: u32,
    pub textures: SecondaryMap<TextureId, Vec2<u32>>,
}

fn compute_texture_pages(
    project: &Project,
    textures: &IdMap<TextureId, Texture>,
) -> Result<Vec<TexturePageData>, Error> {
    // TODO: Hard coded texture page size normally configured by
    // 'options/<platform>/options_<platform>.yy'.
    const TEXTURE_PAGE_SIZE: Vec2<u32> = Vec2::new(2048, 2048);

    let mut texture_groups = HashMap::<String, Vec<TextureId>>::new();

    for (texture_id, texture) in textures.iter() {
        texture_groups
            .entry(texture.texture_group.clone())
            .or_default()
            .push(texture_id);
    }

    log::info!("packing textures...");
    let texture_page_list = texture_groups
        .into_par_iter()
        .map(|(group_name, group)| {
            let project_texture_group = project
                .texture_groups
                .get(&group_name)
                .with_context(|| anyhow!("invalid texture group name {:?}", group_name))?;

            let border = project_texture_group.border as u32;

            let mut to_place = group
                .into_iter()
                .map(|texture_id| (texture_id, ()))
                .collect::<SecondaryMap<_, _>>();

            let mut texture_pages = Vec::new();

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

                let mut texture_page = TexturePageData {
                    size: TEXTURE_PAGE_SIZE,
                    textures: SecondaryMap::new(),
                    border,
                };

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

                texture_pages.push(texture_page);
            }

            log::info!(
                "finished packing textures for group {group_name} with {} texture pages",
                texture_pages.len()
            );

            Ok(texture_pages)
        })
        .collect::<Result<Vec<Vec<_>>, Error>>()?;

    let texture_pages = texture_page_list.into_iter().flatten().collect();

    log::info!("finished packing all textures!");

    Ok(texture_pages)
}
