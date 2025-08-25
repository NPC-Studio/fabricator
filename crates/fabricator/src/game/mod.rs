mod create;
mod maxrects;
mod tick;

use std::collections::HashMap;

use anyhow::{Context as _, Error, anyhow, bail};
use fabricator_math::{Affine2, Vec2};
use fabricator_util::typed_id_map::{IdMap, SecondaryMap, new_id_type};
use fabricator_vm as vm;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};

use crate::{
    project::Project,
    state::{DrawingState, DrawnSpriteFrame, InputState, State, Texture, TextureId},
};

use self::{create::create_state, maxrects::MaxRects, tick::tick_state};

new_id_type! {
    pub struct TexturePageId;
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
    // Transforms the quad (0, 0, texture_width, texture_height) into room coordinates.
    pub transform: Affine2<f32>,
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
    main_thread: vm::StashedThread,
    state: State,
    drawing_state: DrawingState,
    texture_pages: IdMap<TexturePageId, TexturePage>,
}

impl Game {
    pub fn new(project: Project, config: &str) -> Result<Self, Error> {
        // TODO: Hard coded texture page size normally configured by
        // 'options/<platform>/options_<platform>.yy'.
        const TEXTURE_PAGE_SIZE: Vec2<u32> = Vec2::new(2048, 2048);

        let mut interpreter = vm::Interpreter::new();
        let main_thread = interpreter.enter(|ctx| ctx.stash(vm::Thread::new(&ctx)));

        log::info!("creating new game state...");
        let mut state = create_state(&mut interpreter, &project, config)?;
        log::info!("finished creating new game state!");

        log::info!("execuing all global scripts...");
        for script in state.scripts.scripts.clone() {
            interpreter.enter(|ctx| -> Result<_, Error> {
                log::debug!("executing script {}", script.identifier(ctx));
                let thread = ctx.fetch(&main_thread);
                let closure = script.create_closure(ctx);
                State::ctx_cell(ctx).freeze(&mut state, || thread.exec(ctx, closure))?;
                Ok(())
            })?;
        }
        log::info!("finished executing all global scripts!");

        let mut texture_groups = HashMap::<String, Vec<TextureId>>::new();

        for (texture_id, texture) in state.config.textures.iter() {
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
                        let padded_size =
                            state.config.textures[texture_id].size + Vec2::splat(border * 2);
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

                    let mut texture_page = TexturePage {
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

                log::info!("finished packing textures for group {group_name}");

                Ok(texture_pages)
            })
            .collect::<Result<Vec<Vec<_>>, Error>>()?;

        let mut texture_pages = IdMap::<TexturePageId, TexturePage>::new();
        for texture_page in texture_page_list.into_iter().flatten() {
            texture_pages.insert(texture_page);
        }

        log::info!("finished packing all textures!");

        Ok(Game {
            interpreter,
            main_thread,
            state,
            drawing_state: DrawingState::default(),
            texture_pages,
        })
    }

    pub fn texture_pages(&self) -> impl Iterator<Item = (TexturePageId, &TexturePage)> + '_ {
        self.texture_pages.iter()
    }

    pub fn texture_map(&self) -> &IdMap<TextureId, Texture> {
        &self.state.config.textures
    }

    pub fn texture(&self, texture_id: TextureId) -> &Texture {
        &self.state.config.textures[texture_id]
    }

    pub fn tick_rate(&self) -> f64 {
        self.state.config.tick_rate
    }

    pub fn tick(&mut self, input: &InputState, render: &mut Render) -> Result<(), Error> {
        self.drawing_state.clear();

        tick_state(
            &mut self.state,
            &mut self.drawing_state,
            input,
            &mut self.interpreter,
            &self.main_thread,
        )?;

        render.clear();
        render.room_size = self.state.config.rooms[self.state.current_room.unwrap()].size;

        for instance in self.state.instances.values() {
            let object = &self.state.config.objects[instance.object];
            if let Some(sprite_id) = object.sprite {
                let sprite = &self.state.config.sprites[sprite_id];
                let frame_index = match sprite
                    .frames
                    .binary_search_by(|f| f.frame_start.total_cmp(&instance.animation_time))
                {
                    Ok(i) => i,
                    Err(i) => i.checked_sub(1).unwrap(),
                };

                let texture = sprite.frames[frame_index].texture;
                let transform = Affine2::new()
                    .translate(-sprite.origin)
                    .rotate(instance.rotation)
                    .translate(instance.position)
                    .cast();
                let depth = instance.depth;

                render.quads.push(Quad {
                    texture,
                    transform,
                    depth,
                });
            }
        }

        for drawn_sprite in &self.drawing_state.drawn_sprites {
            let sprite = &self.state.config.sprites[drawn_sprite.sprite];
            if let Some(instance) = self.state.instances.get(drawn_sprite.instance) {
                let frame_index = match drawn_sprite.sub_img {
                    DrawnSpriteFrame::CurrentAnimation => match sprite
                        .frames
                        .binary_search_by(|f| f.frame_start.total_cmp(&instance.animation_time))
                    {
                        Ok(i) => i,
                        Err(i) => i.checked_sub(1).unwrap(),
                    },
                    DrawnSpriteFrame::Frame(i) => i % sprite.frames.len(),
                };

                let texture = sprite.frames[frame_index].texture;
                let transform = Affine2::new()
                    .translate(-sprite.origin)
                    .translate(drawn_sprite.position)
                    .cast();
                let depth = instance.depth;

                render.quads.push(Quad {
                    texture,
                    transform,
                    depth,
                });
            }
        }

        Ok(())
    }
}
