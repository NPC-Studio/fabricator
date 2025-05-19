use std::collections::HashMap;

use anyhow::{Context as _, Error, anyhow, bail};
use fabricator_math::Vec2;
use fabricator_util::typed_id_map::{IdMap, SecondaryMap, new_id_type};
use fabricator_vm as vm;

use crate::{
    maxrects::MaxRects,
    project::Project,
    state::{State, Texture, TextureId},
};

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
    state: State,
    texture_pages: IdMap<TexturePageId, TexturePage>,
}

impl Game {
    pub fn new(project: Project) -> Result<Self, Error> {
        let mut interpreter = vm::Interpreter::new();
        let state = State::create(&mut interpreter, &project)?;

        let mut texture_groups = HashMap::<String, Vec<TextureId>>::new();

        for (texture_id, texture) in state.textures.iter() {
            texture_groups
                .entry(texture.texture_group.clone())
                .or_default()
                .push(texture_id);
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
                    let padded_size = state.textures[texture_id].size + Vec2::splat(border * 2);
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

        Ok(Game {
            interpreter,
            state,
            texture_pages,
        })
    }

    pub fn texture_pages(&self) -> impl Iterator<Item = (TexturePageId, &TexturePage)> + '_ {
        self.texture_pages.iter()
    }

    pub fn texture(&self, texture_id: TextureId) -> &Texture {
        &self.state.textures[texture_id]
    }

    pub fn tick_rate(&self) -> f64 {
        self.state.tick_rate
    }

    pub fn tick(&mut self, render: &mut Render) -> Result<(), Error> {
        self.state.tick(&mut self.interpreter)?;

        render.clear();
        render.room_size = self.state.rooms[self.state.current_room].size;

        for instance in self.state.instances.values() {
            let object = &self.state.objects[instance.object];
            if let Some(sprite_id) = object.sprite {
                let sprite = &self.state.sprites[sprite_id];
                let frame_index = match sprite
                    .frames
                    .binary_search_by(|f| f.frame_start.total_cmp(&instance.animation_time))
                {
                    Ok(i) => i,
                    Err(i) => i.checked_sub(1).unwrap(),
                };

                render.quads.push(Quad {
                    texture: sprite.frames[frame_index].texture,
                    position: (instance.position - sprite.origin).cast(),
                    depth: instance.depth,
                });
            }
        }

        Ok(())
    }
}

// TODO: Hard code texture page size normally configured by
// 'options/<platform>/options_<platform>.yy'.
const TEXTURE_PAGE_SIZE: Vec2<u32> = Vec2::new(2048, 2048);
