mod create;
mod maxrects;
mod tick;

use anyhow::Error;
use fabricator_math::{Affine2, Vec2};
use fabricator_vm as vm;

use crate::{
    project::Project,
    state::{
        DrawingState, DrawnSpriteFrame, InputState, State, Texture, TextureId, TexturePage,
        TexturePageId,
    },
};

use self::{create::create_state, tick::tick_state};

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
}

impl Game {
    pub fn new(project: Project, config: &str) -> Result<Self, Error> {
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
                State::ctx_cell(ctx).freeze(&mut state, || thread.run(ctx, closure))?;
                Ok(())
            })?;
        }
        log::info!("finished executing all global scripts!");

        Ok(Game {
            interpreter,
            main_thread,
            state,
            drawing_state: DrawingState::default(),
        })
    }

    pub fn texture_pages(&self) -> impl Iterator<Item = (TexturePageId, &TexturePage)> + '_ {
        self.state.texture_pages.iter()
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
                    .translate(-sprite.origin.cast::<f64>())
                    .rotate(instance.rotation)
                    .translate(instance.position)
                    .cast();
                let depth = self.state.layers[instance.layer].depth;

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
                    .translate(-sprite.origin.cast::<f64>())
                    .translate(drawn_sprite.position)
                    .cast();
                let depth = self.state.layers[instance.layer].depth;

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
