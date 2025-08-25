use fabricator_math::Vec2;
use fabricator_util::freeze::{AccessError, Freeze, FreezeCell};
use fabricator_vm as vm;

use crate::state::{configuration::SpriteId, state::InstanceId};

pub enum DrawnSpriteFrame {
    CurrentAnimation,
    Frame(usize),
}

pub struct DrawnSprite {
    pub instance: InstanceId,
    pub sprite: SpriteId,
    pub sub_img: DrawnSpriteFrame,
    pub position: Vec2<f64>,
}

#[derive(Default)]
pub struct DrawingState {
    pub drawn_sprites: Vec<DrawnSprite>,
}

impl DrawingState {
    pub fn clear(&mut self) {
        self.drawn_sprites.clear();
    }

    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc DrawingStateCell {
        &ctx.singleton::<gc_arena::Static<DrawingStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&DrawingState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }

    pub fn ctx_with_mut<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&mut DrawingState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with_mut(|state| f(state))
    }
}

pub type DrawingStateCell = FreezeCell<Freeze![&'freeze mut DrawingState]>;
