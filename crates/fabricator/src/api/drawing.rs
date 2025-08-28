use fabricator_math::Vec2;
use fabricator_vm as vm;

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _},
    state::{Configuration, DrawingState, DrawnSprite, DrawnSpriteFrame, InstanceState, SpriteId},
};

pub struct SpriteUserData(SpriteId);

impl SpriteUserData {
    pub fn new<'gc>(ctx: vm::Context<'gc>, sprite_id: SpriteId) -> vm::UserData<'gc> {
        vm::UserData::new_static::<SpriteUserData>(&ctx, SpriteUserData(sprite_id))
    }

    pub fn downcast<'gc>(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast_static::<SpriteUserData>()
    }
}

pub fn drawing_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    for sprite in config.sprites.values() {
        magic.add_constant(&ctx, ctx.intern(&sprite.name), ctx.fetch(&sprite.userdata))?;
    }

    let draw_sprite = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (sprite, sub_img, x, y): (vm::UserData, i64, f64, f64) = exec.stack().consume(ctx)?;
        let sprite = SpriteUserData::downcast(sprite)?;

        let instance = InstanceState::ctx_with(ctx, |i| i.instance_id)?;

        DrawingState::ctx_with_mut(ctx, |drawing_state| {
            drawing_state.drawn_sprites.push(DrawnSprite {
                instance,
                sprite: sprite.0,
                sub_img: if sub_img < 0 {
                    DrawnSpriteFrame::CurrentAnimation
                } else {
                    DrawnSpriteFrame::Frame(sub_img as usize)
                },
                position: Vec2::new(x, y),
            })
        })?;

        Ok(())
    });
    magic.add_constant(&ctx, ctx.intern("draw_sprite"), draw_sprite)?;

    Ok(magic)
}
