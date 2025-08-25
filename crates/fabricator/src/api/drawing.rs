use fabricator_math::Vec2;
use fabricator_vm as vm;

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _},
    state::{Configuration, DrawingState, DrawnSprite, DrawnSpriteFrame, InstanceState, SpriteId},
};

pub fn drawing_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    for (sprite_id, sprite) in config.sprites.iter() {
        magic.add_constant(
            &ctx,
            ctx.intern(&sprite.name),
            vm::UserData::new_static(&ctx, sprite_id),
        )?;
    }

    let draw_sprite = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (sprite, sub_img, x, y): (vm::UserData, i64, f64, f64) = exec.stack().consume(ctx)?;
        let sprite = *sprite.downcast_static::<SpriteId>()?;

        let instance = InstanceState::ctx_with(ctx, |i| i.instance_id)?;

        DrawingState::ctx_with_mut(ctx, |drawing_state| {
            drawing_state.drawn_sprites.push(DrawnSprite {
                instance,
                sprite,
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
