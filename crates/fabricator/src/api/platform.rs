use fabricator_vm::{self as vm, IntoValue as _};

use crate::{
    api::magic::{MagicExt as _, create_magic_ro},
    state::{InputState, MouseButtons},
};

pub fn platform_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    let mouse_x_magic = create_magic_ro(&ctx, |ctx| {
        InputState::ctx_with(ctx, |input| Ok(input.mouse_position[0].into_value(ctx)))?
    });
    magic.add(ctx.intern("mouse_x"), mouse_x_magic).unwrap();

    let mouse_y_magic = create_magic_ro(&ctx, |ctx| {
        InputState::ctx_with(ctx, |input| Ok(input.mouse_position[1].into_value(ctx)))?
    });
    magic.add(ctx.intern("mouse_y"), mouse_y_magic).unwrap();

    for (name, val) in [
        ("mb_left", MouseButtons::Left),
        ("mb_middle", MouseButtons::Middle),
        ("mb_right", MouseButtons::Right),
        ("mb_any", MouseButtons::all()),
    ] {
        magic
            .add_constant(
                &ctx,
                ctx.intern(name),
                vm::UserData::new_static(&ctx, val).into(),
            )
            .unwrap();
    }

    let mouse_check_button = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let button: vm::UserData = exec.stack().consume(ctx)?;
        let button = *button.downcast_static::<MouseButtons>()?;
        InputState::ctx_with(ctx, |input| {
            exec.stack()
                .replace(ctx, !(input.mouse_pressed & button).is_empty());
            Ok(())
        })?
    });
    magic
        .add_constant(
            &ctx,
            ctx.intern("mouse_check_button"),
            mouse_check_button.into(),
        )
        .unwrap();

    magic
}
