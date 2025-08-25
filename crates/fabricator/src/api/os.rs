use std::env;

use fabricator_vm as vm;

use crate::api::magic::MagicExt as _;

pub fn os_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    magic
        .add_constant(&ctx, ctx.intern("os_type"), ctx.intern(env::consts::OS))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_windows"), ctx.intern("windows"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_macosx"), ctx.intern("macos"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_linux"), ctx.intern("linux"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_switch"), ctx.intern("switch"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_ps4"), ctx.intern("ps4"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_ps5"), ctx.intern("ps5"))
        .unwrap();
    magic
        .add_constant(&ctx, ctx.intern("os_gdk"), ctx.intern("gdk"))
        .unwrap();
    magic
        .add_constant(
            &ctx,
            ctx.intern("os_xboxseriesx"),
            ctx.intern("xboxseriesx"),
        )
        .unwrap();

    let environment_get_variable = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let var_name: vm::String = exec.stack().consume(ctx)?;
        let env_var = match env::var(var_name.as_str()) {
            Ok(val) => ctx.intern(&val),
            Err(_) => ctx.intern(""),
        };
        exec.stack().replace(ctx, env_var);
        Ok(())
    });
    magic
        .add_constant(
            &ctx,
            ctx.intern("environment_get_variable"),
            environment_get_variable,
        )
        .unwrap();

    magic
}
