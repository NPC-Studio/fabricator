use std::env;

use fabricator_vm as vm;

pub fn os_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    lib.insert(
        ctx.intern("os_type"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern(env::consts::OS)),
    );
    lib.insert(
        ctx.intern("os_windows"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("windows")),
    );
    lib.insert(
        ctx.intern("os_macosx"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("macos")),
    );
    lib.insert(
        ctx.intern("os_linux"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("linux")),
    );
    lib.insert(
        ctx.intern("os_switch"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("switch")),
    );
    lib.insert(
        ctx.intern("os_ps4"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("ps4")),
    );
    lib.insert(
        ctx.intern("os_ps5"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("ps5")),
    );
    lib.insert(
        ctx.intern("os_gdk"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("gdk")),
    );
    lib.insert(
        ctx.intern("os_xboxseriesx"),
        vm::MagicConstant::new_ptr(&ctx, ctx.intern("xboxseriesx")),
    );

    let environment_get_variable = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let var_name: vm::String = exec.stack().consume(ctx)?;
        let env_var = match env::var(var_name.as_str()) {
            Ok(val) => ctx.intern(&val),
            Err(_) => ctx.intern(""),
        };
        exec.stack().replace(ctx, env_var);
        Ok(())
    });
    lib.insert(
        ctx.intern("environment_get_variable"),
        vm::MagicConstant::new_ptr(&ctx, environment_get_variable),
    );
}
