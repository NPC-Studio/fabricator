use std::env;

use fabricator_vm as vm;

use crate::api::magic::MagicExt as _;

pub fn stub_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    fn create_stub_constant<'gc>(
        ctx: vm::Context<'gc>,
        magic: &mut vm::MagicSet<'gc>,
        name: &'static str,
        value: impl Into<vm::Value<'gc>>,
    ) {
        magic
            .add_constant(&ctx, ctx.intern(name), value.into())
            .unwrap();
    }

    fn create_stub_callback<'gc, const RET_COUNT: usize>(
        ctx: vm::Context<'gc>,
        magic: &mut vm::MagicSet<'gc>,
        name: &'static str,
        returns: [vm::Value<'gc>; RET_COUNT],
    ) {
        let stub_callback =
            vm::Callback::from_fn_with_root(&ctx, returns, move |returns, _ctx, mut exec| {
                log::info!("call of stubbed out callback {name}");
                exec.stack().clear();
                exec.stack().extend(returns);
                Ok(())
            });
        magic
            .add_constant(&ctx, ctx.intern(name), stub_callback.into())
            .unwrap();
    }

    let mut magic = vm::MagicSet::new();

    create_stub_constant(
        ctx,
        &mut magic,
        "game_save_id",
        ctx.intern(env::temp_dir().to_str().expect("tempdir not utf-8")),
    );
    create_stub_constant(
        ctx,
        &mut magic,
        "game_project_name",
        ctx.intern("fabricator-project"),
    );

    create_stub_callback(ctx, &mut magic, "application_surface_enable", []);
    create_stub_callback(ctx, &mut magic, "surface_depth_disable", []);
    create_stub_callback(ctx, &mut magic, "display_set_sleep_margin", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_zwriteenable", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_ztestenable", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_zfunc", []);

    create_stub_constant(ctx, &mut magic, "cmpfunc_always", vm::Value::Undefined);

    for key in [
        "vk_delete",
        "vk_end",
        "vk_pagedown",
        "vk_pageup",
        "vk_insert",
        "vk_space",
        "vk_tab",
        "vk_backspace",
        "vk_shift",
        "vk_control",
        "vk_enter",
        "vk_up",
        "vk_down",
        "vk_left",
        "vk_right",
        "vk_f1",
        "vk_f2",
        "vk_f3",
        "vk_f4",
        "vk_f5",
        "vk_f6",
        "vk_f7",
        "vk_f8",
        "vk_f9",
        "vk_f10",
        "vk_f11",
        "vk_f12",
        "vk_home",
        "vk_escape",
    ] {
        create_stub_constant(ctx, &mut magic, key, vm::Value::Integer(0));
    }

    for gp_button in [
        "gp_select",
        "gp_start",
        "gp_stickl",
        "gp_stickr",
        "gp_padu",
        "gp_padd",
        "gp_padl",
        "gp_padr",
        "gp_face4",
        "gp_face3",
        "gp_face2",
        "gp_face1",
        "gp_shoulderlb",
        "gp_shoulderrb",
        "gp_shoulderl",
        "gp_shoulderr",
        "gp_paddler",
        "gp_paddlel",
        "gp_paddlerb",
        "gp_paddlelb",
    ] {
        create_stub_constant(ctx, &mut magic, gp_button, 0);
    }

    for gp_axis in ["gp_axislv", "gp_axislh", "gp_axisrv", "gp_axisrh"] {
        create_stub_constant(ctx, &mut magic, gp_axis, 0);
    }

    magic
}
