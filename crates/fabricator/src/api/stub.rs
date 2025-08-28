use std::env;

use fabricator_vm as vm;

use crate::{api::magic::MagicExt as _, project::Project};

pub fn stub_api<'gc>(ctx: vm::Context<'gc>, project: &Project) -> vm::MagicSet<'gc> {
    fn create_stub_constant<'gc>(
        ctx: vm::Context<'gc>,
        magic: &mut vm::MagicSet<'gc>,
        name: &str,
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
                log::debug!("call of stubbed out callback {name}");
                exec.stack().clear();
                exec.stack().extend(returns);
                Ok(())
            });
        magic
            .add_constant(&ctx, ctx.intern(name), stub_callback)
            .unwrap();
    }

    let mut magic = vm::MagicSet::new();

    let unit_userdata: vm::Value = vm::UserData::new_static(&ctx, ()).into();

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

    create_stub_callback(
        ctx,
        &mut magic,
        "room_get_name",
        [ctx.intern("the_room").into()],
    );

    create_stub_callback(ctx, &mut magic, "application_surface_enable", []);
    create_stub_callback(ctx, &mut magic, "surface_depth_disable", []);
    create_stub_callback(ctx, &mut magic, "display_set_sleep_margin", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_zwriteenable", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_ztestenable", []);
    create_stub_callback(ctx, &mut magic, "gpu_set_zfunc", []);
    create_stub_callback(ctx, &mut magic, "vertex_format_begin", []);
    create_stub_callback(ctx, &mut magic, "vertex_format_add_position_3d", []);
    create_stub_callback(ctx, &mut magic, "vertex_format_add_texcoord", []);
    create_stub_callback(ctx, &mut magic, "vertex_format_add_color", []);
    create_stub_callback(ctx, &mut magic, "vertex_format_end", [unit_userdata]);
    create_stub_callback(ctx, &mut magic, "display_reset", []);
    create_stub_callback(ctx, &mut magic, "window_set_size", []);
    create_stub_callback(ctx, &mut magic, "window_set_fullscreen", []);
    create_stub_callback(ctx, &mut magic, "window_center", []);
    create_stub_callback(ctx, &mut magic, "window_enable_borderless_fullscreen", []);
    create_stub_callback(ctx, &mut magic, "camera_set_view_pos", []);
    create_stub_callback(ctx, &mut magic, "camera_set_view_size", []);
    create_stub_callback(ctx, &mut magic, "shader_get_sampler_index", [unit_userdata]);
    create_stub_callback(ctx, &mut magic, "shader_get_uniform", [unit_userdata]);

    create_stub_constant(
        ctx,
        &mut magic,
        "view_camera",
        vm::Array::from_iter(&ctx, [unit_userdata; 8]),
    );
    create_stub_constant(ctx, &mut magic, "cmpfunc_always", vm::Value::Undefined);

    for shader in project.shaders.values() {
        create_stub_constant(ctx, &mut magic, &shader.name, unit_userdata);
    }

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

    create_stub_callback(ctx, &mut magic, "gamepad_is_connected", [false.into()]);

    for font in project.fonts.values() {
        create_stub_constant(ctx, &mut magic, &font.name, unit_userdata);
    }

    create_stub_callback(ctx, &mut magic, "layer_get_id", [unit_userdata]);
    create_stub_callback(ctx, &mut magic, "layer_tilemap_get_id", [unit_userdata]);
    create_stub_callback(ctx, &mut magic, "layer_get_depth", [0.into()]);
    create_stub_callback(ctx, &mut magic, "layer_depth", []);
    create_stub_callback(ctx, &mut magic, "layer_set_visible", []);
    create_stub_callback(ctx, &mut magic, "layer_create", [unit_userdata]);
    create_stub_callback(ctx, &mut magic, "layer_destroy", []);
    create_stub_callback(
        ctx,
        &mut magic,
        "layer_get_all_elements",
        [vm::Array::new(&ctx).into()],
    );

    create_stub_callback(ctx, &mut magic, "draw_self", []);

    magic
}
