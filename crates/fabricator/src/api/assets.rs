use std::collections::HashMap;

use anyhow::Context as _;
use fabricator_vm as vm;
use gc_arena::Gc;

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _},
    state::Configuration,
};

pub fn assets_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut assets_map = HashMap::new();

    for sprite in config.sprites.values() {
        assets_map.insert(ctx.intern(&sprite.name), ctx.fetch(&sprite.userdata));
    }

    for room in config.rooms.values() {
        assets_map.insert(ctx.intern(&room.name), ctx.fetch(&room.userdata));
    }

    for object in config.objects.values() {
        assets_map.insert(ctx.intern(&object.name), ctx.fetch(&object.userdata));
    }

    let assets_map = Gc::new(&ctx, assets_map);

    let mut magic = vm::MagicSet::new();

    let asset_get_index =
        vm::Callback::from_fn_with_root(&ctx, assets_map, |&assets_map, ctx, mut exec| {
            let name: vm::String = exec.stack().consume(ctx)?;
            let asset = assets_map
                .get(&name)
                .copied()
                .with_context(|| format!("no such asset named {name}"))?;
            exec.stack().replace(ctx, asset);
            Ok(())
        });
    magic.add_constant(&ctx, ctx.intern("asset_get_index"), asset_get_index)?;

    Ok(magic)
}
