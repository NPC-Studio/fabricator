use fabricator_vm as vm;

use crate::api::magic::MagicExt as _;

pub fn tiles_api<'gc>(ctx: vm::Context<'gc>) -> vm::MagicSet<'gc> {
    let mut magic = vm::MagicSet::new();

    magic
        .add_constant(&ctx, ctx.intern("tile_index_mask"), (1 << 20) - 1)
        .unwrap();

    magic
}
