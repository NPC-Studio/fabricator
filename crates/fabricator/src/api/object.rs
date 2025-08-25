use fabricator_vm as vm;
use gc_arena::{Collect, Rootable};

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _},
    state::Configuration,
};

pub fn no_one<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct NoOne;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            Singleton(vm::UserData::new_static(&ctx, NoOne))
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn all<'gc>(ctx: vm::Context<'gc>) -> vm::UserData<'gc> {
    #[derive(Collect)]
    #[collect(require_static)]
    struct All;

    #[derive(Collect)]
    #[collect(no_drop)]
    struct Singleton<'gc>(vm::UserData<'gc>);

    impl<'gc> vm::Singleton<'gc> for Singleton<'gc> {
        fn create(ctx: vm::Context<'gc>) -> Self {
            Singleton(vm::UserData::new_static(&ctx, All))
        }
    }

    ctx.singleton::<Rootable![Singleton<'_>]>().0
}

pub fn object_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    magic
        .add_constant(&ctx, ctx.intern("noone"), no_one(ctx))
        .unwrap();

    magic
        .add_constant(&ctx, ctx.intern("all"), all(ctx))
        .unwrap();

    for (object_id, object) in config.objects.iter() {
        magic.add_constant(
            &ctx,
            ctx.intern(&object.name),
            vm::UserData::new_static(&ctx, object_id),
        )?;
    }

    Ok(magic)
}
