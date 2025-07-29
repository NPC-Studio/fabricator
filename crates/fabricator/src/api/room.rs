use fabricator_vm as vm;

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _, create_magic_rw},
    state::{Configuration, RoomId, State},
};

pub fn room_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    for (room_id, room) in config.rooms.iter() {
        magic.add_constant(
            &ctx,
            ctx.intern(&room.name),
            vm::UserData::new_static(&ctx, room_id).into(),
        )?;
    }

    let room_magic = create_magic_rw(
        &ctx,
        |ctx| {
            State::ctx_with(ctx, |state| {
                Ok(vm::UserData::new_static(&ctx, state.current_room).into())
            })?
        },
        |ctx, value| {
            State::ctx_with_mut(ctx, |state| {
                let room = match value {
                    vm::Value::UserData(user_data) => *user_data.downcast_static::<RoomId>()?,
                    _ => {
                        return Err(vm::Error::msg("cannot set room to non-room value"));
                    }
                };
                state.next_room = Some(room);
                Ok(())
            })?
        },
    );
    magic.add(ctx.intern("room"), room_magic)?;

    Ok(magic)
}
