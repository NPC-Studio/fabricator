use fabricator_vm as vm;

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _, create_magic_ro, create_magic_rw},
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
            vm::UserData::new_static(&ctx, room_id),
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
                        return Err("cannot set room to non-room value".into());
                    }
                };
                state.next_room = Some(room);
                Ok(())
            })?
        },
    );
    magic.add(ctx.intern("room"), room_magic)?;

    let room_width = create_magic_ro(&ctx, |ctx| {
        Ok(State::ctx_with(ctx, |state| {
            vm::Value::Integer(state.config.rooms[state.current_room.unwrap()].size[0] as i64)
        })?)
    });
    magic.add(ctx.intern("room_width"), room_width)?;

    let room_height = create_magic_ro(&ctx, |ctx| {
        Ok(State::ctx_with(ctx, |state| {
            vm::Value::Integer(state.config.rooms[state.current_room.unwrap()].size[1] as i64)
        })?)
    });
    magic.add(ctx.intern("room_height"), room_height)?;

    Ok(magic)
}
