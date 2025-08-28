use fabricator_vm as vm;
use gc_arena::{Collect, Gc, Rootable};

use crate::{
    api::magic::{DuplicateMagicName, MagicExt as _, create_magic_ro, create_magic_rw},
    state::{Configuration, RoomId, State},
};

#[derive(Collect)]
#[collect(no_drop)]
pub struct RoomUserData<'gc> {
    #[collect(require_static)]
    room_id: RoomId,
    name: vm::String<'gc>,
}

impl<'gc> RoomUserData<'gc> {
    pub fn new(ctx: vm::Context<'gc>, room_id: RoomId, name: vm::String<'gc>) -> vm::UserData<'gc> {
        #[derive(Collect)]
        #[collect(require_static)]
        struct RoomMethods;

        impl<'gc> vm::UserDataMethods<'gc> for RoomMethods {
            fn cast_string(
                &self,
                ud: fabricator_vm::UserData<'gc>,
                _ctx: fabricator_vm::Context<'gc>,
            ) -> Option<fabricator_vm::String<'gc>> {
                Some(ud.downcast::<Rootable![RoomUserData<'_>]>().unwrap().name)
            }
        }

        #[derive(Collect)]
        #[collect(no_drop)]
        struct RoomMethodsSingleton<'gc>(Gc<'gc, dyn vm::UserDataMethods<'gc>>);

        impl<'gc> vm::Singleton<'gc> for RoomMethodsSingleton<'gc> {
            fn create(ctx: vm::Context<'gc>) -> Self {
                let methods = Gc::new(&ctx, RoomMethods);
                RoomMethodsSingleton(gc_arena::unsize!(methods => dyn vm::UserDataMethods<'gc>))
            }
        }

        let methods = ctx.singleton::<Rootable![RoomMethodsSingleton<'_>]>().0;

        let userdata =
            vm::UserData::new::<Rootable![RoomUserData<'_>]>(&ctx, RoomUserData { room_id, name });
        userdata.set_methods(&ctx, Some(methods));

        userdata
    }

    pub fn downcast(
        userdata: vm::UserData<'gc>,
    ) -> Result<&'gc Self, vm::userdata::BadUserDataType> {
        userdata.downcast::<Rootable![RoomUserData<'_>]>()
    }
}

pub fn room_api<'gc>(
    ctx: vm::Context<'gc>,
    config: &Configuration,
) -> Result<vm::MagicSet<'gc>, DuplicateMagicName> {
    let mut magic = vm::MagicSet::new();

    for room in config.rooms.values() {
        let room_ud = ctx.fetch(&room.userdata);
        let room_name = RoomUserData::downcast(room_ud).unwrap().name;
        magic.add_constant(&ctx, room_name, room_ud)?;
    }

    let room_magic = create_magic_rw(
        &ctx,
        |ctx| {
            Ok(State::ctx_with(ctx, |state| {
                ctx.fetch(&state.config.rooms[state.current_room.unwrap()].userdata)
                    .into()
            })?)
        },
        |ctx, value| {
            State::ctx_with_mut(ctx, |state| {
                let room_id = match value {
                    vm::Value::UserData(userdata) => RoomUserData::downcast(userdata)?.room_id,
                    _ => {
                        return Err("cannot set room to non-room value".into());
                    }
                };
                state.next_room = Some(room_id);
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
