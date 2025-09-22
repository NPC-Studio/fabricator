use fabricator_util::freeze::{AccessError, Freeze, FreezeCell};
use fabricator_vm as vm;

use crate::{
    project::ObjectEvent,
    state::{configuration::ObjectId, state::InstanceId},
};

pub struct EventState {
    pub instance_id: InstanceId,
    // Specifies which object we are executing an event for. On the top-level execution of an event,
    // this will be the object that matches the instance, but if an inherited event is executed this
    // may be a some parent object of the current instance.
    pub object_id: ObjectId,
    pub current_event: ObjectEvent,
}

impl EventState {
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc EventStateCell {
        &ctx.singleton::<gc_arena::Static<EventStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&EventState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }
}

pub type EventStateCell = FreezeCell<Freeze![&'freeze EventState]>;
