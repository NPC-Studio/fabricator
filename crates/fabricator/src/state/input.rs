use bitflags::bitflags;
use fabricator_math::Vec2;
use fabricator_util::freeze::{AccessError, Freeze, FreezeCell};
use fabricator_vm as vm;

bitflags! {
    #[derive(Debug, Copy, Clone, Default)]
    pub struct MouseButtons: u8 {
        const Left = 0b00000001;
        const Middle = 0b00000010;
        const Right = 0b00000100;
    }
}

#[derive(Debug, Clone, Default)]
pub struct InputState {
    pub mouse_position: Vec2<f32>,
    pub mouse_pressed: MouseButtons,
}

impl InputState {
    pub fn ctx_cell<'gc>(ctx: vm::Context<'gc>) -> &'gc InputStateCell {
        &ctx.singleton::<gc_arena::Static<InputStateCell>>().0
    }

    pub fn ctx_with<'gc, R>(
        ctx: vm::Context<'gc>,
        f: impl FnOnce(&InputState) -> R,
    ) -> Result<R, AccessError> {
        Self::ctx_cell(ctx).with(|state| f(state))
    }
}

pub type InputStateCell = FreezeCell<Freeze![&'freeze InputState]>;
