use gc_arena::Collect;

use crate::{constant::Constant, ops::Instruction, value::String};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Collect)]
#[collect(require_static)]
pub struct StackSize(u8);

impl StackSize {
    // Stack size cannot be zero or greater than 256.
    pub fn new(size: usize) -> Self {
        assert!(size != 0);
        assert!(size <= 256);
        Self((size - 1).try_into().unwrap())
    }

    pub fn get(self) -> usize {
        self.0 as usize + 1
    }
}

#[derive(Debug, Collect)]
#[collect(no_drop)]
pub struct Prototype<'gc> {
    pub fixed_params: u8,
    pub stack_size: StackSize,
    pub constants: Box<[Constant<String<'gc>>]>,
    pub instructions: Box<[Instruction]>,
}
