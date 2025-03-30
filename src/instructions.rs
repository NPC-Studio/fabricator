pub type RegIdx = u8;
pub type HeapIdx = u8;
pub type ConstIdx = u16;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            simple => load_constant = LoadConstant { constant: ConstIdx, dest: RegIdx };
            simple => get_heap = GetHeap { heap: HeapIdx, dest: RegIdx };
            simple => set_heap = SetHeap { source: RegIdx, heap: HeapIdx };
            simple => move_ = Move { source: RegIdx, dest: RegIdx };
            simple => not = Not { arg: RegIdx, dest: RegIdx };
            simple => add = Add { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => sub = Sub { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => test_equal = TestEqual { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => test_not_equal = TestNotEqual { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => test_less = TestLess { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => test_less_equal = TestLessEqual { arg1: RegIdx, arg2: RegIdx, dest: RegIdx };
            simple => push = Push { source: RegIdx, len: u8 };
            simple => pop = Pop { dest: RegIdx, len: u8 };

            other => jump = Jump { offset: i16 };
            other => jump_if = JumpIf { arg: RegIdx, is_true: bool, offset: i16 };
            other => call = Call { func: RegIdx, args: u8, returns: u8 };
            other => return_ = Return { returns: u8 };
        }
    };
}
pub(crate) use for_each_instruction;

macro_rules! define_instruction {
    ($(
        $_:ident => $snake_name:ident = $name:ident { $($field:ident: $field_ty:ty),* };
    )*) => {
        #[derive(Debug, Copy, Clone, Eq, PartialEq)]
        pub enum Instruction {
            $($name {
                $($field: $field_ty),*
            }),*
        }
    };
}
for_each_instruction!(define_instruction);
