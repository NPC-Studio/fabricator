use std::fmt;

pub type RegIdx = u8;
pub type ConstIdx = u16;
pub type ArgIdx = u8;
pub type HeapIdx = u8;
pub type ProtoIdx = u8;
pub type MagicIdx = u16;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            [simple]
            /// Set the `dest` register to `Value::Undefined`.
            undefined = Undefined { dest: RegIdx };

            [simple]
            /// Load a constant into the `dest` register.
            load_constant = LoadConstant { dest: RegIdx, constant: ConstIdx };

            [simple]
            /// Get a heap variable and place it in the `dest` regsiter.
            get_heap = GetHeap { dest: RegIdx, heap: HeapIdx };

            [simple]
            /// Set a heap variable from the `source` register.
            set_heap = SetHeap { heap: HeapIdx, source: RegIdx };

            [simple]
            /// Reset an *owned* heap variable.
            ///
            /// This creates a new owned heap variable with the value `Undefined`, and can be used
            /// to disconnect heap variables that are shared with any previously created closures.
            reset_heap = ResetHeap { heap: HeapIdx };

            [simple] closure = Closure { dest: RegIdx, proto: ProtoIdx };
            [simple] globals = Globals { dest: RegIdx };

            [simple]
            /// Set the `dest` register to the current `self` value.
            this = This { dest: RegIdx };
            [simple]
            /// Set the `dest` register to the current `other` value.
            other = Other { dest: RegIdx };

            [simple]
            /// Set the `dest` register to the currently executing closure.
            current_closure = CurrentClosure { dest: RegIdx };

            [simple] new_object = NewObject { dest: RegIdx };
            [simple] new_array = NewArray { dest: RegIdx };
            [simple] arg_count = ArgCount { dest: RegIdx };
            [simple] argument = Argument { dest: RegIdx, index: ArgIdx };
            [simple] get_field = GetField { dest: RegIdx, object: RegIdx, key: RegIdx };
            [simple] set_field = SetField  { object: RegIdx, key: RegIdx, value: RegIdx };
            [simple] get_field_const = GetFieldConst { dest: RegIdx, object: RegIdx, key: ConstIdx };
            [simple] set_field_const = SetFieldConst  { object: RegIdx, key: ConstIdx, value: RegIdx };
            [simple] get_index = GetIndex { dest: RegIdx, array: RegIdx, index: RegIdx };
            [simple] set_index = SetIndex  { array: RegIdx, index: RegIdx, value: RegIdx };
            [simple] get_index_const = GetIndexConst { dest: RegIdx, array: RegIdx, index: ConstIdx };
            [simple] set_index_const = SetIndexConst { array: RegIdx, index: ConstIdx, value: RegIdx };
            [simple] move_ = Move { dest: RegIdx, source: RegIdx };
            [simple] not = Not { dest: RegIdx, arg: RegIdx };
            [simple] neg = Neg { dest: RegIdx, arg: RegIdx };
            [simple] add = Add { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] sub = Sub { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] mult = Mult { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] div = Div { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] rem = Rem { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] idiv = IDiv { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] test_equal = TestEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] test_not_equal = TestNotEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] test_less = TestLess { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] test_less_equal = TestLessEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] and = And { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] or = Or { dest: RegIdx, left: RegIdx, right: RegIdx };

            [simple]
            /// Push `len` values starting at the `source` register to the stack.
            push = Push { source: RegIdx, len: ArgIdx };

            [simple]
            /// Pop `len` values from the stack to registers starting at `dest`.
            pop = Pop { dest: RegIdx, len: ArgIdx };

            [simple]
            /// Push the given register as the new `self`, making the previous self the new `other`,
            /// and then pushing the previous `other` to the top of the stack.
            push_this = PushThis { source: RegIdx };

            [simple]
            /// Undo the operations done by `PushThis`. Set the current `other` value as the new
            /// `self` and pop the top value off of the stack to become the new `other`.
            pop_this = PopThis {};

            [simple] get_magic = GetMagic { dest: RegIdx, magic: MagicIdx };
            [simple] set_magic = SetMagic { magic: MagicIdx, source: RegIdx };

            [jump] jump = Jump { offset: i16 };
            [jump] jump_if = JumpIf { offset: i16, arg: RegIdx, is_true: bool };

            [call] call = Call { func: RegIdx, arguments: ArgIdx, returns: ArgIdx };
            [call] return_ = Return { count: ArgIdx };
        }
    };
}

pub(crate) use for_each_instruction;

macro_rules! define_instruction {
    ($(
        [$_category:ident] $(#[$attr:meta])* $snake_name:ident = $name:ident { $($field:ident: $field_ty:ty),* };
    )*) => {
        #[derive(Copy, Clone, Eq, PartialEq)]
        pub enum Instruction {
            $(
                $(#[$attr])*
                $name {
                    $($field: $field_ty),*
                }
            ),*
        }
    };
}
for_each_instruction!(define_instruction);

impl Instruction {
    pub fn pretty_print(self, f: &mut dyn fmt::Write) -> fmt::Result {
        macro_rules! prefix {
            (RegIdx) => {
                "R"
            };
            (ConstIdx) => {
                "C"
            };
            (ParamIdx) => {
                "A"
            };
            (HeapIdx) => {
                "H"
            };
            (ProtoIdx) => {
                "P"
            };
            (MagicIdx) => {
                "M"
            };
            ($other:ident) => {
                ""
            };
        }

        macro_rules! impl_debug {
            ($(
                [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $($field:ident: $field_ty:ident),* };
            )*) => {
                match self {
                    $(Instruction::$name { $($field),* } => {
                        write!(f, stringify!($snake_name))?;
                        write!(f, "(")?;
                        #[allow(unused, unused_mut)]
                        let mut prev = false;
                        $(
                            if prev {
                                write!(f, ", ")?;
                            }
                            #[allow(unused)]
                            {
                                prev = true;
                            }

                            write!(f, stringify!($field))?;
                            write!(f, "=")?;
                            write!(f, prefix!($field_ty))?;
                            write!(f, "{}", $field)?;
                        )*
                        write!(f, ")")?;
                    }),*
                }
            };
        }

        for_each_instruction!(impl_debug);
        Ok(())
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_print(f)
    }
}
