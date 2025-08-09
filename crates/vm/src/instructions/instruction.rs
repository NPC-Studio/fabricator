use std::fmt;

pub type RegIdx = u8;
pub type ConstIdx = u16;
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
            /// Set the `dest` register to `Value::Boolean(val)`.
            boolean = Boolean { dest: RegIdx, is_true: bool };

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

            [simple] globals = Globals { dest: RegIdx };

            [simple]
            /// Get the current value of the `this` register and place it in `dest`.
            this = This { dest: RegIdx };

            [simple]
            /// Copy the `source` register to the `this` register.
            set_this = SetThis { source: RegIdx };

            [simple]
            /// Get the current value of the `other` register and place it in `dest`.
            other = Other { dest: RegIdx };

            [simple]
            /// Copy the `source` register to the `other` register.
            set_other = SetOther { source: RegIdx };

            [simple]
            /// Swap the values of the `this` and `other` registers.
            swap_this_other = SwapThisOther {};

            [simple] closure = Closure { dest: RegIdx, proto: ProtoIdx };

            [simple]
            /// Set the `dest` register to the currently executing closure.
            current_closure = CurrentClosure { dest: RegIdx };

            [simple] new_object = NewObject { dest: RegIdx };
            [simple] new_array = NewArray { dest: RegIdx };

            [simple] get_field = GetField { dest: RegIdx, object: RegIdx, key: RegIdx };
            [simple] set_field = SetField  { object: RegIdx, key: RegIdx, value: RegIdx };
            [simple] get_field_const = GetFieldConst { dest: RegIdx, object: RegIdx, key: ConstIdx };
            [simple] set_field_const = SetFieldConst  { object: RegIdx, key: ConstIdx, value: RegIdx };

            [simple] get_index = GetIndex { dest: RegIdx, array: RegIdx, index: RegIdx };
            [simple] set_index = SetIndex  { array: RegIdx, index: RegIdx, value: RegIdx };
            [simple] get_index_const = GetIndexConst { dest: RegIdx, array: RegIdx, index: ConstIdx };
            [simple] set_index_const = SetIndexConst { array: RegIdx, index: ConstIdx, value: RegIdx };

            [simple] copy = Copy { dest: RegIdx, source: RegIdx };

            [simple] is_defined = IsDefined { dest: RegIdx, arg: RegIdx };
            [simple] is_undefined = IsUndefined { dest: RegIdx, arg: RegIdx };
            [simple] test = Test { dest: RegIdx, arg: RegIdx };
            [simple] not = Not { dest: RegIdx, arg: RegIdx };

            [simple] negate = Negate { dest: RegIdx, arg: RegIdx };
            [simple] increment = Increment { dest: RegIdx, arg: RegIdx };
            [simple] decrement = Decrement { dest: RegIdx, arg: RegIdx };

            [simple] add = Add { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] subtract = Subtract { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] multiply = Multiply { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] divide = Divide { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] remainder = Remainder { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] int_divide = IntDivide { dest: RegIdx, left: RegIdx, right: RegIdx };

            [simple] is_equal = IsEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] is_not_equal = IsNotEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] is_less = IsLess { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] is_less_equal = IsLessEqual { dest: RegIdx, left: RegIdx, right: RegIdx };

            [simple] and = And { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] or = Or { dest: RegIdx, left: RegIdx, right: RegIdx };
            [simple] null_coalesce = NullCoalesce { dest: RegIdx, left: RegIdx, right: RegIdx };

            [simple]
            /// Get the current size of the stack and place it in the `dest` register.
            stack_top = StackTop { dest: RegIdx };

            [simple]
            /// Resize the stack to the size in the `stack_top` register.
            stack_resize = StackResize { stack_top: RegIdx };

            [simple] stack_resize_const = StackResizeConst { stack_top: ConstIdx };

            [simple]
            /// Get a value from the stack at the position in the `stack_pos` register.
            ///
            /// If the stack position is out of range of the current stack top, then the destination
            /// register is set to `Undefined`.
            stack_get = StackGet { dest: RegIdx, stack_pos: RegIdx };

            [simple] stack_get_const = StackGetConst { dest: RegIdx, stack_pos: ConstIdx };

            [simple] stack_get_offset = StackGetOffset {
                dest: RegIdx,
                stack_base: RegIdx,
                offset: ConstIdx
            };

            [simple]
            /// Set a value in the stack at the position in the `stack_pos` register.
            ///
            /// If the stack position is out of range of the current stack top, the stack is
            /// implicitly grown to fit the set value.
            stack_set = StackSet { source: RegIdx, stack_pos: RegIdx };

            [simple]
            /// Push a value onto the top of the stack.
            stack_push = StackPush { source: RegIdx };

            [simple]
            /// Pop a value from the top of the stack.
            stack_pop = StackPop { dest: RegIdx };

            [simple]
            /// Get an index from a value with multiple indexes from the stack.
            ///
            /// Index valuess are all stack elements starting above the position in the
            /// `stack_bottom` register.
            get_index_multi = GetIndexMulti { dest: RegIdx, array: RegIdx, stack_bottom: RegIdx };

            [simple]
            /// Set an index on a value with multiple indexes from the stack.
            ///
            /// Index valuess are all stack elements starting above the position in the
            /// `stack_bottom` register.
            set_index_multi = SetIndexMulti { array: RegIdx, stack_bottom: RegIdx, value: RegIdx };

            [simple] get_magic = GetMagic { dest: RegIdx, magic: MagicIdx };
            [simple] set_magic = SetMagic { magic: MagicIdx, source: RegIdx };

            [simple]
            /// Throw an error located at the given register.
            throw = Throw { source: RegIdx };

            [jump] jump = Jump { offset: i16 };
            [jump] jump_if = JumpIf { offset: i16, arg: RegIdx, is_true: bool };

            [call]
            /// Call a function with arguments starting at `stack_bottom`.
            call = Call { func: RegIdx, stack_bottom: RegIdx };

            [call]
            /// Return with values starting at `stack_bottom`.
            return_ = Return { stack_bottom: RegIdx };
        }
    };
}

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
