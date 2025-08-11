use std::fmt;

pub type RegIdx = u8;
pub type ConstIdx = u16;
pub type HeapIdx = u8;
pub type ProtoIdx = u8;
pub type MagicIdx = u16;
pub type InstIdx = u32;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            [basic]
            /// Set the `dest` register to `Value::Undefined`.
            undefined = Undefined { dest: RegIdx };

            [basic]
            /// Set the `dest` register to `Value::Boolean(val)`.
            boolean = Boolean { dest: RegIdx, is_true: bool };

            [basic]
            /// Load a constant into the `dest` register.
            load_constant = LoadConstant { dest: RegIdx, constant: ConstIdx };

            [basic]
            /// Get a heap variable and place it in the `dest` regsiter.
            get_heap = GetHeap { dest: RegIdx, heap: HeapIdx };

            [basic]
            /// Set a heap variable from the `source` register.
            set_heap = SetHeap { heap: HeapIdx, source: RegIdx };

            [basic]
            /// Reset an *owned* heap variable.
            ///
            /// This creates a new owned heap variable with the value `Undefined`, and can be used
            /// to disconnect heap variables that are shared with any previously created closures.
            reset_heap = ResetHeap { heap: HeapIdx };

            [basic] globals = Globals { dest: RegIdx };

            [basic]
            /// Get the current value of the `this` register and place it in `dest`.
            this = This { dest: RegIdx };

            [basic]
            /// Copy the `source` register to the `this` register.
            set_this = SetThis { source: RegIdx };

            [basic]
            /// Get the current value of the `other` register and place it in `dest`.
            other = Other { dest: RegIdx };

            [basic]
            /// Copy the `source` register to the `other` register.
            set_other = SetOther { source: RegIdx };

            [basic]
            /// Swap the values of the `this` and `other` registers.
            swap_this_other = SwapThisOther {};

            [basic] closure = Closure { dest: RegIdx, proto: ProtoIdx };

            [basic]
            /// Set the `dest` register to the currently executing closure.
            current_closure = CurrentClosure { dest: RegIdx };

            [basic] new_object = NewObject { dest: RegIdx };
            [basic] new_array = NewArray { dest: RegIdx };

            [basic] get_field = GetField { dest: RegIdx, object: RegIdx, key: RegIdx };
            [basic] set_field = SetField  { object: RegIdx, key: RegIdx, value: RegIdx };
            [basic] get_field_const = GetFieldConst { dest: RegIdx, object: RegIdx, key: ConstIdx };
            [basic] set_field_const = SetFieldConst  { object: RegIdx, key: ConstIdx, value: RegIdx };

            [basic] get_index = GetIndex { dest: RegIdx, array: RegIdx, index: RegIdx };
            [basic] set_index = SetIndex  { array: RegIdx, index: RegIdx, value: RegIdx };
            [basic] get_index_const = GetIndexConst { dest: RegIdx, array: RegIdx, index: ConstIdx };
            [basic] set_index_const = SetIndexConst { array: RegIdx, index: ConstIdx, value: RegIdx };

            [basic] copy = Copy { dest: RegIdx, source: RegIdx };

            [basic] is_defined = IsDefined { dest: RegIdx, arg: RegIdx };
            [basic] is_undefined = IsUndefined { dest: RegIdx, arg: RegIdx };
            [basic] test = Test { dest: RegIdx, arg: RegIdx };
            [basic] not = Not { dest: RegIdx, arg: RegIdx };

            [basic] negate = Negate { dest: RegIdx, arg: RegIdx };
            [basic] increment = Increment { dest: RegIdx, arg: RegIdx };
            [basic] decrement = Decrement { dest: RegIdx, arg: RegIdx };

            [basic] add = Add { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] subtract = Subtract { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] multiply = Multiply { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] divide = Divide { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] remainder = Remainder { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] int_divide = IntDivide { dest: RegIdx, left: RegIdx, right: RegIdx };

            [basic] is_equal = IsEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] is_not_equal = IsNotEqual { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] is_less = IsLess { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] is_less_equal = IsLessEqual { dest: RegIdx, left: RegIdx, right: RegIdx };

            [basic] and = And { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] or = Or { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] xor = Xor { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] bit_and = BitAnd { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] bit_or = BitOr { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] bit_xor = BitXor { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] bit_shift_left = BitShiftLeft { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] bit_shift_right = BitShiftRight { dest: RegIdx, left: RegIdx, right: RegIdx };
            [basic] null_coalesce = NullCoalesce { dest: RegIdx, left: RegIdx, right: RegIdx };

            [basic]
            /// Get the current size of the stack and place it in the `dest` register.
            stack_top = StackTop { dest: RegIdx };

            [basic]
            /// Resize the stack to the size in the `stack_top` register.
            stack_resize = StackResize { stack_top: RegIdx };

            [basic] stack_resize_const = StackResizeConst { stack_top: ConstIdx };

            [basic]
            /// Get a value from the stack at the position in the `stack_pos` register.
            ///
            /// If the stack position is out of range of the current stack top, then the destination
            /// register is set to `Undefined`.
            stack_get = StackGet { dest: RegIdx, stack_pos: RegIdx };

            [basic] stack_get_const = StackGetConst { dest: RegIdx, stack_pos: ConstIdx };

            [basic] stack_get_offset = StackGetOffset {
                dest: RegIdx,
                stack_base: RegIdx,
                offset: ConstIdx
            };

            [basic]
            /// Set a value in the stack at the position in the `stack_pos` register.
            ///
            /// If the stack position is out of range of the current stack top, the stack is
            /// implicitly grown to fit the set value.
            stack_set = StackSet { source: RegIdx, stack_pos: RegIdx };

            [basic]
            /// Push a value onto the top of the stack.
            stack_push = StackPush { source: RegIdx };

            [basic]
            /// Pop a value from the top of the stack.
            stack_pop = StackPop { dest: RegIdx };

            [basic]
            /// Get an index from a value with multiple indexes from the stack.
            ///
            /// Index valuess are all stack elements starting above the position in the
            /// `stack_bottom` register.
            get_index_multi = GetIndexMulti { dest: RegIdx, array: RegIdx, stack_bottom: RegIdx };

            [basic]
            /// Set an index on a value with multiple indexes from the stack.
            ///
            /// Index valuess are all stack elements starting above the position in the
            /// `stack_bottom` register.
            set_index_multi = SetIndexMulti { array: RegIdx, stack_bottom: RegIdx, value: RegIdx };

            [basic] get_magic = GetMagic { dest: RegIdx, magic: MagicIdx };
            [basic] set_magic = SetMagic { magic: MagicIdx, source: RegIdx };

            [basic]
            /// Throw an error located at the given register.
            throw = Throw { source: RegIdx };

            [jump] jump = Jump { target: InstIdx };

            [jump_if] jump_if = JumpIf { target: InstIdx, arg: RegIdx, is_true: bool };

            [special]
            /// Call a function with arguments starting at `stack_bottom`.
            call = Call { func: RegIdx, stack_bottom: RegIdx };

            [special]
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
            (InstIdx) => {
                "I"
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
