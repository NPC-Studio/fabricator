use std::fmt;

pub type RegIdx = u8;
pub type ConstIdx = u16;
pub type HeapIdx = u8;
pub type ProtoIdx = u8;
pub type MagicIdx = u32;
pub type InstIdx = u32;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            [basic]
            /// Set the `dest` register to `Value::Undefined`.
            undefined = Undefined { dest: RegIdx };

            [basic]
            /// Set the `dest` register to `Value::Boolean(is_true)`.
            boolean = Boolean { dest: RegIdx, value: bool };

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
            /// This resets a heap variable to the value `Undefined`, and also  disconnects heap
            /// variables that are shared with any previously created closures.
            reset_heap = ResetHeap { heap: HeapIdx };

            [basic] globals = Globals { dest: RegIdx };

            [basic]
            /// Push the top value of the `this` stack onto the `this` stack.
            push_this = PushThis {};

            [basic]
            /// Pop the top value off of the `this` stack.
            pop_this = PopThis {};

            [basic]
            /// Get the value at the top of the `this` stack.
            this = This { dest: RegIdx };

            [basic]
            /// Set the value at the top of the `this` stack.
            set_this = SetThis { source: RegIdx };

            [basic]
            /// Get the value one under the top of the `this` stack.
            other = Other { dest: RegIdx };

            [basic] closure = Closure { dest: RegIdx, proto: ProtoIdx, bind_this: bool };

            [basic]
            /// Set the `dest` register to the currently executing closure.
            current_closure = CurrentClosure { dest: RegIdx };

            [basic] arg_count = ArgCount { dest: RegIdx };

            [basic]
            /// Get an argument with index from the value in the `index` register and place it in
            /// the `dest` register.
            ///
            /// If the argument index is out of range of the current argument list, then the
            /// destination register is set to `Undefined`.
            get_arg = GetArg { dest: RegIdx, index: RegIdx };

            [basic] get_arg_const = GetArgConst { dest: RegIdx, index: ConstIdx };

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
            [basic] bit_negate = BitNegate { dest: RegIdx, arg: RegIdx };
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
            /// Push a new frame on the stack.
            push_stack_frame = PushStackFrame {};

            [basic]
            /// Pops the topmost stack frame.
            pop_stack_frame = PopStackFrame {};

            [basic]
            /// Push a value onto the top of the current stack frame.
            stack_push = StackPush { source: RegIdx };

            [basic]
            /// Push two values onto the top of the topmost stack frame.
            stack_push_2 = StackPush2 { source_a: RegIdx, source_b: RegIdx };

            [basic]
            /// Push three values onto the top of the topmost stack frame.
            stack_push_3 = StackPush3 { source_a: RegIdx, source_b: RegIdx, source_c: RegIdx };

            [basic]
            /// Push four values onto the top of the topmost stack frame.
            stack_push_4 = StackPush4 {
                source_a: RegIdx,
                source_b: RegIdx,
                source_c: RegIdx,
                source_d: RegIdx,
            };

            [basic]
            /// Get an element of the current stack frame with index from the value in the `index`
            /// register and place it in the `dest` register.
            ///
            /// If the stack index is out of range of the current stack frame, then the destination
            /// register is set to `Undefined`.
            stack_get = StackGet { dest: RegIdx, index: RegIdx };

            [basic] stack_get_const = StackGetConst { dest: RegIdx, index: ConstIdx };

            [basic]
            /// Get an index from a value with multiple indexes from the topmost stack frame.
            ///
            /// Automatically pops the topmost stack frame.
            get_index_multi = GetIndexMulti { dest: RegIdx, array: RegIdx };

            [basic]
            /// Set an index on a value with multiple indexes from the topmost stack frame.
            ///
            /// Automatically pops the topmost stack frame.
            set_index_multi = SetIndexMulti { array: RegIdx, value: RegIdx };

            [basic] get_magic = GetMagic { dest: RegIdx, magic: MagicIdx };
            [basic] set_magic = SetMagic { magic: MagicIdx, source: RegIdx };

            [basic]
            /// Throw an error located at the given register.
            throw = Throw { source: RegIdx };

            [jump] jump = Jump { target: InstIdx };

            [jump_if] jump_if = JumpIf { target: InstIdx, arg: RegIdx, is_true: bool };

            [special]
            /// Call a function with arguments in the topmost stack frame.
            ///
            /// Pops all arguments from the topmost stack frame, then pushes all returns as a new
            /// stack frame.
            call = Call { func: RegIdx };

            [special]
            /// Return with values in the topmost stack frame.
            return_ = Return {};
        }
    };
}

macro_rules! define_instruction {
    ($(
        [$_category:ident] $(#[$attr:meta])* $snake_name:ident = $name:ident { $($field:ident: $field_ty:ty),* $(,)? };
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
                [$_category:ident] $(#[$_attr:meta])* $snake_name:ident = $name:ident { $($field:ident: $field_ty:ident),* $(,)? };
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
