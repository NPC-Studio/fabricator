use std::fmt;

pub type RegIdx = u8;
pub type ConstIdx = u16;
pub type ParamIdx = u8;
pub type HeapIdx = u8;
pub type ProtoIdx = u8;
pub type MagicIdx = u16;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            simple => undefined = Undefined { dest: RegIdx };
            simple => load_constant = LoadConstant { dest: RegIdx, constant: ConstIdx };
            simple => closure = Closure { dest: RegIdx, proto: ProtoIdx };
            simple => get_heap = GetHeap { dest: RegIdx, heap: HeapIdx };
            simple => set_heap = SetHeap { heap: HeapIdx, source: RegIdx };
            simple => global = Global { dest: RegIdx };
            simple => this = This { dest: RegIdx };
            simple => new_object = NewObject { dest: RegIdx };
            simple => param = Param { dest: RegIdx, index: ParamIdx };
            simple => get_field = GetField { dest: RegIdx, object: RegIdx, key: RegIdx };
            simple => set_field = SetField  { object: RegIdx, key: RegIdx, value: RegIdx };
            simple => get_field_const = GetFieldConst { dest: RegIdx, object: RegIdx, key: ConstIdx };
            simple => set_field_const = SetFieldConst  { object: RegIdx, key: ConstIdx, value: RegIdx };
            simple => move_ = Move { dest: RegIdx, source: RegIdx };
            simple => not = Not { dest: RegIdx, arg: RegIdx };
            simple => add = Add { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => sub = Sub { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => test_equal = TestEqual { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => test_not_equal = TestNotEqual { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => test_less = TestLess { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => test_less_equal = TestLessEqual { dest: RegIdx, arg1: RegIdx, arg2: RegIdx };
            simple => push = Push { source: RegIdx, len: u8 };
            simple => pop = Pop { dest: RegIdx, len: u8 };
            simple => get_magic = GetMagic { dest: RegIdx, magic: MagicIdx };
            simple => set_magic = SetMagic { magic: MagicIdx, source: RegIdx };

            jump => jump = Jump { offset: i16 };
            jump => jump_if = JumpIf { offset: i16, arg: RegIdx, is_true: bool };

            call => call = Call { func: RegIdx, returns: u8 };
            call => method = Method { this: RegIdx, func: RegIdx, returns: u8 };
            call => return_ = Return { };
        }
    };
}

pub(crate) use for_each_instruction;

macro_rules! define_instruction {
    ($(
        $_:ident => $snake_name:ident = $name:ident { $($field:ident: $field_ty:ty),* };
    )*) => {
        #[derive(Copy, Clone, Eq, PartialEq)]
        pub enum Instruction {
            $($name {
                $($field: $field_ty),*
            }),*
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
            ($other:ident) => {
                ""
            };
        }

        macro_rules! impl_debug {
            ($(
                $_:ident => $snake_name:ident = $name:ident { $($field:ident: $field_ty:ident),* };
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
