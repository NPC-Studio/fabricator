use std::fmt;

pub type RegIdx = u8;
pub type HeapIdx = u8;
pub type ConstIdx = u16;

macro_rules! for_each_instruction {
    ($macro:ident) => {
        $macro! {
            simple => load_constant = LoadConstant { dest: RegIdx, constant: ConstIdx };
            simple => get_heap = GetHeap { dest: RegIdx, heap: HeapIdx };
            simple => set_heap = SetHeap { heap: HeapIdx, source: RegIdx };
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

            jump => jump = Jump { offset: i16 };
            jump => jump_if = JumpIf { offset: i16, arg: RegIdx, is_true: bool };

            call => call = Call { func: RegIdx, args: u8, returns: u8 };
            call => return_ = Return { returns: u8 };
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
