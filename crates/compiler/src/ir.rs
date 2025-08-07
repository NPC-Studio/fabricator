use std::fmt;

use arrayvec::ArrayVec;
use fabricator_util::typed_id_map::{IdMap, SecondaryMap, new_id_type};
use fabricator_vm::{FunctionRef, Span};

use crate::constant::Constant;

new_id_type! {
    pub struct BlockId;
    pub struct InstId;
    pub struct VarId;
    pub struct ShadowVar;
    pub struct ThisScope;
    pub struct CallScope;
    pub struct FuncId;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Rem,
    IDiv,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
    NullCoalesce,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Variable<S> {
    /// A heap variable owned by a closure.
    Owned,
    /// A static variable owned by a *prototype*.
    Static(Constant<S>),
    /// A reference to a variable in the immediate parent function. Contains the `VarId` for the
    /// parent function.
    Upper(VarId),
}

impl<S> Variable<S> {
    #[must_use]
    pub fn is_owned(&self) -> bool {
        matches!(self, Self::Owned)
    }
}

/// A single IR instruction.
///
/// IR instructions are always in SSA (Single Static Assignment) form and thus have an implicit
/// "output variable". Other instructions that use the output of a previous instruction will
/// reference that instruction directly via `InstId`.
///
/// In order for the IR to be well-formed, instructions are only allowed to appear *once* in an
/// entire IR, or in other words, the same `InstId` cannot be re-used either in the same block
/// or in different blocks. Also, all uses of an instruction must be "dominated" by its singular
/// definition -- in other words, all paths through the CFG (Control Flow Graph) starting with
/// `start_block` that reach any use of an instruction must always pass through its definition.
///
/// # SSA Form
///
/// SSA form requires that join points in the CFG have special instructions to select between
/// different data definitions depending on the path in the CFG that was taken. These are normally
/// called "phi functions" and are described by Cytron et al. here:
///
/// <https://dl.acm.org/doi/pdf/10.1145/115372.115320>
///
/// We use a slight modification to this system here. Instead of "phi" instructions referencing the
/// instructions they select between, instead a separate "upsilon" instruction writes to a "shadow
/// variable" that is present for every `Phi` instruction. The `ShadowVar` type is the unique
/// identifier for this shadow variable in a single phi instruction.
///
/// This phi / upsilon SSA form was invented by Filip Pizlo is more deeply explained in this
/// document (where he calls it "pizlo-form"):
///
/// <https://gist.github.com/pizlonator/79b0aa601912ff1a0eb1cb9253f5e98d>
///
/// In order for the IR to be well-formed, any `ShadowVar` identifier must be unique and owned by
/// a *single* `Phi` instruction. These shadow variables are Single Static *Use*, they are used
/// only once by a unique `Phi` instruction. Additionally, all paths through the CFG starting with
/// `start_block` that may reach a `Phi` instruction must have an `Upsilon` that assigns to that
/// `Phi`'s shadow variable to ensure that it has a defined value.
///
/// # Variables
///
/// IR "variables" represent notionally heap allocated values that are an escape hatch for SSA form.
/// Each `Variable` references a unique variable, and `GetVariable` and `SetVariable` instructions
/// read from and write to these variables.
///
/// The output of the compiler will use these IR variables to represent actual variables in code,
/// and will rely on IR optimization to convert them to SSA form, potentially by inserting `Phi` and
/// `Upsilon` instructions.
///
/// Normally, *all* owned IR variables can be converted into SSA form in this way, but any variables
/// that are prototype-level statics or shared across parent / child functions will not be converted
/// to SSA form. These shared variables that remain after optimization will instead be represented
/// by VM "heap" variables, allowing them to be shared across closures.
///
/// In order for the IR to be well-formed, variables must follow the following rules:
///
/// * All owned variables must have *exactly one* `OpenVariable` instruction and *at most one*
///   `CloseVariable` instruction.
/// * Static and upvalue variables can be used anywhere in their containing function and in
///   well-formed IR must have neither `OpenVariable` nor `CloseVariable` instructions. These
///   variables are always open for the entire lifetime of the closure that contains them.
/// * There should be nowhere in the CFG where a variable can potentially be either opened or
///   closed, depending on the path taken. Every location must, for every variable, be either
///   *definitely* open or *definitely* closed for that variable. This is meant to imply also that
///   it should not be possible to re-open a variable without closing it first (you should not be
///   able to enter a single block in both an open and closed state).
/// * Every `GetVariable`, `SetVariable`, and `Closure` instruction that uses a variable must be in
///   a definitely-open location in the CFG for that variable.
///
/// These rules are more restrictive than strictly necessary to generate VM code, but they exist
/// to verify that generated IR is correct in situations where a closure is crated and closes over
/// variables that are opened / closed in some kind of loop. These errors can be hard to catch and
/// would otherwise *only* appear when variables are actually captured by a closure, so verifying
/// these rules in the generated IR for *all* variables helps ensure that generated IR is always
/// correct.
///
/// # Scopes
///
/// "Scopes" have similar rules to variables but are even more restrictive. The purpose of scopes is
/// to guard access in the IR to some shared resource, ensuring that only one logical "section" of
/// the IR is operating on this resource at a time.
///
/// There are two kinds of "scopes" that work the same way: "this" scopes and "call" scopes; "this"
/// scopes guard the `this` and `other` registers, and "call" scopes guard the stack.
///
/// The rules for "open" and "close" instructions on scopes are similar to the ones for variables:
///
/// * All scopes must have *exactly one* open instruction and *at most one* close instruction.
/// * All instructions which operate on a scope must fall strictly between the instruction that
///   opens the scope and the instruction that closes it (if it exists).
/// * There should be nowhere in the CFG where a scope can potentially be either opened or closed,
///   depending on the path taken. Every location must, for every scope, be either *definitely* open
///   or *definitely* closed for that scope. Similar to variables, this is meant to imply also that
///   it should not be possible to re-open a scope without closing it first (you should not be able
///   to enter a single block in both an open and closed state).
///
/// There is one additional restriction, due to the fact that all scopes operate on a single shared
/// resource:
///
/// * All instructions which operate on an opened scope must NOT be within some *other* open / close
///   pair for a different scope.
///
/// Scopes are allowed to be nested but must be strictly so: the inner scope must be closed before
/// the outer scope is closed, and none of the operations on the outer scope may fall within the
/// inner scope. This is implied by the above rule but subtly so: the *close* instruction counts as
/// "operating on" for its own scope, so it cannot be within some other open / close pair.
///
/// The reasoning for this rule is that since the guarded resource is shared, any overlapping
/// access of this resource would cause unrelated sections of IR to interfere. Nesting is explicitly
/// allowed becuase open instructions are meant to set-up these shared resources in some way no
/// matter their current state, and the close instructions are meant to *restore the previous
/// state*. As long as scopes are strictly nested, then the state of the resource will be saved and
/// restored in such a way that an outer scope should not be able to observe if an inner scope is
/// opened and closed inside.
///
/// Similar to variables, these rules are not strictly necessary to generate working VM code, but
/// they exist catch IR generation errors which would otherwise be very difficult to debug.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction<S> {
    NoOp,
    Copy(InstId),
    Undefined,
    Constant(Constant<S>),
    Closure(FuncId),
    OpenVariable(VarId),
    GetVariable(VarId),
    SetVariable(VarId, InstId),
    CloseVariable(VarId),
    GetMagic(S),
    SetMagic(S, InstId),
    Globals,
    This,
    Other,
    CurrentClosure,
    OpenThisScope(ThisScope),
    SetThis(ThisScope, InstId),
    CloseThisScope(ThisScope),
    NewObject,
    NewArray,
    Argument(usize),
    GetField {
        object: InstId,
        key: InstId,
    },
    SetField {
        object: InstId,
        key: InstId,
        value: InstId,
    },
    GetFieldConst {
        object: InstId,
        key: Constant<S>,
    },
    SetFieldConst {
        object: InstId,
        key: Constant<S>,
        value: InstId,
    },
    GetIndex {
        array: InstId,
        indexes: Vec<InstId>,
    },
    SetIndex {
        array: InstId,
        indexes: Vec<InstId>,
        value: InstId,
    },
    GetIndexConst {
        array: InstId,
        index: Constant<S>,
    },
    SetIndexConst {
        array: InstId,
        index: Constant<S>,
        value: InstId,
    },
    Phi(ShadowVar),
    Upsilon(ShadowVar, InstId),
    UnOp {
        op: UnOp,
        source: InstId,
    },
    BinOp {
        left: InstId,
        op: BinOp,
        right: InstId,
    },
    OpenCall {
        scope: CallScope,
        func: InstId,
        args: Vec<InstId>,
    },
    GetReturn(CallScope, usize),
    CloseCall(CallScope),
}

impl<S> Instruction<S> {
    pub fn constants(&self) -> impl Iterator<Item = &Constant<S>> + '_ {
        match self {
            Instruction::Constant(constant) => Some(constant),
            Instruction::GetFieldConst { key, .. } => Some(key),
            Instruction::SetFieldConst { key, .. } => Some(key),
            Instruction::GetIndexConst { index, .. } => Some(index),
            Instruction::SetIndexConst { index, .. } => Some(index),
            _ => None,
        }
        .into_iter()
    }

    pub fn sources(&self) -> impl Iterator<Item = InstId> + '_ {
        macro_rules! make_iter {
            ($small:expr, $rest:expr) => {
                ArrayVec::<_, 3>::from_iter($small.into_iter())
                    .into_iter()
                    .chain($rest.iter().copied())
            };

            ($small:expr) => {
                make_iter!($small, &[])
            };
        }

        match self {
            &Instruction::Copy(source) => make_iter!([source]),
            &Instruction::SetVariable(_, source) => make_iter!([source]),
            &Instruction::SetMagic(_, source) => make_iter!([source]),
            &Instruction::SetThis(_, this) => make_iter!([this]),
            &Instruction::GetField { object, key } => make_iter!([object, key]),
            &Instruction::SetField { object, key, value } => make_iter!([object, key, value]),
            &Instruction::GetFieldConst { object, .. } => make_iter!([object]),
            &Instruction::SetFieldConst { object, value, .. } => make_iter!([object, value]),
            Instruction::GetIndex { array, indexes } => make_iter!([*array], indexes),
            Instruction::SetIndex {
                array,
                indexes,
                value,
            } => make_iter!([*array, *value], indexes),
            &Instruction::GetIndexConst { array, .. } => make_iter!([array]),
            &Instruction::SetIndexConst { array, value, .. } => make_iter!([array, value]),
            &Instruction::Upsilon(_, source) => make_iter!([source]),
            &Instruction::UnOp { source, .. } => make_iter!([source]),
            &Instruction::BinOp { left, right, .. } => make_iter!([left, right]),
            Instruction::OpenCall { func, args, .. } => {
                make_iter!([*func], args)
            }
            _ => make_iter!([]),
        }
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut InstId> + '_ {
        macro_rules! make_iter {
            ($small:expr, $rest:expr) => {
                ArrayVec::<_, 3>::from_iter($small.into_iter())
                    .into_iter()
                    .chain($rest.iter_mut())
            };

            ($small:expr) => {
                make_iter!($small, &mut [])
            };
        }

        match self {
            Instruction::Copy(source) => make_iter!([source]),
            Instruction::SetVariable(_, source) => make_iter!([source]),
            Instruction::SetMagic(_, source) => make_iter!([source]),
            Instruction::SetThis(_, this) => make_iter!([this]),
            Instruction::GetField { object, key } => make_iter!([object, key]),
            Instruction::SetField { object, key, value } => make_iter!([object, key, value]),
            Instruction::GetFieldConst { object, .. } => make_iter!([object]),
            Instruction::SetFieldConst { object, value, .. } => make_iter!([object, value]),
            Instruction::GetIndex { array, indexes } => make_iter!([array], indexes),
            Instruction::SetIndex {
                array,
                indexes,
                value,
            } => make_iter!([array, value], indexes),
            Instruction::GetIndexConst { array, .. } => make_iter!([array]),
            Instruction::SetIndexConst { array, value, .. } => make_iter!([array, value]),
            Instruction::Upsilon(_, source) => make_iter!([source]),
            Instruction::UnOp { source, .. } => make_iter!([source]),
            Instruction::BinOp { left, right, .. } => make_iter!([left, right]),
            Instruction::OpenCall { func, args, .. } => {
                make_iter!([func], args)
            }
            _ => make_iter!([]),
        }
    }

    pub fn has_value(&self) -> bool {
        match self {
            Instruction::Copy(..) => true,
            Instruction::Undefined => true,
            Instruction::Constant(..) => true,
            Instruction::Closure(..) => true,
            Instruction::GetVariable(..) => true,
            Instruction::GetMagic(..) => true,
            Instruction::Globals => true,
            Instruction::This => true,
            Instruction::Other => true,
            Instruction::CurrentClosure => true,
            Instruction::NewObject => true,
            Instruction::NewArray => true,
            Instruction::Argument(..) => true,
            Instruction::GetField { .. } => true,
            Instruction::GetFieldConst { .. } => true,
            Instruction::GetIndex { .. } => true,
            Instruction::GetIndexConst { .. } => true,
            Instruction::Phi(..) => true,
            Instruction::UnOp { .. } => true,
            Instruction::BinOp { .. } => true,
            Instruction::GetReturn(..) => true,
            _ => false,
        }
    }

    pub fn has_effect(&self) -> bool {
        match self {
            Instruction::OpenVariable(..) => true,
            Instruction::SetVariable { .. } => true,
            Instruction::CloseVariable(_) => true,
            Instruction::GetMagic(..) => true,
            Instruction::SetMagic(..) => true,
            Instruction::OpenThisScope(..) => true,
            Instruction::SetThis(..) => true,
            Instruction::CloseThisScope(..) => true,
            Instruction::GetField { .. } => true,
            Instruction::SetField { .. } => true,
            Instruction::GetFieldConst { .. } => true,
            Instruction::SetFieldConst { .. } => true,
            Instruction::GetIndex { .. } => true,
            Instruction::SetIndex { .. } => true,
            Instruction::GetIndexConst { .. } => true,
            Instruction::SetIndexConst { .. } => true,
            Instruction::Upsilon(..) => true,
            Instruction::UnOp { .. } => true,
            Instruction::BinOp { .. } => true,
            Instruction::OpenCall { .. } => true,
            Instruction::CloseCall(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Exit {
    Return {
        value: Option<InstId>,
    },
    Jump(BlockId),
    Branch {
        cond: InstId,
        if_true: BlockId,
        if_false: BlockId,
    },
}

impl Exit {
    pub fn sources(&self) -> impl Iterator<Item = InstId> + '_ {
        match self {
            Exit::Return { value } => *value,
            Exit::Branch { cond, .. } => Some(*cond),
            _ => None,
        }
        .into_iter()
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut InstId> + '_ {
        match self {
            Exit::Return { value } => value.as_mut(),
            Exit::Branch { cond, .. } => Some(cond),
            _ => None,
        }
        .into_iter()
    }

    pub fn successors(&self) -> impl Iterator<Item = BlockId> + '_ {
        type Array = ArrayVec<BlockId, 2>;

        match self {
            Exit::Return { .. } => Array::from_iter([]),
            &Exit::Jump(block_id) => Array::from_iter([block_id]),
            &Exit::Branch {
                if_true, if_false, ..
            } => Array::from_iter([if_true, if_false]),
        }
        .into_iter()
    }

    pub fn successors_mut(&mut self) -> impl Iterator<Item = &mut BlockId> + '_ {
        type Array<'a> = ArrayVec<&'a mut BlockId, 2>;

        match self {
            Exit::Return { .. } => Array::from_iter([]),
            Exit::Jump(block_id) => Array::from_iter([block_id]),
            Exit::Branch {
                if_true, if_false, ..
            } => Array::from_iter([if_true, if_false]),
        }
        .into_iter()
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<InstId>,
    pub exit: Exit,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            instructions: Vec::new(),
            exit: Exit::Return { value: None },
        }
    }
}

pub type InstructionMap<S> = IdMap<InstId, Instruction<S>>;
pub type SpanMap = SecondaryMap<InstId, Span>;
pub type BlockMap = IdMap<BlockId, Block>;
pub type VariableMap<S> = IdMap<VarId, Variable<S>>;
pub type ShadowVarSet = IdMap<ShadowVar, ()>;
pub type ThisScopeSet = IdMap<ThisScope, ()>;
pub type CallScopeSet = IdMap<CallScope, ()>;
pub type FunctionMap<S> = IdMap<FuncId, Function<S>>;

#[derive(Clone)]
pub struct Function<S> {
    pub num_parameters: usize,
    pub is_constructor: bool,

    pub reference: FunctionRef,
    pub instructions: InstructionMap<S>,
    pub spans: SpanMap,
    pub blocks: BlockMap,
    pub variables: VariableMap<S>,
    pub shadow_vars: ShadowVarSet,
    pub this_scopes: ThisScopeSet,
    pub call_scopes: CallScopeSet,

    /// Inner functions declared within this function.
    pub functions: FunctionMap<S>,

    pub start_block: BlockId,
}

impl<S: AsRef<str>> Function<S> {
    pub fn pretty_print(&self, f: &mut dyn fmt::Write, indent: u8) -> fmt::Result {
        let base_indent = indent as usize;
        let write_indent = |f: &mut dyn fmt::Write, indent: u8| -> fmt::Result {
            let indent = base_indent + indent as usize;
            write!(f, "{:indent$}", "")?;
            Ok(())
        };

        let write_block = |f: &mut dyn fmt::Write, block_id: BlockId| -> fmt::Result {
            let block = &self.blocks[block_id];

            write_indent(f, 0)?;
            writeln!(f, "block B{}:", block_id.index())?;

            for &inst_id in &block.instructions {
                let inst = &self.instructions[inst_id];

                write_indent(f, 4)?;
                write!(f, "I{}: ", inst_id.index())?;

                match inst {
                    Instruction::NoOp => {
                        writeln!(f, "no_op()")?;
                    }
                    Instruction::Copy(source) => {
                        writeln!(f, "copy(I{})", source.index())?;
                    }
                    Instruction::Undefined => {
                        writeln!(f, "undefined()")?;
                    }
                    Instruction::Constant(constant) => {
                        writeln!(f, "constant({:?})", constant.as_str())?;
                    }
                    Instruction::Closure(closure) => {
                        writeln!(f, "closure(F{})", closure.index())?;
                    }
                    Instruction::OpenVariable(var) => {
                        writeln!(f, "open_var(V{})", var.index())?;
                    }
                    Instruction::GetVariable(var) => {
                        writeln!(f, "get_var(V{})", var.index())?;
                    }
                    Instruction::SetVariable(var, source) => {
                        writeln!(f, "set_var(V{}, I{})", var.index(), source.index())?;
                    }
                    Instruction::CloseVariable(var) => {
                        writeln!(f, "close_var(V{})", var.index())?;
                    }
                    Instruction::GetMagic(magic) => {
                        writeln!(f, "get_magic({:?})", magic.as_ref())?;
                    }
                    Instruction::SetMagic(magic, source) => {
                        writeln!(f, "set_magic({:?}, I{})", magic.as_ref(), source.index())?;
                    }
                    Instruction::Globals => {
                        writeln!(f, "globals()")?;
                    }
                    Instruction::This => {
                        writeln!(f, "this()")?;
                    }
                    Instruction::Other => {
                        writeln!(f, "other()")?;
                    }
                    Instruction::CurrentClosure => {
                        writeln!(f, "current_closure()")?;
                    }
                    Instruction::OpenThisScope(scope) => {
                        writeln!(f, "open_this_scope(TS{})", scope.index())?;
                    }
                    Instruction::SetThis(scope, this) => {
                        writeln!(f, "set_this(TS{}, this = {})", scope.index(), this.index())?;
                    }
                    Instruction::CloseThisScope(scope) => {
                        writeln!(f, "close_this_scope(TS{})", scope.index())?;
                    }
                    Instruction::NewObject => {
                        writeln!(f, "new_object()")?;
                    }
                    Instruction::NewArray => {
                        writeln!(f, "new_array()")?;
                    }
                    Instruction::Argument(ind) => {
                        writeln!(f, "argument({})", ind)?;
                    }
                    Instruction::GetField { object, key } => {
                        writeln!(
                            f,
                            "get_field(object = I{}, key = I{})",
                            object.index(),
                            key.index(),
                        )?;
                    }
                    Instruction::SetField { object, key, value } => {
                        writeln!(
                            f,
                            "set_field(object = I{}, key = I{}, value = I{})",
                            object.index(),
                            key.index(),
                            value.index(),
                        )?;
                    }
                    Instruction::GetFieldConst { object, key } => {
                        writeln!(
                            f,
                            "get_field(object = I{}, key = {:?})",
                            object.index(),
                            key.as_str(),
                        )?;
                    }
                    Instruction::SetFieldConst { object, key, value } => {
                        writeln!(
                            f,
                            "set_field(object = I{}, key = {:?}, value = I{})",
                            object.index(),
                            key.as_str(),
                            value.index()
                        )?;
                    }
                    Instruction::GetIndex { array, indexes } => {
                        write!(f, "get_index(array = I{}, indexes = [", array.index(),)?;
                        for (i, &ind) in indexes.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "I{}", ind.index())?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::SetIndex {
                        array,
                        indexes,
                        value,
                    } => {
                        write!(
                            f,
                            "set_index(array = I{}, value = I{}, indexes = [",
                            array.index(),
                            value.index()
                        )?;
                        for (i, &ind) in indexes.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "I{}", ind.index())?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::GetIndexConst { array, index } => {
                        writeln!(
                            f,
                            "get_index(array = I{}, indexes = [{:?}])",
                            array.index(),
                            index.as_str(),
                        )?;
                    }
                    Instruction::SetIndexConst {
                        array,
                        index,
                        value,
                    } => {
                        writeln!(
                            f,
                            "set_index(array = I{}, value = I{}, indexes = [{:?}])",
                            array.index(),
                            value.index(),
                            index.as_str(),
                        )?;
                    }
                    Instruction::Phi(shadow) => {
                        writeln!(f, "phi(S{})", shadow.index())?;
                    }
                    Instruction::Upsilon(shadow, source) => {
                        writeln!(f, "upsilon(S{}, I{})", shadow.index(), source.index())?;
                    }
                    Instruction::UnOp { op, source } => match op {
                        UnOp::Not => {
                            writeln!(f, "not(I{})", source.index())?;
                        }
                        UnOp::Neg => {
                            writeln!(f, "neg(I{})", source.index())?;
                        }
                    },
                    Instruction::BinOp { left, op, right } => match op {
                        BinOp::Add => {
                            writeln!(f, "add(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Sub => {
                            writeln!(f, "sub(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Mult => {
                            writeln!(f, "mult(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Div => {
                            writeln!(f, "div(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Rem => {
                            writeln!(f, "rem(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::IDiv => {
                            writeln!(f, "idiv(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::LessThan => {
                            writeln!(f, "less_than(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::LessEqual => {
                            writeln!(f, "less_equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Equal => {
                            writeln!(f, "equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::NotEqual => {
                            writeln!(f, "not_equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::GreaterThan => {
                            writeln!(f, "greater_than(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::GreaterEqual => {
                            writeln!(f, "greater_equal(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::And => {
                            writeln!(f, "and(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::Or => {
                            writeln!(f, "or(I{}, I{})", left.index(), right.index())?;
                        }
                        BinOp::NullCoalesce => {
                            writeln!(f, "null_coalesce(I{}, I{})", left.index(), right.index())?;
                        }
                    },
                    Instruction::OpenCall { scope, func, args } => {
                        write!(
                            f,
                            "open_call(CS{}, I{}, args = [",
                            scope.index(),
                            func.index(),
                        )?;
                        for (i, &arg) in args.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "I{}", arg.index())?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::GetReturn(scope, index) => {
                        writeln!(f, "get_return(CS{}, {})", scope.index(), index)?;
                    }
                    Instruction::CloseCall(scope) => {
                        writeln!(f, "close_call(CS{})", scope.index())?;
                    }
                }
            }

            write_indent(f, 4)?;
            match block.exit {
                Exit::Return { value } => match value {
                    Some(value) => {
                        writeln!(f, "return(I{})", value.index())?;
                    }
                    None => {
                        writeln!(f, "return()")?;
                    }
                },
                Exit::Jump(block_id) => {
                    writeln!(f, "jump(B{})", block_id.index())?;
                }
                Exit::Branch {
                    cond,
                    if_true,
                    if_false,
                } => {
                    writeln!(
                        f,
                        "branch(I{}, false => B{}, true => B{})",
                        cond.index(),
                        if_false.index(),
                        if_true.index()
                    )?;
                }
            }

            Ok(())
        };

        write_indent(f, 0)?;
        writeln!(f, "start_block(B{})", self.start_block.index())?;

        for block_id in self.blocks.ids() {
            write_block(f, block_id)?;
        }

        if !self.shadow_vars.is_empty() {
            write_indent(f, 0)?;
            write!(f, "shadow_vars:")?;
            for shadow_var in self.shadow_vars.ids() {
                write!(f, " S{}", shadow_var.index())?;
            }
            writeln!(f)?;
        }

        if !self.variables.is_empty() {
            write_indent(f, 0)?;
            write!(f, "variables:")?;
            for (id, var) in self.variables.iter() {
                write_indent(f, 4)?;
                write!(f, "V{}: ", id.index())?;
                match var {
                    Variable::Owned => write!(f, "Owned")?,
                    Variable::Static(init) => write!(f, "Static({:?})", init.as_str())?,
                    Variable::Upper(uid) => write!(f, "Upper(V{})", uid.index())?,
                }
                writeln!(f)?;
            }
        }

        for (func_id, function) in self.functions.iter() {
            write_indent(f, 0)?;
            writeln!(f, "function F{}:", func_id.index())?;
            function.pretty_print(f, indent + 4)?;
        }

        Ok(())
    }
}

impl<S: AsRef<str>> fmt::Debug for Function<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Function(")?;
        self.pretty_print(f, 4)?;
        writeln!(f, ")")?;
        Ok(())
    }
}
