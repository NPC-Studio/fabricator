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

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "B{}", self.index())
    }
}

impl fmt::Display for InstId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "I{}", self.index())
    }
}

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "V{}", self.index())
    }
}

impl fmt::Display for ShadowVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "S{}", self.index())
    }
}

impl fmt::Display for ThisScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Ts{}", self.index())
    }
}

impl fmt::Display for CallScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Cs{}", self.index())
    }
}

impl fmt::Display for FuncId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "F{}", self.index())
    }
}

/// The location of an instruction in an IR.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InstLocation {
    pub block_id: BlockId,
    pub index: usize,
}

impl fmt::Display for InstLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "B{}:{}", self.block_id.index(), self.index)
    }
}

impl InstLocation {
    pub fn new(block_id: BlockId, inst_index: usize) -> Self {
        Self {
            block_id,
            index: inst_index,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnOp {
    IsDefined,
    IsUndefined,
    Test,
    Not,
    Negate,
    Increment,
    Decrement,
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
    Xor,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
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
/// # Scopes
///
/// Several IR instructions operate on some resource that is shared between instructions. These
/// resources require bounded regions of the CFG where the resource state can be statically
/// determined, and these regions are called IR "scopes".
///
/// Specific scopes may have further restrictions, but all scopes share a number of rules:
///
/// * Scopes must have *exactly one* open instruction.
/// * Scopes may have any number of close instructions, including zero. Any function exit implicitly
///   closes all scopes.
/// * All instructions which operate on a scope must fall within the live region of that scope. The
///   CFG region between the single open instruction and any close is referred as the scope's "live
///   region", or that the scope is "open" within this region.
/// * All incoming edges to a block must be in the same state for every scope, either all open or
///   all closed for that scope. In simpler terms, this means that every region in the CFG must be
///   deterministically either open or closed for every scope. This rule also implies that it is not
///   possible to re-open a scope without closing it first, because if the block is reachable, it
///   has an incoming edge where it must be closed, so every incoming edge must be closed.
/// * All explicit scope closures must occur when a scope is open. Since we know that nowhere in
///   the CFG can be both open and closed depending on the path taken, this simply means that there
///   cannot be a "dead" close instruction that always closes an already closed scope.
///
/// Primarily these rules enable correct and more efficient codegen where scopes represent a
/// resource with strict rules about obtaining and releasing it, but they also serve a second
/// important purpose. The rules may be sometimes more restrictive than strictly required for the
/// resource in question, but the rules help to verify that generated IR is correct in complex
/// situations. It is very easy to accidentally generate IR for loops that places a scope open
/// close on the inside rather than the outside of a loop or vice versa. Since these rules can be
/// statically verified, this helps to catch what would otherwise be very hard to track down IR
/// generation bugs.
///
/// # Variables
///
/// IR "variables" represent notionally heap allocated values that are an escape hatch for SSA form.
/// Each registered `Variable` references a unique variable, and `GetVariable` and `SetVariable`
/// instructions read from and write to these variables.
///
/// The compiler will generate IR that uses these variables to represent actual variables in code,
/// and will rely on IR optimization to convert them to SSA form, potentially by inserting `Phi` and
/// `Upsilon` instructions.
///
/// Normally, *all* owned IR variables can be converted into SSA form in this way, but any variables
/// that are prototype-level statics or shared across parent / child closures will not be converted
/// to SSA form. These shared variables that remain after optimization will instead be represented
/// by VM "heap" variables, allowing them to be shared across closures.
///
/// Variables have a "scope" as described above, their scope is opened with `OpenVariable` and
/// closed with `CloseVariable`. There are very few additional rules:
///
/// * Static and upvalue variables can be used anywhere in their containing function and in
///   well-formed IR must have neither `OpenVariable` nor `CloseVariable` instructions. These
///   variables are always considered open for the entire CFG of the function that contains
///   them.
/// * The `Closure` instruction is considered to use every variable that the child function
///   references as an upvalue.
///
/// # Nested Scopes
///
/// "Nested Scopes" have similar rules to regular scopes but are more restrictive. The purpose of
/// nested scopes is to guard access in the IR to some single shared resource, ensuring that only
/// one logical "section" of the IR is operating on this resource at a time.
///
/// There are two kinds of nested scopes: "this" scopes and "call" scopes; "this" scopes guard the
/// `this` and `other` registers, and "call" scopes guard the stack.
///
/// Nested scopes come with some additional rules:
///
/// * All instructions which operate on a scope must NOT be within the open region of some other,
///   different scope.
/// * As an exception to the above rule, nested scopes, like their name implies, may be *strictly*
///   nested. "Strictly nested" means that the live range of an inner scope must lie *entirely*
///   within the live range of the outer scope. In simpler terms, this means that the inner open
///   must always come after the outer open, and any inner close must always come before any outer
///   close (or they can be closed at the same time due to a function exit). This exception to
///   the above rule only applies to the *innermost* open scope, an outer scope may not have any
///   instructions which fall in the live region of an inner scope.
///
/// The reasoning for this rule is that since these scopes guard a single shared resource, any
/// overlapping access of this resource would cause unrelated sections of IR to interfere. Nesting
/// is explicitly allowed becuase open instructions are meant to save the previous state of the
/// resource and the close instructions are meant to restore it. As long as scopes are strictly
/// nested, then the state of the resource will be saved and restored in such a way that an outer
/// scope should not be able to observe if an inner scope was opened and closed inside.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction<S> {
    NoOp,
    Copy(InstId),
    Undefined,
    Boolean(bool),
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
            Instruction::Boolean(..) => true,
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
    Throw(InstId),
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
            Exit::Throw(value) => Some(*value),
            Exit::Return { value } => *value,
            Exit::Branch { cond, .. } => Some(*cond),
            _ => None,
        }
        .into_iter()
    }

    pub fn sources_mut(&mut self) -> impl Iterator<Item = &mut InstId> + '_ {
        match self {
            Exit::Throw(value) => Some(value),
            Exit::Return { value } => value.as_mut(),
            Exit::Branch { cond, .. } => Some(cond),
            _ => None,
        }
        .into_iter()
    }

    pub fn successors(&self) -> impl Iterator<Item = BlockId> + '_ {
        type Array = ArrayVec<BlockId, 2>;

        match self {
            Exit::Return { .. } | Exit::Throw(_) => Array::from_iter([]),
            &Exit::Jump(block_id) => Array::from_iter([block_id]),
            &Exit::Branch {
                if_true, if_false, ..
            } => Array::from_iter([if_true, if_false]),
        }
        .into_iter()
    }

    /// Returns true if this exit has no successors because it exits the function.
    pub fn exits_function(&self) -> bool {
        match self {
            Exit::Return { .. } | Exit::Throw(_) => true,
            Exit::Jump(_) | Exit::Branch { .. } => false,
        }
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
            writeln!(f, "block {}:", block_id)?;

            for &inst_id in &block.instructions {
                let inst = &self.instructions[inst_id];

                write_indent(f, 4)?;
                write!(f, "I{}: ", inst_id.index())?;

                match *inst {
                    Instruction::NoOp => {
                        writeln!(f, "no_op()")?;
                    }
                    Instruction::Copy(source) => {
                        writeln!(f, "copy({source})")?;
                    }
                    Instruction::Undefined => {
                        writeln!(f, "undefined()")?;
                    }
                    Instruction::Boolean(b) => {
                        writeln!(f, "boolean({b})")?;
                    }
                    Instruction::Constant(ref constant) => {
                        writeln!(f, "constant({:?})", constant.as_str())?;
                    }
                    Instruction::Closure(closure) => {
                        writeln!(f, "closure({closure})")?;
                    }
                    Instruction::OpenVariable(var) => {
                        writeln!(f, "open_var({var})")?;
                    }
                    Instruction::GetVariable(var) => {
                        writeln!(f, "get_var({var})")?;
                    }
                    Instruction::SetVariable(var, source) => {
                        writeln!(f, "set_var({var}, {source})")?;
                    }
                    Instruction::CloseVariable(var) => {
                        writeln!(f, "close_var({var})")?;
                    }
                    Instruction::GetMagic(ref magic) => {
                        writeln!(f, "get_magic({:?})", magic.as_ref())?;
                    }
                    Instruction::SetMagic(ref magic, source) => {
                        writeln!(f, "set_magic({:?}, {source})", magic.as_ref())?;
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
                        writeln!(f, "open_this_scope({scope})")?;
                    }
                    Instruction::SetThis(scope, this) => {
                        writeln!(f, "set_this({scope}, this = {this})")?;
                    }
                    Instruction::CloseThisScope(scope) => {
                        writeln!(f, "close_this_scope({scope})")?;
                    }
                    Instruction::NewObject => {
                        writeln!(f, "new_object()")?;
                    }
                    Instruction::NewArray => {
                        writeln!(f, "new_array()")?;
                    }
                    Instruction::Argument(ind) => {
                        writeln!(f, "argument({ind})")?;
                    }
                    Instruction::GetField { object, key } => {
                        writeln!(f, "get_field(object = {object}, key = {key})",)?;
                    }
                    Instruction::SetField { object, key, value } => {
                        writeln!(
                            f,
                            "set_field(object = {object}, key = {key}, value = {value})",
                        )?;
                    }
                    Instruction::GetFieldConst { object, ref key } => {
                        writeln!(f, "get_field(object = {object}, key = {:?})", key.as_str(),)?;
                    }
                    Instruction::SetFieldConst {
                        object,
                        ref key,
                        value,
                    } => {
                        writeln!(
                            f,
                            "set_field(object = {object}, key = {:?}, value = {value})",
                            key.as_str(),
                        )?;
                    }
                    Instruction::GetIndex { array, ref indexes } => {
                        write!(f, "get_index(array = {array}, indexes = [")?;
                        for (i, &ind) in indexes.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{ind}")?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::SetIndex {
                        array,
                        ref indexes,
                        value,
                    } => {
                        write!(f, "set_index(array = {array}, value = {value}, indexes = [",)?;
                        for (i, &ind) in indexes.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{ind}")?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::GetIndexConst { array, ref index } => {
                        writeln!(
                            f,
                            "get_index(array = {array}, indexes = [{:?}])",
                            index.as_str(),
                        )?;
                    }
                    Instruction::SetIndexConst {
                        array,
                        ref index,
                        value,
                    } => {
                        writeln!(
                            f,
                            "set_index(array = {array}, value = {value}, indexes = [{:?}])",
                            index.as_str(),
                        )?;
                    }
                    Instruction::Phi(shadow) => writeln!(f, "phi({shadow})")?,
                    Instruction::Upsilon(shadow, source) => {
                        writeln!(f, "upsilon({shadow}, {source})")?
                    }
                    Instruction::UnOp { op, source } => match op {
                        UnOp::IsDefined => writeln!(f, "is_defined({source})")?,
                        UnOp::IsUndefined => writeln!(f, "is_undefined({source})")?,
                        UnOp::Test => writeln!(f, "into_bool({source})")?,
                        UnOp::Not => writeln!(f, "not({source})")?,
                        UnOp::Negate => writeln!(f, "neg({source})")?,
                        UnOp::Increment => writeln!(f, "increment({source})")?,
                        UnOp::Decrement => writeln!(f, "decrement({source})")?,
                    },
                    Instruction::BinOp { left, op, right } => match op {
                        BinOp::Add => writeln!(f, "add({left}, {right})")?,
                        BinOp::Sub => writeln!(f, "sub({left}, {right})")?,
                        BinOp::Mult => writeln!(f, "mult({left}, {right})")?,
                        BinOp::Div => writeln!(f, "div({left}, {right})")?,
                        BinOp::Rem => writeln!(f, "rem({left}, {right})")?,
                        BinOp::IDiv => writeln!(f, "idiv({left}, {right})")?,
                        BinOp::LessThan => writeln!(f, "less_than({left}, {right})")?,
                        BinOp::LessEqual => writeln!(f, "less_equal({left}, {right})")?,
                        BinOp::Equal => writeln!(f, "equal({left}, {right})")?,
                        BinOp::NotEqual => writeln!(f, "not_equal({left}, {right})")?,
                        BinOp::GreaterThan => writeln!(f, "greater_than({left}, {right})")?,
                        BinOp::GreaterEqual => writeln!(f, "greater_equal({left}, {right})")?,
                        BinOp::And => writeln!(f, "and({left}, {right})")?,
                        BinOp::Or => writeln!(f, "or({left}, {right})")?,
                        BinOp::Xor => writeln!(f, "xor({left}, {right})")?,
                        BinOp::BitAnd => writeln!(f, "bit_and({left}, {right})")?,
                        BinOp::BitOr => writeln!(f, "bit_or({left}, {right})")?,
                        BinOp::BitXor => writeln!(f, "bit_xor({left}, {right})")?,
                        BinOp::BitShiftLeft => writeln!(f, "bit_shift_left({left}, {right})")?,
                        BinOp::BitShiftRight => writeln!(f, "bit_shift_right({left}, {right})")?,
                        BinOp::NullCoalesce => writeln!(f, "null_coalesce({left}, {right})")?,
                    },
                    Instruction::OpenCall {
                        scope,
                        func,
                        ref args,
                    } => {
                        write!(f, "open_call({scope}, {func}, args = [",)?;
                        for (i, &arg) in args.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{arg}")?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::GetReturn(scope, index) => {
                        writeln!(f, "get_return({scope}, {})", index)?;
                    }
                    Instruction::CloseCall(scope) => {
                        writeln!(f, "close_call({scope})")?;
                    }
                }
            }

            write_indent(f, 4)?;
            match block.exit {
                Exit::Return { value } => match value {
                    Some(value) => {
                        writeln!(f, "return({value})")?;
                    }
                    None => {
                        writeln!(f, "return()")?;
                    }
                },
                Exit::Throw(value) => {
                    writeln!(f, "throw({value})")?;
                }
                Exit::Jump(block_id) => {
                    writeln!(f, "jump({})", block_id)?;
                }
                Exit::Branch {
                    cond,
                    if_true,
                    if_false,
                } => {
                    writeln!(f, "branch({cond}, false => {if_false}, true => {if_true})")?;
                }
            }

            Ok(())
        };

        write_indent(f, 0)?;
        writeln!(f, "start_block({})", self.start_block)?;

        for block_id in self.blocks.ids() {
            write_block(f, block_id)?;
        }

        if !self.shadow_vars.is_empty() {
            write_indent(f, 0)?;
            write!(f, "shadow_vars:")?;
            for shadow_var in self.shadow_vars.ids() {
                write!(f, " {shadow_var}")?;
            }
            writeln!(f)?;
        }

        if !self.variables.is_empty() {
            write_indent(f, 0)?;
            writeln!(f, "variables:")?;
            for (id, var) in self.variables.iter() {
                write_indent(f, 4)?;
                write!(f, "{}: ", id)?;
                match var {
                    Variable::Owned => write!(f, "Owned")?,
                    Variable::Static(init) => write!(f, "Static({:?})", init.as_str())?,
                    Variable::Upper(uid) => write!(f, "Upper({uid})")?,
                }
                writeln!(f)?;
            }
        }

        for (func_id, function) in self.functions.iter() {
            write_indent(f, 0)?;
            writeln!(f, "function {func_id}:")?;
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
