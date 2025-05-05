use std::{collections::HashMap, fmt};

use arrayvec::ArrayVec;
use fabricator_util::typed_id_map::{IdMap, new_id_type};

use crate::constant::Constant;

new_id_type! {
    pub struct BlockId;
    pub struct InstId;
    pub struct Variable;
    pub struct ShadowVar;
    pub struct FuncId;
}

pub type ParamIndex = u8;

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
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
}

/// A single IR instruction.
///
/// Instructions are only allowed to appear in *once* across an entire IR.
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
/// Normally, *all* IR variables can be converted into SSA form in this way, but any variables
/// that are shared across parent / child functions will not be converted to SSA form. These shared
/// variables that remain after optimization will instead be represented by VM "heap" variables,
/// allowing them to be shared across closures.
///
/// In order for the IR to be well-formed, variables must follow the following rules:
///
/// * All (owned) variable must have *exactly one* `OpenVariable` instruction and *at most one*
///   `CloseVariable` instruction.
/// * Upvalue variables can be used anywhere in their containing function and in well-formed IR must
///   have neither `OpenVariable` nor `CloseVariable` instructions. Upvalue variables are always
///   open for the entire lifetime of the closure that contains them.
/// * There should be nowhere in the CFG where a variable can potentially be either opened or
///   closed, depending on the path taken. Every location must, for every variable, be either
///   *definitely* open or *definitely* closed for that variable. This is meant to imply also that
///   it should not be possible to re-open a variable without closing it first (you should not be
///   able to enter a single block in both an open and closed state).
/// * Every `GetVariable`, `SetVariable`, and `Closure` instruction that uses a variable must be in
///   a definitely-open location in the CFG for that variable.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction<S> {
    NoOp,
    Copy(InstId),
    Undefined,
    Constant(Constant<S>),
    Closure(FuncId),
    OpenVariable(Variable),
    GetVariable(Variable),
    SetVariable(Variable, InstId),
    CloseVariable(Variable),
    GetMagic(S),
    SetMagic(S, InstId),
    This,
    NewObject,
    NewArray,
    Parameter(ParamIndex),
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
        index: InstId,
    },
    SetIndex {
        array: InstId,
        index: InstId,
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
        source: InstId,
        op: UnOp,
    },
    BinOp {
        left: InstId,
        right: InstId,
        op: BinOp,
    },
    Call {
        func: InstId,
        args: Vec<InstId>,
        return_value: bool,
    },
    Method {
        this: InstId,
        func: InstId,
        args: Vec<InstId>,
        return_value: bool,
    },
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
            &Instruction::GetField { object, key } => make_iter!([object, key]),
            &Instruction::SetField { object, key, value } => make_iter!([object, key, value]),
            &Instruction::GetFieldConst { object, .. } => make_iter!([object]),
            &Instruction::SetFieldConst { object, value, .. } => make_iter!([object, value]),
            &Instruction::GetIndex { array, index } => make_iter!([array, index]),
            &Instruction::SetIndex {
                array,
                index,
                value,
            } => make_iter!([array, index, value]),
            &Instruction::GetIndexConst { array, .. } => make_iter!([array]),
            &Instruction::SetIndexConst { array, value, .. } => make_iter!([array, value]),
            &Instruction::Upsilon(_, source) => make_iter!([source]),
            &Instruction::UnOp { source, .. } => make_iter!([source]),
            &Instruction::BinOp { left, right, .. } => make_iter!([left, right]),
            Instruction::Call { func, args, .. } => make_iter!([*func], args),
            Instruction::Method {
                this, func, args, ..
            } => make_iter!([*this, *func], args),
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
            Instruction::GetField { object, key } => make_iter!([object, key]),
            Instruction::SetField { object, key, value } => make_iter!([object, key, value]),
            Instruction::GetFieldConst { object, .. } => make_iter!([object]),
            Instruction::SetFieldConst { object, value, .. } => make_iter!([object, value]),
            Instruction::GetIndex { array, index } => make_iter!([array, index]),
            Instruction::SetIndex {
                array,
                index,
                value,
            } => make_iter!([array, index, value]),
            Instruction::GetIndexConst { array, .. } => make_iter!([array]),
            Instruction::SetIndexConst { array, value, .. } => make_iter!([array, value]),
            Instruction::Upsilon(_, source) => make_iter!([source]),
            Instruction::UnOp { source, .. } => make_iter!([source]),
            Instruction::BinOp { left, right, .. } => make_iter!([left, right]),
            Instruction::Call { func, args, .. } => make_iter!([func], args),
            Instruction::Method {
                this, func, args, ..
            } => make_iter!([this, func], args),
            _ => make_iter!([]),
        }
    }

    pub fn has_value(&self) -> bool {
        match self {
            Instruction::Copy(_) => true,
            Instruction::Undefined => true,
            Instruction::Constant(_) => true,
            Instruction::Closure(_) => true,
            Instruction::GetVariable(_) => true,
            Instruction::GetMagic(_) => true,
            Instruction::This => true,
            Instruction::NewObject => true,
            Instruction::NewArray => true,
            Instruction::Parameter(_) => true,
            Instruction::GetField { .. } => true,
            Instruction::GetFieldConst { .. } => true,
            Instruction::GetIndex { .. } => true,
            Instruction::GetIndexConst { .. } => true,
            Instruction::Phi(_) => true,
            Instruction::UnOp { .. } => true,
            Instruction::BinOp { .. } => true,
            Instruction::Call { return_value, .. } => *return_value,
            Instruction::Method { return_value, .. } => *return_value,
            _ => false,
        }
    }

    pub fn has_effect(&self) -> bool {
        match self {
            Instruction::OpenVariable(_) => true,
            Instruction::SetVariable { .. } => true,
            Instruction::CloseVariable(_) => true,
            Instruction::SetMagic(_, _) => true,
            Instruction::SetField { .. } => true,
            Instruction::SetFieldConst { .. } => true,
            Instruction::SetIndex { .. } => true,
            Instruction::SetIndexConst { .. } => true,
            Instruction::Upsilon(_, _) => true,
            Instruction::Call { .. } => true,
            Instruction::Method { .. } => true,
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
        if_false: BlockId,
        if_true: BlockId,
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

#[derive(Debug)]
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
pub type BlockMap = IdMap<BlockId, Block>;
pub type VariableSet = IdMap<Variable, ()>;
pub type ShadowVarSet = IdMap<ShadowVar, ()>;
pub type FunctionMap<S> = IdMap<FuncId, Function<S>>;
pub type UpValueMap = HashMap<Variable, Variable>;

pub struct Function<S> {
    pub num_parameters: usize,
    pub instructions: InstructionMap<S>,
    pub blocks: BlockMap,
    pub variables: VariableSet,
    pub shadow_vars: ShadowVarSet,

    /// Inner functions declared within this function.
    pub functions: FunctionMap<S>,

    /// If this is a sub-function, this maps *this* function's variables to the upper function's
    /// variables for any "upvalues" (variables shared from the parent function).
    ///
    /// For the top-level function, this will be empty.
    pub upvalues: UpValueMap,

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
                        writeln!(f, "constant({:?})", constant.as_ref())?;
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
                    Instruction::This => {
                        writeln!(f, "this()")?;
                    }
                    Instruction::NewObject => {
                        writeln!(f, "new_object()")?;
                    }
                    Instruction::NewArray => {
                        writeln!(f, "new_array()")?;
                    }
                    Instruction::Parameter(ind) => {
                        writeln!(f, "parameter(P{})", ind)?;
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
                            key.as_ref(),
                        )?;
                    }
                    Instruction::SetFieldConst { object, key, value } => {
                        writeln!(
                            f,
                            "set_field(object = I{}, key = {:?}, value = I{})",
                            object.index(),
                            key.as_ref(),
                            value.index()
                        )?;
                    }
                    Instruction::GetIndex { array, index } => {
                        writeln!(
                            f,
                            "get_index(array = I{}, index = I{})",
                            array.index(),
                            index.index(),
                        )?;
                    }
                    Instruction::SetIndex {
                        array,
                        index,
                        value,
                    } => {
                        writeln!(
                            f,
                            "set_index(array = I{}, index = I{}, value = I{})",
                            array.index(),
                            index.index(),
                            value.index(),
                        )?;
                    }
                    Instruction::GetIndexConst { array, index } => {
                        writeln!(
                            f,
                            "get_index(array = I{}, index = {:?})",
                            array.index(),
                            index.as_ref(),
                        )?;
                    }
                    Instruction::SetIndexConst {
                        array,
                        index,
                        value,
                    } => {
                        writeln!(
                            f,
                            "set_index(array = I{}, index = {:?}, value = I{})",
                            array.index(),
                            index.as_ref(),
                            value.index()
                        )?;
                    }
                    Instruction::Phi(shadow) => {
                        writeln!(f, "phi(S{})", shadow.index())?;
                    }
                    Instruction::Upsilon(shadow, source) => {
                        writeln!(f, "upsilon(S{}, I{})", shadow.index(), source.index())?;
                    }
                    Instruction::UnOp { source, op } => match op {
                        UnOp::Not => {
                            writeln!(f, "not(I{})", source.index())?;
                        }
                        UnOp::Neg => {
                            writeln!(f, "neg(I{})", source.index())?;
                        }
                    },
                    Instruction::BinOp { left, right, op } => match op {
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
                    },
                    Instruction::Call {
                        func,
                        args,
                        return_value,
                    } => {
                        write!(
                            f,
                            "call(I{}, return_value = {}, args = [",
                            func.index(),
                            return_value,
                        )?;
                        for (i, &arg) in args.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "I{}", arg.index())?;
                        }
                        writeln!(f, "])")?;
                    }
                    Instruction::Method {
                        this,
                        func,
                        args,
                        return_value,
                    } => {
                        write!(
                            f,
                            "call(I{}, this = I{}, return_value = {}, args = [",
                            func.index(),
                            this.index(),
                            return_value,
                        )?;
                        for (i, &arg) in args.iter().enumerate() {
                            if i != 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "I{}", arg.index())?;
                        }
                        writeln!(f, "])")?;
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

        write_indent(f, 0)?;
        writeln!(f, "num_parameters: {}", self.num_parameters)?;

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
            for var in self.variables.ids() {
                write!(f, " V{}", var.index())?;
            }
            writeln!(f)?;
        }

        if !self.upvalues.is_empty() {
            write_indent(f, 0)?;
            writeln!(f, "upvalues:")?;
            for (&var_id, &up_var_id) in self.upvalues.iter() {
                write_indent(f, 4)?;
                writeln!(f, "V{}: parent V{}", var_id.index(), up_var_id.index())?;
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
