use std::{
    collections::{HashMap, HashSet, hash_map},
    hash::Hash,
    mem,
};

use thiserror::Error;

use crate::{
    constant::Constant,
    ir,
    magic_dict::{MagicDict, MagicMode},
    parser::{self, AssignmentTarget},
};

#[derive(Debug, Error)]
pub enum FrontendError {
    #[error("assignment to read-only magic value")]
    ReadOnlyMagic,
    #[error("too many parameters")]
    ParameterOverflow,
}

#[derive(Debug, Copy, Clone)]
pub struct FrontendSettings {
    /// Use proper lexical scoping for variable declarations.
    ///
    /// if `false`, then all variable declarations will be visible until the end of the enclosing
    /// function even when the enclosing scope ends.
    pub lexical_scoping: bool,

    /// Allow lambda expressions to reference variables from outer functions.
    ///
    /// Without this, such variables will instead be interpreted as implicit `self` variables.
    ///
    /// # Lexical scoping and closures
    ///
    /// Closing over a variable which is declared in the body of a loop will act differently
    /// depending on whether `lexical_scoping` is enabled or not. With `lexical_scoping`, each
    /// variable in a loop iteration is independent, without it, every variable in the body of a
    /// loop is always the same instance. The first behavior is similar to ECMAScript closures with
    /// the `let` keyword, the second behavior is similar to ECMAScript closures with the `var`
    /// keyword.
    ///
    /// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Closures#creating_closures_in_loops_a_common_mistake>
    pub allow_closures: bool,
}

impl Default for FrontendSettings {
    fn default() -> Self {
        Self {
            lexical_scoping: true,
            allow_closures: true,
        }
    }
}

impl FrontendSettings {
    pub fn compile_ir<S>(
        self,
        block: &parser::Block<S>,
        magic_dict: impl MagicDict<S>,
    ) -> Result<ir::Function<S>, FrontendError>
    where
        S: Eq + Hash + Clone + AsRef<str>,
    {
        let mut compiler = Compiler::new(self, magic_dict);
        compiler.block(block)?;
        Ok(compiler.current.function)
    }
}

struct Function<S> {
    function: ir::Function<S>,
    current_block: ir::BlockId,
    variables: HashMap<S, Vec<ir::Variable>>,
    scopes: Vec<HashSet<S>>,
    // Either a `Some` for an upvalue, or a `None` as a negative cache for no upper function having
    // such a variable.
    upvalues: HashMap<S, Option<ir::Variable>>,
}

struct Compiler<S, M> {
    settings: FrontendSettings,
    magic_dict: M,
    upper: Vec<Function<S>>,
    current: Function<S>,
}

impl<S, M> Compiler<S, M>
where
    S: Eq + Hash + Clone + AsRef<str>,
    M: MagicDict<S>,
{
    fn new(settings: FrontendSettings, magic_dict: M) -> Self {
        let instructions = ir::InstructionMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableSet::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let functions = ir::FunctionMap::new();
        let upvalues = ir::UpValueMap::new();

        let start_block = blocks.insert(ir::Block::default());

        Self {
            settings,
            magic_dict,
            upper: Vec::new(),
            current: Function {
                function: ir::Function {
                    num_parameters: 0,
                    instructions,
                    blocks,
                    variables,
                    shadow_vars,
                    functions,
                    upvalues,
                    start_block,
                },
                current_block: start_block,
                variables: Default::default(),
                scopes: if settings.lexical_scoping {
                    Vec::new()
                } else {
                    vec![HashSet::new()]
                },
                upvalues: HashMap::new(),
            },
        }
    }

    fn block(&mut self, block: &parser::Block<S>) -> Result<(), FrontendError> {
        {
            self.push_scope();

            for statement in &block.statements {
                self.statement(statement)?;
            }

            self.pop_scope();
        }
        Ok(())
    }

    fn statement(&mut self, statement: &parser::Statement<S>) -> Result<(), FrontendError> {
        match statement {
            parser::Statement::Var(var_statement) => self.var_statement(var_statement),
            parser::Statement::Assignment(assignment_statement) => {
                self.assignment_statement(assignment_statement)
            }
            parser::Statement::Return(return_) => self.return_statement(return_),
            parser::Statement::If(if_statement) => self.if_statement(if_statement),
            parser::Statement::For(for_statement) => self.for_statement(for_statement),
            parser::Statement::Block(block) => self.block(block),
            parser::Statement::Call(function_call) => {
                let _ = self.call_expr(function_call, false)?;
                Ok(())
            }
        }
    }

    fn var_statement(
        &mut self,
        var_statement: &parser::VarStatement<S>,
    ) -> Result<(), FrontendError> {
        let var = self.declare_var(var_statement.name.clone());
        let inst_id = self.commit_expression(&var_statement.value)?;
        self.push_instruction(ir::Instruction::SetVariable(var, inst_id));
        Ok(())
    }

    fn assignment_statement(
        &mut self,
        assignment_statement: &parser::AssignmentStatement<S>,
    ) -> Result<(), FrontendError> {
        enum Target<S> {
            Var(ir::Variable),
            This {
                key: ir::InstId,
            },
            Field {
                object: ir::InstId,
                key: ir::InstId,
            },
            Index {
                array: ir::InstId,
                index: ir::InstId,
            },
            Magic(S),
        }

        let this = self.push_instruction(ir::Instruction::This);

        let (target, old) = match &assignment_statement.target {
            AssignmentTarget::Name(name) => {
                if let Some(var) = self.get_var(name) {
                    (Target::Var(var), ir::Instruction::GetVariable(var))
                } else if let Some(mode) = self.magic_dict.magic_mode(name) {
                    if mode == MagicMode::ReadOnly {
                        return Err(FrontendError::ReadOnlyMagic);
                    }
                    (
                        Target::Magic(name.clone()),
                        ir::Instruction::GetMagic(name.clone()),
                    )
                } else {
                    let key = self.push_instruction(ir::Instruction::Constant(Constant::String(
                        name.clone(),
                    )));
                    (
                        Target::This { key },
                        ir::Instruction::GetField { object: this, key },
                    )
                }
            }
            AssignmentTarget::Field(field_expr) => {
                let object = self.commit_expression(&field_expr.base)?;
                let key = self.push_instruction(ir::Instruction::Constant(Constant::String(
                    field_expr.field.clone(),
                )));
                (
                    Target::Field { object, key },
                    ir::Instruction::GetField { object, key },
                )
            }
            AssignmentTarget::Index(index_expr) => {
                let array = self.commit_expression(&index_expr.base)?;
                let index = self.commit_expression(&index_expr.index)?;
                (
                    Target::Index { array, index },
                    ir::Instruction::GetIndex { array, index },
                )
            }
        };

        let val = self.commit_expression(&assignment_statement.value)?;
        let assign = match assignment_statement.op {
            parser::AssignmentOp::Equal => val,
            parser::AssignmentOp::PlusEqual => {
                let old = self.push_instruction(old);
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Add,
                })
            }
            parser::AssignmentOp::MinusEqual => {
                let old = self.push_instruction(old);
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Sub,
                })
            }
            parser::AssignmentOp::MultEqual => {
                let old = self.push_instruction(old);
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Mult,
                })
            }
            parser::AssignmentOp::DivEqual => {
                let old = self.push_instruction(old);
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Div,
                })
            }
        };

        match target {
            Target::Var(var) => {
                self.push_instruction(ir::Instruction::SetVariable(var, assign));
            }
            Target::This { key } => {
                self.push_instruction(ir::Instruction::SetField {
                    object: this,
                    key,
                    value: assign,
                });
            }
            Target::Field { object, key } => {
                self.push_instruction(ir::Instruction::SetField {
                    object,
                    key,
                    value: assign,
                });
            }
            Target::Index { array, index } => {
                self.push_instruction(ir::Instruction::SetIndex {
                    array,
                    index,
                    value: assign,
                });
            }
            Target::Magic(magic) => {
                self.push_instruction(ir::Instruction::SetMagic(magic, assign));
            }
        }

        Ok(())
    }

    fn return_statement(
        &mut self,
        return_statement: &parser::ReturnStatement<S>,
    ) -> Result<(), FrontendError> {
        let exit = if let Some(value) = &return_statement.value {
            let val = self.commit_expression(value)?;
            ir::Exit::Return { value: Some(val) }
        } else {
            ir::Exit::Return { value: None }
        };
        self.current.function.blocks[self.current.current_block].exit = exit;
        self.current.current_block = self.current.function.blocks.insert(ir::Block::default());
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &parser::IfStatement<S>) -> Result<(), FrontendError> {
        let cond = self.commit_expression(&if_statement.condition)?;
        let then_block = self.current.function.blocks.insert(ir::Block::default());
        let else_block = self.current.function.blocks.insert(ir::Block::default());
        let successor = self.current.function.blocks.insert(ir::Block::default());

        self.current.function.blocks[self.current.current_block].exit = ir::Exit::Branch {
            cond,
            if_true: then_block,
            if_false: else_block,
        };

        self.current.current_block = then_block;
        {
            self.push_scope();
            self.statement(&if_statement.then_stmt)?;
            self.pop_scope();
        }
        self.current.function.blocks[self.current.current_block].exit = ir::Exit::Jump(successor);

        self.current.current_block = else_block;
        if let Some(else_stmt) = &if_statement.else_stmt {
            {
                self.push_scope();
                self.statement(else_stmt)?;
                self.pop_scope();
            }
        }
        self.current.function.blocks[self.current.current_block].exit = ir::Exit::Jump(successor);

        self.current.current_block = successor;
        Ok(())
    }

    fn for_statement(
        &mut self,
        for_statement: &parser::ForStatement<S>,
    ) -> Result<(), FrontendError> {
        {
            self.push_scope();

            let body = self.current.function.blocks.insert(ir::Block::default());
            let successor = self.current.function.blocks.insert(ir::Block::default());

            self.statement(&for_statement.initializer)?;
            let cond = self.commit_expression(&for_statement.condition)?;

            self.current.function.blocks[self.current.current_block].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };
            self.current.current_block = body;

            // The condition expression is used again at the end, so we guard the body scope so it
            // doesn't affect the condition.
            {
                self.push_scope();
                self.statement(&for_statement.body)?;
                self.pop_scope();
            }

            // We surround the iterator statement so variable declarations have no effect outside
            // of it.
            {
                self.push_scope();
                self.statement(&for_statement.iterator)?;
                self.pop_scope();
            }

            let cond = self.commit_expression(&for_statement.condition)?;
            self.current.function.blocks[self.current.current_block].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };

            self.current.current_block = successor;

            self.pop_scope();
        }

        Ok(())
    }

    fn commit_expression(
        &mut self,
        expr: &parser::Expression<S>,
    ) -> Result<ir::InstId, FrontendError> {
        Ok(match expr {
            parser::Expression::Undefined => {
                self.push_instruction(ir::Instruction::Constant(Constant::Undefined))
            }
            parser::Expression::Boolean(b) => {
                self.push_instruction(ir::Instruction::Constant(Constant::Boolean(*b)))
            }
            parser::Expression::Float(f) => {
                self.push_instruction(ir::Instruction::Constant(Constant::Float(*f)))
            }
            parser::Expression::Integer(i) => {
                self.push_instruction(ir::Instruction::Constant(Constant::Integer(*i as i128)))
            }
            parser::Expression::String(s) => {
                self.push_instruction(ir::Instruction::Constant(Constant::String(s.clone())))
            }
            parser::Expression::Name(s) => {
                if let Some(var) = self.get_var(s) {
                    self.push_instruction(ir::Instruction::GetVariable(var))
                } else if self.magic_dict.magic_mode(s).is_some() {
                    self.push_instruction(ir::Instruction::GetMagic(s.clone()))
                } else {
                    let this = self.push_instruction(ir::Instruction::This);
                    let key = self
                        .push_instruction(ir::Instruction::Constant(Constant::String(s.clone())));
                    self.push_instruction(ir::Instruction::GetField { object: this, key })
                }
            }
            parser::Expression::This => self.push_instruction(ir::Instruction::This),
            parser::Expression::Group(expr) => self.commit_expression(expr)?,
            parser::Expression::Object(fields) => {
                let object = self.push_instruction(ir::Instruction::NewObject);
                for (field, value) in fields {
                    let value = self.commit_expression(value)?;
                    self.push_instruction(ir::Instruction::SetFieldConst {
                        object,
                        key: Constant::String(field.clone()),
                        value,
                    });
                }
                object
            }
            parser::Expression::Array(values) => {
                let array = self.push_instruction(ir::Instruction::NewArray);
                for (i, value) in values.iter().enumerate() {
                    let value = self.commit_expression(value)?;
                    self.push_instruction(ir::Instruction::SetIndexConst {
                        array,
                        index: Constant::Integer(i as i128),
                        value,
                    });
                }
                array
            }
            parser::Expression::Unary(op, expr) => {
                let inst = ir::Instruction::UnOp {
                    source: self.commit_expression(expr)?,
                    op: match op {
                        parser::UnaryOp::Not => ir::UnOp::Not,
                        parser::UnaryOp::Minus => ir::UnOp::Neg,
                    },
                };
                self.push_instruction(inst)
            }
            parser::Expression::Binary(left, op, right) => {
                let left = self.commit_expression(left)?;
                let right = self.commit_expression(right)?;
                let op = match op {
                    parser::BinaryOp::Add => ir::BinOp::Add,
                    parser::BinaryOp::Sub => ir::BinOp::Sub,
                    parser::BinaryOp::Mult => ir::BinOp::Mult,
                    parser::BinaryOp::Div => ir::BinOp::Div,
                    parser::BinaryOp::Equal => ir::BinOp::Equal,
                    parser::BinaryOp::NotEqual => ir::BinOp::NotEqual,
                    parser::BinaryOp::LessThan => ir::BinOp::LessThan,
                    parser::BinaryOp::LessEqual => ir::BinOp::LessEqual,
                    parser::BinaryOp::GreaterThan => ir::BinOp::GreaterThan,
                    parser::BinaryOp::GreaterEqual => ir::BinOp::GreaterEqual,
                    parser::BinaryOp::And => ir::BinOp::And,
                    parser::BinaryOp::Or => ir::BinOp::Or,
                };
                self.push_instruction(ir::Instruction::BinOp { left, right, op })
            }
            parser::Expression::Function(func_expr) => {
                self.push_function(&func_expr.parameters)?;
                self.block(&func_expr.body)?;
                let func_id = self.pop_function();
                self.push_instruction(ir::Instruction::Closure(func_id))
            }
            parser::Expression::Call(func) => self.call_expr(func, true)?,
            parser::Expression::Field(field_expr) => {
                let base = self.commit_expression(&field_expr.base)?;
                let field = self.push_instruction(ir::Instruction::Constant(Constant::String(
                    field_expr.field.clone(),
                )));
                self.push_instruction(ir::Instruction::GetField {
                    object: base,
                    key: field,
                })
            }
            parser::Expression::Index(index_expr) => {
                let base = self.commit_expression(&index_expr.base)?;
                let index = self.commit_expression(&index_expr.index)?;
                self.push_instruction(ir::Instruction::GetIndex { array: base, index })
            }
        })
    }

    fn call_expr(
        &mut self,
        func: &parser::CallExpr<S>,
        return_value: bool,
    ) -> Result<ir::InstId, FrontendError> {
        enum Call {
            Function(ir::InstId),
            Method {
                func: ir::InstId,
                object: ir::InstId,
            },
        }

        let call = match &*func.base {
            parser::Expression::Field(field_expr) => {
                let object = self.commit_expression(&field_expr.base)?;
                let key = self.push_instruction(ir::Instruction::Constant(Constant::String(
                    field_expr.field.clone(),
                )));
                let func = self.push_instruction(ir::Instruction::GetField { object, key });
                Call::Method { func, object }
            }
            expr => Call::Function(self.commit_expression(expr)?),
        };

        let mut args = Vec::new();
        for arg in &func.arguments {
            args.push(self.commit_expression(arg)?);
        }

        Ok(match call {
            Call::Function(func) => self.push_instruction(ir::Instruction::Call {
                func,
                args,
                return_value,
            }),
            Call::Method { func, object } => self.push_instruction(ir::Instruction::Method {
                func,
                this: object,
                args,
                return_value,
            }),
        })
    }

    fn push_function(&mut self, parameters: &[S]) -> Result<(), FrontendError> {
        let instructions = ir::InstructionMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableSet::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let functions = ir::FunctionMap::new();
        let upvalues = ir::UpValueMap::new();
        let start_block = blocks.insert(ir::Block::default());

        let function = ir::Function {
            num_parameters: parameters.len(),
            instructions,
            blocks,
            variables,
            shadow_vars,
            functions,
            upvalues,
            start_block,
        };

        let upper = mem::replace(
            &mut self.current,
            Function {
                function,
                current_block: start_block,
                variables: Default::default(),
                scopes: if self.settings.lexical_scoping {
                    Vec::new()
                } else {
                    vec![HashSet::new()]
                },
                upvalues: Default::default(),
            },
        );
        self.upper.push(upper);

        self.push_scope();

        for (param_index, param_name) in parameters.iter().enumerate() {
            let param_var = self.declare_var(param_name.clone());
            let pop_arg = self.push_instruction(ir::Instruction::Parameter(
                param_index
                    .try_into()
                    .map_err(|_| FrontendError::ParameterOverflow)?,
            ));
            self.push_instruction(ir::Instruction::SetVariable(param_var, pop_arg));
        }

        Ok(())
    }

    fn pop_function(&mut self) -> ir::FuncId {
        self.pop_scope();

        let upper = self.upper.pop().expect("no upper function to pop to");
        let lower = mem::replace(&mut self.current, upper);

        self.current.function.functions.insert(lower.function)
    }

    fn push_scope(&mut self) {
        if self.settings.lexical_scoping {
            self.current.scopes.push(HashSet::new());
        }
    }

    fn pop_scope(&mut self) {
        if self.settings.lexical_scoping {
            if let Some(out_of_scope) = self.current.scopes.pop() {
                for vname in out_of_scope {
                    let hash_map::Entry::Occupied(mut entry) = self.current.variables.entry(vname)
                    else {
                        unreachable!();
                    };
                    let var = entry.get_mut().pop().unwrap();
                    if entry.get().is_empty() {
                        entry.remove();
                    }
                    self.push_instruction(ir::Instruction::CloseVariable(var));
                }
            }
        }
    }

    fn declare_var(&mut self, vname: S) -> ir::Variable {
        let in_scope = !self
            .current
            .scopes
            .last_mut()
            .unwrap()
            .insert(vname.clone());
        let variable_stack = self.current.variables.entry(vname).or_default();

        let var = self.current.function.variables.insert(());
        if in_scope {
            variable_stack.pop().unwrap();
        }
        variable_stack.push(var);

        if self.settings.lexical_scoping {
            self.push_instruction(ir::Instruction::OpenVariable(var));
        } else {
            // If we're not using lexical scoping, just open every variable at the very start of the
            // function. This keeps the IR well-formed even with no lexical scoping and no explicit
            // `CloseVariable` instructions.
            let inst_id = self
                .current
                .function
                .instructions
                .insert(ir::Instruction::OpenVariable(var));
            self.current.function.blocks[self.current.function.start_block]
                .instructions
                .insert(0, inst_id);
        }

        var
    }

    fn get_var(&mut self, vname: &S) -> Option<ir::Variable> {
        if let Some(declared) = self
            .current
            .variables
            .get(vname)
            .and_then(|s| s.last().copied())
        {
            Some(declared)
        } else if let Some(&upvalue) = self.current.upvalues.get(vname) {
            upvalue
        } else if self.settings.allow_closures {
            // Search for any value with the requested name in any upper function. If we find one,
            // create a new variable for it in this function and record it as an upvalue.
            //
            // If we don't find any value with the requested name in any upper function, add a
            // `None` entry to the upvalues map to record that no such upvalue can be found.

            let mut upper_var = None;
            for (index, upper) in self.upper.iter().enumerate().rev() {
                if let Some(declared) = upper.variables.get(vname).and_then(|s| s.last().copied()) {
                    upper_var = Some((index, Some(declared)));
                    break;
                } else if let Some(&upvalue) = upper.upvalues.get(vname) {
                    upper_var = Some((index, upvalue));
                    break;
                }
            }

            if let Some((index, var)) = upper_var {
                if let Some(mut found) = var {
                    let add_upvalue =
                        |f: &mut Function<S>, name: S, upper_var: ir::Variable| -> ir::Variable {
                            let var = f.function.variables.insert(());
                            f.function.upvalues.insert(var, upper_var);
                            f.upvalues.insert(name, Some(var));
                            var
                        };

                    for upper in &mut self.upper[index + 1..] {
                        found = add_upvalue(upper, vname.clone(), found);
                    }
                    Some(add_upvalue(&mut self.current, vname.clone(), found))
                } else {
                    for upper in &mut self.upper[index + 1..] {
                        upper.upvalues.insert(vname.clone(), None);
                    }
                    self.current.upvalues.insert(vname.clone(), None);
                    None
                }
            } else {
                self.current.upvalues.insert(vname.clone(), None);
                None
            }
        } else {
            None
        }
    }

    fn push_instruction(&mut self, inst: ir::Instruction<S>) -> ir::InstId {
        let inst_id = self.current.function.instructions.insert(inst);
        self.current.function.blocks[self.current.current_block]
            .instructions
            .push(inst_id);
        inst_id
    }
}
