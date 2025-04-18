use std::{
    collections::{hash_map, HashMap, HashSet},
    hash::Hash,
    mem,
};

use thiserror::Error;

use crate::compiler::{constant::Constant, ir, parser};

#[derive(Debug, Error)]
pub enum FrontendError {
    #[error("too many parameters")]
    ParameterOverflow,
}

pub fn compile_ir<S: Eq + Hash + Clone>(
    block: &parser::Block<S>,
) -> Result<ir::Function<S>, FrontendError> {
    let mut compiler = Compiler::new();
    compiler.block(block)?;
    Ok(compiler.current.function)
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

struct Compiler<S> {
    upper: Vec<Function<S>>,
    current: Function<S>,
}

impl<S: Eq + Hash + Clone> Compiler<S> {
    fn new() -> Self {
        let instructions = ir::InstructionMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableSet::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let functions = ir::FunctionMap::new();
        let upvalues = ir::UpValueMap::new();

        let start_block = blocks.insert(ir::Block::default());

        Self {
            upper: Vec::new(),
            current: Function {
                function: ir::Function {
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
                scopes: Default::default(),
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
            parser::Statement::Call(function_call) => self.function_call(&function_call, 0),
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
        enum VarOrThis {
            Var(ir::Variable),
            This(ir::InstId),
        }

        let (target, old) = if let Some(var) = self.get_var(&assignment_statement.name) {
            let old = self.push_instruction(ir::Instruction::GetVariable(var));
            (VarOrThis::Var(var), old)
        } else {
            let key = self.push_instruction(ir::Instruction::Constant(Constant::String(
                assignment_statement.name.clone(),
            )));
            let old = self.push_instruction(ir::Instruction::GetThis(key));
            (VarOrThis::This(key), old)
        };

        let val = self.commit_expression(&assignment_statement.value)?;
        let assign = match assignment_statement.op {
            parser::AssignmentOperator::Equal => val,
            parser::AssignmentOperator::PlusEqual => {
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Add,
                })
            }
            parser::AssignmentOperator::MinusEqual => {
                self.push_instruction(ir::Instruction::BinOp {
                    left: old,
                    right: val,
                    op: ir::BinOp::Sub,
                })
            }
            parser::AssignmentOperator::MultEqual => unimplemented!(),
            parser::AssignmentOperator::DivEqual => unimplemented!(),
        };

        match target {
            VarOrThis::Var(var) => {
                self.push_instruction(ir::Instruction::SetVariable(var, assign));
            }
            VarOrThis::This(key) => {
                self.push_instruction(ir::Instruction::SetThis { key, value: assign });
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
            self.push_instruction(ir::Instruction::Push(val));
            ir::Exit::Return { returns: 1 }
        } else {
            ir::Exit::Return { returns: 0 }
        };
        self.current.function.blocks[self.current.current_block].exit = exit;
        self.current.current_block = self.current.function.blocks.insert(ir::Block::default());
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &parser::IfStatement<S>) -> Result<(), FrontendError> {
        let cond = self.commit_expression(&if_statement.condition)?;
        let body = self.current.function.blocks.insert(ir::Block::default());
        let successor = self.current.function.blocks.insert(ir::Block::default());

        self.current.function.blocks[self.current.current_block].exit = ir::Exit::Branch {
            cond,
            if_true: body,
            if_false: successor,
        };
        self.current.current_block = body;

        {
            self.push_scope();
            self.block(&if_statement.body)?;
            self.pop_scope();
        }

        self.current.function.blocks[body].exit = ir::Exit::Jump(successor);
        self.current.current_block = successor;
        Ok(())
    }

    fn for_statement(
        &mut self,
        for_statement: &parser::ForStatement<S>,
    ) -> Result<(), FrontendError> {
        let body = self.current.function.blocks.insert(ir::Block::default());
        let successor = self.current.function.blocks.insert(ir::Block::default());

        {
            self.push_scope();

            self.statement(&for_statement.initializer)?;
            let cond = self.commit_expression(&for_statement.condition)?;

            // The condition expression is used again at the end, so we guard the body scope so it
            // doesn't affect the condition.
            {
                self.push_scope();

                self.current.function.blocks[self.current.current_block].exit = ir::Exit::Branch {
                    cond,
                    if_true: body,
                    if_false: successor,
                };
                self.current.current_block = body;

                self.block(&for_statement.body)?;

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
            self.current.function.blocks[body].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };

            self.pop_scope();
        }

        self.current.current_block = successor;

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
            parser::Expression::True => {
                self.push_instruction(ir::Instruction::Constant(Constant::Boolean(true)))
            }
            parser::Expression::False => {
                self.push_instruction(ir::Instruction::Constant(Constant::Boolean(false)))
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
                } else {
                    let key = self
                        .push_instruction(ir::Instruction::Constant(Constant::String(s.clone())));
                    self.push_instruction(ir::Instruction::GetThis(key))
                }
            }
            parser::Expression::Group(expr) => self.commit_expression(expr)?,
            parser::Expression::Unary(op, expr) => {
                let inst = ir::Instruction::UnOp {
                    source: self.commit_expression(expr)?,
                    op: match op {
                        parser::UnaryOperator::Not => ir::UnOp::Not,
                        parser::UnaryOperator::Minus => unimplemented!(),
                    },
                };
                self.push_instruction(inst)
            }
            parser::Expression::Binary(left, op, right) => {
                let left = self.commit_expression(left)?;
                let right = self.commit_expression(right)?;
                let inst = match op {
                    parser::BinaryOperator::Add => ir::Instruction::BinOp {
                        left,
                        right,
                        op: ir::BinOp::Add,
                    },
                    parser::BinaryOperator::Sub => ir::Instruction::BinOp {
                        left,
                        right,
                        op: ir::BinOp::Sub,
                    },
                    parser::BinaryOperator::Mult => unimplemented!(),
                    parser::BinaryOperator::Div => unimplemented!(),
                    parser::BinaryOperator::Equal => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::Equal,
                    },
                    parser::BinaryOperator::NotEqual => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::NotEqual,
                    },
                    parser::BinaryOperator::LessThan => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::LessThan,
                    },
                    parser::BinaryOperator::LessEqual => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::LessEqual,
                    },
                    parser::BinaryOperator::GreaterThan => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::GreaterThan,
                    },
                    parser::BinaryOperator::GreaterEqual => ir::Instruction::BinComp {
                        left,
                        right,
                        comp: ir::BinComp::GreaterEqual,
                    },
                    parser::BinaryOperator::And => unimplemented!(),
                    parser::BinaryOperator::Or => unimplemented!(),
                };
                self.push_instruction(inst)
            }
            parser::Expression::Function(func_expr) => {
                self.push_function(&func_expr.arguments);
                self.block(&func_expr.body)?;
                let func_id = self.pop_function();
                self.push_instruction(ir::Instruction::Closure(func_id))
            }
            parser::Expression::Call(func) => {
                self.function_call(func, 1)?;
                self.push_instruction(ir::Instruction::Pop)
            }
        })
    }

    fn function_call(
        &mut self,
        func: &parser::FunctionCall<S>,
        returns: u8,
    ) -> Result<(), FrontendError> {
        let args = func
            .arguments
            .len()
            .try_into()
            .map_err(|_| FrontendError::ParameterOverflow)?;
        let base = self.commit_expression(&func.base)?;

        for arg in &func.arguments {
            let arg = self.commit_expression(arg)?;
            self.push_instruction(ir::Instruction::Push(arg));
        }

        self.push_instruction(ir::Instruction::Call {
            source: base,
            args,
            returns,
        });

        Ok(())
    }

    fn push_function(&mut self, args: &[S]) {
        let instructions = ir::InstructionMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableSet::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let functions = ir::FunctionMap::new();
        let upvalues = ir::UpValueMap::new();
        let start_block = blocks.insert(ir::Block::default());

        let function = ir::Function {
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
                scopes: Default::default(),
                upvalues: Default::default(),
            },
        );
        self.upper.push(upper);

        self.push_scope();

        for arg_name in args.iter().rev() {
            let arg_var = self.declare_var(arg_name.clone());
            let pop_arg = self.push_instruction(ir::Instruction::Pop);
            self.push_instruction(ir::Instruction::SetVariable(arg_var, pop_arg));
        }
    }

    fn pop_function(&mut self) -> ir::FuncId {
        self.pop_scope();

        let upper = self.upper.pop().expect("no upper function to pop to");
        let lower = mem::replace(&mut self.current, upper);

        self.current.function.functions.insert(lower.function)
    }

    fn push_scope(&mut self) {
        self.current.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        if let Some(out_of_scope) = self.current.scopes.pop() {
            for vname in out_of_scope {
                let hash_map::Entry::Occupied(mut entry) = self.current.variables.entry(vname)
                else {
                    unreachable!();
                };
                entry.get_mut().pop().unwrap();
                if entry.get().is_empty() {
                    entry.remove();
                }
            }
        }
    }

    fn declare_var(&mut self, vname: S) -> ir::Variable {
        let in_scope = self.current.scopes.last().unwrap().contains(&vname);
        let variable_stack = self.current.variables.entry(vname).or_default();

        let var = self.current.function.variables.insert(());
        if in_scope {
            variable_stack.pop().unwrap();
        }
        variable_stack.push(var);
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
        } else {
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
