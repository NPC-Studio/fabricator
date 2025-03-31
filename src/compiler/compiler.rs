use std::{
    collections::{hash_map, HashMap, HashSet},
    hash::Hash,
};

use thiserror::Error;

use super::{constant::Constant, ir, parser};

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("no such variable declared")]
    NoSuchVar,
    #[error("too many parameters")]
    ParameterOverflow,
}

pub fn compile<S: Eq + Hash + Clone>(
    block: &parser::Block<S>,
) -> Result<ir::Function<S>, CompileError> {
    let mut compiler = Compiler::new();
    compiler.block(block)?;
    Ok(ir::Function {
        parts: compiler.parts,
        start_block: compiler.start_block,
    })
}

struct Compiler<S> {
    parts: ir::FunctionParts<S>,
    start_block: ir::BlockId,

    current_block: ir::BlockId,
    variables: HashMap<S, Vec<ir::VarId>>,
    scopes: Vec<HashSet<S>>,
}

impl<S: Eq + Hash + Clone> Compiler<S> {
    fn new() -> Self {
        let mut parts = ir::FunctionParts::default();
        let start_block = parts.blocks.insert(ir::Block::default());

        Self {
            parts,
            start_block,
            current_block: start_block,
            variables: Default::default(),
            scopes: Default::default(),
        }
    }

    fn block(&mut self, block: &parser::Block<S>) -> Result<(), CompileError> {
        {
            self.push_scope();

            for statement in &block.statements {
                self.statement(statement)?;
            }

            self.pop_scope();
        }
        Ok(())
    }

    fn statement(&mut self, statement: &parser::Statement<S>) -> Result<(), CompileError> {
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
    ) -> Result<(), CompileError> {
        let var_id = self.declare_var(var_statement.name.clone());
        let inst_id = self.commit_expression(&var_statement.value)?;
        self.push_instruction(ir::Instruction::SetVariable {
            source: inst_id,
            dest: var_id,
        });
        Ok(())
    }

    fn assignment_statement(
        &mut self,
        assignment_statement: &parser::AssignmentStatement<S>,
    ) -> Result<(), CompileError> {
        let var = self
            .get_var(&assignment_statement.name)
            .ok_or(CompileError::NoSuchVar)?;
        let old = self.push_instruction(ir::Instruction::GetVariable(var));
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
        self.push_instruction(ir::Instruction::SetVariable {
            source: assign,
            dest: var,
        });
        Ok(())
    }

    fn return_statement(
        &mut self,
        return_statement: &parser::ReturnStatement<S>,
    ) -> Result<(), CompileError> {
        let exit = if let Some(value) = &return_statement.value {
            let val = self.commit_expression(value)?;
            self.push_instruction(ir::Instruction::Push(val));
            ir::Exit::Return { returns: 1 }
        } else {
            ir::Exit::Return { returns: 0 }
        };
        self.parts.blocks[self.current_block].exit = exit;
        self.current_block = self.parts.blocks.insert(ir::Block::default());
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &parser::IfStatement<S>) -> Result<(), CompileError> {
        let cond = self.commit_expression(&if_statement.condition)?;
        let body = self.parts.blocks.insert(ir::Block::default());
        let successor = self.parts.blocks.insert(ir::Block::default());

        self.parts.blocks[self.current_block].exit = ir::Exit::Branch {
            cond,
            if_true: body,
            if_false: successor,
        };
        self.current_block = body;

        {
            self.push_scope();
            self.block(&if_statement.body)?;
            self.pop_scope();
        }

        self.parts.blocks[body].exit = ir::Exit::Jump(successor);
        self.current_block = successor;
        Ok(())
    }

    fn for_statement(
        &mut self,
        for_statement: &parser::ForStatement<S>,
    ) -> Result<(), CompileError> {
        let body = self.parts.blocks.insert(ir::Block::default());
        let successor = self.parts.blocks.insert(ir::Block::default());

        {
            self.push_scope();

            self.statement(&for_statement.initializer)?;
            let cond = self.commit_expression(&for_statement.condition)?;

            // The condition expression is used again at the end, so we guard the body scope so it
            // doesn't affect the condition.
            {
                self.push_scope();

                self.parts.blocks[self.current_block].exit = ir::Exit::Branch {
                    cond,
                    if_true: body,
                    if_false: successor,
                };
                self.current_block = body;

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
            self.parts.blocks[body].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };

            self.pop_scope();
        }

        self.current_block = successor;

        Ok(())
    }

    fn commit_expression(
        &mut self,
        expr: &parser::Expression<S>,
    ) -> Result<ir::InstId, CompileError> {
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
                let var = self.get_var(s).ok_or(CompileError::NoSuchVar)?;
                self.push_instruction(ir::Instruction::GetVariable(var))
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
    ) -> Result<(), CompileError> {
        let args = func
            .arguments
            .len()
            .try_into()
            .map_err(|_| CompileError::ParameterOverflow)?;
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

    fn push_instruction(&mut self, inst: ir::Instruction<S>) -> ir::InstId {
        let inst_id = self.parts.instructions.insert(inst);
        self.parts.blocks[self.current_block]
            .instructions
            .push(inst_id);
        inst_id
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        if let Some(out_of_scope) = self.scopes.pop() {
            for vname in out_of_scope {
                let hash_map::Entry::Occupied(mut entry) = self.variables.entry(vname) else {
                    unreachable!();
                };
                entry.get_mut().pop().unwrap();
                if entry.get().is_empty() {
                    entry.remove();
                }
            }
        }
    }

    fn declare_var(&mut self, vname: S) -> ir::VarId {
        let in_scope = self.scopes.last().unwrap().contains(&vname);
        let variable_stack = self.variables.entry(vname).or_default();

        let var = self.parts.variables.insert(());
        if in_scope {
            variable_stack.pop().unwrap();
        }
        variable_stack.push(var);
        var
    }

    fn get_var(&mut self, vname: &S) -> Option<ir::VarId> {
        self.variables.get(vname).and_then(|v| v.last().copied())
    }
}
