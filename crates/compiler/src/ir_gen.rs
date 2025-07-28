use std::{
    collections::{HashMap, HashSet, hash_map},
    hash::Hash,
};

use fabricator_vm::{BuiltIns, FunctionRef, RefName, Span};
use thiserror::Error;

use crate::{ast, constant::Constant, ir, string_interner::StringInterner};

/// Descriptor for magic values available to the IR generator.
///
/// Reading and writing to magic values compiles as separate kinds of IR instructions. If the
/// magic variable is `MagicMode::ReadOnly`, then assigning to such a variable is a compiler
/// error.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MagicMode {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug, Error)]
pub enum IrGenErrorKind {
    #[error("enum statements are only allowed at the top-level")]
    MisplacedEnum,
    #[error("function statements are only allowed at the top-level")]
    MisplacedFunctionStmt,
    #[error("assignment to read-only magic value")]
    ReadOnlyMagic,
    #[error("parameter default value is not a constant")]
    ParameterDefaultNotConstant,
    #[error("cannot reference a pseudo-variable as a normal variable")]
    PseudoVarAccessed,
    #[error("constructor functions are not permitted")]
    ConstructorsNotAllowed,
    #[error("static variables in constructors must be at the top-level of the function block")]
    ConstructorStaticNotTopLevel,
    #[error("static variable in constructor does not have a unique name")]
    ConstructorStaticNotUnique,
    #[error("static variables in constructors must be initialized")]
    ConstructorStaticNotInitialized,
    #[error("function not allowed to have a return statement with an argument")]
    CannotReturnValue,
    #[error("break statement with no target")]
    BreakWithNoTarget,
    #[error("continue statement with no target")]
    ContinueWithNoTarget,
    #[error("statement after unconditional jump")]
    StatementWithNoCurrentBlock,
}

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct IrGenError {
    #[source]
    pub kind: IrGenErrorKind,
    pub span: Span,
}

#[derive(Debug, Copy, Clone)]
pub struct IrGenSettings {
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

    /// Allow `constructor` annotated function statements with optional inheritance.
    ///
    /// These desugar to the equivalent manual implementation creating a new `this` object and a
    /// static variable holding the shared super object.
    ///
    /// All `static` variables must be declared at the top level of the function with unique names,
    /// and are interpreted as fields on a shared super object. Declaring a `constructor` function
    /// *disables* normal function statics, and all statics not at the top-level trigger errors.
    /// These variables fake and not allowed to be referenced as real variables (but may still be
    /// shadowed).
    ///
    /// This option is included for compatibility purposes only, it is more straightforward to
    /// handle constructors and inheritance manually, and doing so does not disable normal static
    /// variables.
    pub allow_constructors: bool,
}

impl IrGenSettings {
    pub fn modern() -> Self {
        IrGenSettings {
            lexical_scoping: true,
            allow_closures: true,
            allow_constructors: false,
        }
    }

    pub fn compat() -> Self {
        IrGenSettings {
            lexical_scoping: false,
            allow_closures: false,
            allow_constructors: true,
        }
    }

    pub fn gen_chunk_ir<S>(
        self,
        block: &ast::Block<S>,
        find_magic: impl Fn(&S) -> Option<MagicMode>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone,
    {
        let mut compiler = FunctionCompiler::new(self, FunctionRef::Chunk, &find_magic);
        compiler.block(block)?;
        Ok(compiler.finish())
    }

    pub fn gen_func_stmt_ir<S>(
        self,
        interner: impl StringInterner<String = S>,
        func_span: Span,
        func_stmt: &ast::FunctionStatement<S>,
        find_magic: impl Fn(&S) -> Option<MagicMode>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone + AsRef<str>,
    {
        let mut compiler = FunctionCompiler::new(
            self,
            FunctionRef::Named(RefName::new(func_stmt.name.as_ref()), func_span),
            &find_magic,
        );
        compiler.declare_parameters(&func_stmt.parameters)?;
        if func_stmt.is_constructor {
            if !self.allow_constructors {
                return Err(IrGenError {
                    kind: IrGenErrorKind::ConstructorsNotAllowed,
                    span: func_span,
                });
            }
            compiler.constructor(
                interner,
                func_stmt
                    .inherit
                    .as_ref()
                    .map(|(span, inherit)| (*span, inherit)),
                &func_stmt.body,
            )?;
        } else {
            compiler.block(&func_stmt.body)?;
        }
        Ok(compiler.finish())
    }
}

#[derive(Debug, Copy, Clone)]
enum VarDecl {
    // Variable declaration references a real IR variable.
    Real(ir::VarId),
    // Variable declaration looks like a variable declaration but is not a real variable, so
    // accessing it must trigger an error.
    Pseudo,
}

#[derive(Debug, Clone)]
enum MutableTarget<S> {
    Var(ir::VarId),
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

struct FunctionCompiler<'a, S> {
    settings: IrGenSettings,
    find_magic: &'a dyn (Fn(&S) -> Option<MagicMode>),

    function: ir::Function<S>,

    // If we have a final block to jump to, jump here instead of returning. Setting this disallows
    // return statements with values.
    final_block: Option<ir::BlockId>,

    // This will be `Some` if the current block is unfinished and can be appended to.
    current_block: Option<ir::BlockId>,

    break_target_stack: Vec<ir::BlockId>,
    continue_target_stack: Vec<ir::BlockId>,

    // Variable names declared in the current scope.
    scopes: Vec<HashSet<S>>,

    // Stack of declared variables for each variable name.
    //
    // Each variable declared in a scope will push an entry for the corresponding variable here.
    variables: HashMap<S, Vec<VarDecl>>,
}

impl<'a, S> FunctionCompiler<'a, S>
where
    S: Eq + Hash + Clone,
{
    fn new(
        settings: IrGenSettings,
        reference: FunctionRef,
        find_magic: &'a dyn Fn(&S) -> Option<MagicMode>,
    ) -> Self {
        let instructions = ir::InstructionMap::new();
        let spans = ir::SpanMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableMap::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let this_scopes = ir::ThisScopeSet::new();
        let functions = ir::FunctionMap::new();

        // We leave the start block completely empty except for a jump to the "real" start block.
        //
        // This is so that we can use the start block as a place to put non-lexically scoped
        // variable declaration instructions that need to be before any other generated IR, and
        // avoid being O(n^2) in the number of variable declarations.
        let start_block = blocks.insert(ir::Block::default());
        let first_block = blocks.insert(ir::Block::default());
        blocks[start_block].exit = ir::Exit::Jump(first_block);

        let function = ir::Function {
            is_constructor: false,
            reference,
            instructions,
            spans,
            blocks,
            variables,
            shadow_vars,
            this_scopes,
            functions,
            start_block,
        };

        Self {
            settings,
            find_magic,
            function,
            final_block: None,
            current_block: Some(first_block),
            continue_target_stack: Vec::new(),
            break_target_stack: Vec::new(),
            // Even with no lexical scoping, all functions must have one top-level scope.
            scopes: vec![HashSet::new()],
            variables: Default::default(),
        }
    }

    fn declare_parameters(&mut self, parameters: &[ast::Parameter<S>]) -> Result<(), IrGenError> {
        let arg_count = self.push_instruction(Span::null(), ir::Instruction::ArgumentCount);
        for (param_index, param) in parameters.into_iter().enumerate() {
            let arg_var = self
                .declare_var(param.name.clone(), Some(ir::Variable::Owned))
                .unwrap();

            if let Some(default) = &param.default {
                let def_value = default.clone().fold_constant().ok_or_else(|| IrGenError {
                    kind: IrGenErrorKind::ParameterDefaultNotConstant,
                    span: default.span,
                })?;

                let arg_index = self.push_instruction(
                    Span::null(),
                    ir::Instruction::Constant(Constant::Integer(param_index as i64)),
                );
                let arg_provided = self.push_instruction(
                    Span::null(),
                    ir::Instruction::BinOp {
                        left: arg_index,
                        op: ir::BinOp::LessThan,
                        right: arg_count,
                    },
                );

                let provided_block = self.new_block();
                let not_provided_block = self.new_block();
                let successor_block = self.new_block();

                self.end_current_block(ir::Exit::Branch {
                    cond: arg_provided,
                    if_true: provided_block,
                    if_false: not_provided_block,
                });

                self.start_new_block(provided_block);
                let pop_arg_inst =
                    self.push_instruction(Span::null(), ir::Instruction::Argument(param_index));
                self.push_instruction(
                    Span::null(),
                    ir::Instruction::SetVariable(arg_var, pop_arg_inst),
                );
                self.end_current_block(ir::Exit::Jump(successor_block));

                self.start_new_block(not_provided_block);
                let def_value_inst =
                    self.push_instruction(Span::null(), ir::Instruction::Constant(def_value));
                self.push_instruction(
                    Span::null(),
                    ir::Instruction::SetVariable(arg_var, def_value_inst),
                );
                self.end_current_block(ir::Exit::Jump(successor_block));

                self.start_new_block(successor_block);
            } else {
                let pop_arg =
                    self.push_instruction(Span::null(), ir::Instruction::Argument(param_index));
                self.push_instruction(Span::null(), ir::Instruction::SetVariable(arg_var, pop_arg));
            };
        }

        Ok(())
    }

    fn constructor(
        &mut self,
        mut interner: impl StringInterner<String = S>,
        inherit: Option<(Span, &ast::CallExpr<S>)>,
        main_block: &ast::Block<S>,
    ) -> Result<(), IrGenError> {
        self.function.is_constructor = true;

        // Create a hidden static variable to hold whether the constructor static object is
        // initialized.
        let is_initialized = self
            .function
            .variables
            .insert(ir::Variable::Static(Constant::Boolean(false)));

        let get_constructor_super = self.push_instruction(
            Span::null(),
            ir::Instruction::GetMagic(interner.intern(BuiltIns::GET_CONSTRUCTOR_SUPER)),
        );

        let set_super = self.push_instruction(
            Span::null(),
            ir::Instruction::GetMagic(interner.intern(BuiltIns::SET_SUPER)),
        );

        let init_block = self.new_block();
        let successor_block = self.new_block();
        let final_block = self.new_block();
        self.final_block = Some(final_block);

        let parent_func = if let Some((_, inherit)) = inherit {
            Some(self.expression(&inherit.base)?)
        } else {
            None
        };

        let this_closure = self.push_instruction(Span::null(), ir::Instruction::CurrentClosure);

        let our_super = self.push_instruction(
            Span::null(),
            ir::Instruction::Call {
                func: get_constructor_super,
                this: None,
                args: vec![this_closure],
                return_value: true,
            },
        );

        let check_initialized =
            self.push_instruction(Span::null(), ir::Instruction::GetVariable(is_initialized));
        self.end_current_block(ir::Exit::Branch {
            cond: check_initialized,
            if_true: successor_block,
            if_false: init_block,
        });

        self.start_new_block(init_block);

        if inherit.is_some() {
            let parent_super = self.push_instruction(
                Span::null(),
                ir::Instruction::Call {
                    func: get_constructor_super,
                    this: None,
                    args: vec![parent_func.unwrap()],
                    return_value: true,
                },
            );

            self.push_instruction(
                Span::null(),
                ir::Instruction::Call {
                    func: set_super,
                    this: None,
                    args: vec![our_super, parent_super],
                    return_value: false,
                },
            );
        }

        let mut static_names = HashSet::new();

        for stmt in &main_block.statements {
            if let ast::StatementKind::Static(decls) = &*stmt.kind {
                for decl in decls {
                    if !static_names.insert(&decl.name) {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ConstructorStaticNotUnique,
                            span: stmt.span,
                        });
                    }

                    let key = self.push_instruction(
                        stmt.span,
                        ir::Instruction::Constant(Constant::String(decl.name.clone())),
                    );
                    let value =
                        self.expression(decl.value.as_ref().ok_or_else(|| IrGenError {
                            kind: IrGenErrorKind::ConstructorStaticNotInitialized,
                            span: stmt.span,
                        })?)?;

                    self.push_instruction(
                        stmt.span,
                        ir::Instruction::SetField {
                            object: our_super,
                            key,
                            value,
                        },
                    );
                }
            }
        }

        let true_ = self.push_instruction(
            Span::null(),
            ir::Instruction::Constant(Constant::Boolean(true)),
        );
        self.push_instruction(
            Span::null(),
            ir::Instruction::SetVariable(is_initialized, true_),
        );

        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(successor_block);

        let this = if let Some(parent_func) = parent_func {
            let Some((inherit_span, inherit)) = inherit else {
                unreachable!();
            };

            let mut args = Vec::new();
            for arg in &inherit.arguments {
                args.push(self.expression(arg)?);
            }

            self.push_instruction(
                inherit_span,
                ir::Instruction::Call {
                    func: parent_func,
                    this: None,
                    args,
                    return_value: true,
                },
            )
        } else {
            self.push_instruction(Span::null(), ir::Instruction::NewObject)
        };

        self.push_instruction(
            Span::null(),
            ir::Instruction::Call {
                func: set_super,
                this: None,
                args: vec![this, our_super],
                return_value: false,
            },
        );

        let this_scope = self.function.this_scopes.insert(());
        self.push_instruction(
            Span::null(),
            ir::Instruction::OpenThisScope(this_scope, this),
        );

        self.push_scope();

        for stmt in &main_block.statements {
            match &*stmt.kind {
                ast::StatementKind::Static(decls) => {
                    for decl in decls {
                        // Declare a pseudo-variable when encountering the static statement.
                        // This variable's sole purpose is to prevent accessing a constructor
                        // `static`, which *looks* like a variable but is not usable as a variable
                        // in expressions.
                        self.declare_var(decl.name.clone(), None);
                    }
                }
                _ => {
                    self.statement(stmt)?;
                }
            }
        }

        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(final_block));
        }

        self.pop_scope();

        self.start_new_block(final_block);
        self.end_current_block(ir::Exit::Return { value: Some(this) });

        Ok(())
    }

    fn finish(mut self) -> ir::Function<S> {
        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Return { value: None });
        }

        assert!(self.break_target_stack.is_empty());
        assert!(self.continue_target_stack.is_empty());

        self.function
    }

    fn block(&mut self, block: &ast::Block<S>) -> Result<(), IrGenError> {
        self.push_scope();

        for statement in &block.statements {
            self.statement(statement)?;
        }

        self.pop_scope();
        Ok(())
    }

    fn statement(&mut self, statement: &ast::Statement<S>) -> Result<(), IrGenError> {
        if self.current_block.is_none() {
            return Err(IrGenError {
                kind: IrGenErrorKind::StatementWithNoCurrentBlock,
                span: statement.span,
            });
        }

        match &*statement.kind {
            ast::StatementKind::Block(block) => self.block(block),
            ast::StatementKind::Enum(_) => {
                return Err(IrGenError {
                    kind: IrGenErrorKind::MisplacedEnum,
                    span: statement.span,
                });
            }
            ast::StatementKind::Function(_) => {
                return Err(IrGenError {
                    kind: IrGenErrorKind::MisplacedFunctionStmt,
                    span: statement.span,
                });
            }
            ast::StatementKind::Var(var_decls) => {
                for var_decl in var_decls {
                    self.var_declaration(statement.span, var_decl)?;
                }
                Ok(())
            }
            ast::StatementKind::Static(static_decls) => {
                if self.function.is_constructor {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotTopLevel,
                        span: statement.span,
                    });
                }

                for static_decl in static_decls {
                    self.static_declaration(statement.span, static_decl)?;
                }
                Ok(())
            }
            ast::StatementKind::Assignment(assignment_statement) => {
                self.assignment_statement(statement.span, assignment_statement)
            }
            ast::StatementKind::Return(return_) => self.return_statement(statement.span, return_),
            ast::StatementKind::If(if_stmt) => self.if_statement(if_stmt),
            ast::StatementKind::For(for_stmt) => self.for_statement(for_stmt),
            ast::StatementKind::While(while_stmt) => self.while_statement(while_stmt),
            ast::StatementKind::Repeat(stmt) => self.repeat_statement(stmt),
            ast::StatementKind::Switch(switch_stmt) => self.switch_statement(switch_stmt),
            ast::StatementKind::Call(function_call) => {
                let _ = self.call_expr(statement.span, function_call, false)?;
                Ok(())
            }
            ast::StatementKind::Prefix(op, expr) => {
                self.mutation_op(statement.span, expr, *op)?;
                Ok(())
            }
            ast::StatementKind::Postfix(expr, op) => {
                self.mutation_op(statement.span, expr, *op)?;
                Ok(())
            }
            ast::StatementKind::Break => self.break_statement(statement.span),
            ast::StatementKind::Continue => self.continue_statement(statement.span),
        }
    }

    fn var_declaration(
        &mut self,
        span: Span,
        var_decl: &ast::Declaration<S>,
    ) -> Result<(), IrGenError> {
        let var_id = self
            .declare_var(var_decl.name.clone(), Some(ir::Variable::Owned))
            .unwrap();
        if let Some(value) = &var_decl.value {
            let inst_id = self.expression(value)?;
            self.push_instruction(span, ir::Instruction::SetVariable(var_id, inst_id));
        }
        Ok(())
    }

    fn static_declaration(
        &mut self,
        span: Span,
        static_decl: &ast::Declaration<S>,
    ) -> Result<(), IrGenError> {
        if let Some(value) = &static_decl.value {
            if let Some(constant) = value.clone().fold_constant() {
                // If our static is a constant, then we can just initialize it when the prototype is
                // created.
                self.declare_var(
                    static_decl.name.clone(),
                    Some(ir::Variable::Static(constant)),
                );
            } else {
                // Otherwise, we need to initialize two static variables, a hidden one for the
                // initialization state and a visible one to hold the initialized value.

                // Create a hidden static variable to hold whether the real static is initialized.
                let is_initialized = self
                    .function
                    .variables
                    .insert(ir::Variable::Static(Constant::Boolean(false)));
                // Create a normal static variable that holds the real value.
                let var_id = self
                    .declare_var(
                        static_decl.name.clone(),
                        Some(ir::Variable::Static(Constant::Undefined)),
                    )
                    .unwrap();

                let init_block = self.new_block();
                let successor = self.new_block();

                let check_initialized =
                    self.push_instruction(span, ir::Instruction::GetVariable(is_initialized));
                self.end_current_block(ir::Exit::Branch {
                    cond: check_initialized,
                    if_true: successor,
                    if_false: init_block,
                });

                self.start_new_block(init_block);

                let value = self.expression(value)?;
                self.push_instruction(span, ir::Instruction::SetVariable(var_id, value));
                let true_ =
                    self.push_instruction(span, ir::Instruction::Constant(Constant::Boolean(true)));
                self.push_instruction(span, ir::Instruction::SetVariable(is_initialized, true_));

                self.end_current_block(ir::Exit::Jump(successor));

                self.start_new_block(successor);
            }
        } else {
            // If our static has no value then it is just initialized as `Undefined`.
            self.declare_var(
                static_decl.name.clone(),
                Some(ir::Variable::Static(Constant::Undefined)),
            );
        }

        Ok(())
    }

    fn assignment_statement(
        &mut self,
        span: Span,
        assignment_statement: &ast::AssignmentStatement<S>,
    ) -> Result<(), IrGenError> {
        let target = self.mutable_target(span, &assignment_statement.target)?;
        let val = self.expression(&assignment_statement.value)?;

        let assign = match assignment_statement.op {
            ast::AssignmentOp::Equal => val,
            ast::AssignmentOp::PlusEqual => {
                let prev = self.read_mutable_target(span, target.clone());
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Add,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::MinusEqual => {
                let prev = self.read_mutable_target(span, target.clone());
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Sub,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::MultEqual => {
                let prev = self.read_mutable_target(span, target.clone());
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Mult,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::DivEqual => {
                let prev = self.read_mutable_target(span, target.clone());
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Div,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::NullCoalesce => {
                let prev = self.read_mutable_target(span, target.clone());
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::NullCoalesce,
                        right: val,
                    },
                )
            }
        };

        self.write_mutable_target(span, target, assign);

        Ok(())
    }

    fn return_statement(
        &mut self,
        span: Span,
        return_statement: &ast::ReturnStatement<S>,
    ) -> Result<(), IrGenError> {
        if self.final_block.is_some() && return_statement.value.is_some() {
            return Err(IrGenError {
                kind: IrGenErrorKind::CannotReturnValue,
                span,
            });
        }

        let exit = if let Some(value) = &return_statement.value {
            let val = self.expression(value)?;
            ir::Exit::Return { value: Some(val) }
        } else {
            ir::Exit::Return { value: None }
        };
        self.end_current_block(exit);
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &ast::IfStatement<S>) -> Result<(), IrGenError> {
        let cond = self.expression(&if_statement.condition)?;
        let then_block = self.new_block();
        let else_block = self.new_block();
        let successor = self.new_block();

        self.end_current_block(ir::Exit::Branch {
            cond,
            if_true: then_block,
            if_false: else_block,
        });

        self.start_new_block(then_block);
        self.push_scope();
        self.statement(&if_statement.then_stmt)?;
        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(successor));
        }
        self.pop_scope();

        self.start_new_block(else_block);
        if let Some(else_stmt) = &if_statement.else_stmt {
            self.push_scope();
            self.statement(else_stmt)?;
            self.pop_scope();
        }
        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(successor));
        }

        self.start_new_block(successor);
        Ok(())
    }

    fn for_statement(&mut self, for_statement: &ast::ForStatement<S>) -> Result<(), IrGenError> {
        self.push_scope();
        self.statement(&for_statement.initializer)?;

        if self.current_block.is_some() {
            let cond_block = self.new_block();
            let body_block = self.new_block();
            let iter_block = self.new_block();
            let successor_block = self.new_block();

            self.push_break_target(successor_block);
            self.push_continue_target(iter_block);

            self.end_current_block(ir::Exit::Jump(cond_block));
            self.start_new_block(cond_block);

            let cond = self.expression(&for_statement.condition)?;
            self.end_current_block(ir::Exit::Branch {
                cond,
                if_true: body_block,
                if_false: successor_block,
            });

            self.start_new_block(body_block);
            self.push_scope();
            self.statement(&for_statement.body)?;
            self.pop_scope();

            if self.current_block.is_some() {
                self.end_current_block(ir::Exit::Jump(iter_block));
                self.start_new_block(iter_block);

                self.push_scope();
                self.statement(&for_statement.iterator)?;
                self.pop_scope();

                if self.current_block.is_some() {
                    self.end_current_block(ir::Exit::Jump(cond_block));
                }
            }

            self.start_new_block(successor_block);

            self.pop_continue_target(iter_block);
            self.pop_break_target(successor_block);
        }

        self.pop_scope();
        Ok(())
    }

    fn while_statement(&mut self, while_stmt: &ast::WhileStatement<S>) -> Result<(), IrGenError> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_break_target(successor_block);
        self.push_continue_target(cond_block);

        self.end_current_block(ir::Exit::Jump(cond_block));
        self.start_new_block(cond_block);

        let cond = self.expression(&while_stmt.condition)?;
        self.end_current_block(ir::Exit::Branch {
            cond,
            if_true: body_block,
            if_false: successor_block,
        });

        self.start_new_block(body_block);
        self.push_scope();
        self.statement(&while_stmt.body)?;
        self.pop_scope();

        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(cond_block));
        }

        self.start_new_block(successor_block);

        self.pop_continue_target(cond_block);
        self.pop_break_target(successor_block);

        Ok(())
    }

    fn repeat_statement(&mut self, stmt: &ast::Statement<S>) -> Result<(), IrGenError> {
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_break_target(successor_block);
        self.push_continue_target(body_block);

        self.end_current_block(ir::Exit::Jump(body_block));
        self.start_new_block(body_block);
        self.push_scope();
        self.statement(stmt)?;
        self.pop_scope();

        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(body_block));
        }

        self.start_new_block(successor_block);

        self.pop_continue_target(body_block);
        self.pop_break_target(successor_block);

        Ok(())
    }

    fn switch_statement(
        &mut self,
        switch_stmt: &ast::SwitchStatement<S>,
    ) -> Result<(), IrGenError> {
        let target = self.expression(&switch_stmt.target)?;

        let successor_block = self.new_block();
        self.push_break_target(successor_block);

        for (expr, body) in &switch_stmt.cases {
            let expr = self.expression(expr)?;
            let is_equal = self.push_instruction(
                Span::null(),
                ir::Instruction::BinOp {
                    left: expr,
                    op: ir::BinOp::Equal,
                    right: target,
                },
            );

            let next_block = self.new_block();

            let body_block = self.new_block();
            self.end_current_block(ir::Exit::Branch {
                cond: is_equal,
                if_true: body_block,
                if_false: next_block,
            });

            self.start_new_block(body_block);
            self.block(body)?;
            if self.current_block.is_some() {
                self.end_current_block(ir::Exit::Jump(successor_block));
            }

            self.start_new_block(next_block);
        }

        if let Some(default) = &switch_stmt.default {
            self.block(default)?;
            if self.current_block.is_some() {
                self.end_current_block(ir::Exit::Jump(successor_block));
            }
        }

        self.start_new_block(successor_block);
        self.pop_break_target(successor_block);

        Ok(())
    }

    fn break_statement(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&break_target) = self.break_target_stack.last() {
            self.end_current_block(ir::Exit::Jump(break_target));
            Ok(())
        } else {
            Err(IrGenError {
                kind: IrGenErrorKind::BreakWithNoTarget,
                span,
            })
        }
    }

    fn continue_statement(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&continue_target) = self.continue_target_stack.last() {
            self.end_current_block(ir::Exit::Jump(continue_target));
            Ok(())
        } else {
            Err(IrGenError {
                kind: IrGenErrorKind::ContinueWithNoTarget,
                span,
            })
        }
    }

    fn expression(&mut self, expr: &ast::Expression<S>) -> Result<ir::InstId, IrGenError> {
        let span = expr.span;
        Ok(match &*expr.kind {
            ast::ExpressionKind::Constant(c) => {
                self.push_instruction(span, ir::Instruction::Constant(c.clone()))
            }
            ast::ExpressionKind::Name(s) => {
                if let Some(var_id) = self.get_var(span, s)? {
                    self.push_instruction(span, ir::Instruction::GetVariable(var_id))
                } else if (self.find_magic)(s).is_some() {
                    self.push_instruction(span, ir::Instruction::GetMagic(s.clone()))
                } else {
                    let this = self.push_instruction(span, ir::Instruction::This);
                    let key = self.push_instruction(
                        span,
                        ir::Instruction::Constant(Constant::String(s.clone())),
                    );
                    self.push_instruction(span, ir::Instruction::GetField { object: this, key })
                }
            }
            ast::ExpressionKind::Global => self.push_instruction(span, ir::Instruction::Globals),
            ast::ExpressionKind::This => self.push_instruction(span, ir::Instruction::This),
            ast::ExpressionKind::Other => self.push_instruction(span, ir::Instruction::Other),
            ast::ExpressionKind::Group(expr) => self.expression(expr)?,
            ast::ExpressionKind::Object(fields) => {
                let object = self.push_instruction(span, ir::Instruction::NewObject);

                for (field, value) in fields {
                    let this_scope =
                        if matches!(value.kind.as_ref(), ast::ExpressionKind::Function(_)) {
                            // Within a struct literal, closures always bind `self` to the struct currently
                            // being created.
                            let this_scope = self.function.this_scopes.insert(());
                            self.push_instruction(
                                span,
                                ir::Instruction::OpenThisScope(this_scope, object),
                            );
                            Some(this_scope)
                        } else {
                            None
                        };

                    let value = self.expression(value)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::SetFieldConst {
                            object,
                            key: Constant::String(field.clone()),
                            value,
                        },
                    );

                    if let Some(this_scope) = this_scope {
                        self.push_instruction(span, ir::Instruction::CloseThisScope(this_scope));
                    }
                }

                object
            }
            ast::ExpressionKind::Array(values) => {
                let array = self.push_instruction(span, ir::Instruction::NewArray);
                for (i, value) in values.iter().enumerate() {
                    let value = self.expression(value)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::SetIndexConst {
                            array,
                            index: Constant::Integer(i as i64),
                            value,
                        },
                    );
                }
                array
            }
            ast::ExpressionKind::Unary(op, expr) => {
                let inst = ir::Instruction::UnOp {
                    op: match op {
                        ast::UnaryOp::Not => ir::UnOp::Not,
                        ast::UnaryOp::Minus => ir::UnOp::Neg,
                    },
                    source: self.expression(expr)?,
                };
                self.push_instruction(span, inst)
            }
            ast::ExpressionKind::Prefix(op, target) => self.mutation_op(expr.span, target, *op)?.1,
            ast::ExpressionKind::Postfix(target, op) => self.mutation_op(expr.span, target, *op)?.0,
            ast::ExpressionKind::Binary(left, op, right) => match op {
                ast::BinaryOp::Add => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Add,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Sub => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Sub,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Mult => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Mult,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Div => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Div,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Rem => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Rem,
                            right,
                        },
                    )
                }
                ast::BinaryOp::IDiv => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::IDiv,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Equal => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Equal,
                            right,
                        },
                    )
                }
                ast::BinaryOp::NotEqual => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::NotEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::LessThan => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::LessThan,
                            right,
                        },
                    )
                }
                ast::BinaryOp::LessEqual => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::LessEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::GreaterThan => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::GreaterThan,
                            right,
                        },
                    )
                }
                ast::BinaryOp::GreaterEqual => {
                    let left = self.expression(left)?;
                    let right = self.expression(right)?;
                    self.push_instruction(
                        span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::GreaterEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::And => self.short_circuit_and(span, left, right)?,
                ast::BinaryOp::Or => self.short_circuit_or(span, left, right)?,
                ast::BinaryOp::NullCoalesce => {
                    self.short_circuit_null_coalesce(span, left, right)?
                }
            },
            ast::ExpressionKind::Ternary(ternary) => {
                let cond = self.expression(&ternary.cond)?;
                self.if_expression(
                    span,
                    cond,
                    |this| this.expression(&ternary.if_true),
                    |this| this.expression(&ternary.if_false),
                )?
            }
            ast::ExpressionKind::Function(func_expr) => {
                let func_id = self.inner_function(
                    FunctionRef::Expression(span),
                    &func_expr.parameters,
                    &func_expr.body,
                )?;
                self.push_instruction(span, ir::Instruction::Closure(func_id))
            }
            ast::ExpressionKind::Call(func) => self.call_expr(span, func, true)?,
            ast::ExpressionKind::Field(field_expr) => {
                let base = self.expression(&field_expr.base)?;
                let field = self.push_instruction(
                    span,
                    ir::Instruction::Constant(Constant::String(field_expr.field.clone())),
                );
                self.push_instruction(
                    span,
                    ir::Instruction::GetField {
                        object: base,
                        key: field,
                    },
                )
            }
            ast::ExpressionKind::Index(index_expr) => {
                let base = self.expression(&index_expr.base)?;
                let index = self.expression(&index_expr.index)?;
                self.push_instruction(span, ir::Instruction::GetIndex { array: base, index })
            }
        })
    }

    fn call_expr(
        &mut self,
        span: Span,
        func: &ast::CallExpr<S>,
        return_value: bool,
    ) -> Result<ir::InstId, IrGenError> {
        enum Call {
            Function(ir::InstId),
            Method {
                func: ir::InstId,
                object: ir::InstId,
            },
        }

        // Function calls on fields are interpreted as "methods", and implicitly bind the containing
        // object as `self` for the function call.
        let call = if let ast::ExpressionKind::Field(field_expr) = &*func.base.kind {
            let object = self.expression(&field_expr.base)?;
            let key = self.push_instruction(
                func.base.span,
                ir::Instruction::Constant(Constant::String(field_expr.field.clone())),
            );
            let func =
                self.push_instruction(func.base.span, ir::Instruction::GetField { object, key });
            Call::Method { func, object }
        } else {
            Call::Function(self.expression(&func.base)?)
        };

        let mut args = Vec::new();
        for arg in &func.arguments {
            args.push(self.expression(arg)?);
        }

        Ok(match call {
            Call::Function(func) => self.push_instruction(
                span,
                ir::Instruction::Call {
                    func,
                    this: None,
                    args,
                    return_value,
                },
            ),
            Call::Method { func, object } => self.push_instruction(
                span,
                ir::Instruction::Call {
                    func,
                    this: Some(object),
                    args,
                    return_value,
                },
            ),
        })
    }

    fn if_expression(
        &mut self,
        span: Span,
        cond: ir::InstId,
        if_true: impl FnOnce(&mut Self) -> Result<ir::InstId, IrGenError>,
        if_false: impl FnOnce(&mut Self) -> Result<ir::InstId, IrGenError>,
    ) -> Result<ir::InstId, IrGenError> {
        let res_var = self.function.variables.insert(ir::Variable::Owned);
        self.push_instruction(span, ir::Instruction::OpenVariable(res_var));

        let if_true_block = self.new_block();
        let if_false_block = self.new_block();
        let successor = self.new_block();

        self.end_current_block(ir::Exit::Branch {
            cond,
            if_true: if_true_block,
            if_false: if_false_block,
        });

        self.start_new_block(if_true_block);
        let if_true_res = if_true(self)?;
        self.push_instruction(span, ir::Instruction::SetVariable(res_var, if_true_res));
        self.end_current_block(ir::Exit::Jump(successor));

        self.start_new_block(if_false_block);
        let if_false_res = if_false(self)?;
        self.push_instruction(span, ir::Instruction::SetVariable(res_var, if_false_res));
        self.end_current_block(ir::Exit::Jump(successor));

        self.start_new_block(successor);
        let res = self.push_instruction(span, ir::Instruction::GetVariable(res_var));
        self.push_instruction(span, ir::Instruction::CloseVariable(res_var));

        Ok(res)
    }

    fn short_circuit_and(
        &mut self,
        span: Span,
        left: &ast::Expression<S>,
        right: &ast::Expression<S>,
    ) -> Result<ir::InstId, IrGenError> {
        let left = self.expression(left)?;
        self.if_expression(
            span,
            left,
            |this| {
                let right = this.expression(right)?;
                Ok(this.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left,
                        op: ir::BinOp::And,
                        right,
                    },
                ))
            },
            |this| {
                Ok(
                    this.push_instruction(
                        span,
                        ir::Instruction::Constant(Constant::Boolean(false)),
                    ),
                )
            },
        )
    }

    fn short_circuit_or(
        &mut self,
        span: Span,
        left: &ast::Expression<S>,
        right: &ast::Expression<S>,
    ) -> Result<ir::InstId, IrGenError> {
        let left = self.expression(left)?;
        self.if_expression(
            span,
            left,
            |this| {
                Ok(this.push_instruction(span, ir::Instruction::Constant(Constant::Boolean(true))))
            },
            |this| {
                let right = this.expression(right)?;
                Ok(this.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left,
                        op: ir::BinOp::Or,
                        right,
                    },
                ))
            },
        )
    }

    fn short_circuit_null_coalesce(
        &mut self,
        span: Span,
        left: &ast::Expression<S>,
        right: &ast::Expression<S>,
    ) -> Result<ir::InstId, IrGenError> {
        let left = self.expression(left)?;
        let undefined = self.push_instruction(span, ir::Instruction::Undefined);
        let cond = self.push_instruction(
            span,
            ir::Instruction::BinOp {
                left,
                op: ir::BinOp::Equal,
                right: undefined,
            },
        );
        self.if_expression(span, cond, |this| this.expression(right), |_| Ok(left))
    }

    /// Evaluate a `MutationOp` on a `MutableExpr`.
    ///
    /// Returns a tuple of the old and new values for the `MutableExpr`.
    fn mutation_op(
        &mut self,
        span: Span,
        target: &ast::MutableExpr<S>,
        op: ast::MutationOp,
    ) -> Result<(ir::InstId, ir::InstId), IrGenError> {
        let target = self.mutable_target(span, target)?;
        let old = self.read_mutable_target(span, target.clone());
        let one = self.push_instruction(span, ir::Instruction::Constant(Constant::Integer(1)));
        let op = match op {
            ast::MutationOp::Increment => ir::BinOp::Add,
            ast::MutationOp::Decrement => ir::BinOp::Sub,
        };
        let new = self.push_instruction(
            span,
            ir::Instruction::BinOp {
                left: old,
                op,
                right: one,
            },
        );
        self.write_mutable_target(span, target, new);
        Ok((old, new))
    }

    fn mutable_target(
        &mut self,
        span: Span,
        target: &ast::MutableExpr<S>,
    ) -> Result<MutableTarget<S>, IrGenError> {
        Ok(match target {
            ast::MutableExpr::Name(name) => {
                if let Some(var_id) = self.get_var(span, name)? {
                    MutableTarget::Var(var_id)
                } else if let Some(mode) = (self.find_magic)(name) {
                    if mode == MagicMode::ReadOnly {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ReadOnlyMagic,
                            span,
                        });
                    }

                    MutableTarget::Magic(name.clone())
                } else {
                    let key = self.push_instruction(
                        span,
                        ir::Instruction::Constant(Constant::String(name.clone())),
                    );
                    MutableTarget::This { key }
                }
            }
            ast::MutableExpr::Field(field_expr) => {
                let object = self.expression(&field_expr.base)?;
                let key = self.push_instruction(
                    span,
                    ir::Instruction::Constant(Constant::String(field_expr.field.clone())),
                );
                MutableTarget::Field { object, key }
            }
            ast::MutableExpr::Index(index_expr) => {
                let array = self.expression(&index_expr.base)?;
                let index = self.expression(&index_expr.index)?;
                MutableTarget::Index { array, index }
            }
        })
    }

    fn read_mutable_target(&mut self, span: Span, target: MutableTarget<S>) -> ir::InstId {
        let inst = match target {
            MutableTarget::Var(var_id) => ir::Instruction::GetVariable(var_id),
            MutableTarget::This { key } => {
                let this = self.push_instruction(span, ir::Instruction::This);
                ir::Instruction::GetField { object: this, key }
            }
            MutableTarget::Field { object, key } => ir::Instruction::GetField { object, key },
            MutableTarget::Index { array, index } => ir::Instruction::GetIndex { array, index },
            MutableTarget::Magic(name) => ir::Instruction::GetMagic(name),
        };
        self.push_instruction(span, inst)
    }

    fn write_mutable_target(&mut self, span: Span, target: MutableTarget<S>, value: ir::InstId) {
        match target {
            MutableTarget::Var(var_id) => {
                self.push_instruction(span, ir::Instruction::SetVariable(var_id, value));
            }
            MutableTarget::This { key } => {
                let this = self.push_instruction(span, ir::Instruction::This);
                self.push_instruction(
                    span,
                    ir::Instruction::SetField {
                        object: this,
                        key,
                        value,
                    },
                );
            }
            MutableTarget::Field { object, key } => {
                self.push_instruction(span, ir::Instruction::SetField { object, key, value });
            }
            MutableTarget::Index { array, index } => {
                self.push_instruction(
                    span,
                    ir::Instruction::SetIndex {
                        array,
                        index,
                        value,
                    },
                );
            }
            MutableTarget::Magic(magic) => {
                self.push_instruction(span, ir::Instruction::SetMagic(magic, value));
            }
        }
    }

    fn inner_function(
        &mut self,
        reference: FunctionRef,
        parameters: &[ast::Parameter<S>],
        body: &ast::Block<S>,
    ) -> Result<ir::FuncId, IrGenError> {
        let mut compiler = FunctionCompiler::new(self.settings, reference, self.find_magic);

        // If we allow closures, then pass every currently in-scope variable as an upvar.
        if self.settings.allow_closures {
            for (n, l) in &self.variables {
                let &v = l.last().unwrap();
                compiler.declare_var(
                    n.clone(),
                    match v {
                        VarDecl::Real(var_id) => Some(ir::Variable::Upper(var_id)),
                        VarDecl::Pseudo => None,
                    },
                );
            }
        }

        compiler.declare_parameters(parameters)?;
        compiler.block(body)?;

        Ok(self.function.functions.insert(compiler.function))
    }

    fn push_scope(&mut self) {
        if self.settings.lexical_scoping {
            self.scopes.push(HashSet::new());
        }
    }

    fn pop_scope(&mut self) {
        if self.settings.lexical_scoping {
            if let Some(out_of_scope) = self.scopes.pop() {
                for vname in out_of_scope {
                    let hash_map::Entry::Occupied(mut entry) = self.variables.entry(vname) else {
                        unreachable!();
                    };
                    let var_id = entry.get_mut().pop().unwrap();
                    if entry.get().is_empty() {
                        entry.remove();
                    }
                    if let VarDecl::Real(var_id) = var_id {
                        if self.function.variables[var_id].is_owned()
                            && self.current_block.is_some()
                        {
                            self.push_instruction(
                                Span::null(),
                                ir::Instruction::CloseVariable(var_id),
                            );
                        }
                    }
                }
            }
        }
    }

    // Declare a variable in the current scope. If the IR variable is `None`, declares a
    // pseudo-variable.
    fn declare_var(&mut self, vname: S, var: Option<ir::Variable<S>>) -> Option<ir::VarId> {
        let in_scope = !self.scopes.last_mut().unwrap().insert(vname.clone());
        let variable_stack = self.variables.entry(vname).or_default();

        if in_scope {
            // This variable was already in-scope, remove it so we can replace it.
            variable_stack.pop().unwrap();
        }

        let Some(var) = var else {
            variable_stack.push(VarDecl::Pseudo);
            return None;
        };

        let is_owned = var.is_owned();

        let var_id = self.function.variables.insert(var);
        variable_stack.push(VarDecl::Real(var_id));

        if is_owned {
            if self.settings.lexical_scoping {
                self.push_instruction(Span::null(), ir::Instruction::OpenVariable(var_id));
            } else {
                // If we're not using lexical scoping, just open every variable at the very start of
                // the function. This keeps the IR well-formed even with no lexical scoping and no
                // explicit `CloseVariable` instructions.
                //
                // We push this instruction to `start_block`, which is kept otherwise empty for
                // this purpose.
                let inst_id = self
                    .function
                    .instructions
                    .insert(ir::Instruction::OpenVariable(var_id));
                self.function.blocks[self.function.start_block]
                    .instructions
                    .push(inst_id);
            }
        }

        Some(var_id)
    }

    fn get_var(&mut self, span: Span, vname: &S) -> Result<Option<ir::VarId>, IrGenError> {
        if let Some(var) = self.variables.get(vname).and_then(|s| s.last().copied()) {
            match var {
                VarDecl::Real(var_id) => Ok(Some(var_id)),
                VarDecl::Pseudo => Err(IrGenError {
                    kind: IrGenErrorKind::PseudoVarAccessed,
                    span,
                }),
            }
        } else {
            Ok(None)
        }
    }

    fn new_block(&mut self) -> ir::BlockId {
        self.function.blocks.insert(ir::Block::default())
    }

    fn end_current_block(&mut self, exit: ir::Exit) {
        let current_block = self.current_block.expect("no current block to end");
        self.function.blocks[current_block].exit = exit;
        self.current_block = None;
    }

    fn start_new_block(&mut self, block_id: ir::BlockId) {
        assert!(
            self.current_block.is_none(),
            "cannot start new block when current block is not finished"
        );
        self.current_block = Some(block_id);
    }

    fn push_break_target(&mut self, block_id: ir::BlockId) {
        self.break_target_stack.push(block_id);
    }

    fn pop_break_target(&mut self, block_id: ir::BlockId) {
        assert_eq!(
            self.break_target_stack.pop(),
            Some(block_id),
            "mismatched break target pop"
        );
    }

    fn push_continue_target(&mut self, block_id: ir::BlockId) {
        self.continue_target_stack.push(block_id);
    }

    fn pop_continue_target(&mut self, block_id: ir::BlockId) {
        assert_eq!(
            self.continue_target_stack.pop(),
            Some(block_id),
            "mismatched continue target pop"
        );
    }

    fn push_instruction(&mut self, span: Span, inst: ir::Instruction<S>) -> ir::InstId {
        let current_block = self
            .current_block
            .expect("no current block to push instruction");
        let inst_id = self.function.instructions.insert(inst);
        self.function.blocks[current_block]
            .instructions
            .push(inst_id);
        if !span.is_null() {
            self.function.spans.insert(inst_id, span);
        }
        inst_id
    }
}
