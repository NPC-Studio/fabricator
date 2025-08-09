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
    /// Use proper block scoping for variable declarations.
    ///
    /// if `false`, then all variable declarations will be visible until the end of the enclosing
    /// function even when the enclosing scope ends.
    pub block_scoping: bool,

    /// Allow lambda expressions to reference variables from outer functions.
    ///
    /// Without this, such variables will instead be interpreted as implicit `this` variables.
    ///
    /// # Block scoping and closures
    ///
    /// Closing over a variable which is declared in the body of a loop will act differently
    /// depending on whether `block_scoping` is enabled or not. With `block_scoping`, each variable
    /// in a loop iteration is independent, without it, every variable in the body of a loop is
    /// always the same instance. The first behavior is similar to ECMAScript closures with the
    /// `let` keyword, the second behavior is similar to ECMAScript closures with the `var` keyword.
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
            block_scoping: true,
            allow_closures: true,
            allow_constructors: false,
        }
    }

    pub fn compat() -> Self {
        IrGenSettings {
            block_scoping: false,
            allow_closures: false,
            allow_constructors: true,
        }
    }

    pub fn gen_chunk_ir<S>(
        self,
        mut interner: impl StringInterner<String = S>,
        block: &ast::Block<S>,
        find_magic: impl Fn(&S) -> Option<MagicMode>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone,
    {
        let mut compiler =
            FunctionCompiler::new(self, &mut interner, FunctionRef::Chunk, &find_magic);
        compiler.block(block)?;
        Ok(compiler.finish())
    }

    pub fn gen_func_stmt_ir<S>(
        self,
        mut interner: impl StringInterner<String = S>,
        func_span: Span,
        func_stmt: &ast::FunctionStatement<S>,
        find_magic: impl Fn(&S) -> Option<MagicMode>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone + AsRef<str>,
    {
        let mut compiler = FunctionCompiler::new(
            self,
            &mut interner,
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
            compiler.constructor(func_stmt.inherit.as_ref(), &func_stmt.body)?;
        } else {
            compiler.block(&func_stmt.body)?;
        }
        Ok(compiler.finish())
    }
}

struct FunctionCompiler<'a, S, I> {
    settings: IrGenSettings,
    interner: &'a mut I,
    find_magic: &'a dyn (Fn(&S) -> Option<MagicMode>),

    function: ir::Function<S>,

    // If we have a final block to jump to, jump here instead of returning. Setting this disallows
    // return statements with values.
    final_block: Option<ir::BlockId>,

    // This will be `Some` if the current block is unfinished and can be appended to.
    current_block: Option<ir::BlockId>,

    break_target_stack: Vec<NonLocalJump>,
    continue_target_stack: Vec<NonLocalJump>,

    scopes: Vec<Scope<S>>,

    // Maps in-scope variable names to a list of scope indexes for scopes which contain variables
    // with this name.
    //
    // This list is always kept in scope stack order, so the top entry in the list is always the
    // variable currently visible for this name.
    var_lookup: HashMap<ast::Ident<S>, Vec<usize>>,
}

struct Scope<S> {
    // Variables to close when this scope ends. May include shadowed variables.
    to_close: Vec<ir::VarId>,

    // All variable declarations for this scope which have not been shadowed. When a new declaration
    // shadows another, the new declaration will replace the old in this map.
    visible: HashMap<ast::Ident<S>, VarDecl>,
}

#[derive(Debug, Copy, Clone)]
struct NonLocalJump {
    target: ir::BlockId,
    // Stack index to which we are jumping. Non-local jumps need to close any variables between the
    // top-level scope and this.
    pop_vars_to: usize,
}

impl<S> Default for Scope<S> {
    fn default() -> Self {
        Self {
            to_close: Vec::new(),
            visible: HashMap::new(),
        }
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
        indexes: Vec<ir::InstId>,
    },
    Magic(S),
}

impl<'a, S, I> FunctionCompiler<'a, S, I>
where
    S: Eq + Hash + Clone,
    I: StringInterner<String = S>,
{
    fn new(
        settings: IrGenSettings,
        interner: &'a mut I,
        reference: FunctionRef,
        find_magic: &'a dyn Fn(&S) -> Option<MagicMode>,
    ) -> Self {
        let instructions = ir::InstructionMap::new();
        let spans = ir::SpanMap::new();
        let mut blocks = ir::BlockMap::new();
        let variables = ir::VariableMap::new();
        let shadow_vars = ir::ShadowVarSet::new();
        let this_scopes = ir::ThisScopeSet::new();
        let call_scopes = ir::CallScopeSet::new();
        let functions = ir::FunctionMap::new();

        // We leave the start block completely empty except for a jump to the "real" start block.
        //
        // This is so that we can use the start block as a place to put non-block-scoped variable
        // declaration instructions that need to be before any other generated IR, and avoid being
        // O(n^2) in the number of variable declarations.
        let start_block = blocks.insert(ir::Block::default());
        let first_block = blocks.insert(ir::Block::default());
        blocks[start_block].exit = ir::Exit::Jump(first_block);

        let function = ir::Function {
            num_parameters: 0,
            is_constructor: false,
            reference,
            instructions,
            spans,
            blocks,
            variables,
            shadow_vars,
            this_scopes,
            call_scopes,
            functions,
            start_block,
        };

        Self {
            settings,
            interner,
            find_magic,
            function,
            final_block: None,
            current_block: Some(first_block),
            continue_target_stack: Vec::new(),
            break_target_stack: Vec::new(),
            // Even with no block scoping, all functions must have one top-level scope.
            scopes: vec![Scope::default()],
            var_lookup: HashMap::new(),
        }
    }

    fn declare_parameters(&mut self, parameters: &[ast::Parameter<S>]) -> Result<(), IrGenError> {
        self.function.num_parameters = parameters.len();

        for (param_index, param) in parameters.iter().enumerate() {
            let arg_var = self
                .declare_var(param.name.clone(), Some(ir::Variable::Owned))
                .unwrap();

            let mut value =
                self.push_instruction(param.span, ir::Instruction::Argument(param_index));

            if let Some(default) = &param.default {
                let def_value = default.clone().fold_constant().ok_or(IrGenError {
                    kind: IrGenErrorKind::ParameterDefaultNotConstant,
                    span: default.span(),
                })?;
                let def_value =
                    self.push_instruction(param.span, ir::Instruction::Constant(def_value));

                value = self.push_instruction(
                    param.span,
                    ir::Instruction::BinOp {
                        left: value,
                        op: ir::BinOp::NullCoalesce,
                        right: def_value,
                    },
                );
            };

            self.push_instruction(
                param.name.span,
                ir::Instruction::SetVariable(arg_var, value),
            );
        }

        Ok(())
    }

    fn constructor(
        &mut self,
        inherit: Option<&ast::Call<S>>,
        main_block: &ast::Block<S>,
    ) -> Result<(), IrGenError> {
        self.function.is_constructor = true;

        // Create a hidden static variable to hold whether the constructor static object is
        // initialized.
        let is_initialized = self
            .function
            .variables
            .insert(ir::Variable::Static(Constant::Boolean(false)));

        let get_constructor_super_name = self.interner.intern(BuiltIns::GET_CONSTRUCTOR_SUPER);
        let get_constructor_super = self.push_instruction(
            main_block.span,
            ir::Instruction::GetMagic(get_constructor_super_name),
        );

        let set_super_name = self.interner.intern(BuiltIns::SET_SUPER);
        let set_super =
            self.push_instruction(main_block.span, ir::Instruction::GetMagic(set_super_name));

        let init_block = self.new_block();
        let successor_block = self.new_block();
        let final_block = self.new_block();
        self.final_block = Some(final_block);

        let parent_func = if let Some(inherit) = inherit {
            Some(self.expression(&inherit.base)?)
        } else {
            None
        };

        let this_closure = self.push_instruction(main_block.span, ir::Instruction::CurrentClosure);

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            main_block.span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: get_constructor_super,
                args: vec![this_closure],
            },
        );
        let our_super =
            self.push_instruction(main_block.span, ir::Instruction::GetReturn(call_scope, 0));
        self.push_instruction(main_block.span, ir::Instruction::CloseCall(call_scope));

        let check_initialized = self.push_instruction(
            main_block.span,
            ir::Instruction::GetVariable(is_initialized),
        );
        self.end_current_block(ir::Exit::Branch {
            cond: check_initialized,
            if_true: successor_block,
            if_false: init_block,
        });

        self.start_new_block(init_block);

        if inherit.is_some() {
            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                main_block.span,
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: get_constructor_super,
                    args: vec![parent_func.unwrap()],
                },
            );
            let parent_super =
                self.push_instruction(main_block.span, ir::Instruction::GetReturn(call_scope, 0));
            self.push_instruction(main_block.span, ir::Instruction::CloseCall(call_scope));

            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                main_block.span,
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: set_super,
                    args: vec![our_super, parent_super],
                },
            );
            self.push_instruction(main_block.span, ir::Instruction::CloseCall(call_scope));
        }

        let mut static_names = HashSet::new();

        for stmt in &main_block.statements {
            if let ast::Statement::Static(decls) = stmt {
                for (decl_name, decl_value) in &decls.vars {
                    if !static_names.insert(decl_name) {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ConstructorStaticNotUnique,
                            span: decls.span,
                        });
                    }

                    let key = self.push_instruction(
                        decls.span,
                        ir::Instruction::Constant(Constant::String(decl_name.inner.clone())),
                    );
                    let value = self.expression(decl_value.as_ref().ok_or(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotInitialized,
                        span: decls.span,
                    })?)?;

                    self.push_instruction(
                        decls.span,
                        ir::Instruction::SetField {
                            object: our_super,
                            key,
                            value,
                        },
                    );
                }
            }
        }

        let true_ = self.push_instruction(main_block.span, ir::Instruction::Boolean(true));
        self.push_instruction(
            main_block.span,
            ir::Instruction::SetVariable(is_initialized, true_),
        );

        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(successor_block);

        let this = if let Some(parent_func) = parent_func {
            let Some(inherit) = inherit else {
                unreachable!();
            };

            let mut args = Vec::new();
            for arg in &inherit.arguments {
                args.push(self.expression(arg)?);
            }

            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                inherit.span,
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: parent_func,
                    args,
                },
            );
            let ret =
                self.push_instruction(inherit.span, ir::Instruction::GetReturn(call_scope, 0));
            self.push_instruction(inherit.span, ir::Instruction::CloseCall(call_scope));
            ret
        } else {
            self.push_instruction(main_block.span, ir::Instruction::NewObject)
        };

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            main_block.span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: set_super,
                args: vec![this, our_super],
            },
        );
        self.push_instruction(main_block.span, ir::Instruction::CloseCall(call_scope));

        let this_scope = self.function.this_scopes.insert(());
        self.push_instruction(main_block.span, ir::Instruction::OpenThisScope(this_scope));
        self.push_instruction(main_block.span, ir::Instruction::SetThis(this_scope, this));

        self.push_scope();

        for stmt in &main_block.statements {
            match stmt {
                ast::Statement::Static(decls) => {
                    for (decl_name, _) in &decls.vars {
                        // Declare a pseudo-variable when encountering the static statement.
                        // This variable's sole purpose is to prevent accessing a constructor
                        // `static`, which *looks* like a variable but is not usable as a variable
                        // in expressions.
                        self.declare_var(decl_name.clone(), None);
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
                span: statement.span(),
            });
        }

        match statement {
            ast::Statement::Empty(_) => Ok(()),
            ast::Statement::Block(block_stmt) => self.block(&block_stmt.block),
            ast::Statement::Enum(enum_stmt) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedEnum,
                span: enum_stmt.span,
            }),
            ast::Statement::Function(func_stmt) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedFunctionStmt,
                span: func_stmt.span,
            }),
            ast::Statement::Var(var_decls) => {
                for (name, value) in &var_decls.vars {
                    self.var_declaration(var_decls.span, name, value.as_ref())?;
                }
                Ok(())
            }
            ast::Statement::Static(static_decls) => {
                if self.function.is_constructor {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotTopLevel,
                        span: static_decls.span,
                    });
                }

                for (name, value) in &static_decls.vars {
                    self.static_declaration(static_decls.span, name, value.as_ref())?;
                }
                Ok(())
            }
            ast::Statement::Assignment(assignment_statement) => {
                self.assignment_statement(assignment_statement)
            }
            ast::Statement::Return(return_) => self.return_statement(return_),
            ast::Statement::If(if_stmt) => self.if_statement(if_stmt),
            ast::Statement::For(for_stmt) => self.for_statement(for_stmt),
            ast::Statement::While(while_stmt) => self.while_statement(while_stmt),
            ast::Statement::Repeat(repeat_stmt) => self.repeat_statement(repeat_stmt),
            ast::Statement::Switch(switch_stmt) => self.switch_statement(switch_stmt),
            ast::Statement::With(with_stmt) => self.with_statement(with_stmt),
            ast::Statement::Throw(throw_stmt) => {
                let target = self.expression(&throw_stmt.target)?;
                self.push_instruction(throw_stmt.span, ir::Instruction::Throw(target));
                Ok(())
            }
            ast::Statement::Call(function_call) => {
                let _ = self.call_expr(function_call)?;
                Ok(())
            }
            ast::Statement::Prefix(mutation) => {
                self.mutation_op(mutation)?;
                Ok(())
            }
            ast::Statement::Postfix(mutation) => {
                self.mutation_op(mutation)?;
                Ok(())
            }
            ast::Statement::Break(span) => self.break_statement(*span),
            ast::Statement::Continue(span) => self.continue_statement(*span),
        }
    }

    fn var_declaration(
        &mut self,
        span: Span,
        name: &ast::Ident<S>,
        value: Option<&ast::Expression<S>>,
    ) -> Result<(), IrGenError> {
        let var_id = self
            .declare_var(name.clone(), Some(ir::Variable::Owned))
            .unwrap();
        if let Some(value) = value {
            let inst_id = self.expression(value)?;
            self.push_instruction(span, ir::Instruction::SetVariable(var_id, inst_id));
        }
        Ok(())
    }

    fn static_declaration(
        &mut self,
        span: Span,
        name: &ast::Ident<S>,
        value: Option<&ast::Expression<S>>,
    ) -> Result<(), IrGenError> {
        if let Some(value) = value {
            if let Some(constant) = value.clone().fold_constant() {
                // If our static is a constant, then we can just initialize it when the prototype is
                // created.
                self.declare_var(name.clone(), Some(ir::Variable::Static(constant)));
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
                        name.clone(),
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
                let true_ = self.push_instruction(span, ir::Instruction::Boolean(true));
                self.push_instruction(span, ir::Instruction::SetVariable(is_initialized, true_));

                self.end_current_block(ir::Exit::Jump(successor));

                self.start_new_block(successor);
            }
        } else {
            // If our static has no value then it is just initialized as `Undefined`.
            self.declare_var(
                name.clone(),
                Some(ir::Variable::Static(Constant::Undefined)),
            );
        }

        Ok(())
    }

    fn assignment_statement(
        &mut self,
        assign_stmt: &ast::AssignmentStatement<S>,
    ) -> Result<(), IrGenError> {
        let target = self.mutable_target(&assign_stmt.target)?;
        let val = self.expression(&assign_stmt.value)?;

        let assign = match assign_stmt.op {
            ast::AssignmentOp::Equal => val,
            ast::AssignmentOp::PlusEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Add,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::MinusEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Sub,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::MultEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Mult,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::DivEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Div,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::NullCoalesce => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::NullCoalesce,
                        right: val,
                    },
                )
            }
        };

        self.write_mutable_target(assign_stmt.span, target, assign);

        Ok(())
    }

    fn return_statement(&mut self, ret_stmt: &ast::ReturnStatement<S>) -> Result<(), IrGenError> {
        if self.final_block.is_some() && ret_stmt.value.is_some() {
            return Err(IrGenError {
                kind: IrGenErrorKind::CannotReturnValue,
                span: ret_stmt.span,
            });
        }

        let exit = if let Some(value) = &ret_stmt.value {
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

    fn while_statement(&mut self, while_stmt: &ast::LoopStatement<S>) -> Result<(), IrGenError> {
        let cond_block = self.new_block();
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_break_target(successor_block);
        self.push_continue_target(cond_block);

        self.end_current_block(ir::Exit::Jump(cond_block));
        self.start_new_block(cond_block);

        let cond = self.expression(&while_stmt.target)?;
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

    fn repeat_statement(&mut self, repeat_stmt: &ast::LoopStatement<S>) -> Result<(), IrGenError> {
        let times = self.expression(&repeat_stmt.target)?;

        let dec_var = self.function.variables.insert(ir::Variable::Owned);
        self.push_instruction(repeat_stmt.span, ir::Instruction::OpenVariable(dec_var));
        self.push_instruction(
            repeat_stmt.span,
            ir::Instruction::SetVariable(dec_var, times),
        );

        let cond_block = self.new_block();
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_break_target(successor_block);
        self.push_continue_target(cond_block);

        self.end_current_block(ir::Exit::Jump(cond_block));
        self.start_new_block(cond_block);

        let prev = self.push_instruction(repeat_stmt.span, ir::Instruction::GetVariable(dec_var));
        self.end_current_block(ir::Exit::Branch {
            cond: prev,
            if_true: body_block,
            if_false: successor_block,
        });

        self.start_new_block(body_block);

        let dec = self.push_instruction(
            repeat_stmt.span,
            ir::Instruction::UnOp {
                op: ir::UnOp::Decrement,
                source: prev,
            },
        );
        self.push_instruction(repeat_stmt.span, ir::Instruction::SetVariable(dec_var, dec));

        self.push_scope();
        self.statement(&repeat_stmt.body)?;
        self.pop_scope();

        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(cond_block));
        }

        self.start_new_block(successor_block);

        self.pop_continue_target(cond_block);
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

        for case in &switch_stmt.cases {
            let compare = self.expression(&case.compare)?;
            let is_equal = self.push_instruction(
                case.span,
                ir::Instruction::BinOp {
                    left: target,
                    op: ir::BinOp::Equal,
                    right: compare,
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
            self.block(&case.body)?;
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

    fn with_statement(&mut self, with_stmt: &ast::LoopStatement<S>) -> Result<(), IrGenError> {
        let target = self.expression(&with_stmt.target)?;

        let state_var = self.function.variables.insert(ir::Variable::Owned);
        self.push_instruction(with_stmt.span, ir::Instruction::OpenVariable(state_var));

        let with_loop_iter_name = self.interner.intern(BuiltIns::WITH_LOOP_ITER);
        let with_loop_iter = self.push_instruction(
            with_stmt.span,
            ir::Instruction::GetMagic(with_loop_iter_name),
        );

        // The iteration protocol is to call the iter init function and expect two returns, the
        // iteration function and an initial state value.
        //
        // Every time through the loop, if the state value is not `Value::Undefined`, then call the
        // iteration function with the state as its single parameter.
        //
        // The iteration function should return the new value for the state followed by all iter
        // results.

        let setup_call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            with_stmt.span,
            ir::Instruction::OpenCall {
                scope: setup_call_scope,
                func: with_loop_iter,
                args: vec![target],
            },
        );

        let iter_fn = self.push_instruction(
            with_stmt.span,
            ir::Instruction::GetReturn(setup_call_scope, 0),
        );
        let init_state = self.push_instruction(
            with_stmt.span,
            ir::Instruction::GetReturn(setup_call_scope, 1),
        );

        self.push_instruction(with_stmt.span, ir::Instruction::CloseCall(setup_call_scope));

        self.push_instruction(
            with_stmt.span,
            ir::Instruction::SetVariable(state_var, init_state),
        );

        let this_scope = self.function.this_scopes.insert(());
        self.push_instruction(with_stmt.span, ir::Instruction::OpenThisScope(this_scope));

        let check_block = self.new_block();
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_continue_target(check_block);
        self.push_break_target(successor_block);

        self.end_current_block(ir::Exit::Jump(check_block));
        self.start_new_block(check_block);

        let cur_state =
            self.push_instruction(with_stmt.span, ir::Instruction::GetVariable(state_var));

        let state_is_undef = self.push_instruction(
            with_stmt.span,
            ir::Instruction::UnOp {
                op: ir::UnOp::IsUndefined,
                source: cur_state,
            },
        );

        self.end_current_block(ir::Exit::Branch {
            cond: state_is_undef,
            if_true: successor_block,
            if_false: body_block,
        });
        self.start_new_block(body_block);

        let iter_call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            with_stmt.span,
            ir::Instruction::OpenCall {
                scope: iter_call_scope,
                func: iter_fn,
                args: vec![cur_state],
            },
        );
        let next_state = self.push_instruction(
            with_stmt.span,
            ir::Instruction::GetReturn(iter_call_scope, 0),
        );
        let iter_val = self.push_instruction(
            with_stmt.span,
            ir::Instruction::GetReturn(iter_call_scope, 1),
        );
        self.push_instruction(with_stmt.span, ir::Instruction::CloseCall(iter_call_scope));

        self.push_instruction(
            with_stmt.span,
            ir::Instruction::SetVariable(state_var, next_state),
        );

        self.push_instruction(
            with_stmt.span,
            ir::Instruction::SetThis(this_scope, iter_val),
        );

        self.push_scope();
        self.statement(&with_stmt.body)?;
        self.pop_scope();

        if self.current_block.is_some() {
            self.end_current_block(ir::Exit::Jump(successor_block));
        }

        self.start_new_block(successor_block);

        self.pop_break_target(successor_block);
        self.pop_continue_target(check_block);

        self.push_instruction(with_stmt.span, ir::Instruction::CloseThisScope(this_scope));
        self.push_instruction(with_stmt.span, ir::Instruction::CloseVariable(state_var));

        Ok(())
    }

    fn break_statement(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&jump) = self.break_target_stack.last() {
            self.non_local_jump(jump)
        } else {
            Err(IrGenError {
                kind: IrGenErrorKind::BreakWithNoTarget,
                span,
            })
        }
    }

    fn continue_statement(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&jump) = self.continue_target_stack.last() {
            self.non_local_jump(jump)
        } else {
            Err(IrGenError {
                kind: IrGenErrorKind::ContinueWithNoTarget,
                span,
            })
        }
    }

    fn non_local_jump(&mut self, jump: NonLocalJump) -> Result<(), IrGenError> {
        // We may be jumping to an outer scope which should cause variables declared in inner scopes
        // to close. Generate a cleanup block to close all of the variables between this scope and
        // the target scope.
        let cleanup_block = self.new_block();
        self.end_current_block(ir::Exit::Jump(cleanup_block));
        self.start_new_block(cleanup_block);

        for i in (jump.pop_vars_to + 1..self.scopes.len()).rev() {
            for &var_id in &self.scopes[i].to_close {
                let inst_id = self
                    .function
                    .instructions
                    .insert(ir::Instruction::CloseVariable(var_id));
                self.function.blocks[cleanup_block]
                    .instructions
                    .push(inst_id);
            }
        }

        self.end_current_block(ir::Exit::Jump(jump.target));
        Ok(())
    }

    fn expression(&mut self, expr: &ast::Expression<S>) -> Result<ir::InstId, IrGenError> {
        Ok(match expr {
            ast::Expression::Constant(c, span) => {
                self.push_instruction(*span, ir::Instruction::Constant(c.clone()))
            }
            ast::Expression::Ident(s) => self.ident_expr(s)?,
            ast::Expression::Global(span) => self.push_instruction(*span, ir::Instruction::Globals),
            ast::Expression::This(span) => self.push_instruction(*span, ir::Instruction::This),
            ast::Expression::Other(span) => self.push_instruction(*span, ir::Instruction::Other),
            ast::Expression::Group(expr) => self.expression(&expr.inner)?,
            ast::Expression::Object(fields) => {
                let object = self.push_instruction(fields.span, ir::Instruction::NewObject);

                for field in &fields.fields {
                    let field_span = field.span();
                    match field {
                        ast::Field::Value(name, value) => {
                            let this_scope = if matches!(value, ast::Expression::Function(_)) {
                                // Within a struct literal, closures always bind `this` to the
                                // struct currently being created.
                                let this_scope = self.function.this_scopes.insert(());
                                self.push_instruction(
                                    field_span,
                                    ir::Instruction::OpenThisScope(this_scope),
                                );
                                self.push_instruction(
                                    field_span,
                                    ir::Instruction::SetThis(this_scope, object),
                                );
                                Some(this_scope)
                            } else {
                                None
                            };

                            let value = self.expression(value)?;
                            self.push_instruction(
                                field_span,
                                ir::Instruction::SetFieldConst {
                                    object,
                                    key: Constant::String(name.inner.clone()),
                                    value,
                                },
                            );

                            if let Some(this_scope) = this_scope {
                                self.push_instruction(
                                    field_span,
                                    ir::Instruction::CloseThisScope(this_scope),
                                );
                            }
                        }
                        ast::Field::Init(name) => {
                            let value = self.ident_expr(name)?;
                            self.push_instruction(
                                field_span,
                                ir::Instruction::SetFieldConst {
                                    object,
                                    key: Constant::String(name.inner.clone()),
                                    value,
                                },
                            );
                        }
                    }
                }

                object
            }
            ast::Expression::Array(array) => {
                let array_inst = self.push_instruction(array.span, ir::Instruction::NewArray);
                for (i, value) in array.entries.iter().enumerate() {
                    let value = self.expression(value)?;
                    self.push_instruction(
                        array.span,
                        ir::Instruction::SetIndexConst {
                            array: array_inst,
                            index: Constant::Integer(i as i64),
                            value,
                        },
                    );
                }
                array_inst
            }
            ast::Expression::Unary(unary_expr) => {
                let inst = ir::Instruction::UnOp {
                    op: match unary_expr.op {
                        ast::UnaryOp::Not => ir::UnOp::Not,
                        ast::UnaryOp::Minus => ir::UnOp::Neg,
                    },
                    source: self.expression(&unary_expr.target)?,
                };
                self.push_instruction(unary_expr.span, inst)
            }
            ast::Expression::Prefix(mutation) => self.mutation_op(mutation)?.1,
            ast::Expression::Postfix(mutation) => self.mutation_op(mutation)?.0,
            ast::Expression::Binary(bin_expr) => match bin_expr.op {
                ast::BinaryOp::Add => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Add,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Sub => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Sub,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Mult => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Mult,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Div => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Div,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Rem => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Rem,
                            right,
                        },
                    )
                }
                ast::BinaryOp::IDiv => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::IDiv,
                            right,
                        },
                    )
                }
                ast::BinaryOp::Equal => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Equal,
                            right,
                        },
                    )
                }
                ast::BinaryOp::NotEqual => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::NotEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::LessThan => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::LessThan,
                            right,
                        },
                    )
                }
                ast::BinaryOp::LessEqual => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::LessEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::GreaterThan => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::GreaterThan,
                            right,
                        },
                    )
                }
                ast::BinaryOp::GreaterEqual => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::GreaterEqual,
                            right,
                        },
                    )
                }
                ast::BinaryOp::And => {
                    self.short_circuit_and(bin_expr.span, &bin_expr.left, &bin_expr.right)?
                }
                ast::BinaryOp::Or => {
                    self.short_circuit_or(bin_expr.span, &bin_expr.left, &bin_expr.right)?
                }
                ast::BinaryOp::NullCoalesce => self.short_circuit_null_coalesce(
                    bin_expr.span,
                    &bin_expr.left,
                    &bin_expr.right,
                )?,
            },
            ast::Expression::Ternary(tern_expr) => {
                let cond = self.expression(&tern_expr.cond)?;
                self.if_expression(
                    tern_expr.span,
                    cond,
                    |this| this.expression(&tern_expr.if_true),
                    |this| this.expression(&tern_expr.if_false),
                )?
            }
            ast::Expression::Function(func_expr) => {
                let func_id = self.inner_function(
                    FunctionRef::Expression(func_expr.span),
                    &func_expr.parameters,
                    &func_expr.body,
                )?;
                self.push_instruction(func_expr.span, ir::Instruction::Closure(func_id))
            }
            ast::Expression::Call(call) => self.call_expr(call)?,
            ast::Expression::Field(field_expr) => {
                let base = self.expression(&field_expr.base)?;
                let field = self.push_instruction(
                    field_expr.span,
                    ir::Instruction::Constant(Constant::String(field_expr.field.inner.clone())),
                );
                self.push_instruction(
                    field_expr.span,
                    ir::Instruction::GetField {
                        object: base,
                        key: field,
                    },
                )
            }
            ast::Expression::Index(index_expr) => {
                let base = self.expression(&index_expr.base)?;
                let mut indexes = Vec::new();
                for index in &index_expr.indexes {
                    indexes.push(self.expression(index)?);
                }
                self.push_instruction(
                    index_expr.span,
                    ir::Instruction::GetIndex {
                        array: base,
                        indexes,
                    },
                )
            }
        })
    }

    fn call_expr(&mut self, call: &ast::Call<S>) -> Result<ir::InstId, IrGenError> {
        enum CallType {
            Function(ir::InstId),
            Method {
                func: ir::InstId,
                object: ir::InstId,
            },
        }

        // Function calls on fields are interpreted as "methods", and implicitly bind the containing
        // object as `this` for the function call.
        let call_type = if let ast::Expression::Field(field_expr) = &*call.base {
            let object = self.expression(&field_expr.base)?;
            let key = self.push_instruction(
                call.span,
                ir::Instruction::Constant(Constant::String(field_expr.field.inner.clone())),
            );
            let func =
                self.push_instruction(call.base.span(), ir::Instruction::GetField { object, key });
            CallType::Method { func, object }
        } else {
            CallType::Function(self.expression(&call.base)?)
        };

        let mut args = Vec::new();
        for arg in &call.arguments {
            args.push(self.expression(arg)?);
        }

        let (func, this) = match call_type {
            CallType::Function(func) => (func, None),
            CallType::Method { func, object } => (func, Some(object)),
        };

        let call_scope = self.function.call_scopes.insert(());

        let this_scope = if let Some(this) = this {
            let this_scope = self.function.this_scopes.insert(());
            self.push_instruction(call.span, ir::Instruction::OpenThisScope(this_scope));
            self.push_instruction(call.span, ir::Instruction::SetThis(this_scope, this));
            Some(this_scope)
        } else {
            None
        };

        self.push_instruction(
            call.span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func,
                args,
            },
        );

        let ret = self.push_instruction(call.span, ir::Instruction::GetReturn(call_scope, 0));
        self.push_instruction(call.span, ir::Instruction::CloseCall(call_scope));

        if let Some(this_scope) = this_scope {
            self.push_instruction(call.span, ir::Instruction::CloseThisScope(this_scope));
        }

        Ok(ret)
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

    fn ident_expr(&mut self, ident: &ast::Ident<S>) -> Result<ir::InstId, IrGenError> {
        Ok(if let Some(var_id) = self.get_var(ident.span, ident)? {
            self.push_instruction(ident.span, ir::Instruction::GetVariable(var_id))
        } else if (self.find_magic)(ident).is_some() {
            self.push_instruction(ident.span, ir::Instruction::GetMagic(ident.inner.clone()))
        } else {
            let this = self.push_instruction(ident.span, ir::Instruction::This);
            let key = self.push_instruction(
                ident.span,
                ir::Instruction::Constant(Constant::String(ident.inner.clone())),
            );
            self.push_instruction(ident.span, ir::Instruction::GetField { object: this, key })
        })
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
            |this| Ok(this.push_instruction(span, ir::Instruction::Boolean(false))),
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
            |this| Ok(this.push_instruction(span, ir::Instruction::Boolean(true))),
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
        let cond = self.push_instruction(
            span,
            ir::Instruction::UnOp {
                op: ir::UnOp::IsUndefined,
                source: left,
            },
        );
        self.if_expression(span, cond, |this| this.expression(right), |_| Ok(left))
    }

    /// Evaluate a `MutationOp` on a `MutableExpr`.
    ///
    /// Returns a tuple of the old and new values for the `MutableExpr`.
    fn mutation_op(
        &mut self,
        mutation: &ast::Mutation<S>,
    ) -> Result<(ir::InstId, ir::InstId), IrGenError> {
        let target = self.mutable_target(&mutation.target)?;
        let old = self.read_mutable_target(mutation.span, target.clone());
        let op = match mutation.op {
            ast::MutationOp::Increment => ir::UnOp::Increment,
            ast::MutationOp::Decrement => ir::UnOp::Decrement,
        };
        let new = self.push_instruction(mutation.span, ir::Instruction::UnOp { op, source: old });
        self.write_mutable_target(mutation.span, target, new);
        Ok((old, new))
    }

    fn mutable_target(
        &mut self,
        target: &ast::MutableExpr<S>,
    ) -> Result<MutableTarget<S>, IrGenError> {
        Ok(match target {
            ast::MutableExpr::Ident(ident) => {
                if let Some(var_id) = self.get_var(ident.span, ident)? {
                    MutableTarget::Var(var_id)
                } else if let Some(mode) = (self.find_magic)(ident) {
                    if mode == MagicMode::ReadOnly {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ReadOnlyMagic,
                            span: target.span(),
                        });
                    }

                    MutableTarget::Magic(ident.inner.clone())
                } else {
                    let key = self.push_instruction(
                        target.span(),
                        ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                    );
                    MutableTarget::This { key }
                }
            }
            ast::MutableExpr::Field(field_expr) => {
                let object = self.expression(&field_expr.base)?;
                let key = self.push_instruction(
                    target.span(),
                    ir::Instruction::Constant(Constant::String(field_expr.field.inner.clone())),
                );
                MutableTarget::Field { object, key }
            }
            ast::MutableExpr::Index(index_expr) => {
                let array = self.expression(&index_expr.base)?;
                let mut indexes = Vec::new();
                for index in &index_expr.indexes {
                    indexes.push(self.expression(index)?);
                }
                MutableTarget::Index { array, indexes }
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
            MutableTarget::Index { array, indexes } => ir::Instruction::GetIndex { array, indexes },
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
            MutableTarget::Index { array, indexes } => {
                self.push_instruction(
                    span,
                    ir::Instruction::SetIndex {
                        array,
                        indexes,
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
        let mut compiler =
            FunctionCompiler::new(self.settings, self.interner, reference, self.find_magic);

        // If we allow closures, then pass every currently in-scope variable as an upvar.
        if self.settings.allow_closures {
            for (name, scope_list) in &self.var_lookup {
                let &scope_index = scope_list.last().unwrap();
                compiler.declare_var(
                    name.clone(),
                    match self.scopes[scope_index].visible[name] {
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
        if self.settings.block_scoping {
            self.scopes.push(Scope::default());
        }
    }

    fn pop_scope(&mut self) {
        if self.settings.block_scoping {
            if let Some(popped_scope) = self.scopes.pop() {
                // Close every variable in the popped scope. If the current block doesn't exist,
                // then we assume that either this block has exited, which is an implicit close, or
                // that a `break` or `continue` statement has separately closed them.
                if self.current_block.is_some() {
                    for var_id in popped_scope.to_close {
                        self.push_instruction(Span::null(), ir::Instruction::CloseVariable(var_id));
                    }
                }

                // Remove visible variables in the popped scope from the var lookup map.
                for (vname, _) in popped_scope.visible {
                    let hash_map::Entry::Occupied(mut entry) = self.var_lookup.entry(vname) else {
                        unreachable!();
                    };

                    let scope_index = entry.get_mut().pop().unwrap();
                    // The var lookup map should contain every visible variable in this scope.
                    assert!(scope_index == self.scopes.len());

                    // Just remove the variable entry entirely if there are no variables with this
                    // name visible.
                    if entry.get().is_empty() {
                        entry.remove();
                    }
                }
            }
        }
    }

    // Declare a variable in the current scope. If the IR variable is `None`, declares a
    // pseudo-variable.
    fn declare_var(
        &mut self,
        vname: ast::Ident<S>,
        var: Option<ir::Variable<S>>,
    ) -> Option<ir::VarId> {
        let top_scope_index = self.scopes.len() - 1;
        let top_scope = self.scopes.last_mut().unwrap();

        let (var_decl, var_id, var_is_owned) = if let Some(var) = var {
            let is_owned = var.is_owned();
            let var_id = self.function.variables.insert(var);
            let decl = VarDecl::Real(var_id);
            (decl, Some(var_id), is_owned)
        } else {
            (VarDecl::Pseudo, None, false)
        };

        let shadowing = top_scope.visible.insert(vname.clone(), var_decl).is_some();

        let scope_list = self.var_lookup.entry(vname).or_default();
        if shadowing {
            // The new variable shadows a previous one, so there should already be an existing entry
            // at the top of the scope list for the top-level scope.
            assert_eq!(*scope_list.last().unwrap(), top_scope_index);
        } else {
            // If we are not shadowing, we expect any current active entry to be of an outer scope.
            assert!(scope_list.last().is_none_or(|&ind| ind < top_scope_index));
            scope_list.push(top_scope_index);
        }

        if var_is_owned {
            let var_id = var_id.unwrap();
            // We own this variable, so we need to open it when it is declared.
            if self.settings.block_scoping {
                // Since we're using block scoping, we need to close this variable when the scope
                // ends.
                top_scope.to_close.push(var_id);
                self.push_instruction(Span::null(), ir::Instruction::OpenVariable(var_id));
            } else {
                // If we're not using block scoping, just open every variable at the very start of
                // the function. This keeps the IR well-formed even with no block scoping and no
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

        var_id
    }

    fn get_var(&mut self, span: Span, vname: &S) -> Result<Option<ir::VarId>, IrGenError> {
        if let Some(scope_index) = self.var_lookup.get(vname).and_then(|l| l.last().copied()) {
            match self.scopes[scope_index].visible[vname] {
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

    fn push_break_target(&mut self, target_block: ir::BlockId) {
        self.break_target_stack.push(NonLocalJump {
            target: target_block,
            pop_vars_to: self.scopes.len() - 1,
        });
    }

    fn pop_break_target(&mut self, block_id: ir::BlockId) {
        assert!(
            self.break_target_stack
                .pop()
                .is_some_and(|j| j.target == block_id),
            "mismatched break target pop"
        );
    }

    fn push_continue_target(&mut self, target_block: ir::BlockId) {
        self.continue_target_stack.push(NonLocalJump {
            target: target_block,
            pop_vars_to: self.scopes.len() - 1,
        });
    }

    fn pop_continue_target(&mut self, block_id: ir::BlockId) {
        assert!(
            self.continue_target_stack
                .pop()
                .is_some_and(|j| j.target == block_id),
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
