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
            compiler.constructor(interner, func_stmt.inherit.as_ref(), &func_stmt.body)?;
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
        indexes: Vec<ir::InstId>,
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
    scopes: Vec<HashSet<ast::Ident<S>>>,

    // Stack of declared variables for each variable name.
    //
    // Each variable declared in a scope will push an entry for the corresponding variable here.
    variables: HashMap<ast::Ident<S>, Vec<VarDecl>>,
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
        let call_scopes = ir::CallScopeSet::new();
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
                    span: default.span,
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
        mut interner: impl StringInterner<String = S>,
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

        let parent_func = if let Some(inherit) = inherit {
            Some(self.expression(&inherit.base)?)
        } else {
            None
        };

        let this_closure = self.push_instruction(Span::null(), ir::Instruction::CurrentClosure);

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            Span::null(),
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: get_constructor_super,
                args: vec![this_closure],
            },
        );
        let our_super =
            self.push_instruction(Span::null(), ir::Instruction::GetReturn(call_scope, 0));
        self.push_instruction(Span::null(), ir::Instruction::CloseCall(call_scope));

        let check_initialized =
            self.push_instruction(Span::null(), ir::Instruction::GetVariable(is_initialized));
        self.end_current_block(ir::Exit::Branch {
            cond: check_initialized,
            if_true: successor_block,
            if_false: init_block,
        });

        self.start_new_block(init_block);

        if inherit.is_some() {
            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                Span::null(),
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: get_constructor_super,
                    args: vec![parent_func.unwrap()],
                },
            );
            let parent_super =
                self.push_instruction(Span::null(), ir::Instruction::GetReturn(call_scope, 0));
            self.push_instruction(Span::null(), ir::Instruction::CloseCall(call_scope));

            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                Span::null(),
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: set_super,
                    args: vec![our_super, parent_super],
                },
            );
            self.push_instruction(Span::null(), ir::Instruction::CloseCall(call_scope));
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
                        ir::Instruction::Constant(Constant::String(decl.name.inner.clone())),
                    );
                    let value = self.expression(decl.value.as_ref().ok_or(IrGenError {
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
            self.push_instruction(Span::null(), ir::Instruction::CloseCall(call_scope));
            ret
        } else {
            self.push_instruction(Span::null(), ir::Instruction::NewObject)
        };

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            Span::null(),
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: set_super,
                args: vec![this, our_super],
            },
        );
        self.push_instruction(Span::null(), ir::Instruction::CloseCall(call_scope));

        let this_scope = self.function.this_scopes.insert(());
        self.push_instruction(Span::null(), ir::Instruction::OpenThisScope(this_scope));
        self.push_instruction(Span::null(), ir::Instruction::SetThis(this_scope, this));

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
            ast::StatementKind::Enum(_) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedEnum,
                span: statement.span,
            }),
            ast::StatementKind::Function(_) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedFunctionStmt,
                span: statement.span,
            }),
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
            ast::StatementKind::Repeat(repeat_stmt) => self.repeat_statement(repeat_stmt),
            ast::StatementKind::Switch(switch_stmt) => self.switch_statement(switch_stmt),
            ast::StatementKind::Call(function_call) => {
                let _ = self.call_expr(statement.span, function_call)?;
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

    fn repeat_statement(
        &mut self,
        repeat_stmt: &ast::RepeatStatement<S>,
    ) -> Result<(), IrGenError> {
        let times = self.expression(&repeat_stmt.times)?;
        let one = self.push_instruction(
            Span::null(),
            ir::Instruction::Constant(Constant::Integer(1)),
        );

        let dec_var = self.function.variables.insert(ir::Variable::Owned);
        self.push_instruction(
            repeat_stmt.times.span,
            ir::Instruction::OpenVariable(dec_var),
        );
        self.push_instruction(
            repeat_stmt.times.span,
            ir::Instruction::SetVariable(dec_var, times),
        );

        let cond_block = self.new_block();
        let body_block = self.new_block();
        let successor_block = self.new_block();

        self.push_break_target(successor_block);
        self.push_continue_target(cond_block);

        self.end_current_block(ir::Exit::Jump(cond_block));
        self.start_new_block(cond_block);

        let prev = self.push_instruction(
            repeat_stmt.times.span,
            ir::Instruction::GetVariable(dec_var),
        );
        self.end_current_block(ir::Exit::Branch {
            cond: prev,
            if_true: body_block,
            if_false: successor_block,
        });

        self.start_new_block(body_block);

        let dec = self.push_instruction(
            repeat_stmt.times.span,
            ir::Instruction::BinOp {
                left: prev,
                op: ir::BinOp::Sub,
                right: one,
            },
        );
        self.push_instruction(
            repeat_stmt.times.span,
            ir::Instruction::SetVariable(dec_var, dec),
        );

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
                case.compare.span,
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
            ast::ExpressionKind::Ident(s) => self.ident_expr(span, s)?,
            ast::ExpressionKind::Global => self.push_instruction(span, ir::Instruction::Globals),
            ast::ExpressionKind::This => self.push_instruction(span, ir::Instruction::This),
            ast::ExpressionKind::Other => self.push_instruction(span, ir::Instruction::Other),
            ast::ExpressionKind::Group(expr) => self.expression(expr)?,
            ast::ExpressionKind::Object(fields) => {
                let object = self.push_instruction(span, ir::Instruction::NewObject);

                for field in fields {
                    match field {
                        ast::Field::Value(name, value) => {
                            let this_scope = if matches!(
                                value.kind.as_ref(),
                                ast::ExpressionKind::Function(_)
                            ) {
                                // Within a struct literal, closures always bind `self` to the
                                // struct currently being created.
                                let this_scope = self.function.this_scopes.insert(());
                                self.push_instruction(
                                    span,
                                    ir::Instruction::OpenThisScope(this_scope),
                                );
                                self.push_instruction(
                                    span,
                                    ir::Instruction::SetThis(this_scope, object),
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
                                    key: Constant::String(name.inner.clone()),
                                    value,
                                },
                            );

                            if let Some(this_scope) = this_scope {
                                self.push_instruction(
                                    span,
                                    ir::Instruction::CloseThisScope(this_scope),
                                );
                            }
                        }
                        ast::Field::Init(name) => {
                            let value = self.ident_expr(name.span, name)?;
                            self.push_instruction(
                                span,
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
            ast::ExpressionKind::Call(func) => self.call_expr(span, func)?,
            ast::ExpressionKind::Field(field_expr) => {
                let base = self.expression(&field_expr.base)?;
                let field = self.push_instruction(
                    span,
                    ir::Instruction::Constant(Constant::String(field_expr.field.inner.clone())),
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
                let mut indexes = Vec::new();
                for index in &index_expr.indexes {
                    indexes.push(self.expression(index)?);
                }
                self.push_instruction(
                    span,
                    ir::Instruction::GetIndex {
                        array: base,
                        indexes,
                    },
                )
            }
        })
    }

    fn call_expr(&mut self, span: Span, func: &ast::Call<S>) -> Result<ir::InstId, IrGenError> {
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
                ir::Instruction::Constant(Constant::String(field_expr.field.inner.clone())),
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

        let (func, this) = match call {
            Call::Function(func) => (func, None),
            Call::Method { func, object } => (func, Some(object)),
        };

        let call_scope = self.function.call_scopes.insert(());

        let this_scope = if let Some(this) = this {
            let this_scope = self.function.this_scopes.insert(());
            self.push_instruction(span, ir::Instruction::OpenThisScope(this_scope));
            self.push_instruction(span, ir::Instruction::SetThis(this_scope, this));
            Some(this_scope)
        } else {
            None
        };

        self.push_instruction(
            span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func,
                args,
            },
        );

        let ret = self.push_instruction(span, ir::Instruction::GetReturn(call_scope, 0));
        self.push_instruction(span, ir::Instruction::CloseCall(call_scope));

        if let Some(this_scope) = this_scope {
            self.push_instruction(span, ir::Instruction::CloseThisScope(this_scope));
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

    fn ident_expr(&mut self, span: Span, ident: &ast::Ident<S>) -> Result<ir::InstId, IrGenError> {
        Ok(if let Some(var_id) = self.get_var(span, ident)? {
            self.push_instruction(span, ir::Instruction::GetVariable(var_id))
        } else if (self.find_magic)(ident).is_some() {
            self.push_instruction(span, ir::Instruction::GetMagic(ident.inner.clone()))
        } else {
            let this = self.push_instruction(span, ir::Instruction::This);
            let key = self.push_instruction(
                span,
                ir::Instruction::Constant(Constant::String(ident.inner.clone())),
            );
            self.push_instruction(span, ir::Instruction::GetField { object: this, key })
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
            ast::MutableExpr::Ident(ident) => {
                if let Some(var_id) = self.get_var(span, ident)? {
                    MutableTarget::Var(var_id)
                } else if let Some(mode) = (self.find_magic)(ident) {
                    if mode == MagicMode::ReadOnly {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ReadOnlyMagic,
                            span,
                        });
                    }

                    MutableTarget::Magic(ident.inner.clone())
                } else {
                    let key = self.push_instruction(
                        span,
                        ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                    );
                    MutableTarget::This { key }
                }
            }
            ast::MutableExpr::Field(field_expr) => {
                let object = self.expression(&field_expr.base)?;
                let key = self.push_instruction(
                    span,
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
                    let vname_span = vname.span;
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
                                vname_span,
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
    fn declare_var(
        &mut self,
        vname: ast::Ident<S>,
        var: Option<ir::Variable<S>>,
    ) -> Option<ir::VarId> {
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
