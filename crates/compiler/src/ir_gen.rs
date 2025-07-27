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
    #[error("function not allowed to have a return statement with an argument")]
    CannotReturnValue,
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
        Ok(compiler.function)
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
        Ok(compiler.function)
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

struct FunctionCompiler<'a, S> {
    settings: IrGenSettings,
    find_magic: &'a dyn (Fn(&S) -> Option<MagicMode>),

    function: ir::Function<S>,
    current_block: ir::BlockId,
    // If we have a final block to jump to, jump here instead of returning. Setting this disallows
    // return statements with values.
    final_block: Option<ir::BlockId>,

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
            current_block: first_block,
            final_block: None,
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
                        right: arg_count,
                        op: ir::BinOp::LessThan,
                    },
                );

                let provided_block = self.function.blocks.insert(ir::Block::default());
                let not_provided_block = self.function.blocks.insert(ir::Block::default());
                let successor_block = self.function.blocks.insert(ir::Block::default());

                self.function.blocks[self.current_block].exit = ir::Exit::Branch {
                    cond: arg_provided,
                    if_true: provided_block,
                    if_false: not_provided_block,
                };

                self.current_block = provided_block;
                {
                    let pop_arg_inst =
                        self.push_instruction(Span::null(), ir::Instruction::Argument(param_index));
                    self.push_instruction(
                        Span::null(),
                        ir::Instruction::SetVariable(arg_var, pop_arg_inst),
                    );
                }
                self.function.blocks[self.current_block].exit = ir::Exit::Jump(successor_block);

                self.current_block = not_provided_block;
                {
                    let def_value_inst =
                        self.push_instruction(Span::null(), ir::Instruction::Constant(def_value));
                    self.push_instruction(
                        Span::null(),
                        ir::Instruction::SetVariable(arg_var, def_value_inst),
                    );
                }
                self.function.blocks[self.current_block].exit = ir::Exit::Jump(successor_block);

                self.current_block = successor_block;
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

        let init_block = self.function.blocks.insert(ir::Block::default());
        let successor_block = self.function.blocks.insert(ir::Block::default());
        let final_block = self.function.blocks.insert(ir::Block::default());
        self.final_block = Some(final_block);

        let check_initialized =
            self.push_instruction(Span::null(), ir::Instruction::GetVariable(is_initialized));
        self.function.blocks[self.current_block].exit = ir::Exit::Branch {
            cond: check_initialized,
            if_false: init_block,
            if_true: successor_block,
        };

        self.function.blocks[self.current_block].exit = ir::Exit::Jump(init_block);
        self.current_block = init_block;

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

        let mut parent_func = None;

        if let Some((_, inherit)) = inherit {
            let parent = self.commit_expression(&inherit.base)?;

            let parent_super = self.push_instruction(
                Span::null(),
                ir::Instruction::Call {
                    func: get_constructor_super,
                    this: None,
                    args: vec![parent],
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

            parent_func = Some(parent);
        }

        let mut static_names = HashSet::new();

        for stmt in &main_block.statements {
            if let ast::StatementKind::Static(static_decl) = &*stmt.kind {
                if !static_names.insert(&static_decl.name) {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotUnique,
                        span: stmt.span,
                    });
                }

                let key = self.push_instruction(
                    stmt.span,
                    ir::Instruction::Constant(Constant::String(static_decl.name.clone())),
                );
                let value = self.commit_expression(&static_decl.value)?;

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

        let true_ = self.push_instruction(
            Span::null(),
            ir::Instruction::Constant(Constant::Boolean(true)),
        );
        self.push_instruction(
            Span::null(),
            ir::Instruction::SetVariable(is_initialized, true_),
        );

        self.function.blocks[init_block].exit = ir::Exit::Jump(successor_block);
        self.current_block = successor_block;

        let this = if let Some(parent_func) = parent_func {
            let Some((inherit_span, inherit)) = inherit else {
                unreachable!();
            };

            let mut args = Vec::new();
            for arg in &inherit.arguments {
                args.push(self.commit_expression(arg)?);
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

        {
            self.push_scope();

            for stmt in &main_block.statements {
                match &*stmt.kind {
                    ast::StatementKind::Static(declaration) => {
                        // Declare a pseudo-variable when encountering the static statement.
                        // This variable's sole purpose is to prevent accessing a constructor
                        // `static`, which *looks* like a variable but is not usable as a variable
                        // in expressions.
                        self.declare_var(declaration.name.clone(), None);
                    }
                    _ => {
                        self.statement(stmt)?;
                    }
                }
            }

            self.pop_scope();
        }

        self.function.blocks[self.current_block].exit = ir::Exit::Jump(final_block);
        self.current_block = final_block;

        self.function.blocks[self.current_block].exit = ir::Exit::Return { value: Some(this) };

        Ok(())
    }

    fn block(&mut self, block: &ast::Block<S>) -> Result<(), IrGenError> {
        {
            self.push_scope();

            for statement in &block.statements {
                self.statement(statement)?;
            }

            self.pop_scope();
        }
        Ok(())
    }

    fn statement(&mut self, statement: &ast::Statement<S>) -> Result<(), IrGenError> {
        match &*statement.kind {
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
            ast::StatementKind::Var(var_decl) => self.var_statement(statement.span, var_decl),
            ast::StatementKind::Static(static_decl) => {
                if self.function.is_constructor {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotTopLevel,
                        span: statement.span,
                    });
                } else {
                    self.static_statement(statement.span, static_decl)
                }
            }
            ast::StatementKind::Assignment(assignment_statement) => {
                self.assignment_statement(statement.span, assignment_statement)
            }
            ast::StatementKind::Return(return_) => self.return_statement(statement.span, return_),
            ast::StatementKind::If(if_statement) => self.if_statement(if_statement),
            ast::StatementKind::For(for_statement) => self.for_statement(for_statement),
            ast::StatementKind::Block(block) => self.block(block),
            ast::StatementKind::Call(function_call) => {
                let _ = self.call_expr(statement.span, function_call, false)?;
                Ok(())
            }
        }
    }

    fn var_statement(
        &mut self,
        span: Span,
        var_decl: &ast::Declaration<S>,
    ) -> Result<(), IrGenError> {
        let var_id = self
            .declare_var(var_decl.name.clone(), Some(ir::Variable::Owned))
            .unwrap();
        let inst_id = self.commit_expression(&var_decl.value)?;
        self.push_instruction(span, ir::Instruction::SetVariable(var_id, inst_id));
        Ok(())
    }

    fn static_statement(
        &mut self,
        span: Span,
        static_decl: &ast::Declaration<S>,
    ) -> Result<(), IrGenError> {
        if let Some(constant) = static_decl.value.clone().fold_constant() {
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

            let init_block = self.function.blocks.insert(ir::Block::default());
            let successor = self.function.blocks.insert(ir::Block::default());

            let check_initialized =
                self.push_instruction(span, ir::Instruction::GetVariable(is_initialized));
            self.function.blocks[self.current_block].exit = ir::Exit::Branch {
                cond: check_initialized,
                if_false: init_block,
                if_true: successor,
            };

            self.current_block = init_block;

            let value = self.commit_expression(&static_decl.value)?;
            self.push_instruction(span, ir::Instruction::SetVariable(var_id, value));
            let true_ =
                self.push_instruction(span, ir::Instruction::Constant(Constant::Boolean(true)));
            self.push_instruction(span, ir::Instruction::SetVariable(is_initialized, true_));
            self.function.blocks[init_block].exit = ir::Exit::Jump(successor);

            self.current_block = successor;
        }

        Ok(())
    }

    fn assignment_statement(
        &mut self,
        span: Span,
        assignment_statement: &ast::AssignmentStatement<S>,
    ) -> Result<(), IrGenError> {
        enum Target<S> {
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

        let this = self.push_instruction(span, ir::Instruction::This);

        let (target, old) = match &assignment_statement.target {
            ast::AssignmentTarget::Name(name) => {
                if let Some(var_id) = self.get_var(span, name)? {
                    (Target::Var(var_id), ir::Instruction::GetVariable(var_id))
                } else if let Some(mode) = (self.find_magic)(name) {
                    if mode == MagicMode::ReadOnly {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ReadOnlyMagic,
                            span,
                        });
                    }
                    (
                        Target::Magic(name.clone()),
                        ir::Instruction::GetMagic(name.clone()),
                    )
                } else {
                    let key = self.push_instruction(
                        span,
                        ir::Instruction::Constant(Constant::String(name.clone())),
                    );
                    (
                        Target::This { key },
                        ir::Instruction::GetField { object: this, key },
                    )
                }
            }
            ast::AssignmentTarget::Field(field_expr) => {
                let object = self.commit_expression(&field_expr.base)?;
                let key = self.push_instruction(
                    span,
                    ir::Instruction::Constant(Constant::String(field_expr.field.clone())),
                );
                (
                    Target::Field { object, key },
                    ir::Instruction::GetField { object, key },
                )
            }
            ast::AssignmentTarget::Index(index_expr) => {
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
            ast::AssignmentOp::Equal => val,
            ast::AssignmentOp::PlusEqual => {
                let old = self.push_instruction(span, old);
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: old,
                        right: val,
                        op: ir::BinOp::Add,
                    },
                )
            }
            ast::AssignmentOp::MinusEqual => {
                let old = self.push_instruction(span, old);
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: old,
                        right: val,
                        op: ir::BinOp::Sub,
                    },
                )
            }
            ast::AssignmentOp::MultEqual => {
                let old = self.push_instruction(span, old);
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: old,
                        right: val,
                        op: ir::BinOp::Mult,
                    },
                )
            }
            ast::AssignmentOp::DivEqual => {
                let old = self.push_instruction(span, old);
                self.push_instruction(
                    span,
                    ir::Instruction::BinOp {
                        left: old,
                        right: val,
                        op: ir::BinOp::Div,
                    },
                )
            }
        };

        match target {
            Target::Var(var_id) => {
                self.push_instruction(span, ir::Instruction::SetVariable(var_id, assign));
            }
            Target::This { key } => {
                self.push_instruction(
                    span,
                    ir::Instruction::SetField {
                        object: this,
                        key,
                        value: assign,
                    },
                );
            }
            Target::Field { object, key } => {
                self.push_instruction(
                    span,
                    ir::Instruction::SetField {
                        object,
                        key,
                        value: assign,
                    },
                );
            }
            Target::Index { array, index } => {
                self.push_instruction(
                    span,
                    ir::Instruction::SetIndex {
                        array,
                        index,
                        value: assign,
                    },
                );
            }
            Target::Magic(magic) => {
                self.push_instruction(span, ir::Instruction::SetMagic(magic, assign));
            }
        }

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
            let val = self.commit_expression(value)?;
            ir::Exit::Return { value: Some(val) }
        } else {
            ir::Exit::Return { value: None }
        };
        self.function.blocks[self.current_block].exit = exit;
        self.current_block = self.function.blocks.insert(ir::Block::default());
        Ok(())
    }

    fn if_statement(&mut self, if_statement: &ast::IfStatement<S>) -> Result<(), IrGenError> {
        let cond = self.commit_expression(&if_statement.condition)?;
        let then_block = self.function.blocks.insert(ir::Block::default());
        let else_block = self.function.blocks.insert(ir::Block::default());
        let successor = self.function.blocks.insert(ir::Block::default());

        self.function.blocks[self.current_block].exit = ir::Exit::Branch {
            cond,
            if_true: then_block,
            if_false: else_block,
        };

        self.current_block = then_block;
        {
            self.push_scope();
            self.statement(&if_statement.then_stmt)?;
            self.pop_scope();
        }
        self.function.blocks[self.current_block].exit = ir::Exit::Jump(successor);

        self.current_block = else_block;
        if let Some(else_stmt) = &if_statement.else_stmt {
            {
                self.push_scope();
                self.statement(else_stmt)?;
                self.pop_scope();
            }
        }
        self.function.blocks[self.current_block].exit = ir::Exit::Jump(successor);

        self.current_block = successor;
        Ok(())
    }

    fn for_statement(&mut self, for_statement: &ast::ForStatement<S>) -> Result<(), IrGenError> {
        {
            self.push_scope();

            let body = self.function.blocks.insert(ir::Block::default());
            let successor = self.function.blocks.insert(ir::Block::default());

            self.statement(&for_statement.initializer)?;
            let cond = self.commit_expression(&for_statement.condition)?;

            self.function.blocks[self.current_block].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };
            self.current_block = body;

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
            self.function.blocks[self.current_block].exit = ir::Exit::Branch {
                cond,
                if_true: body,
                if_false: successor,
            };

            self.current_block = successor;

            self.pop_scope();
        }

        Ok(())
    }

    fn commit_expression(&mut self, expr: &ast::Expression<S>) -> Result<ir::InstId, IrGenError> {
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
            ast::ExpressionKind::Group(expr) => self.commit_expression(expr)?,
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

                    let value = self.commit_expression(value)?;
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
                    let value = self.commit_expression(value)?;
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
                    source: self.commit_expression(expr)?,
                    op: match op {
                        ast::UnaryOp::Not => ir::UnOp::Not,
                        ast::UnaryOp::Minus => ir::UnOp::Neg,
                    },
                };
                self.push_instruction(span, inst)
            }
            ast::ExpressionKind::Binary(left, op, right) => {
                let left = self.commit_expression(left)?;
                let right = self.commit_expression(right)?;
                let op = match op {
                    ast::BinaryOp::Add => ir::BinOp::Add,
                    ast::BinaryOp::Sub => ir::BinOp::Sub,
                    ast::BinaryOp::Mult => ir::BinOp::Mult,
                    ast::BinaryOp::Div => ir::BinOp::Div,
                    ast::BinaryOp::Equal => ir::BinOp::Equal,
                    ast::BinaryOp::NotEqual => ir::BinOp::NotEqual,
                    ast::BinaryOp::LessThan => ir::BinOp::LessThan,
                    ast::BinaryOp::LessEqual => ir::BinOp::LessEqual,
                    ast::BinaryOp::GreaterThan => ir::BinOp::GreaterThan,
                    ast::BinaryOp::GreaterEqual => ir::BinOp::GreaterEqual,
                    ast::BinaryOp::And => ir::BinOp::And,
                    ast::BinaryOp::Or => ir::BinOp::Or,
                };
                self.push_instruction(span, ir::Instruction::BinOp { left, right, op })
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
                let base = self.commit_expression(&field_expr.base)?;
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
                let base = self.commit_expression(&index_expr.base)?;
                let index = self.commit_expression(&index_expr.index)?;
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
            let object = self.commit_expression(&field_expr.base)?;
            let key = self.push_instruction(
                func.base.span,
                ir::Instruction::Constant(Constant::String(field_expr.field.clone())),
            );
            let func =
                self.push_instruction(func.base.span, ir::Instruction::GetField { object, key });
            Call::Method { func, object }
        } else {
            Call::Function(self.commit_expression(&func.base)?)
        };

        let mut args = Vec::new();
        for arg in &func.arguments {
            args.push(self.commit_expression(arg)?);
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
                        if self.function.variables[var_id].is_owned() {
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

    fn push_instruction(&mut self, span: Span, inst: ir::Instruction<S>) -> ir::InstId {
        let inst_id = self.function.instructions.insert(inst);
        self.function.blocks[self.current_block]
            .instructions
            .push(inst_id);
        if !span.is_null() {
            self.function.spans.insert(inst_id, span);
        }
        inst_id
    }
}
