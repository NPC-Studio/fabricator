use std::{
    collections::{HashMap, HashSet, hash_map},
    hash::Hash,
};

use fabricator_vm::{BuiltIns, FunctionRef, RefName, Span};
use thiserror::Error;

use crate::{ast, constant::Constant, ir, string_interner::StringInterner};

pub enum FreeVarMode {
    /// Free variable name is interpreted as an accessor to the implicit `self`.
    ///
    /// This should be the default.
    This,
    /// Free variable name is always interpreted as a global variable.
    GlobalVar,
    /// Free variable name is a magic variable.
    Magic {
        /// If true, then it is only permitted to read from this magic variable.
        is_read_only: bool,
    },
}

pub trait VarDict<S> {
    /// Should return true if ir-gen should permit this name for a variable or parameter
    /// declaration.
    ///
    /// Return false for names which have other meanings which should not be allowed to be shadowed.
    fn permit_declaration(&self, name: &S) -> bool;

    /// Return the type of free variable for the given identifier.
    fn free_var_mode(&self, ident: &S) -> FreeVarMode;
}

#[derive(Debug, Error)]
pub enum IrGenErrorKind {
    #[error("export statements are only allowed at the top-level")]
    MisplacedExport,
    #[error("constructor functions are not permitted")]
    ConstructorsNotAllowed,
    #[error("try / catch blocks are not permitted")]
    TryCatchNotAllowed,
    #[error("declaration with the given name is not permitted")]
    DeclarationNotPermitted,
    #[error("assignment to read-only magic value")]
    ReadOnlyMagic,
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
    pub closures: bool,

    /// Allow `constructor` annotated function statements with optional inheritance.
    ///
    /// These desugar to the equivalent manual implementation creating a new `this` object and a
    /// static variable holding the shared super object.
    ///
    /// All `static` variables must be declared at the top level of the function with unique names,
    /// and are interpreted as fields on a shared super object. Declaring a `constructor` function
    /// *disables* normal function statics, and all statics not at the top-level trigger errors.
    /// Though these variables are synthetic, they may be referenced as real variables, but they
    /// desugar to accessing a named variable in the super table.
    ///
    /// This option is included for compatibility purposes only, it is more straightforward to
    /// handle constructors and inheritance manually, and doing so does not disable normal static
    /// variables.
    pub allow_constructors: bool,

    /// Allow `function` statements not at the top-level of a script file which do *not* act as
    /// an export. Instead, they are interpreted as `self.func_name = function(...) { ... }`.
    pub allow_non_top_level_func_stmt: bool,

    /// Allow `try {} catch(e) {}` blocks.
    ///
    /// These desugar to the equivalent of creating an inner closure and calling it with `pcall`.
    /// Inside the `try` block, the difference between a normal exit, a thrown error, and `break` /
    /// `continue` / `return` are handled by setting a flag which is checked at `pcall` exit.
    ///
    ///
    pub allow_try_catch_blocks: bool,
}

impl IrGenSettings {
    pub fn modern() -> Self {
        IrGenSettings {
            block_scoping: true,
            closures: true,
            allow_constructors: false,
            allow_non_top_level_func_stmt: false,
            allow_try_catch_blocks: false,
        }
    }

    pub fn compat() -> Self {
        IrGenSettings {
            block_scoping: false,
            closures: false,
            allow_constructors: true,
            allow_non_top_level_func_stmt: true,
            allow_try_catch_blocks: true,
        }
    }

    pub fn gen_chunk_ir<S>(
        self,
        interner: &mut dyn StringInterner<String = S>,
        block: &ast::Block<S>,
        var_dict: &dyn VarDict<S>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone + AsRef<str>,
    {
        let mut compiler = FunctionCompiler::new(self, interner, FunctionRef::Chunk, var_dict);
        compiler.block(block)?;
        Ok(compiler.finish())
    }

    pub fn gen_func_stmt_ir<S>(
        self,
        interner: &mut dyn StringInterner<String = S>,
        func_stmt: &ast::FunctionStmt<S>,
        var_dict: &dyn VarDict<S>,
    ) -> Result<ir::Function<S>, IrGenError>
    where
        S: Eq + Hash + Clone + AsRef<str>,
    {
        let mut compiler = FunctionCompiler::new(
            self,
            interner,
            FunctionRef::Named(RefName::new(func_stmt.name.as_ref()), func_stmt.span),
            var_dict,
        );
        compiler.declare_parameters(&func_stmt.parameters)?;
        if func_stmt.is_constructor {
            if !self.allow_constructors {
                return Err(IrGenError {
                    kind: IrGenErrorKind::ConstructorsNotAllowed,
                    span: func_stmt.span,
                });
            }
            compiler.constructor(func_stmt.inherit.as_ref(), &func_stmt.body)
        } else {
            compiler.block(&func_stmt.body)?;
            Ok(compiler.finish())
        }
    }
}

struct FunctionCompiler<'a, S> {
    settings: IrGenSettings,
    interner: &'a mut dyn StringInterner<String = S>,
    var_dict: &'a dyn VarDict<S>,

    function: ir::Function<S>,

    func_type: FunctionType,

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

#[derive(Debug, Copy, Clone)]
enum TryCatchExitCode {
    /// Return a value from the outer function.
    Return = 0,
    /// Break an outer loop.
    Break = 1,
    /// Continue an outer loop.
    Continue = 2,
}

#[derive(Debug, Copy, Clone)]
enum FunctionType {
    /// The function is a normal function.
    Normal,
    /// All function returns, including the end of the function, implicitly return the `this` value.
    /// No function return may return an explicit value.
    Constructor {
        this: ir::InstId,
        parent: ir::InstId,
    },
    /// The function is a desugared `try {} catch(e) {}` block.
    ///
    /// The function must set a special variable to a value of `TryCatchReturnCode` before
    /// returning.
    TryCatch {
        /// Allow top-level `break` within the function and desugar this into setting the `Break`
        /// return code followed by a return.
        allow_break: bool,
        /// Allow top-level `continue` within the function and desugar this into setting the
        /// `Continue` return code followed by a return.
        allow_continue: bool,
        /// The variable which must hold the `TryCatchReturnCode`. If the variable is not set, then
        /// the outer function will always proceed with normal execution.
        exit_code: ir::VarId,
    },
}

#[derive(Debug, Clone)]
enum VarType<S> {
    /// This variable is a normal IR variable.
    Normal(ir::Variable<S>),
    /// This variable is a constructor static and is inside the `parent` object of a constructor
    /// under the stored field name.
    ConstructorStatic(S),
}

impl<S> From<ir::Variable<S>> for VarType<S> {
    fn from(var: ir::Variable<S>) -> Self {
        VarType::Normal(var)
    }
}

#[derive(Debug, Clone)]
enum VarDecl<S> {
    Normal(ir::VarId),
    ConstructorStatic(S),
}

struct Scope<S> {
    // Variables to close when this scope ends. May include shadowed variables.
    to_close: Vec<ir::VarId>,

    // All variable declarations for this scope which have not been shadowed. When a new declaration
    // shadows another, the new declaration will replace the old in this map.
    visible: HashMap<ast::Ident<S>, VarDecl<S>>,
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

#[derive(Debug, Clone)]
enum MutableTarget<S> {
    Var(VarDecl<S>),
    This {
        key: ir::InstId,
    },
    Globals {
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

impl<'a, S> FunctionCompiler<'a, S>
where
    S: Eq + Hash + Clone + AsRef<str>,
{
    fn new(
        settings: IrGenSettings,
        interner: &'a mut dyn StringInterner<String = S>,
        reference: FunctionRef,
        var_dict: &'a dyn VarDict<S>,
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
            var_dict,
            function,
            func_type: FunctionType::Normal,
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
            let arg_var = self.declare_var(param.name.clone(), ir::Variable::Owned.into())?;

            let mut value =
                self.push_instruction(param.span, ir::Instruction::FixedArgument(param_index));

            if let Some(default) = &param.default {
                let cond = self.push_instruction(
                    param.span,
                    ir::Instruction::UnOp {
                        op: ir::UnOp::IsUndefined,
                        source: value,
                    },
                );

                value = self.if_expr(
                    param.span,
                    cond,
                    |this| this.expression(default),
                    |_| Ok(value),
                )?;
            };

            self.set_var(param.name.span, arg_var, value);
        }

        Ok(())
    }

    fn constructor(
        mut self,
        inherit: Option<&ast::Call<S>>,
        body: &ast::Block<S>,
    ) -> Result<ir::Function<S>, IrGenError> {
        // We need the `init_constructor_super`, `get_constructor_super`, and `set_super`
        // intrinsics.

        let init_constructor_super_name = self.interner.intern(BuiltIns::INIT_CONSTRUCTOR_SUPER);
        let init_constructor_super = self.push_instruction(
            body.span,
            ir::Instruction::GetMagic(init_constructor_super_name),
        );

        let get_constructor_super_name = self.interner.intern(BuiltIns::GET_CONSTRUCTOR_SUPER);
        let get_constructor_super = self.push_instruction(
            body.span,
            ir::Instruction::GetMagic(get_constructor_super_name),
        );

        let set_super_name = self.interner.intern(BuiltIns::SET_SUPER);
        let set_super = self.push_instruction(body.span, ir::Instruction::GetMagic(set_super_name));

        // First, get this prototype's super object.

        let our_closure = self.push_instruction(body.span, ir::Instruction::CurrentClosure);

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            body.span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: init_constructor_super,
                args: vec![our_closure],
            },
        );

        let our_super =
            self.push_instruction(body.span, ir::Instruction::FixedReturn(call_scope, 0));
        self.push_instruction(body.span, ir::Instruction::CloseCall(call_scope));

        let inherit_func = if let Some(inherit) = inherit {
            Some(self.expression(&inherit.base)?)
        } else {
            None
        };

        // Create a new `self` object, if we are inheriting from another constructor, then use that
        // constructor's return value as the `self`.

        let this = if let Some(inherit_func) = inherit_func {
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
                    func: inherit_func,
                    args,
                },
            );
            let ret =
                self.push_instruction(inherit.span, ir::Instruction::FixedReturn(call_scope, 0));
            self.push_instruction(inherit.span, ir::Instruction::CloseCall(call_scope));
            ret
        } else {
            self.push_instruction(body.span, ir::Instruction::NewObject)
        };

        // Set the super-table as the parent object for the `self` value.

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            body.span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: set_super,
                args: vec![this, our_super],
            },
        );
        self.push_instruction(body.span, ir::Instruction::CloseCall(call_scope));

        // We must set this up early, because constructor statics may rely on each other, as long as
        // it is in-order.

        self.func_type = FunctionType::Constructor {
            this,
            parent: our_super,
        };

        let init_block = self.new_block();
        let main_block = self.new_block();

        let our_super_is_initialized = self
            .function
            .variables
            .insert(ir::Variable::Static(Constant::Boolean(false)));

        let check_initialized = self.push_instruction(
            body.span,
            ir::Instruction::GetVariable(our_super_is_initialized),
        );
        self.end_current_block(ir::Exit::Branch {
            cond: check_initialized,
            if_true: main_block,
            if_false: init_block,
        });

        self.start_new_block(init_block);

        if inherit.is_some() {
            // We're explicitly allowing inheriting from a non-constructor here, if there is no
            // initialized inherited super object, then this constructor will not have a super-super
            // object.

            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                body.span,
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: get_constructor_super,
                    args: vec![inherit_func.unwrap()],
                },
            );
            let inherited_super =
                self.push_instruction(body.span, ir::Instruction::FixedReturn(call_scope, 0));
            self.push_instruction(body.span, ir::Instruction::CloseCall(call_scope));

            let call_scope = self.function.call_scopes.insert(());
            self.push_instruction(
                body.span,
                ir::Instruction::OpenCall {
                    scope: call_scope,
                    func: set_super,
                    args: vec![our_super, inherited_super],
                },
            );
            self.push_instruction(body.span, ir::Instruction::CloseCall(call_scope));
        }

        let mut static_names = HashSet::new();

        for stmt in &body.statements {
            match stmt {
                ast::Statement::Static(decls) => {
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

                        self.declare_var(
                            decl_name.clone(),
                            VarType::ConstructorStatic(decl_name.inner.clone()),
                        )?;
                    }
                }
                _ => {}
            }
        }

        let true_ = self.push_instruction(body.span, ir::Instruction::Boolean(true));
        self.push_instruction(
            body.span,
            ir::Instruction::SetVariable(our_super_is_initialized, true_),
        );

        self.end_current_block(ir::Exit::Jump(main_block));

        self.start_new_block(main_block);

        self.push_scope();

        // Our whole constructor body exists inside a single "this" scope.

        let this_scope = self.function.this_scopes.insert(());
        self.push_instruction(body.span, ir::Instruction::OpenThisScope(this_scope));
        self.push_instruction(body.span, ir::Instruction::SetThis(this_scope, this));

        for stmt in &body.statements {
            match stmt {
                ast::Statement::Static(_) => {
                    // We have already handled all static statements.
                }
                _ => {
                    self.statement(stmt)?;
                }
            }
        }

        self.pop_scope();
        self.end_current_block(ir::Exit::Return { value: Some(this) });

        Ok(self.finish())
    }

    fn finish(mut self) -> ir::Function<S> {
        self.end_current_block(ir::Exit::Return { value: None });

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
        match statement {
            ast::Statement::Empty(_) => Ok(()),
            ast::Statement::Block(block_stmt) => self.block(&block_stmt.block),
            ast::Statement::Enum(enum_stmt) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedExport,
                span: enum_stmt.span,
            }),
            ast::Statement::Function(func_stmt) => {
                if !self.settings.allow_non_top_level_func_stmt {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::MisplacedExport,
                        span: func_stmt.span,
                    });
                }

                let allow_constructors = self.settings.allow_constructors;
                let mut compiler = self.start_inner_function(FunctionRef::Named(
                    RefName::new(func_stmt.name.as_ref()),
                    func_stmt.span,
                ));

                compiler.declare_parameters(&func_stmt.parameters)?;
                let function = if func_stmt.is_constructor {
                    if !allow_constructors {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ConstructorsNotAllowed,
                            span: func_stmt.span,
                        });
                    }
                    compiler.constructor(func_stmt.inherit.as_ref(), &func_stmt.body)?
                } else {
                    compiler.block(&func_stmt.body)?;
                    compiler.finish()
                };

                let func_id = self.function.functions.insert(function);
                let func = self.push_instruction(func_stmt.span, ir::Instruction::Closure(func_id));

                let this = self.push_instruction(func_stmt.span, ir::Instruction::This);
                let key = self.push_instruction(
                    func_stmt.span,
                    ir::Instruction::Constant(Constant::String(func_stmt.name.inner.clone())),
                );
                self.push_instruction(
                    func_stmt.span,
                    ir::Instruction::SetField {
                        object: this,
                        key,
                        value: func,
                    },
                );
                Ok(())
            }
            ast::Statement::Var(var_decls) => {
                for (name, value) in &var_decls.vars {
                    self.var_decl(var_decls.span, name, value.as_ref())?;
                }
                Ok(())
            }
            ast::Statement::Static(static_decls) => {
                if matches!(self.func_type, FunctionType::Constructor { .. }) {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::ConstructorStaticNotTopLevel,
                        span: static_decls.span,
                    });
                }

                for (name, value) in &static_decls.vars {
                    self.static_decl(static_decls.span, name, value.as_ref())?;
                }
                Ok(())
            }
            ast::Statement::GlobalVar(ident) => Err(IrGenError {
                kind: IrGenErrorKind::MisplacedExport,
                span: ident.span,
            }),
            ast::Statement::Assignment(assignment_statement) => {
                self.assignment_stmt(assignment_statement)
            }
            ast::Statement::Return(return_stmt) => {
                let value = if let Some(value) = &return_stmt.value {
                    Some(self.expression(value)?)
                } else {
                    None
                };
                self.do_return(return_stmt.span, value)
            }
            ast::Statement::If(if_stmt) => self.if_stmt(if_stmt),
            ast::Statement::For(for_stmt) => self.for_stmt(for_stmt),
            ast::Statement::While(while_stmt) => self.while_stmt(while_stmt),
            ast::Statement::Repeat(repeat_stmt) => self.repeat_stmt(repeat_stmt),
            ast::Statement::Switch(switch_stmt) => self.switch_stmt(switch_stmt),
            ast::Statement::With(with_stmt) => self.with_stmt(with_stmt),
            ast::Statement::TryCatch(try_catch_stmt) => self.try_catch_stmt(try_catch_stmt),
            ast::Statement::Throw(throw_stmt) => {
                let target = self.expression(&throw_stmt.target)?;
                self.end_current_block(ir::Exit::Throw(target));
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
            ast::Statement::Break(span) => self.do_break(*span),
            ast::Statement::Continue(span) => self.do_continue(*span),
        }
    }

    fn var_decl(
        &mut self,
        span: Span,
        name: &ast::Ident<S>,
        value: Option<&ast::Expression<S>>,
    ) -> Result<(), IrGenError> {
        let var = self.declare_var(name.clone(), ir::Variable::Owned.into())?;
        if let Some(value) = value {
            let inst_id = self.expression(value)?;
            self.set_var(span, var, inst_id);
        }
        Ok(())
    }

    fn static_decl(
        &mut self,
        span: Span,
        name: &ast::Ident<S>,
        value: Option<&ast::Expression<S>>,
    ) -> Result<(), IrGenError> {
        if let Some(value) = value {
            if let Some(constant) = value.clone().fold_constant() {
                // If our static is a constant, then we can just initialize it when the prototype is
                // created.
                self.declare_var(name.clone(), ir::Variable::Static(constant).into())?;
            } else {
                // Otherwise, we need to initialize two static variables, a hidden one for the
                // initialization state and a visible one to hold the initialized value.

                // Create a hidden static variable to hold whether the real static is initialized.
                let is_initialized = self
                    .function
                    .variables
                    .insert(ir::Variable::Static(Constant::Boolean(false)));
                // Create a normal static variable that holds the real value.
                let var = self.declare_var(
                    name.clone(),
                    ir::Variable::Static(Constant::Undefined).into(),
                )?;

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
                self.set_var(span, var, value);
                let true_ = self.push_instruction(span, ir::Instruction::Boolean(true));
                self.push_instruction(span, ir::Instruction::SetVariable(is_initialized, true_));

                self.end_current_block(ir::Exit::Jump(successor));

                self.start_new_block(successor);
            }
        } else {
            // If our static has no value then it is just initialized as `Undefined`.
            self.declare_var(
                name.clone(),
                ir::Variable::Static(Constant::Undefined).into(),
            )?;
        }

        Ok(())
    }

    fn assignment_stmt(&mut self, assign_stmt: &ast::AssignmentStmt<S>) -> Result<(), IrGenError> {
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
            ast::AssignmentOp::RemEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::Rem,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::BitAndEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::BitAnd,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::BitOrEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::BitOr,
                        right: val,
                    },
                )
            }
            ast::AssignmentOp::BitXorEqual => {
                let prev = self.read_mutable_target(assign_stmt.span, target.clone());
                self.push_instruction(
                    assign_stmt.span,
                    ir::Instruction::BinOp {
                        left: prev,
                        op: ir::BinOp::BitXor,
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

    fn if_stmt(&mut self, if_statement: &ast::IfStmt<S>) -> Result<(), IrGenError> {
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
        self.end_current_block(ir::Exit::Jump(successor));
        self.pop_scope();

        self.start_new_block(else_block);
        if let Some(else_stmt) = &if_statement.else_stmt {
            self.push_scope();
            self.statement(else_stmt)?;
            self.pop_scope();
        }
        self.end_current_block(ir::Exit::Jump(successor));

        self.start_new_block(successor);
        Ok(())
    }

    fn for_stmt(&mut self, for_statement: &ast::ForStmt<S>) -> Result<(), IrGenError> {
        self.push_scope();
        self.statement(&for_statement.initializer)?;

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

        self.end_current_block(ir::Exit::Jump(iter_block));
        self.start_new_block(iter_block);

        self.push_scope();
        self.statement(&for_statement.iterator)?;
        self.pop_scope();

        self.end_current_block(ir::Exit::Jump(cond_block));

        self.start_new_block(successor_block);

        self.pop_continue_target(iter_block);
        self.pop_break_target(successor_block);

        self.pop_scope();

        Ok(())
    }

    fn while_stmt(&mut self, while_stmt: &ast::LoopStmt<S>) -> Result<(), IrGenError> {
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

        self.end_current_block(ir::Exit::Jump(cond_block));

        self.start_new_block(successor_block);

        self.pop_continue_target(cond_block);
        self.pop_break_target(successor_block);

        Ok(())
    }

    fn repeat_stmt(&mut self, repeat_stmt: &ast::LoopStmt<S>) -> Result<(), IrGenError> {
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

        self.end_current_block(ir::Exit::Jump(cond_block));

        self.start_new_block(successor_block);

        self.pop_continue_target(cond_block);
        self.pop_break_target(successor_block);

        self.push_instruction(repeat_stmt.span, ir::Instruction::CloseVariable(dec_var));

        Ok(())
    }

    fn switch_stmt(&mut self, switch_stmt: &ast::SwitchStmt<S>) -> Result<(), IrGenError> {
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
            self.end_current_block(ir::Exit::Jump(successor_block));

            self.start_new_block(next_block);
        }

        if let Some(default) = &switch_stmt.default {
            self.block(default)?;
        }

        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(successor_block);
        self.pop_break_target(successor_block);

        Ok(())
    }

    fn with_stmt(&mut self, with_stmt: &ast::LoopStmt<S>) -> Result<(), IrGenError> {
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
            ir::Instruction::FixedReturn(setup_call_scope, 0),
        );
        let init_state = self.push_instruction(
            with_stmt.span,
            ir::Instruction::FixedReturn(setup_call_scope, 1),
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
            ir::Instruction::FixedReturn(iter_call_scope, 0),
        );
        let iter_val = self.push_instruction(
            with_stmt.span,
            ir::Instruction::FixedReturn(iter_call_scope, 1),
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

        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(successor_block);

        self.pop_break_target(successor_block);
        self.pop_continue_target(check_block);

        self.push_instruction(with_stmt.span, ir::Instruction::CloseThisScope(this_scope));
        self.push_instruction(with_stmt.span, ir::Instruction::CloseVariable(state_var));

        Ok(())
    }

    fn try_catch_stmt(&mut self, try_catch_stmt: &ast::TryCatchStmt<S>) -> Result<(), IrGenError> {
        if !self.settings.allow_try_catch_blocks {
            return Err(IrGenError {
                kind: IrGenErrorKind::TryCatchNotAllowed,
                span: try_catch_stmt.span,
            });
        }

        // Desugar try / catch statements as `pcall` around an inner closure.

        let allow_break = !self.break_target_stack.is_empty();
        let allow_continue = !self.continue_target_stack.is_empty();

        let closure_span = try_catch_stmt.try_block.span();

        let pcall_name = self.interner.intern(BuiltIns::PCALL);
        let pcall = self.push_instruction(closure_span, ir::Instruction::GetMagic(pcall_name));

        let exit_code_var = self.function.variables.insert(ir::Variable::Owned);
        self.push_instruction(closure_span, ir::Instruction::OpenVariable(exit_code_var));

        let mut compiler = self.start_inner_function(FunctionRef::Expression(closure_span));
        let inner_exit_code_var = compiler
            .function
            .variables
            .insert(ir::Variable::Upper(exit_code_var));
        compiler.func_type = FunctionType::TryCatch {
            allow_break,
            allow_continue,
            exit_code: inner_exit_code_var,
        };
        compiler.statement(&try_catch_stmt.try_block)?;

        let function = compiler.finish();
        let func_id = self.function.functions.insert(function);

        let inner_closure = self.push_instruction(closure_span, ir::Instruction::Closure(func_id));

        let call_scope = self.function.call_scopes.insert(());
        self.push_instruction(
            closure_span,
            ir::Instruction::OpenCall {
                scope: call_scope,
                func: pcall,
                args: vec![inner_closure],
            },
        );
        let success =
            self.push_instruction(closure_span, ir::Instruction::FixedReturn(call_scope, 0));
        let ret_or_err =
            self.push_instruction(closure_span, ir::Instruction::FixedReturn(call_scope, 1));
        self.push_instruction(closure_span, ir::Instruction::CloseCall(call_scope));

        let success_block = self.new_block();
        let err_block = self.new_block();

        self.end_current_block(ir::Exit::Branch {
            cond: success,
            if_true: success_block,
            if_false: err_block,
        });

        self.start_new_block(success_block);

        let exit_code =
            self.push_instruction(closure_span, ir::Instruction::GetVariable(exit_code_var));

        let return_code = self.push_instruction(
            closure_span,
            ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Return as i64)),
        );
        let exit_code_is_return = self.push_instruction(
            closure_span,
            ir::Instruction::BinOp {
                left: exit_code,
                op: ir::BinOp::Equal,
                right: return_code,
            },
        );

        let do_return = self.new_block();
        let no_return = self.new_block();
        self.end_current_block(ir::Exit::Branch {
            cond: exit_code_is_return,
            if_true: do_return,
            if_false: no_return,
        });

        self.start_new_block(do_return);
        self.push_instruction(closure_span, ir::Instruction::CloseVariable(exit_code_var));
        self.do_return(closure_span, Some(ret_or_err))?;

        self.start_new_block(no_return);

        if allow_break {
            let break_code = self.push_instruction(
                closure_span,
                ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Break as i64)),
            );
            let exit_code_is_break = self.push_instruction(
                closure_span,
                ir::Instruction::BinOp {
                    left: exit_code,
                    op: ir::BinOp::Equal,
                    right: break_code,
                },
            );

            let do_break = self.new_block();
            let no_break = self.new_block();
            self.end_current_block(ir::Exit::Branch {
                cond: exit_code_is_break,
                if_true: do_break,
                if_false: no_break,
            });

            self.start_new_block(do_break);
            self.push_instruction(closure_span, ir::Instruction::CloseVariable(exit_code_var));
            self.do_break(closure_span)?;

            self.start_new_block(no_break);
        }

        if allow_continue {
            let continue_code = self.push_instruction(
                closure_span,
                ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Continue as i64)),
            );
            let exit_code_is_continue = self.push_instruction(
                closure_span,
                ir::Instruction::BinOp {
                    left: exit_code,
                    op: ir::BinOp::Equal,
                    right: continue_code,
                },
            );

            let do_continue = self.new_block();
            let no_continue = self.new_block();
            self.end_current_block(ir::Exit::Branch {
                cond: exit_code_is_continue,
                if_true: do_continue,
                if_false: no_continue,
            });

            self.start_new_block(do_continue);
            self.push_instruction(closure_span, ir::Instruction::CloseVariable(exit_code_var));
            self.do_continue(closure_span)?;

            self.start_new_block(no_continue);
        }

        let successor_block = self.new_block();
        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(err_block);
        self.push_scope();
        let err_var =
            self.declare_var(try_catch_stmt.err_ident.clone(), ir::Variable::Owned.into())?;

        self.set_var(try_catch_stmt.err_ident.span, err_var, ret_or_err);
        self.statement(&try_catch_stmt.catch_block)?;
        self.pop_scope();

        self.end_current_block(ir::Exit::Jump(successor_block));

        self.start_new_block(successor_block);
        self.push_instruction(closure_span, ir::Instruction::CloseVariable(exit_code_var));

        Ok(())
    }

    fn do_return(&mut self, span: Span, value: Option<ir::InstId>) -> Result<(), IrGenError> {
        match self.func_type {
            FunctionType::Normal => {
                self.end_current_block(ir::Exit::Return { value });
            }
            FunctionType::Constructor { this, .. } => {
                if value.is_some() {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::CannotReturnValue,
                        span,
                    });
                }
                self.end_current_block(ir::Exit::Return { value: Some(this) });
            }
            FunctionType::TryCatch { exit_code, .. } => {
                let return_code = self.push_instruction(
                    span,
                    ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Return as i64)),
                );
                self.push_instruction(span, ir::Instruction::SetVariable(exit_code, return_code));
                self.end_current_block(ir::Exit::Return { value });
            }
        }
        Ok(())
    }

    fn do_break(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&jump) = self.break_target_stack.last() {
            self.non_local_jump(jump)
        } else if let FunctionType::TryCatch {
            allow_break,
            exit_code,
            ..
        } = self.func_type
            && allow_break
        {
            let return_code = self.push_instruction(
                span,
                ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Break as i64)),
            );
            self.push_instruction(span, ir::Instruction::SetVariable(exit_code, return_code));
            self.end_current_block(ir::Exit::Return { value: None });
            Ok(())
        } else {
            Err(IrGenError {
                kind: IrGenErrorKind::BreakWithNoTarget,
                span,
            })
        }
    }

    fn do_continue(&mut self, span: Span) -> Result<(), IrGenError> {
        if let Some(&jump) = self.continue_target_stack.last() {
            self.non_local_jump(jump)
        } else if let FunctionType::TryCatch {
            allow_continue,
            exit_code,
            ..
        } = self.func_type
            && allow_continue
        {
            let return_code = self.push_instruction(
                span,
                ir::Instruction::Constant(Constant::Integer(TryCatchExitCode::Continue as i64)),
            );
            self.push_instruction(span, ir::Instruction::SetVariable(exit_code, return_code));
            self.end_current_block(ir::Exit::Return { value: None });
            Ok(())
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
            ast::Expression::Ident(s) => self.ident_expr(s),
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
                            let value = self.ident_expr(name);
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
                        ast::UnaryOp::Minus => ir::UnOp::Negate,
                        ast::UnaryOp::BitNegate => ir::UnOp::BitNegate,
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
                ast::BinaryOp::Xor => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::Xor,
                            right,
                        },
                    )
                }
                ast::BinaryOp::BitAnd => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::BitAnd,
                            right,
                        },
                    )
                }
                ast::BinaryOp::BitOr => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::BitOr,
                            right,
                        },
                    )
                }
                ast::BinaryOp::BitXor => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::BitXor,
                            right,
                        },
                    )
                }
                ast::BinaryOp::BitShiftLeft => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::BitShiftLeft,
                            right,
                        },
                    )
                }
                ast::BinaryOp::BitShiftRight => {
                    let left = self.expression(&bin_expr.left)?;
                    let right = self.expression(&bin_expr.right)?;
                    self.push_instruction(
                        bin_expr.span,
                        ir::Instruction::BinOp {
                            left,
                            op: ir::BinOp::BitShiftRight,
                            right,
                        },
                    )
                }
                ast::BinaryOp::NullCoalesce => self.short_circuit_null_coalesce(
                    bin_expr.span,
                    &bin_expr.left,
                    &bin_expr.right,
                )?,
            },
            ast::Expression::Ternary(tern_expr) => {
                let cond = self.expression(&tern_expr.cond)?;
                self.if_expr(
                    tern_expr.span,
                    cond,
                    |this| this.expression(&tern_expr.if_true),
                    |this| this.expression(&tern_expr.if_false),
                )?
            }
            ast::Expression::Function(func_expr) => {
                let allow_constructors = self.settings.allow_constructors;
                let mut compiler =
                    self.start_inner_function(FunctionRef::Expression(func_expr.span));

                compiler.declare_parameters(&func_expr.parameters)?;
                let function = if func_expr.is_constructor {
                    if !allow_constructors {
                        return Err(IrGenError {
                            kind: IrGenErrorKind::ConstructorsNotAllowed,
                            span: func_expr.span,
                        });
                    }
                    compiler.constructor(func_expr.inherit.as_ref(), &func_expr.body)?
                } else {
                    compiler.block(&func_expr.body)?;
                    compiler.finish()
                };

                let func_id = self.function.functions.insert(function);
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
            ast::Expression::Argument(arg_expr) => {
                let arg_index = self.expression(&arg_expr.arg_index)?;
                self.push_instruction(arg_expr.span, ir::Instruction::Argument(arg_index))
            }
            ast::Expression::ArgumentCount(span) => {
                self.push_instruction(*span, ir::Instruction::ArgumentCount)
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

        let ret = self.push_instruction(call.span, ir::Instruction::FixedReturn(call_scope, 0));
        self.push_instruction(call.span, ir::Instruction::CloseCall(call_scope));

        if let Some(this_scope) = this_scope {
            self.push_instruction(call.span, ir::Instruction::CloseThisScope(this_scope));
        }

        Ok(ret)
    }

    fn if_expr(
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

    fn ident_expr(&mut self, ident: &ast::Ident<S>) -> ir::InstId {
        if let Some(var) = self.find_var(ident) {
            self.get_var(ident.span, var)
        } else {
            match self.var_dict.free_var_mode(ident) {
                FreeVarMode::This => {
                    let this = self.push_instruction(ident.span, ir::Instruction::This);
                    let key = self.push_instruction(
                        ident.span,
                        ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                    );
                    self.push_instruction(
                        ident.span,
                        ir::Instruction::GetField { object: this, key },
                    )
                }
                FreeVarMode::GlobalVar => {
                    let globals = self.push_instruction(ident.span, ir::Instruction::Globals);
                    let key = self.push_instruction(
                        ident.span,
                        ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                    );
                    self.push_instruction(
                        ident.span,
                        ir::Instruction::GetField {
                            object: globals,
                            key,
                        },
                    )
                }
                FreeVarMode::Magic { .. } => self
                    .push_instruction(ident.span, ir::Instruction::GetMagic(ident.inner.clone())),
            }
        }
    }

    fn short_circuit_and(
        &mut self,
        span: Span,
        left: &ast::Expression<S>,
        right: &ast::Expression<S>,
    ) -> Result<ir::InstId, IrGenError> {
        let left = self.expression(left)?;
        self.if_expr(
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
        self.if_expr(
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
        self.if_expr(span, cond, |this| this.expression(right), |_| Ok(left))
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
                if let Some(var) = self.find_var(ident) {
                    MutableTarget::Var(var.clone())
                } else {
                    match self.var_dict.free_var_mode(ident) {
                        FreeVarMode::This => {
                            let key = self.push_instruction(
                                target.span(),
                                ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                            );
                            MutableTarget::This { key }
                        }
                        FreeVarMode::GlobalVar => {
                            let key = self.push_instruction(
                                target.span(),
                                ir::Instruction::Constant(Constant::String(ident.inner.clone())),
                            );
                            MutableTarget::Globals { key }
                        }
                        FreeVarMode::Magic { is_read_only } => {
                            if is_read_only {
                                return Err(IrGenError {
                                    kind: IrGenErrorKind::ReadOnlyMagic,
                                    span: target.span(),
                                });
                            }

                            MutableTarget::Magic(ident.inner.clone())
                        }
                    }
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
        match target {
            MutableTarget::Var(var) => self.get_var(span, var),
            MutableTarget::This { key } => {
                let this = self.push_instruction(span, ir::Instruction::This);
                self.push_instruction(span, ir::Instruction::GetField { object: this, key })
            }
            MutableTarget::Globals { key } => {
                let globals = self.push_instruction(span, ir::Instruction::Globals);
                self.push_instruction(
                    span,
                    ir::Instruction::GetField {
                        object: globals,
                        key,
                    },
                )
            }
            MutableTarget::Field { object, key } => {
                self.push_instruction(span, ir::Instruction::GetField { object, key })
            }
            MutableTarget::Index { array, indexes } => {
                self.push_instruction(span, ir::Instruction::GetIndex { array, indexes })
            }
            MutableTarget::Magic(name) => {
                self.push_instruction(span, ir::Instruction::GetMagic(name))
            }
        }
    }

    fn write_mutable_target(&mut self, span: Span, target: MutableTarget<S>, value: ir::InstId) {
        match target {
            MutableTarget::Var(var) => {
                self.set_var(span, var, value);
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
            MutableTarget::Globals { key } => {
                let globals = self.push_instruction(span, ir::Instruction::Globals);
                self.push_instruction(
                    span,
                    ir::Instruction::SetField {
                        object: globals,
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

    fn start_inner_function(&mut self, reference: FunctionRef) -> FunctionCompiler<'_, S> {
        // Right now, we simply don't support closures and constructors being enabled at the same
        // time.
        assert!(
            !(self.settings.allow_constructors && self.settings.closures),
            "constructors and closures cannot be enabled at the same time, constructor statics cannot be captured by closures"
        );

        let mut compiler =
            FunctionCompiler::new(self.settings, self.interner, reference, self.var_dict);

        // If we allow closures, then pass every currently in-scope variable as an upvar.
        if self.settings.closures {
            for (name, scope_list) in &self.var_lookup {
                let &scope_index = scope_list.last().unwrap();
                if let VarDecl::Normal(var_id) = self.scopes[scope_index].visible[name] {
                    compiler
                        .declare_var(name.clone(), ir::Variable::Upper(var_id).into())
                        .unwrap();
                }
            }
        }

        compiler
    }

    fn push_scope(&mut self) {
        if self.settings.block_scoping {
            self.scopes.push(Scope::default());
        }
    }

    fn pop_scope(&mut self) {
        if self.settings.block_scoping {
            if let Some(popped_scope) = self.scopes.pop() {
                // Close every variable in the popped scope.
                for var_id in popped_scope.to_close {
                    self.push_instruction(Span::null(), ir::Instruction::CloseVariable(var_id));
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

    fn declare_var(
        &mut self,
        vname: ast::Ident<S>,
        var_type: VarType<S>,
    ) -> Result<VarDecl<S>, IrGenError> {
        let top_scope_index = self.scopes.len() - 1;
        let top_scope = self.scopes.last_mut().unwrap();

        let var_decl = match var_type {
            VarType::Normal(variable) => {
                if !self.var_dict.permit_declaration(&vname) {
                    return Err(IrGenError {
                        kind: IrGenErrorKind::DeclarationNotPermitted,
                        span: vname.span,
                    });
                }

                let is_owned = variable.is_owned();
                let var_id = self.function.variables.insert(variable);

                if is_owned {
                    // This is an owned variable, so we need to open it when it is declared.
                    if self.settings.block_scoping {
                        // Since we're using block scoping, we need to close this variable when the
                        // scope ends.
                        top_scope.to_close.push(var_id);
                        self.push_instruction(Span::null(), ir::Instruction::OpenVariable(var_id));
                    } else {
                        // If we're not using block scoping, just open every variable at the very
                        // start of the function. This keeps the IR well-formed even with no block
                        // scoping and no explicit `CloseVariable` instructions.
                        //
                        // We push this instruction to `start_block`, which is kept otherwise empty
                        // for this purpose.
                        let inst_id = self
                            .function
                            .instructions
                            .insert(ir::Instruction::OpenVariable(var_id));
                        self.function.blocks[self.function.start_block]
                            .instructions
                            .push(inst_id);
                    }
                }

                VarDecl::Normal(var_id)
            }
            VarType::ConstructorStatic(field) => {
                // Permit any name for a constructor static, since they can be used as field names
                // (which are unrestricted).
                VarDecl::ConstructorStatic(field)
            }
        };

        let top_scope = self.scopes.last_mut().unwrap();
        let shadowing = top_scope
            .visible
            .insert(vname.clone(), var_decl.clone())
            .is_some();

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

        Ok(var_decl)
    }

    fn find_var(&mut self, vname: &S) -> Option<VarDecl<S>> {
        let scope_index = self.var_lookup.get(vname).and_then(|l| l.last().copied())?;
        Some(self.scopes[scope_index].visible[vname].clone())
    }

    fn get_var(&mut self, span: Span, var: VarDecl<S>) -> ir::InstId {
        match var {
            VarDecl::Normal(var_id) => {
                self.push_instruction(span, ir::Instruction::GetVariable(var_id))
            }
            VarDecl::ConstructorStatic(field) => {
                let FunctionType::Constructor { parent, .. } = self.func_type else {
                    panic!("constructor static var in non-constructor function")
                };

                self.push_instruction(
                    span,
                    ir::Instruction::GetFieldConst {
                        object: parent,
                        key: Constant::String(field.clone()),
                    },
                )
            }
        }
    }

    fn set_var(&mut self, span: Span, var: VarDecl<S>, value: ir::InstId) {
        match var {
            VarDecl::Normal(var_id) => {
                self.push_instruction(span, ir::Instruction::SetVariable(var_id, value));
            }
            VarDecl::ConstructorStatic(field) => {
                let FunctionType::Constructor { parent, .. } = self.func_type else {
                    panic!("constructor static var in non-constructor function")
                };

                self.push_instruction(
                    span,
                    ir::Instruction::SetFieldConst {
                        object: parent,
                        key: Constant::String(field),
                        value,
                    },
                );
            }
        }
    }

    fn new_block(&mut self) -> ir::BlockId {
        self.function.blocks.insert(ir::Block::default())
    }

    /// If there is a current block, finishes it with the given `Exit`.
    fn end_current_block(&mut self, exit: ir::Exit) {
        if let Some(current_block) = self.current_block {
            self.function.blocks[current_block].exit = exit;
            self.current_block = None;
        }
    }

    /// Start a new block.
    ///
    /// There must not currently be an active block.
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
        let current_block = if let Some(current) = self.current_block {
            current
        } else {
            // If we do not have an active block, create a new orphan one.
            //
            // Blocks can abruptly end due to statements like break, continue, and return, so this
            // will create a new *most likely unreachable* block to place instructions.
            let dead_block = self.new_block();
            self.current_block = Some(dead_block);
            dead_block
        };

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
