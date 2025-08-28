use gc_arena::Mutation;

use crate::{
    callback::Callback,
    closure::Closure,
    interpreter::Context,
    magic::{MagicConstant, MagicSet},
    object::Object,
    value::{Function, Value},
};

/// FML functions for core VM functionality.
///
/// Can be assumed to be present in any FML environment, and may be required for compilation.
pub struct BuiltIns<'gc> {
    /// Rebind the implicit `this` on a callback or closure.
    ///
    /// ```fml
    /// var f = function() {
    ///     return self.field;
    /// };
    ///
    /// var t = {
    ///     field: true,
    /// };
    ///
    /// var f_rebound = method(t, f);
    /// ```
    pub method: Callback<'gc>,

    /// Call the given function and catch any errors.
    ///
    /// The first parameter is the function to call, the rest are parameters to pass to the provided
    /// function.
    ///
    /// If the given function completes without error, returns `true` followed by the return values
    /// of the inner function.
    ///
    /// If there is an error executing the given function, returns `false` followed by the error.
    ///
    /// ```fml
    /// var success, err = pcall(function() {
    ///     throw "my_error";
    /// });
    ///
    /// assert(success == false);
    /// assert(err == "my_error");
    /// ```
    pub pcall: Callback<'gc>,

    /// Get the parent (super) of an object if it exists.
    pub get_super: Callback<'gc>,

    /// Give an object a new parent (super).
    ///
    /// ```fml
    /// var obj = {
    ///     a: 1,
    /// };
    ///
    /// var parent = {
    ///     b: 2,
    /// };
    ///
    /// super_set(obj, parent);
    ///
    /// assert(obj.a == 1);
    /// assert(obj.b == 2);
    /// ```
    pub set_super: Callback<'gc>,

    /// Get the constructor super object for the prototype of the given closure, initializing it if
    /// it is not yet initialized.
    ///
    /// This is an internal compiler support method.
    pub init_constructor_super: Callback<'gc>,

    /// Get the constructor super object for the prototype of the given closure, if it has been
    /// initialized.
    ///
    /// This is an internal compiler support method.
    pub get_constructor_super: Callback<'gc>,

    /// Return the loop function and initial state for a `with` loop on the given object.
    ///
    /// This is an internal compiler support method.
    pub with_loop_iter: Callback<'gc>,
}

impl<'gc> BuiltIns<'gc> {
    pub const METHOD: &'static str = "method";
    pub const PCALL: &'static str = "pcall";
    pub const GET_SUPER: &'static str = "get_super";
    pub const SET_SUPER: &'static str = "set_super";
    pub const INIT_CONSTRUCTOR_SUPER: &'static str = "__init_constructor_super";
    pub const GET_CONSTRUCTOR_SUPER: &'static str = "__get_constructor_super";
    pub const WITH_LOOP_ITER: &'static str = "__with_loop_iter";

    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            method: Callback::from_fn(mc, |ctx, mut exec| {
                let (obj, func): (Value, Function) = exec.stack().consume(ctx)?;

                match obj {
                    obj @ (Value::Undefined | Value::Object(_) | Value::UserData(_)) => {
                        exec.stack().replace(ctx, func.rebind(&ctx, obj));
                        Ok(())
                    }
                    _ => Err("self value must be an object, userdata, or undefined".into()),
                }
            }),

            get_super: Callback::from_fn(mc, |ctx, mut exec| {
                let obj: Object = exec.stack().consume(ctx)?;
                exec.stack().replace(ctx, obj.parent());
                Ok(())
            }),

            set_super: Callback::from_fn(mc, |ctx, mut exec| {
                let (obj, parent): (Object, Option<Object>) = exec.stack().consume(ctx)?;
                obj.set_parent(&ctx, parent)?;
                Ok(())
            }),

            pcall: Callback::from_fn(mc, |ctx, mut exec| {
                let function: Function = exec.stack().from_index(ctx, 0)?;
                let mut sub_exec = exec.with_stack_bottom(1);
                let res = match function {
                    Function::Closure(closure) => {
                        sub_exec.call_closure(ctx, closure).map_err(|e| e.error)
                    }
                    Function::Callback(callback) => {
                        callback.call(ctx, sub_exec).map_err(|e| e.into())
                    }
                };
                match res {
                    Ok(_) => {
                        exec.stack()[0] = true.into();
                    }
                    Err(err) => {
                        exec.stack().replace(ctx, (false, err.to_value(ctx)));
                    }
                }
                Ok(())
            }),

            init_constructor_super: Callback::from_fn(mc, |ctx, mut exec| {
                let closure: Closure = exec.stack().consume(ctx)?;
                exec.stack()
                    .replace(ctx, closure.prototype().init_constructor_super(&ctx));
                Ok(())
            }),

            get_constructor_super: Callback::from_fn(mc, |ctx, mut exec| {
                let closure: Closure = exec.stack().consume(ctx)?;
                exec.stack()
                    .replace(ctx, closure.prototype().constructor_super());
                Ok(())
            }),

            with_loop_iter: {
                // An iterator function whose state is the single value for iteration.
                let singleton_iter = Callback::from_fn(mc, |_, mut exec| {
                    exec.stack().push_front(Value::Undefined);
                    Ok(())
                });

                Callback::from_fn_with_root(mc, singleton_iter, |&singleton_iter, ctx, mut exec| {
                    let target: Value = exec.stack().consume(ctx)?;
                    match target {
                        Value::Object(object) => {
                            // Objects are a loop with one iteration of the object.
                            exec.stack().push_back(singleton_iter);
                            exec.stack().push_back(object);
                            Ok(())
                        }
                        Value::UserData(user_data) => user_data.iter(ctx),
                        _ => Err("with loop target must be object or userdata".into()),
                    }
                })
            },
        }
    }

    /// Create a `MagicSet` with each builtin function.
    ///
    /// All magic names are string constants available in [`BuiltIns`].
    pub fn magic_set(&self, ctx: Context<'gc>) -> MagicSet<'gc> {
        let mut magic = MagicSet::new();

        magic.insert(
            ctx.intern(Self::METHOD),
            MagicConstant::new_ptr(&ctx, self.method),
        );

        magic.insert(
            ctx.intern(Self::PCALL),
            MagicConstant::new_ptr(&ctx, self.pcall),
        );

        magic.insert(
            ctx.intern(Self::GET_SUPER),
            MagicConstant::new_ptr(&ctx, self.get_super),
        );

        magic.insert(
            ctx.intern(Self::SET_SUPER),
            MagicConstant::new_ptr(&ctx, self.set_super),
        );

        magic.insert(
            ctx.intern(Self::INIT_CONSTRUCTOR_SUPER),
            MagicConstant::new_ptr(&ctx, self.init_constructor_super),
        );

        magic.insert(
            ctx.intern(Self::GET_CONSTRUCTOR_SUPER),
            MagicConstant::new_ptr(&ctx, self.get_constructor_super),
        );

        magic.insert(
            ctx.intern(Self::WITH_LOOP_ITER),
            MagicConstant::new_ptr(&ctx, self.with_loop_iter),
        );

        magic
    }
}
