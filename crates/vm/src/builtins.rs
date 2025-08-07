use gc_arena::Mutation;

use crate::{
    callback::Callback,
    closure::Closure,
    error::Error,
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

    /// Get the constructor super object for the prototype of the given closure, if it is a
    /// constructor.
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
    pub const GET_SUPER: &'static str = "get_super";
    pub const SET_SUPER: &'static str = "set_super";
    pub const GET_CONSTRUCTOR_SUPER: &'static str = "__get_prototype_super";
    pub const WITH_LOOP_ITER: &'static str = "__with_loop_iter";

    pub fn new(mc: &Mutation<'gc>) -> Self {
        Self {
            method: Callback::from_fn_with_err_ctx(
                mc,
                format!("in `{}` builtin", Self::METHOD),
                |ctx, _, mut stack| {
                    let (obj, func): (Value, Function) = stack.consume(ctx)?;

                    match obj {
                        obj @ (Value::Undefined | Value::Object(_) | Value::UserData(_)) => {
                            stack.replace(ctx, func.rebind(&ctx, obj));
                            Ok(())
                        }
                        _ => Err(Error::msg(
                            "self value must be an object, userdata, or undefined",
                        )),
                    }
                },
            ),

            get_super: Callback::from_fn_with_err_ctx(
                mc,
                format!("in `{}` builtin", Self::GET_SUPER),
                |ctx, _, mut stack| {
                    let obj: Object = stack.consume(ctx)?;
                    stack.replace(ctx, obj.parent());
                    Ok(())
                },
            ),

            set_super: Callback::from_fn_with_err_ctx(
                mc,
                format!("in `{}` builtin", Self::SET_SUPER),
                |ctx, _, mut stack| {
                    let (obj, parent): (Object, Option<Object>) = stack.consume(ctx)?;
                    obj.set_parent(&ctx, parent)?;
                    Ok(())
                },
            ),

            get_constructor_super: Callback::from_fn_with_err_ctx(
                mc,
                format!("in `{}` builtin", Self::GET_CONSTRUCTOR_SUPER),
                |ctx, _, mut stack| {
                    let closure: Closure = stack.consume(ctx)?;
                    stack.replace(ctx, closure.prototype().constructor_parent());
                    Ok(())
                },
            ),

            with_loop_iter: {
                // An iterator function whose state is the single value for iteration.
                let singleton_iter = Callback::from_fn(mc, |_, _, mut stack| {
                    stack.push_front(Value::Undefined);
                    Ok(())
                });

                Callback::from_fn_with_root_and_err_ctx(
                    mc,
                    singleton_iter,
                    format!("in `{}` builtin", Self::WITH_LOOP_ITER),
                    |&singleton_iter, ctx, _, mut stack| {
                        let target: Value = stack.consume(ctx)?;
                        match target {
                            Value::Object(object) => {
                                // Objects are a loop with one iteration of the object.
                                stack.push_back(singleton_iter.into());
                                stack.push_back(object.into());
                                Ok(())
                            }
                            Value::UserData(user_data) => {
                                if let Some(methods) = user_data.methods() {
                                    methods.iter(ctx, user_data)
                                } else {
                                    Err(Error::msg("userdata not iterable"))
                                }
                            }
                            _ => Err(Error::msg("with loop target must be object or userdata")),
                        }
                    },
                )
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
            MagicConstant::new_ptr(&ctx, self.method.into()),
        );

        magic.insert(
            ctx.intern(Self::GET_SUPER),
            MagicConstant::new_ptr(&ctx, self.get_super.into()),
        );

        magic.insert(
            ctx.intern(Self::SET_SUPER),
            MagicConstant::new_ptr(&ctx, self.set_super.into()),
        );

        magic.insert(
            ctx.intern(Self::GET_CONSTRUCTOR_SUPER),
            MagicConstant::new_ptr(&ctx, self.get_constructor_super.into()),
        );

        magic.insert(
            ctx.intern(Self::WITH_LOOP_ITER),
            MagicConstant::new_ptr(&ctx, self.with_loop_iter.into()),
        );

        magic
    }
}
