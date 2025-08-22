use std::{
    collections::HashMap,
    env,
    ffi::{CStr, c_void},
    marker::PhantomData,
    mem,
    rc::Rc,
};

use anyhow::{Context, Error};
use fabricator_vm as vm;
use gc_arena::{Collect, Mutation};
use libloading::Library;

use crate::project::{ExtensionFile, FfiType};

pub type ExtensionCallbacks<'gc> = HashMap<vm::String<'gc>, vm::Callback<'gc>>;

pub fn load_extension_file<'gc>(
    ctx: vm::Context<'gc>,
    file: &ExtensionFile,
) -> Result<Option<ExtensionCallbacks<'gc>>, Error> {
    for module_path in &file.module_paths {
        if module_path.extension().and_then(|e| e.to_str()) == Some(env::consts::DLL_EXTENSION) {
            log::info!("loading extension {module_path:?}");
            let library = Rc::new(unsafe { Library::new(module_path)? });
            let mut callbacks = HashMap::new();
            for function in &file.functions {
                let callback = get_extension_callback(
                    &ctx,
                    &library,
                    &function.external_name,
                    &function.arg_types,
                    function.return_type,
                )
                .with_context(|| {
                    format!(
                        "looking for symbol {:?} in {:?}",
                        function.external_name, module_path
                    )
                })?;
                callbacks.insert(ctx.intern(&function.name), callback);
            }
            return Ok(Some(callbacks));
        }
    }
    Ok(None)
}

#[repr(transparent)]
struct Number(f64);

impl Number {
    const FFI_TYPE: FfiType = FfiType::Number;
}

impl<'gc> vm::FromValue<'gc> for Number {
    fn from_value(ctx: vm::Context<'gc>, value: vm::Value<'gc>) -> Result<Self, vm::TypeError> {
        Ok(Self(f64::from_value(ctx, value)?))
    }
}

impl<'gc> vm::IntoValue<'gc> for Number {
    fn into_value(self, ctx: vm::Context<'gc>) -> vm::Value<'gc> {
        self.0.into_value(ctx)
    }
}

#[repr(transparent)]
struct Pointer(*const c_void);

impl Pointer {
    const FFI_TYPE: FfiType = FfiType::Pointer;
}

impl<'gc> vm::FromValue<'gc> for Pointer {
    fn from_value(_ctx: vm::Context<'gc>, value: vm::Value<'gc>) -> Result<Self, vm::TypeError> {
        if let vm::Value::String(s) = value {
            Ok(Self(s.as_ptr() as *const c_void))
        } else {
            Err(vm::TypeError {
                expected: "string",
                found: value.type_name(),
            })
        }
    }
}

impl<'gc> vm::IntoValue<'gc> for Pointer {
    fn into_value(self, ctx: vm::Context<'gc>) -> vm::Value<'gc> {
        let c_str = unsafe { CStr::from_ptr(self.0 as *const _) };
        ctx.intern(c_str.to_string_lossy().as_ref()).into()
    }
}

macro_rules! call_for_arg_type_combinations {
    ($macro:ident, ($($arg_name:ident: $arg_type:ty),*)) => {
        $macro!($($arg_name: $arg_type),*);
    };

    ($macro:ident, ($($arg_name:ident: $arg_type:ty),*), $next:ident $(, $rest:ident)*) => {
        call_for_arg_type_combinations!($macro, ($($arg_name: $arg_type,)* $next: Pointer) $(,$rest)*);
        call_for_arg_type_combinations!($macro, ($($arg_name: $arg_type,)* $next: Number) $(,$rest)*);
    };
}

macro_rules! call_for_uniform_arg_types {
    ($macro:ident, ($($arg_name:ident),*)) => {
        $macro!($($arg_name: Pointer),*);
        $macro!($($arg_name: Number),*);
    };

    ($macro:ident, ($($arg_name:ident),*), $next:ident $(, $rest:ident)*) => {
        call_for_uniform_arg_types!($macro, ($($arg_name,)* $next) $(,$rest)*);
    };
}

macro_rules! call_for_all_signatures {
    ($macro:ident) => {
        call_for_arg_type_combinations!($macro, ());
        call_for_arg_type_combinations!($macro, (), A);
        call_for_arg_type_combinations!($macro, (), A, B);
        call_for_arg_type_combinations!($macro, (), A, B, C);
        call_for_arg_type_combinations!($macro, (), A, B, C, D);
        call_for_arg_type_combinations!($macro, (), A, B, C, D, E);

        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F);
        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F, G);
        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F, G, H);
        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F, G, H, I);
        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F, G, H, I, J);
        call_for_uniform_arg_types!($macro, (), A, B, C, D, E, F, G, H, I, J, K);
    };
}

fn get_extension_callback<'gc>(
    mc: &Mutation<'gc>,
    library: &Rc<Library>,
    symbol: &str,
    arg_types: &[FfiType],
    ret_type: FfiType,
) -> Result<vm::Callback<'gc>, Error> {
    macro_rules! check_arg_types {
        ($($arg_name:ident: $arg_type:ty),*) => {
            if arg_types == &[$(<$arg_type>::FFI_TYPE),*] {
                let callback = unsafe {
                    match ret_type {
                        FfiType::Number => {
                            vm::Callback::new(
                                mc,
                                FfiFn::<Number, ($($arg_type,)*)>::new(library, symbol)?,
                                vm::Value::Undefined,
                            )
                        }
                        FfiType::Pointer => {
                            vm::Callback::new(
                                mc,
                                FfiFn::<Pointer, ($($arg_type,)*)>::new(library, symbol)?,
                                vm::Value::Undefined,
                            )
                        }
                    }
                };

                return Ok(callback);
            }
        };
    }

    call_for_all_signatures!(check_arg_types);

    Err(Error::msg(format!(
        "unsupported function signature {arg_types:?} -> {ret_type:?}"
    )))
}

#[derive(Collect)]
#[collect(require_static)]
struct FfiFn<Ret, Args> {
    fn_ptr: *const c_void,
    _library: Rc<Library>,
    _marker: PhantomData<(Ret, Args)>,
}

macro_rules! impl_ffi_signature {
    ($ret_type:ty, $($arg_name:ident: $arg_type:ty),*) => {
        impl<'gc> FfiFn<$ret_type, ($($arg_type,)*)> {
            unsafe fn new(library: &Rc<Library>, symbol: &str) -> Result<Self, Error> {
                let fn_ptr = unsafe {
                    library.get::<extern "C" fn ($($arg_type),*) -> $ret_type>(symbol.as_bytes())?
                        .try_as_raw_ptr()
                        .ok_or(Error::msg("cannot get symbol raw pointer"))?
                };
                Ok(Self {
                    fn_ptr,
                    _library: library.clone(),
                    _marker: PhantomData,
                })
            } }

        impl<'gc> vm::CallbackFn<'gc> for FfiFn<$ret_type, ($($arg_type,)*)> {
            #[allow(non_snake_case)]
            fn call(&self, ctx: vm::Context<'gc>, mut exec: vm::Execution<'gc, '_>) -> Result<(), vm::RuntimeError> {
                let ($($arg_name,)*): ($($arg_type,)*) = exec.stack().consume(ctx)?;
                let fn_ptr: extern "C" fn($($arg_type),*) -> $ret_type = unsafe { mem::transmute(self.fn_ptr) };
                let ret = (fn_ptr)($($arg_name),*);
                exec.stack().replace(ctx, ret);
                Ok(())
            }
        }
    };
}

macro_rules! impl_ffi_args {
    ($($arg_name:ident: $arg_type:ty),* $(,)?) => {
        impl_ffi_signature!(Pointer, $($arg_name: $arg_type),*);
        impl_ffi_signature!(Number, $($arg_name: $arg_type),*);
    }
}

call_for_all_signatures!(impl_ffi_args);
