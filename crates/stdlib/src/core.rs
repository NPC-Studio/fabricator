use std::{env, mem};

use fabricator_vm::{
    self as vm,
    magic::{MagicConstant, MagicSet},
};

pub fn core_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut MagicSet<'gc>) {
    let typeof_ = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        // Return a *roughly* GML compatible set of type names.
        let type_name = match exec.stack().consume(ctx)? {
            vm::Value::Undefined => "undefined",
            vm::Value::Boolean(_) => "bool",
            vm::Value::Integer(_) => "int64",
            vm::Value::Float(_) => "number",
            vm::Value::String(_) => "string",
            vm::Value::Object(_) => "struct",
            vm::Value::Array(_) => "array",
            vm::Value::Closure(_) => "method",
            vm::Value::Callback(_) => "method",
            vm::Value::UserData(_) => "ptr",
        };

        exec.stack().replace(ctx, ctx.intern(type_name));
        Ok(())
    });
    lib.insert(ctx.intern("typeof"), MagicConstant::new_ptr(&ctx, typeof_));

    let int64 = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::Value = exec.stack().consume(ctx)?;
        let int = if let Some(i) = arg.cast_integer() {
            i
        } else if let vm::Value::String(i) = arg {
            i.parse()?
        } else {
            return Err(vm::TypeError {
                expected: "number or string",
                found: arg.type_name(),
            }
            .into());
        };
        exec.stack().replace(ctx, int);
        Ok(())
    });
    lib.insert(ctx.intern("int64"), MagicConstant::new_ptr(&ctx, int64));

    let ord = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let arg: vm::String = exec.stack().consume(ctx)?;
        let mut iter = arg.as_str().chars();
        let c = iter.next();
        if c.is_none() || iter.next().is_some() {
            return Err("`ord` must be given a single character string".into());
        }
        exec.stack().replace(ctx, c.unwrap() as i64);
        Ok(())
    });
    lib.insert(ctx.intern("ord"), MagicConstant::new_ptr(&ctx, ord));

    lib.insert(
        ctx.intern("os_type"),
        MagicConstant::new_ptr(&ctx, ctx.intern(env::consts::OS)),
    );
    lib.insert(
        ctx.intern("os_windows"),
        MagicConstant::new_ptr(&ctx, ctx.intern("windows")),
    );
    lib.insert(
        ctx.intern("os_macosx"),
        MagicConstant::new_ptr(&ctx, ctx.intern("macos")),
    );
    lib.insert(
        ctx.intern("os_linux"),
        MagicConstant::new_ptr(&ctx, ctx.intern("linux")),
    );
    lib.insert(
        ctx.intern("os_switch"),
        MagicConstant::new_ptr(&ctx, ctx.intern("switch")),
    );
    lib.insert(
        ctx.intern("os_ps4"),
        MagicConstant::new_ptr(&ctx, ctx.intern("ps4")),
    );
    lib.insert(
        ctx.intern("os_ps5"),
        MagicConstant::new_ptr(&ctx, ctx.intern("ps5")),
    );
    lib.insert(
        ctx.intern("os_gdk"),
        MagicConstant::new_ptr(&ctx, ctx.intern("gdk")),
    );
    lib.insert(
        ctx.intern("os_xboxseriesx"),
        MagicConstant::new_ptr(&ctx, ctx.intern("xboxseriesx")),
    );
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FormatPart<'a> {
    Str(&'a str),
    Arg(usize),
}

pub fn split_format<'a>(s: &'a str) -> impl Iterator<Item = FormatPart<'a>> + 'a {
    struct Iter<'a> {
        rest: &'a str,
        next_arg: Option<usize>,
    }

    impl<'a> Iterator for Iter<'a> {
        type Item = FormatPart<'a>;

        fn next(&mut self) -> Option<Self::Item> {
            if let Some(next_arg) = self.next_arg.take() {
                return Some(FormatPart::Arg(next_arg));
            } else if self.rest.is_empty() {
                return None;
            }

            // Try to find a curly brace pair `{xxx}` where `xxx` is a valid `usize`. This is
            // interpreted as a "format argument" with the argument index between the braces.
            //
            // Parsing is completely forgiving because there is no actual specification for `string`
            // and `show_debug_message`. Anything that can be parsed as a valid format arg is,
            // anything else is left in the resulting string.

            let mut find_start_pos = 0;
            loop {
                let Some(left_brace_pos) = self.rest[find_start_pos..].find('{') else {
                    break;
                };
                let left_brace_pos = find_start_pos + left_brace_pos;

                // Stop at any trailing `{` so that we make sure to parse the innermost brace pair.
                //
                // The string `"hello {{0}"` should have one valid format argument.
                let Some(right_brace_pos) = self.rest[left_brace_pos + 1..].find(&['{', '}'])
                else {
                    break;
                };
                let right_brace_pos = left_brace_pos + 1 + right_brace_pos;

                if self.rest[right_brace_pos..].starts_with('}') {
                    let leading = &self.rest[0..left_brace_pos];
                    let trailing = &self.rest[right_brace_pos + 1..];
                    if let Ok(arg_index) =
                        self.rest[left_brace_pos + 1..right_brace_pos].parse::<usize>()
                    {
                        self.rest = trailing;
                        return if leading.is_empty() {
                            Some(FormatPart::Arg(arg_index))
                        } else {
                            self.next_arg = Some(arg_index);
                            Some(FormatPart::Str(leading))
                        };
                    } else {
                        // Try again following the `}`.
                        find_start_pos = right_brace_pos + 1;
                    }
                } else {
                    // Try again at the `{`.
                    find_start_pos = right_brace_pos;
                }
            }

            Some(FormatPart::Str(mem::replace(&mut self.rest, "")))
        }
    }

    Iter {
        rest: s,
        next_arg: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_format() {
        assert_eq!(
            &split_format("{{0}").collect::<Vec<_>>(),
            &[FormatPart::Str("{"), FormatPart::Arg(0)]
        );
        assert_eq!(
            &split_format("{0}{1}foo{2}").collect::<Vec<_>>(),
            &[
                FormatPart::Arg(0),
                FormatPart::Arg(1),
                FormatPart::Str("foo"),
                FormatPart::Arg(2)
            ]
        );
        assert_eq!(
            &split_format("{0}{{1}}foo{2}").collect::<Vec<_>>(),
            &[
                FormatPart::Arg(0),
                FormatPart::Str("{"),
                FormatPart::Arg(1),
                FormatPart::Str("}foo"),
                FormatPart::Arg(2)
            ]
        );
        assert_eq!(
            &split_format("{0{{1}}foo{2").collect::<Vec<_>>(),
            &[
                FormatPart::Str("{0{"),
                FormatPart::Arg(1),
                FormatPart::Str("}foo{2"),
            ]
        );
    }
}
