use std::{
    io::{self, Write},
    mem,
};

use fabricator_vm as vm;

pub fn string_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    let string_trim = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (string, trims): (vm::String, Option<Vec<vm::String>>) = exec.stack().consume(ctx)?;
        let string = if let Some(trims) = trims {
            let mut string = string.as_str();
            for trim in trims {
                string = string.trim_start_matches(trim.as_str());
                string = string.trim_end_matches(trim.as_str());
            }
            ctx.intern(string)
        } else {
            ctx.intern(string.as_str().trim())
        };
        exec.stack().replace(ctx, string);
        Ok(())
    });
    lib.insert(
        ctx.intern("string_trim"),
        vm::MagicConstant::new_ptr(&ctx, string_trim),
    );

    let string_byte_length = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let string: vm::String = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, string.len() as i64);
        Ok(())
    });
    lib.insert(
        ctx.intern("string_byte_length"),
        vm::MagicConstant::new_ptr(&ctx, string_byte_length),
    );

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
    lib.insert(ctx.intern("ord"), vm::MagicConstant::new_ptr(&ctx, ord));

    let show_debug_message = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let Some(fmt_string) = exec.stack().get(0).cast_string(ctx) else {
            return Err("`show_debug_message` must be given a formatting string".into());
        };

        let mut stdout = io::stdout().lock();
        for part in split_format(&fmt_string) {
            match part {
                FormatPart::Str(s) => write!(stdout, "{}", s)?,
                FormatPart::Arg(arg) => write!(stdout, "{}", exec.stack().get(arg + 1))?,
            }
        }
        writeln!(stdout)?;
        Ok(())
    });
    lib.insert(
        ctx.intern("show_debug_message"),
        vm::MagicConstant::new_ptr(&ctx, show_debug_message),
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
