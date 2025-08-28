use std::{
    fmt::Write as _,
    io::{self, Write as _},
    mem,
};

use fabricator_vm as vm;

use crate::util::resolve_array_range;

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

    let string_length = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let string: vm::String = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, string.chars().count() as isize);
        Ok(())
    });
    lib.insert(
        ctx.intern("string_length"),
        vm::MagicConstant::new_ptr(&ctx, string_length),
    );

    let string_byte_length = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let string: vm::String = exec.stack().consume(ctx)?;
        exec.stack().replace(ctx, string.len() as isize);
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
        exec.stack().replace(ctx, c.unwrap() as isize);
        Ok(())
    });
    lib.insert(ctx.intern("ord"), vm::MagicConstant::new_ptr(&ctx, ord));

    let show_debug_message = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let fmt_string: vm::String = exec.stack().from_index(ctx, 0)?;

        let mut stdout = io::stdout().lock();
        for part in split_format(&fmt_string) {
            match part {
                FormatPart::Str(s) => write!(stdout, "{}", s)?,
                FormatPart::Arg(arg) => write!(stdout, "{}", exec.stack().get(arg + 1))?,
            }
        }
        writeln!(stdout)?;
        exec.stack().clear();
        Ok(())
    });
    lib.insert(
        ctx.intern("show_debug_message"),
        vm::MagicConstant::new_ptr(&ctx, show_debug_message),
    );

    let string = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let out = match exec.stack().get(0) {
            vm::Value::String(fmt) => {
                let mut out = String::new();
                for part in split_format(&fmt) {
                    match part {
                        FormatPart::Str(s) => out.push_str(s),
                        FormatPart::Arg(arg) => {
                            write!(&mut out, "{}", exec.stack().get(arg + 1)).unwrap()
                        }
                    }
                }
                ctx.intern(&out)
            }
            other => ctx.intern(&other.to_string()),
        };
        exec.stack().replace(ctx, out);
        Ok(())
    });
    lib.insert(
        ctx.intern("string"),
        vm::MagicConstant::new_ptr(&ctx, string),
    );

    let string_char_at = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (string, index): (vm::String, usize) = exec.stack().consume(ctx)?;
        let mut chars = string.chars();
        let index = index.checked_sub(1).ok_or_else(|| {
            format!("index given to `string_char_at` is 1-indexed and cannot be 0")
        })?;
        let c = chars.nth(index).ok_or_else(|| {
            format!(
                "index {index} is out of range in string of length {}",
                string.chars().count()
            )
        })?;
        exec.stack().replace(ctx, c.to_string());
        Ok(())
    });
    lib.insert(
        ctx.intern("string_char_at"),
        vm::MagicConstant::new_ptr(&ctx, string_char_at),
    );

    let string_digits = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let input: vm::String = exec.stack().consume(ctx)?;
        let mut output = String::new();

        for c in input.chars() {
            if c.is_digit(10) {
                output.push(c);
            }
        }

        exec.stack().replace(ctx, output);
        Ok(())
    });
    lib.insert(
        ctx.intern("string_digits"),
        vm::MagicConstant::new_ptr(&ctx, string_digits),
    );

    let string_pos = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (substr, string): (vm::String, vm::String) = exec.stack().consume(ctx)?;
        let index = string
            .find(substr.as_str())
            .map(|i| i as isize + 1)
            .unwrap_or(0);
        exec.stack().replace(ctx, index);
        Ok(())
    });
    lib.insert(
        ctx.intern("string_pos"),
        vm::MagicConstant::new_ptr(&ctx, string_pos),
    );

    let string_copy = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (string, index, count): (vm::String, usize, usize) = exec.stack().consume(ctx)?;
        let index = index
            .checked_sub(1)
            .ok_or_else(|| format!("index given to `string_copy` is 1-indexed and cannot be 0"))?;
        exec.stack().replace(
            ctx,
            string.chars().skip(index).take(count).collect::<String>(),
        );
        Ok(())
    });
    lib.insert(
        ctx.intern("string_copy"),
        vm::MagicConstant::new_ptr(&ctx, string_copy),
    );

    let string_delete = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (string, index, count): (vm::String, isize, isize) = exec.stack().consume(ctx)?;
        let index = index
            .checked_sub(1)
            .ok_or_else(|| format!("index given to `string_copy` is 1-indexed and cannot be 0"))?;
        let (range, _) = resolve_array_range(string.chars().count(), Some(index), Some(count))?;
        exec.stack().replace(
            ctx,
            string
                .chars()
                .enumerate()
                .filter_map(|(i, c)| if range.contains(&i) { None } else { Some(c) })
                .collect::<String>(),
        );
        Ok(())
    });
    lib.insert(
        ctx.intern("string_delete"),
        vm::MagicConstant::new_ptr(&ctx, string_delete),
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
