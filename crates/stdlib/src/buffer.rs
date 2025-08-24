use std::{
    cell::{Ref, RefCell},
    str,
};

use fabricator_vm::{self as vm, magic::MagicConstant};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BufferType {
    Fixed,
    Growable,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BufferSeek {
    Start,
    Relative,
    End,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum DataType {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    F32,
    F64,
    Bool,
    String,
    Text,
}

pub struct Buffer {
    inner: RefCell<BufferState>,
}

struct BufferState {
    buffer_type: BufferType,
    alignment_power_of_2: u32,
    data: Vec<u8>,
    cursor: usize,
}

impl Buffer {
    pub fn read<'a>(&'a self) -> Ref<'a, [u8]> {
        Ref::map(self.inner.borrow(), |cell| cell.data.as_slice())
    }

    fn new(buffer_type: BufferType, mut size: usize, alignment: usize) -> Self {
        assert!(alignment.is_power_of_two());
        let alignment_power_of_2 = alignment.ilog2();
        size = size.checked_next_multiple_of(alignment).unwrap();
        Self {
            inner: RefCell::new(BufferState {
                buffer_type,
                alignment_power_of_2,
                data: vec![0; size],
                cursor: 0,
            }),
        }
    }
}

impl BufferState {
    fn cursor_write(&mut self, data: &[u8]) -> Result<(), vm::RuntimeError> {
        let cursor = self
            .cursor
            .next_multiple_of(2usize.pow(self.alignment_power_of_2));
        self.write_at(cursor, data)?;
        self.cursor = cursor + data.len();
        Ok(())
    }

    fn cursor_write_unaligned(&mut self, data: &[u8]) -> Result<(), vm::RuntimeError> {
        self.write_at(self.cursor, data)?;
        self.cursor = self.cursor + data.len();
        Ok(())
    }

    fn cursor_read(&mut self, data: &mut [u8]) -> Result<(), vm::RuntimeError> {
        let cursor = self
            .cursor
            .next_multiple_of(2usize.pow(self.alignment_power_of_2));
        self.read_at(cursor, data)?;
        self.cursor = cursor + data.len();
        Ok(())
    }

    fn cursor_read_until_nul(&mut self) -> Result<&[u8], vm::RuntimeError> {
        let cursor = self
            .cursor
            .next_multiple_of(2usize.pow(self.alignment_power_of_2));
        let Some(nul) = self.data[self.cursor..]
            .iter()
            .copied()
            .enumerate()
            .find_map(|(i, b)| if b == 0 { Some(i) } else { None })
        else {
            return Err(format!("read of string from pos {cursor}, no NUL found",).into());
        };
        let end = cursor + nul;

        let slice = &self.data[cursor..end];
        self.cursor = nul + 1;
        Ok(slice)
    }

    fn cursor_read_until_nul_or_end(&mut self) -> &[u8] {
        let cursor = self
            .cursor
            .next_multiple_of(2usize.pow(self.alignment_power_of_2));
        let end = cursor
            + self.data[self.cursor..]
                .iter()
                .copied()
                .enumerate()
                .find_map(|(i, b)| if b == 0 { Some(i) } else { None })
                .unwrap_or(self.data.len() - cursor);
        let slice = &self.data[cursor..end];
        self.cursor = self.data.len();
        slice
    }

    fn write_at(&mut self, pos: usize, data: &[u8]) -> Result<(), vm::RuntimeError> {
        let write_end = pos + data.len();
        if self.data.len() < write_end {
            match self.buffer_type {
                BufferType::Fixed => {
                    return Err(format!(
                        "write to pos {write_end} on buffer of length {}",
                        self.data.len()
                    )
                    .into());
                }
                BufferType::Growable => {
                    let new_size =
                        write_end.next_multiple_of(2usize.pow(self.alignment_power_of_2));
                    self.data.resize(new_size, 0);
                }
            }
        }
        self.data[pos..write_end].copy_from_slice(data);
        Ok(())
    }

    fn read_at(&self, pos: usize, data: &mut [u8]) -> Result<(), vm::RuntimeError> {
        let read_end = pos + data.len();
        if self.data.len() < read_end {
            return Err(format!(
                "read to pos {read_end} on buffer of length {}",
                self.data.len()
            )
            .into());
        }
        data.copy_from_slice(&self.data[pos..read_end]);
        Ok(())
    }
}

pub fn buffer_lib<'gc>(ctx: vm::Context<'gc>, lib: &mut vm::MagicSet<'gc>) {
    for (name, buffer_type) in [
        ("buffer_fixed", BufferType::Fixed),
        ("buffer_grow", BufferType::Growable),
    ] {
        lib.insert(
            ctx.intern(name),
            MagicConstant::new_ptr(&ctx, vm::UserData::new_static(&ctx, buffer_type)),
        );
    }

    for (name, buffer_seek) in [
        ("buffer_seek_start", BufferSeek::Start),
        ("buffer_seek_relative", BufferSeek::Relative),
        ("buffer_seek_end", BufferSeek::End),
    ] {
        lib.insert(
            ctx.intern(name),
            MagicConstant::new_ptr(&ctx, vm::UserData::new_static(&ctx, buffer_seek)),
        );
    }

    for (name, data_type) in [
        ("buffer_u8", DataType::U8),
        ("buffer_i8", DataType::I8),
        ("buffer_u16", DataType::U16),
        ("buffer_i16", DataType::I16),
        ("buffer_u32", DataType::U32),
        ("buffer_i32", DataType::I32),
        ("buffer_u64", DataType::U64),
        ("buffer_f32", DataType::F32),
        ("buffer_f64", DataType::F64),
        ("buffer_bool", DataType::Bool),
        ("buffer_string", DataType::String),
        ("buffer_text", DataType::Text),
    ] {
        lib.insert(
            ctx.intern(name),
            MagicConstant::new_ptr(&ctx, vm::UserData::new_static(&ctx, data_type)),
        );
    }

    let buffer_create = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (size, buf_type, alignment): (usize, vm::UserData, usize) =
            exec.stack().consume(ctx)?;
        if !alignment.is_power_of_two() {
            return Err(format!("buffer alignment {alignment} is not a power of 2").into());
        }
        let buf_type = *buf_type.downcast_static::<BufferType>()?;
        exec.stack().push_back(vm::UserData::new_static(
            &ctx,
            Buffer::new(buf_type, size, alignment),
        ));
        Ok(())
    });
    lib.insert(
        ctx.intern("buffer_create"),
        MagicConstant::new_ptr(&ctx, buffer_create),
    );

    let buffer_write = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (buffer, data_type, value): (vm::UserData, vm::UserData, vm::Value) =
            exec.stack().consume(ctx)?;
        let mut buffer = buffer.downcast_static::<Buffer>()?.inner.borrow_mut();

        macro_rules! write_value {
            ($val_ty:ty) => {{
                let v: $val_ty = vm::FromValue::from_value(ctx, value)?;
                buffer.cursor_write(&v.to_ne_bytes())?;
            }};
        }
        match *data_type.downcast_static::<DataType>()? {
            DataType::U8 => write_value!(u8),
            DataType::I8 => write_value!(i8),
            DataType::U16 => write_value!(u16),
            DataType::I16 => write_value!(i16),
            DataType::U32 => write_value!(u32),
            DataType::I32 => write_value!(i32),
            DataType::U64 => write_value!(i64),
            DataType::F32 => write_value!(f32),
            DataType::F64 => write_value!(f64),
            DataType::Bool => {
                let b = value.cast_bool();
                buffer.cursor_write(if b { &[1] } else { &[0] })?;
            }
            DataType::String => {
                // `buffer_string` is documented to write a string including a trailing NUL. In
                // order to make sure that writes and reads can be synchronized, we are interpreting
                // that here as writing a string which does *not* contain NUL followed by a single
                // NUL.
                let s: vm::String = vm::FromValue::from_value(ctx, value)?;
                if let Some(end) = s.find('\0') {
                    // If the string has an embedded NUL, write the part up to and including the
                    // first NUL.
                    buffer.cursor_write(s[0..=end].as_bytes())?;
                } else {
                    // If the string does not have an embedded NUL, write the whole string followed
                    // by a NUL.
                    buffer.cursor_write(s.as_str().as_bytes())?;
                    buffer.cursor_write_unaligned(&[0])?;
                }
            }
            DataType::Text => {
                // `buffer_text` is documented as writing the string without a trailing NUL.
                //
                // We write the *entire* string here and assume that the string byte length is
                // separately stored.
                let s: vm::String = vm::FromValue::from_value(ctx, value)?;
                buffer.cursor_write(s.as_str().as_bytes())?;
            }
        }
        Ok(())
    });
    lib.insert(
        ctx.intern("buffer_write"),
        MagicConstant::new_ptr(&ctx, buffer_write),
    );

    let buffer_seek = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (buffer, seek_type, seek): (vm::UserData, vm::UserData, isize) =
            exec.stack().consume(ctx)?;
        let mut buffer = buffer.downcast_static::<Buffer>()?.inner.borrow_mut();
        let seek_type = *seek_type.downcast_static::<BufferSeek>()?;

        let base = match seek_type {
            BufferSeek::Start => 0,
            BufferSeek::Relative => buffer.cursor,
            BufferSeek::End => buffer.data.len(),
        };
        buffer.cursor = base.saturating_add_signed(seek).min(buffer.data.len());
        Ok(())
    });
    lib.insert(
        ctx.intern("buffer_seek"),
        MagicConstant::new_ptr(&ctx, buffer_seek),
    );

    let buffer_read = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let (buffer, data_type): (vm::UserData, vm::UserData) = exec.stack().consume(ctx)?;
        let mut buffer = buffer.downcast_static::<Buffer>()?.inner.borrow_mut();
        let data_type = *data_type.downcast_static::<DataType>()?;

        macro_rules! read_value {
            ($val_ty:ty) => {{
                let mut bytes = [0; _];
                buffer.cursor_read(&mut bytes)?;
                vm::IntoValue::into_value(<$val_ty>::from_ne_bytes(bytes), ctx)
            }};
        }
        let v = match data_type {
            DataType::U8 => read_value!(u8),
            DataType::I8 => read_value!(i8),
            DataType::U16 => read_value!(u16),
            DataType::I16 => read_value!(i16),
            DataType::U32 => read_value!(u32),
            DataType::I32 => read_value!(i32),
            DataType::U64 => read_value!(i64),
            DataType::F32 => read_value!(f32),
            DataType::F64 => read_value!(f64),
            DataType::Bool => {
                let mut bytes = [0; 1];
                buffer.cursor_read(&mut bytes)?;
                (bytes[0] != 0).into()
            }
            DataType::String => {
                // Read a string up until the next NUL. If there is no NUL character found, this
                // will error.
                let string = ctx.intern(str::from_utf8(buffer.cursor_read_until_nul()?)?);
                string.into()
            }
            DataType::Text => {
                // Read the entire rest of the buffer as a string, or until encountering the first
                // NUL character.
                let string = ctx.intern(str::from_utf8(buffer.cursor_read_until_nul_or_end())?);
                string.into()
            }
        };

        exec.stack().push_back(v);
        Ok(())
    });
    lib.insert(
        ctx.intern("buffer_read"),
        MagicConstant::new_ptr(&ctx, buffer_read),
    );

    let buffer_delete = vm::Callback::from_fn(&ctx, |ctx, mut exec| {
        let buffer: vm::UserData = exec.stack().consume(ctx)?;
        let mut buffer = buffer.downcast_static::<Buffer>()?.inner.borrow_mut();

        buffer.data = Vec::new();
        buffer.cursor = 0;

        Ok(())
    });
    lib.insert(
        ctx.intern("buffer_delete"),
        MagicConstant::new_ptr(&ctx, buffer_delete),
    );
}
