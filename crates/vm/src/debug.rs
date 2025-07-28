use std::{fmt, ops, string::String as StdString, sync::Arc};

use gc_arena::{Collect, Gc, Mutation, Rootable, arena::Root, barrier};

use crate::any::{Any, AnyInner};

/// A region of some chunk, expressed in byte offsets.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Collect)]
#[collect(require_static)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Create a new `Span`.
    ///
    /// # Panics
    ///
    /// Panics if `start` is not less than or equal to `end`.
    #[must_use]
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start <= end);
        Self { start, end }
    }

    /// Returns a maximally empty span with `start` set to `usize::MAX` and `end` set to `0`.
    ///
    /// Combining any span with a null span will result in the combined span.
    #[must_use]
    pub fn null() -> Self {
        Self {
            start: usize::MAX,
            end: 0,
        }
    }

    /// Returns true if this span is the special null span.
    #[must_use]
    pub fn is_null(&self) -> bool {
        *self == Self::null()
    }

    /// Returns an empty span starting at the given position.
    #[must_use]
    pub fn empty(start: usize) -> Self {
        Self { start, end: start }
    }

    /// Returns true if `start` is not strictly less than `end`.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    #[must_use]
    pub fn start(&self) -> usize {
        self.start
    }

    #[must_use]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Return a span that encloses both this span and the given span.
    #[must_use]
    pub fn combine(&self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

/// A line number within a chunk.
///
/// It is stored as 0-indexed internally, but will display as a more human-readable 1-indexed line
/// number.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct LineNumber(pub usize);

impl fmt::Display for LineNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0 + 1)
    }
}

/// A static, shared string for named references.
///
/// Static so that it can be stored within error types.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct RefName(Arc<StdString>);

impl fmt::Display for RefName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl RefName {
    pub fn new(name: impl Into<StdString>) -> Self {
        Self(Arc::new(name.into()))
    }
}

impl ops::Deref for RefName {
    type Target = StdString;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A trait for representing a single unit of GML source code, generally a single source file, for
/// the purposes of displaying debug information.
pub trait ChunkData {
    /// The name of this chunk, usually the name of the source code file.
    #[must_use]
    fn name(&self) -> &RefName;

    /// Returns the line number for a given byte offset.
    #[must_use]
    fn line_number(&self, byte_offset: usize) -> LineNumber;
}

impl<T: ChunkData> ChunkData for gc_arena::Static<T> {
    fn name(&self) -> &RefName {
        self.0.name()
    }

    fn line_number(&self, byte_offset: usize) -> LineNumber {
        self.0.line_number(byte_offset)
    }
}

#[derive(Debug, Copy, Clone)]
struct ChunkMethods {
    name: for<'gc> fn(Any<'gc, ChunkMeta>) -> &'gc RefName,
    line_number: for<'gc> fn(Any<'gc, ChunkMeta>, usize) -> LineNumber,
}

impl ChunkMethods {
    fn new<R>() -> &'static Self
    where
        R: for<'a> Rootable<'a> + 'static,
        for<'gc> Root<'gc, R>: Sized + ChunkData + Collect<'gc>,
    {
        &Self {
            name: |any| any.downcast::<R>().unwrap().name(),
            line_number: |any, byte_offset| any.downcast::<R>().unwrap().line_number(byte_offset),
        }
    }
}

/// Meta-data for a `Chunk` type.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(require_static)]
pub struct ChunkMeta {
    methods: &'static ChunkMethods,
}

pub type ChunkInner = AnyInner<ChunkMeta>;

/// A handle to metadata for a single unit of GML source code, for the purposes of displaying debug
/// information.
///
/// Internally holds an implementation of `ChunkData` and allows for downcasting.
#[derive(Debug, Copy, Clone, Collect)]
#[collect(no_drop)]
pub struct Chunk<'gc>(Any<'gc, ChunkMeta>);

impl<'gc> PartialEq for Chunk<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<'gc> Eq for Chunk<'gc> {}

impl<'gc> Chunk<'gc> {
    pub fn new<R>(mc: &Mutation<'gc>, chunk: Root<'gc, R>) -> Self
    where
        R: for<'a> Rootable<'a> + 'static,
        for<'a> Root<'a, R>: Sized + ChunkData + Collect<'a>,
    {
        Self(Any::with_metadata::<R>(
            mc,
            ChunkMeta {
                methods: ChunkMethods::new::<R>(),
            },
            chunk,
        ))
    }

    pub fn new_static<T>(mc: &Mutation<'gc>, val: T) -> Self
    where
        T: ChunkData + 'static,
    {
        Self::new::<gc_arena::Static<T>>(mc, val.into())
    }

    pub fn from_inner(inner: Gc<'gc, ChunkInner>) -> Self {
        Self(Any::from_inner(inner))
    }

    pub fn into_inner(self) -> Gc<'gc, ChunkInner> {
        self.0.into_inner()
    }

    pub fn downcast<R>(self) -> Option<&'gc Root<'gc, R>>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast::<R>()
    }

    pub fn downcast_write<R>(self, mc: &Mutation<'gc>) -> Option<&'gc barrier::Write<Root<'gc, R>>>
    where
        R: for<'b> Rootable<'b> + 'static,
        Root<'gc, R>: Sized,
    {
        self.0.downcast_write::<R>(mc)
    }

    pub fn downcast_static<T: 'static>(self) -> Option<&'gc T> {
        self.downcast::<gc_arena::Static<T>>().map(|r| &r.0)
    }

    pub fn name(self) -> &'gc RefName {
        (self.0.metadata().methods.name)(self.0)
    }

    pub fn line_number(self, byte_offset: usize) -> LineNumber {
        (self.0.metadata().methods.line_number)(self.0, byte_offset)
    }
}
