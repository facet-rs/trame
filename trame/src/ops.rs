//! Operations for partial value construction.

/// A segment in a path through a nested structure.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PathSegment {
    /// Struct field, tuple element, array index, enum variant.
    Field(u32),
    /// New list/set element or map entry.
    Append,
    /// Jump to root (only valid as first segment).
    Root,
}

/// A path into a nested structure.
pub type Path<'a> = &'a [PathSegment];

/// How to fill a value.
pub enum Source<P> {
    /// Copy bytes from this pointer.
    Imm(P),
    /// Stage for incremental construction - pushes a frame.
    Stage(Option<usize>),
    /// Use the type's default value.
    Default,
}

impl Source<*mut u8> {
    /// Build an immediate from a reference
    pub fn imm_ref<T>(t: &mut T) -> Source<*mut u8> {
        Source::Imm(t as *mut _ as *mut u8)
    }
}

/// An operation on a Partial.
pub enum Op<'a, P> {
    /// Set a value at a path relative to the current frame.
    Set { dst: Path<'a>, src: Source<P> },
    /// End the current frame and pop back to parent.
    End,
}
