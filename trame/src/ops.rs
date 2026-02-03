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
///
/// This type is opaque - you can only construct it through the provided methods,
/// which ensures the pointer/shape pairing is always valid.
pub struct Source<P, S> {
    pub(crate) kind: SourceKind<P, S>,
}

/// Internal representation of a source.
pub(crate) enum SourceKind<P, S> {
    /// Copy bytes from this pointer, which has the given shape.
    Imm { ptr: P, shape: S },
    /// Stage for incremental construction - pushes a frame.
    Stage(Option<usize>),
    /// Use the type's default value.
    Default,
}

impl<P, S> Source<P, S> {
    /// Stage for incremental construction - pushes a frame.
    pub fn stage(len_hint: Option<usize>) -> Self {
        Source {
            kind: SourceKind::Stage(len_hint),
        }
    }

    /// Use the type's default value.
    pub fn default_value() -> Self {
        Source {
            kind: SourceKind::Default,
        }
    }

    /// Get the shape of an immediate source, if this is an immediate.
    pub fn imm_shape(&self) -> Option<&S> {
        match &self.kind {
            SourceKind::Imm { shape, .. } => Some(shape),
            _ => None,
        }
    }
}

// ============================================================================
// Live runtime constructors (facet shapes, raw pointers)
// ============================================================================

impl Source<*mut u8, &'static facet_core::Shape> {
    /// Build an immediate source from a typed reference.
    ///
    /// This is the only safe way to create an immediate source for live runtimes.
    /// The shape is derived from the type's Facet implementation.
    pub fn from_ref<T: for<'a> facet_core::Facet<'a>>(t: &mut T) -> Self {
        Source {
            kind: SourceKind::Imm {
                ptr: t as *mut T as *mut u8,
                shape: T::SHAPE,
            },
        }
    }

    /// Build an immediate source from a raw pointer and shape.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `ptr` points to valid, initialized memory
    /// that matches the given `shape`.
    pub unsafe fn from_ptr_shape(ptr: *mut u8, shape: &'static facet_core::Shape) -> Self {
        Source {
            kind: SourceKind::Imm { ptr, shape },
        }
    }
}

// ============================================================================
// Verified runtime constructors (VShapeView, VPtr)
// ============================================================================

impl<S: Copy> Source<trame_runtime::verified::VPtr, S> {
    /// Build an immediate source from a VPtr and its shape.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `ptr` was allocated with the given `shape`.
    /// In practice, this is enforced by VHeap which tracks shapes for all allocations.
    pub unsafe fn from_vptr(ptr: trame_runtime::verified::VPtr, shape: S) -> Self {
        Source {
            kind: SourceKind::Imm { ptr, shape },
        }
    }
}

/// An operation on a Partial.
pub enum Op<'a, P, S> {
    /// Set a value at a path relative to the current frame.
    Set { dst: Path<'a>, src: Source<P, S> },
    /// End the current frame and pop back to parent.
    End,
}
