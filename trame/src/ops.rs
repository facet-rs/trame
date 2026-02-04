//! Operations for partial value construction.

mod builder;

use core::marker::PhantomData;
use std::collections::VecDeque;

#[cfg(creusot)]
use creusot_std::prelude::DeepModel;

#[cfg(creusot)]
use creusot_std::std::vec::vec as creusot_vec;

#[cfg(all(not(kani), not(creusot)))]
use smallvec::SmallVec;

#[cfg(all(not(kani), not(creusot)))]
type PathVec = SmallVec<PathSegment, 4>;

#[cfg(any(kani, creusot))]
type PathVec = Vec<PathSegment>;

/// Creates a PathVec with the given elements.
macro_rules! path_vec {
    ($($x:expr),* $(,)?) => {{
        #[cfg(creusot)]
        { creusot_vec![$($x),*] }
        #[cfg(all(not(creusot), not(kani)))]
        { smallvec::smallvec![$($x),*] }
        #[cfg(all(not(creusot), kani))]
        { vec![$($x),*] }
    }};
}

/// A segment in a path through a nested structure.
#[cfg_attr(creusot, derive(DeepModel))]
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
#[derive(Clone, Debug, Default)]
pub struct Path(PathVec);

impl Path {
    /// Create an empty path.
    pub fn empty() -> Self {
        Self(PathVec::new())
    }

    /// Create a path with a single field segment.
    pub fn field(n: u32) -> Self {
        Self(path_vec![PathSegment::Field(n)])
    }

    /// Create a path with a single append segment.
    pub fn append() -> Self {
        Self(path_vec![PathSegment::Append])
    }

    /// Create a path starting from root.
    pub fn root() -> Self {
        Self(path_vec![PathSegment::Root])
    }

    /// Create a path from a slice of segments.
    pub fn from_segments(segments: &[PathSegment]) -> Self {
        Self(segments.iter().cloned().collect())
    }

    /// Add a segment to the path.
    pub fn then(mut self, seg: PathSegment) -> Self {
        self.0.push(seg);
        self
    }

    /// Add a field segment to the path.
    pub fn then_field(self, n: u32) -> Self {
        self.then(PathSegment::Field(n))
    }

    /// Add an append segment to the path.
    pub fn then_append(self) -> Self {
        self.then(PathSegment::Append)
    }

    /// Returns the path segments as a slice.
    pub fn segments(&self) -> &[PathSegment] {
        &self.0
    }

    /// Returns true if the path is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Returns the number of segments in the path.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    // --- Deprecated methods for migration ---

    /// Push a field index onto the path (deprecated: use then_field).
    #[deprecated(note = "Use Path::field(n) or path.then_field(n) instead")]
    pub fn push(&mut self, index: u32) {
        self.0.push(PathSegment::Field(index));
    }

    /// Returns the path as field indices.
    /// Panics if any segment is not a Field.
    /// (deprecated: use segments() instead)
    #[deprecated(note = "Use segments() instead")]
    pub fn as_slice(&self) -> Vec<u32> {
        self.0
            .iter()
            .map(|seg| match seg {
                PathSegment::Field(n) => *n,
                _ => panic!("as_slice() requires all segments to be Field"),
            })
            .collect()
    }

    /// Returns the first field index, if the path starts with a Field.
    /// Panics if path is empty or starts with non-Field.
    /// (deprecated: use segments() instead)
    pub fn first_field_index(&self) -> Option<u32> {
        match self.0.first() {
            Some(PathSegment::Field(n)) => Some(*n),
            _ => None,
        }
    }
}

impl<const N: usize> From<[PathSegment; N]> for Path {
    fn from(value: [PathSegment; N]) -> Self {
        Self(value.into_iter().collect())
    }
}

impl From<&[PathSegment]> for Path {
    fn from(value: &[PathSegment]) -> Self {
        Self::from_segments(value)
    }
}

/// How to fill a value.
///
/// This type is opaque - you can only construct it through the provided methods,
/// which ensures the pointer/shape pairing is always valid.
pub struct Source<'facet, P, S> {
    pub(crate) kind: SourceKind<'facet, P, S>,
}

/// Internal representation of a source.
pub(crate) enum SourceKind<'facet, P, S> {
    /// Copy bytes from this pointer, which has the given shape.
    Imm(Imm<'facet, P, S>),
    /// Stage for incremental construction - pushes a frame.
    Stage(Option<usize>),
    /// Use the type's default value.
    Default,
}

/// A value to move into the destination.
///
/// The lifetime `'facet` ensures the source pointer remains valid until the
/// operation is applied.
pub struct Imm<'facet, P, S> {
    pub(crate) ptr: P,
    pub(crate) shape: S,
    _marker: PhantomData<&'facet ()>,
}

impl<'facet, P, S> Source<'facet, P, S> {
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
            SourceKind::Imm(imm) => Some(&imm.shape),
            _ => None,
        }
    }
}

// ============================================================================
// Live runtime constructors (facet shapes, raw pointers)
// ============================================================================

impl<'facet> Source<'facet, *mut u8, &'static facet_core::Shape> {
    /// Build an immediate source from a typed reference.
    ///
    /// This is the only safe way to create an immediate source for live runtimes.
    /// The shape is derived from the type's Facet implementation.
    pub fn from_ref<T: facet_core::Facet<'facet>>(t: &'facet mut T) -> Self {
        Source {
            kind: SourceKind::Imm(Imm {
                ptr: t as *mut T as *mut u8,
                shape: T::SHAPE,
                _marker: PhantomData,
            }),
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
            kind: SourceKind::Imm(Imm {
                ptr,
                shape,
                _marker: PhantomData,
            }),
        }
    }
}

// ============================================================================
// Verified runtime constructors (VShapeView, VPtr)
// ============================================================================

#[cfg(not(creusot))]
impl<'facet, S: Copy> Source<'facet, trame_runtime::verified::VPtr, S> {
    /// Build an immediate source from a VPtr and its shape.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `ptr` was allocated with the given `shape`.
    /// In practice, this is enforced by VHeap which tracks shapes for all allocations.
    pub unsafe fn from_vptr(ptr: trame_runtime::verified::VPtr, shape: S) -> Self {
        Source {
            kind: SourceKind::Imm(Imm {
                ptr,
                shape,
                _marker: PhantomData,
            }),
        }
    }
}

/// An operation on a Partial.
pub enum Op<'facet, P, S> {
    /// Set a value at a path relative to the current frame.
    Set {
        dst: Path,
        src: Source<'facet, P, S>,
    },
    /// End the current frame and pop back to parent.
    End,
}

impl<'facet, P, S> Op<'facet, P, S> {
    /// Create an End operation.
    pub fn end() -> Self {
        Op::End
    }
}

/// A batch of operations to apply.
///
/// Operations are stored in a `VecDeque` and consumed (popped) from the front
/// as they are processed. After applying a batch:
/// - Consumed ops have been removed from the batch (caller must forget their sources)
/// - Remaining ops in the batch were NOT consumed (caller should drop them normally)
pub struct OpBatch<'facet, P, S> {
    ops: VecDeque<Op<'facet, P, S>>,
}

impl<'facet, P, S> OpBatch<'facet, P, S> {
    /// Create a new empty batch.
    pub fn new() -> Self {
        Self {
            ops: VecDeque::new(),
        }
    }

    /// Create a batch with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            ops: VecDeque::with_capacity(capacity),
        }
    }

    /// Add an operation to the back of the batch.
    pub fn push(&mut self, op: Op<'facet, P, S>) {
        self.ops.push_back(op);
    }

    /// Pop an operation from the front of the batch.
    pub fn pop(&mut self) -> Option<Op<'facet, P, S>> {
        self.ops.pop_front()
    }

    /// Push an operation back to the front of the batch.
    pub fn push_front(&mut self, op: Op<'facet, P, S>) {
        self.ops.push_front(op);
    }

    /// Get the number of operations in the batch.
    pub fn len(&self) -> usize {
        self.ops.len()
    }

    /// Returns true if the batch is empty.
    pub fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }
}

impl<'facet, P, S> Default for OpBatch<'facet, P, S> {
    fn default() -> Self {
        Self::new()
    }
}

pub use builder::SetBuilder;
