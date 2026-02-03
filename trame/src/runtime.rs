//! All traits used to represent a runtime on which trame can operate.

pub mod live;
pub mod verified;

use std::{alloc::Layout, marker::PhantomData};

use crate::node::Node;

/// A heap and a shape implementation, over which Trame can be parameterized
pub trait IRuntime {
    type Shape: IShape;
    type Heap: IHeap<Self::Shape, Ptr: IPtr>;
    type Arena: IArena<Node<Self::Heap, Self::Shape>>;

    fn heap() -> Self::Heap;
    fn arena() -> Self::Arena;
}

/// Marker trait for runtimes that use real facet shapes and raw pointers.
pub trait LiveRuntime:
    IRuntime<Shape = &'static facet_core::Shape, Heap: IHeap<&'static facet_core::Shape, Ptr = *mut u8>>
{
}

impl<T> LiveRuntime for T where
    T: IRuntime<
            Shape = &'static facet_core::Shape,
            Heap: IHeap<&'static facet_core::Shape, Ptr = *mut u8>,
        >
{
}

// ==================================================================
// Shape
// ==================================================================

/// A store of shapes that can be looked up by handle.
pub trait IShapeStore: Clone {
    /// The handle type used to reference shapes in this store.
    type Handle: Copy;

    /// The view type produced by this store.
    type View<'a>: IShape
    where
        Self: 'a;

    /// Look up a shape by handle.
    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a>;
}

/// Common interface for shapes.
///
/// Implemented by:
/// - `&'static facet_core::Shape` (real shapes)
/// - store-specific shape views (synthetic shapes for verification)
///
/// The `PartialEq` bound allows the Heap to verify shapes match on dealloc/drop.
pub trait IShape: Copy + PartialEq {
    /// The struct type returned by `as_struct()`.
    type StructType: IStructType<Field = Self::Field>;

    /// The field type used by struct types.
    type Field: IField<Shape = Self>;

    /// Get the layout (size and alignment) of this shape.
    ///
    /// Returns `None` for unsized types.
    fn layout(&self) -> Option<Layout>;

    /// Check if this is a struct type.
    fn is_struct(&self) -> bool;

    /// Get struct-specific information, if this is a struct.
    fn as_struct(&self) -> Option<Self::StructType>;
}

/// Interface for struct type information.
pub trait IStructType: Copy {
    /// The field type.
    type Field: IField;

    /// Number of fields in this struct.
    fn field_count(&self) -> usize;

    /// Get field by index.
    fn field(&self, idx: usize) -> Option<Self::Field>;
}

/// Interface for field information.
pub trait IField: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Byte offset of this field within the struct.
    fn offset(&self) -> usize;

    /// Shape of this field's type.
    fn shape(&self) -> Self::Shape;
}

// ==================================================================
// Heap
// ==================================================================

/// Heap for memory operations, generic over shape type.
pub trait IHeap<S: IShape> {
    /// Pointer type used by this heap.
    type Ptr: IPtr;

    /// Allocate a region for a value of the given shape.
    ///
    /// # Safety
    /// The caller must ensure `shape` is valid for allocation and that any
    /// constraints required by the heap implementation are satisfied.
    unsafe fn alloc(&mut self, shape: S) -> Self::Ptr;

    /// Deallocate a region.
    ///
    /// # Safety
    /// The caller must ensure `ptr` points to the start of a live allocation
    /// previously returned by `alloc`, that the allocation corresponds to
    /// `shape`, and that no bytes in the region are still initialized.
    unsafe fn dealloc(&mut self, ptr: Self::Ptr, shape: S);

    /// Copy `len` bytes from `src` to `dst`.
    ///
    /// # Safety
    /// The caller must ensure both ranges are in-bounds for their allocations,
    /// that `src` is fully initialized, `dst` is fully uninitialized, and the
    /// ranges do not overlap.
    unsafe fn memcpy(&mut self, dst: Self::Ptr, src: Self::Ptr, len: usize);

    /// Drop the value at `ptr` and mark the range as uninitialized.
    ///
    /// # Safety
    /// The caller must ensure `ptr` points to a value of type `shape`, the
    /// value is fully initialized, and the allocation is still live.
    unsafe fn drop_in_place(&mut self, ptr: Self::Ptr, shape: S);

    /// Default-initialize the value at `ptr` and mark the range as initialized.
    ///
    /// Returns `false` if the shape has no default.
    ///
    /// # Safety
    /// The caller must ensure the destination range is uninitialized, in-bounds,
    /// and corresponds to `shape`.
    unsafe fn default_in_place(&mut self, ptr: Self::Ptr, shape: S) -> bool;
}

/// Pointer type
pub trait IPtr: Copy {
    /// Compute a new pointer at a byte offset from this one.
    ///
    /// # Safety
    /// The caller must ensure the resulting pointer is in-bounds.
    unsafe fn byte_add(self, n: usize) -> Self;
}

impl IPtr for *mut u8 {
    #[inline]
    unsafe fn byte_add(self, n: usize) -> Self {
        // SAFETY: caller ensures the resulting pointer is in-bounds.
        unsafe { self.byte_add(n) }
    }
}

// ============================================================================
// Arena
// ==================================================================

/// Arena for allocating and managing items.
pub trait IArena<T> {
    /// Allocate a new item, returning its index.
    fn alloc(&mut self, value: T) -> Idx<T>;

    /// Free an item, returning it.
    ///
    /// # Panics
    /// Panics if the index is invalid or already freed.
    fn free(&mut self, id: Idx<T>) -> T;

    /// Get a reference to an item.
    ///
    /// # Panics
    /// Panics if the index is invalid or freed.
    fn get(&self, id: Idx<T>) -> &T;

    /// Get a mutable reference to an item.
    ///
    /// # Panics
    /// Panics if the index is invalid or freed.
    fn get_mut(&mut self, id: Idx<T>) -> &mut T;
}

/// A typed index into an arena.
///
/// The phantom type prevents mixing indices from different arenas.
#[derive(Debug)]
pub struct Idx<T> {
    raw: u32,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Idx<T> {}

impl<T> Idx<T> {
    /// Sentinel: slot not started (reserved, slot 0)
    pub const NOT_STARTED: Self = Self {
        raw: 0,
        _ty: PhantomData,
    };

    /// Sentinel: slot completed/freed
    pub const COMPLETE: Self = Self {
        raw: u32::MAX,
        _ty: PhantomData,
    };

    #[inline]
    pub fn is_not_started(self) -> bool {
        self.raw == 0
    }

    #[inline]
    pub fn is_complete(self) -> bool {
        self.raw == u32::MAX
    }

    #[inline]
    pub fn is_valid(self) -> bool {
        self.raw != 0 && self.raw != u32::MAX
    }

    #[inline]
    fn index(self) -> usize {
        debug_assert!(self.is_valid(), "cannot get index of sentinel");
        self.raw as usize
    }

    /// Create an index from a raw value (for internal use).
    fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _ty: PhantomData,
        }
    }
}

// ==================================================================
// Tests
// ==================================================================

// #[cfg(test)]
// mod tests {
//     use super::Idx;

//     #[test]
//     fn idx_sentinels() {
//         assert!(Idx::<u32>::NOT_STARTED.is_not_started());
//         assert!(!Idx::<u32>::NOT_STARTED.is_complete());
//         assert!(!Idx::<u32>::NOT_STARTED.is_valid());

//         assert!(!Idx::<u32>::COMPLETE.is_not_started());
//         assert!(Idx::<u32>::COMPLETE.is_complete());
//         assert!(!Idx::<u32>::COMPLETE.is_valid());
//     }
// }

// #[cfg(kani)]
// mod kani_proofs {
//     use super::Idx;

//     #[kani::proof]
//     fn idx_sentinels_are_distinct() {
//         let not_started: Idx<u32> = Idx::NOT_STARTED;
//         let complete: Idx<u32> = Idx::COMPLETE;

//         kani::assert(not_started.raw != complete.raw, "sentinels must differ");
//         kani::assert(!not_started.is_valid(), "NOT_STARTED is not valid");
//         kani::assert(!complete.is_valid(), "COMPLETE is not valid");
//     }

//     #[kani::proof]
//     fn is_valid_excludes_sentinels() {
//         let raw: u32 = kani::any();

//         let idx: Idx<u32> = Idx::from_raw(raw);

//         let expected_valid = raw != 0 && raw != u32::MAX;
//         kani::assert(idx.is_valid() == expected_valid, "is_valid correctness");
//     }
// }
