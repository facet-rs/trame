//! All traits used to represent a runtime on which trame can operate.

mod live;
mod verified;

use std::{alloc::Layout, marker::PhantomData};

/// A heap and a shape implementation, over which Trame can be parameterized
trait IRuntime {
    type Shape: IShape;
    type Heap: IHeap<Self::Shape>;
    type Arena: IArena;
}

// ==================================================================
// Shape
// ==================================================================

/// A store of shapes that can be looked up by handle.
pub trait IShapeStore: Clone {
    /// The handle type used to reference shapes in this store.
    type Handle: Copy;

    /// Look up a shape by handle.
    fn get(&self, handle: Self::Handle) -> DynShapeView<'_, Self>;
}

/// A view into a shape, borrowing from a store.
///
/// This is the common currency for working with shapes regardless of
/// whether they come from static memory or a DynShapeStore.
#[derive(Clone, Copy)]
pub struct DynShapeView<'a, Store: IShapeStore + ?Sized> {
    pub store: &'a Store,
    pub handle: Store::Handle,
}

/// Common interface for shapes.
///
/// Implemented by:
/// - `&'static facet_core::Shape` (real shapes)
/// - `DynShapeView` (synthetic shapes for Kani)
///
/// The `PartialEq` bound allows the Heap to verify shapes match on dealloc/drop.
pub trait IShape: Copy + PartialEq {
    /// The struct type returned by `as_struct()`.
    type StructType: IStructType<Field = Self::Field>;

    /// The field type used by struct types.
    type Field: IField<Shape = Self>;

    /// Get the layout (size and alignment) of this shape.
    fn layout(&self) -> Layout;

    /// Check if this is a struct type.
    fn is_struct(&self) -> bool;

    /// Get struct-specific information, if this is a struct.
    fn as_struct(&self) -> Option<Self::StructType>;

    /// Drop the value at the given pointer.
    #[deprecated(note = "this is divorced from the heap, therefore it can't help us catch bugs")]
    unsafe fn drop_in_place(&self, ptr: *mut u8);

    /// Default-initialize the value at the given pointer.
    #[deprecated(note = "this is divorced from the heap, therefore it can't help us catch bugs")]
    unsafe fn default_in_place(&self, ptr: *mut u8) -> bool;
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
    unsafe fn alloc(&mut self, shape: S) -> Self::Ptr;

    /// Deallocate a region.
    unsafe fn dealloc(&mut self, ptr: Self::Ptr, shape: S);

    /// Copy `len` bytes from `src` to `dst`.
    unsafe fn memcpy(&mut self, dst: Self::Ptr, src: Self::Ptr, len: usize);

    /// Drop the value at `ptr` and mark the range as uninitialized.
    unsafe fn drop_in_place(&mut self, ptr: Self::Ptr, shape: S);
}

/// Pointer type
pub trait IPtr: Copy {
    /// Compute a new pointer at a byte offset from this one.
    fn byte_add(self, n: usize) -> Self;
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
