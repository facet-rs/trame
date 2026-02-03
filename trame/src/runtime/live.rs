//! Live implementations of all of trame's runtime traits: no verification involved, real memory
//! allocations, etc.

use crate::node::Node;
use crate::runtime::{IArena, IField, IHeap, IRuntime, IShape, IShapeStore, IStructType, Idx};
use facet_core::{Field, Shape, StructType, Type, UserType};

/// A "live" runtime that just peforms raw unsafe Rust operations
pub struct LRuntime;

impl IRuntime for LRuntime {
    type Shape = &'static Shape;
    type Heap = LHeap;
    type Arena = LArena<Node<Self::Heap, Self::Shape>>;
    type ShapeStore = LShapeStore;

    fn parts() -> (Self::ShapeStore, Self::Heap, Self::Arena) {
        (LShapeStore, LHeap::new(), LArena::new())
    }
}

// ==================================================================
// Shape
// ==================================================================

/// Live shape store: handle and view are the static shape itself.
#[derive(Clone, Copy, Default)]
pub struct LShapeStore;

impl IShapeStore for LShapeStore {
    type Handle = &'static Shape;
    type View<'a>
        = &'static Shape
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        handle
    }
}

impl IShape for &'static Shape {
    type StructType = &'static StructType;
    type Field = &'static Field;

    #[inline]
    fn layout(&self) -> Option<std::alloc::Layout> {
        self.layout.sized_layout().ok()
    }

    #[inline]
    fn is_struct(&self) -> bool {
        matches!(self.ty, Type::User(UserType::Struct(_)))
    }

    #[inline]
    fn as_struct(&self) -> Option<Self::StructType> {
        match &self.ty {
            Type::User(UserType::Struct(st)) => Some(st),
            _ => None,
        }
    }
}

impl IStructType for &'static StructType {
    type Field = &'static Field;

    #[inline]
    fn field_count(&self) -> usize {
        self.fields.len()
    }

    #[inline]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        self.fields.get(idx)
    }
}

impl IField for &'static Field {
    type Shape = &'static Shape;

    #[inline]
    fn offset(&self) -> usize {
        self.offset
    }

    #[inline]
    fn shape(&self) -> Self::Shape {
        self.shape.get()
    }
}

// ==================================================================
// Heap
// ==================================================================

/// Live heap that performs actual memory operations.
#[derive(Debug)]
pub struct LHeap;

impl LHeap {
    /// Create a new live heap.
    pub const fn new() -> Self {
        Self
    }
}

impl Default for LHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl IHeap<&'static Shape> for LHeap {
    type Ptr = *mut u8;

    unsafe fn alloc(&mut self, shape: &'static Shape) -> *mut u8 {
        let layout = shape.layout();
        if let Some(layout) = layout {
            if layout.size() == 0 {
                // "Note that layouts are not required to have non-zero size, even though
                // GlobalAlloc requires that all memory requests be non-zero in size. A caller
                // must either ensure that conditions like this are met, use specific allocators
                // with looser requirements, or use the more lenient Allocator interface"
                // (std::alloc::Layout)
                // https://doc.rust-lang.org/stable/std/alloc/struct.Layout.html
                // ZST - no allocation required
                core::ptr::NonNull::dangling().as_ptr()
            } else {
                // SAFETY: layout.size() > 0
                let ptr = unsafe { std::alloc::alloc(layout) };
                if ptr.is_null() {
                    std::alloc::handle_alloc_error(layout);
                }
                ptr
            }
        } else {
            // No layout available (e.g., ZST/unsized) - return dangling.
            core::ptr::NonNull::dangling().as_ptr()
        }
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, shape: &'static Shape) {
        if let Some(layout) = shape.layout() {
            if layout.size() > 0 {
                // SAFETY: caller guarantees this is a live allocation
                unsafe { std::alloc::dealloc(ptr, layout) };
            }
        }
    }

    unsafe fn memcpy(&mut self, dst: *mut u8, src: *mut u8, len: usize) {
        if len > 0 {
            // SAFETY: caller guarantees non-overlapping, valid pointers
            unsafe {
                core::ptr::copy_nonoverlapping(src, dst, len);
            }
        }
    }

    unsafe fn drop_in_place(&mut self, ptr: *mut u8, shape: &'static Shape) {
        unsafe { shape.call_drop_in_place(facet_core::PtrMut::new(ptr)) };
    }

    unsafe fn default_in_place(&mut self, ptr: *mut u8, shape: &'static Shape) -> bool {
        unsafe {
            shape
                .call_default_in_place(facet_core::PtrMut::new(ptr).into())
                .is_some()
        }
    }
}

// ==================================================================
// Arena
// ==================================================================

/// Vec-based arena with free list for production use.
pub struct LArena<T> {
    slots: Vec<Option<T>>,
    free_list: Vec<u32>,
}

impl<T> LArena<T> {
    /// Create a new live arena.
    pub fn new() -> Self {
        Self {
            slots: vec![None], // Slot 0 reserved for NOT_STARTED
            free_list: Vec::new(),
        }
    }
}

impl<T> Default for LArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IArena<T> for LArena<T> {
    fn alloc(&mut self, value: T) -> Idx<T> {
        let raw = if let Some(idx) = self.free_list.pop() {
            debug_assert!(self.slots[idx as usize].is_none());
            self.slots[idx as usize] = Some(value);
            idx
        } else {
            let idx = self.slots.len();
            assert!(idx < u32::MAX as usize, "arena full");
            self.slots.push(Some(value));
            idx as u32
        };
        Idx::from_raw(raw)
    }

    fn free(&mut self, id: Idx<T>) -> T {
        debug_assert!(id.is_valid());
        let value = self.slots[id.index()].take().expect("double-free");
        self.free_list.push(id.raw);
        value
    }

    fn get(&self, id: Idx<T>) -> &T {
        debug_assert!(id.is_valid());
        self.slots[id.index()].as_ref().expect("slot empty")
    }

    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        debug_assert!(id.is_valid());
        self.slots[id.index()].as_mut().expect("slot empty")
    }
}

// ==================================================================
// Tests
// ==================================================================

// #[cfg(test)]
// mod tests {
//     use super::LArena;

//     #[test]
//     fn live_alloc_and_get() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(42u32);

//         assert!(id.is_valid());
//         assert_eq!(*arena.get(id), 42);
//     }

//     #[test]
//     fn live_free_and_reuse() {
//         let mut arena = LArena::new();

//         let id1 = arena.alloc(1u32);
//         let _id2 = arena.alloc(2u32);

//         let val = arena.free(id1);
//         assert_eq!(val, 1);

//         // Next alloc reuses freed slot
//         let id3 = arena.alloc(3u32);
//         assert_eq!(id3.raw, id1.raw);
//         assert_eq!(*arena.get(id3), 3);
//     }

//     #[test]
//     #[should_panic(expected = "double-free")]
//     fn live_double_free_panics() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(1u32);
//         arena.free(id);
//         arena.free(id);
//     }

//     #[test]
//     fn live_get_mut() {
//         let mut arena = LArena::new();
//         let id = arena.alloc(1u32);

//         *arena.get_mut(id) = 99;
//         assert_eq!(*arena.get(id), 99);
//     }
// }
