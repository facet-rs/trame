//! Heap abstraction for memory operations.
//!
//! Verified heap with state tracking for Kani.
//!
//! The key insight: we have ONE implementation of business logic that uses
//! the Heap trait. For verification, we swap in VerifiedHeap which tracks
//! byte-level initialization state.
//!
//! Unlike the old Backend trait which tracked per-slot state, this Heap trait
//! models actual pointer arithmetic and byte-range initialization, which is
//! needed for nested struct construction without separate allocations.

use crate::byte_range::{ByteRangeError, ByteRangeTracker};
use crate::ptr::{IPtr, Ptr};
use crate::shape::{IField, IShape, IStructType};

/// Maximum number of allocations tracked by VerifiedHeap.
pub const MAX_ALLOCS: usize = 8;

// ============================================================================
// VerifiedHeap - state tracking for Kani proofs
// ============================================================================

/// Error from heap operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapError {
    /// Tried to access a freed allocation.
    AllocationFreed,
    /// Shape mismatch on dealloc or drop.
    ShapeMismatch,
    /// Pointer not at allocation start.
    NotAtStart,
    /// Allocation still has initialized bytes.
    NotFullyUninitialized,
    /// Byte range error.
    ByteRange(ByteRangeError),
    /// Too many allocations.
    TooManyAllocations,
}

impl From<ByteRangeError> for HeapError {
    fn from(e: ByteRangeError) -> Self {
        HeapError::ByteRange(e)
    }
}

/// Verified heap that tracks state for Kani proofs.
///
/// This heap doesn't do any real memory operations - it just tracks
/// the abstract state of allocations and asserts that all transitions are valid.
#[derive(Debug)]
pub struct VerifiedHeap<S: IShape> {
    /// Each allocation's initialization state and shape.
    /// None = freed, Some((tracker, shape)) = live allocation.
    allocs: [Option<(ByteRangeTracker, S)>; MAX_ALLOCS],
    /// Next allocation ID to assign.
    next_id: u8,
}

impl<S: IShape> VerifiedHeap<S> {
    /// Create a new verified heap with no allocations.
    pub fn new() -> Self {
        Self {
            allocs: [const { None }; MAX_ALLOCS],
            next_id: 0,
        }
    }

    /// Check that all allocations have been freed (for leak detection).
    #[cfg(test)]
    pub fn assert_no_leaks(&self) {
        for (i, alloc) in self.allocs.iter().enumerate() {
            assert!(alloc.is_none(), "allocation {} not freed", i);
        }
    }

    /// Get the tracker for an allocation, panicking if freed.
    fn get_tracker(&self, alloc_id: u8) -> &(ByteRangeTracker, S) {
        self.allocs[alloc_id as usize]
            .as_ref()
            .expect("allocation already freed")
    }

    /// Get the tracker mutably for an allocation, panicking if freed.
    fn get_tracker_mut(&mut self, alloc_id: u8) -> &mut (ByteRangeTracker, S) {
        self.allocs[alloc_id as usize]
            .as_mut()
            .expect("allocation already freed")
    }
}

impl<S: IShape> Default for VerifiedHeap<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S: IShape> VerifiedHeap<S> {
    fn matches_subshape(stored: S, offset: usize, target: S) -> bool {
        if offset == 0 && stored == target {
            return true;
        }

        if !stored.is_struct() {
            return false;
        }

        let st = stored.as_struct().expect("struct shape");
        let field_count = st.field_count();
        for i in 0..field_count {
            let field = st.field(i).expect("field index in range");
            let field_shape = field.shape();
            let field_size = field_shape
                .layout()
                .expect("IShape requires sized types")
                .size();
            let start = field.offset();
            let end = start + field_size;
            if offset >= start && offset < end {
                return Self::matches_subshape(field_shape, offset - start, target);
            }
        }

        false
    }
}

impl<S: IShape> IHeap<S> for VerifiedHeap<S> {
    type Ptr = Ptr;

    fn alloc(&mut self, shape: S) -> Ptr {
        let id = self.next_id;
        assert!(
            (id as usize) < MAX_ALLOCS,
            "too many allocations (max {})",
            MAX_ALLOCS
        );

        let layout = shape.layout().expect("IShape requires sized types");
        self.allocs[id as usize] = Some((ByteRangeTracker::new(), shape));
        self.next_id += 1;

        Ptr::new(id, layout.size() as u32)
    }

    fn dealloc(&mut self, ptr: Ptr, shape: S) {
        assert!(
            ptr.is_at_start(),
            "dealloc requires pointer to allocation start (offset {} != 0)",
            ptr.offset_bytes()
        );

        let (tracker, stored_shape) = self.get_tracker(ptr.alloc_id());

        assert!(*stored_shape == shape, "dealloc: shape mismatch");
        assert!(
            tracker.is_empty(),
            "dealloc: allocation still has initialized bytes (did you forget to drop?)"
        );

        self.allocs[ptr.alloc_id() as usize] = None;
    }

    fn memcpy(&mut self, dst: Ptr, src: Ptr, len: usize) {
        if len == 0 {
            return;
        }

        // Bounds checks
        assert!(
            dst.offset_bytes() + len <= dst.alloc_size(),
            "memcpy: dst out of bounds"
        );
        assert!(
            src.offset_bytes() + len <= src.alloc_size(),
            "memcpy: src out of bounds"
        );

        // Check src is initialized
        let (src_tracker, _) = self.get_tracker(src.alloc_id());
        assert!(
            src_tracker.is_init(src.offset as u32, src.offset as u32 + len as u32),
            "memcpy: src range not initialized"
        );

        // Check dst is uninitialized and mark it initialized
        let (dst_tracker, _) = self.get_tracker_mut(dst.alloc_id());
        dst_tracker
            .mark_init(dst.offset as u32, dst.offset as u32 + len as u32)
            .expect("memcpy: dst range already initialized (forgot to drop?)");
    }

    unsafe fn drop_in_place(&mut self, ptr: Ptr, shape: S) {
        let layout = shape.layout().expect("IShape requires sized types");
        if layout.size() == 0 {
            return; // ZST - nothing to drop
        }

        let (tracker, stored_shape) = self.get_tracker_mut(ptr.alloc_id());

        assert!(
            Self::matches_subshape(*stored_shape, ptr.offset_bytes(), shape),
            "drop_in_place: shape mismatch"
        );

        assert!(
            ptr.offset_bytes() + layout.size() <= ptr.alloc_size(),
            "drop_in_place: out of bounds"
        );

        let base = ptr.offset as u32;
        let end = base + layout.size() as u32;

        if shape.is_struct() {
            let st = shape.as_struct().expect("struct shape");
            let field_count = st.field_count();

            for i in 0..field_count {
                let field = st.field(i).expect("field index in range");
                let field_shape = field.shape();
                let field_size = field_shape
                    .layout()
                    .expect("IShape requires sized types")
                    .size() as u32;
                if field_size == 0 {
                    continue;
                }
                let start = base + field.offset() as u32;
                let field_end = start + field_size;
                assert!(field_end <= end, "drop_in_place: field out of bounds");

                tracker
                    .mark_uninit(start, field_end)
                    .expect("drop_in_place: field range not initialized");
            }

            tracker
                .clear_range(base, end)
                .expect("drop_in_place: clear_range failed");
            return;
        }

        tracker
            .mark_uninit(base, end)
            .expect("drop_in_place: range not initialized");
    }

    fn mark_init(&mut self, ptr: Ptr, len: usize) {
        if len == 0 {
            return;
        }

        // Bounds check
        assert!(
            ptr.offset_bytes() + len <= ptr.alloc_size(),
            "mark_init: out of bounds"
        );

        let (tracker, _) = self.get_tracker_mut(ptr.alloc_id());
        tracker
            .mark_init(ptr.offset as u32, ptr.offset as u32 + len as u32)
            .expect("mark_init: range already initialized");
    }

    fn is_init(&self, ptr: Ptr, len: usize) -> bool {
        if len == 0 {
            return true;
        }

        let (tracker, _) = self.get_tracker(ptr.alloc_id());
        tracker.is_init(ptr.offset as u32, ptr.offset as u32 + len as u32)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shape::{DynShapeDef, DynShapeStore, DynShapeView};
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;

    // --- VerifiedHeap tests ---

    #[test]
    fn verified_alloc_dealloc() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        assert!(ptr.is_at_start());
        assert_eq!(ptr.alloc_size(), 4);

        heap.dealloc(ptr, shape);
        heap.assert_no_leaks();
    }

    #[test]
    fn verified_mark_init_drop() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        assert!(!heap.is_init(ptr, 4));
        heap.mark_init(ptr, 4);
        assert!(heap.is_init(ptr, 4));

        unsafe { heap.drop_in_place(ptr, shape) };
        assert!(!heap.is_init(ptr, 4));

        heap.dealloc(ptr, shape);
        heap.assert_no_leaks();
    }

    #[test]
    fn verified_memcpy() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let src = heap.alloc(shape);
        let dst = heap.alloc(shape);

        // Initialize src
        heap.mark_init(src, 4);

        // Copy to dst
        heap.memcpy(dst, src, 4);

        // Both should be initialized
        assert!(heap.is_init(src, 4));
        assert!(heap.is_init(dst, 4));

        // Cleanup
        unsafe { heap.drop_in_place(src, shape) };
        unsafe { heap.drop_in_place(dst, shape) };
        heap.dealloc(src, shape);
        heap.dealloc(dst, shape);
        heap.assert_no_leaks();
    }

    #[test]
    #[should_panic(expected = "already initialized")]
    fn verified_double_init_panics() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        heap.mark_init(ptr, 4);
        heap.mark_init(ptr, 4); // Should panic
    }

    #[test]
    #[should_panic(expected = "still has initialized bytes")]
    fn verified_dealloc_while_init_panics() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        heap.mark_init(ptr, 4);
        heap.dealloc(ptr, shape); // Should panic - didn't drop
    }

    #[test]
    #[should_panic(expected = "not initialized")]
    fn verified_drop_uninit_panics() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        unsafe { heap.drop_in_place(ptr, shape) }; // Should panic - never initialized
    }

    #[test]
    fn verified_partial_init() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        // 8-byte struct with two u32 fields
        let h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, u32_h)],
        ));

        let shape = store.view(h);
        let u32_shape = store.view(u32_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        // Init first 4 bytes
        heap.mark_init(ptr, 4);
        assert!(heap.is_init(ptr, 4));
        assert!(!heap.is_init(ptr.offset(4), 4));

        // Init next 4 bytes
        heap.mark_init(ptr.offset(4), 4);
        assert!(heap.is_init(ptr, 8));

        // Cleanup (drop in reverse order for variety)
        unsafe { heap.drop_in_place(ptr.offset(4), u32_shape) };
        unsafe { heap.drop_in_place(ptr, u32_shape) };

        heap.dealloc(ptr, shape);
        heap.assert_no_leaks();
    }

    #[test]
    fn verified_struct_drop_ignores_padding() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (8, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        // Initialize only the fields (leave padding uninitialized).
        heap.mark_init(ptr, 4);
        heap.mark_init(ptr.offset(8), 4);

        unsafe { heap.drop_in_place(ptr, shape) };

        heap.dealloc(ptr, shape);
        heap.assert_no_leaks();
    }

    #[test]
    fn verified_struct_drop_clears_padding_if_initialized() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (8, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        // Initialize the full struct range including padding.
        heap.mark_init(ptr, 12);

        unsafe { heap.drop_in_place(ptr, shape) };

        heap.dealloc(ptr, shape);
        heap.assert_no_leaks();
    }

    #[test]
    fn verified_nested_struct_pointer_arithmetic() {
        // This test demonstrates the key fix for issue #3:
        // nested structs use pointer arithmetic, not separate allocations.
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 } at 8 bytes
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, u32_h)],
        ));

        // Outer struct: { x: u32, inner: Inner } at 12 bytes
        // x at offset 0, inner at offset 4
        let outer_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, inner_h)],
        ));

        let outer_shape = store.view(outer_h);
        let u32_shape = store.view(u32_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();

        // Allocate outer struct - ONE allocation for the whole thing
        let outer_ptr = heap.alloc(outer_shape);
        assert_eq!(outer_ptr.alloc_size(), 12);

        // Pointer arithmetic to get inner struct location
        let inner_ptr = outer_ptr.offset(4);
        assert_eq!(inner_ptr.offset_bytes(), 4);
        assert_eq!(inner_ptr.alloc_id(), outer_ptr.alloc_id()); // SAME allocation!

        // Init outer.x (4 bytes at offset 0)
        heap.mark_init(outer_ptr, 4);

        // Init inner.a (4 bytes at offset 4)
        heap.mark_init(inner_ptr, 4);

        // Init inner.b (4 bytes at offset 8)
        heap.mark_init(inner_ptr.offset(4), 4);

        // All 12 bytes should be initialized
        assert!(heap.is_init(outer_ptr, 12));

        // Cleanup: drop fields
        unsafe { heap.drop_in_place(outer_ptr, u32_shape) }; // x
        unsafe { heap.drop_in_place(inner_ptr, u32_shape) }; // inner.a
        unsafe { heap.drop_in_place(inner_ptr.offset(4), u32_shape) }; // inner.b

        // Dealloc the single outer allocation
        heap.dealloc(outer_ptr, outer_shape);
        heap.assert_no_leaks();
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;
    use crate::shape::{DynShapeDef, DynShapeStore, DynShapeView};
    use core::alloc::Layout;

    type S<'a> = DynShapeView<'a, DynShapeStore>;

    /// Prove: alloc -> init -> drop -> dealloc is valid
    #[kani::proof]
    #[kani::unwind(5)]
    fn alloc_init_drop_dealloc() {
        let size: u32 = kani::any();
        kani::assume(size > 0 && size <= 64);

        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(
            Layout::from_size_align(size as usize, 1).unwrap(),
        ));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        kani::assert(!heap.is_init(ptr, size as usize), "starts uninit");

        heap.mark_init(ptr, size as usize);
        kani::assert(heap.is_init(ptr, size as usize), "now init");

        unsafe { heap.drop_in_place(ptr, shape) };
        kani::assert(!heap.is_init(ptr, size as usize), "now uninit");

        heap.dealloc(ptr, shape);
    }

    /// Prove: memcpy from initialized to uninitialized succeeds
    #[kani::proof]
    #[kani::unwind(5)]
    fn memcpy_init_to_uninit() {
        let size: u32 = kani::any();
        kani::assume(size > 0 && size <= 32);

        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(
            Layout::from_size_align(size as usize, 1).unwrap(),
        ));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let src = heap.alloc(shape);
        let dst = heap.alloc(shape);

        heap.mark_init(src, size as usize);
        heap.memcpy(dst, src, size as usize);

        kani::assert(heap.is_init(src, size as usize), "src still init");
        kani::assert(heap.is_init(dst, size as usize), "dst now init");
    }

    /// Prove: pointer arithmetic preserves allocation identity
    #[kani::proof]
    #[kani::unwind(5)]
    fn pointer_arithmetic_same_alloc() {
        let size: u32 = kani::any();
        let offset: u32 = kani::any();
        kani::assume(size > 0 && size <= 64);
        kani::assume(offset <= size);

        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(
            Layout::from_size_align(size as usize, 1).unwrap(),
        ));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        let offset_ptr = ptr.offset(offset as usize);

        kani::assert(
            offset_ptr.alloc_id() == ptr.alloc_id(),
            "same allocation after offset",
        );
        kani::assert(
            offset_ptr.alloc_size() == ptr.alloc_size(),
            "same size after offset",
        );
    }

    /// Prove: partial initialization tracks correctly
    #[kani::proof]
    #[kani::unwind(5)]
    fn partial_init_tracking() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::from_size_align(8, 1).unwrap()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(shape);

        // Init first half
        heap.mark_init(ptr, 4);

        kani::assert(heap.is_init(ptr, 4), "first half init");
        kani::assert(!heap.is_init(ptr.offset(4), 4), "second half uninit");
        kani::assert(!heap.is_init(ptr, 8), "full range not init");

        // Init second half
        heap.mark_init(ptr.offset(4), 4);

        kani::assert(heap.is_init(ptr, 8), "full range now init");
    }

    /// Prove: multiple allocations have distinct IDs
    #[kani::proof]
    #[kani::unwind(5)]
    fn multiple_allocs_distinct() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let shape = store.view(h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr1 = heap.alloc(shape);
        let ptr2 = heap.alloc(shape);

        kani::assert(ptr1.alloc_id() != ptr2.alloc_id(), "distinct alloc ids");
    }

    /// Prove: nested struct pattern with single allocation
    #[kani::proof]
    #[kani::unwind(5)]
    fn nested_struct_single_alloc() {
        // Simulate: struct Outer { x: u32, inner: struct Inner { a: u32, b: u32 } }
        // Total size: 12 bytes

        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
        let inner_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, u32_h)],
        ));
        let outer_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, inner_h)],
        ));
        let outer_shape = store.view(outer_h);
        let u32_shape = store.view(u32_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();

        // Single allocation for outer
        let outer_ptr = heap.alloc(outer_shape);
        kani::assert(outer_ptr.alloc_size() == 12, "outer is 12 bytes");

        // Pointer arithmetic for inner struct (offset 4)
        let inner_ptr = outer_ptr.offset(4);
        kani::assert(
            inner_ptr.alloc_id() == outer_ptr.alloc_id(),
            "same allocation",
        );
        kani::assert(inner_ptr.offset_bytes() == 4, "at offset 4");

        // Init all fields using pointer arithmetic
        heap.mark_init(outer_ptr, 4); // x at offset 0
        heap.mark_init(inner_ptr, 4); // inner.a at offset 4
        heap.mark_init(inner_ptr.offset(4), 4); // inner.b at offset 8

        // Verify full struct is initialized
        kani::assert(heap.is_init(outer_ptr, 12), "full struct init");

        // Drop all fields
        unsafe { heap.drop_in_place(outer_ptr, u32_shape) };
        unsafe { heap.drop_in_place(inner_ptr, u32_shape) };
        unsafe { heap.drop_in_place(inner_ptr.offset(4), u32_shape) };

        // Dealloc outer
        heap.dealloc(outer_ptr, outer_shape);
    }

    /// Prove: dropping a subshape clears only that range.
    #[kani::proof]
    #[kani::unwind(5)]
    fn drop_subshape_preserves_other_field() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let outer_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, u32_h)],
        ));

        let outer_shape = store.view(outer_h);
        let u32_shape = store.view(u32_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(outer_shape);

        heap.mark_init(ptr, 4);
        heap.mark_init(ptr.offset(4), 4);

        unsafe { heap.drop_in_place(ptr.offset(4), u32_shape) };

        kani::assert(heap.is_init(ptr, 4), "field 0 still init");
        kani::assert(!heap.is_init(ptr.offset(4), 4), "field 1 cleared");
    }

    /// Prove: dropping a nested struct clears only its fields.
    #[kani::proof]
    #[kani::unwind(5)]
    fn drop_nested_struct_clears_inner_only() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let inner_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, u32_h)],
        ));
        let outer_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (4, inner_h)],
        ));

        let outer_shape = store.view(outer_h);
        let inner_shape = store.view(inner_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let outer_ptr = heap.alloc(outer_shape);
        let inner_ptr = outer_ptr.offset(4);

        heap.mark_init(outer_ptr, 4); // outer.x
        heap.mark_init(inner_ptr, 8); // inner.a + inner.b

        unsafe { heap.drop_in_place(inner_ptr, inner_shape) };

        kani::assert(heap.is_init(outer_ptr, 4), "outer.x still init");
        kani::assert(!heap.is_init(inner_ptr, 8), "inner cleared");
    }

    /// Prove: dropping a struct ignores uninitialized padding.
    #[kani::proof]
    #[kani::unwind(5)]
    fn drop_struct_ignores_padding() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let outer_h = store.add(DynShapeDef::struct_with_fields(
            &store,
            &[(0, u32_h), (8, u32_h)],
        ));

        let outer_shape = store.view(outer_h);

        let mut heap = VerifiedHeap::<S<'_>>::new();
        let ptr = heap.alloc(outer_shape);

        heap.mark_init(ptr, 4);
        heap.mark_init(ptr.offset(8), 4);

        unsafe { heap.drop_in_place(ptr, outer_shape) };

        kani::assert(!heap.is_init(ptr, 4), "field 0 cleared");
        kani::assert(!heap.is_init(ptr.offset(8), 4), "field 1 cleared");
    }
}
