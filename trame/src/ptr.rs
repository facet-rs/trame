//! Fat pointer type for pointer-based heap.
//!
//! The `Ptr` type is a fat pointer that carries allocation metadata:
//! - Which allocation it points into (`alloc_id`)
//! - Current byte offset within the allocation (`offset`)
//! - Total size of the allocation (`size`) for bounds checking
//!
//! Key property: `Ptr` is `Copy` and contains all information needed for
//! bounds checking. It doesn't borrow the Heap.

/// Fat pointer for verified heap operations.
///
/// Contains allocation metadata for bounds checking without borrowing the heap.
/// Used by `VerifiedHeap` to track pointers into allocations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ptr {
    /// Which allocation this points into.
    pub alloc_id: u8,
    /// Current byte offset within the allocation.
    pub offset: u32,
    /// Total size of the allocation (for bounds checking).
    pub size: u32,
}

/// Pointer-like behavior used by the Heap and Partial layers.
pub trait PtrLike: Copy {
    /// Compute a new pointer at a byte offset from this one.
    fn offset(self, n: usize) -> Self;
}

impl PtrLike for Ptr {
    #[inline]
    fn offset(self, n: usize) -> Self {
        Ptr::offset(self, n)
    }
}

impl PtrLike for *mut u8 {
    #[inline]
    fn offset(self, n: usize) -> Self {
        // SAFETY: caller ensures the resulting pointer is in-bounds.
        unsafe { self.add(n) }
    }
}

impl Ptr {
    /// Create a new pointer to the start of an allocation.
    #[inline]
    pub const fn new(alloc_id: u8, size: u32) -> Self {
        Self {
            alloc_id,
            offset: 0,
            size,
        }
    }

    /// Compute a new pointer at an offset from this one.
    ///
    /// # Panics
    /// Panics if the resulting offset would exceed the allocation size.
    #[inline]
    pub fn offset(self, n: usize) -> Self {
        let new_offset = self.offset.checked_add(n as u32).expect("offset overflow");
        assert!(
            new_offset <= self.size,
            "pointer arithmetic out of bounds: offset {} + {} = {} > size {}",
            self.offset,
            n,
            new_offset,
            self.size
        );
        Self {
            alloc_id: self.alloc_id,
            offset: new_offset,
            size: self.size,
        }
    }

    /// Returns the current offset within the allocation.
    #[inline]
    pub const fn offset_bytes(self) -> usize {
        self.offset as usize
    }

    /// Returns the allocation ID.
    #[inline]
    pub const fn alloc_id(self) -> u8 {
        self.alloc_id
    }

    /// Returns the total size of the allocation.
    #[inline]
    pub const fn alloc_size(self) -> usize {
        self.size as usize
    }

    /// Check if this pointer is at the start of its allocation.
    #[inline]
    pub const fn is_at_start(self) -> bool {
        self.offset == 0
    }

    /// Returns the number of bytes remaining from this offset to end of allocation.
    #[inline]
    pub const fn remaining(self) -> usize {
        (self.size - self.offset) as usize
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_ptr_at_start() {
        let ptr = Ptr::new(0, 16);
        assert_eq!(ptr.alloc_id(), 0);
        assert_eq!(ptr.offset_bytes(), 0);
        assert_eq!(ptr.alloc_size(), 16);
        assert!(ptr.is_at_start());
        assert_eq!(ptr.remaining(), 16);
    }

    #[test]
    fn offset_within_bounds() {
        let ptr = Ptr::new(0, 16);
        let ptr2 = ptr.offset(8);
        assert_eq!(ptr2.alloc_id(), 0);
        assert_eq!(ptr2.offset_bytes(), 8);
        assert_eq!(ptr2.alloc_size(), 16);
        assert!(!ptr2.is_at_start());
        assert_eq!(ptr2.remaining(), 8);
    }

    #[test]
    fn offset_to_end() {
        let ptr = Ptr::new(0, 16);
        let ptr2 = ptr.offset(16);
        assert_eq!(ptr2.offset_bytes(), 16);
        assert_eq!(ptr2.remaining(), 0);
    }

    #[test]
    fn chained_offsets() {
        let ptr = Ptr::new(0, 32);
        let ptr2 = ptr.offset(8).offset(8).offset(8);
        assert_eq!(ptr2.offset_bytes(), 24);
        assert_eq!(ptr2.remaining(), 8);
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn offset_past_end_panics() {
        let ptr = Ptr::new(0, 16);
        let _ = ptr.offset(17);
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn chained_offset_past_end_panics() {
        let ptr = Ptr::new(0, 16);
        let _ = ptr.offset(8).offset(9);
    }

    #[test]
    fn different_alloc_ids() {
        let ptr1 = Ptr::new(0, 16);
        let ptr2 = Ptr::new(1, 32);
        assert_ne!(ptr1.alloc_id(), ptr2.alloc_id());
    }

    #[test]
    fn ptr_equality() {
        let ptr1 = Ptr::new(0, 16);
        let ptr2 = Ptr::new(0, 16);
        assert_eq!(ptr1, ptr2);

        let ptr3 = ptr1.offset(4);
        let ptr4 = ptr2.offset(4);
        assert_eq!(ptr3, ptr4);

        // Different offset
        let ptr5 = ptr1.offset(8);
        assert_ne!(ptr3, ptr5);

        // Different alloc_id
        let ptr6 = Ptr::new(1, 16);
        assert_ne!(ptr1, ptr6);
    }

    #[test]
    fn zero_size_allocation() {
        let ptr = Ptr::new(0, 0);
        assert!(ptr.is_at_start());
        assert_eq!(ptr.remaining(), 0);
        // Can offset by 0 even in zero-size allocation
        let ptr2 = ptr.offset(0);
        assert_eq!(ptr2.offset_bytes(), 0);
    }

    #[test]
    #[should_panic(expected = "out of bounds")]
    fn zero_size_nonzero_offset_panics() {
        let ptr = Ptr::new(0, 0);
        let _ = ptr.offset(1);
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;

    /// Prove: offset preserves alloc_id and size
    #[kani::proof]
    fn offset_preserves_metadata() {
        let alloc_id: u8 = kani::any();
        let size: u32 = kani::any();
        let offset: u32 = kani::any();

        kani::assume(size <= 1024); // Bound for tractability
        kani::assume(offset <= size);

        let ptr = Ptr::new(alloc_id, size);
        let ptr2 = ptr.offset(offset as usize);

        kani::assert(ptr2.alloc_id() == alloc_id, "alloc_id preserved");
        kani::assert(ptr2.alloc_size() == size as usize, "size preserved");
        kani::assert(ptr2.offset_bytes() == offset as usize, "offset correct");
    }

    /// Prove: chained offsets are additive
    #[kani::proof]
    fn chained_offsets_additive() {
        let size: u32 = kani::any();
        let offset1: u32 = kani::any();
        let offset2: u32 = kani::any();

        kani::assume(size <= 256);
        kani::assume(offset1 <= size);
        kani::assume(offset2 <= size - offset1);

        let ptr = Ptr::new(0, size);
        let ptr_chained = ptr.offset(offset1 as usize).offset(offset2 as usize);
        let ptr_direct = ptr.offset((offset1 + offset2) as usize);

        kani::assert(ptr_chained == ptr_direct, "chained offsets equal direct");
    }

    /// Prove: remaining + offset_bytes == size
    #[kani::proof]
    fn remaining_plus_offset_equals_size() {
        let size: u32 = kani::any();
        let offset: u32 = kani::any();

        kani::assume(size <= 1024);
        kani::assume(offset <= size);

        let ptr = Ptr::new(0, size);
        let ptr2 = ptr.offset(offset as usize);

        kani::assert(
            ptr2.remaining() + ptr2.offset_bytes() == size as usize,
            "remaining + offset == size",
        );
    }

    /// Prove: is_at_start iff offset == 0
    #[kani::proof]
    fn is_at_start_iff_zero_offset() {
        let size: u32 = kani::any();
        let offset: u32 = kani::any();

        kani::assume(size <= 256);
        kani::assume(offset <= size);

        let ptr = Ptr::new(0, size);
        let ptr2 = ptr.offset(offset as usize);

        kani::assert(
            ptr2.is_at_start() == (offset == 0),
            "is_at_start iff offset is 0",
        );
    }
}
