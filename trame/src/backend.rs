//! Backend abstraction for memory operations.
//!
//! Two implementations:
//! - `RealBackend`: Actual memory operations (for production)
//! - `VerifiedBackend`: State tracking with assertions (for Kani)
//!
//! The key insight: we have ONE implementation of business logic that uses
//! the Backend trait. For verification, we swap in VerifiedBackend which
//! tracks state and asserts valid transitions. For production, RealBackend
//! performs actual memory operations with zero overhead.

use crate::dyn_shape::{IShape, IStructType};
use core::alloc::Layout;
use core::marker::PhantomData;

/// Maximum number of allocations tracked by VerifiedBackend.
pub const MAX_ALLOCS: usize = 8;

/// Maximum number of slots tracked by VerifiedBackend.
pub const MAX_SLOTS: usize = 32;

/// State of a memory slot.
///
/// The valid state transitions are:
///
/// ```text
/// Unallocated  --alloc-->  Allocated  --init-->  Initialized
///                               ^                     |
///                               |----drop_in_place----|
///                               |
/// Unallocated  <--dealloc-------+
/// ```
///
/// Key invariants:
/// - Cannot init an Unallocated slot (no memory!)
/// - Cannot init an already Initialized slot (double-init!)
/// - Cannot drop an Unallocated or Allocated slot (nothing to drop!)
/// - Cannot dealloc while any slot is Initialized (leak the drop!)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SlotState {
    /// Not allocated - no memory exists for this slot.
    Unallocated,
    /// Allocated but not initialized - memory exists but contains garbage.
    Allocated,
    /// Allocated and initialized - memory contains a valid value.
    Initialized,
}

/// Backend for memory operations, generic over shape type.
///
/// # Safety
///
/// All methods are unsafe because they have preconditions that cannot be
/// checked at compile time. The VerifiedBackend asserts these preconditions
/// for Kani proofs; RealBackend assumes they hold (undefined behavior otherwise).
pub trait Backend<S> {
    /// Handle to an allocation (a contiguous region of slots).
    type Alloc: Copy;

    /// Handle to a single slot within an allocation.
    type Slot: Copy;

    /// Allocate memory for a shape.
    ///
    /// # Safety
    /// - Caller must eventually call `dealloc` to avoid leaks
    /// - All slots start in `Allocated` state
    unsafe fn alloc(&mut self, shape: S) -> Self::Alloc;

    /// Deallocate memory.
    ///
    /// # Safety
    /// - `alloc` must be a live allocation (not already freed)
    /// - All slots must be in `Allocated` state (not `Initialized` - drop first!)
    unsafe fn dealloc(&mut self, alloc: Self::Alloc);

    /// Get a slot handle for a field within an allocation.
    ///
    /// # Safety
    /// - `alloc` must be a live allocation
    /// - `field_idx` must be within bounds for the shape
    unsafe fn slot(&self, alloc: Self::Alloc, field_idx: usize) -> Self::Slot;

    /// Mark a slot as initialized.
    ///
    /// # Safety
    /// - `slot` must be valid
    /// - Slot must be in `Allocated` state (not Unallocated or Initialized)
    /// - Memory must actually be initialized before calling this
    unsafe fn mark_init(&mut self, slot: Self::Slot);

    /// Mark a slot as uninitialized (after drop_in_place).
    ///
    /// # Safety
    /// - `slot` must be valid
    /// - Slot must be in `Initialized` state
    /// - `drop_in_place` must have been called on the memory
    unsafe fn mark_uninit(&mut self, slot: Self::Slot);

    /// Check if a slot is initialized.
    ///
    /// # Safety
    /// - `slot` must be valid
    unsafe fn is_init(&self, slot: Self::Slot) -> bool;
}

/// Verified backend that tracks state for Kani proofs.
///
/// This backend doesn't do any real memory operations - it just tracks
/// the abstract state of slots and asserts that all transitions are valid.
#[derive(Debug)]
pub struct VerifiedBackend<S> {
    /// State of each slot.
    slots: [SlotState; MAX_SLOTS],
    /// Next slot index to allocate from.
    next_slot: usize,
    /// Allocations: Some((start_slot, slot_count)) if live, None if freed.
    allocs: [Option<(u16, u16)>; MAX_ALLOCS],
    /// Next allocation index.
    next_alloc: usize,
    /// Marker for the shape type.
    _marker: PhantomData<S>,
}

impl<S> VerifiedBackend<S> {
    /// Create a new verified backend with all slots unallocated.
    pub fn new() -> Self {
        Self {
            slots: [SlotState::Unallocated; MAX_SLOTS],
            next_slot: 0,
            allocs: [None; MAX_ALLOCS],
            next_alloc: 0,
            _marker: PhantomData,
        }
    }

    /// Check that all allocations have been freed (for leak detection).
    #[cfg(test)]
    pub fn assert_no_leaks(&self) {
        for (i, alloc) in self.allocs.iter().enumerate() {
            assert!(alloc.is_none(), "allocation {} not freed: {:?}", i, alloc);
        }
    }
}

impl<S> Default for VerifiedBackend<S> {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper to get slot count from a shape.
fn slot_count<S: IShape>(shape: S) -> usize {
    if let Some(st) = shape.as_struct() {
        st.field_count()
    } else {
        1 // Scalars have 1 slot
    }
}

impl<S: IShape> Backend<S> for VerifiedBackend<S> {
    type Alloc = u8;
    type Slot = u16;

    unsafe fn alloc(&mut self, shape: S) -> Self::Alloc {
        let slots = slot_count(shape);

        assert!(self.next_alloc < MAX_ALLOCS, "too many allocations");
        assert!(self.next_slot + slots <= MAX_SLOTS, "too many slots");

        let alloc_id = self.next_alloc;
        let start_slot = self.next_slot;

        // Mark slots as allocated
        for i in 0..slots {
            assert!(
                self.slots[start_slot + i] == SlotState::Unallocated,
                "slot {} already allocated",
                start_slot + i
            );
            self.slots[start_slot + i] = SlotState::Allocated;
        }

        self.allocs[alloc_id] = Some((start_slot as u16, slots as u16));
        self.next_alloc += 1;
        self.next_slot += slots;

        alloc_id as u8
    }

    unsafe fn dealloc(&mut self, alloc: Self::Alloc) {
        let alloc_idx = alloc as usize;
        let (start, count) = self.allocs[alloc_idx].expect("dealloc: already freed");

        // Verify all slots are Allocated (not Initialized - must drop first!)
        for i in 0..(count as usize) {
            let slot_idx = (start as usize) + i;
            assert!(
                self.slots[slot_idx] == SlotState::Allocated,
                "dealloc: slot {} is {:?}, expected Allocated (did you forget to drop?)",
                slot_idx,
                self.slots[slot_idx]
            );
            self.slots[slot_idx] = SlotState::Unallocated;
        }

        self.allocs[alloc_idx] = None;
    }

    unsafe fn slot(&self, alloc: Self::Alloc, field_idx: usize) -> Self::Slot {
        let alloc_idx = alloc as usize;
        let (start, count) = self.allocs[alloc_idx].expect("slot: allocation not live");

        assert!(
            field_idx < count as usize,
            "slot: field_idx {} out of bounds (count {})",
            field_idx,
            count
        );

        (start as usize + field_idx) as u16
    }

    unsafe fn mark_init(&mut self, slot: Self::Slot) {
        let idx = slot as usize;
        assert!(
            self.slots[idx] == SlotState::Allocated,
            "mark_init: slot {} is {:?}, expected Allocated",
            idx,
            self.slots[idx]
        );
        self.slots[idx] = SlotState::Initialized;
    }

    unsafe fn mark_uninit(&mut self, slot: Self::Slot) {
        let idx = slot as usize;
        assert!(
            self.slots[idx] == SlotState::Initialized,
            "mark_uninit: slot {} is {:?}, expected Initialized",
            idx,
            self.slots[idx]
        );
        self.slots[idx] = SlotState::Allocated;
    }

    unsafe fn is_init(&self, slot: Self::Slot) -> bool {
        self.slots[slot as usize] == SlotState::Initialized
    }
}

// ============================================================================
// RealBackend - production memory operations
// ============================================================================

/// Real backend that performs actual memory operations.
///
/// This is the production backend - it allocates real memory, tracks slot
/// states minimally (just enough to know what to drop), and has zero overhead
/// for the state tracking itself (the slot states are just indices).
///
/// Unlike VerifiedBackend which tracks everything for proofs, RealBackend
/// trusts that callers follow the contract. Violating the contract is UB.
#[derive(Debug)]
pub struct RealBackend<S> {
    /// Marker for the shape type.
    _marker: PhantomData<S>,
}

/// A real allocation - owns heap memory.
#[derive(Debug)]
pub struct RealAlloc {
    /// Pointer to allocated memory (null for ZSTs).
    ptr: *mut u8,
    /// Layout used for allocation (needed for dealloc).
    layout: Layout,
    /// Number of slots in this allocation.
    slot_count: usize,
}

impl Copy for RealAlloc {}
impl Clone for RealAlloc {
    fn clone(&self) -> Self {
        *self
    }
}

/// A real slot - pointer to a field within an allocation.
#[derive(Debug, Clone, Copy)]
pub struct RealSlot {
    /// Pointer to the slot's memory.
    pub ptr: *mut u8,
    /// Layout of this slot (for potential drop operations).
    pub layout: Layout,
}

impl<S> RealBackend<S> {
    /// Create a new real backend.
    pub const fn new() -> Self {
        Self {
            _marker: PhantomData,
        }
    }
}

impl<S> Default for RealBackend<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S: IShape> Backend<S> for RealBackend<S> {
    type Alloc = RealAlloc;
    type Slot = RealSlot;

    unsafe fn alloc(&mut self, shape: S) -> Self::Alloc {
        let layout = shape.layout();
        let slot_count = slot_count(shape);

        let ptr = if layout.size() == 0 {
            // ZST - don't allocate, use dangling pointer
            layout.align() as *mut u8
        } else {
            // SAFETY: layout.size() > 0, so this is valid
            let ptr = unsafe { std::alloc::alloc(layout) };
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            ptr
        };

        RealAlloc {
            ptr,
            layout,
            slot_count,
        }
    }

    unsafe fn dealloc(&mut self, alloc: Self::Alloc) {
        if alloc.layout.size() > 0 {
            // SAFETY: caller guarantees this is a live allocation with all slots
            // in Allocated state (not Initialized)
            unsafe { std::alloc::dealloc(alloc.ptr, alloc.layout) };
        }
    }

    unsafe fn slot(&self, alloc: Self::Alloc, field_idx: usize) -> Self::Slot {
        debug_assert!(
            field_idx < alloc.slot_count,
            "field_idx {} out of bounds (count {})",
            field_idx,
            alloc.slot_count
        );

        // For now, we don't track individual field offsets in RealBackend.
        // This is a simplified version - real usage would need the shape info
        // to compute proper offsets.
        //
        // TODO: Store field offsets in RealAlloc or require shape parameter
        RealSlot {
            ptr: alloc.ptr, // Placeholder - real impl needs offset calculation
            layout: alloc.layout,
        }
    }

    unsafe fn mark_init(&mut self, _slot: Self::Slot) {
        // No-op in production - we trust the caller
    }

    unsafe fn mark_uninit(&mut self, _slot: Self::Slot) {
        // No-op in production - we trust the caller
    }

    unsafe fn is_init(&self, _slot: Self::Slot) -> bool {
        // RealBackend doesn't track this - caller must know
        // This method shouldn't really be called on RealBackend
        panic!("RealBackend::is_init should not be called - caller must track init state")
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dyn_shape::{DynShapeDef, DynShapeStore, DynShapeView};
    use core::alloc::Layout;

    /// Type alias for the shape view we use in tests.
    type S<'a> = DynShapeView<'a, DynShapeStore>;

    // --- VerifiedBackend tests ---

    #[test]
    fn verified_scalar_lifecycle() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };
        let slot = unsafe { backend.slot(alloc, 0) };

        assert!(!unsafe { backend.is_init(slot) });
        unsafe { backend.mark_init(slot) };
        assert!(unsafe { backend.is_init(slot) });
        unsafe { backend.mark_uninit(slot) };
        unsafe { backend.dealloc(alloc) };
        backend.assert_no_leaks();
    }

    #[test]
    fn verified_struct_lifecycle() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        let slot0 = unsafe { backend.slot(alloc, 0) };
        let slot1 = unsafe { backend.slot(alloc, 1) };

        unsafe { backend.mark_init(slot0) };
        unsafe { backend.mark_init(slot1) };
        unsafe { backend.mark_uninit(slot0) };
        unsafe { backend.mark_uninit(slot1) };
        unsafe { backend.dealloc(alloc) };
        backend.assert_no_leaks();
    }

    #[test]
    #[should_panic(expected = "mark_init")]
    fn verified_double_init_panics() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };
        let slot = unsafe { backend.slot(alloc, 0) };

        unsafe { backend.mark_init(slot) };
        unsafe { backend.mark_init(slot) };
    }

    #[test]
    #[should_panic(expected = "dealloc")]
    fn verified_dealloc_while_init_panics() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };
        let slot = unsafe { backend.slot(alloc, 0) };

        unsafe { backend.mark_init(slot) };
        unsafe { backend.dealloc(alloc) };
    }

    // --- RealBackend tests ---

    #[test]
    fn real_scalar_alloc_dealloc() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut backend = RealBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // Write something to verify memory is usable
        unsafe {
            std::ptr::write(alloc.ptr as *mut u32, 42);
            assert_eq!(std::ptr::read(alloc.ptr as *const u32), 42);
        }

        unsafe { backend.dealloc(alloc) };
    }

    #[test]
    fn real_zst_alloc_dealloc() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<()>()));
        let shape = store.view(h);

        let mut backend = RealBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // ZST should have non-null aligned pointer but no actual allocation
        assert!(!alloc.ptr.is_null());

        unsafe { backend.dealloc(alloc) };
    }

    #[test]
    fn real_struct_alloc_dealloc() {
        let mut store = DynShapeStore::new();
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let u64_h = store.add(DynShapeDef::scalar(Layout::new::<u64>()));
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u64_h)]);
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut backend = RealBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // Write to both fields
        unsafe {
            std::ptr::write(alloc.ptr as *mut u32, 123);
            std::ptr::write(alloc.ptr.add(4) as *mut u64, 456);

            assert_eq!(std::ptr::read(alloc.ptr as *const u32), 123);
            assert_eq!(std::ptr::read(alloc.ptr.add(4) as *const u64), 456);
        }

        unsafe { backend.dealloc(alloc) };
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;
    use crate::dyn_shape::{
        DynDef, DynFieldDef, DynShapeDef, DynShapeHandle, DynShapeStore, DynShapeView,
        DynStructDef, MAX_FIELDS,
    };
    use core::alloc::Layout;

    /// Type alias for the shape view we use in proofs.
    type S<'a> = DynShapeView<'a, DynShapeStore>;

    /// Prove: scalar alloc -> init -> uninit -> dealloc is valid
    #[kani::proof]
    #[kani::unwind(10)]
    fn scalar_lifecycle() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
        let shape = store.view(h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };
        let slot = unsafe { backend.slot(alloc, 0) };

        unsafe { backend.mark_init(slot) };
        unsafe { backend.mark_uninit(slot) };
        unsafe { backend.dealloc(alloc) };
    }

    /// Prove: struct with symbolic field count works correctly
    #[kani::proof]
    #[kani::unwind(10)]
    fn struct_lifecycle() {
        let field_count: u8 = kani::any();
        kani::assume(field_count > 0 && field_count <= 4);

        // Build store with a scalar shape for fields
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let mut fields = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
        for i in 0..(field_count as usize) {
            fields[i] = DynFieldDef::new(i * 4, scalar_h);
        }

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count,
                fields,
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // Init all fields
        for i in 0..(field_count as usize) {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_init(slot) };
        }

        // Uninit all fields
        for i in 0..(field_count as usize) {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_uninit(slot) };
        }

        unsafe { backend.dealloc(alloc) };
    }

    /// Prove: fields can be initialized in any order
    #[kani::proof]
    #[kani::unwind(10)]
    fn struct_any_init_order() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 3,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr[2] = DynFieldDef::new(8, scalar_h);
                    arr
                },
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // Choose arbitrary init order via symbolic permutation
        let first: u8 = kani::any();
        let second: u8 = kani::any();
        let third: u8 = kani::any();
        kani::assume(first < 3 && second < 3 && third < 3);
        kani::assume(first != second && second != third && first != third);

        let order = [first as usize, second as usize, third as usize];

        // Init in arbitrary order
        for &i in &order {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_init(slot) };
        }

        // Uninit in reverse order (simulating drop)
        for &i in order.iter().rev() {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_uninit(slot) };
        }

        unsafe { backend.dealloc(alloc) };
    }

    /// Prove: partial init then cleanup is safe (panic safety)
    #[kani::proof]
    #[kani::unwind(10)]
    fn partial_init_cleanup() {
        let mut store = DynShapeStore::new();
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 3,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr[2] = DynFieldDef::new(8, scalar_h);
                    arr
                },
            }),
        };
        let struct_h = store.add(struct_def);
        let shape = store.view(struct_h);

        let mut backend = VerifiedBackend::<S<'_>>::new();
        let alloc = unsafe { backend.alloc(shape) };

        // Init only some fields (0 to k where k is symbolic)
        let init_count: u8 = kani::any();
        kani::assume(init_count <= 3);

        for i in 0..(init_count as usize) {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_init(slot) };
        }

        // "Panic" - cleanup only the initialized fields
        for i in 0..(init_count as usize) {
            let slot = unsafe { backend.slot(alloc, i) };
            unsafe { backend.mark_uninit(slot) };
        }

        // Safe to dealloc - all slots are back to Allocated state
        unsafe { backend.dealloc(alloc) };
    }

    /// Prove: multiple allocations can coexist
    #[kani::proof]
    #[kani::unwind(10)]
    fn multiple_allocations() {
        let mut store = DynShapeStore::new();
        let h1 = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
        let h2 = store.add(DynShapeDef::scalar(Layout::from_size_align(8, 8).unwrap()));

        let shape1 = store.view(h1);
        let shape2 = store.view(h2);

        let mut backend = VerifiedBackend::<S<'_>>::new();

        let alloc1 = unsafe { backend.alloc(shape1) };
        let alloc2 = unsafe { backend.alloc(shape2) };

        let slot1 = unsafe { backend.slot(alloc1, 0) };
        let slot2 = unsafe { backend.slot(alloc2, 0) };

        // Init both
        unsafe { backend.mark_init(slot1) };
        unsafe { backend.mark_init(slot2) };

        // Uninit and dealloc in different order than alloc
        unsafe { backend.mark_uninit(slot1) };
        unsafe { backend.dealloc(alloc1) };

        unsafe { backend.mark_uninit(slot2) };
        unsafe { backend.dealloc(alloc2) };
    }
}
