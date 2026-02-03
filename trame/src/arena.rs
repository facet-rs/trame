//! Arena allocation with pluggable heaps.
//!
//! Two implementations:
//! - `RealArena<T>`: Vec-based with free list (for production)
//! - `VerifiedArena<T, N>`: Fixed-size array (for Kani verification)

use core::marker::PhantomData;

/// Maximum number of arena slots for verification.
pub const MAX_ARENA_SLOTS: usize = 16;

// ============================================================================
// Idx - typed index into an arena
// ============================================================================

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

// ============================================================================
// Arena trait
// ============================================================================

/// Arena for allocating and managing items.
pub trait Arena<T> {
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

// ============================================================================
// VerifiedArena - fixed-size for Kani
// ============================================================================

/// Slot state for verified arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SlotState {
    Empty,
    Occupied,
}

/// Fixed-size arena for Kani verification.
///
/// Uses a fixed-size array instead of Vec to keep state space bounded.
pub struct VerifiedArena<T, const N: usize = MAX_ARENA_SLOTS> {
    /// Slot storage. Index 0 is reserved (NOT_STARTED sentinel).
    /// Using Vec here since we can't initialize [Option<T>; N] without T: Copy.
    slots: [Option<T>; N],
    /// State of each slot.
    states: [SlotState; N],
    /// Next slot to try allocating from.
    next: usize,
}

impl<T, const N: usize> VerifiedArena<T, N> {
    /// Create a new verified arena.
    pub fn new() -> Self {
        Self {
            slots: core::array::from_fn(|_| None),
            states: [SlotState::Empty; N],
            next: 1, // Skip slot 0 (reserved for NOT_STARTED)
        }
    }
}

impl<T, const N: usize> Default for VerifiedArena<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> Arena<T> for VerifiedArena<T, N> {
    fn alloc(&mut self, value: T) -> Idx<T> {
        // Find an empty slot starting from next
        for i in self.next..N {
            if self.states[i] == SlotState::Empty {
                self.slots[i] = Some(value);
                self.states[i] = SlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        // Wrap around and search from beginning (skip slot 0)
        for i in 1..self.next {
            if self.states[i] == SlotState::Empty {
                self.slots[i] = Some(value);
                self.states[i] = SlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        panic!("arena full");
    }

    fn free(&mut self, id: Idx<T>) -> T {
        assert!(id.is_valid(), "cannot free sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == SlotState::Occupied,
            "double-free or freeing empty slot"
        );

        self.states[idx] = SlotState::Empty;
        self.slots[idx].take().expect("slot was occupied but empty")
    }

    fn get(&self, id: Idx<T>) -> &T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == SlotState::Occupied,
            "slot is not occupied"
        );

        self.slots[idx]
            .as_ref()
            .expect("slot was occupied but empty")
    }

    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == SlotState::Occupied,
            "slot is not occupied"
        );

        self.slots[idx]
            .as_mut()
            .expect("slot was occupied but empty")
    }
}

// ============================================================================
// RealArena - Vec-based for production
// ============================================================================

/// Vec-based arena with free list for production use.
pub struct RealArena<T> {
    slots: Vec<Option<T>>,
    free_list: Vec<u32>,
}

impl<T> RealArena<T> {
    /// Create a new real arena.
    pub fn new() -> Self {
        Self {
            slots: vec![None], // Slot 0 reserved for NOT_STARTED
            free_list: Vec::new(),
        }
    }
}

impl<T> Default for RealArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Arena<T> for RealArena<T> {
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn idx_sentinels() {
        assert!(Idx::<u32>::NOT_STARTED.is_not_started());
        assert!(!Idx::<u32>::NOT_STARTED.is_complete());
        assert!(!Idx::<u32>::NOT_STARTED.is_valid());

        assert!(!Idx::<u32>::COMPLETE.is_not_started());
        assert!(Idx::<u32>::COMPLETE.is_complete());
        assert!(!Idx::<u32>::COMPLETE.is_valid());
    }

    // --- VerifiedArena tests ---

    #[test]
    fn verified_alloc_and_get() {
        let mut arena = VerifiedArena::<u32, 8>::new();
        let id = arena.alloc(42);

        assert!(id.is_valid());
        assert_eq!(*arena.get(id), 42);
    }

    #[test]
    fn verified_free_and_reuse() {
        let mut arena = VerifiedArena::<u32, 8>::new();

        let id1 = arena.alloc(1);
        let _id2 = arena.alloc(2);

        let val = arena.free(id1);
        assert_eq!(val, 1);

        // Next alloc can reuse freed slot (or use a new one)
        let id3 = arena.alloc(3);
        assert!(id3.is_valid());
        assert_eq!(*arena.get(id3), 3);
    }

    #[test]
    #[should_panic(expected = "double-free")]
    fn verified_double_free_panics() {
        let mut arena = VerifiedArena::<u32, 8>::new();
        let id = arena.alloc(1);
        arena.free(id);
        arena.free(id);
    }

    #[test]
    fn verified_get_mut() {
        let mut arena = VerifiedArena::<u32, 8>::new();
        let id = arena.alloc(1);

        *arena.get_mut(id) = 99;
        assert_eq!(*arena.get(id), 99);
    }

    // --- RealArena tests ---

    #[test]
    fn real_alloc_and_get() {
        let mut arena = RealArena::new();
        let id = arena.alloc(42u32);

        assert!(id.is_valid());
        assert_eq!(*arena.get(id), 42);
    }

    #[test]
    fn real_free_and_reuse() {
        let mut arena = RealArena::new();

        let id1 = arena.alloc(1u32);
        let _id2 = arena.alloc(2u32);

        let val = arena.free(id1);
        assert_eq!(val, 1);

        // Next alloc reuses freed slot
        let id3 = arena.alloc(3u32);
        assert_eq!(id3.raw, id1.raw);
        assert_eq!(*arena.get(id3), 3);
    }

    #[test]
    #[should_panic(expected = "double-free")]
    fn real_double_free_panics() {
        let mut arena = RealArena::new();
        let id = arena.alloc(1u32);
        arena.free(id);
        arena.free(id);
    }

    #[test]
    fn real_get_mut() {
        let mut arena = RealArena::new();
        let id = arena.alloc(1u32);

        *arena.get_mut(id) = 99;
        assert_eq!(*arena.get(id), 99);
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;

    #[kani::proof]
    fn idx_sentinels_are_distinct() {
        let not_started: Idx<u32> = Idx::NOT_STARTED;
        let complete: Idx<u32> = Idx::COMPLETE;

        kani::assert(not_started.raw != complete.raw, "sentinels must differ");
        kani::assert(!not_started.is_valid(), "NOT_STARTED is not valid");
        kani::assert(!complete.is_valid(), "COMPLETE is not valid");
    }

    #[kani::proof]
    fn is_valid_excludes_sentinels() {
        let raw: u32 = kani::any();

        let idx: Idx<u32> = Idx::from_raw(raw);

        let expected_valid = raw != 0 && raw != u32::MAX;
        kani::assert(idx.is_valid() == expected_valid, "is_valid correctness");
    }

    #[kani::proof]
    #[kani::unwind(6)]
    fn verified_arena_alloc_free() {
        let mut arena = VerifiedArena::<u32, 4>::new();

        let id1 = arena.alloc(1);
        let id2 = arena.alloc(2);

        kani::assert(id1.is_valid(), "id1 is valid");
        kani::assert(id2.is_valid(), "id2 is valid");
        kani::assert(id1 != id2, "ids are distinct");

        kani::assert(*arena.get(id1) == 1, "id1 holds 1");
        kani::assert(*arena.get(id2) == 2, "id2 holds 2");

        let v1 = arena.free(id1);
        kani::assert(v1 == 1, "freed value is 1");

        // Can still access id2
        kani::assert(*arena.get(id2) == 2, "id2 still holds 2");

        // Can alloc again (may reuse id1's slot)
        let id3 = arena.alloc(3);
        kani::assert(id3.is_valid(), "id3 is valid");
        kani::assert(*arena.get(id3) == 3, "id3 holds 3");
    }
}
