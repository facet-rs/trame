//! Byte range tracking for allocation initialization state.
//!
//! Tracks which byte ranges within an allocation are initialized.
//! Used by the verified backend to ensure proper initialization discipline:
//! - No double-init (writing to already-initialized bytes)
//! - No use-after-free (reading uninitialized bytes)
//! - No leaks (deallocating while bytes are still initialized)

/// Maximum number of disjoint ranges we can track.
/// Chosen to be small for Kani verification but large enough for realistic structs.
pub const MAX_RANGES: usize = 16;

/// Tracks initialized byte ranges within an allocation.
///
/// Invariants:
/// - Ranges are sorted by start offset
/// - Ranges are non-overlapping and non-adjacent (merged when possible)
/// - `count` reflects the actual number of valid entries in `ranges`
#[derive(Debug, Clone, Copy)]
pub struct ByteRangeTracker {
    /// Initialized ranges as (start, end) pairs, where end is exclusive.
    /// Only the first `count` entries are valid.
    ranges: [(u32, u32); MAX_RANGES],
    /// Number of valid ranges.
    count: u8,
}

/// Error from byte range operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteRangeError {
    /// Tried to initialize bytes that are already initialized.
    AlreadyInitialized { start: u32, end: u32 },
    /// Tried to uninitialize bytes that aren't initialized.
    NotInitialized { start: u32, end: u32 },
    /// Too many disjoint ranges (would exceed MAX_RANGES).
    TooManyRanges,
    /// Invalid range (start >= end).
    InvalidRange { start: u32, end: u32 },
}

impl ByteRangeTracker {
    /// Create a new tracker with no initialized ranges.
    pub const fn new() -> Self {
        Self {
            ranges: [(0, 0); MAX_RANGES],
            count: 0,
        }
    }

    /// Returns true if no bytes are initialized.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Returns the number of disjoint initialized ranges.
    pub fn range_count(&self) -> usize {
        self.count as usize
    }

    /// Check if a byte range is fully uninitialized.
    pub fn is_uninit(&self, start: u32, end: u32) -> bool {
        if start >= end {
            return true; // Empty range is trivially uninit
        }

        for i in 0..self.count as usize {
            let (r_start, r_end) = self.ranges[i];
            // Check for overlap
            if start < r_end && end > r_start {
                return false;
            }
        }
        true
    }

    /// Check if a byte range is fully initialized.
    pub fn is_init(&self, start: u32, end: u32) -> bool {
        if start >= end {
            return true; // Empty range is trivially init
        }

        for i in 0..self.count as usize {
            let (r_start, r_end) = self.ranges[i];
            // Check if this range fully contains [start, end)
            if r_start <= start && end <= r_end {
                return true;
            }
        }
        false
    }

    /// Mark a byte range as initialized.
    ///
    /// Returns error if any part of the range is already initialized.
    pub fn mark_init(&mut self, start: u32, end: u32) -> Result<(), ByteRangeError> {
        if start >= end {
            return Err(ByteRangeError::InvalidRange { start, end });
        }

        // Check for overlap with existing ranges
        if !self.is_uninit(start, end) {
            return Err(ByteRangeError::AlreadyInitialized { start, end });
        }

        // Find insertion point and check for adjacent ranges to merge
        let mut merge_start = start;
        let mut merge_end = end;
        let mut remove_mask = 0u16; // Bitmap of ranges to remove (merged)

        for i in 0..self.count as usize {
            let (r_start, r_end) = self.ranges[i];

            // Check if adjacent (can merge)
            if r_end == start {
                // This range ends where we start - merge
                merge_start = r_start;
                remove_mask |= 1 << i;
            } else if end == r_start {
                // We end where this range starts - merge
                merge_end = r_end;
                remove_mask |= 1 << i;
            }
        }

        // Remove merged ranges and insert the new merged range
        let ranges_to_remove = remove_mask.count_ones() as usize;
        let new_count = self.count as usize - ranges_to_remove + 1;

        if new_count > MAX_RANGES {
            return Err(ByteRangeError::TooManyRanges);
        }

        // Compact: remove marked ranges
        let mut write_idx = 0;
        for read_idx in 0..self.count as usize {
            if remove_mask & (1 << read_idx) == 0 {
                self.ranges[write_idx] = self.ranges[read_idx];
                write_idx += 1;
            }
        }
        self.count = write_idx as u8;

        // Insert the new/merged range in sorted order
        let mut insert_idx = 0;
        while insert_idx < self.count as usize && self.ranges[insert_idx].0 < merge_start {
            insert_idx += 1;
        }

        // Shift ranges to make room
        for i in (insert_idx..self.count as usize).rev() {
            self.ranges[i + 1] = self.ranges[i];
        }
        self.ranges[insert_idx] = (merge_start, merge_end);
        self.count += 1;

        Ok(())
    }

    /// Mark a byte range as uninitialized.
    ///
    /// Returns error if any part of the range is not initialized.
    pub fn mark_uninit(&mut self, start: u32, end: u32) -> Result<(), ByteRangeError> {
        if start >= end {
            return Err(ByteRangeError::InvalidRange { start, end });
        }

        // Find the range that contains [start, end)
        let mut containing_idx = None;
        for i in 0..self.count as usize {
            let (r_start, r_end) = self.ranges[i];
            if r_start <= start && end <= r_end {
                containing_idx = Some(i);
                break;
            }
        }

        let idx = containing_idx.ok_or(ByteRangeError::NotInitialized { start, end })?;
        let (r_start, r_end) = self.ranges[idx];

        // Four cases:
        // 1. Exact match: remove the range
        // 2. Remove from start: shrink range
        // 3. Remove from end: shrink range
        // 4. Remove from middle: split into two ranges

        if r_start == start && r_end == end {
            // Case 1: exact match - remove
            for i in idx..self.count as usize - 1 {
                self.ranges[i] = self.ranges[i + 1];
            }
            self.count -= 1;
        } else if r_start == start {
            // Case 2: remove from start
            self.ranges[idx] = (end, r_end);
        } else if r_end == end {
            // Case 3: remove from end
            self.ranges[idx] = (r_start, start);
        } else {
            // Case 4: split - need to add a new range
            if self.count as usize >= MAX_RANGES {
                return Err(ByteRangeError::TooManyRanges);
            }

            // Shrink existing range to [r_start, start)
            self.ranges[idx] = (r_start, start);

            // Insert new range [end, r_end) after idx
            for i in (idx + 1..self.count as usize).rev() {
                self.ranges[i + 1] = self.ranges[i];
            }
            self.ranges[idx + 1] = (end, r_end);
            self.count += 1;
        }

        Ok(())
    }

    /// Clear any initialized bytes within a range.
    ///
    /// Unlike `mark_uninit`, this does not require the range to be fully
    /// initialized; it removes any overlap with existing ranges.
    pub fn clear_range(&mut self, start: u32, end: u32) -> Result<(), ByteRangeError> {
        if start >= end {
            return Err(ByteRangeError::InvalidRange { start, end });
        }

        let mut new_ranges = [(0, 0); MAX_RANGES];
        let mut new_count = 0usize;

        for i in 0..self.count as usize {
            let (r_start, r_end) = self.ranges[i];
            if end <= r_start || start >= r_end {
                new_ranges[new_count] = (r_start, r_end);
                new_count += 1;
                continue;
            }

            if r_start < start {
                new_ranges[new_count] = (r_start, start);
                new_count += 1;
            }

            if r_end > end {
                new_ranges[new_count] = (end, r_end);
                new_count += 1;
            }
        }

        self.ranges = new_ranges;
        self.count = new_count as u8;
        Ok(())
    }

    /// Get the initialized ranges as a slice.
    pub fn ranges(&self) -> &[(u32, u32)] {
        &self.ranges[..self.count as usize]
    }
}

impl Default for ByteRangeTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_tracker_is_empty() {
        let tracker = ByteRangeTracker::new();
        assert!(tracker.is_empty());
        assert_eq!(tracker.range_count(), 0);
    }

    #[test]
    fn mark_init_single_range() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();

        assert!(!tracker.is_empty());
        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 4));
        assert!(tracker.is_init(1, 3)); // Subset is also init
        assert!(!tracker.is_init(0, 5)); // Superset is not
    }

    #[test]
    fn mark_init_disjoint_ranges() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();
        tracker.mark_init(8, 12).unwrap();

        assert_eq!(tracker.range_count(), 2);
        assert!(tracker.is_init(0, 4));
        assert!(tracker.is_init(8, 12));
        assert!(!tracker.is_init(4, 8)); // Gap is not init
    }

    #[test]
    fn mark_init_adjacent_merges() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();
        tracker.mark_init(4, 8).unwrap();

        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 8));
    }

    #[test]
    fn mark_init_adjacent_merges_reverse_order() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(4, 8).unwrap();
        tracker.mark_init(0, 4).unwrap();

        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 8));
    }

    #[test]
    fn mark_init_bridges_gap() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();
        tracker.mark_init(8, 12).unwrap();
        tracker.mark_init(4, 8).unwrap(); // Bridge the gap

        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 12));
    }

    #[test]
    fn mark_init_overlap_fails() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();

        let err = tracker.mark_init(4, 12).unwrap_err();
        assert!(matches!(err, ByteRangeError::AlreadyInitialized { .. }));
    }

    #[test]
    fn mark_init_exact_overlap_fails() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();

        let err = tracker.mark_init(0, 8).unwrap_err();
        assert!(matches!(err, ByteRangeError::AlreadyInitialized { .. }));
    }

    #[test]
    fn mark_uninit_exact() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();
        tracker.mark_uninit(0, 8).unwrap();

        assert!(tracker.is_empty());
    }

    #[test]
    fn mark_uninit_from_start() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();
        tracker.mark_uninit(0, 4).unwrap();

        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(4, 8));
        assert!(!tracker.is_init(0, 4));
    }

    #[test]
    fn mark_uninit_from_end() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();
        tracker.mark_uninit(4, 8).unwrap();

        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 4));
        assert!(!tracker.is_init(4, 8));
    }

    #[test]
    fn mark_uninit_from_middle_splits() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 12).unwrap();
        tracker.mark_uninit(4, 8).unwrap();

        assert_eq!(tracker.range_count(), 2);
        assert!(tracker.is_init(0, 4));
        assert!(tracker.is_init(8, 12));
        assert!(!tracker.is_init(4, 8));
    }

    #[test]
    fn clear_range_partial_overlap() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 12).unwrap();
        tracker.clear_range(4, 8).unwrap();

        assert_eq!(tracker.range_count(), 2);
        assert!(tracker.is_init(0, 4));
        assert!(tracker.is_init(8, 12));
        assert!(!tracker.is_init(4, 8));
    }

    #[test]
    fn clear_range_full_overlap() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();
        tracker.mark_init(6, 10).unwrap();
        tracker.clear_range(0, 10).unwrap();

        assert!(tracker.is_empty());
    }

    #[test]
    fn mark_uninit_not_init_fails() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 4).unwrap();

        let err = tracker.mark_uninit(4, 8).unwrap_err();
        assert!(matches!(err, ByteRangeError::NotInitialized { .. }));
    }

    #[test]
    fn mark_uninit_partial_overlap_fails() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(0, 8).unwrap();

        // Trying to uninit [4, 12) but only [4, 8) is init
        let err = tracker.mark_uninit(4, 12).unwrap_err();
        assert!(matches!(err, ByteRangeError::NotInitialized { .. }));
    }

    #[test]
    fn invalid_range_fails() {
        let mut tracker = ByteRangeTracker::new();

        let err = tracker.mark_init(8, 4).unwrap_err();
        assert!(matches!(err, ByteRangeError::InvalidRange { .. }));

        let err = tracker.mark_init(4, 4).unwrap_err();
        assert!(matches!(err, ByteRangeError::InvalidRange { .. }));
    }

    #[test]
    fn is_uninit_checks() {
        let mut tracker = ByteRangeTracker::new();
        assert!(tracker.is_uninit(0, 100));

        tracker.mark_init(10, 20).unwrap();
        assert!(tracker.is_uninit(0, 10));
        assert!(tracker.is_uninit(20, 30));
        assert!(!tracker.is_uninit(5, 15)); // Overlaps
        assert!(!tracker.is_uninit(10, 20)); // Exact match
        assert!(!tracker.is_uninit(15, 25)); // Overlaps
    }

    #[test]
    fn sorted_order_maintained() {
        let mut tracker = ByteRangeTracker::new();
        tracker.mark_init(20, 24).unwrap();
        tracker.mark_init(0, 4).unwrap();
        tracker.mark_init(10, 14).unwrap();

        let ranges = tracker.ranges();
        assert_eq!(ranges.len(), 3);
        assert_eq!(ranges[0], (0, 4));
        assert_eq!(ranges[1], (10, 14));
        assert_eq!(ranges[2], (20, 24));
    }

    #[test]
    fn complex_lifecycle() {
        let mut tracker = ByteRangeTracker::new();

        // Simulate: struct { a: u32, inner: { x: u32, y: u32 }, b: u32 }
        // Offsets: a=0, inner.x=4, inner.y=8, b=12

        // Init fields in arbitrary order
        tracker.mark_init(4, 8).unwrap(); // inner.x
        tracker.mark_init(12, 16).unwrap(); // b
        tracker.mark_init(0, 4).unwrap(); // a
        tracker.mark_init(8, 12).unwrap(); // inner.y

        // Should merge into one range
        assert_eq!(tracker.range_count(), 1);
        assert!(tracker.is_init(0, 16));

        // Uninit in reverse order (simulating drop)
        tracker.mark_uninit(8, 12).unwrap();
        tracker.mark_uninit(0, 4).unwrap();
        tracker.mark_uninit(12, 16).unwrap();
        tracker.mark_uninit(4, 8).unwrap();

        assert!(tracker.is_empty());
    }
}

#[cfg(kani)]
mod kani_proofs {
    use super::*;

    /// Prove: mark_init followed by mark_uninit returns to empty
    #[kani::proof]
    #[kani::unwind(5)]
    fn init_then_uninit_is_empty() {
        let start: u32 = kani::any();
        let end: u32 = kani::any();
        kani::assume(start < end);
        kani::assume(end - start <= 64); // Reasonable size bound

        let mut tracker = ByteRangeTracker::new();

        if tracker.mark_init(start, end).is_ok() {
            kani::assert(!tracker.is_empty(), "should not be empty after init");

            let result = tracker.mark_uninit(start, end);
            kani::assert(result.is_ok(), "uninit of exact range should succeed");
            kani::assert(tracker.is_empty(), "should be empty after uninit");
        }
    }

    /// Prove: double init of same range fails
    #[kani::proof]
    #[kani::unwind(5)]
    fn double_init_fails() {
        let start: u32 = kani::any();
        let end: u32 = kani::any();
        kani::assume(start < end);
        kani::assume(end <= 64);

        let mut tracker = ByteRangeTracker::new();

        if tracker.mark_init(start, end).is_ok() {
            let result = tracker.mark_init(start, end);
            kani::assert(result.is_err(), "double init should fail");
        }
    }

    /// Prove: uninit of uninitialized range fails
    #[kani::proof]
    #[kani::unwind(5)]
    fn uninit_without_init_fails() {
        let start: u32 = kani::any();
        let end: u32 = kani::any();
        kani::assume(start < end);
        kani::assume(end <= 64);

        let tracker = ByteRangeTracker::new();
        // Don't mutate - create a mutable copy
        let mut tracker = tracker;

        let result = tracker.mark_uninit(start, end);
        kani::assert(result.is_err(), "uninit without init should fail");
    }

    /// Prove: is_init and is_uninit are consistent
    #[kani::proof]
    #[kani::unwind(5)]
    fn init_uninit_consistency() {
        let start: u32 = kani::any();
        let end: u32 = kani::any();
        kani::assume(start < end);
        kani::assume(end <= 32);

        let mut tracker = ByteRangeTracker::new();

        // Initially, range should be uninit
        kani::assert(tracker.is_uninit(start, end), "fresh tracker is uninit");
        kani::assert(!tracker.is_init(start, end), "fresh tracker is not init");

        if tracker.mark_init(start, end).is_ok() {
            kani::assert(tracker.is_init(start, end), "after init, is_init is true");
            kani::assert(
                !tracker.is_uninit(start, end),
                "after init, is_uninit is false",
            );
        }
    }

    /// Prove: adjacent ranges merge correctly
    #[kani::proof]
    #[kani::unwind(5)]
    fn adjacent_merge() {
        let mid: u32 = kani::any();
        let size: u32 = kani::any();
        kani::assume(mid >= 4 && mid <= 28);
        kani::assume(size >= 1 && size <= 4);

        let start1 = mid - size;
        let end1 = mid;
        let start2 = mid;
        let end2 = mid + size;

        let mut tracker = ByteRangeTracker::new();

        if tracker.mark_init(start1, end1).is_ok() {
            if tracker.mark_init(start2, end2).is_ok() {
                // Should have merged into one range
                kani::assert(tracker.range_count() == 1, "adjacent ranges should merge");
                kani::assert(tracker.is_init(start1, end2), "merged range covers both");
            }
        }
    }

    /// Prove: disjoint ranges stay separate
    #[kani::proof]
    #[kani::unwind(5)]
    fn disjoint_stay_separate() {
        let mut tracker = ByteRangeTracker::new();

        // Two clearly disjoint ranges
        let r1_start: u32 = kani::any();
        let r1_size: u32 = kani::any();
        let gap: u32 = kani::any();
        let r2_size: u32 = kani::any();

        kani::assume(r1_start <= 8);
        kani::assume(r1_size >= 1 && r1_size <= 4);
        kani::assume(gap >= 1 && gap <= 4);
        kani::assume(r2_size >= 1 && r2_size <= 4);

        let r1_end = r1_start + r1_size;
        let r2_start = r1_end + gap;
        let r2_end = r2_start + r2_size;

        kani::assume(r2_end <= 32);

        if tracker.mark_init(r1_start, r1_end).is_ok() {
            if tracker.mark_init(r2_start, r2_end).is_ok() {
                kani::assert(tracker.range_count() == 2, "disjoint ranges stay separate");
                kani::assert(tracker.is_uninit(r1_end, r2_start), "gap remains uninit");
            }
        }
    }

    /// Prove: split from middle creates two ranges
    #[kani::proof]
    #[kani::unwind(5)]
    fn middle_uninit_splits() {
        let mut tracker = ByteRangeTracker::new();

        // Create a range [0, 12)
        if tracker.mark_init(0, 12).is_ok() {
            kani::assert(tracker.range_count() == 1, "starts with one range");

            // Uninit the middle [4, 8)
            if tracker.mark_uninit(4, 8).is_ok() {
                kani::assert(tracker.range_count() == 2, "split into two ranges");
                kani::assert(tracker.is_init(0, 4), "left part still init");
                kani::assert(tracker.is_init(8, 12), "right part still init");
                kani::assert(tracker.is_uninit(4, 8), "middle now uninit");
            }
        }
    }

    /// Prove: overlapping init fails
    #[kani::proof]
    #[kani::unwind(5)]
    fn overlapping_init_fails() {
        let mut tracker = ByteRangeTracker::new();

        let start1: u32 = kani::any();
        let end1: u32 = kani::any();
        let start2: u32 = kani::any();
        let end2: u32 = kani::any();

        kani::assume(start1 < end1 && end1 <= 32);
        kani::assume(start2 < end2 && end2 <= 32);
        // Ensure overlap: start2 < end1 && start1 < end2
        kani::assume(start2 < end1);
        kani::assume(start1 < end2);

        if tracker.mark_init(start1, end1).is_ok() {
            let result = tracker.mark_init(start2, end2);
            kani::assert(result.is_err(), "overlapping init should fail");
        }
    }
}
