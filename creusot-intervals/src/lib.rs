#![cfg_attr(not(creusot), allow(dead_code, unused_imports))]

#[cfg(creusot)]
use creusot_std::logic::{Int, Mapping};
#[cfg(creusot)]
use creusot_std::macros::{ensures, logic, pearlite, proof_assert, requires};
#[cfg(creusot)]
use creusot_std::prelude::variant;
#[cfg(creusot)]
use creusot_std::snapshot::Snapshot;

/// A half-open byte range `[start, start + len)`.
#[derive(Clone, Copy, Debug)]
pub struct Range {
    pub start: usize,
    pub len: usize,
}

impl Range {
    pub const fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn end(r: Range) -> Int {
    pearlite! { r.start@ + r.len@ }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn contains(r: Range, i: Int) -> bool {
    pearlite! { r.start@ <= i && i < end(r) }
}

/// True if `i` is covered by any range in `ranges`.
#[cfg(creusot)]
#[logic(open, inline)]
pub fn in_any(ranges: &Vec<Range>, i: Int) -> bool {
    pearlite! {
        exists<j: Int>
            0 <= j && j < ranges@.len() && contains(ranges[j], i)
    }
}

/// Helper: a direct witness implies `in_any`.
#[cfg(creusot)]
#[logic]
pub fn witness_implies_in_any(ranges: &Vec<Range>, k: Int, i: Int) -> bool {
    pearlite! {
        (0 <= k && k < ranges@.len() && contains(ranges[k], i)) ==> in_any(ranges, i)
    }
}

/// Find a covering range index for `i` in the prefix `[0..=j]`.
#[cfg(creusot)]
#[logic]
#[variant(j)]
#[requires(0 <= j)]
#[requires(j < ranges@.len())]
#[requires(
    forall<i: Int>
        0 <= i && i + 1 < ranges@.len() ==> end(ranges[i]) == ranges[i + 1].start@
)]
#[requires(ranges[0].start@ <= i && i < end(ranges[j]))]
#[ensures(0 <= result && result <= j)]
#[ensures(contains(ranges[result], i))]
pub fn find_range(ranges: &Vec<Range>, j: Int, i: Int) -> Int {
    pearlite! {
        if j == 0 {
            0
        } else if i < end(ranges[j - 1]) {
            find_range(ranges, j - 1, i)
        } else {
            // i >= end(ranges[j - 1]) == ranges[j].start, and i < end(ranges[j])
            j
        }
    }
}

/// De-risk lemma: two adjacent ranges cover their combined span.
#[cfg(creusot)]
#[requires(end(r1) == r2.start@)]
#[ensures(
    forall<i: Int>
        r1.start@ <= i && i < end(r2) ==> (contains(r1, i) || contains(r2, i))
)]
pub fn adjacent_two_cover(r1: Range, r2: Range) {
    proof_assert! {
        forall<i: Int>
            r1.start@ <= i && i < end(r2) ==> (contains(r1, i) || contains(r2, i))
    }
}

/// De-risk lemma: a sequence of adjacent ranges covers their combined span.
#[cfg(creusot)]
#[requires(ranges@.len() > 0)]
#[requires(
    forall<i: Int>
        0 <= i && i + 1 < ranges@.len() ==> end(ranges[i]) == ranges[i + 1].start@
)]
#[ensures(
    forall<i: Int>
        ranges[0].start@ <= i && i < end(ranges[ranges@.len() - 1]) ==>
            exists<k: Int>
                k == find_range(ranges, ranges@.len() - 1, i) &&
                0 <= k && k < ranges@.len() &&
                contains(ranges[k], i)
)]
pub fn adjacent_seq_cover(ranges: &Vec<Range>) {
    proof_assert! {
        forall<i: Int>
            ranges[0].start@ <= i && i < end(ranges[ranges@.len() - 1]) ==>
                exists<k: Int>
                    k == find_range(ranges, ranges@.len() - 1, i) &&
                    0 <= k && k < ranges@.len() &&
                    contains(ranges[k], i)
    };
    proof_assert! {
        forall<i: Int>
            ranges[0].start@ <= i && i < end(ranges[ranges@.len() - 1]) ==>
                witness_implies_in_any(ranges, find_range(ranges, ranges@.len() - 1, i), i)
    };
}

/// Example: express "ignore padding" by allowing holes.
/// Provide a predicate `is_padding` and prove that every byte in the struct
/// is either initialized (in_any) or is padding.
#[cfg(creusot)]
#[logic]
pub fn covers_except_padding(
    ranges: &Vec<Range>,
    size: usize,
    is_padding: Snapshot<Mapping<Int, bool>>,
) -> bool {
    pearlite! {
        forall<i: Int>
            0 <= i && i < size@ ==> (in_any(ranges, i) || (*is_padding)[i])
    }
}
