#![cfg_attr(not(creusot), allow(dead_code, unused_imports))]

#[cfg(creusot)]
use creusot_std::logic::{Int, Mapping};
#[cfg(creusot)]
use creusot_std::macros::{ensures, logic, pearlite, proof_assert, requires};
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
#[logic]
pub fn in_any(ranges: &Vec<Range>, i: Int) -> bool {
    pearlite! {
        exists<j: Int>
            0 <= j && j < ranges@.len() && contains(ranges[j], i)
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
        ranges[0].start@ <= i && i < end(ranges[(ranges@.len() - 1)]) ==> in_any(ranges, i)
)]
pub fn adjacent_seq_cover(ranges: &Vec<Range>) {
    proof_assert! {
        forall<i: Int>
            ranges[0].start@ <= i && i < end(ranges[(ranges@.len() - 1)]) ==> in_any(ranges, i)
    }
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
