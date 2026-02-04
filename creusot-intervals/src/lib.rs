#![cfg_attr(not(creusot), allow(dead_code, unused_imports))]

#[cfg(creusot)]
use creusot_std::logic::Int;
#[cfg(creusot)]
use creusot_std::macros::{ensures, logic, pearlite, proof_assert, requires};
#[cfg(creusot)]
use creusot_std::prelude::variant;

/// A half-open byte range `[start, start + len)`.
///
/// Think of this as "these bytes are initialized" in a logical model.
/// This is *not* a runtime range; it is only used in proofs.
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
    // Logical end of the range: start + len.
    pearlite! { r.start@ + r.len@ }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn contains(r: Range, i: Int) -> bool {
    // True iff byte offset i is inside r.
    pearlite! { r.start@ <= i && i < end(r) }
}

/// True if `i` is covered by any range in `ranges`.
#[cfg(creusot)]
#[logic(open, inline)]
pub fn in_any(ranges: &Vec<Range>, i: Int) -> bool {
    // Existential witness: there is some range that covers i.
    pearlite! {
        exists<j: Int>
            0 <= j && j < ranges@.len() && contains(ranges[j], i)
    }
}

/// Helper: a direct witness implies `in_any`.
///
/// "Witness" here just means: an explicit index `k` such that
/// `ranges[k]` covers `i`. Since `in_any` is defined with an `exists`,
/// providing a concrete `k` is the standard way to prove it.
#[cfg(creusot)]
#[logic]
pub fn witness_implies_in_any(ranges: &Vec<Range>, k: Int, i: Int) -> bool {
    pearlite! {
        (0 <= k && k < ranges@.len() && contains(ranges[k], i)) ==> in_any(ranges, i)
    }
}

/// True if `i` is covered by any discriminant or payload range.
#[cfg(creusot)]
#[logic(open, inline)]
pub fn in_any_or_disc(disc_ranges: &Vec<Range>, payload_ranges: &Vec<Range>, i: Int) -> bool {
    pearlite! { in_any(disc_ranges, i) || in_any(payload_ranges, i) }
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
    // If the ranges are adjacent and i is within the overall span,
    // this finds an index k such that ranges[k] covers i.
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
    // This lemma *assumes* adjacency (no holes) and proves full coverage
    // across the combined span.
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
    // IMPORTANT: this lemma does NOT allow holes. It only applies when the
    // ranges are perfectly adjacent (end of one equals start of next).
    // If you want holes (padding), use covers_except_padding below.
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
/// Provide padding *ranges* and prove that every byte in the struct
/// is either initialized (in_any) or is in a padding range.
#[cfg(creusot)]
#[logic]
pub fn covers_except_padding(
    ranges: &Vec<Range>,
    size: usize,
    padding_ranges: &Vec<Range>,
) -> bool {
    // This is the "allow holes" version: bytes not covered by ranges
    // are acceptable if they are padding.
    pearlite! {
        forall<i: Int>
            0 <= i && i < size@ ==> (in_any(ranges, i) || in_any(padding_ranges, i))
    }
}

/// "Ignore padding" for enums: discriminant + payload ranges + padding ranges.
#[cfg(creusot)]
#[logic]
pub fn covers_except_padding_with_discriminant(
    disc_ranges: &Vec<Range>,
    payload_ranges: &Vec<Range>,
    size: usize,
    padding_ranges: &Vec<Range>,
) -> bool {
    // Same as covers_except_padding, but discriminant bytes are also
    // considered initialized.
    pearlite! {
        forall<i: Int>
            0 <= i && i < size@ ==>
                (in_any_or_disc(disc_ranges, payload_ranges, i) || in_any(padding_ranges, i))
    }
}

/// Start of the discriminant+payload span.
#[cfg(creusot)]
#[logic(open, inline)]
pub fn span_start(disc_ranges: &Vec<Range>, payload_ranges: &Vec<Range>) -> Int {
    pearlite! {
        if disc_ranges@.len() > 0 { disc_ranges[0].start@ } else { payload_ranges[0].start@ }
    }
}

/// End of the discriminant span (or payload start if there is no discriminant).
#[cfg(creusot)]
#[logic(open, inline)]
pub fn disc_end(disc_ranges: &Vec<Range>, payload_ranges: &Vec<Range>) -> Int {
    pearlite! {
        if disc_ranges@.len() > 0 {
            end(disc_ranges[disc_ranges@.len() - 1])
        } else {
            payload_ranges[0].start@
        }
    }
}

/// Adjacent payload ranges (and optional adjacent discriminant) cover the span.
#[cfg(creusot)]
#[requires(payload_ranges@.len() > 0)]
#[requires(
    forall<i: Int>
        0 <= i && i + 1 < payload_ranges@.len() ==>
            end(payload_ranges[i]) == payload_ranges[i + 1].start@
)]
#[requires(
    disc_ranges@.len() == 0 ||
        forall<i: Int>
            0 <= i && i + 1 < disc_ranges@.len() ==>
                end(disc_ranges[i]) == disc_ranges[i + 1].start@
)]
#[requires(
    disc_ranges@.len() == 0 ||
        end(disc_ranges[disc_ranges@.len() - 1]) == payload_ranges[0].start@
)]
#[ensures(
    forall<i: Int>
        span_start(disc_ranges, payload_ranges) <= i &&
        i < end(payload_ranges[payload_ranges@.len() - 1]) ==>
            if i < disc_end(disc_ranges, payload_ranges) {
                exists<k: Int>
                    k == find_range(disc_ranges, disc_ranges@.len() - 1, i) &&
                    0 <= k && k < disc_ranges@.len() &&
                    contains(disc_ranges[k], i)
            } else {
                exists<k: Int>
                    k == find_range(payload_ranges, payload_ranges@.len() - 1, i) &&
                    0 <= k && k < payload_ranges@.len() &&
                    contains(payload_ranges[k], i)
            }
)]
pub fn disc_and_adjacent_payload_cover(disc_ranges: &Vec<Range>, payload_ranges: &Vec<Range>) {
    // This lemma assumes a *specific* layout shape:
    // - Discriminant ranges (if any) are adjacent.
    // - The last discriminant range (if any) ends exactly where the
    //   first payload range starts.
    // - The payload ranges themselves are adjacent (no holes).
    // It does NOT model padding; padding is handled by
    // covers_except_padding_with_discriminant.
    adjacent_seq_cover(payload_ranges);
    if disc_ranges.len() > 0 {
        adjacent_seq_cover(disc_ranges);
    }
    proof_assert! {
        forall<i: Int>
            disc_ranges@.len() > 0 &&
            span_start(disc_ranges, payload_ranges) <= i &&
            i < disc_end(disc_ranges, payload_ranges) ==>
                exists<k: Int>
                    k == find_range(disc_ranges, disc_ranges@.len() - 1, i) &&
                    0 <= k && k < disc_ranges@.len() &&
                    contains(disc_ranges[k], i)
    };
    proof_assert! {
        forall<i: Int>
            disc_end(disc_ranges, payload_ranges) <= i &&
            i < end(payload_ranges[payload_ranges@.len() - 1]) ==>
                exists<k: Int>
                    k == find_range(payload_ranges, payload_ranges@.len() - 1, i) &&
                    0 <= k && k < payload_ranges@.len() &&
                    contains(payload_ranges[k], i)
    };
    proof_assert! {
        forall<i: Int>
            span_start(disc_ranges, payload_ranges) <= i &&
            i < end(payload_ranges[payload_ranges@.len() - 1]) ==>
                if i < disc_end(disc_ranges, payload_ranges) {
                    exists<k: Int>
                        k == find_range(disc_ranges, disc_ranges@.len() - 1, i) &&
                        0 <= k && k < disc_ranges@.len() &&
                        contains(disc_ranges[k], i)
                } else {
                    exists<k: Int>
                        k == find_range(payload_ranges, payload_ranges@.len() - 1, i) &&
                        0 <= k && k < payload_ranges@.len() &&
                        contains(payload_ranges[k], i)
                }
    };
}

/// Simple enum layout model: discriminant + per-variant payload ranges + padding ranges.
#[derive(Clone, Debug)]
pub struct EnumLayout {
    pub discriminant: Vec<Range>,
    pub size: usize,
    pub variants: Vec<Vec<Range>>,
    pub padding: Vec<Range>,
}

/// Coverage predicate for a chosen variant.
#[cfg(creusot)]
#[logic]
pub fn enum_variant_covers(layout: &EnumLayout, variant: Int) -> bool {
    pearlite! {
        0 <= variant && variant < layout.variants@.len() ==>
            covers_except_padding_with_discriminant(
                &layout.discriminant,
                &layout.variants[variant],
                layout.size,
                &layout.padding
            )
    }
}
