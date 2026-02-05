use vstd::prelude::*;

verus! {

pub struct Range {
    pub start: int,
    pub end: int,
}

pub closed spec fn valid_range(r: Range) -> bool {
    r.start < r.end
}

pub closed spec fn in_range(r: Range, x: int) -> bool {
    r.start <= x < r.end
}

pub closed spec fn no_overlap(r: Range, clear: Range) -> bool {
    clear.end <= r.start || clear.start >= r.end
}

pub closed spec fn clear_output_contains(r: Range, clear: Range, x: int) -> bool {
    if no_overlap(r, clear) {
        in_range(r, x)
    } else {
        (r.start < clear.start && r.start <= x < clear.start)
            || (clear.end < r.end && clear.end <= x < r.end)
    }
}

pub proof fn lemma_clear_output_matches_set_difference(r: Range, clear: Range)
    requires
        valid_range(r),
        valid_range(clear),
    ensures
        forall|x: int| #![auto] clear_output_contains(r, clear, x)
            == (in_range(r, x) && !in_range(clear, x)),
{
    assert forall|x: int| #![auto] clear_output_contains(r, clear, x)
        == (in_range(r, x) && !in_range(clear, x)) by {
        if no_overlap(r, clear) {
            if in_range(r, x) {
                if clear.end <= r.start {
                    assert(x >= r.start);
                    assert(x >= clear.end);
                    assert(!in_range(clear, x));
                } else {
                    assert(clear.start >= r.end);
                    assert(x < r.end);
                    assert(x < clear.start);
                    assert(!in_range(clear, x));
                }
            }
        } else {
            if clear_output_contains(r, clear, x) {
                assert(in_range(r, x));
                assert(!in_range(clear, x));
            }

            if in_range(r, x) && !in_range(clear, x) {
                if x < clear.start {
                    assert(r.start < clear.start);
                    assert(r.start <= x < clear.start);
                } else {
                    assert(clear.end <= x);
                    assert(clear.end < r.end);
                    assert(clear.end <= x < r.end);
                }
            }
        }
    }
}

pub proof fn lemma_cleared_bytes_are_removed(r: Range, clear: Range, x: int)
    requires
        valid_range(r),
        valid_range(clear),
        in_range(clear, x),
    ensures
        !clear_output_contains(r, clear, x),
{
    lemma_clear_output_matches_set_difference(r, clear);
    assert(clear_output_contains(r, clear, x) == (in_range(r, x) && !in_range(clear, x)));
    if clear_output_contains(r, clear, x) {
        assert(!in_range(clear, x));
        assert(false);
    } else {
        assert(!clear_output_contains(r, clear, x));
    }
}

fn main() {}

}
