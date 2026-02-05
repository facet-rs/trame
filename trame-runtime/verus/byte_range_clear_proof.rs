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

pub proof fn lemma_no_overlap_clear_is_identity(r: Range, clear: Range)
    requires
        valid_range(r),
        valid_range(clear),
        no_overlap(r, clear),
    ensures
        forall|x: int| #![auto] clear_output_contains(r, clear, x) == in_range(r, x),
{
    assert forall|x: int| #![auto] clear_output_contains(r, clear, x) == in_range(r, x) by {
    }
}

pub proof fn lemma_full_cover_clear_is_empty(r: Range, clear: Range)
    requires
        valid_range(r),
        valid_range(clear),
        clear.start <= r.start,
        r.end <= clear.end,
    ensures
        forall|x: int| !clear_output_contains(r, clear, x),
{
    assert forall|x: int| !clear_output_contains(r, clear, x) by {
        assert(!no_overlap(r, clear));
        if clear_output_contains(r, clear, x) {
            if r.start < clear.start && r.start <= x < clear.start {
                assert(false);
            }
            if clear.end < r.end && clear.end <= x < r.end {
                assert(false);
            }
        }
    }
}

pub proof fn lemma_two_clears_compose_and_commute(r: Range, clear1: Range, clear2: Range)
    requires
        valid_range(r),
        valid_range(clear1),
        valid_range(clear2),
    ensures
        forall|x: int| #![auto]
            (clear_output_contains(r, clear1, x) && !in_range(clear2, x))
                == (in_range(r, x) && !in_range(clear1, x) && !in_range(clear2, x)),
        forall|x: int| #![auto]
            (clear_output_contains(r, clear1, x) && !in_range(clear2, x))
                == (clear_output_contains(r, clear2, x) && !in_range(clear1, x)),
{
    lemma_clear_output_matches_set_difference(r, clear1);
    lemma_clear_output_matches_set_difference(r, clear2);

    assert forall|x: int| #![auto]
        (clear_output_contains(r, clear1, x) && !in_range(clear2, x))
            == (in_range(r, x) && !in_range(clear1, x) && !in_range(clear2, x)) by {
        assert(clear_output_contains(r, clear1, x) == (in_range(r, x) && !in_range(clear1, x)));
    }

    assert forall|x: int| #![auto]
        (clear_output_contains(r, clear1, x) && !in_range(clear2, x))
            == (clear_output_contains(r, clear2, x) && !in_range(clear1, x)) by {
        assert(clear_output_contains(r, clear1, x) == (in_range(r, x) && !in_range(clear1, x)));
        assert(clear_output_contains(r, clear2, x) == (in_range(r, x) && !in_range(clear2, x)));
    }
}

fn main() {}

}
