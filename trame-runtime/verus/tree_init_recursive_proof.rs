use vstd::prelude::*;

verus! {

pub struct FieldDef {
    pub offset: int,
    pub shape_handle: nat,
}

pub enum ShapeKind {
    Scalar,
    Struct(Seq<FieldDef>),
}

pub struct ShapeDef {
    pub size: nat,
    pub kind: ShapeKind,
}

pub closed spec fn valid_handle(store: Seq<ShapeDef>, handle: nat) -> bool {
    handle < store.len()
}

pub closed spec fn in_range(start: int, len: nat, x: int) -> bool {
    start <= x < start + len as int
}

pub closed spec fn range_init(init: Set<int>, start: int, len: nat) -> bool {
    forall|x: int| #![auto] in_range(start, len, x) ==> init.contains(x)
}

pub closed spec fn all_init(
    store: Seq<ShapeDef>,
    handle: nat,
    base: int,
    init: Set<int>,
    fuel: nat,
) -> bool
    decreases fuel
{
    if fuel == 0 || !valid_handle(store, handle) {
        false
    } else {
        let shape = store[handle as int];
        &&& range_init(init, base, shape.size)
        &&& match shape.kind {
            ShapeKind::Scalar => true,
            ShapeKind::Struct(fields) => forall|i: nat| #![auto]
                i < fields.len() ==> all_init(
                    store,
                    fields[i as int].shape_handle,
                    base + fields[i as int].offset,
                    init,
                    (fuel - 1) as nat,
                ),
        }
    }
}

pub proof fn lemma_all_init_implies_root_range_init(
    store: Seq<ShapeDef>,
    handle: nat,
    base: int,
    init: Set<int>,
    fuel: nat,
)
    requires
        all_init(store, handle, base, init, fuel),
    ensures
        range_init(init, base, store[handle as int].size),
{
}

pub proof fn lemma_all_init_implies_struct_field_init(
    store: Seq<ShapeDef>,
    handle: nat,
    base: int,
    init: Set<int>,
    fuel: nat,
    fields: Seq<FieldDef>,
    target_idx: nat,
)
    requires
        fuel > 0,
        valid_handle(store, handle),
        store[handle as int].kind == ShapeKind::Struct(fields),
        target_idx < fields.len(),
        all_init(store, handle, base, init, fuel),
    ensures
        all_init(
            store,
            fields[target_idx as int].shape_handle,
            base + fields[target_idx as int].offset,
            init,
            (fuel - 1) as nat,
        ),
{
    let shape = store[handle as int];
    assert(shape.kind == ShapeKind::Struct(fields));
    assert(match shape.kind {
        ShapeKind::Scalar => true,
        ShapeKind::Struct(fs) => forall|i: nat| #![auto]
            i < fs.len() ==> all_init(
                store,
                fs[i as int].shape_handle,
                base + fs[i as int].offset,
                init,
                (fuel - 1) as nat,
            ),
    });
    assert(forall|i: nat| #![auto]
        i < fields.len() ==> all_init(
            store,
            fields[i as int].shape_handle,
            base + fields[i as int].offset,
            init,
            (fuel - 1) as nat,
        )) by {
        if shape.kind == ShapeKind::Struct(fields) {
        }
    }
}

fn main() {}

}
