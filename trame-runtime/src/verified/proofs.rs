use super::*;
use core::alloc::Layout;

type S<'a> = VShapeView<'a, VShapeStore>;

/// Prove: pointer arithmetic preserves allocation identity
#[kani::proof]
#[kani::unwind(5)]
fn pointer_arithmetic_same_alloc() {
    let size: u32 = kani::any();
    let offset: u32 = kani::any();
    kani::assume(size > 0 && size <= 64);
    kani::assume(offset <= size);

    let mut store = VShapeStore::new();
    let h = store.add(VShapeDef::scalar(
        Layout::from_size_align(size as usize, 1).unwrap(),
    ));
    let shape = store.view(h);

    let mut heap = VHeap::<S<'_>>::new();
    let ptr = unsafe { heap.alloc(shape) };

    let offset_ptr = ptr.offset(offset as usize);

    kani::assert(
        offset_ptr.alloc_id() == ptr.alloc_id(),
        "same allocation after offset",
    );
    kani::assert(
        offset_ptr.alloc_size() == ptr.alloc_size(),
        "same size after offset",
    );
}

/// Prove: multiple allocations have distinct IDs
#[kani::proof]
#[kani::unwind(5)]
fn multiple_allocs_distinct() {
    let mut store = VShapeStore::new();
    let h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let shape = store.view(h);

    let mut heap = VHeap::<S<'_>>::new();
    let ptr1 = unsafe { heap.alloc(shape) };
    let ptr2 = unsafe { heap.alloc(shape) };

    kani::assert(ptr1.alloc_id() != ptr2.alloc_id(), "distinct alloc ids");
}

/// Prove: store handles are valid after adding shapes
#[kani::proof]
#[kani::unwind(10)]
fn store_handles_valid() {
    let mut store = VShapeStore::new();

    let num_shapes: u8 = kani::any();
    kani::assume(num_shapes > 0 && num_shapes <= 4);

    for i in 0..num_shapes {
        let size: usize = kani::any();
        kani::assume(size > 0 && size <= 8);
        let layout = Layout::from_size_align(size, 1).unwrap();
        let h = store.add(VShapeDef::scalar(layout));
        kani::assert(h.0 == i, "handle matches index");
    }

    // All handles should be retrievable
    for i in 0..num_shapes {
        let view = store.view(VShapeHandle(i));
        kani::assert(
            view.store as *const _ == &store as *const _,
            "view borrows store",
        );
    }
}

/// Prove: nested struct field navigation works correctly
#[kani::proof]
#[kani::unwind(10)]
fn nested_struct_navigation() {
    let mut store = VShapeStore::new();

    // Scalar shape
    let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    // Inner struct with 2 scalar fields
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(8, 1).unwrap(),
        type_ops: VTypeOps::pod(),
        def: VDef::Struct(VStructDef {
            field_count: 2,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr[1] = VFieldDef::new(4, scalar_h);
                arr
            },
        }),
    };
    let inner_h = store.add(inner_def);

    // Outer struct with scalar + inner struct
    let outer_def = VShapeDef {
        layout: Layout::from_size_align(12, 1).unwrap(),
        type_ops: VTypeOps::pod(),
        def: VDef::Struct(VStructDef {
            field_count: 2,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr[1] = VFieldDef::new(4, inner_h);
                arr
            },
        }),
    };
    let outer_h = store.add(outer_def);

    // Navigate: outer -> field[1] -> inner struct -> field[0] -> scalar
    let outer = store.view(outer_h);
    kani::assert(outer.is_struct(), "outer is struct");

    let outer_st = outer.as_struct().unwrap();
    kani::assert(outer_st.field_count() == 2, "outer has 2 fields");

    let field0 = outer_st.field(0).unwrap();
    kani::assert(!field0.shape().is_struct(), "field 0 is scalar");

    let field1 = outer_st.field(1).unwrap();
    kani::assert(field1.shape().is_struct(), "field 1 is struct");

    let inner = field1.shape();
    let inner_st = inner.as_struct().unwrap();
    kani::assert(inner_st.field_count() == 2, "inner has 2 fields");

    let inner_field0 = inner_st.field(0).unwrap();
    kani::assert(!inner_field0.shape().is_struct(), "inner field 0 is scalar");
    kani::assert(
        inner_field0.shape().layout().unwrap().size() == 4,
        "inner field 0 is 4 bytes",
    );
}

/// Prove: field shape handles point to valid shapes in store
#[kani::proof]
#[kani::unwind(10)]
fn field_handles_valid() {
    let mut store = VShapeStore::new();

    // Add scalars
    let num_scalars: u8 = kani::any();
    kani::assume(num_scalars >= 1 && num_scalars <= 3);

    for _ in 0..num_scalars {
        store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    }

    // Add a struct referencing those scalars
    let field_count: u8 = kani::any();
    kani::assume(field_count > 0 && field_count <= 3);

    let mut fields = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
    for i in 0..(field_count as usize) {
        let shape_idx: u8 = kani::any();
        kani::assume(shape_idx < num_scalars);
        fields[i] = VFieldDef::new(i * 4, VShapeHandle(shape_idx));
    }

    let struct_def = VShapeDef {
        layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
        type_ops: VTypeOps::pod(),
        def: VDef::Struct(VStructDef {
            field_count,
            fields,
        }),
    };
    let struct_h = store.add(struct_def);

    // Navigate to each field and verify its shape is accessible
    let view = store.view(struct_h);
    let st = view.as_struct().unwrap();

    for i in 0..(field_count as usize) {
        let field = st.field(i).unwrap();
        let field_shape = field.shape();
        // Should not panic - the handle should be valid
        let _layout = field_shape.layout().unwrap();
        kani::assert(!field_shape.is_struct(), "field points to scalar");
    }
}

#[kani::proof]
#[kani::unwind(6)]
fn verified_arena_alloc_free() {
    let mut arena = VArena::<u32, 4>::new();

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
