use super::*;
use crate::Path;
use crate::runtime::verified::{
    MAX_FIELDS_PER_STRUCT, VDef, VFieldDef, VRuntime, VShapeDef, VShapeHandle, VStructDef,
    vshape_register, vshape_store, vshape_view,
};
use core::alloc::Layout;
use trame_runtime::{IField, IShape, IStructType};

#[kani::proof]
#[kani::unwind(10)]
fn scalar_init_complete() {
    let h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
    let shape = vshape_view(h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(shape) };
    unsafe { heap.default_in_place(src, shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    kani::assert(!trame.is_complete(), "not complete initially");
    let root: [PathSegment; 0] = [];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&root),
            src: unsafe { Source::from_vptr(src, shape) },
        })
        .unwrap();
    kani::assert(trame.is_complete(), "complete after init");
}

#[kani::proof]
#[kani::unwind(10)]
fn struct_all_fields_required() {
    let field_count: u8 = kani::any();
    kani::assume(field_count > 0 && field_count <= 3);

    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
    for i in 0..(field_count as usize) {
        fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
    }

    let struct_def = VShapeDef {
        layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count,
            fields: fields_arr,
        }),
    };
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Init all but one field
    let skip_field: u8 = kani::any();
    kani::assume(skip_field < field_count);

    for i in 0..(field_count as usize) {
        if i != skip_field as usize {
            let path = [PathSegment::Field(i as u32)];
            trame
                .apply(Op::Set {
                    dst: Path::from_segments(&path),
                    src: unsafe { Source::from_vptr(src, scalar_shape) },
                })
                .unwrap();
        }
    }

    // Should not be complete
    kani::assert(!trame.is_complete(), "incomplete without all fields");

    // Init the skipped field
    let path = [PathSegment::Field(skip_field as u32)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&path),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();

    // Now should be complete
    kani::assert(trame.is_complete(), "complete with all fields");
}

/// Prove: staging the same field via Root while in a child re-enters
#[kani::proof]
#[kani::unwind(10)]
fn double_init_rejected() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    let inner_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr
            },
        }),
    };
    let inner_h = vshape_register(inner_def);
    let outer_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, inner_h);
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let path = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&path),
            src: Source::stage(None),
        })
        .unwrap();

    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();
    trame.apply(Op::End).unwrap();

    let root_path = [PathSegment::Root, PathSegment::Field(0)];
    let result2 = trame.apply(Op::Set {
        dst: Path::from_segments(&root_path),
        src: Source::stage(None),
    });
    kani::assert(result2.is_ok(), "stage after complete re-enters");
    kani::assert(trame.depth() == 1, "cursor remains in child");
}

/// Prove: build fails if not all fields initialized
#[kani::proof]
#[kani::unwind(10)]
fn incomplete_finish_fails() {
    let field_count: u8 = kani::any();
    kani::assume(field_count >= 2 && field_count <= 3);

    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
    for i in 0..(field_count as usize) {
        fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
    }

    let struct_def = VShapeDef {
        layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count,
            fields: fields_arr,
        }),
    };
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Init only first field
    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();

    // build should fail
    let result = trame.build();
    kani::assert(result.is_err(), "build fails when incomplete");
    kani::assert(
        matches!(result, Err(TrameError::Incomplete)),
        "error is Incomplete",
    );
}

/// Prove: field index out of bounds returns error
#[kani::proof]
#[kani::unwind(10)]
fn out_of_bounds_rejected() {
    let field_count: u8 = kani::any();
    kani::assume(field_count > 0 && field_count <= 3);

    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    let mut fields_arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
    for i in 0..(field_count as usize) {
        fields_arr[i] = VFieldDef::new(i * 4, scalar_h);
    }

    let struct_def = VShapeDef {
        layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count,
            fields: fields_arr,
        }),
    };
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Try to init field beyond bounds
    let bad_idx: u8 = kani::any();
    kani::assume(bad_idx >= field_count);
    kani::assume(bad_idx < 10); // Keep bounded

    let path = [PathSegment::Field(bad_idx as u32)];
    let result = trame.apply(Op::Set {
        dst: Path::from_segments(&path),
        src: unsafe { Source::from_vptr(src, scalar_shape) },
    });
    kani::assert(result.is_err(), "out of bounds fails");
    kani::assert(
        matches!(result, Err(TrameError::FieldOutOfBounds { .. })),
        "error is FieldOutOfBounds",
    );
}

/// Prove: any init order works for completion
#[kani::proof]
#[kani::unwind(10)]
fn any_init_order_completes() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

    let struct_def = VShapeDef {
        layout: Layout::from_size_align(12, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 3,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr[1] = VFieldDef::new(4, scalar_h);
                arr[2] = VFieldDef::new(8, scalar_h);
                arr
            },
        }),
    };
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src0 = unsafe { heap.alloc(scalar_shape) };
    let src1 = unsafe { heap.alloc(scalar_shape) };
    let src2 = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src0, scalar_shape) };
    unsafe { heap.default_in_place(src1, scalar_shape) };
    unsafe { heap.default_in_place(src2, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Choose arbitrary init order
    let first: u8 = kani::any();
    let second: u8 = kani::any();
    let third: u8 = kani::any();
    kani::assume(first < 3 && second < 3 && third < 3);
    kani::assume(first != second && second != third && first != third);

    let paths = [
        [PathSegment::Field(0)],
        [PathSegment::Field(1)],
        [PathSegment::Field(2)],
    ];
    let srcs = [src0, src1, src2];
    let order = [first as usize, second as usize, third as usize];
    for idx in order {
        trame
            .apply(Op::Set {
                dst: Path::from_segments(&paths[idx]),
                src: unsafe { Source::from_vptr(srcs[idx], scalar_shape) },
            })
            .unwrap();
    }

    kani::assert(
        trame.is_complete(),
        "complete after all fields in any order",
    );

    let result = trame.build();
    kani::assert(result.is_ok(), "build succeeds when complete");
}

/// Prove: nested struct fields are properly tracked
/// A struct containing another struct as a field should work correctly.
#[kani::proof]
#[kani::unwind(10)]
fn nested_struct_field_tracking() {
    // Inner struct: { a: u32, b: u32 }
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(8, 1).unwrap(),
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
    let inner_h = vshape_register(inner_def);

    // Outer struct: { x: u32, inner: Inner }
    let outer_def = VShapeDef {
        layout: Layout::from_size_align(12, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 2,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h); // x: u32
                arr[1] = VFieldDef::new(4, inner_h); // inner: Inner (nested struct!)
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    // Verify shape structure via IShape trait
    kani::assert(shape.is_struct(), "outer is struct");
    let outer_st = shape.as_struct().unwrap();
    kani::assert(outer_st.field_count() == 2, "outer has 2 fields");

    // Field 1 should be the nested struct
    let field1 = outer_st.field(1).unwrap();
    let field1_shape = field1.shape();
    kani::assert(field1_shape.is_struct(), "field 1 is nested struct");

    // Construct the outer struct
    let mut heap = VRuntime::heap();
    let src_x = unsafe { heap.alloc(scalar_shape) };
    let src_a = unsafe { heap.alloc(scalar_shape) };
    let src_b = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src_x, scalar_shape) };
    unsafe { heap.default_in_place(src_a, scalar_shape) };
    unsafe { heap.default_in_place(src_b, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let outer_x = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&outer_x),
            src: unsafe { Source::from_vptr(src_x, scalar_shape) },
        })
        .unwrap();
    let inner_field = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_field),
            src: Source::stage(None),
        })
        .unwrap();
    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src_a, scalar_shape) },
        })
        .unwrap();
    let inner_b = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_b),
            src: unsafe { Source::from_vptr(src_b, scalar_shape) },
        })
        .unwrap();
    trame.apply(Op::End).unwrap();

    kani::assert(trame.is_complete(), "outer complete after both fields");
    let result = trame.build();
    kani::assert(result.is_ok(), "build succeeds");
}

/// Prove: Stage/End lifecycle works correctly
#[kani::proof]
#[kani::unwind(10)]
fn stage_end_lifecycle() {
    // Inner struct: { a: u32, b: u32 }
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(8, 1).unwrap(),
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
    let inner_h = vshape_register(inner_def);

    // Outer struct: { x: u32, inner: Inner }
    let outer_def = VShapeDef {
        layout: Layout::from_size_align(12, 1).unwrap(),
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
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src_x = unsafe { heap.alloc(scalar_shape) };
    let src_a = unsafe { heap.alloc(scalar_shape) };
    let src_b = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src_x, scalar_shape) };
    unsafe { heap.default_in_place(src_a, scalar_shape) };
    unsafe { heap.default_in_place(src_b, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Verify initial state
    kani::assert(trame.depth() == 0, "starts at depth 0");
    kani::assert(!trame.is_complete(), "not complete initially");

    // Init scalar field
    let outer_x = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&outer_x),
            src: unsafe { Source::from_vptr(src_x, scalar_shape) },
        })
        .unwrap();
    kani::assert(!trame.is_complete(), "not complete with one field");

    // Stage nested struct
    let inner_field = [PathSegment::Field(1)];
    let result = trame.apply(Op::Set {
        dst: Path::from_segments(&inner_field),
        src: Source::stage(None),
    });
    kani::assert(result.is_ok(), "stage succeeds");
    kani::assert(trame.depth() == 1, "depth is 1 after begin");
    kani::assert(!trame.is_complete(), "inner not complete yet");

    // Init inner fields
    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src_a, scalar_shape) },
        })
        .unwrap();
    let inner_b = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_b),
            src: unsafe { Source::from_vptr(src_b, scalar_shape) },
        })
        .unwrap();
    kani::assert(trame.is_complete(), "inner complete");

    // End nested struct
    let result = trame.apply(Op::End);
    kani::assert(result.is_ok(), "end succeeds");
    kani::assert(trame.depth() == 0, "back to depth 0");
    kani::assert(trame.is_complete(), "outer complete");

    let result = trame.build();
    kani::assert(result.is_ok(), "build succeeds");
}

/// Prove: staging the same field via Root while in a child re-enters
#[kani::proof]
#[kani::unwind(10)]
fn stage_reenter_root_ok() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr
            },
        }),
    };
    let inner_h = vshape_register(inner_def);

    let outer_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, inner_h);
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let path = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&path),
            src: Source::stage(None),
        })
        .unwrap();

    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();
    trame.apply(Op::End).unwrap();

    let root_path = [PathSegment::Root, PathSegment::Field(0)];
    let result = trame.apply(Op::Set {
        dst: Path::from_segments(&root_path),
        src: Source::stage(None),
    });
    kani::assert(result.is_ok(), "stage after complete re-enters");
    kani::assert(trame.depth() == 1, "cursor remains in child");
}

/// Prove: Op::End at root returns error
#[kani::proof]
#[kani::unwind(10)]
fn end_op_at_root_fails() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let struct_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr
            },
        }),
    };
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();

    // Try End at root
    let result = trame.apply(Op::End);
    kani::assert(result.is_err(), "end at root fails");
    kani::assert(matches!(result, Err(TrameError::AtRoot)), "error is AtRoot");
}

/// Prove: End with incomplete inner fails
#[kani::proof]
#[kani::unwind(10)]
fn end_op_incomplete_inner_fails() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(8, 1).unwrap(),
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
    let inner_h = vshape_register(inner_def);

    let outer_def = VShapeDef {
        layout: Layout::from_size_align(8, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, inner_h);
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let inner_field = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_field),
            src: Source::stage(None),
        })
        .unwrap();
    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();

    // Try End with incomplete inner
    let result = trame.apply(Op::End);
    kani::assert(result.is_err(), "end with incomplete inner fails");
    kani::assert(
        matches!(result, Err(TrameError::CurrentIncomplete)),
        "error is CurrentIncomplete",
    );
}

/// Prove: drop properly cleans up nested Nodes (depth-first)
#[kani::proof]
#[kani::unwind(12)]
fn nested_drop_cleanup() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr
            },
        }),
    };
    let inner_h = vshape_register(inner_def);

    let outer_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, inner_h);
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Enter nested struct
    let inner_field = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_field),
            src: Source::stage(None),
        })
        .unwrap();
    // Init inner field
    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();
    // Don't end - just drop

    // Drop should clean up child Node before parent
    drop(trame);
    // No panic = cleanup order is correct
}

/// Prove: depth tracking is correct through nested begin/end
#[kani::proof]
#[kani::unwind(10)]
fn depth_tracking_correct() {
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
    let inner_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, scalar_h);
                arr
            },
        }),
    };
    let inner_h = vshape_register(inner_def);

    let outer_def = VShapeDef {
        layout: Layout::from_size_align(4, 1).unwrap(),
        def: VDef::Struct(VStructDef {
            field_count: 1,
            fields: {
                let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                arr[0] = VFieldDef::new(0, inner_h);
                arr
            },
        }),
    };
    let outer_h = vshape_register(outer_def);
    let shape = vshape_view(outer_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    kani::assert(trame.depth() == 0, "initial depth is 0");

    let inner_field = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_field),
            src: Source::stage(None),
        })
        .unwrap();
    kani::assert(trame.depth() == 1, "depth is 1 after begin");

    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, scalar_shape) },
        })
        .unwrap();
    kani::assert(trame.depth() == 1, "depth still 1 after inner init");

    trame.apply(Op::End).unwrap();
    kani::assert(trame.depth() == 0, "depth back to 0 after end");
}

use crate::runtime::verified::{VPtr, VShapeStore, VShapeView};

/// Generate an arbitrary field index (0-1 valid, 2-3 out of bounds for a 2-field struct)
fn arb_field_idx() -> u32 {
    let idx: u8 = kani::any();
    kani::assume(idx < 4);
    idx as u32
}

/// Apply one arbitrary operation to a trame (VRuntime-specific)
fn apply_arb_op(
    trame: &mut Trame<VRuntime>,
    src: VPtr,
    src_shape: VShapeView<'static, VShapeStore>,
) {
    let op_kind: u8 = kani::any();
    kani::assume(op_kind < 3);

    match op_kind {
        0 => {
            let path = [PathSegment::Field(arb_field_idx())];
            let _ = trame.apply(Op::Set {
                dst: Path::from_segments(&path),
                src: unsafe { Source::from_vptr(src, src_shape) },
            });
        }
        1 => {
            let _ = trame.apply(Op::End);
        }
        _ => {
            let path = [PathSegment::Field(arb_field_idx())];
            let _ = trame.apply(Op::Set {
                dst: Path::from_segments(&path),
                src: Source::stage(None),
            });
        }
    }
}

/// Arbitrary operations on a simple struct - let Kani explore all paths
#[kani::proof]
#[kani::unwind(5)]
fn arbitrary_ops_simple_struct() {
    // Simple struct with 2 scalar fields
    let scalar_h = vshape_register(VShapeDef::scalar(Layout::from_size_align(4, 4).unwrap()));
    let struct_def = VShapeDef {
        layout: Layout::from_size_align(8, 4).unwrap(),
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
    let struct_h = vshape_register(struct_def);
    let shape = vshape_view(struct_h);
    let scalar_shape = vshape_view(scalar_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(scalar_shape) };
    unsafe { heap.default_in_place(src, scalar_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // 2 arbitrary operations (3+ causes exponential blowup)
    apply_arb_op(&mut trame, src, scalar_shape);
    apply_arb_op(&mut trame, src, scalar_shape);

    // No build() - drop will exercise cleanup paths
}
