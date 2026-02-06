use super::*;
use crate::Path;
use crate::runtime::live::*;
use crate::runtime::verified::*;
use crate::vshape_store_reset;
use core::alloc::Layout;
use facet_core::Facet;

/// Guard that resets the global shape store on creation and drop.
struct FreshStore;

impl FreshStore {
    fn new() -> Self {
        unsafe { vshape_store_reset() };
        Self
    }
}

impl Drop for FreshStore {
    fn drop(&mut self) {
        // Don't reset during panic - could cause double-panic
        if !std::thread::panicking() {
            unsafe { vshape_store_reset() };
        }
    }
}

#[test]
fn scalar_lifecycle_verified() {
    let _g = FreshStore::new();
    let h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let shape = vshape_view(h);
    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(shape) };
    unsafe { heap.default_in_place(src, shape) };
    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    assert!(!trame.is_complete());
    let root: [PathSegment; 0] = [];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&root),
            src: unsafe { Source::from_vptr(src, shape) },
        })
        .unwrap();
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}

#[test]
fn scalar_lifecycle_live() {
    let shape = u32::SHAPE;
    let mut heap = LRuntime::heap();
    let src = unsafe { heap.alloc(shape) };
    unsafe { heap.default_in_place(src, shape) };
    let mut trame = unsafe { Trame::<LRuntime>::new(heap, shape) };

    assert!(!trame.is_complete());
    let root: [PathSegment; 0] = [];
    let mut value: u32 = 0;
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&root),
            src: Source::from_ref(&mut value),
        })
        .unwrap();
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}

#[test]
fn scalar_lifecycle_live_stack_src() {
    let shape = u32::SHAPE;
    let heap = LRuntime::heap();
    let mut trame = unsafe { Trame::<LRuntime>::new(heap, shape) };

    let mut value: u32 = 123;

    assert!(!trame.is_complete());
    let root: [PathSegment; 0] = [];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&root),
            src: Source::from_ref(&mut value),
        })
        .unwrap();
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}

#[test]
fn scalar_lifecycle_live_alloc() {
    let mut trame = Trame::<LRuntime>::alloc::<u32>().unwrap();

    assert!(!trame.is_complete());
    trame
        .apply(Op::Set {
            dst: Path::empty(),
            src: Source::from_ref(&mut 123_u32),
        })
        .unwrap();
    assert!(trame.is_complete());

    let hv = trame.build().unwrap();
    let value = hv.materialize::<u32>().unwrap();
    assert_eq!(value, 123);
}

#[test]
fn struct_lifecycle() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src0 = unsafe { heap.alloc(u32_shape) };
    let src1 = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src0, u32_shape) };
    unsafe { heap.default_in_place(src1, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    assert!(!trame.is_complete());

    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src0, u32_shape) },
        })
        .unwrap();
    assert!(!trame.is_complete());

    let f1 = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f1),
            src: unsafe { Source::from_vptr(src1, u32_shape) },
        })
        .unwrap();
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}

#[test]
fn struct_any_order() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h), (8, u32_h)],
    ));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src0 = unsafe { heap.alloc(u32_shape) };
    let src1 = unsafe { heap.alloc(u32_shape) };
    let src2 = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src0, u32_shape) };
    unsafe { heap.default_in_place(src1, u32_shape) };
    unsafe { heap.default_in_place(src2, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    // Init in reverse order
    let f2 = [PathSegment::Field(2)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f2),
            src: unsafe { Source::from_vptr(src2, u32_shape) },
        })
        .unwrap();
    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src0, u32_shape) },
        })
        .unwrap();
    let f1 = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f1),
            src: unsafe { Source::from_vptr(src1, u32_shape) },
        })
        .unwrap();

    assert!(trame.is_complete());
    let _ = trame.build().unwrap();
}

#[test]
fn stage_field_twice_reenters() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &[(0, u32_h)]));
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, inner_h)],
    ));
    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let field0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&field0),
            src: Source::stage(None),
        })
        .unwrap();

    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();
    trame.apply(Op::End).unwrap();

    let root_field0 = [PathSegment::Root, PathSegment::Field(0)];
    let result = trame.apply(Op::Set {
        dst: Path::from_segments(&root_field0),
        src: Source::stage(None),
    });
    assert!(result.is_ok());
    assert_eq!(trame.depth(), 1);
}

#[test]
fn incomplete_build_fails() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    let err = trame.build();
    assert!(matches!(err, Err(TrameError::Incomplete)));
}

#[test]
fn drop_cleans_up() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    drop(trame);
}

// --- Nested struct tests ---

#[test]
fn nested_struct_begin_end() {
    let _g = FreshStore::new();
    // Inner struct: { a: u32, b: u32 }
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));

    // Outer struct: { x: u32, inner: Inner }
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, inner_h)],
    ));
    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src_x = unsafe { heap.alloc(u32_shape) };
    let src_a = unsafe { heap.alloc(u32_shape) };
    let src_b = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src_x, u32_shape) };
    unsafe { heap.default_in_place(src_a, u32_shape) };
    unsafe { heap.default_in_place(src_b, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    assert_eq!(trame.depth(), 0);
    assert!(!trame.is_complete());

    let outer_x = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&outer_x),
            src: unsafe { Source::from_vptr(src_x, u32_shape) },
        })
        .unwrap();
    assert!(!trame.is_complete());

    let inner_field = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_field),
            src: Source::stage(None),
        })
        .unwrap();
    assert_eq!(trame.depth(), 1);

    let inner_a = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_a),
            src: unsafe { Source::from_vptr(src_a, u32_shape) },
        })
        .unwrap();
    let inner_b = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_b),
            src: unsafe { Source::from_vptr(src_b, u32_shape) },
        })
        .unwrap();
    assert!(trame.is_complete());

    trame.apply(Op::End).unwrap();
    assert_eq!(trame.depth(), 0);
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}

#[test]
fn nested_struct_drop_cleans_up() {
    let _g = FreshStore::new();
    // Inner struct: { a: u32 }
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &[(0, u32_h)]));

    // Outer struct: { inner: Inner }
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, inner_h)],
    ));
    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

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
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    drop(trame);
}

#[test]
fn nested_struct_trame_inner_cleanup() {
    let _g = FreshStore::new();
    // Inner struct: { a: u32, b: u32 }
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));

    // Outer struct: { inner: Inner }
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, inner_h)],
    ));
    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

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
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    drop(trame);
}

#[test]
fn end_op_incomplete_fails() {
    let _g = FreshStore::new();
    // Inner struct: { a: u32, b: u32 }
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));

    // Outer struct: { inner: Inner }
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, inner_h)],
    ));
    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

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
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    let err = trame.apply(Op::End);
    assert_eq!(err, Err(TrameError::CurrentIncomplete));
}

#[test]
fn end_op_at_root_fails() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &[(0, u32_h)]));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let f0 = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&f0),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    let err = trame.apply(Op::End);
    assert_eq!(err, Err(TrameError::AtRoot));
}

#[test]
fn apply_set_imm_field() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let struct_h = vshape_register(VShapeDef::struct_with_fields(vshape_store(), &[(0, u32_h)]));
    let shape = vshape_view(struct_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let path = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&path),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    assert!(trame.is_complete());
    let _ = trame.build().unwrap();
}

#[test]
fn apply_stage_and_end() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, u32_h)],
    ));
    let outer_h = vshape_register(VShapeDef::struct_with_fields(
        vshape_store(),
        &[(0, u32_h), (4, inner_h)],
    ));

    let shape = vshape_view(outer_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src1 = unsafe { heap.alloc(u32_shape) };
    let src2 = unsafe { heap.alloc(u32_shape) };
    let src3 = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src1, u32_shape) };
    unsafe { heap.default_in_place(src2, u32_shape) };
    unsafe { heap.default_in_place(src3, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    let outer_x = [PathSegment::Field(0)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&outer_x),
            src: unsafe { Source::from_vptr(src1, u32_shape) },
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
            src: unsafe { Source::from_vptr(src2, u32_shape) },
        })
        .unwrap();

    let inner_b = [PathSegment::Field(1)];
    trame
        .apply(Op::Set {
            dst: Path::from_segments(&inner_b),
            src: unsafe { Source::from_vptr(src3, u32_shape) },
        })
        .unwrap();

    trame.apply(Op::End).unwrap();
    assert!(trame.is_complete());
    let _ = trame.build().unwrap();
}

#[test]
fn box_live_stage_end_builds() {
    let mut trame = Trame::<LRuntime>::alloc::<Box<u32>>().unwrap();

    trame
        .apply(Op::Set {
            dst: Path::from_segments(&[PathSegment::Field(0)]),
            src: Source::stage(None),
        })
        .unwrap();
    assert_eq!(trame.depth(), 1);

    let mut value = 42_u32;
    trame
        .apply(Op::Set {
            dst: Path::empty(),
            src: Source::from_ref(&mut value),
        })
        .unwrap();
    trame.apply(Op::End).unwrap();

    assert_eq!(trame.depth(), 0);
    assert!(trame.is_complete());

    let hv = trame.build().unwrap();
    let value = hv.materialize::<Box<u32>>().unwrap();
    assert_eq!(*value, 42);
}

#[test]
fn box_live_imm_whole_value() {
    let mut trame = Trame::<LRuntime>::alloc::<Box<u32>>().unwrap();
    let mut boxed = Box::new(9_u32);
    trame
        .apply(Op::Set {
            dst: Path::empty(),
            src: Source::from_ref(&mut boxed),
        })
        .unwrap();
    core::mem::forget(boxed);

    assert!(trame.is_complete());
    let hv = trame.build().unwrap();
    let value = hv.materialize::<Box<u32>>().unwrap();
    assert_eq!(*value, 9);
}

#[test]
fn box_verified_stage_end_builds() {
    let _g = FreshStore::new();
    let u32_h = vshape_register(VShapeDef::scalar(Layout::new::<u32>()));
    let box_h = vshape_register(VShapeDef::pointer_to(u32_h, true, true));
    let shape = vshape_view(box_h);
    let u32_shape = vshape_view(u32_h);

    let mut heap = VRuntime::heap();
    let src = unsafe { heap.alloc(u32_shape) };
    unsafe { heap.default_in_place(src, u32_shape) };

    let mut trame = unsafe { Trame::<VRuntime>::new(heap, shape) };

    trame
        .apply(Op::Set {
            dst: Path::from_segments(&[PathSegment::Field(0)]),
            src: Source::stage(None),
        })
        .unwrap();
    assert_eq!(trame.depth(), 1);

    trame
        .apply(Op::Set {
            dst: Path::empty(),
            src: unsafe { Source::from_vptr(src, u32_shape) },
        })
        .unwrap();

    trame.apply(Op::End).unwrap();
    assert_eq!(trame.depth(), 0);
    assert!(trame.is_complete());

    let _ = trame.build().unwrap();
}
