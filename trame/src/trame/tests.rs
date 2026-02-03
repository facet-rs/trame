use super::*;
use crate::runtime::live::*;
use crate::runtime::verified::*;
use core::alloc::Layout;
use facet_core::Facet;

#[test]
fn scalar_lifecycle_verified() {
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
            dst: &root,
            src: Source::Imm(src),
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
    trame
        .apply(Op::Set {
            dst: &root,
            src: Source::Imm(src),
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
    let src = (&mut value as *mut u32).cast::<u8>();

    assert!(!trame.is_complete());
    let root: [PathSegment; 0] = [];
    trame
        .apply(Op::Set {
            dst: &root,
            src: Source::Imm(src),
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
            dst: &[],
            src: Source::imm_ref(&mut 123_u32),
        })
        .unwrap();
    assert!(trame.is_complete());

    let hv = trame.build().unwrap();
    let value = hv.materialize::<u32>().unwrap();
    assert_eq!(value, 123);
}

//     #[test]
//     fn struct_lifecycle() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src0 = unsafe { heap.alloc(u32_shape) };
//         let src1 = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src0, u32_shape) };
//         unsafe { heap.default_in_place(src1, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         assert!(!trame.is_complete());

//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src0),
//             })
//             .unwrap();
//         assert!(!trame.is_complete());

//         let f1 = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &f1,
//                 src: Source::Imm(src1),
//             })
//             .unwrap();
//         assert!(trame.is_complete());

//         let _ = trame.build().unwrap();
//     }

//     #[test]
//     fn struct_any_order() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def =
//             VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h), (8, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src0 = unsafe { heap.alloc(u32_shape) };
//         let src1 = unsafe { heap.alloc(u32_shape) };
//         let src2 = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src0, u32_shape) };
//         unsafe { heap.default_in_place(src1, u32_shape) };
//         unsafe { heap.default_in_place(src2, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         // Init in reverse order
//         let f2 = [PathSegment::Field(2)];
//         trame
//             .apply(Op::Set {
//                 dst: &f2,
//                 src: Source::Imm(src2),
//             })
//             .unwrap();
//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src0),
//             })
//             .unwrap();
//         let f1 = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &f1,
//                 src: Source::Imm(src1),
//             })
//             .unwrap();

//         assert!(trame.is_complete());
//         let _ = trame.build().unwrap();
//     }

//     #[test]
//     fn stage_field_twice_reenters() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
//         let inner_h = store.add(inner_def);
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);

//         let heap = TestHeap::new();
//         let arena = TestArena::new();

//         let mut trame = unsafe { Trame::new(shape) };

//         let field0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &field0,
//                 src: Source::Stage(None),
//             })
//             .unwrap();

//         let inner_a = [PathSegment::Field(0)];
//         let u32_shape = store.view(u32_h);
//         let src = unsafe { trame.heap.alloc(u32_shape) };
//         unsafe { trame.heap.default_in_place(src, u32_shape) };
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();
//         trame.apply(Op::End).unwrap();

//         let root_field0 = [PathSegment::Root, PathSegment::Field(0)];
//         let result = trame.apply(Op::Set {
//             dst: &root_field0,
//             src: Source::Stage(None),
//         });
//         assert!(result.is_ok());
//         assert_eq!(trame.depth(), 1);
//     }

//     #[test]
//     fn incomplete_build_fails() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         let err = trame.build();
//         assert!(matches!(err, Err(TrameError::Incomplete)));
//     }

//     #[test]
//     fn drop_cleans_up() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         drop(trame);
//     }

//     // --- Nested struct tests ---

//     #[test]
//     fn nested_struct_begin_end() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32, b: u32 }
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let inner_h = store.add(inner_def);

//         // Outer struct: { x: u32, inner: Inner }
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src_x = unsafe { heap.alloc(u32_shape) };
//         let src_a = unsafe { heap.alloc(u32_shape) };
//         let src_b = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src_x, u32_shape) };
//         unsafe { heap.default_in_place(src_a, u32_shape) };
//         unsafe { heap.default_in_place(src_b, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         assert_eq!(trame.depth(), 0);
//         assert!(!trame.is_complete());

//         let outer_x = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &outer_x,
//                 src: Source::Imm(src_x),
//             })
//             .unwrap();
//         assert!(!trame.is_complete());

//         let inner_field = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         assert_eq!(trame.depth(), 1);

//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src_a),
//             })
//             .unwrap();
//         let inner_b = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_b,
//                 src: Source::Imm(src_b),
//             })
//             .unwrap();
//         assert!(trame.is_complete());

//         trame.apply(Op::End).unwrap();
//         assert_eq!(trame.depth(), 0);
//         assert!(trame.is_complete());

//         let _ = trame.build().unwrap();
//     }

//     #[test]
//     fn nested_struct_drop_cleans_up() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32 }
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
//         let inner_h = store.add(inner_def);

//         // Outer struct: { inner: Inner }
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         drop(trame);
//     }

//     #[test]
//     fn nested_struct_trame_inner_cleanup() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32, b: u32 }
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let inner_h = store.add(inner_def);

//         // Outer struct: { inner: Inner }
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         drop(trame);
//     }

//     #[test]
//     fn end_op_incomplete_fails() {
//         let mut store = VShapeStore::new();

//         // Inner struct: { a: u32, b: u32 }
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let inner_h = store.add(inner_def);

//         // Outer struct: { inner: Inner }
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, inner_h)]);
//         let outer_h = store.add(outer_def);
//         let shape = store.view(outer_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let inner_field = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();
//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         let err = trame.apply(Op::End);
//         assert_eq!(err, Err(TrameError::CurrentIncomplete));
//     }

//     #[test]
//     fn end_op_at_root_fails() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let f0 = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &f0,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         let err = trame.apply(Op::End);
//         assert_eq!(err, Err(TrameError::AtRoot));
//     }

//     #[test]
//     fn apply_set_imm_field() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h)]);
//         let struct_h = store.add(struct_def);
//         let shape = store.view(struct_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let path = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &path,
//                 src: Source::Imm(src),
//             })
//             .unwrap();

//         assert!(trame.is_complete());
//         let _ = trame.build().unwrap();
//     }

//     #[test]
//     fn apply_stage_and_end() {
//         let mut store = VShapeStore::new();
//         let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
//         let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
//         let inner_h = store.add(inner_def);
//         let outer_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, inner_h)]);
//         let outer_h = store.add(outer_def);

//         let shape = store.view(outer_h);
//         let u32_shape = store.view(u32_h);

//         let mut heap = TestHeap::new();
//         let src1 = unsafe { heap.alloc(u32_shape) };
//         let src2 = unsafe { heap.alloc(u32_shape) };
//         unsafe { heap.default_in_place(src1, u32_shape) };
//         unsafe { heap.default_in_place(src2, u32_shape) };

//         let arena = TestArena::new();
//         let mut trame = unsafe { Trame::new(shape) };

//         let outer_x = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &outer_x,
//                 src: Source::Imm(src1),
//             })
//             .unwrap();

//         let inner_field = [PathSegment::Field(1)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_field,
//                 src: Source::Stage(None),
//             })
//             .unwrap();

//         let inner_a = [PathSegment::Field(0)];
//         trame
//             .apply(Op::Set {
//                 dst: &inner_a,
//                 src: Source::Imm(src2),
//             })
//             .unwrap();

//         let inner_b = [PathSegment::Field(1)];
//         let src3 = unsafe { trame.heap.alloc(u32_shape) };
//         unsafe { trame.heap.default_in_place(src3, u32_shape) };
//         trame
//             .apply(Op::Set {
//                 dst: &inner_b,
//                 src: Source::Imm(src3),
//             })
//             .unwrap();

//         trame.apply(Op::End).unwrap();
//         assert!(trame.is_complete());
//         let _ = trame.build().unwrap();
//     }
