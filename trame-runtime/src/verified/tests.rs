use super::*;
use crate::IHeap;
use crate::live::LHeap;
use core::alloc::Layout;
use core::sync::atomic::{AtomicUsize, Ordering};
use facet_core::{Facet, Shape};

type S<'a> = VShapeView<'a, VShapeStore>;

static DROP_HOOK_CALLS: AtomicUsize = AtomicUsize::new(0);

unsafe fn count_drop_hook(_ptr: *mut u8) {
    DROP_HOOK_CALLS.fetch_add(1, Ordering::SeqCst);
}

unsafe fn write_magic_u32_default(ptr: *mut u8) -> bool {
    unsafe {
        ptr.cast::<u32>().write(0xA11C_E555);
    }
    true
}

unsafe fn copy_usize_new_into(dst: *mut u8, src: *mut u8) {
    let value = unsafe { core::ptr::read(src.cast::<usize>()) };
    unsafe {
        core::ptr::write(dst.cast::<usize>(), value);
    }
}

#[test]
fn verified_alloc_dealloc() {
    let mut store = VShapeStore::new();
    let h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    let shape = store.view(h);

    let mut heap = VHeap::<S<'_>>::new();
    let ptr = unsafe { heap.alloc(shape) };

    assert!(ptr.is_at_start());
    assert_eq!(ptr.alloc_size(), 4);

    unsafe { heap.dealloc(ptr, shape) };
    heap.assert_no_leaks();
}

#[test]
fn lheap_vshape_exec_hooks_run() {
    DROP_HOOK_CALLS.store(0, Ordering::SeqCst);

    let mut store = VShapeStore::new();
    let shape_h = store.add(VShapeDef::scalar_with_ops(
        Layout::new::<u32>(),
        VTypeOps::new(true, count_drop_hook, Some(write_magic_u32_default)),
    ));
    let shape = store.view(shape_h);

    let mut heap = LHeap::new();
    let ptr = unsafe { heap.alloc(shape) };
    let did_default = unsafe { heap.default_in_place(ptr, shape) };
    assert!(did_default);
    assert_eq!(unsafe { ptr.cast::<u32>().read() }, 0xA11C_E555);

    unsafe {
        heap.drop_in_place(ptr, shape);
        heap.dealloc(ptr, shape);
    }
    assert_eq!(DROP_HOOK_CALLS.load(Ordering::SeqCst), 1);
}

#[test]
fn lheap_vshape_needs_drop_false_skips_drop_hook() {
    DROP_HOOK_CALLS.store(0, Ordering::SeqCst);

    let mut store = VShapeStore::new();
    let shape_h = store.add(VShapeDef::scalar_with_ops(
        Layout::new::<u32>(),
        VTypeOps::new(false, count_drop_hook, None),
    ));
    let shape = store.view(shape_h);

    let mut heap = LHeap::new();
    let ptr = unsafe { heap.alloc(shape) };
    unsafe {
        heap.drop_in_place(ptr, shape);
        heap.dealloc(ptr, shape);
    }
    assert_eq!(DROP_HOOK_CALLS.load(Ordering::SeqCst), 0);
}

#[test]
fn lheap_vshape_pointer_new_into_hook_runs() {
    let mut store = VShapeStore::new();
    let pointee_h = store.add(VShapeDef::scalar(Layout::new::<usize>()));
    let pointer_h = store.add(VShapeDef::pointer_to_with_ops(
        pointee_h,
        true,
        false,
        VTypeOps::pod(),
        VPointerVTable::new(Some(copy_usize_new_into)),
    ));

    let pointee_shape = store.view(pointee_h);
    let pointer_shape = store.view(pointer_h);

    let mut heap = LHeap::new();
    let src = unsafe { heap.alloc(pointee_shape) };
    unsafe {
        src.cast::<usize>().write(0xDEAD_BEEF_usize);
    }
    let dst = unsafe { heap.alloc(pointer_shape) };

    let ok = unsafe { heap.pointer_from_pointee(dst, pointer_shape, src, pointee_shape) };
    assert!(ok);
    assert_eq!(unsafe { dst.cast::<usize>().read() }, 0xDEAD_BEEF_usize);

    unsafe {
        heap.dealloc_moved(src, pointee_shape);
        heap.dealloc(dst, pointer_shape);
    }
}

#[test]
fn vshape_scalar_is_not_struct() {
    let mut store = VShapeStore::new();
    let h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    let s = store.view(h);
    assert!(!s.is_struct());
    assert!(s.as_struct().is_none());
}

#[test]
fn vshape_struct_is_struct() {
    let mut store = VShapeStore::new();
    // Add field shapes first
    let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));

    // Create struct with two u32 fields
    let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    let struct_h = store.add(struct_def);

    let s = store.view(struct_h);
    assert!(s.is_struct());
    assert!(s.as_struct().is_some());
}

#[test]
fn vshape_struct_field_access() {
    let mut store = VShapeStore::new();
    // Add field shapes
    let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    let u64_h = store.add(VShapeDef::scalar(Layout::new::<u64>()));

    // Create struct
    let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (8, u64_h)]);
    let struct_h = store.add(struct_def);

    let s = store.view(struct_h);
    let st = s.as_struct().unwrap();

    assert_eq!(st.field_count(), 2);

    let f0 = st.field(0).unwrap();
    assert_eq!(f0.offset(), 0);
    assert_eq!(f0.shape().layout().unwrap().size(), 4);

    let f1 = st.field(1).unwrap();
    assert_eq!(f1.offset(), 8);
    assert_eq!(f1.shape().layout().unwrap().size(), 8);

    assert!(st.field(2).is_none());
}

#[test]
fn vshape_nested_struct() {
    let mut store = VShapeStore::new();

    // Inner struct: { a: u32, b: u32 }
    let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
    let inner_h = store.add(inner_def);

    // Outer struct: { x: u64, inner: Inner }
    let u64_h = store.add(VShapeDef::scalar(Layout::new::<u64>()));
    let outer_def = VShapeDef::struct_with_fields(
        &store,
        &[
            (0, u64_h),
            (8, inner_h), // inner struct at offset 8
        ],
    );
    let outer_h = store.add(outer_def);

    // Verify outer struct
    let outer = store.view(outer_h);
    assert!(outer.is_struct());
    let outer_st = outer.as_struct().unwrap();
    assert_eq!(outer_st.field_count(), 2);

    // Field 0: u64
    let f0 = outer_st.field(0).unwrap();
    assert_eq!(f0.offset(), 0);
    assert!(!f0.shape().is_struct());
    assert_eq!(f0.shape().layout().unwrap().size(), 8);

    // Field 1: inner struct
    let f1 = outer_st.field(1).unwrap();
    assert_eq!(f1.offset(), 8);
    assert!(f1.shape().is_struct()); // This is the key test!

    // Navigate into inner struct
    let inner_st = f1.shape().as_struct().unwrap();
    assert_eq!(inner_st.field_count(), 2);

    let inner_f0 = inner_st.field(0).unwrap();
    assert_eq!(inner_f0.offset(), 0);
    assert_eq!(inner_f0.shape().layout().unwrap().size(), 4);

    let inner_f1 = inner_st.field(1).unwrap();
    assert_eq!(inner_f1.offset(), 4);
    assert_eq!(inner_f1.shape().layout().unwrap().size(), 4);
}

#[test]
fn vshape_option_is_not_struct_or_pointer() {
    let mut store = VShapeStore::new();
    let payload_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
    let option_h = store.add(VShapeDef::option_of(
        payload_h,
        Layout::new::<Option<u32>>(),
        VTypeOps::pod(),
    ));

    let option_shape = store.view(option_h);
    assert!(!option_shape.is_struct());
    assert!(option_shape.as_struct().is_none());
    assert!(!option_shape.is_pointer());
    assert!(option_shape.as_pointer().is_none());
    assert_eq!(option_shape.layout().unwrap(), Layout::new::<Option<u32>>());

    match store.get_def(option_h).def {
        VDef::Option(def) => assert_eq!(def.some_handle, payload_h),
        _ => panic!("expected option def"),
    }
}

#[test]
fn vshape_enum_exposes_variants_and_payload_fields() {
    let mut store = VShapeStore::new();
    let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));

    let mut payload_fields = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
    payload_fields[0] = VFieldDef::new(4, u32_h);
    payload_fields[1] = VFieldDef::new(8, u32_h);
    let pair_payload = VStructDef {
        field_count: 2,
        fields: payload_fields,
    };

    let variants = [
        VVariantDef::new("Unit", "Unit", Some(0), VStructDef::empty()),
        VVariantDef::new("Pair", "Pair", Some(1), pair_payload),
    ];
    let enum_h = store.add(VShapeDef::enum_with_variants_and_repr(
        Layout::from_size_align(16, 4).expect("valid enum layout"),
        EnumReprKind::ExternallyTagged,
        EnumDiscriminantRepr::U32,
        &variants,
        VTypeOps::pod(),
    ));

    let shape = store.view(enum_h);
    assert!(shape.is_enum());
    assert_eq!(shape.enum_repr_kind(), Some(EnumReprKind::ExternallyTagged));

    let en = shape.as_enum().expect("enum metadata should be present");
    assert_eq!(en.variant_count(), 2);
    assert!(en.variant(2).is_none());

    let unit = en.variant(0).expect("unit variant should exist");
    assert_eq!(unit.name(), "Unit");
    assert_eq!(unit.effective_name(), "Unit");
    assert_eq!(unit.data().field_count(), 0);

    let pair = en.variant(1).expect("pair variant should exist");
    assert_eq!(pair.name(), "Pair");
    assert_eq!(pair.effective_name(), "Pair");
    let payload = pair.data();
    assert_eq!(payload.field_count(), 2);
    assert_eq!(payload.field(0).unwrap().offset(), 4);
    assert_eq!(payload.field(1).unwrap().offset(), 8);
    assert_eq!(
        payload.field(0).unwrap().shape().layout().unwrap().size(),
        4
    );
}

// Tests for real Shape implementation

#[derive(facet::Facet)]
struct TestStruct {
    a: u32,
    b: u64,
}

#[test]
fn real_shape_is_struct() {
    let shape: &'static Shape = TestStruct::SHAPE;
    assert!(shape.is_struct());
    assert!(shape.as_struct().is_some());
}

#[test]
fn real_shape_field_access() {
    let shape: &'static Shape = TestStruct::SHAPE;
    let st = shape.as_struct().unwrap();

    assert_eq!(st.field_count(), 2);

    // Just verify we can access fields - don't assume layout
    let f0 = st.field(0).unwrap();
    let f1 = st.field(1).unwrap();

    // Fields exist and have non-zero sized shapes
    assert!(f0.shape().layout().unwrap().size() > 0);
    assert!(f1.shape().layout().unwrap().size() > 0);

    assert!(st.field(2).is_none());
}

#[test]
fn real_scalar_is_not_struct() {
    let shape: &'static Shape = u32::SHAPE;
    assert!(!shape.is_struct());
    assert!(shape.as_struct().is_none());
}

#[derive(facet::Facet)]
struct Inner {
    a: u32,
    b: u32,
}

#[derive(facet::Facet)]
struct Outer {
    x: u64,
    inner: Inner,
}

#[test]
fn real_nested_struct() {
    let shape: &'static Shape = Outer::SHAPE;
    assert!(shape.is_struct());

    let st = shape.as_struct().unwrap();
    assert_eq!(st.field_count(), 2);

    // Find the 'inner' field (don't assume order)
    let inner_field = st
        .field(0)
        .filter(|f| f.shape().is_struct())
        .or_else(|| st.field(1).filter(|f| f.shape().is_struct()))
        .expect("should have a struct field");

    // Navigate into inner struct
    let inner_st = inner_field.shape().as_struct().unwrap();
    assert_eq!(inner_st.field_count(), 2);
}

// --- VArena tests ---

#[test]
fn verified_arena_alloc_and_get() {
    let mut arena = VArena::<u32, 8>::new();
    let id = arena.alloc(42);

    assert!(id.is_valid());
    assert_eq!(*arena.get(id), 42);
}

#[test]
fn verified_arena_free_and_reuse() {
    let mut arena = VArena::<u32, 8>::new();

    let id1 = arena.alloc(1);
    let _id2 = arena.alloc(2);

    let val = arena.free(id1);
    assert_eq!(val, 1);

    // Next alloc can reuse freed slot (or use a new one)
    let id3 = arena.alloc(3);
    assert!(id3.is_valid());
    assert_eq!(*arena.get(id3), 3);
}

#[test]
#[should_panic(expected = "double-free")]
fn verified_arena_double_free_panics() {
    let mut arena = VArena::<u32, 8>::new();
    let id = arena.alloc(1);
    arena.free(id);
    arena.free(id);
}

#[test]
fn verified_arena_get_mut() {
    let mut arena = VArena::<u32, 8>::new();
    let id = arena.alloc(1);

    *arena.get_mut(id) = 99;
    assert_eq!(*arena.get(id), 99);
}
