use core::alloc::Layout;
use facet_core::{Field, Shape, StructType, Type, UserType};

use crate::runtime::{IField, IShape, IShapeStore, IStructType};

// ============================================================================
// Traits
// ============================================================================

// ============================================================================
// IShapeStore implementation for static shapes
// ============================================================================

/// A zero-sized "store" for static shapes.
///
/// For `&'static Shape`, shapes live in static memory and don't need
/// a separate store. The handle IS the shape reference.
#[derive(Clone, Copy, Default)]
pub struct StaticShapeStore;

impl IShapeStore for StaticShapeStore {
    type Handle = &'static Shape;
    type View<'a>
        = &'static Shape
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        handle
    }
}

// ============================================================================
// IShape implementation for &'static Shape (real shapes)
// ============================================================================

impl IShape for &'static Shape {
    type StructType = &'static StructType;
    type Field = &'static Field;

    #[inline]
    fn layout(&self) -> Layout {
        self.layout
            .sized_layout()
            .expect("IShape requires sized types")
    }

    #[inline]
    fn is_struct(&self) -> bool {
        matches!(self.ty, Type::User(UserType::Struct(_)))
    }

    #[inline]
    fn as_struct(&self) -> Option<Self::StructType> {
        match &self.ty {
            Type::User(UserType::Struct(st)) => Some(st),
            _ => None,
        }
    }

    #[inline]
    unsafe fn drop_in_place(&self, ptr: *mut u8) {
        // Use the shape's call_drop_in_place method.
        // This handles types with destructors through the vtable.
        unsafe { self.call_drop_in_place(facet_core::PtrMut::new(ptr)) };
    }

    #[inline]
    unsafe fn default_in_place(&self, ptr: *mut u8) -> bool {
        unsafe {
            self.call_default_in_place(facet_core::PtrMut::new(ptr).into())
                .is_some()
        }
    }
}

impl IStructType for &'static StructType {
    type Field = &'static Field;

    #[inline]
    fn field_count(&self) -> usize {
        self.fields.len()
    }

    #[inline]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        self.fields.get(idx)
    }
}

impl IField for &'static Field {
    type Shape = &'static Shape;

    #[inline]
    fn offset(&self) -> usize {
        self.offset
    }

    #[inline]
    fn shape(&self) -> Self::Shape {
        self.shape.get()
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use facet_core::Facet;

    #[test]
    fn scalar_is_not_struct() {
        let mut store = DynShapeStore::new();
        let h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let s = store.view(h);
        assert!(!s.is_struct());
        assert!(s.as_struct().is_none());
    }

    #[test]
    fn struct_is_struct() {
        let mut store = DynShapeStore::new();
        // Add field shapes first
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));

        // Create struct with two u32 fields
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);

        let s = store.view(struct_h);
        assert!(s.is_struct());
        assert!(s.as_struct().is_some());
    }

    #[test]
    fn struct_field_access() {
        let mut store = DynShapeStore::new();
        // Add field shapes
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let u64_h = store.add(DynShapeDef::scalar(Layout::new::<u64>()));

        // Create struct
        let struct_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (8, u64_h)]);
        let struct_h = store.add(struct_def);

        let s = store.view(struct_h);
        let st = s.as_struct().unwrap();

        assert_eq!(st.field_count(), 2);

        let f0 = st.field(0).unwrap();
        assert_eq!(f0.offset(), 0);
        assert_eq!(f0.shape().layout().size(), 4);

        let f1 = st.field(1).unwrap();
        assert_eq!(f1.offset(), 8);
        assert_eq!(f1.shape().layout().size(), 8);

        assert!(st.field(2).is_none());
    }

    #[test]
    fn nested_struct() {
        let mut store = DynShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(DynShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = DynShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { x: u64, inner: Inner }
        let u64_h = store.add(DynShapeDef::scalar(Layout::new::<u64>()));
        let outer_def = DynShapeDef::struct_with_fields(
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
        assert_eq!(f0.shape().layout().size(), 8);

        // Field 1: inner struct
        let f1 = outer_st.field(1).unwrap();
        assert_eq!(f1.offset(), 8);
        assert!(f1.shape().is_struct()); // This is the key test!

        // Navigate into inner struct
        let inner_st = f1.shape().as_struct().unwrap();
        assert_eq!(inner_st.field_count(), 2);

        let inner_f0 = inner_st.field(0).unwrap();
        assert_eq!(inner_f0.offset(), 0);
        assert_eq!(inner_f0.shape().layout().size(), 4);

        let inner_f1 = inner_st.field(1).unwrap();
        assert_eq!(inner_f1.offset(), 4);
        assert_eq!(inner_f1.shape().layout().size(), 4);
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
        assert!(f0.shape().layout().size() > 0);
        assert!(f1.shape().layout().size() > 0);

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
}

// ============================================================================
// Kani Proofs
// ============================================================================

#[cfg(kani)]
mod kani_proofs {
    use super::*;

    /// Prove: store handles are valid after adding shapes
    #[kani::proof]
    #[kani::unwind(10)]
    fn store_handles_valid() {
        let mut store = DynShapeStore::new();

        let num_shapes: u8 = kani::any();
        kani::assume(num_shapes > 0 && num_shapes <= 4);

        for i in 0..num_shapes {
            let size: usize = kani::any();
            kani::assume(size > 0 && size <= 8);
            let layout = Layout::from_size_align(size, 1).unwrap();
            let h = store.add(DynShapeDef::scalar(layout));
            kani::assert(h.0 == i, "handle matches index");
        }

        // All handles should be retrievable
        for i in 0..num_shapes {
            let view = store.view(DynShapeHandle(i));
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
        let mut store = DynShapeStore::new();

        // Scalar shape
        let scalar_h = store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        // Inner struct with 2 scalar fields
        let inner_def = DynShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        // Outer struct with scalar + inner struct
        let outer_def = DynShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
                    arr[0] = DynFieldDef::new(0, scalar_h);
                    arr[1] = DynFieldDef::new(4, inner_h);
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
            inner_field0.shape().layout().size() == 4,
            "inner field 0 is 4 bytes",
        );
    }

    /// Prove: field shape handles point to valid shapes in store
    #[kani::proof]
    #[kani::unwind(10)]
    fn field_handles_valid() {
        let mut store = DynShapeStore::new();

        // Add scalars
        let num_scalars: u8 = kani::any();
        kani::assume(num_scalars >= 1 && num_scalars <= 3);

        for _ in 0..num_scalars {
            store.add(DynShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        }

        // Add a struct referencing those scalars
        let field_count: u8 = kani::any();
        kani::assume(field_count > 0 && field_count <= 3);

        let mut fields = [DynFieldDef::new(0, DynShapeHandle(0)); MAX_FIELDS];
        for i in 0..(field_count as usize) {
            let shape_idx: u8 = kani::any();
            kani::assume(shape_idx < num_scalars);
            fields[i] = DynFieldDef::new(i * 4, DynShapeHandle(shape_idx));
        }

        let struct_def = DynShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: DynDef::Struct(DynStructDef {
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
            let _layout = field_shape.layout();
            kani::assert(!field_shape.is_struct(), "field points to scalar");
        }
    }
}
