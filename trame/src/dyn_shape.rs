//! Shape abstractions for formal verification.
//!
//! In regular operation, trame uses `&'static Shape` from facet_core.
//! However, for testing, we use `DynShape`, which implements Arbitrary.
//! Both implement `IShape`, a common interface for shapes.
//!
//! The `Partial` type is generic over `S: IShape`, so:
//! - Production: `Partial<&'static Shape>`
//! - Kani proofs: `Partial<DynShape>`
//!
//! # Important: Layout is not the point
//!
//! Rust's struct layout is **not stable, not guaranteed, and not documented**.
//! The compiler is free to reorder fields, insert padding, and generally do
//! whatever it wants (unless you use `#[repr(C)]` or similar).
//!
//! **We are not trying to replicate real Rust layouts here.**
//!
//! For Kani verification, what matters is the **state machine**, not the memory layout:
//! - How many fields need independent initialization tracking?
//! - What are the valid state transitions? (Unallocated → Allocated → Initialized → ...)
//! - Are all invariants maintained? (no double-init, no drop-before-init, no leak)
//!
//! # Store-based shape system
//!
//! To support nested structs (a struct containing another struct as a field),
//! we use a store-based approach that avoids recursive type definitions:
//!
//! - For `&'static Shape`: The "store" is implicit (static memory), and the
//!   handle is just the reference itself.
//! - For `DynShape`: The store is an array of shape definitions, and fields
//!   reference shapes by index (u8).
//!
//! This breaks the type-level recursion by putting the recursion in data
//! instead of types.

use core::alloc::Layout;
use facet_core::{Field, Shape, StructType, Type, UserType};

// ============================================================================
// Traits
// ============================================================================

/// A store of shapes that can be looked up by handle.
///
/// For real shapes, this is a ZST (shapes live in static memory).
/// For DynShape, this holds an array of shape definitions.
pub trait IShapeStore: Clone {
    /// The handle type used to reference shapes in this store.
    type Handle: Copy;

    /// Look up a shape by handle.
    fn get(&self, handle: Self::Handle) -> DynShapeView<'_, Self>;
}

/// A view into a shape, borrowing from a store.
///
/// This is the common currency for working with shapes regardless of
/// whether they come from static memory or a DynShapeStore.
#[derive(Clone, Copy)]
pub struct DynShapeView<'a, Store: IShapeStore + ?Sized> {
    pub store: &'a Store,
    pub handle: Store::Handle,
}

/// Common interface for shapes.
///
/// Implemented by:
/// - `&'static facet_core::Shape` (real shapes)
/// - `DynShapeView` (synthetic shapes for Kani)
pub trait IShape: Copy {
    /// The struct type returned by `as_struct()`.
    type StructType: IStructType<Field = Self::Field>;

    /// The field type used by struct types.
    type Field: IField<Shape = Self>;

    /// Get the layout (size and alignment) of this shape.
    fn layout(&self) -> Layout;

    /// Check if this is a struct type.
    fn is_struct(&self) -> bool;

    /// Get struct-specific information, if this is a struct.
    fn as_struct(&self) -> Option<Self::StructType>;
}

/// Interface for struct type information.
pub trait IStructType: Copy {
    /// The field type.
    type Field: IField;

    /// Number of fields in this struct.
    fn field_count(&self) -> usize;

    /// Get field by index.
    fn field(&self, idx: usize) -> Option<Self::Field>;
}

/// Interface for field information.
pub trait IField: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Byte offset of this field within the struct.
    fn offset(&self) -> usize;

    /// Shape of this field's type.
    fn shape(&self) -> Self::Shape;
}

// ============================================================================
// DynShape - synthetic shapes for Kani
// ============================================================================

/// Maximum number of fields in a struct (for bounded verification).
pub const MAX_FIELDS: usize = 8;

/// Maximum number of shapes in a store (for bounded verification).
pub const MAX_SHAPES: usize = 8;

/// A handle to a shape in a DynShapeStore.
///
/// This is just an index into the store's shape array.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynShapeHandle(pub u8);

/// A synthetic field for Kani verification.
///
/// Fields reference their type's shape by handle (index) rather than
/// containing the shape directly. This avoids recursive type definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynFieldDef {
    /// Byte offset of this field within the struct.
    pub offset: usize,
    /// Handle to the shape of this field's type.
    pub shape_handle: DynShapeHandle,
}

/// A synthetic struct type for Kani verification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynStructDef {
    /// Number of fields.
    pub field_count: u8,
    /// Field information (only first `field_count` entries are valid).
    pub fields: [DynFieldDef; MAX_FIELDS],
}

/// A bounded shape definition for Kani verification.
///
/// Unlike `facet_core::Shape` which uses static references and can be recursive,
/// these shapes are bounded and can implement `kani::Arbitrary`.
///
/// Shape definitions live in a `DynShapeStore` and are referenced by handle.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynShapeDef {
    /// Layout of this type.
    pub layout: Layout,
    /// Type-specific information.
    pub def: DynDef,
}

/// Type-specific definition for DynShape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DynDef {
    /// A scalar type (no internal structure to track).
    Scalar,
    /// A struct with indexed fields.
    Struct(DynStructDef),
    // TODO: Enum, Option, Result, List, Map, etc.
}

/// A store of DynShape definitions.
///
/// Shapes are stored in an array and referenced by index (DynShapeHandle).
/// This allows shapes to reference other shapes (nested structs) without
/// creating recursive type definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DynShapeStore {
    /// Number of shapes in the store.
    pub shape_count: u8,
    /// Shape definitions (only first `shape_count` entries are valid).
    pub shapes: [DynShapeDef; MAX_SHAPES],
}

impl DynShapeStore {
    /// Create a new empty store.
    pub const fn new() -> Self {
        Self {
            shape_count: 0,
            shapes: [DynShapeDef {
                layout: Layout::new::<()>(),
                def: DynDef::Scalar,
            }; MAX_SHAPES],
        }
    }

    /// Add a shape to the store and return its handle.
    pub fn add(&mut self, shape: DynShapeDef) -> DynShapeHandle {
        assert!((self.shape_count as usize) < MAX_SHAPES, "shape store full");
        let handle = DynShapeHandle(self.shape_count);
        self.shapes[self.shape_count as usize] = shape;
        self.shape_count += 1;
        handle
    }

    /// Get a shape definition by handle.
    pub fn get_def(&self, handle: DynShapeHandle) -> &DynShapeDef {
        assert!(
            (handle.0 as usize) < self.shape_count as usize,
            "invalid handle"
        );
        &self.shapes[handle.0 as usize]
    }

    /// Get a view into a shape.
    pub fn view(&self, handle: DynShapeHandle) -> DynShapeView<'_, Self> {
        DynShapeView {
            store: self,
            handle,
        }
    }
}

impl Default for DynShapeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl IShapeStore for DynShapeStore {
    type Handle = DynShapeHandle;

    fn get(&self, handle: Self::Handle) -> DynShapeView<'_, Self> {
        self.view(handle)
    }
}

/// A field view that borrows from a store.
#[derive(Clone, Copy)]
pub struct DynFieldView<'a> {
    pub store: &'a DynShapeStore,
    pub def: &'a DynFieldDef,
}

/// A struct type view that borrows from a store.
#[derive(Clone, Copy)]
pub struct DynStructView<'a> {
    pub store: &'a DynShapeStore,
    pub def: &'a DynStructDef,
}

// ============================================================================
// IShape implementation for DynShapeView
// ============================================================================

impl<'a> IShape for DynShapeView<'a, DynShapeStore> {
    type StructType = DynStructView<'a>;
    type Field = DynFieldView<'a>;

    #[inline]
    fn layout(&self) -> Layout {
        self.store.get_def(self.handle).layout
    }

    #[inline]
    fn is_struct(&self) -> bool {
        matches!(self.store.get_def(self.handle).def, DynDef::Struct(_))
    }

    #[inline]
    fn as_struct(&self) -> Option<Self::StructType> {
        match &self.store.get_def(self.handle).def {
            DynDef::Struct(s) => Some(DynStructView {
                store: self.store,
                def: s,
            }),
            _ => None,
        }
    }
}

impl<'a> IStructType for DynStructView<'a> {
    type Field = DynFieldView<'a>;

    #[inline]
    fn field_count(&self) -> usize {
        self.def.field_count as usize
    }

    #[inline]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        if idx < self.def.field_count as usize {
            Some(DynFieldView {
                store: self.store,
                def: &self.def.fields[idx],
            })
        } else {
            None
        }
    }
}

impl<'a> IField for DynFieldView<'a> {
    type Shape = DynShapeView<'a, DynShapeStore>;

    #[inline]
    fn offset(&self) -> usize {
        self.def.offset
    }

    #[inline]
    fn shape(&self) -> Self::Shape {
        // Look up the field's shape in the store by handle
        self.store.view(self.def.shape_handle)
    }
}

// ============================================================================
// Constructors
// ============================================================================

impl DynShapeDef {
    /// Create a scalar shape with the given layout.
    pub const fn scalar(layout: Layout) -> Self {
        Self {
            layout,
            def: DynDef::Scalar,
        }
    }

    /// Create a struct shape with the given fields.
    ///
    /// The `field_shapes` parameter provides the shape handle for each field.
    /// Use `store.get_def(handle).layout` to get layouts for size calculation.
    pub fn struct_with_fields(store: &DynShapeStore, fields: &[(usize, DynShapeHandle)]) -> Self {
        assert!(fields.len() <= MAX_FIELDS, "too many fields");

        // Calculate overall layout from fields
        let mut size = 0usize;
        let mut align = 1usize;

        for &(offset, shape_handle) in fields {
            let field_layout = store.get_def(shape_handle).layout;
            align = align.max(field_layout.align());
            let field_end = offset + field_layout.size();
            size = size.max(field_end);
        }

        // Round up size to alignment
        size = (size + align - 1) & !(align - 1);

        let layout = Layout::from_size_align(size, align).expect("valid layout");

        let mut field_array = [DynFieldDef {
            offset: 0,
            shape_handle: DynShapeHandle(0),
        }; MAX_FIELDS];
        for (i, &(offset, shape_handle)) in fields.iter().enumerate() {
            field_array[i] = DynFieldDef {
                offset,
                shape_handle,
            };
        }

        Self {
            layout,
            def: DynDef::Struct(DynStructDef {
                field_count: fields.len() as u8,
                fields: field_array,
            }),
        }
    }
}

impl DynFieldDef {
    /// Create a new field definition.
    pub const fn new(offset: usize, shape_handle: DynShapeHandle) -> Self {
        Self {
            offset,
            shape_handle,
        }
    }
}

// ============================================================================
// Arbitrary for Kani
// ============================================================================

#[cfg(kani)]
impl kani::Arbitrary for DynShapeDef {
    fn any() -> Self {
        let is_struct: bool = kani::any();

        if is_struct {
            // Struct with 1-4 fields
            let field_count: u8 = kani::any();
            kani::assume(field_count > 0 && field_count <= 4);

            let mut fields = [DynFieldDef {
                offset: 0,
                shape_handle: DynShapeHandle(0),
            }; MAX_FIELDS];

            let mut offset = 0usize;
            for i in 0..(field_count as usize) {
                let field_size: usize = kani::any();
                kani::assume(field_size > 0 && field_size <= 8);

                // For arbitrary shapes, fields point to shape 0 (a placeholder)
                // In real use, the store would be populated first
                fields[i] = DynFieldDef::new(offset, DynShapeHandle(0));
                offset += field_size;
            }

            kani::assume(offset <= 64);

            let layout = Layout::from_size_align(offset, 1).unwrap();

            DynShapeDef {
                layout,
                def: DynDef::Struct(DynStructDef {
                    field_count,
                    fields,
                }),
            }
        } else {
            // Scalar
            let size: usize = kani::any();
            let align_pow: u8 = kani::any();
            kani::assume(size <= 64);
            kani::assume(align_pow <= 3);
            let align = 1usize << align_pow;
            kani::assume(size == 0 || size % align == 0);

            let layout = Layout::from_size_align(size, align).unwrap();
            DynShapeDef::scalar(layout)
        }
    }
}

/// Generate an arbitrary shape store with nested struct support.
#[cfg(kani)]
impl kani::Arbitrary for DynShapeStore {
    fn any() -> Self {
        let mut store = DynShapeStore::new();

        // First, add some scalar shapes that can be used by struct fields
        let num_scalars: u8 = kani::any();
        kani::assume(num_scalars >= 1 && num_scalars <= 4);

        for _ in 0..num_scalars {
            let size: usize = kani::any();
            kani::assume(size > 0 && size <= 8);
            let layout = Layout::from_size_align(size, 1).unwrap();
            store.add(DynShapeDef::scalar(layout));
        }

        // Then optionally add a struct that uses those scalars as fields
        let add_struct: bool = kani::any();
        if add_struct {
            let field_count: u8 = kani::any();
            kani::assume(field_count > 0 && field_count <= 4);

            let mut fields = [DynFieldDef {
                offset: 0,
                shape_handle: DynShapeHandle(0),
            }; MAX_FIELDS];

            let mut offset = 0usize;
            for i in 0..(field_count as usize) {
                // Pick a random scalar shape for this field
                let shape_idx: u8 = kani::any();
                kani::assume(shape_idx < num_scalars);

                let field_layout = store.get_def(DynShapeHandle(shape_idx)).layout;
                fields[i] = DynFieldDef::new(offset, DynShapeHandle(shape_idx));
                offset += field_layout.size();
            }

            kani::assume(offset <= 64);

            let layout = Layout::from_size_align(offset, 1).unwrap();
            store.add(DynShapeDef {
                layout,
                def: DynDef::Struct(DynStructDef {
                    field_count,
                    fields,
                }),
            });
        }

        store
    }
}

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

    fn get(&self, handle: Self::Handle) -> DynShapeView<'_, Self> {
        DynShapeView {
            store: self,
            handle,
        }
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
