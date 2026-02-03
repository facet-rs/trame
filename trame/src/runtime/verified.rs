//! Verified implementations of all of trame's runtime traits: storage is heavily bounded, all
//! operations are checked, we track a lot of things.

use std::alloc::Layout;

use crate::runtime::{DynShapeView, IField, IShape, IShapeStore, IStructType};

// ==================================================================
// Shape
// ==================================================================

/// Maximum number of shapes in a store (for bounded verification).
pub const MAX_SHAPES_PER_STORE: usize = 8;

/// Maximum number of fields in a struct (for bounded verification).
pub const MAX_FIELDS_PER_STRUCT: usize = 8;

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
    pub fields: [DynFieldDef; MAX_FIELDS_PER_STRUCT],
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
    ///
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
    pub shapes: [DynShapeDef; MAX_SHAPES_PER_STORE],
}

impl DynShapeStore {
    /// Create a new empty store.
    pub const fn new() -> Self {
        Self {
            shape_count: 0,
            shapes: [DynShapeDef {
                layout: Layout::new::<()>(),
                def: DynDef::Scalar,
            }; MAX_SHAPES_PER_STORE],
        }
    }

    /// Add a shape to the store and return its handle.
    pub fn add(&mut self, shape: DynShapeDef) -> DynShapeHandle {
        assert!(
            (self.shape_count as usize) < MAX_SHAPES_PER_STORE,
            "shape store full"
        );
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

impl<'a> PartialEq for DynShapeView<'a, DynShapeStore> {
    fn eq(&self, other: &Self) -> bool {
        // Two views are equal if they point to the same store and handle.
        // We compare store pointers and handle values.
        core::ptr::eq(self.store, other.store) && self.handle == other.handle
    }
}

impl<'a> Eq for DynShapeView<'a, DynShapeStore> {}

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

    #[inline]
    unsafe fn drop_in_place(&self, _ptr: *mut u8) {
        // No-op for DynShapeView - we only track state, not actual values.
        // In Kani verification, we don't have real values to drop.
    }

    #[inline]
    unsafe fn default_in_place(&self, _ptr: *mut u8) -> bool {
        // For verification, treat default as always available.
        true
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
        assert!(fields.len() <= MAX_FIELDS_PER_STRUCT, "too many fields");

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
        }; MAX_FIELDS_PER_STRUCT];
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
            }; MAX_FIELDS_PER_STRUCT];

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
            }; MAX_FIELDS_PER_STRUCT];

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

// ==================================================================
// Heap
// ==================================================================

// ==================================================================
// Arena
// ==================================================================
