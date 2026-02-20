//! Verified implementations of all of trame's runtime traits: storage is heavily bounded, all
//! operations are checked, we track a lot of things.

#[cfg(not(creusot))]
use core::cell::UnsafeCell;
use std::alloc::Layout;

#[cfg(creusot)]
use creusot_std::model::DeepModel;

mod byte_range;
use byte_range::{ByteRangeError, ByteRangeTracker, Range};

use crate::{
    CopyDesc, EnumDiscriminantRepr, EnumReprKind, IArena, IEnumType, IExecShape, IField, IHeap,
    IPointerType, IPtr, IRuntime, IShape, IShapeStore, IStructType, IVariantType, Idx,
};

/// A runtime that verifies all operations
pub struct VRuntime;

#[cfg(not(creusot))]
struct VShapeStoreCell(UnsafeCell<VShapeStore>);

// SAFETY: Verified runtime is single-threaded. Concurrent access is undefined behavior.
#[cfg(not(creusot))]
unsafe impl Sync for VShapeStoreCell {}

#[cfg(not(creusot))]
static VSHAPE_STORE: VShapeStoreCell = VShapeStoreCell(UnsafeCell::new(VShapeStore::new()));

#[cfg(not(creusot))]
pub fn vshape_store() -> &'static VShapeStore {
    unsafe { &*VSHAPE_STORE.0.get() }
}

/// Register a new verified shape in the global store.
#[cfg(not(creusot))]
pub fn vshape_register(shape: VShapeDef) -> VShapeHandle {
    unsafe { (&mut *VSHAPE_STORE.0.get()).add(shape) }
}

/// View a previously registered shape from the global store.
#[cfg(not(creusot))]
pub fn vshape_view(handle: VShapeHandle) -> VShapeView<'static, VShapeStore> {
    unsafe { (&*VSHAPE_STORE.0.get()).view(handle) }
}

/// Reset the global shape store. For testing only.
///
/// # Safety
/// Caller must ensure no VShapeView or VShapeHandle from this store is in use.
#[cfg(not(creusot))]
pub unsafe fn vshape_store_reset() {
    unsafe { *VSHAPE_STORE.0.get() = VShapeStore::new() }
}

#[cfg(creusot)]
pub fn vshape_store() -> &'static VShapeStore {
    panic!("vshape_store is unavailable under creusot")
}

#[cfg(creusot)]
pub fn vshape_register(_shape: VShapeDef) -> VShapeHandle {
    panic!("vshape_register is unavailable under creusot")
}

#[cfg(creusot)]
pub fn vshape_view(_handle: VShapeHandle) -> VShapeView<'static, VShapeStore> {
    panic!("vshape_view is unavailable under creusot")
}

#[cfg(creusot)]
pub unsafe fn vshape_store_reset() {
    // no-op under creusot
}

impl IRuntime for VRuntime {
    type Shape = VShapeView<'static, VShapeStore>;
    type Heap = VHeap<Self::Shape>;
    type Arena<T> = VArena<T, MAX_VARENA_SLOTS>;

    fn heap() -> Self::Heap {
        VHeap::new()
    }

    fn arena<T>() -> Self::Arena<T> {
        VArena::new()
    }
}

// ==================================================================
// Shape
// ==================================================================

/// Maximum number of shapes in a store (for bounded verification).
pub const MAX_SHAPES_PER_STORE: usize = 8;

/// Maximum number of fields in a struct (for bounded verification).
pub const MAX_FIELDS_PER_STRUCT: usize = 8;

/// Maximum number of variants in an enum (for bounded verification).
pub const MAX_VARIANTS_PER_ENUM: usize = 8;

/// Drop hook used by executable shapes.
pub type VDropInPlaceFn = unsafe fn(*mut u8);

/// Default hook used by executable shapes.
pub type VDefaultInPlaceFn = unsafe fn(*mut u8) -> bool;

/// Pointer-construction hook used by executable pointer shapes.
pub type VPointerNewIntoFn = unsafe fn(dst: *mut u8, src: *mut u8);

unsafe fn vdrop_noop(_ptr: *mut u8) {}

/// Executable per-shape operations used by `LHeap + VShapeView`.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy)]
pub struct VTypeOps {
    /// Whether dropping values of this shape can have observable effects.
    pub needs_drop: bool,
    /// Drop operation for this shape.
    pub drop_in_place: VDropInPlaceFn,
    /// Default operation for this shape, if available.
    pub default_in_place: Option<VDefaultInPlaceFn>,
}

impl PartialEq for VTypeOps {
    fn eq(&self, other: &Self) -> bool {
        self.needs_drop == other.needs_drop
            && core::ptr::fn_addr_eq(self.drop_in_place, other.drop_in_place)
            && match (self.default_in_place, other.default_in_place) {
                (Some(a), Some(b)) => core::ptr::fn_addr_eq(a, b),
                (None, None) => true,
                _ => false,
            }
    }
}

impl Eq for VTypeOps {}

impl VTypeOps {
    /// Construct a `VTypeOps` value.
    pub const fn new(
        needs_drop: bool,
        drop_in_place: VDropInPlaceFn,
        default_in_place: Option<VDefaultInPlaceFn>,
    ) -> Self {
        Self {
            needs_drop,
            drop_in_place,
            default_in_place,
        }
    }

    /// Operations for POD-like shapes that don't need drop/default hooks.
    pub const fn pod() -> Self {
        Self::new(false, vdrop_noop, None)
    }
}

/// Pointer-specific executable hooks.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy)]
pub struct VPointerVTable {
    /// Construct pointer value at destination from pointee source.
    pub new_into_fn: Option<VPointerNewIntoFn>,
}

impl PartialEq for VPointerVTable {
    fn eq(&self, other: &Self) -> bool {
        match (self.new_into_fn, other.new_into_fn) {
            (Some(a), Some(b)) => core::ptr::fn_addr_eq(a, b),
            (None, None) => true,
            _ => false,
        }
    }
}

impl Eq for VPointerVTable {}

impl VPointerVTable {
    /// Create pointer hooks with optional construction function.
    pub const fn new(new_into_fn: Option<VPointerNewIntoFn>) -> Self {
        Self { new_into_fn }
    }
}

/// A handle to a shape in a DynShapeStore.
///
/// This is just an index into the store's shape array.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VShapeHandle(pub u8);

/// A synthetic field for Kani verification.
///
/// Fields reference their type's shape by handle (index) rather than
/// containing the shape directly. This avoids recursive type definitions.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VFieldDef {
    /// Byte offset of this field within the struct.
    pub offset: usize,

    /// Handle to the shape of this field's type.
    pub shape_handle: VShapeHandle,
}

/// A synthetic struct type for Kani verification.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VStructDef {
    /// Number of fields.
    pub field_count: u8,

    /// Field information (only first `field_count` entries are valid).
    pub fields: [VFieldDef; MAX_FIELDS_PER_STRUCT],
}

/// A synthetic enum variant for verification.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VVariantDef {
    /// Rust variant name.
    pub name: &'static str,
    /// Effective serialized variant name.
    pub effective_name: &'static str,
    /// In-memory discriminant value for this variant.
    pub discriminant: Option<i64>,
    /// Struct-like payload layout for this variant.
    pub data: VStructDef,
}

/// A synthetic enum type for verification.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VEnumDef {
    /// Number of variants.
    pub variant_count: u8,
    /// Enum representation metadata.
    pub repr_kind: EnumReprKind,
    /// In-memory discriminant representation.
    pub discriminant_repr: EnumDiscriminantRepr,
    /// Variant information (only first `variant_count` entries are valid).
    pub variants: [VVariantDef; MAX_VARIANTS_PER_ENUM],
}

/// A bounded shape definition for Kani verification.
///
/// Unlike `facet_core::Shape` which uses static references and can be recursive,
/// these shapes are bounded and can implement `kani::Arbitrary`.
///
/// Shape definitions live in a `DynShapeStore` and are referenced by handle.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VShapeDef {
    /// Layout of this type.
    pub layout: VLayout,
    /// Executable operations for this shape.
    pub type_ops: VTypeOps,
    ///
    /// Type-specific information.
    pub def: VDef,
}

/// Type-specific definition for verified shapes.
#[cfg_attr(creusot, derive(DeepModel))]
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VDef {
    /// A scalar type (no internal structure to track).
    Scalar,
    /// A struct with indexed fields.
    Struct(VStructDef),
    /// A smart pointer with a known pointee shape.
    Pointer(VPointerDef),
    /// An enum with bounded variants.
    Enum(VEnumDef),
    /// An `Option<T>`-like sum type.
    ///
    /// This is modeled as a scalar for now (no partial field addressing), but
    /// we track the payload shape explicitly so verified shape stores can
    /// represent optional values without erasing type structure.
    Option(VOptionDef),
    // TODO: Result, List, Map, etc.
}

/// A synthetic smart-pointer definition for verification.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VPointerDef {
    /// Handle to pointee shape.
    pub pointee_handle: VShapeHandle,
    /// Whether pointer can be created from pointee.
    pub constructible_from_pointee: bool,
    /// Whether pointer is known to be `Box<T>`.
    pub known_box: bool,
    /// Pointer-construction hooks.
    pub vtable: VPointerVTable,
}

/// A synthetic option-like definition for verification.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VOptionDef {
    /// Handle to the payload shape `T` for `Option<T>`.
    pub some_handle: VShapeHandle,
}

/// A store of DynShape definitions.
///
/// Shapes are stored in an array and referenced by index (DynShapeHandle).
/// This allows shapes to reference other shapes (nested structs) without
/// creating recursive type definitions.
#[cfg_attr(creusot, derive(DeepModel))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VShapeStore {
    /// Number of shapes in the store.
    pub shape_count: u8,

    /// Shape definitions (only first `shape_count` entries are valid).
    pub shapes: [VShapeDef; MAX_SHAPES_PER_STORE],
}

/// A view into a shape, borrowing from a store.
///
/// This is the common currency for working with shapes in the verified runtime.
#[derive(Clone, Copy)]
pub struct VShapeView<'a, Store: IShapeStore> {
    pub store: &'a Store,
    pub handle: Store::Handle,
}

impl VShapeStore {
    /// Create a new empty store.
    pub const fn new() -> Self {
        Self {
            shape_count: 0,
            shapes: [VShapeDef {
                layout: VLayout::new::<()>(),
                type_ops: VTypeOps::pod(),
                def: VDef::Scalar,
            }; MAX_SHAPES_PER_STORE],
        }
    }

    /// Add a shape to the store and return its handle.
    pub fn add(&mut self, shape: VShapeDef) -> VShapeHandle {
        assert!(
            (self.shape_count as usize) < MAX_SHAPES_PER_STORE,
            "shape store full"
        );
        let handle = VShapeHandle(self.shape_count);
        self.shapes[self.shape_count as usize] = shape;
        self.shape_count += 1;
        handle
    }

    /// Get a shape definition by handle.
    pub fn get_def(&self, handle: VShapeHandle) -> &VShapeDef {
        assert!(
            (handle.0 as usize) < self.shape_count as usize,
            "invalid handle"
        );
        &self.shapes[handle.0 as usize]
    }

    /// Get a view into a shape.
    pub fn view(&self, handle: VShapeHandle) -> VShapeView<'_, Self> {
        VShapeView {
            store: self,
            handle,
        }
    }
}

impl Default for VShapeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl IShapeStore for VShapeStore {
    type Handle = VShapeHandle;
    type View<'a>
        = VShapeView<'a, VShapeStore>
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        self.view(handle)
    }
}

impl IShapeStore for &'static VShapeStore {
    type Handle = VShapeHandle;
    type View<'a>
        = VShapeView<'a, VShapeStore>
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        VShapeView {
            store: *self,
            handle,
        }
    }
}

/// A field view that borrows from a store.
#[derive(Clone, Copy)]
pub struct VFieldView<'a> {
    pub store: &'a VShapeStore,
    pub def: &'a VFieldDef,
}

/// A struct type view that borrows from a store.
#[derive(Clone, Copy)]
pub struct VStructView<'a> {
    pub store: &'a VShapeStore,
    pub def: &'a VStructDef,
}

/// A pointer type view that borrows from a store.
#[derive(Clone, Copy)]
pub struct VPointerView<'a> {
    pub store: &'a VShapeStore,
    pub def: &'a VPointerDef,
}

/// Enum type view for verified shapes.
#[derive(Clone, Copy)]
pub struct VEnumView<'a> {
    pub store: &'a VShapeStore,
    pub def: &'a VEnumDef,
}

/// Enum variant view for verified shapes.
#[derive(Clone, Copy)]
pub struct VVariantView<'a> {
    pub store: &'a VShapeStore,
    pub def: &'a VVariantDef,
}

impl<'a> PartialEq for VShapeView<'a, VShapeStore> {
    fn eq(&self, other: &Self) -> bool {
        // Two views are equal if they point to the same store and handle.
        // We compare store pointers and handle values.
        core::ptr::eq(self.store, other.store) && self.handle == other.handle
    }
}

impl<'a> Eq for VShapeView<'a, VShapeStore> {}

impl<'a> IShape for VShapeView<'a, VShapeStore> {
    type StructType = VStructView<'a>;
    type Field = VFieldView<'a>;
    type EnumType = VEnumView<'a>;
    type PointerType = VPointerView<'a>;

    #[inline]
    fn layout(&self) -> Option<Layout> {
        #[cfg(creusot)]
        {
            Some(self.store.get_def(self.handle).layout.to_layout())
        }
        #[cfg(not(creusot))]
        {
            Some(self.store.get_def(self.handle).layout)
        }
    }

    #[inline]
    fn is_struct(&self) -> bool {
        matches!(self.store.get_def(self.handle).def, VDef::Struct(_))
    }

    #[inline]
    fn as_struct(&self) -> Option<Self::StructType> {
        match &self.store.get_def(self.handle).def {
            VDef::Struct(s) => Some(VStructView {
                store: self.store,
                def: s,
            }),
            _ => None,
        }
    }

    #[inline]
    fn as_enum(&self) -> Option<Self::EnumType> {
        match &self.store.get_def(self.handle).def {
            VDef::Enum(e) => Some(VEnumView {
                store: self.store,
                def: e,
            }),
            _ => None,
        }
    }

    #[inline]
    fn enum_repr_kind(&self) -> Option<EnumReprKind> {
        match &self.store.get_def(self.handle).def {
            VDef::Enum(e) => Some(e.repr_kind),
            _ => None,
        }
    }

    #[inline]
    fn enum_discriminant_repr(&self) -> Option<EnumDiscriminantRepr> {
        match &self.store.get_def(self.handle).def {
            VDef::Enum(e) => Some(e.discriminant_repr),
            _ => None,
        }
    }

    #[inline]
    fn enum_variant_discriminant(&self, variant_idx: usize) -> Option<i64> {
        let VDef::Enum(e) = &self.store.get_def(self.handle).def else {
            return None;
        };
        if variant_idx >= e.variant_count as usize {
            return None;
        }
        e.variants[variant_idx].discriminant
    }

    #[inline]
    fn type_identifier(&self) -> &'static str {
        "<verified-shape>"
    }

    #[inline]
    fn option_payload(&self) -> Option<Self> {
        match self.store.get_def(self.handle).def {
            VDef::Option(opt) => Some(self.store.view(opt.some_handle)),
            _ => None,
        }
    }

    #[inline]
    fn is_pointer(&self) -> bool {
        matches!(self.store.get_def(self.handle).def, VDef::Pointer(_))
    }

    #[inline]
    fn as_pointer(&self) -> Option<Self::PointerType> {
        match &self.store.get_def(self.handle).def {
            VDef::Pointer(def) => Some(VPointerView {
                store: self.store,
                def,
            }),
            _ => None,
        }
    }
}

impl<'a> IExecShape<*mut u8> for VShapeView<'a, VShapeStore> {
    #[inline]
    fn needs_drop(&self) -> bool {
        self.store.get_def(self.handle).type_ops.needs_drop
    }

    #[inline]
    unsafe fn drop_in_place(&self, ptr: *mut u8) {
        let ops = self.store.get_def(self.handle).type_ops;
        unsafe {
            (ops.drop_in_place)(ptr);
        }
    }

    #[inline]
    unsafe fn default_in_place(&self, ptr: *mut u8) -> bool {
        let ops = self.store.get_def(self.handle).type_ops;
        let Some(default_in_place) = ops.default_in_place else {
            return false;
        };
        unsafe { default_in_place(ptr) }
    }

    #[inline]
    unsafe fn pointer_from_pointee(&self, dst: *mut u8, src: *mut u8, pointee_shape: Self) -> bool {
        if !core::ptr::eq(self.store, pointee_shape.store) {
            return false;
        }
        let def = self.store.get_def(self.handle);
        let VDef::Pointer(pointer_def) = def.def else {
            return false;
        };
        if !pointer_def.constructible_from_pointee {
            return false;
        }
        if pointer_def.pointee_handle != pointee_shape.handle {
            return false;
        }
        let Some(new_into_fn) = pointer_def.vtable.new_into_fn else {
            return false;
        };
        unsafe {
            new_into_fn(dst, src);
        }
        true
    }
}

impl<'a> IPointerType for VPointerView<'a> {
    type Shape = VShapeView<'a, VShapeStore>;

    #[inline]
    fn pointee(&self) -> Option<Self::Shape> {
        Some(self.store.view(self.def.pointee_handle))
    }

    #[inline]
    fn constructible_from_pointee(&self) -> bool {
        self.def.constructible_from_pointee
    }

    #[inline]
    fn is_known_box(&self) -> bool {
        self.def.known_box
    }
}

impl<'a> IStructType for VStructView<'a> {
    type Field = VFieldView<'a>;

    #[inline]
    fn field_count(&self) -> usize {
        self.def.field_count as usize
    }

    #[inline]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        if idx < self.def.field_count as usize {
            Some(VFieldView {
                store: self.store,
                def: &self.def.fields[idx],
            })
        } else {
            None
        }
    }
}

impl<'a> IEnumType for VEnumView<'a> {
    type StructType = VStructView<'a>;
    type Variant = VVariantView<'a>;

    #[inline]
    fn variant_count(&self) -> usize {
        self.def.variant_count as usize
    }

    #[inline]
    fn variant(&self, idx: usize) -> Option<Self::Variant> {
        if idx < self.def.variant_count as usize {
            Some(VVariantView {
                store: self.store,
                def: &self.def.variants[idx],
            })
        } else {
            None
        }
    }
}

impl<'a> IVariantType for VVariantView<'a> {
    type StructType = VStructView<'a>;

    #[inline]
    fn name(&self) -> &'static str {
        self.def.name
    }

    #[inline]
    fn effective_name(&self) -> &'static str {
        self.def.effective_name
    }

    #[inline]
    fn data(&self) -> Self::StructType {
        VStructView {
            store: self.store,
            def: &self.def.data,
        }
    }
}

impl<'a> IField for VFieldView<'a> {
    type Shape = VShapeView<'a, VShapeStore>;

    #[inline]
    fn offset(&self) -> usize {
        self.def.offset
    }

    #[inline]
    fn shape(&self) -> Self::Shape {
        // Look up the field's shape in the store by handle
        self.store.view(self.def.shape_handle)
    }

    #[inline]
    fn name(&self) -> &'static str {
        ""
    }

    #[inline]
    fn effective_name(&self) -> &'static str {
        ""
    }
}

// ============================================================================
// Constructors
// ============================================================================

impl VShapeDef {
    /// Create a scalar shape with the given layout.
    pub const fn scalar(layout: VLayout) -> Self {
        Self::scalar_with_ops(layout, VTypeOps::pod())
    }

    /// Create a scalar shape with explicit executable operations.
    pub const fn scalar_with_ops(layout: VLayout, type_ops: VTypeOps) -> Self {
        Self {
            layout,
            type_ops,
            def: VDef::Scalar,
        }
    }

    /// Create a struct shape with the given fields.
    ///
    /// The `field_shapes` parameter provides the shape handle for each field.
    /// Use `store.get_def(handle).layout` to get layouts for size calculation.
    pub fn struct_with_fields(store: &VShapeStore, fields: &[(usize, VShapeHandle)]) -> Self {
        Self::struct_with_fields_and_ops(store, fields, VTypeOps::pod())
    }

    /// Create a struct shape with explicit executable operations.
    pub fn struct_with_fields_and_ops(
        store: &VShapeStore,
        fields: &[(usize, VShapeHandle)],
        type_ops: VTypeOps,
    ) -> Self {
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

        let layout = VLayout::from_size_align(size, align).expect("valid layout");

        let mut field_array = [VFieldDef {
            offset: 0,
            shape_handle: VShapeHandle(0),
        }; MAX_FIELDS_PER_STRUCT];
        for (i, &(offset, shape_handle)) in fields.iter().enumerate() {
            field_array[i] = VFieldDef {
                offset,
                shape_handle,
            };
        }

        Self {
            layout,
            type_ops,
            def: VDef::Struct(VStructDef {
                field_count: fields.len() as u8,
                fields: field_array,
            }),
        }
    }

    /// Create a smart-pointer shape with a pointee.
    pub fn pointer_to(pointee: VShapeHandle, constructible: bool, known_box: bool) -> Self {
        Self::pointer_to_with_ops(
            pointee,
            constructible,
            known_box,
            VTypeOps::pod(),
            VPointerVTable::new(None),
        )
    }

    /// Create a smart-pointer shape with explicit executable operations and hooks.
    pub fn pointer_to_with_ops(
        pointee: VShapeHandle,
        constructible: bool,
        known_box: bool,
        type_ops: VTypeOps,
        vtable: VPointerVTable,
    ) -> Self {
        Self {
            layout: VLayout::new::<usize>(),
            type_ops,
            def: VDef::Pointer(VPointerDef {
                pointee_handle: pointee,
                constructible_from_pointee: constructible,
                known_box,
                vtable,
            }),
        }
    }

    /// Create an option-like shape with payload `T`.
    pub fn option_of(payload: VShapeHandle, layout: VLayout, type_ops: VTypeOps) -> Self {
        Self {
            layout,
            type_ops,
            def: VDef::Option(VOptionDef {
                some_handle: payload,
            }),
        }
    }

    /// Create an enum shape with the given variants.
    pub fn enum_with_variants(
        layout: VLayout,
        repr_kind: EnumReprKind,
        variants: &[VVariantDef],
    ) -> Self {
        Self::enum_with_variants_and_repr(
            layout,
            repr_kind,
            EnumDiscriminantRepr::U8,
            variants,
            VTypeOps::pod(),
        )
    }

    /// Create an enum shape with explicit executable operations.
    pub fn enum_with_variants_and_ops(
        layout: VLayout,
        repr_kind: EnumReprKind,
        variants: &[VVariantDef],
        type_ops: VTypeOps,
    ) -> Self {
        Self::enum_with_variants_and_repr(
            layout,
            repr_kind,
            EnumDiscriminantRepr::U8,
            variants,
            type_ops,
        )
    }

    /// Create an enum shape with explicit discriminant representation.
    pub fn enum_with_variants_and_repr(
        layout: VLayout,
        repr_kind: EnumReprKind,
        discriminant_repr: EnumDiscriminantRepr,
        variants: &[VVariantDef],
        type_ops: VTypeOps,
    ) -> Self {
        assert!(variants.len() <= MAX_VARIANTS_PER_ENUM, "too many variants");

        let mut variant_array = [VVariantDef::unit(""); MAX_VARIANTS_PER_ENUM];
        for (idx, variant) in variants.iter().copied().enumerate() {
            variant_array[idx] = VVariantDef {
                discriminant: variant.discriminant.or(Some(idx as i64)),
                ..variant
            };
        }

        Self {
            layout,
            type_ops,
            def: VDef::Enum(VEnumDef {
                variant_count: variants.len() as u8,
                repr_kind,
                discriminant_repr,
                variants: variant_array,
            }),
        }
    }
}

impl VFieldDef {
    /// Create a new field definition.
    pub const fn new(offset: usize, shape_handle: VShapeHandle) -> Self {
        Self {
            offset,
            shape_handle,
        }
    }
}

impl VStructDef {
    /// Create an empty struct payload definition.
    pub const fn empty() -> Self {
        Self {
            field_count: 0,
            fields: [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT],
        }
    }
}

impl VVariantDef {
    /// Create a variant definition with explicit payload fields.
    pub const fn new(
        name: &'static str,
        effective_name: &'static str,
        discriminant: Option<i64>,
        data: VStructDef,
    ) -> Self {
        Self {
            name,
            effective_name,
            discriminant,
            data,
        }
    }

    /// Create a unit variant definition.
    pub const fn unit(name: &'static str) -> Self {
        Self::new(name, name, None, VStructDef::empty())
    }
}

// ============================================================================
// Arbitrary for Kani
// ============================================================================

#[cfg(kani)]
impl kani::Arbitrary for VShapeDef {
    fn any() -> Self {
        let is_struct: bool = kani::any();

        if is_struct {
            // Struct with 1-4 fields
            let field_count: u8 = kani::any();
            kani::assume(field_count > 0 && field_count <= 4);

            let mut fields = [VFieldDef {
                offset: 0,
                shape_handle: VShapeHandle(0),
            }; MAX_FIELDS_PER_STRUCT];

            let mut offset = 0usize;
            for i in 0..(field_count as usize) {
                let field_size: usize = kani::any();
                kani::assume(field_size > 0 && field_size <= 8);

                // For arbitrary shapes, fields point to shape 0 (a placeholder)
                // In real use, the store would be populated first
                fields[i] = VFieldDef::new(offset, VShapeHandle(0));
                offset += field_size;
            }

            kani::assume(offset <= 64);

            let layout = VLayout::from_size_align(offset, 1).unwrap();

            VShapeDef {
                layout,
                type_ops: VTypeOps::pod(),
                def: VDef::Struct(VStructDef {
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

            let layout = VLayout::from_size_align(size, align).unwrap();
            VShapeDef::scalar(layout)
        }
    }
}

/// Generate an arbitrary shape store with nested struct support.
#[cfg(kani)]
impl kani::Arbitrary for VShapeStore {
    fn any() -> Self {
        let mut store = VShapeStore::new();

        // First, add some scalar shapes that can be used by struct fields
        let num_scalars: u8 = kani::any();
        kani::assume(num_scalars >= 1 && num_scalars <= 4);

        for _ in 0..num_scalars {
            let size: usize = kani::any();
            kani::assume(size > 0 && size <= 8);
            let layout = VLayout::from_size_align(size, 1).unwrap();
            store.add(VShapeDef::scalar(layout));
        }

        // Then optionally add a struct that uses those scalars as fields
        let add_struct: bool = kani::any();
        if add_struct {
            let field_count: u8 = kani::any();
            kani::assume(field_count > 0 && field_count <= 4);

            let mut fields = [VFieldDef {
                offset: 0,
                shape_handle: VShapeHandle(0),
            }; MAX_FIELDS_PER_STRUCT];

            let mut offset = 0usize;
            for i in 0..(field_count as usize) {
                // Pick a random scalar shape for this field
                let shape_idx: u8 = kani::any();
                kani::assume(shape_idx < num_scalars);

                let field_layout = store.get_def(VShapeHandle(shape_idx)).layout;
                fields[i] = VFieldDef::new(offset, VShapeHandle(shape_idx));
                offset += field_layout.size();
            }

            kani::assume(offset <= 64);

            let layout = VLayout::from_size_align(offset, 1).unwrap();
            store.add(VShapeDef {
                layout,
                type_ops: VTypeOps::pod(),
                def: VDef::Struct(VStructDef {
                    field_count,
                    fields,
                }),
            });
        }

        store
    }
}

// ==================================================================
// Pointer (Verified)
// ==================================================================

/// Fat pointer for verified heap operations.
///
/// Contains allocation metadata for bounds checking without borrowing the heap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VPtr {
    /// Which allocation this points into.
    pub alloc_id: u8,
    /// Current byte offset within the allocation.
    pub offset: u32,
    /// Total size of the allocation (for bounds checking).
    pub size: u32,
}

impl IPtr for VPtr {
    #[inline]
    unsafe fn byte_add(self, n: usize) -> Self {
        VPtr::offset(self, n)
    }
}

impl VPtr {
    /// Create a new pointer to the start of an allocation.
    #[inline]
    pub const fn new(alloc_id: u8, size: u32) -> Self {
        Self {
            alloc_id,
            offset: 0,
            size,
        }
    }

    /// Compute a new pointer at an offset from this one.
    ///
    /// # Panics
    /// Panics if the resulting offset would exceed the allocation size.
    #[inline]
    pub fn offset(self, n: usize) -> Self {
        let new_offset = self.offset.checked_add(n as u32).expect("offset overflow");
        #[cfg(not(creusot))]
        assert!(
            new_offset <= self.size,
            "pointer arithmetic out of bounds: offset {} + {} = {} > size {}",
            self.offset,
            n,
            new_offset,
            self.size
        );
        #[cfg(creusot)]
        {
            if new_offset > self.size {
                panic!("pointer arithmetic out of bounds");
            }
        }
        Self {
            alloc_id: self.alloc_id,
            offset: new_offset,
            size: self.size,
        }
    }

    /// Returns the current offset within the allocation.
    #[inline]
    pub const fn offset_bytes(self) -> usize {
        self.offset as usize
    }

    /// Returns the allocation ID.
    #[inline]
    pub const fn alloc_id(self) -> u8 {
        self.alloc_id
    }

    /// Returns the total size of the allocation.
    #[inline]
    pub const fn alloc_size(self) -> usize {
        self.size as usize
    }

    /// Check if this pointer is at the start of its allocation.
    #[inline]
    pub const fn is_at_start(self) -> bool {
        self.offset == 0
    }

    /// Returns the number of bytes remaining from this offset to end of allocation.
    #[inline]
    pub const fn remaining(self) -> usize {
        (self.size - self.offset) as usize
    }
}

// ==================================================================
// Heap
// ==================================================================

/// Maximum number of allocations tracked by VHeap.
pub const MAX_VHEAP_ALLOCS: usize = 8;

/// Error from heap operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VHeapError {
    /// Tried to access a freed allocation.
    AllocationFreed,
    /// Shape mismatch on dealloc or drop.
    ShapeMismatch,
    /// Pointer not at allocation start.
    NotAtStart,
    /// Allocation still has initialized bytes.
    NotFullyUninitialized,
    /// Byte range error.
    ByteRange(ByteRangeError),
    /// Too many allocations.
    TooManyAllocations,
}

impl From<ByteRangeError> for VHeapError {
    fn from(e: ByteRangeError) -> Self {
        VHeapError::ByteRange(e)
    }
}

// ============================================================================
// Operation history tracking (for debugging, not for Kani)
// ============================================================================

/// An operation that was performed on an allocation.
#[cfg(all(not(kani), not(creusot)))]
#[derive(Clone)]
pub struct HeapOp {
    /// What kind of operation
    pub kind: HeapOpKind,
    /// Byte range affected (if applicable)
    pub range: Option<Range>,
    /// Backtrace at the time of the operation (stored as string since Backtrace isn't Clone)
    pub backtrace: String,
}

#[cfg(all(not(kani), not(creusot)))]
impl std::fmt::Debug for HeapOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)?;
        if let Some(range) = self.range {
            write!(f, " [{}..{}]", range.start, range.end)?;
        }
        write!(f, "\n{}", self.backtrace)
    }
}

/// The kind of heap operation.
#[derive(Debug, Clone, Copy)]
pub enum HeapOpKind {
    Alloc,
    MarkInit,
    MarkUninit,
    DropInPlace,
    Dealloc,
    Memcpy,
    DefaultInPlace,
}

/// History of operations for one allocation.
#[cfg(all(not(kani), not(creusot)))]
#[derive(Clone, Default, Debug)]
pub struct AllocHistory {
    pub ops: Vec<HeapOp>,
}

#[cfg(all(not(kani), not(creusot)))]
impl AllocHistory {
    const fn new() -> Self {
        Self { ops: Vec::new() }
    }

    fn record(&mut self, kind: HeapOpKind, range: Option<Range>) {
        let bt = std::backtrace::Backtrace::capture();
        self.ops.push(HeapOp {
            kind,
            range,
            backtrace: bt.to_string(),
        });
    }

    fn print(&self, alloc_id: u8) {
        eprintln!("=== History for allocation {} ===", alloc_id);
        for (i, op) in self.ops.iter().enumerate() {
            eprintln!("--- Op {} ---", i);
            eprintln!("{:?}", op);
        }
        eprintln!("=== End history ===");
    }
}

/// A range in the allocation with its init status and shape info.
#[cfg(all(not(kani), not(creusot)))]
struct LayoutRange {
    start: u32,
    end: u32,
    depth: usize,
    shape_kind: &'static str,
    is_init: bool,
}

/// Print the allocation layout like a "layer cake" or flame graph.
#[cfg(all(not(kani), not(creusot)))]
fn print_allocation_layout<S: IShape>(tracker: &ByteRangeTracker, shape: S, alloc_size: u32) {
    let mut ranges: Vec<LayoutRange> = Vec::new();

    // Recursively collect all ranges from the shape hierarchy
    fn collect_ranges<S: IShape>(
        ranges: &mut Vec<LayoutRange>,
        tracker: &ByteRangeTracker,
        shape: S,
        base_offset: u32,
        depth: usize,
    ) {
        let layout = shape.layout().expect("IShape requires sized types");
        let size = layout.size() as u32;
        if size == 0 {
            return;
        }

        let start = base_offset;
        let end = base_offset + size;
        let is_init = tracker.is_init(start, end);

        let shape_kind = if shape.is_struct() {
            "struct"
        } else {
            "scalar"
        };

        ranges.push(LayoutRange {
            start,
            end,
            depth,
            shape_kind,
            is_init,
        });

        // If struct, recurse into fields
        if let Some(st) = shape.as_struct() {
            for i in 0..st.field_count() {
                if let Some(field) = st.field(i) {
                    let field_shape = field.shape();
                    let field_offset = base_offset + field.offset() as u32;
                    collect_ranges(ranges, tracker, field_shape, field_offset, depth + 1);
                }
            }
        }
    }

    collect_ranges(&mut ranges, tracker, shape, 0, 0);

    // Sort by depth (deepest first for printing), then by start
    ranges.sort_by(|a, b| b.depth.cmp(&a.depth).then(a.start.cmp(&b.start)));

    eprintln!("=== Allocation Layout (size={}) ===", alloc_size);
    eprintln!("Legend: [####] = initialized, [....] = uninitialized");
    eprintln!();

    // Find max depth for indentation
    let _max_depth = ranges.iter().map(|r| r.depth).max().unwrap_or(0);

    // Print from shallowest to deepest (reverse the sort for display)
    ranges.sort_by(|a, b| a.depth.cmp(&b.depth).then(a.start.cmp(&b.start)));

    for range in &ranges {
        let indent = "  ".repeat(range.depth);
        let width = (range.end - range.start) as usize;
        let bar = if range.is_init {
            "#".repeat(width.min(40))
        } else {
            ".".repeat(width.min(40))
        };
        let status = if range.is_init { "INIT" } else { "UNINIT" };
        eprintln!(
            "{}[{:3}..{:3}] {} {} [{}]",
            indent, range.start, range.end, range.shape_kind, status, bar
        );
    }

    // Also show the raw init ranges from the tracker
    eprintln!();
    eprintln!("Raw initialized ranges:");
    let init_ranges = tracker.ranges();
    if init_ranges.is_empty() {
        eprintln!("  (none)");
    } else {
        for range in init_ranges {
            eprintln!("  [{}..{})", range.start, range.end);
        }
    }
    eprintln!("=== End Layout ===");
}

/// Verified heap that tracks state for Kani proofs.
///
/// This heap doesn't do any real memory operations - it just tracks
/// the abstract state of allocations and asserts that all transitions are valid.
#[derive(Debug)]
pub struct VHeap<S: IShape> {
    /// Each allocation's initialization state and shape.
    /// None = freed, Some((tracker, shape)) = live allocation.
    allocs: [Option<(ByteRangeTracker, S)>; MAX_VHEAP_ALLOCS],
    /// Next allocation ID to assign.
    next_id: u8,
    /// Operation history for each allocation (only when not running under Kani).
    #[cfg(all(not(kani), not(creusot)))]
    history: [AllocHistory; MAX_VHEAP_ALLOCS],
}

impl<S: IShape> VHeap<S> {
    /// Create a new verified heap with no allocations.
    pub fn new() -> Self {
        Self {
            allocs: [const { None }; MAX_VHEAP_ALLOCS],
            next_id: 0,
            #[cfg(all(not(kani), not(creusot)))]
            history: [const { AllocHistory::new() }; MAX_VHEAP_ALLOCS],
        }
    }

    /// Record an operation in the history (no-op under Kani).
    #[cfg(all(not(kani), not(creusot)))]
    fn record_op(&mut self, alloc_id: u8, kind: HeapOpKind, range: Option<Range>) {
        self.history[alloc_id as usize].record(kind, range);
    }

    #[cfg(any(kani, creusot))]
    fn record_op(&mut self, _alloc_id: u8, _kind: HeapOpKind, _range: Option<Range>) {}

    /// Print the history for an allocation (no-op under Kani).
    #[cfg(all(not(kani), not(creusot)))]
    fn print_history(&self, alloc_id: u8) {
        self.history[alloc_id as usize].print(alloc_id);
        // Also print the current layout if allocation is still live
        if let Some((tracker, shape)) = &self.allocs[alloc_id as usize] {
            let alloc_size = shape.layout().map(|l| l.size() as u32).unwrap_or(0);
            print_allocation_layout(tracker, *shape, alloc_size);
        }
    }

    #[cfg(any(kani, creusot))]
    fn print_history(&self, _alloc_id: u8) {}

    /// Assert a condition, printing history and panicking with message if false.
    fn assert_with_history(&self, cond: bool, alloc_id: u8, msg: &str) {
        if !cond {
            self.print_history(alloc_id);
            panic!("{}", msg);
        }
    }

    /// Check that all allocations have been freed (for leak detection).
    #[cfg(test)]
    pub fn assert_no_leaks(&self) {
        for (i, alloc) in self.allocs.iter().enumerate() {
            assert!(alloc.is_none(), "allocation {} not freed", i);
        }
    }

    /// Get the tracker for an allocation, panicking if freed.
    fn get_tracker(&self, alloc_id: u8) -> &(ByteRangeTracker, S) {
        self.allocs[alloc_id as usize]
            .as_ref()
            .expect("allocation already freed")
    }

    /// Get the tracker mutably for an allocation, panicking if freed.
    fn get_tracker_mut(&mut self, alloc_id: u8) -> &mut (ByteRangeTracker, S) {
        self.allocs[alloc_id as usize]
            .as_mut()
            .expect("allocation already freed")
    }

    #[cfg(not(creusot))]
    fn matches_subshape(stored: S, offset: usize, target: S) -> bool {
        if offset == 0 && stored == target {
            return true;
        }

        if stored.is_struct() {
            let st = stored.as_struct().expect("struct shape");
            let field_count = st.field_count();
            for i in 0..field_count {
                let field = st.field(i).expect("field index in range");
                let field_shape = field.shape();
                let field_size = field_shape
                    .layout()
                    .expect("IShape requires sized types")
                    .size();
                let start = field.offset();
                let end = start + field_size;
                if offset >= start && offset < end {
                    return Self::matches_subshape(field_shape, offset - start, target);
                }
            }
            return false;
        }

        if let Some(en) = stored.as_enum() {
            let variant_count = en.variant_count();
            for variant_idx in 0..variant_count {
                let variant = en.variant(variant_idx).expect("variant index in range");
                let payload = variant.data();
                let field_count = payload.field_count();
                for field_idx in 0..field_count {
                    let field = payload.field(field_idx).expect("field index in range");
                    let field_shape = field.shape();
                    let field_size = field_shape
                        .layout()
                        .expect("IShape requires sized types")
                        .size();
                    let start = field.offset();
                    let end = start + field_size;
                    if offset >= start && offset < end {
                        return Self::matches_subshape(field_shape, offset - start, target);
                    }
                }
            }
        }

        false
    }

    #[cfg(creusot)]
    fn matches_subshape(_stored: S, _offset: usize, _target: S) -> bool {
        true
    }

    fn discriminant_size_bytes(repr: EnumDiscriminantRepr) -> Option<u32> {
        match repr {
            EnumDiscriminantRepr::RustNpo => None,
            EnumDiscriminantRepr::U8 | EnumDiscriminantRepr::I8 => Some(1),
            EnumDiscriminantRepr::U16 | EnumDiscriminantRepr::I16 => Some(2),
            EnumDiscriminantRepr::U32 | EnumDiscriminantRepr::I32 => Some(4),
            EnumDiscriminantRepr::U64
            | EnumDiscriminantRepr::I64
            | EnumDiscriminantRepr::Usize
            | EnumDiscriminantRepr::Isize => Some(8),
        }
    }
}

impl<S: IShape> Default for VHeap<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<S: IShape> IHeap<S> for VHeap<S> {
    type Ptr = VPtr;

    unsafe fn alloc(&mut self, shape: S) -> VPtr {
        let id = self.next_id;
        #[cfg(not(creusot))]
        assert!(
            (id as usize) < MAX_VHEAP_ALLOCS,
            "too many allocations (max {})",
            MAX_VHEAP_ALLOCS
        );
        #[cfg(creusot)]
        {
            if (id as usize) >= MAX_VHEAP_ALLOCS {
                panic!("too many allocations");
            }
        }

        let layout = shape.layout().expect("IShape requires sized types");
        self.allocs[id as usize] = Some((ByteRangeTracker::new(), shape));
        self.next_id += 1;

        self.record_op(
            id,
            HeapOpKind::Alloc,
            Some(Range::new(0, layout.size() as u32)),
        );

        VPtr::new(id, layout.size() as u32)
    }

    unsafe fn dealloc(&mut self, ptr: VPtr, shape: S) {
        let id = ptr.alloc_id();
        self.assert_with_history(
            ptr.is_at_start(),
            id,
            "dealloc: pointer not at allocation start",
        );

        let (tracker, stored_shape) = self.get_tracker(id);
        #[cfg(not(creusot))]
        self.assert_with_history(*stored_shape == shape, id, "dealloc: shape mismatch");
        #[cfg(creusot)]
        {
            if *stored_shape != shape {
                self.print_history(id);
                panic!("dealloc: shape mismatch");
            }
        }
        self.assert_with_history(
            tracker.is_empty(),
            id,
            "dealloc: allocation still has initialized bytes",
        );

        self.record_op(id, HeapOpKind::Dealloc, None);
        self.allocs[id as usize] = None;
    }

    unsafe fn dealloc_moved(&mut self, ptr: VPtr, shape: S) {
        let id = ptr.alloc_id();
        self.assert_with_history(
            ptr.is_at_start(),
            id,
            "dealloc_moved: pointer not at allocation start",
        );

        let (_tracker, stored_shape) = self.get_tracker(id);
        #[cfg(not(creusot))]
        self.assert_with_history(*stored_shape == shape, id, "dealloc_moved: shape mismatch");
        #[cfg(creusot)]
        {
            if *stored_shape != shape {
                self.print_history(id);
                panic!("dealloc_moved: shape mismatch");
            }
        }

        self.record_op(id, HeapOpKind::Dealloc, None);
        self.allocs[id as usize] = None;
    }

    unsafe fn memcpy(&mut self, dst: VPtr, src: VPtr, desc: CopyDesc<S>) {
        let len = desc.byte_len();
        if len == 0 {
            return;
        }

        // Bounds checks
        if dst.offset_bytes() + len > dst.alloc_size() {
            self.print_history(dst.alloc_id());
            panic!("memcpy: dst out of bounds");
        }
        if src.offset_bytes() + len > src.alloc_size() {
            self.print_history(src.alloc_id());
            panic!("memcpy: src out of bounds");
        }

        // Check src is initialized
        let (src_tracker, _) = self.get_tracker(src.alloc_id());
        if !src_tracker.is_init(src.offset, src.offset + len as u32) {
            self.print_history(src.alloc_id());
            panic!("memcpy: src range not initialized");
        }

        // Record the memcpy on dst
        self.record_op(
            dst.alloc_id(),
            HeapOpKind::Memcpy,
            Some(Range::new(dst.offset, dst.offset + len as u32)),
        );

        // Check dst is uninitialized and mark it initialized
        let (dst_tracker, _) = self.get_tracker_mut(dst.alloc_id());
        if let Err(e) = dst_tracker.mark_init(dst.offset, dst.offset + len as u32) {
            self.print_history(dst.alloc_id());
            panic!(
                "memcpy: dst range already initialized (forgot to drop?): {:?}",
                e
            );
        }
    }

    unsafe fn drop_in_place(&mut self, ptr: VPtr, shape: S) {
        let layout = shape.layout().expect("IShape requires sized types");
        if layout.size() == 0 {
            return; // ZST - nothing to drop
        }

        let alloc_id = ptr.alloc_id();
        self.record_op(
            alloc_id,
            HeapOpKind::DropInPlace,
            Some(Range::new(ptr.offset, ptr.offset + layout.size() as u32)),
        );

        let (tracker, stored_shape) = self.get_tracker_mut(alloc_id);

        if !Self::matches_subshape(*stored_shape, ptr.offset_bytes(), shape) {
            self.print_history(alloc_id);
            panic!("drop_in_place: shape mismatch");
        }

        if ptr.offset_bytes() + layout.size() > ptr.alloc_size() {
            self.print_history(alloc_id);
            panic!("drop_in_place: out of bounds");
        }

        let base = ptr.offset;
        let end = base + layout.size() as u32;

        if shape.is_struct() {
            let st = shape.as_struct().expect("struct shape");
            let field_count = st.field_count();

            for i in 0..field_count {
                let field = st.field(i).expect("field index in range");
                let field_shape = field.shape();
                let field_size = field_shape
                    .layout()
                    .expect("IShape requires sized types")
                    .size() as u32;
                if field_size == 0 {
                    continue;
                }
                let start = base + field.offset() as u32;
                let field_end = start + field_size;
                if field_end > end {
                    self.print_history(alloc_id);
                    panic!("drop_in_place: field out of bounds");
                }

                if let Err(e) = tracker.mark_uninit(start, field_end) {
                    self.print_history(alloc_id);
                    panic!("drop_in_place: field range not initialized: {:?}", e);
                }
            }

            if let Err(e) = tracker.clear_range(base, end) {
                self.print_history(alloc_id);
                panic!("drop_in_place: clear_range failed: {:?}", e);
            }
            return;
        }

        if shape.is_enum() {
            if let Err(e) = tracker.clear_range(base, end) {
                self.print_history(alloc_id);
                panic!("drop_in_place: clear_range failed: {:?}", e);
            }
            return;
        }

        if let Err(e) = tracker.mark_uninit(base, end) {
            self.print_history(alloc_id);
            panic!("drop_in_place: range not initialized: {:?}", e);
        }
    }

    unsafe fn default_in_place(&mut self, ptr: VPtr, shape: S) -> bool {
        let layout = match shape.layout() {
            Some(layout) => layout,
            None => return false,
        };
        let len = layout.size();
        if len == 0 {
            return true;
        }

        let alloc_id = ptr.alloc_id();

        // Bounds check
        if ptr.offset_bytes() + len > ptr.alloc_size() {
            self.print_history(alloc_id);
            panic!("default_in_place: out of bounds");
        }

        self.record_op(
            alloc_id,
            HeapOpKind::DefaultInPlace,
            Some(Range::new(ptr.offset, ptr.offset + len as u32)),
        );

        let (tracker, _) = self.get_tracker_mut(alloc_id);
        if let Err(e) = tracker.mark_init(ptr.offset, ptr.offset + len as u32) {
            self.print_history(alloc_id);
            panic!("default_in_place: range already initialized: {:?}", e);
        }
        true
    }

    unsafe fn pointer_from_pointee(
        &mut self,
        dst: VPtr,
        pointer_shape: S,
        _src: VPtr,
        _pointee_shape: S,
    ) -> bool {
        let Some(layout) = pointer_shape.layout() else {
            return false;
        };
        let len = layout.size();
        if len == 0 {
            return true;
        }

        let alloc_id = dst.alloc_id();
        if dst.offset_bytes() + len > dst.alloc_size() {
            self.print_history(alloc_id);
            panic!("pointer_from_pointee: out of bounds");
        }

        self.record_op(
            alloc_id,
            HeapOpKind::Memcpy,
            Some(Range::new(dst.offset, dst.offset + len as u32)),
        );

        let (tracker, _) = self.get_tracker_mut(alloc_id);
        if let Err(e) = tracker.mark_init(dst.offset, dst.offset + len as u32) {
            self.print_history(alloc_id);
            panic!(
                "pointer_from_pointee: destination already initialized: {:?}",
                e
            );
        }
        true
    }

    unsafe fn select_enum_variant(&mut self, dst: VPtr, enum_shape: S, variant_idx: usize) -> bool {
        let Some(discriminant_repr) = enum_shape.enum_discriminant_repr() else {
            return false;
        };
        let Some(_discriminant) = enum_shape.enum_variant_discriminant(variant_idx) else {
            return false;
        };
        let Some(len) = Self::discriminant_size_bytes(discriminant_repr) else {
            return false;
        };
        if len == 0 {
            return false;
        }

        let alloc_id = dst.alloc_id();
        let end = dst.offset.saturating_add(len);
        if (end as usize) > dst.alloc_size() {
            self.print_history(alloc_id);
            panic!("select_enum_variant: out of bounds");
        }

        self.record_op(
            alloc_id,
            HeapOpKind::Memcpy,
            Some(Range::new(dst.offset, end)),
        );

        let (tracker, _) = self.get_tracker_mut(alloc_id);
        if !tracker.is_init(dst.offset, end) {
            if let Err(e) = tracker.mark_init(dst.offset, end) {
                self.print_history(alloc_id);
                panic!("select_enum_variant: discriminant init failed: {:?}", e);
            }
        }
        true
    }
}

// ==================================================================
// Arena
// ==================================================================

/// Maximum number of arena slots for verification.
pub const MAX_VARENA_SLOTS: usize = 16;

/// Slot state for verified arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VSlotState {
    Empty,
    Occupied,
}

/// Fixed-size arena for Kani verification.
///
/// Uses a fixed-size array instead of Vec to keep state space bounded.
pub struct VArena<T, const N: usize = MAX_VARENA_SLOTS> {
    /// Slot storage. Index 0 is reserved (NOT_STARTED sentinel).
    slots: [Option<T>; N],
    /// State of each slot.
    states: [VSlotState; N],
    /// Next slot to try allocating from.
    next: usize,
}

impl<T, const N: usize> VArena<T, N> {
    /// Create a new verified arena.
    pub fn new() -> Self {
        Self {
            slots: core::array::from_fn(|_| None),
            states: [VSlotState::Empty; N],
            next: 1, // Skip slot 0 (reserved for NOT_STARTED)
        }
    }
}

impl<T, const N: usize> Default for VArena<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> IArena<T> for VArena<T, N> {
    fn alloc(&mut self, value: T) -> Idx<T> {
        // Find an empty slot starting from next
        for i in self.next..N {
            if self.states[i] == VSlotState::Empty {
                self.slots[i] = Some(value);
                self.states[i] = VSlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        // Wrap around and search from beginning (skip slot 0)
        for i in 1..self.next {
            if self.states[i] == VSlotState::Empty {
                self.slots[i] = Some(value);
                self.states[i] = VSlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        panic!("arena full");
    }

    fn free(&mut self, id: Idx<T>) -> T {
        assert!(id.is_valid(), "cannot free sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == VSlotState::Occupied,
            "double-free or freeing empty slot"
        );

        self.states[idx] = VSlotState::Empty;
        self.slots[idx].take().expect("slot was occupied but empty")
    }

    fn get(&self, id: Idx<T>) -> &T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == VSlotState::Occupied,
            "slot is not occupied"
        );

        self.slots[idx]
            .as_ref()
            .expect("slot was occupied but empty")
    }

    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            self.states[idx] == VSlotState::Occupied,
            "slot is not occupied"
        );

        self.slots[idx]
            .as_mut()
            .expect("slot was occupied but empty")
    }
}

// ==================================================================
// Tests
// ==================================================================

#[cfg(test)]
mod tests;
#[cfg(test)]
mod verus_bridge;

#[cfg(kani)]
mod proofs;
#[cfg(creusot)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, DeepModel)]
pub struct VLayout {
    pub size: usize,
    pub align: usize,
}

#[cfg(creusot)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VLayoutError;

#[cfg(creusot)]
impl VLayout {
    #[inline]
    pub const fn new<T>() -> Self {
        Self {
            size: core::mem::size_of::<T>(),
            align: core::mem::align_of::<T>(),
        }
    }

    #[inline]
    pub const fn size(self) -> usize {
        self.size
    }

    #[inline]
    pub const fn align(self) -> usize {
        self.align
    }

    pub fn from_size_align(size: usize, align: usize) -> Result<Self, VLayoutError> {
        if align == 0 || !align.is_power_of_two() {
            return Err(VLayoutError);
        }
        Ok(Self { size, align })
    }

    #[inline]
    pub fn to_layout(self) -> Layout {
        Layout::from_size_align(self.size, self.align).expect("valid layout")
    }
}

#[cfg(not(creusot))]
pub type VLayout = Layout;

#[cfg(not(creusot))]
pub type VLayoutError = std::alloc::LayoutError;
