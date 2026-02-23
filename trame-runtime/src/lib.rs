//! All traits used to represent a runtime on which trame can operate.

#![cfg_attr(kani, feature(stmt_expr_attributes))]
#![cfg_attr(kani, feature(proc_macro_hygiene))]

pub mod live;

#[cfg(not(creusot))]
pub mod verified;

use std::{alloc::Layout, marker::PhantomData};

#[cfg(creusot)]
use creusot_std::logic::Int;
#[cfg(creusot)]
use creusot_std::model::DeepModel;

#[cfg(creusot)]
use creusot_std::macros::{ensures, logic, requires};

#[cfg(creusot)]
use creusot_std::prelude::{check, extern_spec, trusted};

#[cfg(creusot)]
pub type VLayout = std::alloc::Layout;

#[cfg(creusot)]
pub type VLayoutError = std::alloc::LayoutError;

#[cfg(not(creusot))]
pub use verified::{VLayout, VLayoutError};

/// A heap and a shape implementation, over which Trame can be parameterized
pub trait IRuntime {
    type Shape: IShape;
    type Heap: IHeap<Self::Shape, Ptr: IPtr>;
    type Arena<T>: IArena<T>;

    fn heap() -> Self::Heap;
    fn arena<T>() -> Self::Arena<T>;
}

/// Marker trait for runtimes that use real facet shapes and raw pointers.
#[cfg(not(creusot))]
pub trait LiveRuntime:
    IRuntime<Shape = &'static facet_core::Shape, Heap: IHeap<&'static facet_core::Shape, Ptr = *mut u8>>
{
}

#[cfg(not(creusot))]
impl<T> LiveRuntime for T where
    T: IRuntime<
            Shape = &'static facet_core::Shape,
            Heap: IHeap<&'static facet_core::Shape, Ptr = *mut u8>,
        >
{
}

// ==================================================================
// Shape
// ==================================================================

/// A store of shapes that can be looked up by handle.
pub trait IShapeStore: Clone {
    /// The handle type used to reference shapes in this store.
    type Handle: Copy;

    /// The view type produced by this store.
    type View<'a>: IShape
    where
        Self: 'a;

    /// Look up a shape by handle.
    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a>;
}

#[cfg(creusot)]
pub trait IShapeExtra {
    #[logic]
    fn size_logic(self) -> usize;
}

#[cfg(creusot)]
impl<T> IShapeExtra for T {
    #[logic(opaque)]
    fn size_logic(self) -> usize {
        dead
    }
}

#[cfg(not(creusot))]
pub trait IShapeExtra {}

#[cfg(not(creusot))]
impl<T> IShapeExtra for T {}

/// Common interface for shapes.
///
/// Implemented by:
/// - `&'static facet_core::Shape` (real shapes)
/// - store-specific shape views (synthetic shapes for verification)
///
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnumReprKind {
    Flattened,
    ExternallyTagged,
    InternallyTagged {
        tag: &'static str,
    },
    AdjacentlyTagged {
        tag: &'static str,
        content: &'static str,
    },
}

/// In-memory representation of an enum discriminant/tag.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnumDiscriminantRepr {
    RustNpo,
    U8,
    U16,
    U32,
    U64,
    Usize,
    I8,
    I16,
    I32,
    I64,
    Isize,
}

/// Interface for enum type information.
pub trait IEnumType: Copy {
    /// Struct type used for variant payloads.
    type StructType: IStructType;
    /// Variant descriptor type.
    type Variant: IVariantType<StructType = Self::StructType>;

    /// Number of variants in this enum.
    fn variant_count(&self) -> usize;

    /// Get variant by index.
    fn variant(&self, idx: usize) -> Option<Self::Variant>;
}

/// Interface for enum variant information.
pub trait IVariantType: Copy {
    /// Struct layout of this variant's payload.
    type StructType: IStructType;

    /// Rust variant name.
    fn name(&self) -> &'static str;

    /// Effective serialized variant name.
    fn effective_name(&self) -> &'static str {
        self.name()
    }

    /// Variant payload struct.
    fn data(&self) -> Self::StructType;
}

/// Interface for list type information.
pub trait IListType: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Shape of list elements.
    fn element(&self) -> Self::Shape;

    /// Initialize the list in place with a capacity hint.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `dst` must point at storage for this list type.
    unsafe fn init_in_place_with_capacity(&self, _dst: *mut u8, _capacity: usize) -> bool {
        false
    }

    /// Push one element into this list.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `list_ptr` must point to an initialized list and `elem_ptr` to an
    /// initialized element matching `element()`.
    unsafe fn push_element(&self, _list_ptr: *mut u8, _elem_ptr: *mut u8) -> bool {
        false
    }
}

/// Interface for set type information.
pub trait ISetType: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Shape of set elements.
    fn element(&self) -> Self::Shape;

    /// Initialize the set in place with a capacity hint.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `dst` must point at storage for this set type.
    unsafe fn init_in_place_with_capacity(&self, _dst: *mut u8, _capacity: usize) -> bool {
        false
    }

    /// Insert one element into this set.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `set_ptr` must point to an initialized set and `elem_ptr` to an
    /// initialized element matching `element()`.
    unsafe fn insert_element(&self, _set_ptr: *mut u8, _elem_ptr: *mut u8) -> bool {
        false
    }
}

/// Interface for map type information.
pub trait IMapType: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Shape of map keys.
    fn key(&self) -> Self::Shape;

    /// Shape of map values.
    fn value(&self) -> Self::Shape;

    /// Initialize the map in place with a capacity hint.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `dst` must point at storage for this map type.
    unsafe fn init_in_place_with_capacity(&self, _dst: *mut u8, _capacity: usize) -> bool {
        false
    }

    /// Insert one entry into this map.
    ///
    /// Returns `false` when unsupported.
    ///
    /// # Safety
    /// `map_ptr` must point to an initialized map, and `key_ptr`/`value_ptr`
    /// must point to initialized values matching `key()`/`value()`.
    unsafe fn insert_entry(
        &self,
        _map_ptr: *mut u8,
        _key_ptr: *mut u8,
        _value_ptr: *mut u8,
    ) -> bool {
        false
    }
}

pub trait IShape: Copy + PartialEq + IShapeExtra {
    /// The struct type returned by `as_struct()`.
    type StructType: IStructType<Field = Self::Field>;

    /// The field type used by struct types.
    type Field: IField<Shape = Self>;

    /// The enum type returned by `as_enum()`.
    type EnumType: IEnumType<StructType = Self::StructType>;

    /// The smart-pointer metadata returned by `as_pointer()`.
    type PointerType: IPointerType<Shape = Self>;

    /// The list metadata returned by `as_list()`.
    type ListType: IListType<Shape = Self>;

    /// The set metadata returned by `as_set()`.
    type SetType: ISetType<Shape = Self>;

    /// The map metadata returned by `as_map()`.
    type MapType: IMapType<Shape = Self>;

    /// Get the layout (size and alignment) of this shape.
    ///
    /// Returns `None` for unsized types.
    #[cfg_attr(creusot, ensures(match result {
        Some(l) => layout_size_logic(l) == (*self).size_logic()@,
        None => true,
    }))]
    fn layout(&self) -> Option<Layout>;

    /// Check if this is a struct type.
    fn is_struct(&self) -> bool;

    /// Get struct-specific information, if this is a struct.
    fn as_struct(&self) -> Option<Self::StructType>;

    /// Check if this is an enum type.
    fn is_enum(&self) -> bool {
        self.as_enum().is_some()
    }

    /// Get enum-specific information, if this is an enum.
    fn as_enum(&self) -> Option<Self::EnumType>;

    /// Enum representation metadata for this shape, if this is an enum.
    fn enum_repr_kind(&self) -> Option<EnumReprKind> {
        None
    }

    /// In-memory discriminant representation for this shape, if this is an enum.
    fn enum_discriminant_repr(&self) -> Option<EnumDiscriminantRepr> {
        None
    }

    /// In-memory discriminant value for the given variant, if available.
    fn enum_variant_discriminant(&self, _variant_idx: usize) -> Option<i64> {
        None
    }

    /// Type identifier (without generic params), when available.
    fn type_identifier(&self) -> &'static str {
        "<unknown>"
    }

    /// If this shape is `Option<T>`, returns `T`.
    fn option_payload(&self) -> Option<Self> {
        None
    }

    /// Check if this is an option type.
    fn is_option(&self) -> bool {
        self.option_payload().is_some()
    }

    /// Check if this is a smart-pointer type.
    fn is_pointer(&self) -> bool;

    /// Get smart-pointer specific information, if this is a pointer.
    fn as_pointer(&self) -> Option<Self::PointerType>;

    /// Check if this is a list type.
    fn is_list(&self) -> bool {
        self.as_list().is_some()
    }

    /// Get list-specific information, if this is a list.
    fn as_list(&self) -> Option<Self::ListType>;

    /// Element shape for sequence-like types, when available.
    ///
    /// By default this mirrors `as_list()`, but runtimes may override to
    /// expose unsized slice-like element types as well.
    fn sequence_element(&self) -> Option<Self> {
        self.as_list().map(|list| list.element())
    }

    /// Check if this is a set type.
    fn is_set(&self) -> bool {
        self.as_set().is_some()
    }

    /// Get set-specific information, if this is a set.
    fn as_set(&self) -> Option<Self::SetType>;

    /// Check if this is a map type.
    fn is_map(&self) -> bool {
        self.as_map().is_some()
    }

    /// Get map-specific information, if this is a map.
    fn as_map(&self) -> Option<Self::MapType>;
}

/// Shape operations required by executable heaps.
///
/// This extends `IShape` with the operations that an actual heap needs at runtime
/// to initialize, drop, and build pointer values.
pub trait IExecShape<P>: IShape {
    /// Whether dropping values of this shape can have observable effects.
    fn needs_drop(&self) -> bool;

    /// Drop the value at `ptr`.
    ///
    /// # Safety
    /// `ptr` must point to a valid initialized value of this shape.
    unsafe fn drop_in_place(&self, ptr: P);

    /// Default-initialize the value at `ptr`.
    ///
    /// Returns false if this shape has no default operation.
    ///
    /// # Safety
    /// `ptr` must point to uninitialized storage for this shape.
    unsafe fn default_in_place(&self, ptr: P) -> bool;

    /// Construct a pointer value at `dst` from a pointee value at `src`.
    ///
    /// Returns false if this pointer shape cannot be constructed from the given pointee shape.
    ///
    /// # Safety
    /// `dst` must point to uninitialized storage for `self`, and `src` must point to an
    /// initialized value of `pointee_shape`.
    unsafe fn pointer_from_pointee(&self, dst: P, src: P, pointee_shape: Self) -> bool;
}

/// Interface for smart pointer type information.
pub trait IPointerType: Copy {
    /// The shape type.
    type Shape: IShape;

    /// Shape of the pointee, if available.
    fn pointee(&self) -> Option<Self::Shape>;

    /// Whether this pointer can be constructed from a pointee value.
    fn constructible_from_pointee(&self) -> bool;

    /// Whether this pointer supports slice-builder based construction for unsized payloads.
    fn supports_slice_builder(&self) -> bool {
        false
    }

    /// Create a new pointer slice builder.
    ///
    /// Returns `None` if slice builders are unsupported.
    ///
    /// # Safety
    /// The returned pointer must be managed according to the corresponding
    /// `slice_builder_*` operations.
    unsafe fn slice_builder_new(&self) -> Option<*mut u8> {
        None
    }

    /// Push one initialized item into a pointer slice builder.
    ///
    /// Returns `false` if unsupported.
    ///
    /// # Safety
    /// `builder_ptr` must be a live builder previously returned by
    /// `slice_builder_new`; `item_ptr` must point to an initialized item.
    unsafe fn slice_builder_push(&self, _builder_ptr: *mut u8, _item_ptr: *mut u8) -> bool {
        false
    }

    /// Convert a pointer slice builder into an allocated pointer value blob.
    ///
    /// Returns `None` if unsupported.
    ///
    /// # Safety
    /// `builder_ptr` must be a live builder and is consumed by this call.
    unsafe fn slice_builder_convert(&self, _builder_ptr: *mut u8) -> Option<*mut u8> {
        None
    }

    /// Free a pointer slice builder without converting it.
    ///
    /// # Safety
    /// `builder_ptr` must be a live builder and is consumed by this call.
    unsafe fn slice_builder_free(&self, _builder_ptr: *mut u8) {}

    /// Whether this pointer is specifically `Box<T>`.
    fn is_known_box(&self) -> bool;
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

    /// Rust field name.
    fn name(&self) -> &'static str {
        ""
    }

    /// Effective serialized field name.
    fn effective_name(&self) -> &'static str {
        self.name()
    }

    /// Field alias, if any.
    fn alias(&self) -> Option<&'static str> {
        None
    }

    /// Whether this field is flattened.
    fn is_flattened(&self) -> bool {
        false
    }

    /// Whether this field has a default.
    fn has_default(&self) -> bool {
        false
    }

    /// Whether this field should be skipped during deserialization.
    fn should_skip_deserializing(&self) -> bool {
        false
    }
}

// ==================================================================
// Heap
// ==================================================================

/// Descriptor for typed copy operations.
#[derive(Clone, Copy, Debug)]
pub enum CopyDesc<S> {
    /// Copy one value of this shape.
    Value(S),
    /// Copy `count` consecutive elements of this shape.
    Repeat { elem: S, count: usize },
}

impl<S> CopyDesc<S> {
    #[cfg(creusot)]
    #[ensures(result == Self::Value(shape))]
    pub fn value(shape: S) -> Self {
        Self::Value(shape)
    }

    #[cfg(not(creusot))]
    pub const fn value(shape: S) -> Self {
        Self::Value(shape)
    }

    #[cfg(creusot)]
    #[ensures(result == Self::Repeat { elem, count })]
    pub fn repeat(elem: S, count: usize) -> Self {
        Self::Repeat { elem, count }
    }

    #[cfg(not(creusot))]
    pub const fn repeat(elem: S, count: usize) -> Self {
        Self::Repeat { elem, count }
    }
}

impl<S: IShape> CopyDesc<S> {
    /// Compute the byte length described by this descriptor.
    #[cfg_attr(creusot, trusted)]
    pub fn byte_len(self) -> usize {
        match self {
            Self::Value(shape) => shape
                .layout()
                .expect("CopyDesc::Value requires a sized shape")
                .size(),
            Self::Repeat { elem, count } => elem
                .layout()
                .expect("CopyDesc::Repeat requires a sized element shape")
                .size()
                .checked_mul(count)
                .expect("CopyDesc byte length overflow"),
        }
    }

    #[cfg(creusot)]
    #[logic(open, inline)]
    pub fn byte_len_logic(self) -> usize {
        pearlite! {
            match self {
                CopyDesc::Value(shape) => shape.size_logic(),
                CopyDesc::Repeat { elem, count } => elem.size_logic() * count,
            }
        }
    }
}

#[cfg(creusot)]
/// State of an allocation
pub enum MemState {
    /// Allocated but uninitialized
    Uninit,
    /// Initialized
    Init,
}

/// Heap for memory operations, generic over shape type.
pub trait IHeap<S: IShape> {
    /// Pointer type used by this heap.
    type Ptr: IPtr;

    /// Allocate a region for a value of the given shape.
    ///
    /// # Safety
    /// The caller must ensure `shape` is valid for allocation and that any
    /// constraints required by the heap implementation are satisfied.
    #[cfg_attr(creusot,
        ensures((^self).is_uninit(result, shape)),
        ensures(forall<p, s, z> self.is(z, p, s) ==> (^self).is(z, p, s) && p != result)
    )]
    unsafe fn alloc(&mut self, shape: S) -> Self::Ptr;

    /// Deallocate a region.
    ///
    /// # Safety
    /// The caller must ensure `ptr` points to the start of a live allocation
    /// previously returned by `alloc`, that the allocation corresponds to
    /// `shape`, and that no bytes in the region are still initialized.
    #[cfg_attr(creusot,
        requires(self.is_uninit(ptr, shape)),
        ensures(forall<p, s, z> p != ptr && self.is(z, p, s) ==> (^self).is(z, p, s))
    )]
    unsafe fn dealloc(&mut self, ptr: Self::Ptr, shape: S);

    /// Deallocate storage that was moved out without running drop.
    ///
    /// This is used when ownership has been transferred elsewhere (for example,
    /// when constructing a smart pointer from a staged pointee allocation).
    ///
    /// # Safety
    /// The caller must ensure `ptr` points to a live allocation for `shape`.
    unsafe fn dealloc_moved(&mut self, ptr: Self::Ptr, shape: S);

    /// Allocate contiguous staging storage for `count` values of `elem_shape`.
    ///
    /// This is used by container rope staging to reserve stable chunk buffers.
    ///
    /// # Safety
    /// The caller must ensure `elem_shape` is valid for allocation and `count`
    /// is the intended number of elements for this chunk.
    unsafe fn alloc_repeat(&mut self, elem_shape: S, count: usize) -> Self::Ptr;

    /// Deallocate repeat storage previously allocated by `alloc_repeat`.
    ///
    /// # Safety
    /// The caller must ensure all bytes in the chunk are uninitialized.
    unsafe fn dealloc_repeat(&mut self, ptr: Self::Ptr, elem_shape: S, count: usize);

    /// Deallocate repeat storage whose values were moved out without drop.
    ///
    /// # Safety
    /// The caller must ensure `ptr` is a live allocation from `alloc_repeat`.
    unsafe fn dealloc_repeat_moved(&mut self, ptr: Self::Ptr, elem_shape: S, count: usize);

    /// Copy bytes from `src` to `dst` according to a typed descriptor.
    ///
    /// # Safety
    /// The caller must ensure both ranges are in-bounds for their allocations,
    /// that `src` is fully initialized for the descriptor span, `dst` is fully
    /// uninitialized for that same span, and the ranges do not overlap.
    #[cfg_attr(creusot,
        requires(self.is_init_copy(src, desc)),
        requires(self.is_uninit_copy(dst, desc)),
        ensures((^self).is_init_copy(dst, desc)),
        ensures(forall<p, s, z> p != dst && (*self).is(z, p, s) ==> (^self).is(z, p, s))
    )]
    unsafe fn memcpy(&mut self, dst: Self::Ptr, src: Self::Ptr, desc: CopyDesc<S>);

    /// Default-initialize the value at `ptr` and mark the range as initialized.
    ///
    /// Returns `false` if the shape has no default.
    ///
    /// # Safety
    /// The caller must ensure the destination range is uninitialized, in-bounds,
    /// and corresponds to `shape`.
    #[cfg_attr(creusot,
        requires(self.is_uninit(ptr, shape)),
        ensures(if result { (^self).is_init(ptr, shape) } else { (^self).is_uninit(ptr, shape) }),
        ensures(forall<p, s, z> p != ptr && (*self).is(z, p, s) ==> (^self).is(z, p, s))
    )]
    unsafe fn default_in_place(&mut self, ptr: Self::Ptr, shape: S) -> bool;

    /// Drop the value at `ptr` and mark the range as uninitialized.
    ///
    /// # Safety
    /// The caller must ensure `ptr` points to a value of type `shape`, the
    /// value is fully initialized, and the allocation is still live.
    #[cfg_attr(creusot,
        requires(self.is_init(ptr, shape)),
        ensures((^self).is_uninit(ptr, shape)),
        ensures(forall<p, s, z> p != ptr && (*self).is(z, p, s) ==> (^self).is(z, p, s))
    )]
    unsafe fn drop_in_place(&mut self, ptr: Self::Ptr, shape: S);

    /// Construct a pointer value at `dst` from a pointee value at `src`.
    ///
    /// Returns `false` if this pointer type cannot be constructed from a pointee.
    ///
    /// # Safety
    /// The caller must ensure `dst` points to uninitialized storage for `pointer_shape`
    /// and `src` points to an initialized value of `pointee_shape`.
    #[cfg_attr(creusot,
        requires(self.is_init(src, pointee_shape)),
        requires(self.is_uninit(dst, pointee_shape)),
        ensures(result ==> (^self).is_init(dst, pointer_shape)),
        ensures(!result ==> (^self).is_uninit(dst, pointer_shape) && (^self).is_init(src, pointee_shape)),
        ensures(forall<p, s, z> p != src && p != dst && (*self).is(z, p, s) ==> (^self).is(z, p, s))
    )]
    unsafe fn pointer_from_pointee(
        &mut self,
        dst: Self::Ptr,
        pointer_shape: S,
        src: Self::Ptr,
        pointee_shape: S,
    ) -> bool;

    /// Create a slice builder for an unsized pointer payload (for example `Arc<[T]>`).
    ///
    /// Returns `None` if unsupported for this pointer shape.
    ///
    /// # Safety
    /// The caller must ensure `pointer_shape` is a valid pointer shape for this runtime.
    unsafe fn pointer_slice_builder_new(&mut self, _pointer_shape: S) -> Option<Self::Ptr> {
        None
    }

    /// Push one initialized element into a pointer slice builder.
    ///
    /// Returns `false` if unsupported or if the shapes are incompatible.
    ///
    /// # Safety
    /// The caller must ensure `builder_ptr` is a live builder for `pointer_shape` and
    /// `item_ptr` points at an initialized value of `item_shape`.
    unsafe fn pointer_slice_builder_push(
        &mut self,
        _builder_ptr: Self::Ptr,
        _pointer_shape: S,
        _item_ptr: Self::Ptr,
        _item_shape: S,
    ) -> bool {
        false
    }

    /// Convert a pointer slice builder into a final pointer value written to `dst`.
    ///
    /// Returns `false` if unsupported.
    ///
    /// # Safety
    /// The caller must ensure `dst` points at uninitialized storage for `pointer_shape`
    /// and `builder_ptr` is a live builder for `pointer_shape`.
    unsafe fn pointer_slice_builder_convert_into(
        &mut self,
        _dst: Self::Ptr,
        _pointer_shape: S,
        _builder_ptr: Self::Ptr,
    ) -> bool {
        false
    }

    /// Free a pointer slice builder without converting it.
    ///
    /// # Safety
    /// The caller must ensure `builder_ptr` is a live builder for `pointer_shape`.
    unsafe fn pointer_slice_builder_free(&mut self, _pointer_shape: S, _builder_ptr: Self::Ptr) {}

    /// Select an enum variant by writing/updating its discriminant/tag in memory.
    ///
    /// Returns `false` if the enum representation or discriminant metadata is unsupported.
    ///
    /// # Safety
    /// The caller must ensure `dst` points at storage for `enum_shape`.
    unsafe fn select_enum_variant(
        &mut self,
        dst: Self::Ptr,
        enum_shape: S,
        variant_idx: usize,
    ) -> bool;

    /// Initialize a list value in place, optionally with a capacity hint.
    ///
    /// Returns `false` if this shape is not a list, or if list initialization
    /// is unsupported for this runtime/shape combination.
    ///
    /// # Safety
    /// The caller must ensure `dst` points at storage for `list_shape`.
    unsafe fn list_init_in_place_with_capacity(
        &mut self,
        dst: Self::Ptr,
        list_shape: S,
        capacity: usize,
    ) -> bool;

    /// Push one initialized element into a list value.
    ///
    /// Returns `false` if shapes are incompatible, or if list push is unsupported.
    ///
    /// # Safety
    /// The caller must ensure `list_ptr` points at an initialized list of shape
    /// `list_shape`, and `elem_ptr` points at an initialized value of `elem_shape`.
    unsafe fn list_push_element(
        &mut self,
        list_ptr: Self::Ptr,
        list_shape: S,
        elem_ptr: Self::Ptr,
        elem_shape: S,
    ) -> bool;

    /// Initialize a set value in place, optionally with a capacity hint.
    ///
    /// Returns `false` if this shape is not a set, or if set initialization
    /// is unsupported for this runtime/shape combination.
    ///
    /// # Safety
    /// The caller must ensure `dst` points at storage for `set_shape`.
    unsafe fn set_init_in_place_with_capacity(
        &mut self,
        dst: Self::Ptr,
        set_shape: S,
        capacity: usize,
    ) -> bool;

    /// Insert one initialized element into a set value.
    ///
    /// Returns `false` if shapes are incompatible, or if set insert is unsupported.
    ///
    /// # Safety
    /// The caller must ensure `set_ptr` points at an initialized set of shape
    /// `set_shape`, and `elem_ptr` points at an initialized value of `elem_shape`.
    unsafe fn set_insert_element(
        &mut self,
        set_ptr: Self::Ptr,
        set_shape: S,
        elem_ptr: Self::Ptr,
        elem_shape: S,
    ) -> bool;

    /// Initialize a map value in place, optionally with a capacity hint.
    ///
    /// Returns `false` if this shape is not a map, or if map initialization
    /// is unsupported for this runtime/shape combination.
    ///
    /// # Safety
    /// The caller must ensure `dst` points at storage for `map_shape`.
    unsafe fn map_init_in_place_with_capacity(
        &mut self,
        dst: Self::Ptr,
        map_shape: S,
        capacity: usize,
    ) -> bool;

    /// Insert one initialized key/value entry into a map value.
    ///
    /// Returns `false` if shapes are incompatible, or if map insert is unsupported.
    ///
    /// # Safety
    /// The caller must ensure `map_ptr` points at an initialized map of shape
    /// `map_shape`, and `key_ptr`/`value_ptr` point at initialized values of
    /// `key_shape`/`value_shape`.
    unsafe fn map_insert_entry(
        &mut self,
        map_ptr: Self::Ptr,
        map_shape: S,
        key_ptr: Self::Ptr,
        key_shape: S,
        value_ptr: Self::Ptr,
        value_shape: S,
    ) -> bool;

    /// Creusot predicate for the state of an allocation.
    #[cfg(creusot)]
    #[logic]
    fn is(&self, state: MemState, ptr: Self::Ptr, shape: S) -> bool;

    /// Creusot predicate for allocated but uninitialized memory. Produced by `alloc`.
    #[cfg(creusot)]
    #[logic(open, inline, sealed)]
    fn is_uninit(&self, ptr: Self::Ptr, shape: S) -> bool {
        pearlite! { self.is(MemState::Uninit, ptr, shape) }
    }

    /// Creusot predicate for initialized memory. Produced by `memcpy` and `default_in_place`.
    #[cfg(creusot)]
    #[logic(open, inline, sealed)]
    fn is_init(&self, ptr: Self::Ptr, shape: S) -> bool {
        pearlite! { self.is(MemState::Init, ptr, shape) }
    }

    /// Extension of `is` for `CopyDesc<S>`.
    #[cfg(creusot)]
    #[logic(open, sealed)]
    fn is_copy(&self, state: MemState, ptr: Self::Ptr, shape: CopyDesc<S>) -> bool {
        pearlite! {
            match shape {
                CopyDesc::Value(shape) => self.is(state, ptr, shape),
                CopyDesc::Repeat { elem, count } => forall<i> 0usize <= i && i < count ==>
                    self.is(state, ptr.byte_add_logic(i * elem.size_logic()), elem),
            }
        }
    }

    /// Extension of `is_uninit` for `CopyDesc<S>`.
    #[cfg(creusot)]
    #[logic(open, sealed)]
    fn is_uninit_copy(&self, ptr: Self::Ptr, shape: CopyDesc<S>) -> bool {
        pearlite! {
            self.is_copy(MemState::Uninit, ptr, shape)
        }
    }

    /// Extension of `is_init` for `CopyDesc<S>`.
    #[cfg(creusot)]
    #[logic(open, sealed)]
    fn is_init_copy(&self, ptr: Self::Ptr, shape: CopyDesc<S>) -> bool {
        pearlite! {
            self.is_copy(MemState::Init, ptr, shape)
        }
    }
}

/// Pointer type
pub trait IPtr: Copy {
    /// Compute a new pointer at a byte offset from this one.
    ///
    /// # Safety
    /// The caller must ensure the resulting pointer is in-bounds.
    unsafe fn byte_add(self, n: usize) -> Self;

    #[cfg(creusot)]
    #[logic]
    fn byte_add_logic(self, n: usize) -> Self;
}

impl IPtr for *mut u8 {
    #[inline]
    unsafe fn byte_add(self, n: usize) -> Self {
        // SAFETY: caller ensures the resulting pointer is in-bounds.
        unsafe { self.byte_add(n) }
    }

    #[cfg(creusot)]
    #[logic(opaque)]
    fn byte_add_logic(self, _n: usize) -> Self {
        dead
    }
}

// ============================================================================
// Arena
// ==================================================================

/// Arena for allocating and managing items.
pub trait IArena<T> {
    /// Allocate a new item, returning its index.
    fn alloc(&mut self, value: T) -> Idx<T>;

    /// Free an item, returning it.
    ///
    /// # Panics
    /// Panics if the index is invalid or already freed.
    fn free(&mut self, id: Idx<T>) -> T;

    /// Get a reference to an item.
    ///
    /// # Panics
    /// Panics if the index is invalid or freed.
    #[cfg_attr(creusot,
        requires(self.contains(id)),
        ensures(*result == self.get_logic(id))
    )]
    fn get(&self, id: Idx<T>) -> &T;

    /// Get a mutable reference to an item.
    ///
    /// # Panics
    /// Panics if the index is invalid or freed.
    #[cfg_attr(creusot,
        requires(self.contains(id)),
        ensures(*result == (*self).get_logic(id)),
        ensures(^result == (^self).get_logic(id)),
        ensures((^self).contains(id)),
        ensures(forall<j> j != id ==> (*self).contains(j) == (^self).contains(j)),
        ensures(forall<j> j != id ==> (*self).get_logic(j) == (^self).get_logic(j))
    )]
    fn get_mut(&mut self, id: Idx<T>) -> &mut T;

    #[cfg(creusot)]
    #[logic]
    fn contains(self, id: Idx<T>) -> bool;

    #[cfg(creusot)]
    #[logic]
    fn get_logic(self, id: Idx<T>) -> T;
}

/// A typed index into an arena.
///
/// The phantom type prevents mixing indices from different arenas.
#[derive(Debug)]
pub struct Idx<T> {
    pub raw: u32,
    _ty: PhantomData<fn() -> T>,
}

#[cfg(creusot)]
impl<T> creusot_std::model::DeepModel for Idx<T> {
    type DeepModelTy = u32;

    #[creusot_std::macros::logic(open, inline)]
    fn deep_model(self) -> Self::DeepModelTy {
        self.raw
    }
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    #[cfg_attr(
        creusot,
        creusot_std::macros::ensures(result == (self.deep_model() == other.deep_model()))
    )]
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Idx<T> {}

impl<T> Idx<T> {
    /// Sentinel: slot not started (reserved, slot 0)
    pub const fn not_started() -> Self {
        Self {
            raw: 0,
            _ty: PhantomData,
        }
    }

    /// Sentinel: slot completed/freed
    pub const fn complete() -> Self {
        Self {
            raw: u32::MAX,
            _ty: PhantomData,
        }
    }

    /// Sentinel: slot not started (reserved, slot 0)
    #[cfg(not(creusot))]
    pub const NOT_STARTED: Self = Self::not_started();

    /// Sentinel: slot completed/freed
    #[cfg(not(creusot))]
    pub const COMPLETE: Self = Self::complete();

    #[inline]
    pub fn is_not_started(self) -> bool {
        self.raw == 0
    }

    #[inline]
    pub fn is_complete(self) -> bool {
        self.raw == u32::MAX
    }

    #[inline]
    pub fn is_valid(self) -> bool {
        self.raw != 0 && self.raw != u32::MAX
    }

    #[inline]
    pub fn same(self, other: Self) -> bool {
        self.raw == other.raw
    }

    #[inline]
    #[cfg_attr(creusot, trusted)]
    fn index(self) -> usize {
        debug_assert!(self.is_valid(), "cannot get index of sentinel");
        self.raw as usize
    }

    /// Create an index from a raw value (for internal use).
    fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _ty: PhantomData,
        }
    }
}

#[cfg(creusot)]
extern_spec! {
    mod core {
        mod alloc {
            impl Layout {
                #[check(ghost)]
                #[ensures(result@ == layout_size_logic(*self))]
                fn size(&self) -> usize;
            }
        }
    }
}

#[cfg(creusot)]
#[logic(opaque)]
pub fn layout_size_logic(_layout: std::alloc::Layout) -> Int {
    dead
}

// ==================================================================
// Tests
// ==================================================================

// #[cfg(test)]
// mod tests {
//     use super::Idx;

//     #[test]
//     fn idx_sentinels() {
//         assert!(Idx::<u32>::NOT_STARTED.is_not_started());
//         assert!(!Idx::<u32>::NOT_STARTED.is_complete());
//         assert!(!Idx::<u32>::NOT_STARTED.is_valid());

//         assert!(!Idx::<u32>::COMPLETE.is_not_started());
//         assert!(Idx::<u32>::COMPLETE.is_complete());
//         assert!(!Idx::<u32>::COMPLETE.is_valid());
//     }
// }

// #[cfg(kani)]
// mod kani_proofs {
//     use super::Idx;

//     #[kani::proof]
//     fn idx_sentinels_are_distinct() {
//         let not_started: Idx<u32> = Idx::NOT_STARTED;
//         let complete: Idx<u32> = Idx::COMPLETE;

//         kani::assert(not_started.raw != complete.raw, "sentinels must differ");
//         kani::assert(!not_started.is_valid(), "NOT_STARTED is not valid");
//         kani::assert(!complete.is_valid(), "COMPLETE is not valid");
//     }

//     #[kani::proof]
//     fn is_valid_excludes_sentinels() {
//         let raw: u32 = kani::any();

//         let idx: Idx<u32> = Idx::from_raw(raw);

//         let expected_valid = raw != 0 && raw != u32::MAX;
//         kani::assert(idx.is_valid() == expected_valid, "is_valid correctness");
//     }
// }
