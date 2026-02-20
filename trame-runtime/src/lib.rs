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

pub trait IShape: Copy + PartialEq + IShapeExtra {
    /// The struct type returned by `as_struct()`.
    type StructType: IStructType<Field = Self::Field>;

    /// The field type used by struct types.
    type Field: IField<Shape = Self>;

    /// The enum type returned by `as_enum()`.
    type EnumType: IEnumType<StructType = Self::StructType>;

    /// The smart-pointer metadata returned by `as_pointer()`.
    type PointerType: IPointerType<Shape = Self>;

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

    /// Whether this shape supports partial enum-path construction.
    ///
    /// Live facet-core enums currently use safe whole-value initialization only.
    /// Verified shapes can opt into path-based enum staging.
    fn supports_partial_enum_paths(&self) -> bool {
        false
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
