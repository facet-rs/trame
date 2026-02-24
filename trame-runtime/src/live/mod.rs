//! Live implementations of all of trame's runtime traits: no verification involved, real memory
//! allocations, etc.

#[cfg(creusot)]
use crate::MemState;
use crate::{
    CopyDesc, EnumDiscriminantRepr, EnumReprKind, IArena, IEnumType, IExecShape, IField, IHeap,
    IListType, IMapType, IPointerType, IRuntime, ISetType, IShape, IShapeStore, IStructType,
    IVariantType, Idx,
};
use core::marker::PhantomData;
#[cfg(creusot)]
use creusot_std::macros::{logic, trusted};
use facet_core::{
    Def, EnumRepr, EnumType, Field, KnownPointer, ListDef, MapDef, PointerDef, PtrMut, PtrUninit,
    SetDef, Shape, StructType, Type, UserType, Variant,
};

#[cfg(creusot)]
#[trusted]
unsafe fn call_pointer_new_into(
    f: unsafe fn(PtrUninit, PtrMut) -> PtrMut,
    dst: *mut u8,
    src: *mut u8,
) {
    let _ = unsafe { f(PtrUninit::new(dst), PtrMut::new(src)) };
}

#[cfg(creusot)]
#[trusted]
fn pointer_pointee_matches(pointer_def: PointerDef, pointee_shape: &'static Shape) -> bool {
    pointer_def.pointee() == Some(pointee_shape)
}

/// A "live" runtime that just peforms raw unsafe Rust operations
pub struct LRuntime<S = &'static Shape> {
    _shape: PhantomData<fn() -> S>,
}

impl LRuntime<&'static Shape> {
    /// Build the default live heap over facet's static shapes.
    pub fn heap() -> LHeap {
        LHeap::new()
    }

    /// Build the default live arena over facet's static shapes.
    pub fn arena<T>() -> LArena<T> {
        LArena::new()
    }
}

impl<S> IRuntime for LRuntime<S>
where
    S: IExecShape<*mut u8>,
{
    type Shape = S;
    type Heap = LHeap;
    type Arena<T> = LArena<T>;

    fn heap() -> Self::Heap {
        LHeap::new()
    }

    fn arena<T>() -> Self::Arena<T> {
        LArena::new()
    }
}

// ==================================================================
// Shape
// ==================================================================

/// Live shape store: handle and view are the static shape itself.
#[derive(Clone, Copy, Default)]
pub struct LShapeStore;

impl IShapeStore for LShapeStore {
    type Handle = &'static Shape;
    type View<'a>
        = &'static Shape
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        handle
    }
}

impl IShape for &'static Shape {
    type StructType = &'static StructType;
    type Field = &'static Field;
    type EnumType = &'static EnumType;
    type PointerType = PointerDef;
    type ListType = ListDef;
    type SetType = SetDef;
    type MapType = MapDef;

    #[inline]
    fn layout(&self) -> Option<std::alloc::Layout> {
        self.layout.sized_layout().ok()
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
    fn as_enum(&self) -> Option<Self::EnumType> {
        match &self.ty {
            Type::User(UserType::Enum(e)) => Some(e),
            _ => None,
        }
    }

    #[inline]
    fn enum_repr_kind(&self) -> Option<EnumReprKind> {
        if !matches!(self.ty, Type::User(UserType::Enum(_))) {
            return None;
        }

        let tag = self.get_tag_attr();
        let content = self.get_content_attr();
        let untagged = self.is_untagged();
        let repr = match (tag, content, untagged) {
            (_, _, true) => EnumReprKind::Flattened,
            (Some(t), Some(c), false) => EnumReprKind::AdjacentlyTagged { tag: t, content: c },
            (Some(t), None, false) => EnumReprKind::InternallyTagged { tag: t },
            _ => EnumReprKind::ExternallyTagged,
        };
        Some(repr)
    }

    #[inline]
    fn enum_discriminant_repr(&self) -> Option<EnumDiscriminantRepr> {
        let en = self.as_enum()?;
        let repr = match en.enum_repr {
            EnumRepr::RustNPO => EnumDiscriminantRepr::RustNpo,
            EnumRepr::Rust => EnumDiscriminantRepr::RustNpo,
            EnumRepr::U8 => EnumDiscriminantRepr::U8,
            EnumRepr::U16 => EnumDiscriminantRepr::U16,
            EnumRepr::U32 => EnumDiscriminantRepr::U32,
            EnumRepr::U64 => EnumDiscriminantRepr::U64,
            EnumRepr::USize => EnumDiscriminantRepr::Usize,
            EnumRepr::I8 => EnumDiscriminantRepr::I8,
            EnumRepr::I16 => EnumDiscriminantRepr::I16,
            EnumRepr::I32 => EnumDiscriminantRepr::I32,
            EnumRepr::I64 => EnumDiscriminantRepr::I64,
            EnumRepr::ISize => EnumDiscriminantRepr::Isize,
        };
        Some(repr)
    }

    #[inline]
    fn enum_variant_discriminant(&self, variant_idx: usize) -> Option<i64> {
        let en = self.as_enum()?;
        let variant = en.variant(variant_idx)?;
        variant.discriminant
    }

    #[inline]
    fn type_identifier(&self) -> &'static str {
        self.type_identifier
    }

    #[inline]
    fn option_payload(&self) -> Option<Self> {
        match self.def {
            Def::Option(opt) => Some(opt.t),
            _ => None,
        }
    }

    #[inline]
    fn is_pointer(&self) -> bool {
        matches!(self.def, Def::Pointer(_))
    }

    #[inline]
    fn as_pointer(&self) -> Option<Self::PointerType> {
        match self.def {
            Def::Pointer(def) => Some(def),
            _ => None,
        }
    }

    #[inline]
    fn as_list(&self) -> Option<Self::ListType> {
        match self.def {
            Def::List(def) => Some(def),
            _ => None,
        }
    }

    #[inline]
    fn array_len(&self) -> Option<usize> {
        match self.def {
            Def::Array(def) => Some(def.n),
            _ => None,
        }
    }

    #[inline]
    fn array_element(&self) -> Option<Self> {
        match self.def {
            Def::Array(def) => Some(def.t()),
            _ => None,
        }
    }

    #[inline]
    fn sequence_element(&self) -> Option<Self> {
        match self.def {
            Def::List(def) => Some(def.t()),
            Def::Array(def) => Some(def.t()),
            Def::Slice(def) => Some(def.t()),
            _ => None,
        }
    }

    #[inline]
    fn as_set(&self) -> Option<Self::SetType> {
        match self.def {
            Def::Set(def) => Some(def),
            _ => None,
        }
    }

    #[inline]
    fn as_map(&self) -> Option<Self::MapType> {
        match self.def {
            Def::Map(def) => Some(def),
            _ => None,
        }
    }
}

impl IListType for ListDef {
    type Shape = &'static Shape;

    #[inline]
    fn element(&self) -> Self::Shape {
        self.t()
    }

    #[inline]
    unsafe fn init_in_place_with_capacity(&self, dst: *mut u8, capacity: usize) -> bool {
        let Some(init) = ListDef::init_in_place_with_capacity(self) else {
            return false;
        };
        unsafe {
            init(PtrUninit::new(dst), capacity);
        }
        true
    }

    #[inline]
    unsafe fn push_element(&self, list_ptr: *mut u8, elem_ptr: *mut u8) -> bool {
        let Some(push) = ListDef::push(self) else {
            return false;
        };
        unsafe {
            push(PtrMut::new(list_ptr), PtrMut::new(elem_ptr));
        }
        true
    }
}

impl IMapType for MapDef {
    type Shape = &'static Shape;

    #[inline]
    fn key(&self) -> Self::Shape {
        self.k()
    }

    #[inline]
    fn value(&self) -> Self::Shape {
        self.v()
    }

    #[inline]
    unsafe fn init_in_place_with_capacity(&self, dst: *mut u8, capacity: usize) -> bool {
        unsafe {
            (self.vtable.init_in_place_with_capacity)(PtrUninit::new(dst), capacity);
        }
        true
    }

    #[inline]
    unsafe fn insert_entry(&self, map_ptr: *mut u8, key_ptr: *mut u8, value_ptr: *mut u8) -> bool {
        unsafe {
            (self.vtable.insert)(
                PtrMut::new(map_ptr),
                PtrMut::new(key_ptr),
                PtrMut::new(value_ptr),
            );
        }
        true
    }
}

impl ISetType for SetDef {
    type Shape = &'static Shape;

    #[inline]
    fn element(&self) -> Self::Shape {
        self.t()
    }

    #[inline]
    unsafe fn init_in_place_with_capacity(&self, dst: *mut u8, capacity: usize) -> bool {
        unsafe {
            (self.vtable.init_in_place_with_capacity)(PtrUninit::new(dst), capacity);
        }
        true
    }

    #[inline]
    unsafe fn insert_element(&self, set_ptr: *mut u8, elem_ptr: *mut u8) -> bool {
        unsafe {
            let _ = (self.vtable.insert)(PtrMut::new(set_ptr), PtrMut::new(elem_ptr));
        }
        true
    }
}

impl IPointerType for PointerDef {
    type Shape = &'static Shape;

    #[inline]
    fn pointee(&self) -> Option<Self::Shape> {
        self.pointee()
    }

    #[inline]
    fn constructible_from_pointee(&self) -> bool {
        self.constructible_from_pointee()
    }

    #[inline]
    fn supports_slice_builder(&self) -> bool {
        self.vtable.slice_builder_vtable.is_some()
    }

    #[inline]
    unsafe fn slice_builder_new(&self) -> Option<*mut u8> {
        let slice_builder = self.vtable.slice_builder_vtable?;
        Some(unsafe { (slice_builder.new_fn)().as_ptr::<u8>() as *mut u8 })
    }

    #[inline]
    unsafe fn slice_builder_push(&self, builder_ptr: *mut u8, item_ptr: *mut u8) -> bool {
        let Some(slice_builder) = self.vtable.slice_builder_vtable else {
            return false;
        };
        unsafe {
            (slice_builder.push_fn)(PtrMut::new(builder_ptr), PtrMut::new(item_ptr));
        }
        true
    }

    #[inline]
    unsafe fn slice_builder_convert(&self, builder_ptr: *mut u8) -> Option<*mut u8> {
        let slice_builder = self.vtable.slice_builder_vtable?;
        let out = unsafe { (slice_builder.convert_fn)(PtrMut::new(builder_ptr)) };
        Some(unsafe { out.as_ptr::<u8>() as *mut u8 })
    }

    #[inline]
    unsafe fn slice_builder_free(&self, builder_ptr: *mut u8) {
        let Some(slice_builder) = self.vtable.slice_builder_vtable else {
            return;
        };
        unsafe {
            (slice_builder.free_fn)(PtrMut::new(builder_ptr));
        }
    }

    #[inline]
    fn is_known_box(&self) -> bool {
        matches!(self.known, Some(KnownPointer::Box))
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

impl IEnumType for &'static EnumType {
    type StructType = &'static StructType;
    type Variant = &'static Variant;

    #[inline]
    fn variant_count(&self) -> usize {
        self.variants.len()
    }

    #[inline]
    fn variant(&self, idx: usize) -> Option<Self::Variant> {
        self.variants.get(idx)
    }
}

impl IVariantType for &'static Variant {
    type StructType = &'static StructType;

    #[inline]
    fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    fn effective_name(&self) -> &'static str {
        Variant::effective_name(self)
    }

    #[inline]
    fn data(&self) -> Self::StructType {
        &self.data
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

    #[inline]
    fn name(&self) -> &'static str {
        self.name
    }

    #[inline]
    fn effective_name(&self) -> &'static str {
        Field::effective_name(self)
    }

    #[inline]
    fn alias(&self) -> Option<&'static str> {
        self.alias
    }

    #[inline]
    fn is_flattened(&self) -> bool {
        Field::is_flattened(self)
    }

    #[inline]
    fn has_default(&self) -> bool {
        Field::has_default(self)
    }

    #[inline]
    fn should_skip_deserializing(&self) -> bool {
        Field::should_skip_deserializing(self)
    }
}

impl IExecShape<*mut u8> for &'static Shape {
    #[inline]
    fn needs_drop(&self) -> bool {
        true
    }

    #[inline]
    unsafe fn drop_in_place(&self, ptr: *mut u8) {
        unsafe {
            self.call_drop_in_place(PtrMut::new(ptr));
        }
    }

    #[inline]
    unsafe fn default_in_place(&self, ptr: *mut u8) -> bool {
        unsafe {
            self.call_default_in_place(PtrMut::new(ptr).into())
                .is_some()
        }
    }

    #[inline]
    unsafe fn pointer_from_pointee(&self, dst: *mut u8, src: *mut u8, pointee_shape: Self) -> bool {
        let Def::Pointer(pointer_def) = self.def else {
            return false;
        };
        #[cfg(creusot)]
        if !pointer_pointee_matches(pointer_def, pointee_shape) {
            return false;
        }
        #[cfg(not(creusot))]
        if pointer_def.pointee() != Some(pointee_shape) {
            return false;
        }
        let Some(new_into_fn) = pointer_def.vtable.new_into_fn else {
            return false;
        };
        unsafe {
            #[cfg(creusot)]
            call_pointer_new_into(new_into_fn, dst, src);
            #[cfg(not(creusot))]
            new_into_fn(PtrUninit::new(dst), PtrMut::new(src));
        }
        true
    }
}

// ==================================================================
// Heap
// ==================================================================

/// Live heap that performs actual memory operations.
#[derive(Debug)]
pub struct LHeap;

impl LHeap {
    /// Create a new live heap.
    pub const fn new() -> Self {
        Self
    }

    fn repeat_layout<S: IExecShape<*mut u8>>(
        elem_shape: S,
        count: usize,
    ) -> Option<std::alloc::Layout> {
        let elem_layout = elem_shape.layout()?;
        if elem_layout.size() == 0 || count == 0 {
            return None;
        }
        let total_size = elem_layout
            .size()
            .checked_mul(count)
            .expect("repeat allocation size overflow");
        std::alloc::Layout::from_size_align(total_size, elem_layout.align()).ok()
    }
}

impl Default for LHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl<S: IExecShape<*mut u8>> IHeap<S> for LHeap {
    type Ptr = *mut u8;

    unsafe fn alloc(&mut self, shape: S) -> *mut u8 {
        let layout = shape.layout();
        if let Some(layout) = layout {
            if layout.size() == 0 {
                // "Note that layouts are not required to have non-zero size, even though
                // GlobalAlloc requires that all memory requests be non-zero in size. A caller
                // must either ensure that conditions like this are met, use specific allocators
                // with looser requirements, or use the more lenient Allocator interface"
                // (std::alloc::Layout)
                // https://doc.rust-lang.org/stable/std/alloc/struct.Layout.html
                // ZST - no allocation required
                core::ptr::NonNull::dangling().as_ptr()
            } else {
                // SAFETY: layout.size() > 0
                let ptr = unsafe { std::alloc::alloc(layout) };
                if ptr.is_null() {
                    std::alloc::handle_alloc_error(layout);
                }
                ptr
            }
        } else {
            // No layout available (e.g., ZST/unsized) - return dangling.
            core::ptr::NonNull::dangling().as_ptr()
        }
    }

    unsafe fn dealloc(&mut self, ptr: *mut u8, shape: S) {
        if let Some(layout) = shape.layout() {
            if layout.size() > 0 {
                // SAFETY: caller guarantees this is a live allocation
                unsafe { std::alloc::dealloc(ptr, layout) };
            }
        }
    }

    unsafe fn dealloc_moved(&mut self, ptr: *mut u8, shape: S) {
        unsafe { self.dealloc(ptr, shape) };
    }

    unsafe fn alloc_repeat(&mut self, elem_shape: S, count: usize) -> *mut u8 {
        if let Some(layout) = Self::repeat_layout(elem_shape, count) {
            // SAFETY: layout.size() > 0
            let ptr = unsafe { std::alloc::alloc(layout) };
            if ptr.is_null() {
                std::alloc::handle_alloc_error(layout);
            }
            ptr
        } else {
            core::ptr::NonNull::dangling().as_ptr()
        }
    }

    unsafe fn dealloc_repeat(&mut self, ptr: *mut u8, elem_shape: S, count: usize) {
        if let Some(layout) = Self::repeat_layout(elem_shape, count) {
            // SAFETY: caller guarantees this is the original live allocation.
            unsafe { std::alloc::dealloc(ptr, layout) };
        }
    }

    unsafe fn dealloc_repeat_moved(&mut self, ptr: *mut u8, elem_shape: S, count: usize) {
        unsafe { self.dealloc_repeat(ptr, elem_shape, count) };
    }

    unsafe fn memcpy(&mut self, dst: *mut u8, src: *mut u8, desc: CopyDesc<S>) {
        let len = desc.byte_len();
        if len > 0 {
            // SAFETY: caller guarantees non-overlapping, valid pointers
            unsafe {
                core::ptr::copy_nonoverlapping(src, dst, len);
            }
        }
    }

    unsafe fn drop_in_place(&mut self, ptr: *mut u8, shape: S) {
        if shape.needs_drop() {
            unsafe { shape.drop_in_place(ptr) };
        }
    }

    unsafe fn default_in_place(&mut self, ptr: *mut u8, shape: S) -> bool {
        unsafe { shape.default_in_place(ptr) }
    }

    unsafe fn pointer_from_pointee(
        &mut self,
        dst: *mut u8,
        pointer_shape: S,
        src: *mut u8,
        pointee_shape: S,
    ) -> bool {
        unsafe { pointer_shape.pointer_from_pointee(dst, src, pointee_shape) }
    }

    unsafe fn pointer_slice_builder_new(&mut self, pointer_shape: S) -> Option<*mut u8> {
        let pointer = pointer_shape.as_pointer()?;
        unsafe { pointer.slice_builder_new() }
    }

    unsafe fn pointer_slice_builder_push(
        &mut self,
        builder_ptr: *mut u8,
        pointer_shape: S,
        item_ptr: *mut u8,
        item_shape: S,
    ) -> bool {
        let Some(pointer) = pointer_shape.as_pointer() else {
            return false;
        };
        let Some(pointee) = pointer.pointee() else {
            return false;
        };
        let Some(expected_item_shape) = pointee.sequence_element() else {
            return false;
        };
        if expected_item_shape != item_shape {
            return false;
        }
        unsafe { pointer.slice_builder_push(builder_ptr, item_ptr) }
    }

    unsafe fn pointer_slice_builder_convert_into(
        &mut self,
        dst: *mut u8,
        pointer_shape: S,
        builder_ptr: *mut u8,
    ) -> bool {
        let Some(pointer) = pointer_shape.as_pointer() else {
            return false;
        };
        let Some(layout) = pointer_shape.layout() else {
            return false;
        };
        let Some(src_ptr) = (unsafe { pointer.slice_builder_convert(builder_ptr) }) else {
            return false;
        };
        unsafe {
            if layout.size() > 0 {
                core::ptr::copy_nonoverlapping(src_ptr, dst, layout.size());
                std::alloc::dealloc(src_ptr, layout);
            }
        }
        true
    }

    unsafe fn pointer_slice_builder_free(&mut self, pointer_shape: S, builder_ptr: *mut u8) {
        let Some(pointer) = pointer_shape.as_pointer() else {
            return;
        };
        unsafe { pointer.slice_builder_free(builder_ptr) }
    }

    unsafe fn select_enum_variant(
        &mut self,
        dst: *mut u8,
        enum_shape: S,
        variant_idx: usize,
    ) -> bool {
        let Some(repr) = enum_shape.enum_discriminant_repr() else {
            return false;
        };
        let Some(discriminant) = enum_shape.enum_variant_discriminant(variant_idx) else {
            return false;
        };

        unsafe {
            match repr {
                EnumDiscriminantRepr::RustNpo => return false,
                EnumDiscriminantRepr::U8 => *dst.cast::<u8>() = discriminant as u8,
                EnumDiscriminantRepr::U16 => *dst.cast::<u16>() = discriminant as u16,
                EnumDiscriminantRepr::U32 => *dst.cast::<u32>() = discriminant as u32,
                EnumDiscriminantRepr::U64 => *dst.cast::<u64>() = discriminant as u64,
                EnumDiscriminantRepr::Usize => *dst.cast::<usize>() = discriminant as usize,
                EnumDiscriminantRepr::I8 => *dst.cast::<i8>() = discriminant as i8,
                EnumDiscriminantRepr::I16 => *dst.cast::<i16>() = discriminant as i16,
                EnumDiscriminantRepr::I32 => *dst.cast::<i32>() = discriminant as i32,
                EnumDiscriminantRepr::I64 => *dst.cast::<i64>() = discriminant,
                EnumDiscriminantRepr::Isize => *dst.cast::<isize>() = discriminant as isize,
            }
        }
        true
    }

    unsafe fn list_init_in_place_with_capacity(
        &mut self,
        dst: *mut u8,
        list_shape: S,
        capacity: usize,
    ) -> bool {
        let Some(list) = list_shape.as_list() else {
            return false;
        };
        unsafe { list.init_in_place_with_capacity(dst, capacity) }
    }

    unsafe fn list_push_element(
        &mut self,
        list_ptr: *mut u8,
        list_shape: S,
        elem_ptr: *mut u8,
        elem_shape: S,
    ) -> bool {
        let Some(list) = list_shape.as_list() else {
            return false;
        };
        let elem_matches = list.element() == elem_shape;
        if !elem_matches {
            return false;
        }
        unsafe { list.push_element(list_ptr, elem_ptr) }
    }

    unsafe fn set_init_in_place_with_capacity(
        &mut self,
        dst: *mut u8,
        set_shape: S,
        capacity: usize,
    ) -> bool {
        let Some(set) = set_shape.as_set() else {
            return false;
        };
        unsafe { set.init_in_place_with_capacity(dst, capacity) }
    }

    unsafe fn set_insert_element(
        &mut self,
        set_ptr: *mut u8,
        set_shape: S,
        elem_ptr: *mut u8,
        elem_shape: S,
    ) -> bool {
        let Some(set) = set_shape.as_set() else {
            return false;
        };
        if set.element() != elem_shape {
            return false;
        }
        unsafe { set.insert_element(set_ptr, elem_ptr) }
    }

    unsafe fn map_init_in_place_with_capacity(
        &mut self,
        dst: *mut u8,
        map_shape: S,
        capacity: usize,
    ) -> bool {
        let Some(map) = map_shape.as_map() else {
            return false;
        };
        unsafe { map.init_in_place_with_capacity(dst, capacity) }
    }

    unsafe fn map_insert_entry(
        &mut self,
        map_ptr: *mut u8,
        map_shape: S,
        key_ptr: *mut u8,
        key_shape: S,
        value_ptr: *mut u8,
        value_shape: S,
    ) -> bool {
        let Some(map) = map_shape.as_map() else {
            return false;
        };
        if map.key() != key_shape || map.value() != value_shape {
            return false;
        }
        unsafe { map.insert_entry(map_ptr, key_ptr, value_ptr) }
    }

    #[cfg(creusot)]
    #[logic(opaque)]
    fn is(&self, _state: MemState, _ptr: Self::Ptr, _shape: S) -> bool {
        dead
    }
}

// ==================================================================
// Arena
// ==================================================================

/// Vec-based arena with free list for production use.
pub struct LArena<T> {
    slots: Vec<Option<T>>,
    free_list: Vec<u32>,
}

impl<T> LArena<T> {
    /// Create a new live arena.
    pub fn new() -> Self {
        #[cfg(creusot)]
        let mut slots = {
            let mut v = Vec::new();
            v.push(None);
            v
        };
        #[cfg(not(creusot))]
        let slots = vec![None];
        Self {
            slots, // Slot 0 reserved for NOT_STARTED
            free_list: Vec::new(),
        }
    }
}

impl<T> Default for LArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IArena<T> for LArena<T> {
    fn alloc(&mut self, value: T) -> Idx<T> {
        let raw = if let Some(idx) = self.free_list.pop() {
            debug_assert!(self.slots[idx as usize].is_none());
            self.slots[idx as usize] = Some(value);
            idx
        } else {
            let idx = self.slots.len();
            assert!(idx < u32::MAX as usize, "arena full");
            self.slots.push(Some(value));
            idx as u32
        };
        Idx::from_raw(raw)
    }

    fn free(&mut self, id: Idx<T>) -> T {
        debug_assert!(id.is_valid());
        let value = self.slots[id.index()].take().expect("double-free");
        self.free_list.push(id.raw);
        value
    }

    fn get(&self, id: Idx<T>) -> &T {
        debug_assert!(id.is_valid());
        self.slots[id.index()].as_ref().expect("slot empty")
    }

    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        debug_assert!(id.is_valid());
        self.slots[id.index()].as_mut().expect("slot empty")
    }

    #[cfg(creusot)]
    #[logic(opaque)]
    fn contains(self, _id: Idx<T>) -> bool {
        dead
    }

    #[cfg(creusot)]
    #[logic(opaque)]
    fn get_logic(self, _id: Idx<T>) -> T {
        dead
    }
}

#[cfg(test)]
mod tests;
