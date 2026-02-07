//! Live implementations of all of trame's runtime traits: no verification involved, real memory
//! allocations, etc.

use crate::{
    CopyDesc, IArena, IExecShape, IField, IHeap, IPointerType, IRuntime, IShape, IShapeStore,
    IStructType, Idx,
};
use core::marker::PhantomData;
use facet_core::{
    Def, Field, KnownPointer, PointerDef, PtrMut, PtrUninit, Shape, StructType, Type, UserType,
};

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
    type PointerType = PointerDef;

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
        if pointer_def.pointee() != Some(pointee_shape) {
            return false;
        }
        let Some(new_into_fn) = pointer_def.vtable.new_into_fn else {
            return false;
        };
        unsafe {
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
        Self {
            slots: vec![None], // Slot 0 reserved for NOT_STARTED
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
}

#[cfg(test)]
mod tests;
