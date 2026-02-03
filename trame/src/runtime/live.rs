//! Live implementations of all of trame's runtime traits: no verification involved, real memory
//! allocations, etc.

use facet_core::{Field, Shape, StructType, Type, UserType};

use crate::runtime::{IField, IShape, IShapeStore, IStructType};

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

    #[inline]
    fn layout(&self) -> Option<std::alloc::Layout> {
        self.layout.sized_layout()
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

// ==================================================================
// Heap
// ==================================================================

// TODO: import from arena.rs + ptr.rs, rename to LHeap

// ==================================================================
// Arena
// ==================================================================

// TODO: import from arena.rs, rename to LArena
