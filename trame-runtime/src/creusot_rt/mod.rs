use core::{marker::PhantomData, mem};

use creusot_std::logic::FSet;
use creusot_std::model::DeepModel;
use creusot_std::prelude::{View, logic, trusted};

use crate::{IArena, IField, IHeap, IPtr, IRuntime, IShape, IShapeStore, IStructType, Idx};

/// Maximum number of shapes in a store (for bounded verification).
pub const MAX_SHAPES_PER_STORE: usize = 32;

/// Maximum number of fields in a struct (for bounded verification).
pub const MAX_FIELDS_PER_STRUCT: usize = 32;

/// Logical layout for creusot builds.
#[derive(Clone, Copy, DeepModel)]
pub struct CLayout {
    pub size: usize,
    pub align: usize,
}

#[derive(Clone, Copy, Debug, DeepModel)]
pub struct CLayoutError;

impl CLayout {
    #[inline]
    pub const fn new<T>() -> Self {
        Self {
            size: mem::size_of::<T>(),
            align: mem::align_of::<T>(),
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

    pub fn from_size_align(size: usize, align: usize) -> Result<Self, CLayoutError> {
        if align == 0 || !align.is_power_of_two() {
            return Err(CLayoutError);
        }
        Ok(Self { size, align })
    }

    #[inline]
    #[trusted]
    pub fn to_layout(self) -> std::alloc::Layout {
        std::alloc::Layout::from_size_align(self.size, self.align).expect("valid layout")
    }
}

/// A handle to a shape in a store.
#[derive(DeepModel, Clone, Copy)]
pub struct CShapeHandle(pub u32);

/// A field definition (offset + shape handle).
#[derive(DeepModel, Clone, Copy)]
pub struct CFieldDef {
    pub offset: usize,
    pub shape_handle: CShapeHandle,
}

/// A struct definition (bounded array of fields).
#[derive(DeepModel, Clone, Copy)]
pub struct CStructDef {
    pub field_count: u8,
    pub fields: [CFieldDef; MAX_FIELDS_PER_STRUCT],
}

/// Shape definition.
#[derive(DeepModel, Clone, Copy)]
pub struct CShapeDef {
    pub layout: CLayout,
    pub def: CDef,
}

/// Shape kind.
#[derive(DeepModel, Clone, Copy)]
pub enum CDef {
    Scalar,
    Struct(CStructDef),
}

/// Shape store (bounded).
#[derive(DeepModel, Clone, Copy)]
pub struct CShapeStore {
    pub id: u32,
    pub shape_count: u32,
    pub shapes: [CShapeDef; MAX_SHAPES_PER_STORE],
}

impl CShapeStore {
    pub const fn new(id: u32) -> Self {
        Self {
            id,
            shape_count: 0,
            shapes: [CShapeDef {
                layout: CLayout { size: 0, align: 1 },
                def: CDef::Scalar,
            }; MAX_SHAPES_PER_STORE],
        }
    }

    #[trusted]
    pub fn add(&mut self, shape: CShapeDef) -> CShapeHandle {
        if (self.shape_count as usize) >= MAX_SHAPES_PER_STORE {
            panic!("shape store full");
        }
        let handle = CShapeHandle(self.shape_count);
        self.shapes[self.shape_count as usize] = shape;
        self.shape_count += 1;
        handle
    }

    #[trusted]
    pub fn get_def(&self, handle: CShapeHandle) -> &CShapeDef {
        if (handle.0 as usize) >= (self.shape_count as usize) {
            panic!("invalid handle");
        }
        &self.shapes[handle.0 as usize]
    }

    pub fn view(&self, handle: CShapeHandle) -> CShapeView<'_> {
        CShapeView {
            store: self,
            handle,
        }
    }
}

impl Default for CShapeStore {
    fn default() -> Self {
        Self::new(0)
    }
}

impl IShapeStore for CShapeStore {
    type Handle = CShapeHandle;
    type View<'a>
        = CShapeView<'a>
    where
        Self: 'a;

    fn get<'a>(&'a self, handle: Self::Handle) -> Self::View<'a> {
        self.view(handle)
    }
}

/// Shape view into a store.
#[derive(Clone, Copy)]
pub struct CShapeView<'a> {
    pub store: &'a CShapeStore,
    pub handle: CShapeHandle,
}

#[derive(DeepModel, Clone, Copy)]
pub struct CShapeKey {
    pub store_id: u32,
    pub handle: u32,
}

#[cfg(creusot)]
impl<'a> DeepModel for CShapeView<'a> {
    type DeepModelTy = CShapeKey;

    #[logic(open, inline)]
    fn deep_model(self) -> Self::DeepModelTy {
        CShapeKey {
            store_id: self.store.id,
            handle: self.handle.0,
        }
    }
}

impl<'a> PartialEq for CShapeView<'a> {
    #[cfg_attr(
        creusot,
        creusot_std::macros::ensures(result == (self.deep_model() == other.deep_model()))
    )]
    fn eq(&self, other: &Self) -> bool {
        self.store.id == other.store.id && self.handle.0 == other.handle.0
    }
}

impl<'a> Eq for CShapeView<'a> {}

/// Field view.
#[derive(Clone, Copy)]
pub struct CFieldView<'a> {
    pub store: &'a CShapeStore,
    pub def: &'a CFieldDef,
}

/// Struct view.
#[derive(Clone, Copy)]
pub struct CStructView<'a> {
    pub store: &'a CShapeStore,
    pub def: &'a CStructDef,
}

impl<'a> IShape for CShapeView<'a> {
    type StructType = CStructView<'a>;
    type Field = CFieldView<'a>;

    fn layout(&self) -> Option<std::alloc::Layout> {
        Some(self.store.get_def(self.handle).layout.to_layout())
    }

    fn is_struct(&self) -> bool {
        matches!(self.store.get_def(self.handle).def, CDef::Struct(_))
    }

    fn as_struct(&self) -> Option<Self::StructType> {
        match &self.store.get_def(self.handle).def {
            CDef::Struct(def) => Some(CStructView {
                store: self.store,
                def,
            }),
            _ => None,
        }
    }
}

impl<'a> IStructType for CStructView<'a> {
    type Field = CFieldView<'a>;

    fn field_count(&self) -> usize {
        self.def.field_count as usize
    }

    #[trusted]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        if idx < self.def.field_count as usize {
            Some(CFieldView {
                store: self.store,
                def: &self.def.fields[idx],
            })
        } else {
            None
        }
    }
}

impl<'a> IField for CFieldView<'a> {
    type Shape = CShapeView<'a>;

    fn offset(&self) -> usize {
        self.def.offset
    }

    fn shape(&self) -> Self::Shape {
        self.store.view(self.def.shape_handle)
    }
}

impl CShapeDef {
    pub const fn scalar(layout: CLayout) -> Self {
        Self {
            layout,
            def: CDef::Scalar,
        }
    }

    #[trusted]
    pub fn struct_with_fields(store: &CShapeStore, fields: &[(usize, CShapeHandle)]) -> Self {
        if fields.len() > MAX_FIELDS_PER_STRUCT {
            panic!("too many fields");
        }

        let mut size = 0usize;
        let mut align = 1usize;
        for &(offset, shape_handle) in fields {
            let field_layout = store.get_def(shape_handle).layout;
            align = align.max(field_layout.align());
            let field_end = offset + field_layout.size();
            size = size.max(field_end);
        }

        size = (size + align - 1) & !(align - 1);
        let layout = CLayout::from_size_align(size, align).expect("valid layout");

        let mut field_array = [CFieldDef {
            offset: 0,
            shape_handle: CShapeHandle(0),
        }; MAX_FIELDS_PER_STRUCT];
        for (i, &(offset, shape_handle)) in fields.iter().enumerate() {
            field_array[i] = CFieldDef {
                offset,
                shape_handle,
            };
        }

        Self {
            layout,
            def: CDef::Struct(CStructDef {
                field_count: fields.len() as u8,
                fields: field_array,
            }),
        }
    }
}

/// Creusot runtime.
pub struct CRuntime<'s> {
    _marker: PhantomData<&'s CShapeStore>,
}

impl<'s> IRuntime for CRuntime<'s> {
    type Shape = CShapeView<'s>;
    type Heap = CHeap;
    type Arena<T> = CArena<T, MAX_CARENA_SLOTS>;

    fn heap() -> Self::Heap {
        CHeap::new()
    }

    fn arena<T>() -> Self::Arena<T> {
        CArena::new()
    }
}

// ==================================================================
// Pointer (Creusot)
// ==================================================================

#[derive(Clone, Copy)]
pub struct CPtr {
    pub alloc_id: u32,
    pub offset: u32,
    pub size: u32,
}

impl IPtr for CPtr {
    #[inline]
    unsafe fn byte_add(self, n: usize) -> Self {
        CPtr::offset(self, n)
    }
}

impl CPtr {
    #[inline]
    pub const fn new(alloc_id: u32, size: u32) -> Self {
        Self {
            alloc_id,
            offset: 0,
            size,
        }
    }

    #[inline]
    #[trusted]
    pub fn offset(self, n: usize) -> Self {
        let new_offset = self.offset.checked_add(n as u32).expect("offset overflow");
        if new_offset > self.size {
            panic!("pointer arithmetic out of bounds");
        }
        Self {
            alloc_id: self.alloc_id,
            offset: new_offset,
            size: self.size,
        }
    }

    #[inline]
    pub const fn offset_bytes(self) -> usize {
        self.offset as usize
    }

    #[inline]
    pub const fn alloc_id(self) -> u32 {
        self.alloc_id
    }
}

// ==================================================================
// Heap (Creusot)
// ==================================================================

#[derive(DeepModel, Clone, Copy)]
pub struct InitFact {
    pub alloc: u32,
    pub offset: usize,
    pub shape: CShapeHandle,
}

pub struct HeapModel {
    pub live: FSet<u32>,
    pub init: FSet<InitFact>,
}

pub struct CHeap {
    next_id: u32,
}

impl CHeap {
    pub const fn new() -> Self {
        Self { next_id: 1 }
    }
}

#[cfg(creusot)]
impl View for CHeap {
    type ViewTy = HeapModel;

    #[logic(opaque)]
    fn view(self) -> Self::ViewTy {
        dead
    }
}

#[cfg(creusot)]
#[logic]
pub fn init_fact(alloc: u32, offset: usize, shape: CShapeHandle) -> InitFact {
    InitFact {
        alloc,
        offset,
        shape,
    }
}

impl IHeap<CShapeView<'_>> for CHeap {
    type Ptr = CPtr;

    #[trusted]
    unsafe fn alloc(&mut self, shape: CShapeView<'_>) -> CPtr {
        let layout = shape.layout().expect("IShape requires sized types");
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        CPtr::new(id, layout.size() as u32)
    }

    #[trusted]
    unsafe fn dealloc(&mut self, ptr: CPtr, _shape: CShapeView<'_>) {
        let _ = ptr;
    }

    #[trusted]
    unsafe fn memcpy(&mut self, dst: CPtr, src: CPtr, _len: usize) {
        let _ = (dst, src);
    }

    #[trusted]
    #[cfg_attr(
        creusot,
        creusot_std::macros::requires(
            self@.init.contains(init_fact(
                ptr.alloc_id,
                ptr.offset as usize,
                shape.handle
            ))
        )
    )]
    unsafe fn drop_in_place(&mut self, ptr: CPtr, shape: CShapeView<'_>) {
        let _ = (ptr, shape);
    }

    #[trusted]
    unsafe fn default_in_place(&mut self, ptr: CPtr, shape: CShapeView<'_>) -> bool {
        let _ = (ptr, shape);
        true
    }
}

// ==================================================================
// Arena (Creusot)
// ==================================================================

pub const MAX_CARENA_SLOTS: usize = 32;

#[derive(DeepModel, Clone, Copy)]
enum CSlotState {
    Empty,
    Occupied,
}

pub struct CArena<T, const N: usize = MAX_CARENA_SLOTS> {
    slots: [Option<T>; N],
    states: [CSlotState; N],
    next: usize,
}

impl<T, const N: usize> CArena<T, N> {
    #[trusted]
    pub fn new() -> Self {
        Self {
            slots: core::array::from_fn(|_| None),
            states: [CSlotState::Empty; N],
            next: 1,
        }
    }
}

impl<T, const N: usize> Default for CArena<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> IArena<T> for CArena<T, N> {
    #[trusted]
    fn alloc(&mut self, value: T) -> Idx<T> {
        for i in self.next..N {
            if matches!(self.states[i], CSlotState::Empty) {
                self.slots[i] = Some(value);
                self.states[i] = CSlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        for i in 1..self.next {
            if matches!(self.states[i], CSlotState::Empty) {
                self.slots[i] = Some(value);
                self.states[i] = CSlotState::Occupied;
                self.next = i + 1;
                return Idx::from_raw(i as u32);
            }
        }
        panic!("arena full");
    }

    #[trusted]
    fn free(&mut self, id: Idx<T>) -> T {
        assert!(id.is_valid(), "cannot free sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "double-free"
        );

        self.states[idx] = CSlotState::Empty;
        self.slots[idx].take().expect("slot was occupied but empty")
    }

    #[trusted]
    fn get(&self, id: Idx<T>) -> &T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "slot not occupied"
        );

        self.slots[idx]
            .as_ref()
            .expect("slot was occupied but empty")
    }

    #[trusted]
    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < N, "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "slot not occupied"
        );

        self.slots[idx]
            .as_mut()
            .expect("slot was occupied but empty")
    }
}
