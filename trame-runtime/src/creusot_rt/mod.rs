#![allow(unused_imports)]

use core::{marker::PhantomData, mem};

use creusot_std::logic::FSet;
use creusot_std::macros::{ensures, pearlite, requires};
use creusot_std::model::DeepModel;
use creusot_std::prelude::{View, logic, trusted};

use crate::{
    IArena, IField, IHeap, IPointerType, IPtr, IRuntime, IShape, IShapeExtra, IShapeStore,
    IStructType, Idx,
};

/// Logical layout for creusot builds.
#[derive(Clone, Copy, Debug, DeepModel)]
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

/// A struct definition (unbounded fields).
#[derive(DeepModel, Clone)]
pub struct CStructDef {
    pub fields: Vec<CFieldDef>,
}

/// Shape definition.
#[derive(DeepModel, Clone)]
pub struct CShapeDef {
    pub layout: CLayout,
    pub def: CDef,
}

/// Shape kind.
#[derive(DeepModel, Clone)]
pub enum CDef {
    Scalar,
    Struct(CStructDef),
    Pointer(CPointerDef),
}

/// A smart-pointer definition.
#[derive(DeepModel, Clone, Copy)]
pub struct CPointerDef {
    pub pointee_handle: CShapeHandle,
    pub constructible_from_pointee: bool,
    pub known_box: bool,
}

/// Shape store (bounded).
#[derive(DeepModel, Clone)]
pub struct CShapeStore {
    pub id: u32,
    pub shapes: Vec<CShapeDef>,
}

impl CShapeStore {
    pub fn new(id: u32) -> Self {
        Self {
            id,
            shapes: Vec::new(),
        }
    }

    #[trusted]
    pub fn add(&mut self, shape: CShapeDef) -> CShapeHandle {
        let handle = CShapeHandle(self.shapes.len() as u32);
        self.shapes.push(shape);
        handle
    }

    #[trusted]
    pub fn get_def(&self, handle: CShapeHandle) -> &CShapeDef {
        self.shapes.get(handle.0 as usize).expect("invalid handle")
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

#[cfg(creusot)]
#[logic(open, inline)]
pub fn shape_is_scalar(shape: CShapeView<'_>) -> bool {
    pearlite! {
        match shape.store.shapes[shape.handle.0 as usize].def {
            CDef::Scalar => true,
            CDef::Struct(_) => false,
            CDef::Pointer(_) => false,
        }
    }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn shape_is_pointer(shape: CShapeView<'_>) -> bool {
    pearlite! {
        match shape.store.shapes[shape.handle.0 as usize].def {
            CDef::Scalar => false,
            CDef::Struct(_) => false,
            CDef::Pointer(_) => true,
        }
    }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn shape_size(shape: CShapeView<'_>) -> usize {
    pearlite! { shape.store.shapes[shape.handle.0 as usize].layout.size }
}

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

/// Pointer view.
#[derive(Clone, Copy)]
pub struct CPointerView<'a> {
    pub store: &'a CShapeStore,
    pub def: &'a CPointerDef,
}

impl<'a> IShape for CShapeView<'a> {
    type StructType = CStructView<'a>;
    type Field = CFieldView<'a>;
    type PointerType = CPointerView<'a>;

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

    fn is_pointer(&self) -> bool {
        matches!(self.store.get_def(self.handle).def, CDef::Pointer(_))
    }

    fn as_pointer(&self) -> Option<Self::PointerType> {
        match &self.store.get_def(self.handle).def {
            CDef::Pointer(def) => Some(CPointerView {
                store: self.store,
                def,
            }),
            _ => None,
        }
    }
}

impl<'a> IPointerType for CPointerView<'a> {
    type Shape = CShapeView<'a>;

    fn pointee(&self) -> Option<Self::Shape> {
        Some(self.store.view(self.def.pointee_handle))
    }

    fn constructible_from_pointee(&self) -> bool {
        self.def.constructible_from_pointee
    }

    fn is_known_box(&self) -> bool {
        self.def.known_box
    }
}

impl<'a> IStructType for CStructView<'a> {
    type Field = CFieldView<'a>;

    fn field_count(&self) -> usize {
        self.def.fields.len()
    }

    #[trusted]
    fn field(&self, idx: usize) -> Option<Self::Field> {
        self.def.fields.get(idx).map(|def| CFieldView {
            store: self.store,
            def,
        })
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

        let mut field_vec = Vec::with_capacity(fields.len());
        for &(offset, shape_handle) in fields.iter() {
            field_vec.push(CFieldDef {
                offset,
                shape_handle,
            });
        }

        Self {
            layout,
            def: CDef::Struct(CStructDef { fields: field_vec }),
        }
    }

    pub const fn pointer_to(pointee: CShapeHandle, constructible: bool, known_box: bool) -> Self {
        Self {
            layout: CLayout::new::<usize>(),
            def: CDef::Pointer(CPointerDef {
                pointee_handle: pointee,
                constructible_from_pointee: constructible,
                known_box,
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
    type Arena<T> = CArena<T>;

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

#[derive(DeepModel, Clone, Copy)]
pub struct InitRange {
    pub alloc: u32,
    pub start: usize,
    pub len: usize,
}

#[derive(DeepModel, Clone, Copy)]
pub struct PointerEdge {
    pub ptr_alloc: u32,
    pub ptr_offset: usize,
    pub ptr_shape: CShapeHandle,
    pub pointee_alloc: u32,
    pub pointee_offset: usize,
    pub pointee_shape: CShapeHandle,
}

pub struct HeapModel {
    pub live: FSet<u32>,
    pub init: FSet<InitFact>,
    pub init_ranges: FSet<InitRange>,
    pub pointer_edges: FSet<PointerEdge>,
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

#[cfg(creusot)]
#[logic]
pub fn init_range(alloc: u32, start: usize, len: usize) -> InitRange {
    InitRange { alloc, start, len }
}

#[cfg(creusot)]
#[logic]
pub fn pointer_edge(
    ptr_alloc: u32,
    ptr_offset: usize,
    ptr_shape: CShapeHandle,
    pointee_alloc: u32,
    pointee_offset: usize,
    pointee_shape: CShapeHandle,
) -> PointerEdge {
    PointerEdge {
        ptr_alloc,
        ptr_offset,
        ptr_shape,
        pointee_alloc,
        pointee_offset,
        pointee_shape,
    }
}

#[cfg(creusot)]
#[logic(open, inline)]
pub fn pointer_requires_tracked_pointee(shape: CShapeView<'_>) -> bool {
    pearlite! {
        match shape.store.shapes[shape.handle.0 as usize].def {
            CDef::Pointer(def) => def.known_box && def.constructible_from_pointee,
            _ => false,
        }
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

    unsafe fn dealloc_moved(&mut self, ptr: CPtr, shape: CShapeView<'_>) {
        let _ = (ptr, shape);
    }

    #[trusted]
    #[cfg_attr(creusot, requires(self.range_init(src, _len)))]
    #[cfg_attr(creusot, ensures(self.range_init(dst, _len)))]
    #[cfg_attr(creusot, ensures(forall<ptr2, shape2> ptr2 != dst ==> (^self).can_drop(ptr2, shape2) == (*self).can_drop(ptr2, shape2)))]
    #[cfg_attr(creusot, ensures(forall<ptr2, range2> ptr2 != dst ==> (^self).range_init(ptr2, range2) == (*self).range_init(ptr2, range2)))]
    unsafe fn memcpy(&mut self, dst: CPtr, src: CPtr, _src_shape: CShapeView<'_>, _len: usize) {
        let _ = (dst, src);
    }

    #[trusted]
    #[cfg_attr(
        creusot,
        requires(shape_is_scalar(shape) ==> self.range_init(ptr, shape_size(shape)))
    )]
    #[cfg_attr(creusot, ensures(!self.can_drop(ptr, shape)))]
    #[cfg_attr(
        creusot,
        ensures(shape_is_scalar(shape) ==> !self.range_init(ptr, shape_size(shape)))
    )]
    unsafe fn drop_in_place(&mut self, ptr: CPtr, shape: CShapeView<'_>) {
        let _ = (ptr, shape);
    }

    #[trusted]
    #[cfg_attr(creusot, ensures(result ==> self.can_drop(ptr, shape)))]
    #[cfg_attr(creusot, ensures(result ==> self.range_init(ptr, shape.size_logic())))]
    #[cfg_attr(creusot, ensures(!result ==> (^self).can_drop(ptr, shape) == (*self).can_drop(ptr, shape)))]
    #[cfg_attr(creusot, ensures(!result ==> (^self).range_init(ptr, shape.size_logic()) == (*self).range_init(ptr, shape.size_logic())))]
    #[cfg_attr(creusot, ensures(forall<ptr2, shape2> ptr2 != ptr ==> (^self).can_drop(ptr2, shape2) == (*self).can_drop(ptr2, shape2)))]
    #[cfg_attr(creusot, ensures(forall<ptr2, range2> ptr2 != ptr ==> (^self).range_init(ptr2, range2) == (*self).range_init(ptr2, range2)))]
    unsafe fn default_in_place(&mut self, ptr: CPtr, shape: CShapeView<'_>) -> bool {
        let _ = (ptr, shape);
        true
    }

    #[trusted]
    #[cfg_attr(
        creusot,
        ensures(result && pointer_requires_tracked_pointee(pointer_shape) ==> (^self)@.pointer_edges.contains(pointer_edge(
            dst.alloc_id,
            dst.offset as usize,
            pointer_shape.handle,
            src.alloc_id,
            src.offset as usize,
            pointee_shape.handle
        )))
    )]
    unsafe fn pointer_from_pointee(
        &mut self,
        dst: CPtr,
        pointer_shape: CShapeView<'_>,
        src: CPtr,
        pointee_shape: CShapeView<'_>,
    ) -> bool {
        let _ = (dst, src, pointee_shape);
        pointer_shape
            .as_pointer()
            .is_some_and(|p| p.constructible_from_pointee())
    }

    #[logic(open, inline)]
    fn can_drop(&self, ptr: CPtr, shape: CShapeView<'_>) -> bool {
        pearlite! {
            if shape_is_scalar(shape) {
                self.range_init(ptr, shape_size(shape))
            } else if shape_is_pointer(shape) {
                self@.init.contains(init_fact(
                    ptr.alloc_id,
                    ptr.offset as usize,
                    shape.handle
                ))
            } else {
                self@.init.contains(init_fact(
                    ptr.alloc_id,
                    ptr.offset as usize,
                    shape.handle
                ))
            }
        }
    }

    #[logic(open, inline)]
    fn range_init(&self, ptr: CPtr, len: usize) -> bool {
        pearlite! {
            self@.init_ranges.contains(init_range(
                ptr.alloc_id,
                ptr.offset as usize,
                len
            ))
        }
    }
}

#[cfg(creusot)]
mod proofs;

// ==================================================================
// Arena (Creusot)
// ==================================================================

#[derive(DeepModel, Clone, Copy)]
enum CSlotState {
    Empty,
    Occupied,
}

pub struct CArena<T> {
    slots: Vec<Option<T>>,
    states: Vec<CSlotState>,
    next: usize,
}

impl<T> CArena<T> {
    #[trusted]
    pub fn new() -> Self {
        // Slot 0 is reserved for NOT_STARTED.
        let mut slots = Vec::new();
        slots.push(None);
        let mut states = Vec::new();
        states.push(CSlotState::Empty);

        Self {
            slots,
            states,
            next: 1,
        }
    }
}

impl<T> Default for CArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> IArena<T> for CArena<T> {
    #[trusted]
    fn alloc(&mut self, value: T) -> Idx<T> {
        for i in self.next..self.states.len() {
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

        // No free slot: extend.
        let idx = self.states.len();
        self.slots.push(Some(value));
        self.states.push(CSlotState::Occupied);
        self.next = idx + 1;
        Idx::from_raw(idx as u32)
    }

    #[trusted]
    fn free(&mut self, id: Idx<T>) -> T {
        assert!(id.is_valid(), "cannot free sentinel index");
        let idx = id.index();
        assert!(idx < self.states.len(), "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "double-free"
        );

        self.states[idx] = CSlotState::Empty;
        self.slots[idx].take().expect("slot was occupied but empty")
    }

    #[trusted]
    #[cfg_attr(creusot, ensures(*result == self.get_logic(id)))]
    fn get(&self, id: Idx<T>) -> &T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < self.states.len(), "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "slot not occupied"
        );

        self.slots[idx]
            .as_ref()
            .expect("slot was occupied but empty")
    }

    #[trusted]
    #[cfg_attr(creusot, ensures(*result == (*self).get_logic(id)))]
    #[cfg_attr(creusot, ensures(^result == (^self).get_logic(id)))]
    #[cfg_attr(creusot, ensures((^self).contains(id)))]
    #[cfg_attr(creusot, ensures(forall<j> j != id ==> (*self).contains(j) == (^self).contains(j)))]
    #[cfg_attr(creusot, ensures(forall<j> j != id ==> (*self).get_logic(j) == (^self).get_logic(j)))]
    fn get_mut(&mut self, id: Idx<T>) -> &mut T {
        assert!(id.is_valid(), "cannot get sentinel index");
        let idx = id.index();
        assert!(idx < self.states.len(), "index out of bounds");
        assert!(
            matches!(self.states[idx], CSlotState::Occupied),
            "slot not occupied"
        );

        self.slots[idx]
            .as_mut()
            .expect("slot was occupied but empty")
    }

    #[logic(opaque)]
    fn contains(self, id: Idx<T>) -> bool {
        dead
    }

    #[logic(opaque)]
    fn get_logic(self, id: Idx<T>) -> T {
        dead
    }
}
