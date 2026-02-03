//! Verified implementations of all of trame's runtime traits: storage is heavily bounded, all
//! operations are checked, we track a lot of things.

use core::cell::UnsafeCell;
use std::alloc::Layout;

mod byte_range;
use byte_range::{ByteRangeError, ByteRangeTracker};

use crate::{
    IRuntime,
    node::Node,
    runtime::{IArena, IField, IHeap, IPtr, IShape, IShapeStore, IStructType, Idx},
};

/// A runtime that verifies all operations
pub struct VRuntime;

struct VShapeStoreCell(UnsafeCell<VShapeStore>);

// SAFETY: Verified runtime is single-threaded. Concurrent access is undefined behavior.
unsafe impl Sync for VShapeStoreCell {}

static VSHAPE_STORE: VShapeStoreCell = VShapeStoreCell(UnsafeCell::new(VShapeStore::new()));

pub fn vshape_store() -> &'static VShapeStore {
    unsafe { &*VSHAPE_STORE.0.get() }
}

/// Register a new verified shape in the global store.
pub fn vshape_register(shape: VShapeDef) -> VShapeHandle {
    unsafe { (&mut *VSHAPE_STORE.0.get()).add(shape) }
}

/// View a previously registered shape from the global store.
pub fn vshape_view(handle: VShapeHandle) -> VShapeView<'static, VShapeStore> {
    unsafe { (&*VSHAPE_STORE.0.get()).view(handle) }
}

impl IRuntime for VRuntime {
    type Shape = VShapeView<'static, VShapeStore>;

    type Heap = VHeap<Self::Shape>;

    type Arena = VArena<Node<Self::Heap, Self::Shape>, MAX_VARENA_SLOTS>;

    fn heap() -> Self::Heap {
        VHeap::new()
    }

    fn arena() -> Self::Arena {
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

/// A handle to a shape in a DynShapeStore.
///
/// This is just an index into the store's shape array.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VShapeHandle(pub u8);

/// A synthetic field for Kani verification.
///
/// Fields reference their type's shape by handle (index) rather than
/// containing the shape directly. This avoids recursive type definitions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VFieldDef {
    /// Byte offset of this field within the struct.
    pub offset: usize,

    /// Handle to the shape of this field's type.
    pub shape_handle: VShapeHandle,
}

/// A synthetic struct type for Kani verification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VStructDef {
    /// Number of fields.
    pub field_count: u8,

    /// Field information (only first `field_count` entries are valid).
    pub fields: [VFieldDef; MAX_FIELDS_PER_STRUCT],
}

/// A bounded shape definition for Kani verification.
///
/// Unlike `facet_core::Shape` which uses static references and can be recursive,
/// these shapes are bounded and can implement `kani::Arbitrary`.
///
/// Shape definitions live in a `DynShapeStore` and are referenced by handle.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VShapeDef {
    /// Layout of this type.
    pub layout: Layout,
    ///
    /// Type-specific information.
    pub def: VDef,
}

/// Type-specific definition for verified shapes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VDef {
    /// A scalar type (no internal structure to track).
    Scalar,
    /// A struct with indexed fields.
    Struct(VStructDef),
    // TODO: Enum, Option, Result, List, Map, etc.
}

/// A store of DynShape definitions.
///
/// Shapes are stored in an array and referenced by index (DynShapeHandle).
/// This allows shapes to reference other shapes (nested structs) without
/// creating recursive type definitions.
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
                layout: Layout::new::<()>(),
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

    #[inline]
    fn layout(&self) -> Option<Layout> {
        Some(self.store.get_def(self.handle).layout)
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
}

// ============================================================================
// Constructors
// ============================================================================

impl VShapeDef {
    /// Create a scalar shape with the given layout.
    pub const fn scalar(layout: Layout) -> Self {
        Self {
            layout,
            def: VDef::Scalar,
        }
    }

    /// Create a struct shape with the given fields.
    ///
    /// The `field_shapes` parameter provides the shape handle for each field.
    /// Use `store.get_def(handle).layout` to get layouts for size calculation.
    pub fn struct_with_fields(store: &VShapeStore, fields: &[(usize, VShapeHandle)]) -> Self {
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
            def: VDef::Struct(VStructDef {
                field_count: fields.len() as u8,
                fields: field_array,
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

            let layout = Layout::from_size_align(offset, 1).unwrap();

            VShapeDef {
                layout,
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

            let layout = Layout::from_size_align(size, align).unwrap();
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
            let layout = Layout::from_size_align(size, 1).unwrap();
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

            let layout = Layout::from_size_align(offset, 1).unwrap();
            store.add(VShapeDef {
                layout,
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
        assert!(
            new_offset <= self.size,
            "pointer arithmetic out of bounds: offset {} + {} = {} > size {}",
            self.offset,
            n,
            new_offset,
            self.size
        );
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
}

impl<S: IShape> VHeap<S> {
    /// Create a new verified heap with no allocations.
    pub fn new() -> Self {
        Self {
            allocs: [const { None }; MAX_VHEAP_ALLOCS],
            next_id: 0,
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

    fn matches_subshape(stored: S, offset: usize, target: S) -> bool {
        if offset == 0 && stored == target {
            return true;
        }

        if !stored.is_struct() {
            return false;
        }

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

        false
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
        assert!(
            (id as usize) < MAX_VHEAP_ALLOCS,
            "too many allocations (max {})",
            MAX_VHEAP_ALLOCS
        );

        let layout = shape.layout().expect("IShape requires sized types");
        self.allocs[id as usize] = Some((ByteRangeTracker::new(), shape));
        self.next_id += 1;

        VPtr::new(id, layout.size() as u32)
    }

    unsafe fn dealloc(&mut self, ptr: VPtr, shape: S) {
        assert!(
            ptr.is_at_start(),
            "dealloc requires pointer to allocation start (offset {} != 0)",
            ptr.offset_bytes()
        );

        let (tracker, stored_shape) = self.get_tracker(ptr.alloc_id());

        assert!(*stored_shape == shape, "dealloc: shape mismatch");
        assert!(
            tracker.is_empty(),
            "dealloc: allocation still has initialized bytes (did you forget to drop?)"
        );

        self.allocs[ptr.alloc_id() as usize] = None;
    }

    unsafe fn memcpy(&mut self, dst: VPtr, src: VPtr, len: usize) {
        if len == 0 {
            return;
        }

        // Bounds checks
        assert!(
            dst.offset_bytes() + len <= dst.alloc_size(),
            "memcpy: dst out of bounds"
        );
        assert!(
            src.offset_bytes() + len <= src.alloc_size(),
            "memcpy: src out of bounds"
        );

        // Check src is initialized
        let (src_tracker, _) = self.get_tracker(src.alloc_id());
        assert!(
            src_tracker.is_init(src.offset, src.offset + len as u32),
            "memcpy: src range not initialized"
        );

        // Check dst is uninitialized and mark it initialized
        let (dst_tracker, _) = self.get_tracker_mut(dst.alloc_id());
        dst_tracker
            .mark_init(dst.offset, dst.offset + len as u32)
            .expect("memcpy: dst range already initialized (forgot to drop?)");
    }

    unsafe fn drop_in_place(&mut self, ptr: VPtr, shape: S) {
        let layout = shape.layout().expect("IShape requires sized types");
        if layout.size() == 0 {
            return; // ZST - nothing to drop
        }

        let (tracker, stored_shape) = self.get_tracker_mut(ptr.alloc_id());

        assert!(
            Self::matches_subshape(*stored_shape, ptr.offset_bytes(), shape),
            "drop_in_place: shape mismatch"
        );

        assert!(
            ptr.offset_bytes() + layout.size() <= ptr.alloc_size(),
            "drop_in_place: out of bounds"
        );

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
                assert!(field_end <= end, "drop_in_place: field out of bounds");

                tracker
                    .mark_uninit(start, field_end)
                    .expect("drop_in_place: field range not initialized");
            }

            tracker
                .clear_range(base, end)
                .expect("drop_in_place: clear_range failed");
            return;
        }

        tracker
            .mark_uninit(base, end)
            .expect("drop_in_place: range not initialized");
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

        // Bounds check
        assert!(
            ptr.offset_bytes() + len <= ptr.alloc_size(),
            "default_in_place: out of bounds"
        );

        let (tracker, _) = self.get_tracker_mut(ptr.alloc_id());
        tracker
            .mark_init(ptr.offset, ptr.offset + len as u32)
            .expect("default_in_place: range already initialized");
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
mod tests {
    use super::*;
    use core::alloc::Layout;
    use facet_core::{Facet, Shape};

    type S<'a> = VShapeView<'a, VShapeStore>;

    #[test]
    fn verified_alloc_dealloc() {
        let mut store = VShapeStore::new();
        let h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
        let shape = store.view(h);

        let mut heap = VHeap::<S<'_>>::new();
        let ptr = unsafe { heap.alloc(shape) };

        assert!(ptr.is_at_start());
        assert_eq!(ptr.alloc_size(), 4);

        unsafe { heap.dealloc(ptr, shape) };
        heap.assert_no_leaks();
    }

    #[test]
    fn vshape_scalar_is_not_struct() {
        let mut store = VShapeStore::new();
        let h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
        let s = store.view(h);
        assert!(!s.is_struct());
        assert!(s.as_struct().is_none());
    }

    #[test]
    fn vshape_struct_is_struct() {
        let mut store = VShapeStore::new();
        // Add field shapes first
        let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));

        // Create struct with two u32 fields
        let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let struct_h = store.add(struct_def);

        let s = store.view(struct_h);
        assert!(s.is_struct());
        assert!(s.as_struct().is_some());
    }

    #[test]
    fn vshape_struct_field_access() {
        let mut store = VShapeStore::new();
        // Add field shapes
        let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
        let u64_h = store.add(VShapeDef::scalar(Layout::new::<u64>()));

        // Create struct
        let struct_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (8, u64_h)]);
        let struct_h = store.add(struct_def);

        let s = store.view(struct_h);
        let st = s.as_struct().unwrap();

        assert_eq!(st.field_count(), 2);

        let f0 = st.field(0).unwrap();
        assert_eq!(f0.offset(), 0);
        assert_eq!(f0.shape().layout().unwrap().size(), 4);

        let f1 = st.field(1).unwrap();
        assert_eq!(f1.offset(), 8);
        assert_eq!(f1.shape().layout().unwrap().size(), 8);

        assert!(st.field(2).is_none());
    }

    #[test]
    fn vshape_nested_struct() {
        let mut store = VShapeStore::new();

        // Inner struct: { a: u32, b: u32 }
        let u32_h = store.add(VShapeDef::scalar(Layout::new::<u32>()));
        let inner_def = VShapeDef::struct_with_fields(&store, &[(0, u32_h), (4, u32_h)]);
        let inner_h = store.add(inner_def);

        // Outer struct: { x: u64, inner: Inner }
        let u64_h = store.add(VShapeDef::scalar(Layout::new::<u64>()));
        let outer_def = VShapeDef::struct_with_fields(
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
        assert_eq!(f0.shape().layout().unwrap().size(), 8);

        // Field 1: inner struct
        let f1 = outer_st.field(1).unwrap();
        assert_eq!(f1.offset(), 8);
        assert!(f1.shape().is_struct()); // This is the key test!

        // Navigate into inner struct
        let inner_st = f1.shape().as_struct().unwrap();
        assert_eq!(inner_st.field_count(), 2);

        let inner_f0 = inner_st.field(0).unwrap();
        assert_eq!(inner_f0.offset(), 0);
        assert_eq!(inner_f0.shape().layout().unwrap().size(), 4);

        let inner_f1 = inner_st.field(1).unwrap();
        assert_eq!(inner_f1.offset(), 4);
        assert_eq!(inner_f1.shape().layout().unwrap().size(), 4);
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
        assert!(f0.shape().layout().unwrap().size() > 0);
        assert!(f1.shape().layout().unwrap().size() > 0);

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

    // --- VArena tests ---

    #[test]
    fn verified_arena_alloc_and_get() {
        let mut arena = VArena::<u32, 8>::new();
        let id = arena.alloc(42);

        assert!(id.is_valid());
        assert_eq!(*arena.get(id), 42);
    }

    #[test]
    fn verified_arena_free_and_reuse() {
        let mut arena = VArena::<u32, 8>::new();

        let id1 = arena.alloc(1);
        let _id2 = arena.alloc(2);

        let val = arena.free(id1);
        assert_eq!(val, 1);

        // Next alloc can reuse freed slot (or use a new one)
        let id3 = arena.alloc(3);
        assert!(id3.is_valid());
        assert_eq!(*arena.get(id3), 3);
    }

    #[test]
    #[should_panic(expected = "double-free")]
    fn verified_arena_double_free_panics() {
        let mut arena = VArena::<u32, 8>::new();
        let id = arena.alloc(1);
        arena.free(id);
        arena.free(id);
    }

    #[test]
    fn verified_arena_get_mut() {
        let mut arena = VArena::<u32, 8>::new();
        let id = arena.alloc(1);

        *arena.get_mut(id) = 99;
        assert_eq!(*arena.get(id), 99);
    }
}

// ==================================================================
// Kani Proofs
// ==================================================================

#[cfg(kani)]
mod kani_proofs {
    use super::*;
    use core::alloc::Layout;

    type S<'a> = VShapeView<'a, VShapeStore>;

    /// Prove: pointer arithmetic preserves allocation identity
    #[kani::proof]
    #[kani::unwind(5)]
    fn pointer_arithmetic_same_alloc() {
        let size: u32 = kani::any();
        let offset: u32 = kani::any();
        kani::assume(size > 0 && size <= 64);
        kani::assume(offset <= size);

        let mut store = VShapeStore::new();
        let h = store.add(VShapeDef::scalar(
            Layout::from_size_align(size as usize, 1).unwrap(),
        ));
        let shape = store.view(h);

        let mut heap = VHeap::<S<'_>>::new();
        let ptr = unsafe { heap.alloc(shape) };

        let offset_ptr = ptr.offset(offset as usize);

        kani::assert(
            offset_ptr.alloc_id() == ptr.alloc_id(),
            "same allocation after offset",
        );
        kani::assert(
            offset_ptr.alloc_size() == ptr.alloc_size(),
            "same size after offset",
        );
    }

    /// Prove: multiple allocations have distinct IDs
    #[kani::proof]
    #[kani::unwind(5)]
    fn multiple_allocs_distinct() {
        let mut store = VShapeStore::new();
        let h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        let shape = store.view(h);

        let mut heap = VHeap::<S<'_>>::new();
        let ptr1 = unsafe { heap.alloc(shape) };
        let ptr2 = unsafe { heap.alloc(shape) };

        kani::assert(ptr1.alloc_id() != ptr2.alloc_id(), "distinct alloc ids");
    }

    /// Prove: store handles are valid after adding shapes
    #[kani::proof]
    #[kani::unwind(10)]
    fn store_handles_valid() {
        let mut store = VShapeStore::new();

        let num_shapes: u8 = kani::any();
        kani::assume(num_shapes > 0 && num_shapes <= 4);

        for i in 0..num_shapes {
            let size: usize = kani::any();
            kani::assume(size > 0 && size <= 8);
            let layout = Layout::from_size_align(size, 1).unwrap();
            let h = store.add(VShapeDef::scalar(layout));
            kani::assert(h.0 == i, "handle matches index");
        }

        // All handles should be retrievable
        for i in 0..num_shapes {
            let view = store.view(VShapeHandle(i));
            kani::assert(
                view.store as *const _ == &store as *const _,
                "view borrows store",
            );
        }
    }

    /// Prove: nested struct field navigation works correctly
    #[kani::proof]
    #[kani::unwind(10)]
    fn nested_struct_navigation() {
        let mut store = VShapeStore::new();

        // Scalar shape
        let scalar_h = store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));

        // Inner struct with 2 scalar fields
        let inner_def = VShapeDef {
            layout: Layout::from_size_align(8, 1).unwrap(),
            def: VDef::Struct(VStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                    arr[0] = VFieldDef::new(0, scalar_h);
                    arr[1] = VFieldDef::new(4, scalar_h);
                    arr
                },
            }),
        };
        let inner_h = store.add(inner_def);

        // Outer struct with scalar + inner struct
        let outer_def = VShapeDef {
            layout: Layout::from_size_align(12, 1).unwrap(),
            def: VDef::Struct(VStructDef {
                field_count: 2,
                fields: {
                    let mut arr = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
                    arr[0] = VFieldDef::new(0, scalar_h);
                    arr[1] = VFieldDef::new(4, inner_h);
                    arr
                },
            }),
        };
        let outer_h = store.add(outer_def);

        // Navigate: outer -> field[1] -> inner struct -> field[0] -> scalar
        let outer = store.view(outer_h);
        kani::assert(outer.is_struct(), "outer is struct");

        let outer_st = outer.as_struct().unwrap();
        kani::assert(outer_st.field_count() == 2, "outer has 2 fields");

        let field0 = outer_st.field(0).unwrap();
        kani::assert(!field0.shape().is_struct(), "field 0 is scalar");

        let field1 = outer_st.field(1).unwrap();
        kani::assert(field1.shape().is_struct(), "field 1 is struct");

        let inner = field1.shape();
        let inner_st = inner.as_struct().unwrap();
        kani::assert(inner_st.field_count() == 2, "inner has 2 fields");

        let inner_field0 = inner_st.field(0).unwrap();
        kani::assert(!inner_field0.shape().is_struct(), "inner field 0 is scalar");
        kani::assert(
            inner_field0.shape().layout().unwrap().size() == 4,
            "inner field 0 is 4 bytes",
        );
    }

    /// Prove: field shape handles point to valid shapes in store
    #[kani::proof]
    #[kani::unwind(10)]
    fn field_handles_valid() {
        let mut store = VShapeStore::new();

        // Add scalars
        let num_scalars: u8 = kani::any();
        kani::assume(num_scalars >= 1 && num_scalars <= 3);

        for _ in 0..num_scalars {
            store.add(VShapeDef::scalar(Layout::from_size_align(4, 1).unwrap()));
        }

        // Add a struct referencing those scalars
        let field_count: u8 = kani::any();
        kani::assume(field_count > 0 && field_count <= 3);

        let mut fields = [VFieldDef::new(0, VShapeHandle(0)); MAX_FIELDS_PER_STRUCT];
        for i in 0..(field_count as usize) {
            let shape_idx: u8 = kani::any();
            kani::assume(shape_idx < num_scalars);
            fields[i] = VFieldDef::new(i * 4, VShapeHandle(shape_idx));
        }

        let struct_def = VShapeDef {
            layout: Layout::from_size_align((field_count as usize) * 4, 1).unwrap(),
            def: VDef::Struct(VStructDef {
                field_count,
                fields,
            }),
        };
        let struct_h = store.add(struct_def);

        // Navigate to each field and verify its shape is accessible
        let view = store.view(struct_h);
        let st = view.as_struct().unwrap();

        for i in 0..(field_count as usize) {
            let field = st.field(i).unwrap();
            let field_shape = field.shape();
            // Should not panic - the handle should be valid
            let _layout = field_shape.layout().unwrap();
            kani::assert(!field_shape.is_struct(), "field points to scalar");
        }
    }

    #[kani::proof]
    #[kani::unwind(6)]
    fn verified_arena_alloc_free() {
        let mut arena = VArena::<u32, 4>::new();

        let id1 = arena.alloc(1);
        let id2 = arena.alloc(2);

        kani::assert(id1.is_valid(), "id1 is valid");
        kani::assert(id2.is_valid(), "id2 is valid");
        kani::assert(id1 != id2, "ids are distinct");

        kani::assert(*arena.get(id1) == 1, "id1 holds 1");
        kani::assert(*arena.get(id2) == 2, "id2 holds 2");

        let v1 = arena.free(id1);
        kani::assert(v1 == 1, "freed value is 1");

        // Can still access id2
        kani::assert(*arena.get(id2) == 2, "id2 still holds 2");

        // Can alloc again (may reuse id1's slot)
        let id3 = arena.alloc(3);
        kani::assert(id3.is_valid(), "id3 is valid");
        kani::assert(*arena.get(id3) == 3, "id3 holds 3");
    }
}
