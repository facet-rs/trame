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
mod tests;

#[cfg(kani)]
mod proofs;
